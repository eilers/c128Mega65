// The REU's memory and its image file (reu.h)
#include <string.h>
#include "config_ids.h"
#include "core.h"
#include "heap.h"
#include "m2m_rv.h"
#include "reu.h"
#include "uci.h"

#define BLOCK 512                    // through uci_data, a sector at a time

// reu_sd.vhd: SD card blocks straight to and from the REU's memory, and the REU's cache
#define REU_SD_DEVICE 0x010Au        // C_DEV_REU_SD (globals.vhd)
#define SD_MAGIC      0x000          // reads 0x5D52
#define SD_CTRL       0x001          // W: start, bit 0 a save. R: bit 0 busy, 1 failed, 2 dropping
#define SD_BLK_LO     0x002
#define SD_BLK_HI     0x003
#define SD_COUNT      0x004
#define SD_ADDR_LO    0x005          // REU byte address, even
#define SD_ADDR_HI    0x006
#define SD_LEN_LO     0x007          // a load writes this many bytes
#define SD_LEN_HI     0x008
#define SD_DROP       0x009          // W: the REU's cache drops its line
#define SD_CARD       0x00A          // the card the blocks are on (Q2C_CARD)
#define DMA_BLOCKS    64             // a step: 32 KB, a few ms
#define DMA_RUNS      256            // a file in more pieces goes through the CPU
#define DMA_TIMEOUT   (2 * CPU_HZ)

typedef uint32_t __attribute__((may_alias)) word_t;

static char file[UCI_PATH_MAX];      // the file picked in the menu, or ""
static DWORD *runs;                  // FatFs's link map of a file: length, then cluster, of each
                                     // run (in HyperRAM: the data RAM is tight)
static const reu_xfer_t *runs_of;    // the transfer whose file that is

uint32_t reu_size(void)
{
   // The REU submenu's items: Off, 512 KB, 2 MB, 16 MB (the last two not on R3 memory)
   static const uint32_t sizes[] = { 0, 512u << 10, 2u << 20, 16u << 20 };
   for (int i = 1; i < 4; i++)
      if (shell_setting(OPTM_G_REU, i) == 1)
         return sizes[i];
   return 0;
}

uint32_t reu_room(void)
{
   return uci_reu_mem() & 0x8000 ? 16u << 20 : 512u << 10;
}

// Where REU byte 0 is for the CPU: an 8 KiB window of SDRAM or HyperRAM
static uint8_t *reu_mem(void)
{
   uint16_t w = uci_reu_mem();
   uintptr_t base = w & 0x8000 ? M2M_SDRAM : M2M_HYPERRAM;
   return (uint8_t *)(base + (uint32_t)(w & 0x1FFF) * 8192);
}

const char *reu_file(void)
{
   return file[0] ? file : REU_DEFAULT_FILE;
}

int reu_file_picked(void)
{
   return file[0] != 0;
}

void reu_file_set(const char *path)
{
   file[0] = 0;
   if (path && strlen(path) < sizeof file)
      strcpy(file, path);
}

static uint16_t dma_get(int reg)
{
   uint32_t was = M2M_RAMROM_DEV;
   M2M_RAMROM_DEV = REU_SD_DEVICE;
   uint16_t v = M2M_DEV16[reg];
   M2M_RAMROM_DEV = was;
   return v;
}

static void dma_set(int reg, uint32_t v)
{
   uint32_t was = M2M_RAMROM_DEV;
   M2M_RAMROM_DEV = REU_SD_DEVICE;
   M2M_DEV16[reg] = v;
   M2M_RAMROM_DEV = was;
}

// The bitstream has reu_sd.vhd
static int dma_there(void)
{
   return dma_get(SD_MAGIC) == 0x5D52;
}

// After the CPU wrote the REU's memory, up to last: once a read behind the writes is
// answered they are in, then the REU's cache drops its line, so the C128 sees them
static void cpu_wrote(const uint8_t *last)
{
   if (!dma_there())
      return;
   (void)*(const volatile uint8_t *)last;
   dma_set(SD_DROP, 1);
   uint32_t start = MTIME_LO;
   while ((dma_get(SD_CTRL) & 4) && MTIME_LO - start < CPU_HZ / 100)
      ;
}

// From src to dst, both as far past a 4-byte boundary: whole words where it can, so that
// each is one access through rv_soc's bridge, not four
static void copy(uint8_t *dst, const uint8_t *src, uint32_t n)
{
   for (; n && ((uintptr_t)dst & 3); n--)
      *dst++ = *src++;
   for (; n >= 4; n -= 4, dst += 4, src += 4)
      *(word_t *)dst = *(const word_t *)src;
   while (n--)
      *dst++ = *src++;
}

// The runs of clusters j's file lies in, from FatFs (fast seek's map), as the card has
// them now; 0 if there are more than DMA_RUNS
static int map(reu_xfer_t *j)
{
   if (!runs)
      runs = heap_alloc((2 * DMA_RUNS + 2) * sizeof *runs);
   runs[0] = 2 * DMA_RUNS + 2;
   j->card = dma_get(SD_CARD) & 0xFF;
   j->f->cltbl = runs;
   FRESULT r = f_lseek(j->f, CREATE_LINKMAP);
   j->f->cltbl = 0;
   runs_of = r == FR_OK ? j : 0;
   return r == FR_OK;
}

// The SD block that holds file position pos, and in *n how many follow it in its run
static uint32_t block_at(const FATFS *fs, FSIZE_t pos, uint32_t *n)
{
   uint32_t c = pos / 512 / fs->csize, s = pos / 512 % fs->csize;
   for (const DWORD *run = runs + 1; run[0]; run += 2) {
      if (c < run[0]) {
         *n = (run[0] - c) * fs->csize - s;
         return fs->database + (run[1] - 2 + c) * fs->csize + s;
      }
      c -= run[0];
   }
   *n = 0;
   return 0;
}

// The DMA takes a transfer that starts on a block of the file and an even REU byte. A
// save's clusters are all taken first (as far as the card has room), so that the map holds
// them. Else (or with too many runs) the CPU does it all. FatFs's one window (FF_FS_TINY)
// then holds no block of the file: making the map reads the FAT through it, writing back
// a changed block first, and nothing reads the file between the steps.
static FRESULT dma_setup(reu_xfer_t *j)
{
   FIL *f = j->f;
   FSIZE_t pos = f_tell(f);
   FRESULT r = FR_OK;

   j->dma = -1;
   if (!dma_there() || (j->at & 1) || pos % 512 || (j->save && j->left < 512))
      return FR_OK;
   if (j->save && pos + j->left > f_size(f)) {
      r = f_lseek(f, pos + j->left);
      if (r == FR_OK && f_tell(f) < pos + j->left)          // a full card: as much as it took
         j->left = f_tell(f) > pos ? f_tell(f) - pos : 0;
      if (r == FR_OK)
         r = f_lseek(f, pos);
   }
   if (r == FR_OK && map(j))
      j->dma = 1;
   return r;
}

// One DMA step at the file's position; j->dma -1 if the CPU has to go on (a save's last
// bytes of a block)
static FRESULT dma_step(reu_xfer_t *j)
{
   FIL     *f = j->f;
   FATFS   *fs = f->obj.fs;
   FSIZE_t  pos = f_tell(f);
   uint32_t bytes = j->left, n = 0, blk;

   if (!j->save && bytes > f_size(f) - pos)
      bytes = f_size(f) - pos;
   if (!bytes) {
      j->left = 0;                                          // a load at the file's end
      return FR_OK;
   }
   if ((j->save && bytes < 512) || (runs_of != j && !map(j))) {
      j->dma = -1;
      return FR_OK;
   }
   blk = block_at(fs, pos, &n);
   if (!n) {
      j->dma = -1;
      return FR_OK;
   }
   if (n > DMA_BLOCKS)
      n = DMA_BLOCKS;
   if (bytes > n * 512)
      bytes = n * 512;
   if (j->save)
      bytes &= ~511u;
   n = (bytes + 511) / 512;

   dma_set(SD_BLK_LO, blk & 0xFFFF);
   dma_set(SD_BLK_HI, blk >> 16);
   dma_set(SD_COUNT, n);
   dma_set(SD_ADDR_LO, j->at & 0xFFFF);
   dma_set(SD_ADDR_HI, j->at >> 16);
   dma_set(SD_LEN_LO, bytes & 0xFFFF);
   dma_set(SD_LEN_HI, bytes >> 16);
   dma_set(SD_CARD, j->card);
   dma_set(SD_CTRL, j->save);
   uint32_t start = MTIME_LO, st;
   while ((st = dma_get(SD_CTRL)) & 1)
      if (MTIME_LO - start > DMA_TIMEOUT)
         return FR_TIMEOUT;
   if (st & 2)
      return FR_DISK_ERR;

   if (j->save && fs->winsect - blk < n)
      fs->winsect = (LBA_t)-1;                              // a copy there would be old now
   j->at   += bytes;
   j->done += bytes;
   j->left -= bytes;
   return f_lseek(f, pos + bytes);
}

FRESULT reu_step(reu_xfer_t *j, uint32_t max)
{
   if (j->left && !j->dma) {
      FRESULT r = dma_setup(j);
      if (r != FR_OK)
         return r;
   }
   if (j->left && j->dma > 0) {
      FRESULT r = dma_step(j);
      if (r != FR_OK || j->dma > 0)
         return r;
   }

   uint8_t *mem = reu_mem();
   uint32_t wrote = 0;                             // a load: the REU's bytes up to here are new
   FRESULT  r = FR_OK;
   while (j->left && max) {
      UINT n = j->left < BLOCK ? j->left : BLOCK, got;
      if (n > max)
         n = max;
      uint8_t *b = uci_data + (j->at & 3);         // as far off a word as the REU's byte
      if (j->save) {
         copy(b, mem + j->at, n);
         r = f_write(j->f, b, n, &got);
      } else {
         r = f_read(j->f, b, n, &got);
         if (r == FR_OK)
            copy(mem + j->at, b, got);
         if (got)
            wrote = j->at + got;
      }
      if (r != FR_OK)
         break;
      j->done += got;
      if (got < n) {                               // the file's end, or a full card
         j->left = 0;
         break;
      }
      j->at   += n;
      j->left -= n;
      max     -= n;
   }
   if (wrote)
      cpu_wrote(mem + wrote - 1);
   return r;
}
