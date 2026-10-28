// Auto-loaded ROMs, as CRTROM_AUTOLOAD reads them from globals.vhd through the sysinfo
// device: a record of four words per ROM (type, device or HyperRAM window, mandatory or
// optional, where its name starts in the names), then the names. The manually loaded
// ones have two words each: type, device.
#include <string.h>
#include "roms.h"
#include "cfg.h"
#include "console.h"
#include "core.h"
#include "ff.h"
#include "m2m_rv.h"
#include "screen.h"
#include "sdcard.h"
#include "shell.h"

#define CRTROM_MAN_NUM   0x000
#define CRTROM_AUT_NUM   0x001
#define CRTROM_MAN_BUF   0x100
#define CRTROM_AUT_BUF   0x200
#define CRTROM_AUT_NAMES 0x300
#define CRTROM_AUT_MAX   16

#define TYPE_DEVICE      0
#define TYPE_HYPERRAM    1
#define MODE_OPTIONAL    4

static uint8_t buf[512];           // a block of the file being loaded

#define CSR_WINDOW       0xFFFFu
#define CSR_STATUS       0x000     // to the device: load status, CSR_ST_*
#define CSR_FS_LO        0x001     // file size
#define CSR_FS_HI        0x002
#define CSR_PARSEST      0x010     // from the device: parser status, CSR_PT_*
#define CSR_PARSEE1      0x011     // its error code
#define CSR_ERR_STR      0x100     // and message
#define CSR_ST_IDLE      0
#define CSR_ST_LOADING   1
#define CSR_ST_ERR       2
#define CSR_ST_OK        3
#define CSR_PT_OK        2
#define CSR_PT_ERR       3

// sd_dma.vhd behind a device's CSR (QNICE's sdload.asm)
#define SDL_CSR_MAGIC    0x030
#define SDL_MAGIC        0x5D4A
#define SDL_CSR_CTRL     0x031     // W: start, R: bit 0 busy, bit 1 error
#define SDL_CSR_BLK_LO   0x032
#define SDL_CSR_BLK_HI   0x033
#define SDL_CSR_COUNT    0x034
#define SDL_CSR_DEST_LO  0x035
#define SDL_CSR_DEST_HI  0x036
#define SDL_CSR_LEN_LO   0x037     // nothing is written from here on
#define SDL_CSR_LEN_HI   0x038
#define SDL_CHUNK        128       // blocks per transfer: a step of the progress bar
#define SDL_ERR_DMA      0xEE40
#define SDL_RUNS         32        // a file in more pieces is copied by the CPU

uint16_t crtrom_csr_get(uint16_t dev, int reg)
{
   M2M_RAMROM_DEV = dev;
   return M2M_DEV16[(CSR_WINDOW << 12) + reg];
}

void crtrom_csr_set(uint16_t dev, int reg, uint16_t v)
{
   M2M_RAMROM_DEV = dev;
   M2M_DEV16[(CSR_WINDOW << 12) + reg] = v;
}

// One byte per 16-bit word, at the start of a device, or of a HyperRAM window
static int load(FIL *f, int type, uint16_t target)
{
   uint32_t at = 0;
   UINT got;

   do {
      if (f_read(f, buf, sizeof buf, &got) != FR_OK)
         return -1;
      volatile uint16_t *dst = (volatile uint16_t *)M2M_HYPERRAM + ((uint32_t)target << 12);
      if (type != TYPE_HYPERRAM) {
         M2M_RAMROM_DEV = target;
         dst = M2M_DEV16;
      }
      for (UINT i = 0; i < got; i++)
         dst[at++] = buf[i];
   } while (got == sizeof buf);
   return 0;
}

void roms_autoload(void)
{
   static const char err_mount[] = "This core needs to load one or more\n"
                                   "mandatory ROMs from SD card. But no SD\n"
                                   "card can be mounted.\n";
   static const char err_load[]  = "\n\nFile not found or file read error.\n"
                                   "The core needs this ROM to start.\n\n";
   int n = sysinfo_get(SYS_CRTSANDROMS, CRTROM_AUT_NUM);
   int mounted = sdcard_state() == SDCARD_OK;

   if (n > CRTROM_AUT_MAX)
      shell_fatal("Too many auto-load ROMs in globals.vhd\n", n);

   // The manually loaded ones' devices start idle. QNICE could also load them into a
   // HyperRAM window; no core here does.
   for (int i = 0; i < crtrom_count(); i++) {
      if (sysinfo_get(SYS_CRTSANDROMS, CRTROM_MAN_BUF + 2 * i) != TYPE_DEVICE)
         shell_fatal("globals.vhd: C_CRTROMS_MAN: this firmware\nloads manual CRTs/ROMs only into\ndevices.\n", i);
      crtrom_csr_set(crtrom_dev(i), 0, CSR_ST_IDLE);
   }

   for (int i = 0; i < n; i++) {
      int      base   = CRTROM_AUT_BUF + 4 * i;
      int      type   = sysinfo_get(SYS_CRTSANDROMS, base);
      uint16_t target = sysinfo_get(SYS_CRTSANDROMS, base + 1);
      int      opt    = sysinfo_get(SYS_CRTSANDROMS, base + 2) == MODE_OPTIONAL;
      int      start  = sysinfo_get(SYS_CRTSANDROMS, base + 3);
      char     name[64], msg[sizeof name + sizeof err_load];
      int      len = 0;
      FIL      f;

      if (!mounted) {
         if (!opt)
            shell_fatal(err_mount, 0);
         continue;
      }
      for (char c; len < (int)sizeof name - 1 &&
                   (c = sysinfo_get(SYS_CRTSANDROMS, CRTROM_AUT_NAMES + start + len)); len++)
         name[len] = c;
      name[len] = 0;

      FRESULT r = f_open(&f, name, FA_READ);
      int bad = r != FR_OK || load(&f, type, target) != 0;
      if (r == FR_OK)
         f_close(&f);
      if (bad && !opt) {
         int k = 0;
         for (const char *s = name; *s; s++)
            msg[k++] = *s;
         for (const char *s = err_load; *s; s++)
            msg[k++] = *s;
         msg[k] = 0;
         shell_fatal(msg, r);
      }
   }
}

// -------------------------------------------------------------------------------------
// Manually loaded cartridges and ROMs
// -------------------------------------------------------------------------------------

int crtrom_count(void)
{
   return sysinfo_get(SYS_CRTSANDROMS, CRTROM_MAN_NUM);
}

uint16_t crtrom_dev(int k)
{
   return sysinfo_get(SYS_CRTSANDROMS, CRTROM_MAN_BUF + 2 * k + 1);
}

int crtrom_loaded(int k)
{
   return crtrom_csr_get(crtrom_dev(k), CSR_STATUS) == CSR_ST_OK;
}

// The progress bar on the frame's bottom line, a character per 39th of the file
static uint32_t prog_step, prog_left;
static int      prog_x;

static void progress_start(uint32_t size)
{
   char bar[OSM_DX];
   int w = OSM_DX - 6;
   bar[0] = (char)185;
   memset(bar + 1, ' ', w);
   bar[w + 1] = (char)204;
   bar[w + 2] = 0;
   scr_print(2, OSM_DY - 1, 2, bar);
   prog_step = size / w ? size / w : 1;
   prog_left = prog_step;
   prog_x    = 3;
}

static void progress(uint32_t bytes)
{
   static const char c[] = { (char)254, 0 };
   while (bytes >= prog_left) {
      bytes    -= prog_left;
      prog_left = prog_step;
      if (prog_x < OSM_DX - 3)
         scr_print(prog_x++, OSM_DY - 1, 0, c);
   }
   prog_left -= bytes;
}

// The file straight from the SD card by the device's DMA, a run of clusters at a time:
// 0 = loaded, 1 = the device has no DMA or the file has too many pieces, else an error
static int dma_load(uint16_t dev, FIL *f)
{
   DWORD    runs[2 * SDL_RUNS + 2];
   FATFS   *fs = f->obj.fs;
   uint32_t size = f_size(f), blocks = (size + 511) / 512, dest = 0;

   if (crtrom_csr_get(dev, SDL_CSR_MAGIC) != SDL_MAGIC || size >= 32u << 20)
      return 1;
   runs[0] = sizeof runs / sizeof runs[0];
   f->cltbl = runs;
   FRESULT r = f_lseek(f, CREATE_LINKMAP);
   f->cltbl = 0;
   if (r != FR_OK)
      return 1;

   crtrom_csr_set(dev, SDL_CSR_LEN_LO, size & 0xFFFF);
   crtrom_csr_set(dev, SDL_CSR_LEN_HI, size >> 16);
   for (DWORD *run = runs + 1; run[0] && blocks; run += 2) {
      uint32_t lba = fs->database + (run[1] - 2) * fs->csize;
      uint32_t left = run[0] * fs->csize;
      if (left > blocks)
         left = blocks;
      blocks -= left;
      while (left) {
         uint32_t n = left < SDL_CHUNK ? left : SDL_CHUNK;
         crtrom_csr_set(dev, SDL_CSR_BLK_LO, lba & 0xFFFF);
         crtrom_csr_set(dev, SDL_CSR_BLK_HI, lba >> 16);
         crtrom_csr_set(dev, SDL_CSR_COUNT, n);
         crtrom_csr_set(dev, SDL_CSR_DEST_LO, dest & 0xFFFF);
         crtrom_csr_set(dev, SDL_CSR_DEST_HI, dest >> 16);
         crtrom_csr_set(dev, SDL_CSR_CTRL, 0);          // any write starts it
         uint16_t st;
         while ((st = crtrom_csr_get(dev, SDL_CSR_CTRL)) & 1)
            ;
         if (st & 2)
            return SDL_ERR_DMA;
         lba += n;
         dest += n * 512;
         left -= n;
         progress(n * 512);
      }
   }
   return 0;
}

// The CPU copies it: a byte per 16-bit word of the device
static int copy_load(uint16_t dev, FIL *f)
{
   uint32_t at = 0;
   UINT got;
   do {
      FRESULT r = f_read(f, buf, sizeof buf, &got);
      if (r != FR_OK)
         return 0x100 + r;
      M2M_RAMROM_DEV = dev;
      for (UINT i = 0; i < got; i++)
         M2M_DEV16[at++] = buf[i];
      progress(got);
   } while (got == sizeof buf);
   return 0;
}

int crtrom_load(int k, const char *path, int group, const char **msg)
{
   static char parse_msg[128];
   uint16_t dev = crtrom_dev(k);
   int type, err;
   FIL f;

   *msg = 0;
   crtrom_csr_set(dev, CSR_STATUS, CSR_ST_LOADING);
   FRESULT r = f_open(&f, path, FA_READ);
   if (r != FR_OK) {
      crtrom_csr_set(dev, CSR_STATUS, CSR_ST_ERR);
      shell_fatal("File selected in the browser not found.\n", r);
   }
   uint32_t size = f_size(&f);
   if ((err = core_prep_load(size, CTX_LOAD_ROM, group, &type, msg))) {
      f_close(&f);
      return err;
   }
   progress_start(size);
   if ((err = dma_load(dev, &f)) == 1)
      err = copy_load(dev, &f);
   f_close(&f);
   if (err)
      shell_fatal("SD Card:\nUnkown error while loading disk image\n", err);

   // The device parses it
   crtrom_csr_set(dev, CSR_FS_LO, size & 0xFFFF);
   crtrom_csr_set(dev, CSR_FS_HI, size >> 16);
   crtrom_csr_set(dev, CSR_STATUS, CSR_ST_OK);
   uint16_t st;
   while ((st = crtrom_csr_get(dev, CSR_PARSEST)) != CSR_PT_OK && st != CSR_PT_ERR)
      ;
   if (st == CSR_PT_OK)
      return 0;
   int i = 0;
   for (char c; i < (int)sizeof parse_msg - 1 && (c = crtrom_csr_get(dev, CSR_ERR_STR + i)); i++)
      parse_msg[i] = c;
   parse_msg[i] = 0;
   *msg = parse_msg;
   return crtrom_csr_get(dev, CSR_PARSEE1);
}
