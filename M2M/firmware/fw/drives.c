// The virtual drives, as vdrives.asm, MAP_LOAD and VDSD_OFF (sdload.asm) run them. Each
// drive's device reads its image straight from the SD card (CORE/vhdl/vd_sd.vhd in the
// C128 core): mounting hands it the SD block of each 4 KB piece of the image, then
// vdrives.vhd tells the core the image's size and type.
//
// Drives that keep their image in a RAM buffer, which QNICE fills and serves block by
// block (HANDLE_DRV_RD, HANDLE_DRV_WR, FLUSH_CACHE), are not supported.
#include "drives.h"
#include "cfg.h"
#include "core.h"
#include "ff.h"
#include "heap.h"
#include "m2m_rv.h"
#include "roms.h"
#include "shell.h"

// vdrives.vhd: control and data registers in window 0
#define VD_IMG_MOUNT  0x000        // strobed: the image of a drive changes
#define VD_RO         0x001
#define VD_SIZE_L     0x002
#define VD_SIZE_H     0x003
#define VD_TYPE       0x004
#define VD_DRV_MOUNT  0x00A        // a bit per mounted drive

// vd_sd.vhd: the block table in window 0, the CSR in window 0xFFFF
#define MAP_MAGIC     0x5D4D
#define MAP_CSR_MAGIC 0x030
#define MAP_CSR_CTRL  0x031        // W: 1 on, 0 off; R: bit 0 on, bit 2 busy, bit 5 ready
#define MAP_READY     0x20         // every 1541/1571 out of reset has its disk (iec_drive)
#define MAP_CSR_NBLK  0x032        // blocks of the image
#define MAP_CSR_POWER 0x034        // bits 1-0 enum drive_power; R: bit 2 the drive runs
#define MAP_PIECE     8            // blocks per table entry
#define MAP_MAX       2048         // blocks a table holds
#define MAP_RUNS      (MAP_MAX / MAP_PIECE + 1)

#define ERR_CHAIN     0xEE41       // the cluster chain ends too early
#define ERR_TWICE     0xEE42       // the image is in another drive
#define ERR_LARGE     0xEE43       // too large to map
#define ERR_SPLIT     0xEE44       // a 4 KB piece of it is split

static int       num;
static uint16_t  vd_dev, map_dev[DRIVES_MAX];
static uint32_t  seen;                     // the mount status, as drives_changed saw it
static int       img_type[DRIVES_MAX];     // what each drive was last given (vdrives' img_type)
static DWORD     sclust[DRIVES_MAX];       // where each drive's image starts on the card
static DWORD    *runs;                     // an image's runs of clusters (FatFs link map)
static uint32_t *table;                    // its table, until it goes to the drive

static uint16_t vd_get(int reg)
{
   M2M_RAMROM_DEV = vd_dev;
   return M2M_DEV16[reg];
}

static void vd_set(int reg, uint16_t v)
{
   M2M_RAMROM_DEV = vd_dev;
   M2M_DEV16[reg] = v;
}

void drives_init(void)
{
   num = sysinfo_get(SYS_VDRIVES, 0);
   if (num > DRIVES_MAX)
      shell_fatal("Too many virtual drives: Hint: VDRIVES_MAX\n", num);
   if (num == 0)
      return;
   vd_dev = sysinfo_get(SYS_VDRIVES, 1);
   for (int d = 0; d < num; d++) {
      map_dev[d] = sysinfo_get(SYS_VDRIVES, 0x100 + d);
      if (map_dev[d] == 0 || map_dev[d] == 0xEEEE)
         shell_fatal("Not enough buffers for virtual drives.\n", 0);
      if (crtrom_csr_get(map_dev[d], MAP_CSR_MAGIC) != MAP_MAGIC)
         shell_fatal("This firmware mounts disk images only\nin drives that read them from the SD\ncard (vd_sd.vhd).\n", d);
   }
   seen  = vd_get(VD_DRV_MOUNT);
   runs  = heap_alloc((2 * MAP_RUNS + 2) * sizeof *runs);
   table = heap_alloc(MAP_MAX / MAP_PIECE * sizeof *table);
}

int drives_count(void)
{
   return num;
}

int drive_mounted(int d)
{
   return d < num && (vd_get(VD_DRV_MOUNT) >> d & 1);
}

int drives_ready(void)
{
   return num && (crtrom_csr_get(map_dev[0], MAP_CSR_CTRL) & MAP_READY);
}

int drive_image_type(int d)
{
   return drive_mounted(d) ? img_type[d] : -1;
}

void drive_power(int d, enum drive_power p)
{
   if (d < num)
      crtrom_csr_set(map_dev[d], MAP_CSR_POWER, p);
}

enum drive_power drive_power_of(int d)
{
   return d < num ? crtrom_csr_get(map_dev[d], MAP_CSR_POWER) & 3 : DRIVE_AUTO;
}

int drive_running(int d)
{
   return d < num && (crtrom_csr_get(map_dev[d], MAP_CSR_POWER) & 4);
}

int drives_changed(void)
{
   uint32_t now = num ? vd_get(VD_DRV_MOUNT) : 0;
   if (now == seen)
      return 0;
   seen = now;
   return 1;
}

// vdrives.vhd: a nonzero size mounts, size 0 unmounts. The core sees the strobe a few
// of its clocks later.
static void strobe(int d, uint32_t size, int type)
{
   img_type[d] = type;
   vd_set(VD_SIZE_L, size & 0xFFFF);
   vd_set(VD_SIZE_H, size >> 16);
   vd_set(VD_RO, 0);
   vd_set(VD_TYPE, type);
   uint16_t m = vd_get(VD_IMG_MOUNT);
   vd_set(VD_IMG_MOUNT, m | 1u << d);
   uint32_t start = MTIME_LO;
   while (MTIME_LO - start < CPU_HZ / 500000)
      ;
   vd_set(VD_IMG_MOUNT, m & ~(1u << d));
   start = MTIME_LO;
   while (drive_mounted(d) != (size != 0) && MTIME_LO - start < CPU_HZ / 1000)
      ;
   seen = vd_get(VD_DRV_MOUNT);
}

static void map_off(int d)
{
   crtrom_csr_set(map_dev[d], MAP_CSR_CTRL, 0);
   while (crtrom_csr_get(map_dev[d], MAP_CSR_CTRL) & 4)   // busy writing back
      ;
}

// The SD block of each 4 KB piece of the image into table. A piece must lie in one place
// on the card, which clusters of 4 KB and more guarantee.
static int map(FIL *f, uint32_t blocks)
{
   FATFS *fs = f->obj.fs;
   uint32_t at = 0;                        // image block

   runs[0] = 2 * MAP_RUNS + 2;
   f->cltbl = runs;
   FRESULT r = f_lseek(f, CREATE_LINKMAP);
   f->cltbl = 0;
   if (r == FR_NOT_ENOUGH_CORE)            // more runs than pieces
      return ERR_SPLIT;
   if (r != FR_OK)
      return 0x100 + r;
   for (DWORD *run = runs + 1; run[0] && at < blocks; run += 2) {
      uint32_t lba = fs->database + (run[1] - 2) * fs->csize;
      uint32_t n = run[0] * fs->csize;
      if (at % MAP_PIECE)                  // the last run ended inside a piece
         return ERR_SPLIT;
      for (uint32_t i = 0; i < n && at + i < blocks; i += MAP_PIECE)
         table[(at + i) / MAP_PIECE] = lba + i;
      at += n;
   }
   return at < blocks ? ERR_CHAIN : 0;
}

int drive_mount(int d, const char *path, int group, const char **msg)
{
   static const char twice[] = "This disk image is already\nin the other drive.\n";
   static const char split[] = "This disk image is split into\npieces under 4 KB on the card.\n"
                               "Copy it again, or format the\ncard with 4 KB clusters.\n";
   int type, err;
   FIL f;

   *msg = 0;
   FRESULT r = f_open(&f, path, FA_READ);
   if (r != FR_OK)
      shell_fatal("File selected in the browser not found.\n", r);
   uint32_t size = f_size(&f), blocks = (size + 511) / 512;
   if ((err = core_prep_load(size, CTX_MOUNT_DISKIMG, group, &type, msg)))
      goto out;

   // The same file in two drives: their changes would overwrite each other on the card
   for (int o = 0; o < num; o++)
      if (o != d && (crtrom_csr_get(map_dev[o], MAP_CSR_CTRL) & 1) && sclust[o] == f.obj.sclust) {
         *msg = twice;
         err = ERR_TWICE;
         goto out;
      }
   if (blocks > MAP_MAX) {
      err = ERR_LARGE;
      goto out;
   }
   if ((err = map(&f, blocks))) {
      if (err == ERR_SPLIT)
         *msg = split;
      goto out;
   }

   // Off: the old image's changes go to the card, and it is forgotten. Then the new
   // one's table and size, and on before the drive asks.
   map_off(d);
   M2M_RAMROM_DEV = map_dev[d];
   for (uint32_t p = 0; p < (blocks + MAP_PIECE - 1) / MAP_PIECE; p++) {
      M2M_DEV16[2 * p]     = table[p] & 0xFFFF;
      M2M_DEV16[2 * p + 1] = table[p] >> 16;
   }
   crtrom_csr_set(map_dev[d], MAP_CSR_NBLK, blocks);
   crtrom_csr_set(map_dev[d], MAP_CSR_CTRL, 1);
   while (crtrom_csr_get(map_dev[d], MAP_CSR_CTRL) & 4)
      ;
   sclust[d] = f.obj.sclust;
   strobe(d, size, type);
out:
   f_close(&f);
   return err;
}

void drive_unmount(int d)
{
   map_off(d);
   strobe(d, 0, 0);
}
