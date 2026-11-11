// Why the SD card can't be used, kept small for the 8 KB boot ROM. The file system part
// names the likely reason FatFs (ff.c, FF_FS_EXFAT 0, 512-byte sectors) said
// FR_NO_FILESYSTEM; checks of FatFs's that real cards don't fail come out as one.
#include <string.h>
#include "ff.h"
#include "diskio.h"
#include "sd.h"
#include "sd_diag.h"

static sd_diag_t *dg;
static int        col;

static void put(const char *s)
{
   char *l = dg->line[dg->n];
   while (*s && col < SD_DIAG_W)
      l[col++] = *s++;
   l[col] = 0;
}

static void put_hex(uint32_t v, int digits)
{
   char b[9], *p = b + digits;
   *p = 0;
   while (p > b) {
      *--p = "0123456789ABCDEF"[v & 15];
      v >>= 4;
   }
   put(b);
}

static void end_line(void)
{
   if (dg->n < SD_DIAG_LINES - 1)
      dg->n++;
   col = 0;
   dg->line[dg->n][0] = 0;
}

static uint32_t ld16(const uint8_t *p) { return p[0] | p[1] << 8; }
static uint32_t ld32(const uint8_t *p) { return ld16(p) | ld16(p + 2) << 16; }

enum { W_OK, W_READ, W_EXFAT, W_NTFS, W_SIG, W_NOFAT, W_TYPE, W_FEW, W_BPB, W_GPT };
static const char *const why_txt[] = {
   "FatFs takes it", "can't read it", "exFAT, not FAT32", "NTFS, not FAT32",
   "no 55AA signature", "no FAT boot sector", "no FAT32 type text",
   "too few clusters for FAT32", "bad boot sector values", "GPT, needs an MBR"
};

// check_fs: FatFs takes b as a volume's first block (FAT32 by its type text, FAT12/16 by
// its BPB)
static int is_vbr(const uint8_t *b)
{
   return (b[0] == 0xEB || b[0] == 0xE9 || b[0] == 0xE8) &&
          ((ld16(b + 510) == 0xAA55 && !memcmp(b + 82, "FAT32   ", 8)) ||
           (ld16(b + 11) == 512 && ld16(b + 17) && ld16(b + 22)));
}

// A volume's first block, as FatFs's check_fs and mount_volume see it
static int why_vol(const uint8_t *b)
{
   uint32_t cs = b[13], nf = b[16], rsv = ld16(b + 14), fsz = ld32(b + 36);
   uint32_t ts = ld16(b + 19) ? ld16(b + 19) : ld32(b + 32), sys = rsv + fsz * nf, ncl;

   if (!memcmp(b + 3, "EXFAT   ", 8))
      return W_EXFAT;
   if (!memcmp(b + 3, "NTFS    ", 8))
      return W_NTFS;
   if (b[0] != 0xEB && b[0] != 0xE9 && b[0] != 0xE8)
      return ld16(b + 510) == 0xAA55 ? W_NOFAT : W_SIG;
   if (ld16(b + 22))                                // FAT12/16: FatFs reads these
      return ld16(b + 11) == 512 && ld16(b + 17) ? W_OK : W_BPB;
   if (ld16(b + 510) != 0xAA55)
      return W_SIG;
   if (memcmp(b + 82, "FAT32   ", 8))
      return W_TYPE;
   if (ld16(b + 11) != 512 || !cs || (cs & (cs - 1)) || nf - 1 > 1 || !rsv || ld16(b + 17) ||
       ts < sys || ld16(b + 42))
      return W_BPB;
   ncl = (ts - sys) / cs;
   if (ncl <= 0xFFF5)
      return W_FEW;
   return fsz < (ncl + 2 + 127) / 128 ? W_BPB : W_OK;
}

static void volumes(void)
{
   static uint8_t b[512] __attribute__((aligned(4)));
   uint8_t pt[64];
   int w;

   put("Block 0: ");
   if (disk_read(0, b, 0, 1) != RES_OK) {
      put(why_txt[W_READ]);
      return;
   }
   // FatFs reads a partition table only from a valid boot record that is no volume
   w = why_vol(b);
   if (is_vbr(b) || w == W_SIG || w == W_EXFAT || w == W_NTFS) {
      put(is_vbr(b) ? "a volume, " : "");
      put(why_txt[w]);
      return;
   }
   memcpy(pt, b + 446, sizeof pt);
   put("partition table");
   for (int i = 0; i < 4; i++) {
      const uint8_t *e = pt + 16 * i;
      uint32_t lba = ld32(e + 8);
      if (!lba)
         continue;
      end_line();
      put("P");
      put_hex(i + 1, 1);
      put(" ");
      put_hex(e[4], 2);
      put("@");
      put_hex(lba, 8);
      put(": ");
      put(why_txt[e[4] == 0xEE ? W_GPT : disk_read(0, b, lba, 1) != RES_OK ? W_READ : why_vol(b)]);
   }
}

void sd_diag(sd_diag_t *d, int fr)
{
   dg = d;
   d->n = 0;
   col = 0;
   d->line[0][0] = 0;
   put("FatFs ");
   put_hex(fr, 2);
   put(", SD error ");
   put_hex(sd_log.err, 4);
   if (sd_log.err) {
      put(" at ");
      if (sd_log.block == SD_AT_INIT)
         put("init");
      else
         put_hex(sd_log.block, 8);
   }
   end_line();
   put("SD mode ");
   put_hex(sd_log.mode, 5);
   put(", now ");
   put_hex(sd_mode(), 5);
   end_line();
   if (fr == FR_NO_FILESYSTEM) {
      volumes();
      end_line();
   }
}
