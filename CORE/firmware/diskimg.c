// Empty D64, D71 and D81 images: Gideon's FileSystemD64/D71/D81::format (filesystem/
// filesystem_d64.cc) at 4b00732, and his create_file_of_size's zeros.
// Ported from the 1541 Ultimate firmware (github.com/GideonZ/1541ultimate), Copyright
// Gideon Zweijtzer and contributors, GPL v3 as this core; changed for c128Mega65 in 2026.
#include <ctype.h>
#include <string.h>
#include "diskimg.h"
#include "heap.h"

#define ZERO_STEP 4096

const uint32_t diskimg_bytes[3] = { 683 * 256, 683 * 2 * 256, 3200 * 256 };

// Tracks of a sector count each, from track 1; -1 for the rest
static const int layout_d64[] = { 17, 21, 7, 19, 6, 18, -1, 17 };
static const int layout_d71[] = { 17, 21, 7, 19, 6, 18, 5, 17, 17, 21, 7, 19, 6, 18, -1, 17 };
static const int layout_d81[] = { -1, 40 };
static const int *const layouts[3] = { layout_d64, layout_d71, layout_d81 };

// --- his FileSystemCBM, as format needs it ------------------------------------------------

// get_abs_sector: the sectors of the tracks before it, then the sector
static int abs_sector(const int *m, int track, int sector)
{
   int abs = sector;
   for (--track; m[0] >= 1 && track >= m[0]; m += 2) {
      abs += m[0] * m[1];
      track -= m[0];
   }
   return abs + track * m[1];
}

static void track_sector(const int *m, int abs, int *track, int *sector)
{
   *track = 1;
   for (;;) {
      int t = abs / m[1];
      if (m[0] < 1 || abs < m[0] * m[1]) {
         *track += t;
         abs -= t * m[1];
         break;
      }
      *track += m[0];
      abs -= m[0] * m[1];
      m += 2;
   }
   *sector = abs;
}

// modify_allocation_bit: a free or a used sector in a track's count and bits
static void allocation(uint8_t *fr, uint8_t *bits, int sector, int alloc)
{
   int n = sector >> 3, b = sector & 7;
   if (!alloc) {
      if (bits[n] & 1 << b)
         return;
      bits[n] |= 1 << b;
      (*fr)++;
   } else {
      if (!(bits[n] & 1 << b))
         return;
      bits[n] &= ~(1 << b);
      if (*fr)
         (*fr)--;
   }
}

static void set_allocation(diskimg_t *d, int track, int sector, int alloc)
{
   uint8_t *fr, *m;
   if (d->kind == DISK_D64) {
      fr = &d->root[4 * track];
      m = fr + 1;
   } else if (d->kind == DISK_D71) {
      if (track > 35) {
         fr = &d->root[0xDD + track - 36];
         m = &d->bam[3 * (track - 36)];
      } else {
         fr = &d->root[4 * track];
         m = fr + 1;
      }
   } else {
      fr = &d->bam[track > 40 ? 0x110 + 6 * (track - 41) : 0x10 + 6 * (track - 1)];
      m = fr + 1;
   }
   allocation(fr, m, sector, alloc);
}

// set_volume_name: 27 bytes of $A0, the DOS type at 21, up to 16 characters of the name and
// 2 of an id after a ',', in capitals
static void volume_name(const char *name, uint8_t *bam_name, const char *dos)
{
   memset(bam_name, 0xA0, 27);
   bam_name[21] = dos[0];
   bam_name[22] = dos[1];
   const char *p = name;
   for (int t = 0; t < 16 && *p && *p != ','; t++)
      bam_name[t] = toupper((unsigned char)*p++);
   while (*p && *p != ',')
      p++;
   if (*p == ',') {
      p++;
      for (int t = 0; t < 2 && p[t]; t++)
         bam_name[18 + t] = toupper((unsigned char)p[t]);
   }
}

// FileSystemD64/D71/D81::format
void diskimg_format(diskimg_t *d, int kind, const char *name)
{
   static const int sectors[] = { 683, 683 * 2, 3200 };
   uint8_t *root = d->root, *bam = d->bam;
   int t, s;
   d->kind = kind;
   memset(root, 0, 256);
   memset(bam, 0, 512);
   memset(d->dir, 0, 256);
   d->dir[1] = 0xFF;
   root[0] = kind == DISK_D81 ? 40 : 18;
   root[1] = kind == DISK_D81 ? 3 : 1;
   root[2] = kind == DISK_D81 ? 0x44 : 0x41;
   if (kind != DISK_D81)
      root[3] = kind == DISK_D71 ? 0x80 : 0x20;
   volume_name(name, kind == DISK_D81 ? root + 4 : root + 144, kind == DISK_D81 ? "3D" : "2A");
   for (int i = 0; i < sectors[kind]; i++) {
      track_sector(layouts[kind], i, &t, &s);
      set_allocation(d, t, s, 0);
   }
   if (kind == DISK_D64) {
      set_allocation(d, 18, 0, 1);
      set_allocation(d, 18, 1, 1);
   } else if (kind == DISK_D71) {
      set_allocation(d, 18, 0, 1);
      set_allocation(d, 18, 1, 1);
      set_allocation(d, 18, 1, 1);
      for (int i = 0; i < 19; i++)
         set_allocation(d, 53, i, 1);
   } else {
      for (int i = 0; i < 4; i++)
         set_allocation(d, 40, i, 1);
      bam[0] = 40;
      bam[1] = 2;
      bam[2] = 0x44;
      bam[3] = 0xBB;
      bam[4] = root[22];
      bam[5] = root[23];
      bam[6] = 0xC0;
      memcpy(bam + 256, bam, 6);
      bam[256] = 0x00;
      bam[257] = 0xFF;
   }
}

static FRESULT put_sectors(FIL *f, const int *layout, int track, int sector, const uint8_t *d, int n)
{
   UINT put;
   FRESULT res = f_lseek(f, (FSIZE_t)abs_sector(layout, track, sector) * 256);
   if (res == FR_OK)
      res = f_write(f, d, 256 * n, &put);
   return res == FR_OK && put != (UINT)(256 * n) ? FR_DISK_ERR : res;
}

// sync: the directory block, the header (root), and the D71's or D81's BAM blocks
FRESULT diskimg_write(const diskimg_t *d, FIL *f)
{
   const int *l = layouts[d->kind];
   int d81 = d->kind == DISK_D81;
   FRESULT res = put_sectors(f, l, d81 ? 40 : 18, d81 ? 3 : 1, d->dir, 1);
   if (res == FR_OK)
      res = put_sectors(f, l, d81 ? 40 : 18, 0, d->root, 1);
   if (res == FR_OK && d->kind == DISK_D71)
      res = put_sectors(f, l, 53, 0, d->bam, 1);
   if (res == FR_OK && d81)
      res = put_sectors(f, l, 40, 1, d->bam, 2);
   return res;
}

// In one piece if the card has room for that: a drive takes no image split under 4 KB, which
// a card of small clusters can do to a file written as it comes
void diskimg_expand(FIL *f, uint32_t bytes)
{
   f_expand(f, bytes, 1);
}

FRESULT diskimg_create(const char *path, int kind, const char *name)
{
   static uint8_t *zeros;
   static diskimg_t *img;
   FIL f;
   UINT put;
   if (!zeros) {
      zeros = heap_alloc(ZERO_STEP);
      img = heap_alloc(sizeof *img);
   }
   FRESULT res = f_open(&f, path, FA_WRITE | FA_CREATE_NEW);
   if (res != FR_OK)
      return res;
   diskimg_expand(&f, diskimg_bytes[kind]);
   for (uint32_t left = diskimg_bytes[kind]; res == FR_OK && left; left -= put) {
      uint32_t n = left < ZERO_STEP ? left : ZERO_STEP;
      res = f_write(&f, zeros, n, &put);
      if (res == FR_OK && put != n)
         res = FR_DENIED;                  // the card is full
   }
   if (res == FR_OK) {
      diskimg_format(img, kind, name);
      res = diskimg_write(img, &f);
   }
   FRESULT cl = f_close(&f);
   if (res == FR_OK)
      res = cl;
   if (res != FR_OK)
      f_unlink(path);
   return res;
}
