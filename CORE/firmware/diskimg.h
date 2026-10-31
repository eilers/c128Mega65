// Empty D64, D71 and D81 images, as Gideon's FileSystemD64/D71/D81::format: zeros, then the
// header and BAM blocks and the first directory block. For REST's create_d64/d71/d81 and
// the file browser's F5.
#pragma once
#include <stdint.h>
#include "ff.h"

enum { DISK_D64, DISK_D71, DISK_D81 };

typedef struct {
   int     kind;
   uint8_t root[256], bam[512], dir[256];
} diskimg_t;

extern const uint32_t diskimg_bytes[3];   // an image's size (a D64 of 35 tracks)

// The blocks of an empty image; name is "NAME,ID": up to 16 characters, then a 2-character id
void    diskimg_format(diskimg_t *d, int kind, const char *name);
// The blocks into an image file that holds zeros
FRESULT diskimg_write(const diskimg_t *d, FIL *f);
// An empty file's room for an image of bytes, in one piece where the card has one
void    diskimg_expand(FIL *f, uint32_t bytes);
// A whole new image at path, at once; never over another file (FR_EXIST), none left on an error
FRESULT diskimg_create(const char *path, int kind, const char *name);
