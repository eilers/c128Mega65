// FatFs's disk layer on the host: a disk image file.
#include <stdio.h>
#include "ff.h"
#include "diskio.h"

static FILE *img;

int disk_file_open(const char *path)
{
   if (img)
      fclose(img);
   img = fopen(path, "r+b");
   return img != NULL;
}

DSTATUS disk_status(BYTE pdrv)
{
   return pdrv || !img ? STA_NOINIT : 0;
}

DSTATUS disk_initialize(BYTE pdrv)
{
   return disk_status(pdrv);
}

DRESULT disk_read(BYTE pdrv, BYTE *buff, LBA_t sector, UINT count)
{
   if (pdrv || !img)
      return RES_NOTRDY;
   if (fseek(img, (long)sector * 512, SEEK_SET) || fread(buff, 512, count, img) != count)
      return RES_ERROR;
   return RES_OK;
}

#if !FF_FS_READONLY
DRESULT disk_write(BYTE pdrv, const BYTE *buff, LBA_t sector, UINT count)
{
   if (pdrv || !img)
      return RES_NOTRDY;
   if (fseek(img, (long)sector * 512, SEEK_SET) || fwrite(buff, 512, count, img) != count)
      return RES_ERROR;
   return RES_OK;
}
#endif

DRESULT disk_ioctl(BYTE pdrv, BYTE cmd, void *buff)
{
   (void)buff;
   return !pdrv && cmd == CTRL_SYNC && !fflush(img) ? RES_OK : RES_PARERR;
}
