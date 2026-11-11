// FatFs's disk layer on the host: a disk image file. disk_idle after every sector, as
// lib/diskio.c has it, where the firmware's network takes a turn (test_ftp's does too).
#include <stdio.h>
#include "ff.h"
#include "diskio.h"
#include "sd.h"

// lib/sd.c's log and mode for lib/sd_diag.c: a card in SD mode, 4-bit at 50 MHz, SDHC, that
// never failed
sd_log_t sd_log = { 0, 0, 0x30035 };
uint32_t host_sd_now = 0x30035;

uint32_t sd_mode(void)
{
   return host_sd_now;
}

static FILE *img;

__attribute__((weak)) void disk_idle(void)
{
}

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
   for (; count; count--, sector++, buff += 512) {
      if (fseek(img, (long)sector * 512, SEEK_SET) || fread(buff, 512, 1, img) != 1)
         return RES_ERROR;
      disk_idle();
   }
   return RES_OK;
}

#if !FF_FS_READONLY
DRESULT disk_write(BYTE pdrv, const BYTE *buff, LBA_t sector, UINT count)
{
   if (pdrv || !img)
      return RES_NOTRDY;
   for (; count; count--, sector++, buff += 512) {
      if (fseek(img, (long)sector * 512, SEEK_SET) || fwrite(buff, 512, 1, img) != 1)
         return RES_ERROR;
      disk_idle();
   }
   return RES_OK;
}
#endif

DRESULT disk_ioctl(BYTE pdrv, BYTE cmd, void *buff)
{
   (void)buff;
   return !pdrv && cmd == CTRL_SYNC && !fflush(img) ? RES_OK : RES_PARERR;
}
