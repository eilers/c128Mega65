// FatFs's disk layer: one drive, the active SD card (lib/sd.c).
#include "ff.h"
#include "diskio.h"
#include "sd.h"

static DSTATUS status = STA_NOINIT;

// Called at every sector; the firmware has its own (fw/main.c)
__attribute__((weak)) void disk_idle(void)
{
}

DSTATUS disk_status(BYTE pdrv)
{
   return pdrv ? STA_NOINIT : status;
}

// FatFs calls this at every mount, so a mount after a card change starts the card afresh
DSTATUS disk_initialize(BYTE pdrv)
{
   if (pdrv)
      return STA_NOINIT;
   status = sd_init() ? STA_NOINIT : 0;
   return status;
}

DRESULT disk_read(BYTE pdrv, BYTE *buff, LBA_t sector, UINT count)
{
   if (pdrv || (status & STA_NOINIT))
      return RES_NOTRDY;
   for (; count; count--, sector++, buff += 512) {
      if (sd_read(sector, buff))
         return RES_ERROR;
      disk_idle();
   }
   return RES_OK;
}

#if !FF_FS_READONLY
DRESULT disk_write(BYTE pdrv, const BYTE *buff, LBA_t sector, UINT count)
{
   if (pdrv || (status & STA_NOINIT))
      return RES_NOTRDY;
   for (; count; count--, sector++, buff += 512) {
      if (sd_write(sector, buff))
         return RES_ERROR;
      disk_idle();
   }
   return RES_OK;
}
#endif

DRESULT disk_ioctl(BYTE pdrv, BYTE cmd, void *buff)
{
   (void)buff;
   if (pdrv)
      return RES_PARERR;
   return cmd == CTRL_SYNC ? RES_OK : RES_PARERR;   // writes finish before sd_write returns
}
