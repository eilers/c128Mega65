// The active SD card, in 512-byte blocks, as QNICE's monitor/sd_library.asm does it.
// The controller's buffer is read and written a byte at a time through a cursor.
#include "sd.h"
#include "m2m_rv.h"

#define SD_CMD_RESET  0u
#define SD_CMD_READ   1u
#define SD_CMD_WRITE  2u
#define SD_BUSY       0x8000u
#define SD_ERROR      0x4000u
#define SD_TIMEOUT    (2 * CPU_HZ)   // 2 s; a card switch alone holds the controller 100 ms

sd_log_t sd_log;

uint32_t sd_mode(void)
{
   return (M2M_SD_STATUS & 0xFFFF) | ((M2M_SD_CSR >> 12) & 3) << 16;
}

static int logged(int err, uint32_t block)
{
   if (err) {
      sd_log.err   = err;
      sd_log.block = block;
   } else {
      sd_log.mode = sd_mode();
   }
   return err;
}

static int sd_wait(void)
{
   uint32_t start = MTIME_LO;
   while (M2M_SD_CSR & SD_BUSY)
      if (MTIME_LO - start > SD_TIMEOUT)
         return SD_ERR_TIMEOUT;
   if (M2M_SD_CSR & SD_ERROR) {
      int err = M2M_SD_ERROR;
      return err ? err : SD_ERR_OTHER;
   }
   return 0;
}

static int sd_command(uint32_t block, uint32_t cmd)
{
   M2M_SD_ADDR_LO = block & 0xFFFF;
   M2M_SD_ADDR_HI = block >> 16;
   M2M_SD_CSR = cmd;
   return sd_wait();
}

int sd_init(void)
{
   sd_log.err  = 0;
   sd_log.mode = 0;
   M2M_SD_CSR = SD_CMD_RESET;
   return logged(sd_wait(), SD_AT_INIT);
}

// Whole words where it can: a word to HyperRAM is one access, a byte is one too
int sd_read(uint32_t block, void *buf)
{
   int err = sd_wait();
   if (!err)
      err = sd_command(block, SD_CMD_READ);
   if (err)
      return logged(err, block);
   if (((uintptr_t)buf & 3) == 0) {
      uint32_t *w = buf;
      for (uint32_t i = 0; i < 512; i += 4) {
         uint32_t v = 0;
         for (uint32_t b = 0; b < 4; b++) {
            M2M_SD_POS = i + b;
            v |= M2M_SD_DATA << (8 * b);
         }
         *w++ = v;
      }
   } else {
      uint8_t *p = buf;
      for (uint32_t i = 0; i < 512; i++) {
         M2M_SD_POS = i;
         p[i] = M2M_SD_DATA;
      }
   }
   return logged(0, block);
}

int sd_write(uint32_t block, const void *buf)
{
   int err = sd_wait();
   if (err)
      return logged(err, block);
   const uint8_t *p = buf;
   for (uint32_t i = 0; i < 512; i++) {
      M2M_SD_POS = i;
      M2M_SD_DATA = p[i];
   }
   return logged(sd_command(block, SD_CMD_WRITE), block);
}
