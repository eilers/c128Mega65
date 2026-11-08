// A model of reu_sd.vhd (host_reu_sd.h): on the device access after the firmware wrote its
// control word, the whole transfer at once
#include <string.h>
#include "ff.h"
#include "diskio.h"
#include "host_m2m.h"
#include "host_reu_sd.h"
#include "host_uci_model.h"

#define DEV      0x010A
#define IDLE     0x8000            // the control word while no start waits (the firmware writes 0 or 1)

struct reu_sd_model rs;

static void hw(void)
{
   volatile uint16_t *d = host_dev_at(DEV);
   static uint8_t b[512];

   if (d[9]) {                     // a drop
      d[9] = 0;
      rs.drops++;
   }
   if (d[1] & IDLE)
      return;
   int      save = d[1] & 1, failed = 0;
   uint32_t blk = d[2] | (uint32_t)d[3] << 16, n = d[4];
   uint32_t at = d[5] | (uint32_t)d[6] << 16, len = d[7] | (uint32_t)d[8] << 16;
   uint32_t room = um.reu_mem & 0x8000 ? 16u << 20 : 512u << 10;
   uint8_t *reu = (uint8_t *)(um.reu_mem & 0x8000 ? host_sr : host_hr) + (um.reu_mem & 0x1FFF) * 8192;

   if (rs.fail_at && !--rs.fail_at)
      failed = 1;
   for (uint32_t k = 0; k < n && !failed; k++) {
      uint32_t o = (at & ~1u) + k * 512;
      if (save) {
         for (int i = 0; i < 512; i++)
            b[i] = reu[(o + i) % room];
         failed = disk_write(0, b, blk + k, 1) != RES_OK;
      } else {
         failed = disk_read(0, b, blk + k, 1) != RES_OK;
         for (uint32_t i = 0; i < 512 && k * 512 + i < len && !failed; i++)
            reu[(o + i) % room] = b[i];
      }
      rs.blocks += !failed;
   }
   if (save)
      rs.saves++;
   else {
      rs.loads++;
      rs.drops++;
   }
   d[1] = IDLE | (failed ? 2 : 0);
}

void host_reu_sd(int on)
{
   volatile uint16_t *d = host_dev_at(DEV);
   memset(&rs, 0, sizeof rs);
   d[0] = on ? 0x5D52 : 0;
   d[1] = IDLE;
   host_dev_hw = on ? hw : 0;
}
