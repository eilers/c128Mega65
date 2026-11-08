// Firmware of the RISC-V system CPU, loaded from the SD card by the boot ROM and run from
// HyperRAM. A main loop polls each module in turn, and none of them waits for anything,
// as in the QNICE Shell.
#include "console.h"
#include "core.h"
#include "dmem.h"
#include "fmt.h"
#include "keys.h"
#include "m2m_rv.h"
#include "monitor.h"
#include "sdcard.h"
#include "shell.h"
#include "version.h"
#include "watchdog.h"

struct mon_loop mon_loop;
const char *const mon_module[MON_MODULES] = { "keys", "sdcard", "shell", "core", "net", "monitor" };

// The call that ended now, which started at *t
DMEM_CODE static void timed(int m, uint32_t *t)
{
   uint32_t now = MTIME_LO;
   if (now - *t > mon_loop.max[m])
      mon_loop.max[m] = now - *t;
   *t = now;
}

// FatFs's disk layer, at every sector: long disk work keeps the watchdog and the network
// going
void disk_idle(void)
{
   watchdog_kick();
   core_idle();
}

DMEM_CODE int main(void)
{
   uart_init(2000000);   // as the MEGA65's own tools use; the boot ROM sends at 115200
   trap_init();
   uart_printf("MiSTer2MEGA65 RISC-V firmware %s, hardware interface %u\n", FW_VERSION,
               M2M_RV_HW_VERSION);
   monitor_init();

   shell_start();
   watchdog_start();
   for (;;) {
      uint32_t start = MTIME_LO, t = start;
      watchdog_kick();
      keys_poll();
      timed(0, &t);
      sdcard_poll();
      timed(1, &t);
      shell_poll();
      timed(2, &t);
      core_poll();
      timed(3, &t);
      core_net();
      timed(4, &t);
      monitor_poll();
      timed(5, &t);

      int b = 0;
      for (uint32_t lim = CPU_HZ / 100000; b < MON_BUCKETS - 1 && t - start >= lim; lim *= 10)
         b++;
      mon_loop.hist[b]++;
      mon_loop.rounds++;
   }
}
