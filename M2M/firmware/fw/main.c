// Firmware of the RISC-V system CPU, loaded from the SD card by the boot ROM and run from
// HyperRAM. A main loop polls each module in turn, and none of them waits for anything,
// as in the QNICE Shell.
#include "console.h"
#include "core.h"
#include "fmt.h"
#include "keys.h"
#include "m2m_rv.h"
#include "monitor.h"
#include "sdcard.h"
#include "shell.h"
#include "version.h"

int main(void)
{
   uart_init(2000000);   // as the MEGA65's own tools use; the boot ROM sends at 115200
   trap_init();
   uart_printf("MiSTer2MEGA65 RISC-V firmware %s, hardware interface %u\n", FW_VERSION,
               M2M_RV_HW_VERSION);
   monitor_init();

   shell_start();
   for (;;) {
      keys_poll();
      sdcard_poll();
      shell_poll();
      core_poll();
      monitor_poll();
   }
}
