// Firmware of the RISC-V system CPU, loaded from the SD card by the boot ROM and run from
// HyperRAM. A main loop polls each module in turn, and none of them waits for anything,
// as in the QNICE Shell. The core stays in reset for now (step 4, the firmware base).
#include "browse.h"
#include "cli.h"
#include "console.h"
#include "fmt.h"
#include "keys.h"
#include "m2m_rv.h"
#include "sdcard.h"
#include "version.h"

int main(void)
{
   uart_init(115200);
   trap_init();
   uart_printf("MiSTer2MEGA65 RISC-V firmware %s, hardware interface %u\n", FW_VERSION,
               M2M_RV_HW_VERSION);

   osm_clear(M2M_SA_COL_STD);
   M2M_CSR = M2M_CSR | M2M_CSR_OSM;
   for (;;) {
      keys_poll();
      browse_poll();      // first, so the screen is up before the card's first mount
      sdcard_poll();
      cli_poll();
   }
}
