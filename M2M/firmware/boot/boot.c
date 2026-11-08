// Boot ROM of the RISC-V system CPU: loads the firmware (fw_image.h) from the active SD
// card into HyperRAM (load.c) and starts it. If it can't, it says why on the
// On-Screen-Menu and the serial port, and tries again when the cards change or Return
// is pressed. The core stays in reset; the firmware decides when it runs.
#include "console.h"
#include "load.h"
#include "m2m_rv.h"

#define CARD_BITS (M2M_CSR_SD_ACTIVE | M2M_CSR_SD_DET_INT | M2M_CSR_SD_DET_EXT)

static fw_block_t hdr;

static void show_error(const char *why)
{
   uart_puts("boot: ");
   uart_puts(why);
   uart_puts("\n");
   osm_clear(M2M_SA_COL_STD);
   osm_print(1, 1, M2M_SA_COL_TTLE, "MiSTer2MEGA65 RISC-V boot ROM");
   osm_print(1, 3, M2M_SA_COL_SEL, why);
   osm_print(1, 5, M2M_SA_COL_STD, "The menu's firmware is read from");
   osm_print(1, 6, M2M_SA_COL_STD, FW_PATH " on the SD card.");
   osm_print(1, 8, M2M_SA_COL_STD, "Change the card, or press Return");
   osm_print(1, 9, M2M_SA_COL_STD, "to try again.");
   osm_print(1, 11, M2M_SA_COL_STD, "Card in use:");
   osm_print(14, 11, M2M_SA_COL_STD, M2M_CSR & M2M_CSR_SD_ACTIVE ? "back slot" : "bottom tray");
   osm_print(1, 12, M2M_SA_COL_STD, "Hardware interface:");
   osm_hex(21, 12, M2M_SA_COL_STD, M2M_RV_HW_VERSION, 2);
   if (hdr.h.magic == FW_MAGIC) {
      osm_print(24, 12, M2M_SA_COL_STD, "firmware's:");
      osm_hex(36, 12, M2M_SA_COL_STD, hdr.h.hw_ver, 2);
   }
   M2M_CSR = M2M_CSR | M2M_CSR_OSM;
}

// Until the cards change, or Return goes down
static void wait_retry(void)
{
   uint32_t cards = M2M_CSR & CARD_BITS;
   int ret_up = 0;
   for (;;) {
      if ((M2M_CSR & CARD_BITS) != cards)
         break;
      if (M2M_KEYS & M2M_KEY_RETURN)
         ret_up = 1;
      else if (ret_up)
         break;
   }
   uint32_t t = MTIME_LO;           // let a card settle in its slot
   while (MTIME_LO - t < CPU_HZ / 2)
      ;
}

int main(void)
{
   uart_init(115200);
   trap_init();
   uart_puts("MiSTer2MEGA65 RISC-V boot ROM, hardware interface ");
   uart_hex(M2M_RV_HW_VERSION, 2);
   uart_puts("\n");

   // Always try: an R3 can't see a card in its bottom tray (sdmux.vhd)
   for (;;) {
      const char *why = fw_load(FW_PATH, &hdr);
      if (!why)
         break;
      if (why == fw_err_mount && !(M2M_CSR & (M2M_CSR_SD_DET_INT | M2M_CSR_SD_DET_EXT)))
         why = "No SD card";
      show_error(why);
      wait_retry();
   }

   hdr.h.version[sizeof hdr.h.version - 1] = 0;
   uart_puts("boot: firmware ");
   uart_puts(hdr.h.version);
   uart_puts("\n");
   M2M_CSR = M2M_CSR & ~M2M_CSR_OSM;
   asm volatile ("fence.i");
   ((void (*)(void))hdr.h.entry)();
   for (;;)
      ;
}
