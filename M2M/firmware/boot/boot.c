// Boot ROM of the RISC-V system CPU. For now a bring-up check: a banner on the
// On-Screen-Menu and the serial port, the SD card's first block, and the keys, live.
// The core stays in reset.
#include "console.h"
#include "m2m_rv.h"

static void sd_check(void)
{
   M2M_SD_CSR = 0;                         // (re)initialise the card
   while (M2M_SD_CSR & 0x8000)
      ;
   M2M_SD_ADDR_LO = 0;
   M2M_SD_ADDR_HI = 0;
   M2M_SD_CSR = 1;                         // read block 0
   while (M2M_SD_CSR & 0x8000)
      ;
   uint32_t csr = M2M_SD_CSR;
   osm_hex(11, 5, M2M_SA_COL_SEL, csr, 4);
   osm_hex(16, 5, M2M_SA_COL_SEL, M2M_SD_ERROR, 4);
   osm_hex(21, 5, M2M_SA_COL_SEL, M2M_SD_STATUS, 4);
   if (csr & 0x4000) {
      osm_print(26, 5, M2M_SA_COL_SEL, "error");
      return;
   }
   M2M_SD_POS = 510;
   uint32_t sig = M2M_SD_DATA;
   M2M_SD_POS = 511;
   sig |= M2M_SD_DATA << 8;
   osm_print(26, 5, M2M_SA_COL_SEL, sig == 0xAA55 ? "55AA ok" : "no 55AA");
}

int main(void)
{
   uart_init(115200);
   trap_init();
   uart_puts("MiSTer2MEGA65 RISC-V boot ROM\n");

   osm_clear(M2M_SA_COL_STD);
   osm_print(1, 1, M2M_SA_COL_TTLE, "MiSTer2MEGA65 RISC-V boot ROM");
   osm_print(1, 3, M2M_SA_COL_STD, "Keys");
   osm_print(1, 4, M2M_SA_COL_STD, "CSR");
   osm_print(1, 5, M2M_SA_COL_STD, "SD block 0");
   osm_print(1, 6, M2M_SA_COL_STD, "Seconds");
   M2M_CSR = M2M_CSR | M2M_CSR_OSM;

   sd_check();
   for (;;) {
      osm_hex(11, 3, M2M_SA_COL_SEL, ~M2M_KEYS & 0xFFFF, 4);
      osm_hex(11, 4, M2M_SA_COL_SEL, M2M_CSR, 4);
      osm_hex(11, 6, M2M_SA_COL_SEL, MTIME_LO / CPU_HZ, 4);
      if (!(M2M_KEYS & M2M_KEY_RETURN))
         sd_check();
   }
}
