// Trap handler: a bus error or a bad instruction stops here, with the cause on the
// serial port and in the bottom row of the On-Screen-Menu, on the whole screen so that
// the row shows whatever window was open.
#include "console.h"
#include "m2m_rv.h"

static void __attribute__((interrupt("machine"), aligned(4))) trap_handler(void)
{
   uint32_t cause, epc, tval;
   asm volatile ("csrr %0, mcause" : "=r"(cause));
   asm volatile ("csrr %0, mepc" : "=r"(epc));
   asm volatile ("csrr %0, mtval" : "=r"(tval));

   uart_puts("\ntrap: mcause ");
   uart_hex(cause, 8);
   uart_puts(" mepc ");
   uart_hex(epc, 8);
   uart_puts(" mtval ");
   uart_hex(tval, 8);
   uart_puts("\n");

   M2M_OSM_XY   = 0;
   M2M_OSM_DXDY = OSM_DX << 8 | OSM_DY;
   M2M_CSR      = M2M_CSR | M2M_CSR_OSM;
   osm_print(0, OSM_DY - 1, M2M_SA_COL_SEL, "trap");
   osm_hex(5, OSM_DY - 1, M2M_SA_COL_SEL, cause, 8);
   osm_hex(14, OSM_DY - 1, M2M_SA_COL_SEL, epc, 8);
   osm_hex(23, OSM_DY - 1, M2M_SA_COL_SEL, tval, 8);
   for (;;)
      ;
}

void trap_init(void)
{
   asm volatile ("csrw mtvec, %0" :: "r"(trap_handler));
}
