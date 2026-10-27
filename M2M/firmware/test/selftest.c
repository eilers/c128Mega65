// Self-test for CORE/sim/tb_rv_soc.vhd: runs from the boot ROM and checks each part of
// rv_soc.vhd against the testbench's models. Row 1 of the On-Screen-Menu ends up
// "PASS", or "FAIL" and the number of the first failed check.
#include "console.h"
#include "m2m_rv.h"

#define TB_DEV_RAM     0x0100u          // testbench: RAM with wait states
#define HR_TEST        (M2M_HYPERRAM + 0x700000u)

typedef int (*add_fn)(int);

static int failed;

static void check(int n, int ok)
{
   if (!ok && !failed) {
      failed = n;
      uart_puts("check ");
      uart_hex(n, 2);
      uart_puts(" failed\n");
   }
}

int main(void)
{
   uart_init(115200);
   trap_init();
   uart_puts("selftest\n");

   // 1-2: registers
   check(1, (M2M_CSR & 0x08FF) == 0x0839);
   M2M_CONTROL_D(3) = 0xDEADBEEF;
   M2M_CONTROL_M(7) = 0x12345678;
   check(2, M2M_CONTROL_D(3) == 0xDEADBEEF && M2M_CONTROL_M(7) == 0x12345678);

   // 3: keys (the testbench holds F1)
   check(3, (~M2M_KEYS & 0xFFFF) == M2M_KEY_F1);

   // 4-5: On-Screen-Menu RAM, 16- and 32-bit
   osm_clear(M2M_SA_COL_STD);
   osm_print(0, 0, M2M_SA_COL_TTLE, "SELFTEST");
   M2M_RAMROM_DEV = M2M_DEV_VRAM_DATA;
   check(4, M2M_DEV16[0] == 'S' && M2M_DEV16[7] == 'T' && M2M_DEV16[8] == ' ');
   check(5, M2M_DEV32[1] == ('L' | ('F' << 16)));

   // 6-7: a device with wait states
   M2M_RAMROM_DEV = TB_DEV_RAM;
   for (int i = 0; i < 8; i++)
      M2M_DEV32[i] = 0x10001u * i + 0xA0005000u;
   M2M_DEV16[5] = 0x1234;
   check(6, M2M_DEV16[4] == 0x5002 && M2M_DEV16[5] == 0x1234 && M2M_DEV16[6] == 0x5003);
   check(7, M2M_DEV32[7] == 0xA0075007u);

   // 8: SD card registers
   M2M_SD_ADDR_LO = 0x1A2B;
   M2M_SD_ADDR_HI = 0x3C4D;
   check(8, M2M_SD_ADDR_LO == 0x1A2B && M2M_SD_ADDR_HI == 0x3C4D);

   // 9-10: HyperRAM data: words, halves, bytes
   volatile uint32_t *w = (volatile uint32_t *)HR_TEST;
   volatile uint16_t *h = (volatile uint16_t *)HR_TEST;
   volatile uint8_t  *b = (volatile uint8_t *)HR_TEST;
   w[0] = 0x11223344;
   w[1] = 0x55667788;
   h[1] = 0xBEEF;
   b[1] = 0x5A;
   b[6] = 0xC3;
   check(9, w[0] == 0xBEEF5A44 && w[1] == 0x55C37788);
   check(10, h[0] == 0x5A44 && b[3] == 0xBE && b[4] == 0x88);

   // 11: code in HyperRAM through the i-cache: 100 additions over several cache lines
   volatile uint32_t *code = (volatile uint32_t *)(HR_TEST + 0x1000);
   for (int i = 0; i < 100; i++)
      code[i] = 0x00150513;             // addi a0, a0, 1
   code[100] = 0x00008067;              // ret
   asm volatile ("fence.i" ::: "memory");
   check(11, ((add_fn)(HR_TEST + 0x1000))(7) == 107);

   if (failed) {
      osm_print(0, 1, M2M_SA_COL_SEL, "FAIL");
      osm_hex(5, 1, M2M_SA_COL_SEL, failed, 2);
   } else {
      osm_print(0, 1, M2M_SA_COL_SEL, "PASS");
      uart_puts("PASS\n");
   }
   for (;;)
      ;
}
