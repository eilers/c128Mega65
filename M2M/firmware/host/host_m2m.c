// The hardware as test_shell's firmware sees it: registers in an array, each device of
// the device bus a lazily mapped 2^28-word area, HyperRAM and SDRAM others, and the CLINT timer
// running on a little with every read.
#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include "host_m2m.h"
#include "m2m_rv.h"

#define DEV_WORDS (1u << 28)

volatile uint16_t *host_hr;
volatile uint16_t *host_sr;
uint32_t           host_regs[0x80];      // 0xE0000000 to 0xE00001FF
uint64_t           host_time;
static uint32_t    other;
static volatile uint16_t *devs[0x200];

static void *area(size_t bytes)
{
   void *p = mmap(NULL, bytes, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
   if (p == MAP_FAILED) {
      perror("mmap");
      exit(2);
   }
   return p;
}

volatile uint32_t *host_reg32(uint32_t a)
{
   static uint32_t t;
   if (a >= 0xE0000000u && a < 0xE0000200u)
      return &host_regs[(a - 0xE0000000u) / 4];
   if (a == 0xFFF4BFF8u || a == 0xFFF4BFFCu) {
      host_time += 1000;                 // 10 us a read
      t = a == 0xFFF4BFF8u ? (uint32_t)host_time : (uint32_t)(host_time >> 32);
      return &t;
   }
   other = 1u << 19;                     // UART0: room in the TX FIFO
   return &other;
}

void (*host_dev_hook)(void);
void (*host_dev_hw)(void);

volatile uint16_t *host_dev_at(uint32_t d)
{
   if (!devs[d])
      devs[d] = area(DEV_WORDS * 2);
   return devs[d];
}

volatile uint16_t *host_dev(void)
{
   if (host_dev_hook)
      host_dev_hook();
   if (host_dev_hw)
      host_dev_hw();
   return host_dev_at(host_regs[7] & 0x1FF);
}

void host_init(void)
{
   host_hr = area((1u << 23) * 2);
   host_sr = area((1u << 25) * 2);
   host_regs[0] = 0x0839;                // C_CSR_DEFAULT: in reset, keyboard and joysticks on
   host_regs[6] = 0xFFFF;                // no key held
}
