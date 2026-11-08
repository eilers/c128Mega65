// Loads HyperRAM the way the system CPU can, to see what it does to HDMI's scaler.
// Mode 1: i-cache line fills. Mode 2: single reads, as constants are read.
#include "hog.h"
#include "m2m_rv.h"
#include "net.h"
#include "watchdog.h"

#define SWEEP_LINES  256   // 16 KB of code in 64-byte lines: twice the i-cache

// Straight-line code too big for the i-cache, so every line is a miss
static void __attribute__((noinline)) sweep(void)
{
   asm volatile(".option push\n .option norvc\n .rept 4096\n nop\n .endr\n .option pop");
}

uint32_t hog(int mode, uint32_t seconds)
{
   const volatile uint32_t *image = (const volatile uint32_t *)M2M_FW_BASE;
   uint32_t reads = 0;
   uint32_t start = net_millis();
   while (net_millis() - start < seconds * 1000) {
      watchdog_kick();
      if (mode == 1) {
         sweep();
         reads += SWEEP_LINES;
      } else {
         for (int i = 0; i < 1024; i++)
            (void)image[i];   // no d-cache: each is its own HyperRAM read
         reads += 1024;
      }
   }
   return reads;
}
