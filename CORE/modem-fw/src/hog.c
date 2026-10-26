// Loads HyperRAM the way the modem can, to see what it does to HDMI's scaler.
// Mode 1: 64-word bursts (i-cache line fills). Mode 2: 4-word reads (constants).
#include "hog.h"
#include "hw.h"
#include "trap.h"
#include "util.h"

#define SWEEP_LINES  256   // 16 KB of code in 64-byte lines: twice the i-cache

// Straight-line code too big for the i-cache, so every line is a miss
static void __attribute__((noinline)) sweep(void)
{
   asm volatile(".option push\n .option norvc\n .rept 4096\n nop\n .endr\n .option pop");
}

uint32_t hog(int mode, uint32_t seconds)
{
   const volatile uint32_t *image = (const volatile uint32_t *)IMAGE_BASE;
   uint32_t reads = 0;
   uint32_t start = millis();
   while (millis() - start < seconds * 1000) {
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
