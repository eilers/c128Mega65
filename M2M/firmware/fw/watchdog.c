#include "dmem.h"
#include "m2m_rv.h"
#include "watchdog.h"

#define TIMEOUT  CPU_HZ               // 1 s
#define RENEW    (CPU_HZ / 100)       // the deadline moves on at most every 10 ms
#define MIE_MTIE (1u << 7)

static uint32_t kicked;

DMEM_CODE void watchdog_kick(void)
{
   if (MTIME_LO - kicked < RENEW)
      return;
   uint32_t hi, lo;
   do {
      hi = MTIME_HI;
      lo = MTIME_LO;
   } while (hi != MTIME_HI);
   kicked = lo;
   uint64_t at = ((uint64_t)hi << 32 | lo) + TIMEOUT;
   MTIMECMP_HI = 0xFFFFFFFF;          // no early interrupt between the two halves
   MTIMECMP_LO = (uint32_t)at;
   MTIMECMP_HI = (uint32_t)(at >> 32);
}

void watchdog_start(void)
{
   kicked = MTIME_LO - RENEW;
   watchdog_kick();
   asm volatile ("csrs mie, %0" :: "r"(MIE_MTIE));
}
