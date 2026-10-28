// Traps. NEORV32's own handler halts the CPU, which leaves the C128 with a dead modem until
// the MEGA65 is switched off. This one notes what happened and starts the firmware again;
// the next boot tells the C128 and the log. A timer interrupt is the watchdog.
#include "hw.h"
#include "log.h"
#include "modem.h"
#include "trap.h"
#include "util.h"

#define TRAP_NEW  0x50415254   // a trap not reported yet
#define TRAP_SEEN 0x5345454E
#define STR_(x)   #x
#define STR(x)    STR_(x)

#define MTIMECMP_LO REG32(0xFFF44000u)
#define MTIMECMP_HI REG32(0xFFF44004u)

// In memory that crt0 leaves alone, so it survives the restart. A fault can repeat many
// times before a restart gets anywhere, so the first one is kept apart from the latest.
struct trap_rec {
   uint32_t magic;                            //  0
   uint32_t cause, pc, val;                   //  4: latest
   uint32_t first_cause, first_pc, first_val; // 16: first since the last report
   uint32_t count;                            // 28: traps since the last report
   uint32_t total;                            // 32: restarts since power-up
};
__attribute__((section(".noinit"))) struct trap_rec trap_rec;

static struct trap_rec seen;   // what trap_report tells the C128

__attribute__((naked, aligned(4))) static void trap_entry(void)
{
   asm volatile(
      "la   t0, trap_rec\n"
      "lw   t1, 28(t0)\n"
      "bnez t1, 1f\n"
      "csrr t2, mcause\n"
      "sw   t2, 16(t0)\n"
      "csrr t2, mepc\n"
      "sw   t2, 20(t0)\n"
      "csrr t2, mtval\n"
      "sw   t2, 24(t0)\n"
      "1:\n"
      "addi t1, t1, 1\n"
      "sw   t1, 28(t0)\n"
      "lw   t1, 32(t0)\n"
      "addi t1, t1, 1\n"
      "sw   t1, 32(t0)\n"
      "csrr t1, mcause\n"
      "sw   t1, 4(t0)\n"
      "csrr t1, mepc\n"
      "sw   t1, 8(t0)\n"
      "csrr t1, mtval\n"
      "sw   t1, 12(t0)\n"
      "li   t1, " STR(TRAP_NEW) "\n"
      "sw   t1, 0(t0)\n"
      "j    _start\n");
}

void trap_init(void)
{
   asm volatile("csrw mtvec, %0" :: "r"(trap_entry));
   if (trap_rec.magic == TRAP_NEW) {
      seen = trap_rec;
      log_printf("restart after %u traps: first mcause %x pc %x val %x, last mcause %x pc %x val %x",
                 seen.count, seen.first_cause, seen.first_pc, seen.first_val,
                 seen.cause, seen.pc, seen.val);
   } else if (trap_rec.magic != TRAP_SEEN) {
      trap_rec.total = 0;                     // power-up: the record is noise
   }
   trap_rec.count = 0;
   trap_rec.magic = TRAP_SEEN;
}

void trap_report(void)
{
   char buf[120];
   if (seen.count == 1)
      fmt(buf, sizeof buf, "\r\nMODEM RESTARTED: CAUSE %x AT %x VALUE %x\r\n",
          seen.cause, seen.pc, seen.val);
   else if (seen.count > 1)
      fmt(buf, sizeof buf, "\r\nMODEM RESTARTED AFTER %u TRAPS: FIRST CAUSE %x AT %x VALUE %x,"
          " LAST CAUSE %x AT %x\r\n", seen.count, seen.first_cause, seen.first_pc, seen.first_val,
          seen.cause, seen.pc);
   else
      return;
   modem_say(buf);
}

uint32_t trap_restarts(void)
{
   return trap_rec.total;
}

// The main loop kicks it; if a second goes by without, the timer interrupt is a trap
void watchdog_kick(void)
{
   uint32_t hi, lo;
   do {
      hi = MTIME_HI;
      lo = MTIME_LO;
   } while (hi != MTIME_HI);
   uint64_t t = ((uint64_t)hi << 32 | lo) + CPU_HZ;
   MTIMECMP_HI = 0xFFFFFFFFu;
   MTIMECMP_LO = (uint32_t)t;
   MTIMECMP_HI = (uint32_t)(t >> 32);
}

void watchdog_start(void)
{
   watchdog_kick();
   asm volatile("csrs mie, %0" :: "r"(1u << 7));   // machine timer interrupt
   asm volatile("csrsi mstatus, 8");               // interrupts on
}
