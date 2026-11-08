// Serial monitor: commands on the serial port that read and write memory, the registers
// and the core's devices while everything runs. tools/mon.py is its host side.
#pragma once
#include <stdbool.h>
#include <stdint.h>

// monitor_hw.c: the serial port and the CPU
void monitor_init(void);   // after trap_init
void monitor_poll(void);

// monitor.c: the commands, which host/test_mon.c runs on the Mac
void mon_start(void);      // the first prompt
void mon_input(char c);    // a character from the serial port
void mon_step(void);       // carries on with a read
bool mon_busy(void);       // a read is under way

// What the commands need from monitor_hw.c
void mon_putc(char c);
int  mon_room(void);       // characters mon_putc can still take
bool mon_access(uint32_t addr, int width, bool write, uint32_t *v);   // false: bus error
// n accesses to the units from addr on (width apart), and the CPU clocks they took
bool mon_time(uint32_t addr, int width, bool write, uint32_t v, uint32_t n, uint32_t *clocks);

#define MON_CLOCK_NS 10    // the CPU's clock period (m2m_rv.h CPU_HZ)

// The main loop's timing, kept by main.c for the lt command
#define MON_MODULES 6
#define MON_BUCKETS 7      // rounds under 10 us, 100 us, 1 ms, 10 ms, 100 ms, 1 s, longer
struct mon_loop {
   uint32_t max[MON_MODULES];   // each module's longest call, in CPU clocks
   uint32_t rounds;
   uint32_t hist[MON_BUCKETS];
};
extern struct mon_loop mon_loop;
extern const char *const mon_module[MON_MODULES];
