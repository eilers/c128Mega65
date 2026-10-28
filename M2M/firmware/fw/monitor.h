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
