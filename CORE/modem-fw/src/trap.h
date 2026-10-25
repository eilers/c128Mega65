#pragma once
#include <stdint.h>

// Traps restart the firmware; a timer interrupt is the watchdog (trap.c)
void trap_init(void);        // first thing in main
void trap_report(void);      // after modem_init: tell the C128 about a restart
uint32_t trap_restarts(void);    // since power-up
void watchdog_start(void);
void watchdog_kick(void);    // at least once a second
