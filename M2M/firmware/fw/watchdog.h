// The main loop's watchdog. A round of the main loop, or a sector of long disk work, has to
// come at least once a second; otherwise the timer interrupt stops the firmware on trap.c's
// screen, marked "stuck", with the address it was stuck at.
#pragma once

void watchdog_start(void);
void watchdog_kick(void);
