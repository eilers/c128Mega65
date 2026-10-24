#pragma once
#include <stdbool.h>
#include <stdint.h>

// The C128 side: the emulated 6551 behind two FIFOs.
int      acia_rx_free(void);              // room for bytes to the C128
void     acia_put(uint8_t c);             // one byte to the C128 (check acia_rx_free first)
int      acia_get(void);                  // next byte from the C128, or -1
bool     acia_dtr(void);                  // C128 has DTR on
bool     acia_enabled(void);              // switched on in the menu
uint32_t acia_baud(void);                 // rate the C128 has set
void     acia_set_lines(bool dcd, bool dsr, bool cts);
uint32_t acia_events(void);               // ACIA_EV_* since the last call
