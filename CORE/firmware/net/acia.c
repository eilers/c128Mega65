// The C128's side: the emulated 6551 behind two FIFOs (net_hw.h)
#include "acia.h"
#include "net_hw.h"

// SwiftLink rates: its 3.6864 MHz crystal doubles the 6551's table. Rate 0 is
// the Turbo232 extension, picked by the register at base+7.
static const uint32_t rates[16] = {
   0, 100, 150, 220, 269, 300, 600, 1200, 2400, 3600, 4800, 7200, 9600, 14400, 19200, 38400
};
static const uint32_t turbo_rates[4] = { 230400, 115200, 57600, 28800 };

int acia_rx_free(void)
{
   return (int)(ACIA_RX & 0xFF);
}

void acia_put(uint8_t c)
{
   ACIA_RX = c;
}

int acia_get(void)
{
   uint32_t v = ACIA_TX;
   return (v & 0x100) ? (int)(v & 0xFF) : -1;
}

bool acia_dtr(void)
{
   return ACIA_CMD & 1;
}

bool acia_enabled(void)
{
   return (ACIA_STATE & ACIA_ST_ENABLED) != 0;
}

uint32_t acia_baud(void)
{
   uint32_t i = (ACIA_CMD >> 8) & 0x0F;
   return i ? rates[i] : turbo_rates[ACIA_STATE & 3];
}

void acia_set_lines(bool dcd, bool dsr, bool cts)
{
   ACIA_LINES = (dcd ? ACIA_LN_DCD : 0) | (dsr ? ACIA_LN_DSR : 0) | (cts ? ACIA_LN_CTS : 0);
}

uint32_t acia_events(void)
{
   uint32_t e = ACIA_EVENTS;
   if (e)
      ACIA_EVENTS = e;
   return e;
}

uint32_t acia_agains(void)
{
   return ACIA_AGAINS;
}
