// Text on the On-Screen-Menu and the serial port.
#include "console.h"
#include "m2m_rv.h"

static void osm_fill(uint32_t dev, uint8_t v)
{
   M2M_RAMROM_DEV = dev;
   uint32_t pair = v | ((uint32_t)v << 16);
   for (int i = 0; i < OSM_DX * OSM_DY / 2; i++)
      M2M_DEV32[i] = pair;
   M2M_DEV16[OSM_DX * OSM_DY - 1] = v;
}

void osm_clear(uint8_t attr)
{
   osm_fill(M2M_DEV_VRAM_DATA, ' ');
   osm_fill(M2M_DEV_VRAM_ATTR, attr);
}

void osm_print(int x, int y, uint8_t attr, const char *s)
{
   int pos = y * OSM_DX + x;
   int n = 0;
   M2M_RAMROM_DEV = M2M_DEV_VRAM_DATA;
   for (const char *p = s; *p; p++, n++)
      M2M_DEV16[pos + n] = (uint8_t)*p;
   M2M_RAMROM_DEV = M2M_DEV_VRAM_ATTR;
   for (int i = 0; i < n; i++)
      M2M_DEV16[pos + i] = attr;
}

static void to_hex(char *buf, uint32_t v, int digits)
{
   for (int i = digits - 1; i >= 0; i--, v >>= 4)
      buf[i] = "0123456789ABCDEF"[v & 15];
   buf[digits] = 0;
}

void osm_hex(int x, int y, uint8_t attr, uint32_t v, int digits)
{
   char buf[9];
   to_hex(buf, v, digits);
   osm_print(x, y, attr, buf);
}

// NEORV32 UART0: prescaler 2, so the divisor is CPU_HZ / (2 * baud). Bit 1 makes a
// simulation print the text instead of sending it.
void uart_init(uint32_t baud)
{
   uint32_t div = CPU_HZ / (2 * baud) - 1;
   UART0_CTRL = 0;
   UART0_CTRL = 1u | (1u << 1) | (div << 6);
}

void uart_putc(char c)
{
   while (!(UART0_CTRL & (1u << 19)))   // TX FIFO not full
      ;
   UART0_DATA = (uint8_t)c;
}

void uart_puts(const char *s)
{
   for (; *s; s++) {
      if (*s == '\n')
         uart_putc('\r');
      uart_putc(*s);
   }
}

void uart_hex(uint32_t v, int digits)
{
   char buf[9];
   to_hex(buf, v, digits);
   uart_puts(buf);
}
