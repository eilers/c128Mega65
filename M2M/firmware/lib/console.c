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

// At most width characters of s, then spaces up to width; width < 0: all of s
static void osm_put(int x, int y, uint8_t attr, const char *s, int width)
{
   int pos = y * OSM_DX + x;
   int n = 0;
   M2M_RAMROM_DEV = M2M_DEV_VRAM_DATA;
   for (; *s && n != width; s++, n++)
      M2M_DEV16[pos + n] = (uint8_t)*s;
   for (; n < width; n++)
      M2M_DEV16[pos + n] = ' ';
   M2M_RAMROM_DEV = M2M_DEV_VRAM_ATTR;
   for (int i = 0; i < n; i++)
      M2M_DEV16[pos + i] = attr;
}

void osm_print(int x, int y, uint8_t attr, const char *s)
{
   osm_put(x, y, attr, s, -1);
}

void osm_printn(int x, int y, uint8_t attr, const char *s, int width)
{
   osm_put(x, y, attr, s, width);
}

// The menu font's box characters (M2M/rom/sysdef.asm M2M$FC_* and M2M$NC_*)
static void osm_row(int x, int y, uint8_t attr, int dx, uint8_t l, uint8_t m, uint8_t r)
{
   char row[OSM_DX + 1];
   row[0] = l;
   for (int i = 1; i < dx - 1; i++)
      row[i] = m;
   row[dx - 1] = r;
   row[dx] = 0;
   osm_put(x, y, attr, row, -1);
}

void osm_frame(int x, int y, int dx, int dy, uint8_t attr)
{
   osm_row(x, y, attr, dx, 201, 205, 187);
   for (int i = 1; i < dy - 1; i++)
      osm_row(x, y + i, attr, dx, 186, ' ', 186);
   osm_row(x, y + dy - 1, attr, dx, 200, 205, 188);
}

void osm_hline(int x, int y, int dx, uint8_t attr)
{
   osm_row(x, y, attr, dx, 199, 196, 182);
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
// simulation print the text instead of sending it. Text the boot ROM still has to send
// goes out first.
void uart_init(uint32_t baud)
{
   uint32_t div = CPU_HZ / (2 * baud) - 1;
   while (UART0_CTRL & (1u << 31))      // sending
      ;
   UART0_CTRL = 0;
   UART0_CTRL = 1u | (1u << 1) | (div << 6);
}

void uart_putc(char c)
{
   while (!(UART0_CTRL & (1u << 19)))   // TX FIFO not full
      ;
   UART0_DATA = (uint8_t)c;
}

int uart_getc(void)
{
   if (!(UART0_CTRL & (1u << 16)))      // RX FIFO empty
      return -1;
   return UART0_DATA & 0xFF;
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
