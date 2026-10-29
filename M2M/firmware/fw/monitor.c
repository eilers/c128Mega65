// The serial monitor's commands, one per line, numbers in hex:
//   rb|rh|rw ADDR [N]    read N bytes, halfwords or words (1 if no N)
//   wb|wh|ww ADDR V...   write them, one after the other
//   tr ADDR [N]          time N reads of the words from ADDR on (400 if no N)
//   tw ADDR V [N]        time N writes of V to them
//   lt                   the main loop's timing since the last lt
// ADDR is a CPU address, or DEV:WORD, word WORD of core device DEV (rb and wb: its low
// byte). A command is echoed, then come its lines, "ok" or "? why", and the prompt.
#include <string.h>
#include "monitor.h"

#define LINE_MAX       128   // mon.py sends up to about 110
#define DEV_BASE       0xC0000000u   // the device bus window (m2m_rv.h M2M_DEV16)
#define REG_RAMROM_DEV 0xE000001Cu   // the device it shows
#define NO_DEV         0xFFFFFFFFu

static char line[LINE_MAX + 1];
static int  len;
static bool after_cr;   // a LF right after a CR ends the same line

static struct {
   uint32_t addr;       // CPU address, or device word
   uint32_t dev;        // NO_DEV for a CPU address
   uint32_t n;          // still to read; 0: no read under way
   int      width;
} rd;

static const char reading[] = "";   // run's answer when mon_step carries on
static char       why_bus[] = "bus error at 00000000";

static void puts_(const char *s)
{
   for (; *s; s++)
      mon_putc(*s);
}

static char *hex(char *p, uint32_t v, int digits)
{
   while (digits--)
      *p++ = "0123456789ABCDEF"[(v >> (4 * digits)) & 15];
   return p;
}

static void dec(uint32_t v)
{
   char t[10];
   int n = 0;
   do
      t[n++] = '0' + v % 10;
   while (v /= 10);
   while (n)
      mon_putc(t[--n]);
}

static void end(const char *why)   // why = 0: ok
{
   if (why) {
      puts_("? ");
      puts_(why);
   } else {
      puts_("ok");
   }
   puts_("\r\n> ");
}

static const char *bus_error(uint32_t a)
{
   hex(why_bus + 13, a, 8);
   return why_bus;
}

// The device bus window shows dev until put back; the Shell sets it before each use
static bool dev_select(uint32_t dev, uint32_t *old)
{
   return dev == NO_DEV ||
          (mon_access(REG_RAMROM_DEV, 4, false, old) && mon_access(REG_RAMROM_DEV, 4, true, &dev));
}

static void dev_restore(uint32_t dev, uint32_t old)
{
   if (dev != NO_DEV)
      mon_access(REG_RAMROM_DEV, 4, true, &old);
}

// One unit; a device word is 16 bits, of which a byte access uses the low 8
static bool unit(uint32_t dev, uint32_t a, int width, bool write, uint32_t *v)
{
   if (dev == NO_DEV)
      return mon_access(a, width, write, v);
   bool ok = mon_access(DEV_BASE + 2 * a, 2, write, v);
   if (width == 1)
      *v &= 0xFF;
   return ok;
}

static uint32_t cpu_addr(uint32_t dev, uint32_t a)   // for a bus error
{
   return dev == NO_DEV ? a : DEV_BASE + 2 * a;
}

static bool parse_hex(const char *s, uint32_t *v)
{
   uint32_t x = 0;
   int n = 0;
   for (; *s; s++, n++) {
      char c = *s | 0x20;
      if (c >= '0' && c <= '9')
         x = x << 4 | (c - '0');
      else if (c >= 'a' && c <= 'f')
         x = x << 4 | (c - 'a' + 10);
      else
         return false;
   }
   *v = x;
   return n > 0 && n <= 8;
}

static bool parse_addr(char *s, uint32_t *dev, uint32_t *a)
{
   *dev = NO_DEV;
   for (char *p = s; *p; p++) {
      if (*p == ':') {
         *p = 0;
         return parse_hex(s, dev) && *dev <= 0xFFFF && parse_hex(p + 1, a) && *a < 1u << 28;
      }
   }
   return parse_hex(s, a);
}

// n units from a stay inside the CPU's address space, or the device's 2^28 words
static bool fits(uint32_t dev, uint32_t a, uint32_t n, uint32_t step)
{
   return (uint64_t)a + (uint64_t)n * step <= (dev == NO_DEV ? 1ull << 32 : 1ull << 28);
}

static const char *help(void)
{
   puts_("rb|rh|rw ADDR [N]   read N bytes, halfwords or words\r\n"
         "wb|wh|ww ADDR V...  write them\r\n"
         "ADDR: a CPU address, or DEV:WORD, a word of a device (rb, wb: its low byte)\r\n"
         "40000000 HyperRAM, C0000000 device bus, E0000000 registers\r\n"
         "Ctrl-C stops a read\r\n"
         "tr ADDR [N]         time N reads of the words from ADDR on (tw ADDR V [N]: writes)\r\n"
         "lt                  the main loop's timing since the last lt\r\n");
   return 0;
}

// N accesses (default 400), 32-bit words at a CPU address or a device's 16-bit words
static const char *time_access(bool write, char **tok, int n)
{
   uint32_t dev, a, v = 0, count = 0x400, clocks;
   int at = write ? 3 : 2;   // the count's place
   if (n < 2 || !parse_addr(tok[1], &dev, &a))
      return "address";
   if (write && (n < 3 || !parse_hex(tok[2], &v) || (dev != NO_DEV && v >> 16)))
      return "value";
   if (n > at + 1 || (n == at + 1 && (!parse_hex(tok[at], &count) || count == 0)))
      return "count";
   if (dev == NO_DEV && a % 4)
      return "alignment";
   if (!fits(dev, a, count, dev == NO_DEV ? 4 : 1))
      return "past the end";

   uint32_t old;
   if (!dev_select(dev, &old))
      return bus_error(REG_RAMROM_DEV);
   bool ok = mon_time(cpu_addr(dev, a), dev == NO_DEV ? 4 : 2, write, v, count, &clocks);
   dev_restore(dev, old);
   if (!ok)
      return "bus error";
   dec(count);
   puts_(write ? " writes in " : " reads in ");
   dec(clocks / (1000 / MON_CLOCK_NS));
   puts_(" us, ");
   dec((uint64_t)clocks * MON_CLOCK_NS / count);
   puts_(" ns each\r\n");
   return 0;
}

static const char *loop_times(void)
{
   static const char *const under[MON_BUCKETS] = {
      "10 us", "100 us", "1 ms", "10 ms", "100 ms", "1 s", "longer"
   };
   puts_("rounds ");
   dec(mon_loop.rounds);
   puts_("\r\nlongest call:");
   for (int i = 0; i < MON_MODULES; i++) {
      puts_(i ? ", " : " ");
      puts_(mon_module[i]);
      mon_putc(' ');
      dec(mon_loop.max[i] / (1000 / MON_CLOCK_NS));
      puts_(" us");
   }
   puts_("\r\nrounds under");
   for (int i = 0; i < MON_BUCKETS; i++) {
      puts_(i ? ", " : " ");
      puts_(under[i]);
      mon_putc(' ');
      dec(mon_loop.hist[i]);
   }
   puts_("\r\n");
   memset(&mon_loop, 0, sizeof mon_loop);
   return 0;
}

static const char *run(void)
{
   char *tok[LINE_MAX / 2 + 1];
   int n = 0;
   for (char *p = line; *p;) {
      while (*p == ' ')
         *p++ = 0;
      if (*p)
         tok[n++] = p;
      while (*p && *p != ' ')
         p++;
   }
   if (n == 0)
      return 0;

   char op = tok[0][0] | 0x20, w = tok[0][1] | 0x20;
   if ((op == 'h' || op == '?') && !tok[0][1])
      return help();
   if (op == 't' && (w == 'r' || w == 'w') && !tok[0][2])
      return time_access(w == 'w', tok, n);
   if (op == 'l' && w == 't' && !tok[0][2])
      return n == 1 ? loop_times() : "what? h for help";
   int width = w == 'b' ? 1 : w == 'h' ? 2 : w == 'w' ? 4 : 0;
   if ((op != 'r' && op != 'w') || !width || tok[0][2])
      return "what? h for help";

   uint32_t dev, a, count = 1;
   if (n < 2 || !parse_addr(tok[1], &dev, &a))
      return "address";
   if (dev != NO_DEV && width == 4)
      return "device words are 16 bits";
   if (dev == NO_DEV && a % width)
      return "alignment";
   uint32_t step = dev == NO_DEV ? width : 1;

   if (op == 'r') {
      if (n > 3 || (n == 3 && (!parse_hex(tok[2], &count) || count == 0)))
         return "count";
      if (!fits(dev, a, count, step))
         return "past the end";
      rd.addr  = a;
      rd.dev   = dev;
      rd.n     = count;
      rd.width = width;
      return reading;
   }

   uint32_t v[LINE_MAX / 2];
   if (n < 3)
      return "value";
   if (!fits(dev, a, n - 2, step))
      return "past the end";
   for (int i = 2; i < n; i++)
      if (!parse_hex(tok[i], &v[i - 2]) || (width < 4 && v[i - 2] >> (8 * width)))
         return "value";

   uint32_t old;
   if (!dev_select(dev, &old))
      return bus_error(REG_RAMROM_DEV);
   for (int i = 0; i < n - 2; i++, a += step) {
      if (!unit(dev, a, width, true, &v[i])) {
         dev_restore(dev, old);
         return bus_error(cpu_addr(dev, a));
      }
   }
   dev_restore(dev, old);
   return 0;
}

void mon_start(void)
{
   puts_("> ");
}

bool mon_busy(void)
{
   return rd.n != 0;
}

// One line of a read, when there's room to send it
void mon_step(void)
{
   if (!rd.n || mon_room() < 80)
      return;
   int per = rd.dev == NO_DEV ? 16 / rd.width : rd.width == 1 ? 16 : 8;
   char buf[80], *p = buf;
   if (rd.dev == NO_DEV) {
      p = hex(p, rd.addr, 8);
   } else {
      p = hex(p, rd.dev, 4);
      *p++ = ':';
      p = hex(p, rd.addr, 7);
   }
   *p++ = ' ';
   char *values = p;

   const char *why = 0;
   uint32_t old;
   if (!dev_select(rd.dev, &old)) {
      why = bus_error(REG_RAMROM_DEV);
   } else {
      for (int i = 0; i < per && rd.n; i++, rd.n--) {
         uint32_t v;
         if (!unit(rd.dev, rd.addr, rd.width, false, &v)) {
            why = bus_error(cpu_addr(rd.dev, rd.addr));
            break;
         }
         *p++ = ' ';
         p = hex(p, v, 2 * rd.width);
         rd.addr += rd.dev == NO_DEV ? rd.width : 1;
      }
      dev_restore(rd.dev, old);
   }
   if (p != values) {
      *p = 0;
      puts_(buf);
      puts_("\r\n");
   }
   if (why) {
      rd.n = 0;
      end(why);
   } else if (!rd.n) {
      end(0);
   }
}

void mon_input(char c)
{
   if (c == 3) {   // Ctrl-C
      if (rd.n) {
         rd.n = 0;
         end("stopped");
      } else {
         len = 0;
         puts_("^C\r\n> ");
      }
      return;
   }
   if (c == '\n' && after_cr) {
      after_cr = false;
      return;
   }
   after_cr = c == '\r';
   if (c == '\r' || c == '\n') {
      line[len] = 0;
      len = 0;
      puts_("\r\n");
      const char *why = run();
      if (why != reading)
         end(why);
   } else if (c == 8 || c == 127) {
      if (len) {
         len--;
         puts_("\b \b");
      }
   } else if (c >= ' ' && c < 127 && len < LINE_MAX) {
      line[len++] = c;
      mon_putc(c);
   }
}
