#include <stdarg.h>
#include "log.h"
#include "util.h"

#define LOG_SIZE  1024   // power of 2
#define LOG_MAGIC 0x4C4F4721

// Survives a restart after a trap (trap.c), so ATI2 shows what came before it
static struct {
   uint32_t magic;
   uint32_t head;        // total bytes ever written
   char ring[LOG_SIZE];
} lg __attribute__((section(".noinit")));

void log_init(void)
{
   if (lg.magic != LOG_MAGIC) {
      lg.magic = LOG_MAGIC;
      lg.head = 0;
   }
}

void log_printf(const char *f, ...)
{
   char line[96];
   va_list ap;
   va_start(ap, f);
   int n = fmt(line, sizeof line, "%u ", millis() / 100);
   n += vfmt(line + n, sizeof line - n, f, ap);
   va_end(ap);
   if (n > (int)sizeof line - 2)
      n = sizeof line - 2;
   line[n++] = '\r';
   for (int i = 0; i < n; i++)
      lg.ring[lg.head++ & (LOG_SIZE - 1)] = line[i];
}

size_t log_copy(uint32_t *pos, char *out, size_t size)
{
   uint32_t oldest = lg.head > LOG_SIZE ? lg.head - LOG_SIZE : 0;
   uint32_t p = *pos;
   if (p < oldest) {
      // start at the first whole line
      p = oldest;
      if (p)
         while (p < lg.head && lg.ring[p++ & (LOG_SIZE - 1)] != '\r')
            ;
   }
   size_t n = 0;
   while (p < lg.head && n < size)
      out[n++] = lg.ring[p++ & (LOG_SIZE - 1)];
   *pos = p;
   return n;
}
