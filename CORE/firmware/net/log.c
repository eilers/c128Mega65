#include <stdarg.h>
#include "fmt.h"
#include "log.h"
#include "net.h"

#define LOG_SIZE  1024   // power of 2

// What the network did lately; ATI2 prints it on the C128
static struct {
   uint32_t head;        // total bytes ever written
   char ring[LOG_SIZE];
} lg;

void log_printf(const char *f, ...)
{
   char line[96];
   va_list ap;
   va_start(ap, f);
   int n = fmt_snprintf(line, sizeof line, "%u ", net_millis() / 100);
   n += fmt_vsnprintf(line + n, sizeof line - n, f, ap);
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
