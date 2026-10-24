#include <stdarg.h>
#include "log.h"
#include "util.h"

#define LOG_SIZE 1024   // power of 2

static char ring[LOG_SIZE];
static uint32_t head;   // total bytes ever written

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
      ring[head++ & (LOG_SIZE - 1)] = line[i];
}

size_t log_copy(uint32_t *pos, char *out, size_t size)
{
   uint32_t oldest = head > LOG_SIZE ? head - LOG_SIZE : 0;
   uint32_t p = *pos;
   if (p < oldest) {
      // start at the first whole line
      p = oldest;
      if (p)
         while (p < head && ring[p++ & (LOG_SIZE - 1)] != '\r')
            ;
   }
   size_t n = 0;
   while (p < head && n < size)
      out[n++] = ring[p++ & (LOG_SIZE - 1)];
   *pos = p;
   return n;
}
