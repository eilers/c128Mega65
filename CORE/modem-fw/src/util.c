#include "util.h"
#include "hw.h"

static void put(char *buf, size_t size, size_t *n, char c)
{
   if (*n + 1 < size)
      buf[*n] = c;
   (*n)++;
}

int vfmt(char *buf, size_t size, const char *f, va_list ap)
{
   size_t n = 0;
   for (; *f; f++) {
      if (*f != '%') {
         put(buf, size, &n, *f);
         continue;
      }
      f++;
      char pad = ' ';
      if (*f == '0') {
         pad = '0';
         f++;
      }
      int width = 0;
      while (*f >= '0' && *f <= '9')
         width = width * 10 + (*f++ - '0');

      char tmp[12];
      int len = 0;
      const char *s = tmp;
      switch (*f) {
         case 's':
            s = va_arg(ap, const char *);
            while (s[len])
               len++;
            break;
         case 'c':
            tmp[0] = (char)va_arg(ap, int);
            len = 1;
            break;
         case 'd':
         case 'u':
         case 'x': {
            uint32_t v = va_arg(ap, uint32_t);
            bool neg = false;
            if (*f == 'd' && (int32_t)v < 0) {
               neg = true;
               v = -v;
            }
            uint32_t base = *f == 'x' ? 16 : 10;
            char rev[11];
            int r = 0;
            do {
               rev[r++] = "0123456789abcdef"[v % base];
               v /= base;
            } while (v);
            if (neg)
               tmp[len++] = '-';
            while (r)
               tmp[len++] = rev[--r];
            break;
         }
         case '\0':
            f--;
            continue;
         default:
            tmp[0] = *f;
            len = 1;
            break;
      }
      for (int i = len; i < width; i++)
         put(buf, size, &n, pad);
      for (int i = 0; i < len; i++)
         put(buf, size, &n, s[i]);
   }
   if (size)
      buf[n < size ? n : size - 1] = '\0';
   return (int)n;
}

int fmt(char *buf, size_t size, const char *f, ...)
{
   va_list ap;
   va_start(ap, f);
   int n = vfmt(buf, size, f, ap);
   va_end(ap);
   return n;
}

uint32_t millis(void)
{
   uint32_t hi, lo;
   do {
      hi = MTIME_HI;
      lo = MTIME_LO;
   } while (hi != MTIME_HI);
   uint64_t t = ((uint64_t)hi << 32) | lo;
   return (uint32_t)(t / (CPU_HZ / 1000));
}

// xorshift32, seeded from the device DNA and the timer
uint32_t rand32(void)
{
   static uint32_t s;
   if (!s)
      s = (SYS_DNA_LO ^ MTIME_LO) | 1;
   s ^= s << 13;
   s ^= s >> 17;
   s ^= s << 5;
   return s;
}
