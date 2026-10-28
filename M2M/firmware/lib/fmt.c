// A small printf, instead of newlib's, which wants a heap and much more code.
#include <stdint.h>
#include "console.h"
#include "fmt.h"

int fmt_vsnprintf(char *buf, size_t size, const char *f, va_list ap)
{
   size_t n = 0;
#define PUT(c) do { if (n + 1 < size) buf[n] = (c); n++; } while (0)

   for (; *f; f++) {
      if (*f != '%') {
         PUT(*f);
         continue;
      }
      int left = 0, zero = 0, width = 0;
      for (f++; *f == '-' || *f == '0'; f++)
         *f == '-' ? (left = 1) : (zero = 1);
      while (*f >= '0' && *f <= '9')
         width = width * 10 + *f++ - '0';

      char tmp[12];
      const char *s = tmp + sizeof tmp;   // digits are written backwards from the end
      int len = 0;
      char sign = 0;
      switch (*f) {
      case 'c':
         tmp[0] = (char)va_arg(ap, int);
         s = tmp;
         len = 1;
         break;
      case 's':
         s = va_arg(ap, const char *);
         if (!s)
            s = "(null)";
         while (s[len])
            len++;
         break;
      case 'd':
      case 'u':
      case 'x':
      case 'X': {
         uint32_t v = va_arg(ap, uint32_t);
         if (*f == 'd' && (int32_t)v < 0) {
            sign = '-';
            v = -v;
         }
         uint32_t base = *f == 'x' || *f == 'X' ? 16 : 10;
         const char *digits = *f == 'x' ? "0123456789abcdef" : "0123456789ABCDEF";
         do {
            *(char *)--s = digits[v % base];
            len++;
         } while (v /= base);
         break;
      }
      case 0:
         f--;                             // a '%' at the very end
         /* fall through */
      case '%':
         tmp[0] = '%';
         s = tmp;
         len = 1;
         break;
      default:                            // not ours: print it as it was
         tmp[0] = '%';
         tmp[1] = *f;
         s = tmp;
         len = 2;
         break;
      }

      int pad = width - len - (sign != 0);
      if (!left && !zero)
         for (; pad > 0; pad--)
            PUT(' ');
      if (sign)
         PUT(sign);
      if (!left)
         for (; pad > 0; pad--)
            PUT('0');
      for (int i = 0; i < len; i++)
         PUT(s[i]);
      for (; pad > 0; pad--)
         PUT(' ');
   }
   if (size)
      buf[n < size ? n : size - 1] = 0;
   return n;
#undef PUT
}

int fmt_snprintf(char *buf, size_t size, const char *f, ...)
{
   va_list ap;
   va_start(ap, f);
   int n = fmt_vsnprintf(buf, size, f, ap);
   va_end(ap);
   return n;
}

void uart_printf(const char *f, ...)
{
   char buf[161];
   va_list ap;
   va_start(ap, f);
   fmt_vsnprintf(buf, sizeof buf, f, ap);
   va_end(ap);
   uart_puts(buf);
}
