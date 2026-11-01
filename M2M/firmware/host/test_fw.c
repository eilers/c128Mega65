// The firmware's FatFs setup (fw/ffconf.h: long names, code page 437) against a
// macOS-made FAT32 image, and lib/fmt.c.
// usage: test_fw fat32.img
#include <stdio.h>
#include <string.h>
#include "ff.h"
#include "fmt.h"
#include "m2m_rv.h"

int disk_file_open(const char *path);

static int fails, runs;

void uart_puts(const char *s)   // lib/fmt.c's uart_printf ends here
{
   fputs(s, stdout);
}

static void check(int ok, const char *what)
{
   runs++;
   if (!ok) {
      fails++;
      printf("FAIL %s\n", what);
   }
}

static void fmt(const char *want, const char *f, ...)
{
   char buf[64];
   va_list ap;
   va_start(ap, f);
   int n = fmt_vsnprintf(buf, sizeof buf, f, ap);
   va_end(ap);
   runs++;
   if (strcmp(buf, want) || n != (int)strlen(want)) {
      fails++;
      printf("FAIL fmt \"%s\": got \"%s\" (%d), want \"%s\"\n", f, buf, n, want);
   }
}

int main(int argc, char **argv)
{
   static FATFS fs;
   static FILINFO fi;
   DIR dir;

   if (argc != 2 || !disk_file_open(argv[1])) {
      fprintf(stderr, "usage: test_fw fat32.img\n");
      return 2;
   }
   check(f_mount(&fs, "", 1) == FR_OK, "mount");

   int found = 0;
   check(f_opendir(&dir, "/Long folder name with spaces") == FR_OK, "open a folder by its long name");
   while (f_readdir(&dir, &fi) == FR_OK && fi.fname[0])
      if (!strcmp(fi.fname, "A file with a rather long name, to see.d64") && fi.fsize == 174848)
         found = 1;
   f_closedir(&dir);
   check(found, "a long file name and its size");
   check(f_stat("/c128/firmware.bin", &fi) == FR_OK && !strcmp(fi.altname, "FIRMWARE.BIN"),
         "the 8.3 name the boot ROM uses");

   fmt("x 42 y", "x %d y", 42);
   fmt("-7", "%d", -7);
   fmt("   -7", "%5d", -7);
   fmt("-0007", "%05d", -7);
   fmt("-7   |", "%-5d|", -7);
   fmt("4294967295", "%u", 0xFFFFFFFFu);
   fmt("-2147483648", "%d", (int)0x80000000);
   fmt("00c0ffee", "%08x", 0xC0FFEE);
   fmt("DEADBEEF", "%X", 0xDEADBEEF);
   fmt("0", "%x", 0);
   fmt("ab   |  cd|", "%-5s|%4s|", "ab", "cd");
   fmt("%", "%%");
   fmt("%q", "%q");
   fmt("c=Z", "c=%c", 'Z');
   fmt("(null)", "%s", (char *)0);

   char small[4];
   int n = fmt_snprintf(small, sizeof small, "%s", "abcdef");
   check(n == 6 && !strcmp(small, "abc"), "fmt cuts to the buffer and counts it all");

   // mtime_ms divides a byte at a time: the same as a 64-bit division
   uint64_t x = 0x9E3779B97F4A7C15u;
   int same = mtime_ms(0xFFFFFFFF, 0xFFFFFFFF) == (uint32_t)(UINT64_MAX / (CPU_HZ / 1000));
   for (int i = 0; i < 100000; i++) {
      x ^= x << 13;
      x ^= x >> 7;
      x ^= x << 17;
      uint64_t t = x >> (i & 63);
      same &= mtime_ms(t >> 32, (uint32_t)t) == (uint32_t)(t / (CPU_HZ / 1000));
   }
   check(same, "mtime_ms gives MTIME in milliseconds");

   printf("test_fw: %d of %d checks passed\n", runs - fails, runs);
   return fails != 0;
}
