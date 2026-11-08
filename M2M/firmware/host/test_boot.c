// The boot ROM's loader (boot/load.c) on the host, against the FAT32 images run.sh makes.
// usage: test_boot dir   (dir holds fat*.img, blank.img and the files copied onto them)
#include <stdio.h>
#include <string.h>
#include "load.h"
#include "m2m_rv.h"

int disk_file_open(const char *path);

uint8_t host_hr[M2M_FW_END - M2M_FW_BASE + 4] = { 0 };

static const struct {
   const char *path;
   const char *want;      // 0: loads
   const char *src;       // what it loads, in dir
} cases[] = {
   { FW_PATH,              0, "firmware.bin" },
   { "/C128/FIRMWARE.BIN", 0, "firmware.bin" },
   { "/big/fw1m.bin",      0, "fw1m.bin" },
   { "/bad/crc.bin",       "The firmware's checksum is wrong", 0 },
   { "/bad/hwver.bin",     "The firmware doesn't fit this core", 0 },
   { "/bad/short.bin",     "The firmware file is cut short", 0 },
   { "/bad/hdrshort.bin",  "The firmware file is no firmware", 0 },
   { "/bad/magic.bin",     "The firmware file is no firmware", 0 },
   { "/bad/big.bin",       "The firmware's header is broken", 0 },
   { "/bad/entry.bin",     "The firmware's header is broken", 0 },
   { "/bad/align.bin",     "The firmware's header is broken", 0 },
   { "/nofile.bin",        "The firmware file is missing", 0 },
};

static int fails;

static void check(int ok, const char *what, const char *img, const char *path, const char *got)
{
   if (!ok) {
      fails++;
      printf("FAIL %s %s: %s (got \"%s\")\n", img, path, what, got ? got : "loaded");
   }
}

// The source file's image (after its header) must be in host_hr at the header's load address
static int same(const char *dir, const char *src, const fw_header_t *h)
{
   static uint8_t buf[sizeof host_hr];
   char name[512];
   snprintf(name, sizeof name, "%s/%s", dir, src);
   FILE *f = fopen(name, "rb");
   if (!f)
      return 0;
   size_t n = fread(buf, 1, sizeof buf, f);
   fclose(f);
   return n == FW_HEADER_SIZE + h->size &&
          !memcmp(host_hr + (h->load_addr - M2M_FW_BASE), buf + FW_HEADER_SIZE, h->size);
}

int main(int argc, char **argv)
{
   if (argc != 2) {
      fprintf(stderr, "usage: test_boot dir\n");
      return 2;
   }
   const char *imgs[] = { "fat32.img", "fat16.img" };
   char name[512];
   int runs = 0;

   for (unsigned i = 0; i < sizeof imgs / sizeof *imgs; i++) {
      snprintf(name, sizeof name, "%s/%s", argv[1], imgs[i]);
      if (!disk_file_open(name)) {
         printf("FAIL can't open %s\n", name);
         return 1;
      }
      for (unsigned c = 0; c < sizeof cases / sizeof *cases; c++) {
         fw_block_t hdr;
         memset(host_hr, 0xA5, sizeof host_hr);
         const char *got = fw_load(cases[c].path, &hdr);
         runs++;
         if (cases[c].want)
            check(got && !strcmp(got, cases[c].want), cases[c].want, imgs[i], cases[c].path, got);
         else
            check(!got && same(argv[1], cases[c].src, &hdr.h), "should load", imgs[i],
                  cases[c].path, got);
      }
   }

   // A card with no FAT on it
   snprintf(name, sizeof name, "%s/blank.img", argv[1]);
   fw_block_t hdr;
   const char *got = disk_file_open(name) ? fw_load(FW_PATH, &hdr) : "can't open";
   check(got && !strcmp(got, "Can't read the SD card"), "Can't read the SD card", "blank.img",
         FW_PATH, got);
   runs++;

   printf("test_boot: %d of %d cases passed\n", runs - fails, runs);
   return fails != 0;
}
