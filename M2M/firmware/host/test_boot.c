// The boot ROM's loader (boot/load.c) on the host, against the FAT32 images run.sh makes.
// usage: test_boot dir   (dir holds the images run.sh makes and the files copied onto them)
#include <stdio.h>
#include <string.h>
#include "ff.h"
#include "load.h"
#include "m2m_rv.h"
#include "sd.h"
#include "sd_diag.h"

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

   // Cards FatFs won't mount: the reason, and the line of lib/sd_diag.c's that says why
   static const struct {
      const char *img, *want, *line;
   } bad[] = {
      { "blank.img",   "No FAT32 file system on the SD card", "Block 0: no 55AA signature" },
      { "exfat.img",   "No FAT32 file system on the SD card", ": exFAT, not FAT32" },
      { "gpt.img",     "No FAT32 file system on the SD card", "P1 EE@00000001: GPT, needs an MBR" },
      { "nolabel.img", "No FAT32 file system on the SD card", ": no FAT32 type text" },
      { "fewclus.img", "No FAT32 file system on the SD card", ": too few clusters for FAT32" },
   };
   for (unsigned i = 0; i < sizeof bad / sizeof *bad; i++) {
      fw_block_t hdr;
      sd_diag_t d;
      snprintf(name, sizeof name, "%s/%s", argv[1], bad[i].img);
      const char *got = disk_file_open(name) ? fw_load(FW_PATH, &hdr) : "can't open";
      check(got && !strcmp(got, bad[i].want), bad[i].want, bad[i].img, FW_PATH, got);
      sd_diag(&d, fw_fr);
      int hit = 0;
      for (int l = 0; l < d.n; l++) {
         printf("  %-12s %s\n", bad[i].img, d.line[l]);
         hit |= strstr(d.line[l], bad[i].line) != 0;
      }
      check(fw_mount_failed && hit, bad[i].line, bad[i].img, FW_PATH, got);
      runs += 2;
   }

   // Cards FatFs mounts: the diagnosis must not blame them
   for (unsigned i = 0; i < sizeof imgs / sizeof *imgs; i++) {
      sd_diag_t d;
      snprintf(name, sizeof name, "%s/%s", argv[1], imgs[i]);
      disk_file_open(name);
      sd_diag(&d, FR_NO_FILESYSTEM);
      check(d.n == 4 && strstr(d.line[3], ": FatFs takes it") != 0, "FatFs takes it", imgs[i], "",
            d.n > 3 ? d.line[3] : "no partition line");
      runs++;
   }

   // The SD controller's side, as lib/sd.c logs it: a read that failed after the fallback
   // to SPI, then an init that timed out
   {
      extern uint32_t host_sd_now;
      static const char *const want[2][3] = {
         { "FatFs 01, SD error EE33 at 00000800", "SD mode 30035, now 20306", 0 },
         { "FatFs 03, SD error EEFF at init", "SD mode 00000, now 00000", 0 },
      };
      sd_diag_t d;
      sd_log = (sd_log_t){ 0xEE33, 0x800, 0x30035 };
      host_sd_now = 0x20306;
      for (int k = 0; k < 2; k++) {
         sd_diag(&d, k ? FR_NOT_READY : FR_DISK_ERR);
         for (int l = 0; l < 3; l++) {
            const char *got = l < d.n ? d.line[l] : 0;
            int ok = want[k][l] ? got && !strcmp(got, want[k][l]) : !got;
            check(ok, want[k][l] ? want[k][l] : "no more lines", "sd_log", "", got);
            runs++;
         }
         sd_log = (sd_log_t){ 0xEEFF, SD_AT_INIT, 0 };
         host_sd_now = 0;
      }
      sd_log = (sd_log_t){ 0, 0, 0x30035 };
      host_sd_now = 0x30035;
   }

   // A folder that isn't there
   snprintf(name, sizeof name, "%s/fat32.img", argv[1]);
   fw_block_t hdr;
   const char *got = disk_file_open(name) ? fw_load("/nodir/firmware.bin", &hdr) : "can't open";
   check(got && !strcmp(got, "The firmware's folder is missing"), "The firmware's folder is missing",
         "fat32.img", "/nodir/firmware.bin", got);
   runs++;

   printf("test_boot: %d of %d cases passed\n", runs - fails, runs);
   return fails != 0;
}
