// The CMD HD's firmware (CORE/firmware/cmdhd.c) against a model of cmdhd.vhd's device and of
// the controller on the other end of the SCSI bus, on a FAT32 card image.
// usage: test_cmdhd fat32.img
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cmdhd.h"
#include "ff.h"
#include "diskio.h"
#include "host_m2m.h"

int  disk_file_open(const char *path);
void host_init(void);

#define DEV     0x010F
#define CSR(r)  ((0xFFFFu << 12) + (r))
#define BUF     0x4000
#define MARK    0x8000                 // in the events word: the model's, not the firmware's

static int fails, runs;

void shell_fatal(const char *msg, uint32_t code)   // heap.c's, when it runs out
{
   printf("FATAL %s (%u)\n", msg, (unsigned)code);
   exit(1);
}

static void check(int ok, const char *what)
{
   runs++;
   if (!ok) {
      fails++;
      printf("FAIL %s\n", what);
   }
}

// --- the model ----------------------------------------------------------------------------

static struct {
   uint16_t events, ctl;
   int      len;
   uint8_t  din[1024];                 // what DATA IN gave the controller
   int      din_n;
   const uint8_t *dout;                // what it gives on DATA OUT
   int      dout_at;
   int      status, done;
   unsigned sd_reads, sd_writes;
   int      sd_fail;                   // the next transfer fails
} m;

static void hw(void)
{
   volatile uint16_t *d = host_dev_at(DEV);
   if (d[CSR(1)] != (m.events | MARK))     // the firmware wrote 1s to clear
      m.events &= ~d[CSR(1)];
   uint16_t ctl = d[CSR(8)];
   if ((ctl & 0x10) && !(m.ctl & 0x10) && !(ctl & 1))
      m.events |= 8;                    // the RAM cleared at once
   m.ctl = ctl;
   d[CSR(2)] = (ctl & 1) | m.len << 8;
   d[CSR(9)] = 0xF880;                  // in HD DOS's idle loop

   if (d[CSR(3)] != 0xFFFF) {           // go
      int ph = d[CSR(3)], n = d[CSR(4)], base = d[CSR(5)];
      d[CSR(3)] = 0xFFFF;
      if (ph == 0) {
         for (int i = 0; i < n && m.din_n < (int)sizeof m.din; i++)
            m.din[m.din_n++] = d[BUF + base + i] & 0xFF;
         m.events |= 2;
      } else if (ph == 1) {
         for (int i = 0; i < n; i++)
            d[BUF + base + i] = m.dout[m.dout_at++];
         m.events |= 2;
      } else {
         m.status = d[CSR(6)];
         m.done   = 1;
         m.events |= 4;
      }
   }
   if (!(d[CSR(0x13)] & 0x8000)) {      // a block between the card and the buffer
      int wr = d[CSR(0x13)] & 1, half = d[CSR(0x12)] & 1, bad = m.sd_fail;
      uint32_t blk = d[CSR(0x10)] | (uint32_t)d[CSR(0x11)] << 16;
      uint8_t b[512];
      m.sd_fail = 0;
      if (!bad && wr) {
         for (int i = 0; i < 512; i++)
            b[i] = d[BUF + half * 512 + i];
         bad = disk_write(0, b, blk, 1) != RES_OK;
         m.sd_writes++;
      } else if (!bad) {
         bad = disk_read(0, b, blk, 1) != RES_OK;
         for (int i = 0; i < 512; i++)
            d[BUF + half * 512 + i] = b[i];
         m.sd_reads++;
      }
      d[CSR(0x13)] = 0x8000 | (bad ? 2 : 0);
   }
   d[CSR(1)] = m.events | MARK;
}

static void model_on(void)
{
   volatile uint16_t *d = host_dev_at(DEV);
   memset(&m, 0, sizeof m);
   d[CSR(0)]    = 0x5D48;
   d[CSR(1)]    = MARK;
   d[CSR(3)]    = 0xFFFF;
   d[CSR(0x13)] = 0x8000;
   host_dev_hw  = hw;
}

// A command from the controller: its status, or -1 if it never ended
static int scsi(const uint8_t *cdb, int len, const uint8_t *out)
{
   volatile uint16_t *d = host_dev_at(DEV);
   for (int i = 0; i < len; i++)
      d[BUF + 0x400 + i] = cdb[i];
   m.len     = len;
   m.din_n   = 0;
   m.dout    = out;
   m.dout_at = 0;
   m.done    = 0;
   m.events |= 1;
   d[CSR(1)] = m.events | MARK;
   for (int k = 0; k < 200000 && !m.done; k++)
      cmdhd_poll();
   for (int k = 0; k < 10; k++)          // its bus free seen
      cmdhd_poll();
   return m.done ? m.status : -1;
}

static int read10(uint32_t lba, int n)
{
   uint8_t c[10] = { 0x28, 0, lba >> 24, lba >> 16, lba >> 8, lba, 0, n >> 8, n, 0 };
   return scsi(c, 10, NULL);
}

static int write10(uint32_t lba, int n, const uint8_t *data)
{
   uint8_t c[10] = { 0x2A, 0, lba >> 24, lba >> 16, lba >> 8, lba, 0, n >> 8, n, 0 };
   return scsi(c, 10, data);
}

static int sense(void)
{
   uint8_t c[6] = { 0x03, 0, 0, 0, 18, 0 };
   return scsi(c, 6, NULL) == 0 && m.din_n == 18 ? m.din[2] << 8 | m.din[12] : -1;
}

// --- the card -----------------------------------------------------------------------------

static uint8_t pat(uint32_t blk, int i)
{
   return (uint8_t)(blk * 7 + i) ^ (uint8_t)(blk >> 8);
}

static int make(const char *path, uint32_t blocks)
{
   FIL f;
   UINT put;
   uint8_t b[512];
   if (f_open(&f, path, FA_WRITE | FA_CREATE_ALWAYS) != FR_OK)
      return 0;
   for (uint32_t k = 0; k < blocks; k++) {
      for (int i = 0; i < 512; i++)
         b[i] = pat(k, i);
      if (f_write(&f, b, 512, &put) != FR_OK || put != 512)
         return 0;
   }
   return f_close(&f) == FR_OK;
}

// A file whose clusters lie in many pieces: written a few blocks at a time between another's
static int make_split(const char *path, uint32_t blocks)
{
   FIL f, g;
   UINT put;
   uint8_t b[512] = { 0 };
   if (f_open(&f, path, FA_WRITE | FA_CREATE_ALWAYS) != FR_OK ||
       f_open(&g, "/c128/hd/pad.bin", FA_WRITE | FA_CREATE_ALWAYS) != FR_OK)
      return 0;
   for (uint32_t k = 0; k < blocks; k++) {
      for (int i = 0; i < 512; i++)
         b[i] = pat(k, i);
      f_write(&f, b, 512, &put);
      f_sync(&f);
      if (k % 8 == 7) {
         f_write(&g, b, 512, &put);
         f_sync(&g);
      }
   }
   return f_close(&f) == FR_OK && f_close(&g) == FR_OK;
}

static int file_byte(const char *path, uint32_t at, uint8_t *v)
{
   FIL f;
   UINT got;
   int ok = f_open(&f, path, FA_READ) == FR_OK && f_lseek(&f, at) == FR_OK &&
            f_read(&f, v, 1, &got) == FR_OK && got == 1;
   f_close(&f);
   return ok;
}

static int din_is(uint32_t lba, int n)
{
   if (m.din_n != n * 512)
      return 0;
   for (int k = 0; k < n; k++)
      for (int i = 0; i < 512; i++)
         if (m.din[(k * 512 + i) % sizeof m.din] != pat(lba + k, i) && k * 512 + i < (int)sizeof m.din)
            return 0;
   return 1;
}

int main(int argc, char **argv)
{
   static FATFS fs;
   static uint8_t data[1024];
   const char *msg;
   FIL f;
   UINT put;

   if (argc != 2 || !disk_file_open(argv[1])) {
      fprintf(stderr, "usage: test_cmdhd fat32.img\n");
      return 2;
   }
   host_init();
   check(f_mount(&fs, "", 1) == FR_OK, "mount the card");
   f_mkdir("/c128/hd");
   f_mkdir("/c128/roms");
   check(make("/c128/hd/test.dhd", 4096), "a 2 MB image");
   check(make_split("/c128/hd/split.dhd", 600), "an image in pieces");
   check(f_open(&f, "/c128/roms/hd.rom", FA_WRITE | FA_CREATE_ALWAYS) == FR_OK, "a boot ROM file");
   for (int k = 0; k < 32; k++)
      f_write(&f, data, 512, &put);
   f_close(&f);

   model_on();
   cmdhd_init();
   check(cmdhd_present(), "the device is there");
   check(f_open(&f, "/c128/roms/hd.rom", FA_READ) == FR_OK && !cmdhd_rom_load(&f), "boot ROM loaded");
   f_close(&f);
   check(!cmdhd_mount("/c128/hd/test.dhd", &msg), "mount the image");
   for (int k = 0; k < 1000; k++)
      cmdhd_poll();
   check(m.ctl & 1, "it runs, after the RAM was cleared");
   check(!(m.ctl & 0x0E), "no button held for a disk with a system");

   uint8_t tur[6] = { 0 };
   check(scsi(tur, 6, NULL) == 0, "TEST UNIT READY: good");

   uint8_t inq[6] = { 0x12, 0, 0, 0, 36, 0 };
   check(scsi(inq, 6, NULL) == 0 && m.din_n == 36 && m.din[0] == 0 && m.din[2] == 1 &&
         !memcmp(m.din + 8, "C128M65 ", 8), "INQUIRY");

   uint8_t cap[10] = { 0x25 };
   check(scsi(cap, 10, NULL) == 0 && m.din_n == 8 && m.din[2] == 0x0F && m.din[3] == 0xFF &&
         m.din[6] == 2 && m.din[7] == 0, "READ CAPACITY: 4096 blocks of 512");

   check(read10(5, 1) == 0 && din_is(5, 1), "READ (10) of a block");
   check(read10(4094, 2) == 0 && din_is(4094, 2), "READ (10) of the last two");
   uint8_t r6[6] = { 0x08, 0, 0, 9, 1, 0 };
   check(scsi(r6, 6, NULL) == 0 && din_is(9, 1), "READ (6)");

   check(read10(4095, 2) == 2, "READ past the end: CHECK CONDITION");
   check(sense() == 0x0521, "its sense: illegal request, LBA out of range");
   check(sense() == 0x0000, "and then none");

   for (int i = 0; i < 1024; i++)
      data[i] = i * 13 + 5;
   check(write10(100, 2, data) == 0, "WRITE (10) of two blocks");
   uint8_t v;
   check(file_byte("/c128/hd/test.dhd", 100 * 512 + 7, &v) && v == data[7] &&
         file_byte("/c128/hd/test.dhd", 101 * 512 + 511, &v) && v == data[1023], "they are in the file");
   check(read10(100, 2) == 0 && m.din_n == 1024 && !memcmp(m.din, data, 1024), "and read back");

   // The bus ID in the configuration block after a system header (block 2 + 128 n)
   static const uint8_t sig[16] = { 'C', 'M', 'D', ' ', 'H', 'D', ' ', ' ',
                                    0x8D, 0x03, 0x88, 0x8E, 0x02, 0x88, 0xEA, 0x60 };
   for (int i = 0; i < 512; i++)
      data[i] = 0;
   memcpy(data + 0x1F0, sig, 16);
   data[0x1E1] = data[0x1E4] = 9;
   check(write10(386, 1, data) == 0, "a configuration block at 386");
   cmdhd_set_id(12);
   check(read10(386, 1) == 0 && m.din[0x1E1] == 12 && m.din[0x1E4] == 12 && !memcmp(m.din + 0x1F0, sig, 16),
         "read: the menu's bus ID in it");
   check(file_byte("/c128/hd/test.dhd", 386 * 512 + 0x1E1, &v) && v == 9, "the file keeps its own");
   check(write10(387, 1, data) == 0 && read10(387, 1) == 0 && m.din[0x1E1] == 9,
         "a block elsewhere is left alone");

   uint8_t fmt[6] = { 0x04, 0, 0, 0, 0, 0 };
   check(scsi(fmt, 6, NULL) == 0, "FORMAT UNIT");
   check(file_byte("/c128/hd/test.dhd", 386 * 512 + 0x1F0, &v) && v == 0, "the system's signature is gone");

   uint8_t tur1[6] = { 0x00, 0x20, 0, 0, 0, 0 };
   check(scsi(tur1, 6, NULL) == 2 && sense() >> 8 == 5, "LUN 1: CHECK CONDITION");
   uint8_t inq1[6] = { 0x12, 0x20, 0, 0, 36, 0 };
   check(scsi(inq1, 6, NULL) == 0 && m.din[0] == 0x60, "INQUIRY of LUN 1: not there");
   uint8_t bad[6] = { 0xC0 };
   check(scsi(bad, 6, NULL) == 2 && sense() == 0x0520, "an unknown command");

   m.sd_fail = 1;
   check(read10(7, 1) == 2 && sense() >> 8 == 3, "a failed card read: medium error");

   // An image in pieces
   check(!cmdhd_mount("/c128/hd/split.dhd", &msg), "mount the image in pieces");
   for (int k = 0; k < 1000; k++)
      cmdhd_poll();
   int good = 1;
   for (uint32_t lba = 0; lba + 4 <= 600 && good; lba += 37)
      good = read10(lba, 4) == 0 && din_is(lba, 2);
   check(good, "it reads right across its pieces");

   // An empty image starts in install mode: SWAP 8 and SWAP 9 held at the start
   check(f_open(&f, "/c128/hd/new.dhd", FA_WRITE | FA_CREATE_ALWAYS) == FR_OK, "an empty image");
   memset(data, 0, sizeof data);
   for (int k = 0; k < 1100; k++)
      f_write(&f, data, 512, &put);
   f_close(&f);
   check(!cmdhd_mount("/c128/hd/new.dhd", &msg), "mount it");
   for (int k = 0; k < 1000; k++)
      cmdhd_poll();
   check((m.ctl & 0x0D) == 0x0D, "install mode: running, SWAP 8 and SWAP 9 held");

   check(cmdhd_create("/c128/hd/made.dhd", 16u << 20) == FR_OK, "F5: a 16 MB image");
   FILINFO fi;
   check(f_stat("/c128/hd/made.dhd", &fi) == FR_OK && fi.fsize == 16u << 20, "of 16 MB");
   check(cmdhd_create("/c128/hd/made.dhd", 16u << 20) == FR_EXIST, "not over another");

   cmdhd_unmount();
   for (int k = 0; k < 100; k++)
      cmdhd_poll();
   check(!(host_dev_at(DEV)[CSR(8)] & 1), "out: held in reset");

   printf("test_cmdhd: %d checks, %d failed\n", runs, fails);
   return fails != 0;
}
