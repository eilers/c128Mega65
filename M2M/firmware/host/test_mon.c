// The serial monitor's commands (fw/monitor.c) against a pretend CPU: 64 KB of HyperRAM,
// two devices behind the device bus window, two registers, and bus errors elsewhere.
#include <stdio.h>
#include <string.h>
#include "monitor.h"

static int fails, runs;

static uint8_t  hr[0x10000];                   // at 0x40000000
static uint16_t ram[0x40000], osm[0x1000];     // devices 0100 and 0000
static uint32_t ramrom_dev, csr;
static int      dev_writes;                    // writes to RAMROM_DEV

bool mon_access(uint32_t a, int width, bool write, uint32_t *v)
{
   if (a >= 0x40000000u && a + width <= 0x40010000u) {
      for (int i = 0; i < width; i++) {
         if (write)
            hr[a - 0x40000000u + i] = *v >> (8 * i);
         else
            *v = (i ? *v : 0) | (uint32_t)hr[a - 0x40000000u + i] << (8 * i);
      }
      return true;
   }
   if (a >= 0xC0000000u && a < 0xE0000000u && width == 2) {
      uint32_t w = (a - 0xC0000000u) / 2;
      uint16_t *d = ramrom_dev == 0x100 && w < 0x40000 ? &ram[w] :
                    ramrom_dev == 0 && w < 0x1000      ? &osm[w] : 0;
      if (write && d)
         *d = *v;
      else if (!write)
         *v = d ? *d : 0;
      return true;
   }
   uint32_t *r = a == 0xE000001Cu ? &ramrom_dev : a == 0xE0000000u ? &csr : 0;
   if (r && width == 4) {
      if (write) {
         *r = *v;
         dev_writes += r == &ramrom_dev;
      } else {
         *v = *r;
      }
      return true;
   }
   return false;
}

// Each access takes 7 clocks; a bus error stops the run
static uint32_t timed_n;   // accesses made
bool mon_time(uint32_t addr, int width, bool write, uint32_t v, uint32_t n, uint32_t *clocks)
{
   bool ok = true;
   for (timed_n = 0; timed_n < n && ok; timed_n++, addr += width)
      ok = mon_access(addr, width, write, &v);
   *clocks = 7 * timed_n;
   return ok;
}

struct mon_loop mon_loop;
const char *const mon_module[MON_MODULES] = { "keys", "sdcard", "shell", "core", "net", "monitor" };

static char out[1 << 20];
static int  out_n, room = 1 << 20;

void mon_putc(char c)
{
   out[out_n++] = c;
}

int mon_room(void)
{
   return room;
}

// As monitor_poll: input waits while a read is under way, except for Ctrl-C
static void feed(const char *s)
{
   for (;;) {
      while (*s && (!mon_busy() || *s == 3))
         mon_input(*s++);
      if (!*s && !mon_busy())
         return;
      mon_step();
   }
}

static void expect(const char *in, const char *want)
{
   out_n = 0;
   feed(in);
   out[out_n] = 0;
   runs++;
   if (strcmp(out, want)) {
      fails++;
      printf("FAIL \"%s\"\n got: \"%s\"\nwant: \"%s\"\n", in, out, want);
   }
}

static void check(int ok, const char *what)
{
   runs++;
   if (!ok) {
      fails++;
      printf("FAIL %s\n", what);
   }
}

int main(void)
{
   for (int i = 0; i < 0x10000; i++)
      hr[i] = i;
   mon_start();
   check(out_n == 2 && !memcmp(out, "> ", 2), "first prompt");

   // Reads: echo, lines of 16 bytes, ok, prompt
   expect("rb 40000000 4\r", "rb 40000000 4\r\n40000000  00 01 02 03\r\nok\r\n> ");
   expect("rb 40000010\r", "rb 40000010\r\n40000010  10\r\nok\r\n> ");
   expect("rh 40000002 9\r",
          "rh 40000002 9\r\n40000002  0302 0504 0706 0908 0B0A 0D0C 0F0E 1110\r\n"
          "40000012  1312\r\nok\r\n> ");
   expect("RW 40000004 5\r",
          "RW 40000004 5\r\n40000004  07060504 0B0A0908 0F0E0D0C 13121110\r\n"
          "40000014  17161514\r\nok\r\n> ");

   // A CR LF line end is one line end; LF alone is one too
   expect("rb 40000020\r\n", "rb 40000020\r\n40000020  20\r\nok\r\n> ");
   expect("rb 40000021\n", "rb 40000021\r\n40000021  21\r\nok\r\n> ");
   expect("\r", "\r\nok\r\n> ");

   // Writes
   expect("wb 40000100 AA bb 1\r", "wb 40000100 AA bb 1\r\nok\r\n> ");
   check(hr[0x100] == 0xAA && hr[0x101] == 0xBB && hr[0x102] == 1, "wb wrote 3 bytes");
   expect("ww 40000200 12345678 9ABCDEF0\r", "ww 40000200 12345678 9ABCDEF0\r\nok\r\n> ");
   check(hr[0x200] == 0x78 && hr[0x203] == 0x12 && hr[0x204] == 0xF0 && hr[0x207] == 0x9A,
         "ww wrote 2 words, low byte first");
   expect("wh 40000300 BEEF\r", "wh 40000300 BEEF\r\nok\r\n> ");
   check(hr[0x300] == 0xEF && hr[0x301] == 0xBE, "wh wrote a halfword");

   // Devices: the window shows the device for the access and the old one after it
   ram[0x1C01] = 0x0B;
   ram[0x1C02] = 0x1C;
   osm[5] = 0x1234;
   ramrom_dev = 7;
   dev_writes = 0;
   expect("rb 100:1C01 2\r", "rb 100:1C01 2\r\n0100:0001C01  0B 1C\r\nok\r\n> ");
   check(ramrom_dev == 7 && dev_writes == 2, "rb on a device puts the window back");
   expect("rh 0:5\r", "rh 0:5\r\n0000:0000005  1234\r\nok\r\n> ");
   expect("rb 0:5\r", "rb 0:5\r\n0000:0000005  34\r\nok\r\n> ");
   expect("wb 100:3FFFE 12 34\r", "wb 100:3FFFE 12 34\r\nok\r\n> ");
   check(ram[0x3FFFE] == 0x12 && ram[0x3FFFF] == 0x34 && ramrom_dev == 7, "wb on a device");
   expect("rb 100:0 11\r",
          "rb 100:0 11\r\n0100:0000000  00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00\r\n"
          "0100:0000010  00\r\nok\r\n> ");
   expect("rh 0:4 9\r",
          "rh 0:4 9\r\n0000:0000004  0000 1234 0000 0000 0000 0000 0000 0000\r\n"
          "0000:000000C  0000\r\nok\r\n> ");

   // Registers
   expect("ww E0000000 2\r", "ww E0000000 2\r\nok\r\n> ");
   expect("rw E0000000\r", "rw E0000000\r\nE0000000  00000002\r\nok\r\n> ");

   // Bus errors, also in the middle of a read, which keeps the lines before it
   expect("rb 50000000\r", "rb 50000000\r\n? bus error at 50000000\r\n> ");
   expect("rb 4000FFFE 4\r",
          "rb 4000FFFE 4\r\n4000FFFE  FE FF\r\n? bus error at 40010000\r\n> ");
   expect("ww E0000004 1\r", "ww E0000004 1\r\n? bus error at E0000004\r\n> ");
   ramrom_dev = 7;
   expect("rw E0000008 1\r", "rw E0000008 1\r\n? bus error at E0000008\r\n> ");

   // Mistakes
   expect("rh 40000001\r", "rh 40000001\r\n? alignment\r\n> ");
   expect("rw 100:0\r", "rw 100:0\r\n? device words are 16 bits\r\n> ");
   expect("rx 0\r", "rx 0\r\n? what? h for help\r\n> ");
   expect("rbb 0\r", "rbb 0\r\n? what? h for help\r\n> ");
   expect("rb\r", "rb\r\n? address\r\n> ");
   expect("rb 10000:0\r", "rb 10000:0\r\n? address\r\n> ");
   expect("rb 100:10000000\r", "rb 100:10000000\r\n? address\r\n> ");
   expect("rb 123456789\r", "rb 123456789\r\n? address\r\n> ");
   expect("rb 40000000 0\r", "rb 40000000 0\r\n? count\r\n> ");
   expect("rb 40000000 1 2\r", "rb 40000000 1 2\r\n? count\r\n> ");
   expect("rw FFFFFFFC 2\r", "rw FFFFFFFC 2\r\n? past the end\r\n> ");
   expect("rb 100:FFFFFFF 2\r", "rb 100:FFFFFFF 2\r\n? past the end\r\n> ");
   expect("wb 40000000\r", "wb 40000000\r\n? value\r\n> ");
   expect("wb 40000000 100\r", "wb 40000000 100\r\n? value\r\n> ");
   expect("wh 40000000 10000\r", "wh 40000000 10000\r\n? value\r\n> ");
   expect("wb 40000000 1 x\r", "wb 40000000 1 x\r\n? value\r\n> ");
   check(hr[0] == 0, "a bad value writes nothing");

   // Typing: backspace, Ctrl-C, the help
   expect("rx\bb 40000003\r", "rx\b \bb 40000003\r\n40000003  03\r\nok\r\n> ");
   expect("rb 4\x03", "rb 4^C\r\n> ");
   expect("h\r", "h\r\nrb|rh|rw ADDR [N]   read N bytes, halfwords or words\r\n"
          "wb|wh|ww ADDR V...  write them\r\n"
          "ADDR: a CPU address, or DEV:WORD, a word of a device (rb, wb: its low byte)\r\n"
          "40000000 HyperRAM, C0000000 device bus, E0000000 registers\r\n"
          "Ctrl-C stops a read\r\n"
          "tr ADDR [N]         time N reads of the words from ADDR on (tw ADDR V [N]: writes)\r\n"
          "lt                  the main loop's timing since the last lt\r\nok\r\n> ");

   // Timed accesses: words of the CPU, or of a device behind the window, which is put back
   expect("tr 40000000 10\r", "tr 40000000 10\r\n16 reads in 1 us, 70 ns each\r\nok\r\n> ");
   check(timed_n == 16, "tr makes N reads");
   expect("tr 40000000\r", "tr 40000000\r\n1024 reads in 71 us, 70 ns each\r\nok\r\n> ");
   expect("tw 40000400 A5A5A5A5 3\r", "tw 40000400 A5A5A5A5 3\r\n3 writes in 0 us, 70 ns each\r\nok\r\n> ");
   check(hr[0x400] == 0xA5 && hr[0x40B] == 0xA5 && hr[0x40C] == 0x0C, "tw writes N words, no more");
   ramrom_dev = 7;
   expect("tw 100:20 BEEF 2\r", "tw 100:20 BEEF 2\r\n2 writes in 0 us, 70 ns each\r\nok\r\n> ");
   check(ram[0x20] == 0xBEEF && ram[0x21] == 0xBEEF && ram[0x22] == 0 && ramrom_dev == 7,
         "tw on a device writes its words and puts the window back");
   expect("tr 4000FFF8 4\r", "tr 4000FFF8 4\r\n? bus error\r\n> ");
   check(timed_n == 3, "a bus error stops the run");
   expect("tr 40000002\r", "tr 40000002\r\n? alignment\r\n> ");
   expect("tr 40000000 0\r", "tr 40000000 0\r\n? count\r\n> ");
   expect("tr 40000000 1 2\r", "tr 40000000 1 2\r\n? count\r\n> ");
   expect("tw 40000000\r", "tw 40000000\r\n? value\r\n> ");
   expect("tw 100:0 10000\r", "tw 100:0 10000\r\n? value\r\n> ");
   expect("tr FFFFFFFC 2\r", "tr FFFFFFFC 2\r\n? past the end\r\n> ");
   expect("trr 0\r", "trr 0\r\n? what? h for help\r\n> ");

   // The main loop's timing, then cleared
   mon_loop.rounds = 123456;
   mon_loop.max[0] = 250;
   mon_loop.max[2] = 45000000;
   mon_loop.max[4] = 1300;
   mon_loop.hist[0] = 120000;
   mon_loop.hist[6] = 2;
   expect("lt\r", "lt\r\nrounds 123456\r\n"
          "longest call: keys 2 us, sdcard 0 us, shell 450000 us, core 0 us, net 13 us, monitor 0 us\r\n"
          "rounds under 10 us 120000, 100 us 0, 1 ms 0, 10 ms 0, 100 ms 0, 1 s 0, longer 2\r\n"
          "ok\r\n> ");
   expect("lt\r", "lt\r\nrounds 0\r\n"
          "longest call: keys 0 us, sdcard 0 us, shell 0 us, core 0 us, net 0 us, monitor 0 us\r\n"
          "rounds under 10 us 0, 100 us 0, 1 ms 0, 10 ms 0, 100 ms 0, 1 s 0, longer 0\r\n"
          "ok\r\n> ");
   expect("lt 1\r", "lt 1\r\n? what? h for help\r\n> ");

   // A read waits for room to send its next line, and Ctrl-C stops it
   out_n = 0;
   room = 79;
   for (const char *s = "rb 40000000 10000\r"; *s; s++)
      mon_input(*s);
   out[out_n] = 0;
   check(mon_busy() && !strcmp(out, "rb 40000000 10000\r\n"), "a read waits for room");
   int before = out_n;
   mon_step();
   check(out_n == before, "no line without room for it");
   room = 1 << 20;
   mon_step();
   mon_step();
   check(mon_busy() && out_n > before, "lines once there's room");
   mon_input(3);
   out[out_n] = 0;
   const char *stopped = "? stopped\r\n> ";
   check(!mon_busy() && strstr(out, "\r\n40000010  10 11 ") &&
         !strcmp(out + out_n - strlen(stopped), stopped), "Ctrl-C stops the read");

   printf("test_mon: %d of %d checks passed\n", runs - fails, runs);
   return fails != 0;
}
