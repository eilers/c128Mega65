// Runs a C64 or C128 test program (test/uci's UCITEST, UCIDOS) on a 6502 against the UCI
// firmware (CORE/firmware/uci*.c) and host_uci_model.c's command_protocol.vhd, with a FAT32
// image as the card. The firmware is polled between instructions, as its main loop runs
// beside the C128. The KERNAL is faked where the programs call it: CHROUT prints, GETIN
// gives RETURN, the file calls read "$" from drive 9, the image shell_mount was given, and
// OPEN of a buffer ("#") finds drive 8 or 9 while it runs (drive_running). An OPEN while a
// drive that was just switched on holds DATA fails the run, as the KERNAL would hang.
// The REU answers at $DF00 while main.vhd has it on; its STASH and FETCH run at once, in the
// memory the firmware sees for it, wrapping at its size. A REU change that main.vhd would
// answer with a reset of the C128 (one without uci.vhd's keep bit) fails the run.
// usage: test_prg [-reu|-reu16 [-dma]] [-r3] [-blink] fat32.img program.prg [stop]   (-reu: a
// 512 KB REU is on, -reu16: 16 MB; -dma: its files go by the model of reu_sd.vhd; -r3: the menu
// of R3 memory, REU 512 KB at most; -blink: as an Ultimate, the interface is off the bus for
// 5 ms after each REU change, $DF1C-$DF1F reading $FF; exit 0 if it prints ALL PASSED, or with
// stop, once it has printed that text: a run cut short, as by a reset)
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ff.h"
#include "m2m_rv.h"
#include "host_uci_model.h"
#include "config_ids.h"
#include "drives.h"
#include "uci.h"
#include "host_reu_sd.h"

void host_init(void);
int  disk_file_open(const char *path);
extern uint64_t host_time;   // host_m2m.c's CLINT timer, in CPU clocks

static uint8_t  mem[0x10000];
static uint8_t  a, x, y, s = 0xFF, n, v, d, i, z, c;
static uint16_t pc;
static char     out[20000];
static int      outn;
static uint64_t cycles;

// Each opcode's cycles; a taken branch adds one, page crossings are left out
static const uint8_t op_cycles[256] = {
   7,6,2,8,3,3,5,5,3,2,2,2,4,4,6,6, 2,5,2,8,4,4,6,6,2,4,2,7,4,4,7,7,
   6,6,2,8,3,3,5,5,4,2,2,2,4,4,6,6, 2,5,2,8,4,4,6,6,2,4,2,7,4,4,7,7,
   6,6,2,8,3,3,5,5,3,2,2,2,3,4,6,6, 2,5,2,8,4,4,6,6,2,4,2,7,4,4,7,7,
   6,6,2,8,3,3,5,5,4,2,2,2,5,4,6,6, 2,5,2,8,4,4,6,6,2,4,2,7,4,4,7,7,
   2,6,2,6,3,3,3,3,2,2,2,2,4,4,4,4, 2,6,2,6,4,4,4,4,2,5,2,5,5,5,5,5,
   2,6,2,6,3,3,3,3,2,2,2,2,4,4,4,4, 2,5,2,5,4,4,4,4,2,4,2,4,4,4,4,4,
   2,6,2,8,3,3,5,5,2,2,2,2,4,4,6,6, 2,5,2,8,4,4,6,6,2,4,2,7,4,4,7,7,
   2,6,2,8,3,3,5,5,2,2,2,2,4,4,6,6, 2,5,2,8,4,4,6,6,2,4,2,7,4,4,7,7,
};

// -----------------------------------------------------------------------------------------
// The serial bus: a drive the control target switches on starts from reset, and a 1541 or
// 1571 holds DATA until its DOS listens for ATN (61 jiffies in VICE). An OPEN then waits
// for ever in the KERNAL. A mount's start is over before MOUNT answers (it waits for
// disk_ready, 2.1 s on the core), so only these starts hold DATA here.
// -----------------------------------------------------------------------------------------

#define DATA_HELD_CYCLES 1020000
static uint64_t held_until[2];
static int      was_on[2], hung;

static void drives_watch(void)
{
   for (int dr = 0; dr < 2; dr++) {
      int on = drive_running(dr);
      if (on && !was_on[dr] && drive_pow[dr] == DRIVE_ON)
         held_until[dr] = cycles + DATA_HELD_CYCLES;
      was_on[dr] = on;
   }
}

static int data_held(void)
{
   return (was_on[0] && cycles < held_until[0]) || (was_on[1] && cycles < held_until[1]);
}

// -----------------------------------------------------------------------------------------
// The bus: RAM, the serial bus's lines in $DD00 and the command interface at $DF18-$DF1F
// -----------------------------------------------------------------------------------------

// -blink: an Ultimate's LOAD_CONFIG takes the interface off the bus while the settings take
// effect (set_emulation_flags), until this cycle
static int      blink;
static uint64_t gone_until;

static uint8_t rd(uint16_t ad)
{
   if (ad >= 0xDF1C && ad <= 0xDF1F && cycles < gone_until)
      return 0xFF;
   switch (ad) {
   case 0xDD00: return (mem[ad] & 0x3F) | (data_held() ? 0x40 : 0xC0);   // 7 DATA, 6 CLK
   case 0xDF1B: return 0;
   case 0xDF1C: return uci_status();
   case 0xDF1D: return c128_ident();
   case 0xDF1E: return c128_response();
   case 0xDF1F: return c128_status_byte();
   }
   if ((ad >= 0xDF18 && ad <= 0xDF1A) || (ad >= 0xDF00 && ad < 0xDF18 && !reu_main))
      return 0xFF;
   return mem[ad];
}

// Each command as the program pushes it, held to what a real Ultimate needs: its FILE_STAT
// (dos.cc) and LOAD_CONFIG (control_target.cc) take the name up to a 0 byte, reading on past
// the command into what earlier ones left in its buffer, so a program has to send that 0
static uint8_t cmd[1024];
static int     cmd_n, unended;

// The REU's DMA, as far as UCIREU and UCICFG use it: STASH or FETCH at once ($DF01 bit 7,
// then 0 or 1), both addresses counting up, a byte per cycle, in the memory of uci.vhd's
// word 0x011, the REU address wrapping at its size
static void reu_dma(uint8_t command)
{
   static const uint32_t wrap[] = { 0, 0x7FFFF, 0x1FFFFF, 0xFFFFFF };
   uint8_t *reu = (uint8_t *)(um.reu_mem & 0x8000 ? host_sr : host_hr) + (um.reu_mem & 0x1FFF) * 8192;
   uint16_t c = mem[0xDF02] | mem[0xDF03] << 8;
   uint32_t r = (mem[0xDF04] | mem[0xDF05] << 8 | mem[0xDF06] << 16) & wrap[reu_main];
   uint32_t n = mem[0xDF07] | mem[0xDF08] << 8;
   for (uint32_t i = 0; i < (n ? n : 65536); i++, c++, r = (r + 1) & wrap[reu_main]) {
      if (command & 1)
         mem[c] = reu[r];
      else
         reu[r] = mem[c];
   }
   cycles += n ? n : 65536;
}

static void wr(uint16_t ad, uint8_t b)
{
   if (ad >= 0xDF1C && ad <= 0xDF1F && cycles < gone_until)
      return;
   if (ad == 0xDF1C) {
      if (b & 1) {
         int t = cmd[0] & 0x7F;
         if (cmd_n > 2 && (((t == 1 || t == 2) && cmd[1] == 0x08) || (t == 4 && cmd[1] == 0x50)) &&
             !memchr(cmd + 2, 0, cmd_n - 2))
            unended++;
         cmd_n = 0;
      }
      c128_control(b);
   } else if (ad == 0xDF1D) {
      if (cmd_n < (int)sizeof cmd)
         cmd[cmd_n++] = b;
      c128_command(b);
   } else if ((ad < 0xDF18 || ad > 0xDF1F) && !(ad >= 0xDF00 && ad < 0xDF18 && !reu_main)) {
      mem[ad] = b;
      if (ad == 0xDF01 && (b & 0x80))
         reu_dma(b);
   }
}

// -----------------------------------------------------------------------------------------
// The KERNAL calls the programs make
// -----------------------------------------------------------------------------------------

static uint8_t file_dev, name_len, st;
static uint16_t name_at;
static uint8_t stream[64];
static int     stream_n, stream_at;

// The first line of drive 9's directory, as LOAD"$" gets it, from the BAM of its image
static int open_dir9(void)
{
   FIL f;
   UINT got;
   uint8_t bam[256];
   if (file_dev != 9 || name_len != 1 || mem[name_at] != '$' || mounted_d != 1)
      return 0;
   if (f_open(&f, mounted_path, FA_READ) != FR_OK)
      return 0;
   int ok = f_lseek(&f, 0x16500) == FR_OK && f_read(&f, bam, 256, &got) == FR_OK && got == 256;
   f_close(&f);
   if (!ok)
      return 0;
   static const uint8_t start[] = { 0x01, 0x04, 0x01, 0x01, 0x00, 0x00, 0x12, '"' };
   memcpy(stream, start, sizeof start);
   stream_n = sizeof start;
   for (int k = 0; k < 16; k++)
      stream[stream_n++] = bam[0x90 + k] == 0xA0 ? ' ' : bam[0x90 + k];
   stream[stream_n++] = '"';
   stream[stream_n++] = ' ';
   stream[stream_n++] = bam[0xA2];
   stream[stream_n++] = bam[0xA3];
   stream[stream_n++] = ' ';
   stream[stream_n++] = '2';
   stream[stream_n++] = 'A';
   stream[stream_n++] = 0;
   stream_at = 0;
   return 1;
}

// A KERNAL entry: do it, then return as RTS would; 0 if pc is no KERNAL entry
static int kernal(void)
{
   switch (pc) {
   case 0xFFD2:                                        // CHROUT
      if (outn < (int)sizeof out - 1)
         out[outn++] = a == 13 ? '\n' : a >= 32 && a < 96 ? a : a == 147 ? '\f' : '?';
      c = 0;
      break;
   case 0xFF68: break;                                 // SETBNK
   case 0xFFBA: file_dev = x; break;                   // SETLFS
   case 0xFFBD: name_len = a; name_at = x | y << 8; break;   // SETNAM
   case 0xFFC0:                                        // OPEN
      hung |= data_held();
      st = 0;
      if (name_len == 1 && mem[name_at] == '#') {
         c  = file_dev < 8 || file_dev > 9 || !drive_running(file_dev - 8);
         st = c ? 0x80 : 0;                            // DEVICE NOT PRESENT
      } else
         c = !open_dir9();
      break;
   case 0xFFC6: c = 0; break;                          // CHKIN
   case 0xFFE4: a = 13; c = 0; break;                  // GETIN
   case 0xFFCF:                                        // CHRIN
      a = stream_at < stream_n ? stream[stream_at++] : 0;
      st = stream_at >= stream_n ? 0x40 : 0;
      c = 0;
      break;
   case 0xFFB7: a = st; z = !a; n = a >> 7; break;     // READST
   case 0xFFCC: case 0xFFC3: c = 0; break;             // CLRCHN, CLOSE
   default:
      return 0;
   }
   s += 2;
   pc = (mem[0x100 + (uint8_t)(s - 1)] | mem[0x100 + s] << 8) + 1;
   return 1;
}

// -----------------------------------------------------------------------------------------
// The 6502 (NMOS, the documented opcodes, binary arithmetic only)
// -----------------------------------------------------------------------------------------

static void    push8(uint8_t b) { mem[0x100 + s--] = b; }
static uint8_t pull8(void)      { return mem[0x100 + ++s]; }
static void    nz(uint8_t r)    { n = r >> 7; z = r == 0; }
static uint8_t flags(int brk)   { return n << 7 | v << 6 | 0x20 | brk << 4 | d << 3 | i << 2 | z << 1 | c; }
static void    set_flags(uint8_t f) { n = f >> 7; v = f >> 6 & 1; d = f >> 3 & 1; i = f >> 2 & 1; z = f >> 1 & 1; c = f & 1; }

static uint16_t w16(uint16_t ad) { return rd(ad) | rd(ad + 1) << 8; }
static uint16_t zpw(uint8_t ad)  { return mem[ad] | mem[(uint8_t)(ad + 1)] << 8; }

static void adc(uint8_t m)
{
   unsigned r = a + m + c;
   v = (~(a ^ m) & (a ^ r) & 0x80) != 0;
   c = r > 0xFF;
   a = r;
   nz(a);
}

static void cmp(uint8_t r, uint8_t m)
{
   c = r >= m;
   nz(r - m);
}

static void branch(int take)
{
   int8_t off = rd(pc++);
   if (take) {
      pc += off;
      cycles++;
   }
}

static int step(void)
{
   uint8_t op = rd(pc++), m;
   uint16_t ad = 0;
   cycles += op_cycles[op];
   // The operand's address, by the low bits of the opcode, as the 6502 lays them out
   switch (op) {
   case 0x69: case 0x29: case 0xC9: case 0xE0: case 0xC0: case 0x49: case 0xA9: case 0xA2:
   case 0xA0: case 0x09: case 0xE9:
      ad = pc++; break;                                                       // #imm
   case 0x65: case 0x25: case 0x06: case 0x24: case 0xC5: case 0xE4: case 0xC4: case 0xC6:
   case 0x45: case 0xE6: case 0xA5: case 0xA6: case 0xA4: case 0x46: case 0x05: case 0x26:
   case 0x66: case 0xE5: case 0x85: case 0x86: case 0x84:
      ad = rd(pc++); break;                                                   // zp
   case 0x75: case 0x35: case 0x16: case 0xD5: case 0xD6: case 0x55: case 0xF6: case 0xB5:
   case 0xB4: case 0x56: case 0x15: case 0x36: case 0x76: case 0xF5: case 0x95: case 0x94:
      ad = (uint8_t)(rd(pc++) + x); break;                                    // zp,x
   case 0xB6: case 0x96:
      ad = (uint8_t)(rd(pc++) + y); break;                                    // zp,y
   case 0x6D: case 0x2D: case 0x0E: case 0x2C: case 0xCD: case 0xEC: case 0xCC: case 0xCE:
   case 0x4D: case 0xEE: case 0x4C: case 0x20: case 0xAD: case 0xAE: case 0xAC: case 0x4E:
   case 0x0D: case 0x2E: case 0x6E: case 0xED: case 0x8D: case 0x8E: case 0x8C:
      ad = w16(pc); pc += 2; break;                                           // abs
   case 0x7D: case 0x3D: case 0x1E: case 0xDD: case 0xDE: case 0x5D: case 0xFE: case 0xBD:
   case 0xBC: case 0x5E: case 0x1D: case 0x3E: case 0x7E: case 0xFD: case 0x9D:
      ad = w16(pc) + x; pc += 2; break;                                       // abs,x
   case 0x79: case 0x39: case 0xD9: case 0x59: case 0xB9: case 0xBE: case 0x19: case 0xF9:
   case 0x99:
      ad = w16(pc) + y; pc += 2; break;                                       // abs,y
   case 0x61: case 0x21: case 0xC1: case 0x41: case 0xA1: case 0x01: case 0xE1: case 0x81:
      ad = zpw(rd(pc++) + x); break;                                          // (zp,x)
   case 0x71: case 0x31: case 0xD1: case 0x51: case 0xB1: case 0x11: case 0xF1: case 0x91:
      ad = zpw(rd(pc++)) + y; break;                                          // (zp),y
   case 0x6C:
      ad = w16(pc);
      ad = rd(ad) | rd((ad & 0xFF00) | (uint8_t)(ad + 1)) << 8;               // (abs), page bug
      break;
   }
   switch (op) {
   case 0x69: case 0x65: case 0x75: case 0x6D: case 0x7D: case 0x79: case 0x61: case 0x71:
      adc(rd(ad)); break;
   case 0xE9: case 0xE5: case 0xF5: case 0xED: case 0xFD: case 0xF9: case 0xE1: case 0xF1:
      adc(~rd(ad)); break;
   case 0x29: case 0x25: case 0x35: case 0x2D: case 0x3D: case 0x39: case 0x21: case 0x31:
      nz(a &= rd(ad)); break;
   case 0x09: case 0x05: case 0x15: case 0x0D: case 0x1D: case 0x19: case 0x01: case 0x11:
      nz(a |= rd(ad)); break;
   case 0x49: case 0x45: case 0x55: case 0x4D: case 0x5D: case 0x59: case 0x41: case 0x51:
      nz(a ^= rd(ad)); break;
   case 0xC9: case 0xC5: case 0xD5: case 0xCD: case 0xDD: case 0xD9: case 0xC1: case 0xD1:
      cmp(a, rd(ad)); break;
   case 0xE0: case 0xE4: case 0xEC: cmp(x, rd(ad)); break;
   case 0xC0: case 0xC4: case 0xCC: cmp(y, rd(ad)); break;
   case 0xA9: case 0xA5: case 0xB5: case 0xAD: case 0xBD: case 0xB9: case 0xA1: case 0xB1:
      nz(a = rd(ad)); break;
   case 0xA2: case 0xA6: case 0xB6: case 0xAE: case 0xBE: nz(x = rd(ad)); break;
   case 0xA0: case 0xA4: case 0xB4: case 0xAC: case 0xBC: nz(y = rd(ad)); break;
   case 0x85: case 0x95: case 0x8D: case 0x9D: case 0x99: case 0x81: case 0x91: wr(ad, a); break;
   case 0x86: case 0x96: case 0x8E: wr(ad, x); break;
   case 0x84: case 0x94: case 0x8C: wr(ad, y); break;
   case 0x24: case 0x2C: m = rd(ad); z = (a & m) == 0; n = m >> 7; v = m >> 6 & 1; break;
   case 0xC6: case 0xD6: case 0xCE: case 0xDE: m = rd(ad) - 1; wr(ad, m); nz(m); break;
   case 0xE6: case 0xF6: case 0xEE: case 0xFE: m = rd(ad) + 1; wr(ad, m); nz(m); break;
   case 0x0A: c = a >> 7; nz(a <<= 1); break;
   case 0x4A: c = a & 1; nz(a >>= 1); break;
   case 0x2A: m = c; c = a >> 7; nz(a = a << 1 | m); break;
   case 0x6A: m = c; c = a & 1; nz(a = a >> 1 | m << 7); break;
   case 0x06: case 0x16: case 0x0E: case 0x1E:
      m = rd(ad); c = m >> 7; m <<= 1; wr(ad, m); nz(m); break;
   case 0x46: case 0x56: case 0x4E: case 0x5E:
      m = rd(ad); c = m & 1; m >>= 1; wr(ad, m); nz(m); break;
   case 0x26: case 0x36: case 0x2E: case 0x3E: {
      uint8_t o = c;
      m = rd(ad); c = m >> 7; m = m << 1 | o; wr(ad, m); nz(m); break;
   }
   case 0x66: case 0x76: case 0x6E: case 0x7E: {
      uint8_t o = c;
      m = rd(ad); c = m & 1; m = m >> 1 | o << 7; wr(ad, m); nz(m); break;
   }
   case 0x90: branch(!c); break;
   case 0xB0: branch(c); break;
   case 0xF0: branch(z); break;
   case 0xD0: branch(!z); break;
   case 0x30: branch(n); break;
   case 0x10: branch(!n); break;
   case 0x50: branch(!v); break;
   case 0x70: branch(v); break;
   case 0x4C: case 0x6C: pc = ad; break;
   case 0x20: pc--; push8(pc >> 8); push8(pc); pc = ad; break;
   case 0x60: pc = pull8(); pc |= pull8() << 8; pc++; break;
   case 0x40: set_flags(pull8()); pc = pull8(); pc |= pull8() << 8; break;
   case 0x48: push8(a); break;
   case 0x68: nz(a = pull8()); break;
   case 0x08: push8(flags(1)); break;
   case 0x28: set_flags(pull8()); break;
   case 0xAA: nz(x = a); break;
   case 0xA8: nz(y = a); break;
   case 0x8A: nz(a = x); break;
   case 0x98: nz(a = y); break;
   case 0xBA: nz(x = s); break;
   case 0x9A: s = x; break;
   case 0xE8: nz(++x); break;
   case 0xC8: nz(++y); break;
   case 0xCA: nz(--x); break;
   case 0x88: nz(--y); break;
   case 0x18: c = 0; break;
   case 0x38: c = 1; break;
   case 0x58: i = 0; break;
   case 0x78: i = 1; break;
   case 0xB8: v = 0; break;
   case 0xD8: d = 0; break;
   case 0xF8: d = 1; break;
   case 0xEA: break;
   default:
      fprintf(stderr, "test_prg: opcode %02X at %04X\n", op, (uint16_t)(pc - 1));
      return 0;
   }
   return 1;
}

int main(int argc, char **argv)
{
   static uint8_t prg[0x10000];
   FATFS fs;
   FILE *f;
   int reu16 = argc > 1 && !strcmp(argv[1], "-reu16");
   int reu = reu16 || (argc > 1 && !strcmp(argv[1], "-reu"));
   argc -= reu;
   argv += reu;
   int dma = reu && argc > 1 && !strcmp(argv[1], "-dma");
   argc -= dma;
   argv += dma;
   int r3 = argc > 1 && !strcmp(argv[1], "-r3");
   argc -= r3;
   argv += r3;
   blink = argc > 1 && !strcmp(argv[1], "-blink");
   argc -= blink;
   argv += blink;
   if (argc < 3 || argc > 4 || (r3 && reu16) || !disk_file_open(argv[1]) || !(f = fopen(argv[2], "rb"))) {
      printf("usage: test_prg [-reu|-reu16 [-dma]] [-r3] [-blink] fat32.img program.prg [stop]\n");
      return 2;
   }
   const char *stop = argc == 4 ? argv[3] : NULL;
   size_t len = fread(prg, 1, sizeof prg, f);
   fclose(f);
   uint16_t load = prg[0] | prg[1] << 8;
   memcpy(mem + load, prg + 2, len - 2);

   host_init();
   uci_model_init();
   host_reu_sd(dma);
   if (reu) {
      settings[OPTM_G_REU][0] = 0;                    // REU: 512 KB, or 16 MB
      settings[OPTM_G_REU][reu16 ? 3 : 1] = 1;
   }
   if (r3)
      reu_items = 2;
   reu_sync();
   if (f_mount(&fs, "", 1) != FR_OK) {
      printf("test_prg: can't mount %s\n", argv[1]);
      return 2;
   }

   // 10 SYS<address>: start there, and stop at the RTS back to BASIC
   pc = atoi((char *)mem + load + 5);
   push8(0xFF);
   push8(0xFE);
   long count = 0;
   uint64_t polled = 0;
   int shown = 0;
   while (pc != 0xFFFF && count < 400000000) {
      if ((!kernal() && !step()) || hung)
         break;
      // The firmware's clock goes on with the C128's, at 1 MHz
      if (++count % 16 == 0) {
         unsigned forced = reu_forces;
         uci_poll();
         reu_settle();
         if (blink && reu_forces != forced)
            gone_until = cycles + 5000;
         drives_watch();
         host_time += (cycles - polled) * (CPU_HZ / 1000000);
         polled = cycles;
      }
      if (stop && outn != shown) {
         shown = outn;
         if (strstr(out, stop))
            break;
      }
   }
   out[outn] = 0;
   printf("%s\n", out);
   if (unended) {
      printf("test_prg: %d FILE_STAT or LOAD_CONFIG without the name's 0 byte: an Ultimate reads past it\n",
             unended);
      return 1;
   }
   if (hung) {
      printf("test_prg: OPEN while a drive just switched on holds DATA: the KERNAL waits for ever\n");
      return 1;
   }
   if (um.reu_resets) {
      printf("test_prg: %u REU changes without uci.vhd's keep bit: main.vhd resets the C128\n", um.reu_resets);
      return 1;
   }
   if (dma && !(rs.loads && rs.saves)) {
      printf("test_prg: -dma, but reu_sd.vhd's model moved nothing (%u loads, %u saves)\n", rs.loads, rs.saves);
      return 1;
   }
   printf("test_prg: %.2f s at 1 MHz\n", cycles / 1e6);
   if (stop && strstr(out, stop))
      return 0;
   if (pc != 0xFFFF)
      printf("test_prg: %s stopped at %04X after %ld instructions\n", argv[2], pc, count);
   return pc == 0xFFFF && strstr(out, "ALL PASSED") ? 0 : 1;
}
