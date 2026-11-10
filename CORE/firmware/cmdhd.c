// The CMD HD (cmdhd.h). The disk's commands as VICE's src/core/scsi.c answers them (VICE,
// GPL v2 or later), with its CMD HD touches (src/drive/iec/cmdhd.c): the bus ID patched into
// the configuration block as it is read, and FORMAT UNIT wiping the system's signature.
#include <string.h>
#include "cmdhd.h"
#include "core.h"
#include "ff.h"
#include "heap.h"
#include "m2m_rv.h"

#define HD_DEVICE   0x010Fu        // C_DEV_CMDHD (globals.vhd)
#define HD_ROM      0x0000         // device words: the boot ROM,
#define HD_BUF      0x4000         // the buffer (two halves), its command bytes from 0x400
#define HD_CDB      0x4400
#define CSR(r)      ((0xFFFFu << 12) + (r))
#define R_MAGIC     0x000
#define R_EVENTS    0x001          // bit 0 command, 1 data phase done, 2 bus free, 3 RAM clear
#define R_STATUS    0x002          // bit 0 runs, bits 11..8 the command's length
#define R_GO        0x003
#define R_COUNT     0x004
#define R_BASE      0x005
#define R_STAT      0x006
#define R_CTL       0x008          // bit 0 run, 1-3 buttons, 4 clear the RAM
#define R_PC        0x009
#define R_SD_LO     0x010
#define R_SD_HI     0x011
#define R_SD_HALF   0x012
#define R_SD_GO     0x013          // W: bit 0 write; R: bit 0 busy, bit 1 failed
#define R_SD_CARD   0x014
#define MAGIC       0x5D48
#define EV_CMD      1
#define EV_DONE     2
#define EV_FREE     4
#define EV_CLEAR    8
#define CTL_RUN     1
#define CTL_CLEAR   0x10
#define GO_DIN      0
#define GO_DOUT     1
#define GO_STATUS   2

#define RUNS        1024           // pieces of the image on the card, at most
#define SYS_BLOCKS  144            // less than this is an empty disk, as VICE has it
#define BLANK_CHECK 1024           // blocks at the start that are all zero: an empty disk
#define MAX_BLOCKS  0xFFFFFEu      // READ CAPACITY's most, as VICE's

// sense keys and codes, as VICE's
#define SK_NOT_READY      0x02
#define SK_MEDIUM_ERROR   0x03
#define SK_ILLEGAL        0x05
#define SK_DATA_PROTECT   0x07
#define ASC_BAD_OPCODE    0x20
#define ASC_LBA_RANGE     0x21
#define ASC_BAD_CDB       0x24
#define ASC_NO_LUN        0x25
#define ASC_BAD_PARAMS    0x26
#define ASC_PROTECTED     0x27

static const uint8_t hd_sig[16] = { 'C', 'M', 'D', ' ', 'H', 'D', ' ', ' ',
                                    0x8D, 0x03, 0x88, 0x8E, 0x02, 0x88, 0xEA, 0x60 };

static int      there;
static int      busy;              // in a FatFs call of ours: the poll from disk_idle waits
static int      rom_ok;
static char     path[256];
static int      mounted, readonly;
static FATFS   *fs;
static uint8_t  card;
static uint32_t blocks;            // the image's, at most MAX_BLOCKS
static DWORD   *runs;              // FatFs's link map: length, then cluster, of each run
static uint32_t r_first, r_idx = 1;   // block_at's run: its first cluster in the file, its entry
static int      on = 1, bus_id = 12;

// power: off, clearing the RAM, running; buttons held until held_until
static enum { P_OFF, P_CLEAR, P_RUN } power;
static int      cold;              // the next start clears the RAM first
static int      want_mode;         // HD_RESET_* for the next start
static int      mode;              // the one it started in; normal once the DOS runs
static int      held;              // buttons held now (CSR bits)
static uint32_t held_since, hold_for;
static int      dos_seen;          // the DOS ran since the RAM was cleared (a warm reset then)

// the disk
static enum { S_IDLE, S_STATUS, S_DIN, S_RD_SD, S_RD_XFER, S_WR_XFER, S_WR_SD, S_FMT_HDR,
              S_FMT_RD, S_FMT_WR, S_REASSIGN } st;
static uint8_t  cdb[16];
static uint32_t lba, left;
static uint8_t  sense, asc;

// --- the device ---------------------------------------------------------------------------

static uint16_t get(uint32_t a)
{
   uint32_t was = M2M_RAMROM_DEV;
   M2M_RAMROM_DEV = HD_DEVICE;
   uint16_t v = M2M_DEV16[a];
   M2M_RAMROM_DEV = was;
   return v;
}

static void set(uint32_t a, uint16_t v)
{
   uint32_t was = M2M_RAMROM_DEV;
   M2M_RAMROM_DEV = HD_DEVICE;
   M2M_DEV16[a] = v;
   M2M_RAMROM_DEV = was;
}

static void buf_zero(int n)
{
   for (int i = 0; i < n; i++)
      set(HD_BUF + i, 0);
}

static void go(int phase, int count, int base, int status)
{
   set(CSR(R_COUNT), count);
   set(CSR(R_BASE), base);
   set(CSR(R_STAT), status);
   set(CSR(R_GO), phase);
}

static void status(int s)
{
   go(GO_STATUS, 1, 0, s);
   st = S_STATUS;
}

static void fail(int key, int code)
{
   sense = key;
   asc   = code;
   status(2);                      // CHECK CONDITION
}

static void din(int n)
{
   if (n == 0) {
      status(0);
      return;
   }
   go(GO_DIN, n, 0, 0);
   st = S_DIN;
}

// --- the image on the card ----------------------------------------------------------------

// The SD block of image block b: the run it is in (the last one asked, first)
static uint32_t block_at(uint32_t b)
{
   uint32_t per = fs->csize, c = b / per;
   if (r_idx < 1 || c < r_first) {
      r_first = 0;
      r_idx   = 1;
   }
   while (runs[r_idx]) {
      if (c < r_first + runs[r_idx])
         return fs->database + (runs[r_idx + 1] - 2 + (c - r_first)) * per + b % per;
      r_first += runs[r_idx];
      r_idx   += 2;
   }
   r_first = 0;
   r_idx   = 1;
   return 0;
}

static int sd_start(uint32_t b, int write)
{
   uint32_t blk = block_at(b);
   if (!blk)
      return 0;
   set(CSR(R_SD_LO), blk & 0xFFFF);
   set(CSR(R_SD_HI), blk >> 16);
   set(CSR(R_SD_HALF), 0);
   set(CSR(R_SD_GO), write);
   return 1;
}

// --- the disk's commands ------------------------------------------------------------------

static uint32_t be(int at, int n)
{
   uint32_t v = 0;
   for (int i = 0; i < n; i++)
      v = v << 8 | cdb[at + i];
   return v;
}

// The bus ID in the configuration block, as VICE's cmdhd_scsiread: the block after a system
// header (2 + 128 n) with the CMD signature at its end
static void patch_id(void)
{
   if (lba < 2 || (lba - 2) % 128)
      return;
   for (int i = 0; i < 16; i++)
      if ((get(HD_BUF + 0x1F0 + i) & 0xFF) != hd_sig[i])
         return;
   set(HD_BUF + 0x1E1, bus_id);
   set(HD_BUF + 0x1E4, bus_id);
}

static void read_next(void)
{
   if (lba >= blocks)
      fail(SK_ILLEGAL, ASC_LBA_RANGE);
   else if (!sd_start(lba, 0))
      fail(SK_MEDIUM_ERROR, 0);
   else
      st = S_RD_SD;
}

// FORMAT UNIT's search for the system header's block, every 128 blocks from 2
static void fmt_next(void)
{
   read_next();
   if (st == S_RD_SD)
      st = S_FMT_RD;
}

static void command(void)
{
   int len = get(CSR(R_STATUS)) >> 8 & 15, lun, n;
   uint32_t cap;
   for (int i = 0; i < len; i++)
      cdb[i] = get(HD_CDB + i);
   lun = cdb[1] >> 5;
   cap = lun ? 0 : blocks;

   switch (cdb[0]) {
   case 0x00:                      // TEST UNIT READY
   case 0x01:                      // REZERO UNIT
   case 0x1B:                      // START STOP UNIT
   case 0x1D:                      // SEND DIAGNOSTIC
   case 0x2F:                      // VERIFY
      if (!cap)
         fail(SK_ILLEGAL, ASC_NO_LUN);
      else
         status(0);
      break;

   case 0x03:                      // REQUEST SENSE
      n = cdb[4] ? cdb[4] : 4;
      if (n > 18)
         n = 18;
      buf_zero(18);
      set(HD_BUF + 0, 0xF0);
      set(HD_BUF + 2, sense);
      set(HD_BUF + 12, asc);
      sense = asc = 0;
      din(n);
      break;

   case 0x12: {                    // INQUIRY
      static const char id[] = "C128M65 CMD HD Image    1.0 ";   // vendor, product, revision
      n = cdb[4];
      buf_zero(n);
      set(HD_BUF + 0, cap ? 0x00 : 0x60);
      set(HD_BUF + 2, 0x01);       // SCSI 1
      set(HD_BUF + 3, 0x02);
      set(HD_BUF + 4, 92);
      for (int i = 0; i < 28; i++)
         set(HD_BUF + 8 + i, id[i]);
      din(n);
      break;
   }

   case 0x25:                      // READ CAPACITY
      buf_zero(8);
      if (cap) {
         uint32_t last = (cap > MAX_BLOCKS ? MAX_BLOCKS : cap) - 1;
         for (int i = 0; i < 4; i++)
            set(HD_BUF + i, last >> (24 - 8 * i) & 0xFF);
         set(HD_BUF + 6, 0x02);    // 512 bytes
      }
      din(8);
      break;

   case 0x1A:                      // MODE SENSE (6) and (10): page 0 or 0x20, as VICE's
   case 0x5A: {
      static const char rev[] = "C128MEGA65-SCSI";
      n = cdb[0] == 0x1A ? cdb[4] : (int)be(7, 2);
      if (n > 255)
         n = 255;
      if ((cdb[2] & 0x3F) != 0x00 && (cdb[2] & 0x3F) != 0x20) {
         fail(SK_ILLEGAL, ASC_BAD_CDB);
         break;
      }
      buf_zero(n);
      set(HD_BUF + 3, 1);
      int k = 0;
      for (; rev[k] && 7 + k < n; k++)
         set(HD_BUF + 7 + k, rev[k]);
      set(HD_BUF + 6, k);
      din(n);
      break;
   }

   case 0x08:                      // READ (6) and (10)
   case 0x28:
      if (cdb[0] == 0x08) {
         lba  = be(1, 3) & 0x1FFFFF;
         left = cdb[4] ? cdb[4] : 256;
      } else {
         lba  = be(2, 4);
         left = be(7, 2);
      }
      if (!left)
         status(0);
      else if (!cap)
         fail(SK_ILLEGAL, ASC_LBA_RANGE);
      else
         read_next();
      break;

   case 0x0A:                      // WRITE (6), (10), WRITE AND VERIFY
   case 0x2A:
   case 0x2E:
      if (cdb[0] == 0x0A) {
         lba  = be(1, 3) & 0x1FFFFF;
         left = cdb[4] ? cdb[4] : 256;
      } else {
         lba  = be(2, 4);
         left = be(7, 2);
      }
      if (!left)
         status(0);
      else if (lba >= cap)
         fail(SK_ILLEGAL, ASC_LBA_RANGE);
      else if (readonly)
         fail(SK_DATA_PROTECT, ASC_PROTECTED);
      else {
         go(GO_DOUT, 512, 0, 0);
         st = S_WR_XFER;
      }
      break;

   case 0x04:                      // FORMAT UNIT: the system's signature goes, as VICE's
      if ((cdb[1] & 0x17) == 0x10) {
         go(GO_DOUT, 4, 0, 0);     // a defect list header first, all zeros
         st = S_FMT_HDR;
      } else if ((cdb[1] & 0x17) == 0x00 && cap) {
         lba = 2;
         fmt_next();
      } else
         fail(SK_ILLEGAL, ASC_BAD_PARAMS);
      break;

   case 0x07:                      // REASSIGN BLOCKS: the list is taken, nothing done
      left = 0;                    // its header first
      go(GO_DOUT, 4, 0, 0);
      st = S_REASSIGN;
      break;

   default:
      fail(SK_ILLEGAL, ASC_BAD_OPCODE);
      break;
   }
}

// One step of the disk: never waits
static void disk_poll(void)
{
   int ev = get(CSR(R_EVENTS)), sd = st == S_RD_SD || st == S_WR_SD || st == S_FMT_RD ||
            st == S_FMT_WR ? get(CSR(R_SD_GO)) : 0;

   if (ev & EV_FREE) {             // after STATUS, or a bus reset in the middle
      set(CSR(R_EVENTS), EV_FREE);
      st = S_IDLE;
   }
   switch (st) {
   case S_IDLE:
      if (ev & EV_CMD) {
         set(CSR(R_EVENTS), EV_CMD);
         command();
      }
      break;

   case S_STATUS:
      break;

   case S_DIN:
      if (ev & EV_DONE) {
         set(CSR(R_EVENTS), EV_DONE);
         status(0);
      }
      break;

   case S_RD_SD:
      if (sd & 1)
         break;
      if (sd & 2) {
         fail(SK_MEDIUM_ERROR, 0);
         break;
      }
      patch_id();
      go(GO_DIN, 512, 0, 0);
      st = S_RD_XFER;
      break;

   case S_RD_XFER:
      if (!(ev & EV_DONE))
         break;
      set(CSR(R_EVENTS), EV_DONE);
      lba++;
      if (--left == 0)
         status(0);
      else
         read_next();
      break;

   case S_WR_XFER:
      if (!(ev & EV_DONE))
         break;
      set(CSR(R_EVENTS), EV_DONE);
      if (sd_start(lba, 1))
         st = S_WR_SD;
      else
         fail(SK_MEDIUM_ERROR, 0);
      break;

   case S_WR_SD:
      if (sd & 1)
         break;
      if (sd & 2) {
         fail(SK_MEDIUM_ERROR, 0);
         break;
      }
      lba++;
      if (--left == 0)
         status(0);
      else if (lba >= blocks)
         fail(SK_ILLEGAL, ASC_LBA_RANGE);
      else {
         go(GO_DOUT, 512, 0, 0);
         st = S_WR_XFER;
      }
      break;

   case S_FMT_HDR:
      if (!(ev & EV_DONE))
         break;
      set(CSR(R_EVENTS), EV_DONE);
      for (int i = 0; i < 4; i++)
         if (get(HD_BUF + i) & 0xFF) {
            fail(SK_ILLEGAL, ASC_BAD_PARAMS);
            return;
         }
      lba = 2;
      fmt_next();
      break;

   case S_FMT_RD: {
      if (sd & 1)
         break;
      int sig = !(sd & 2);
      for (int i = 0; sig && i < 16; i++)
         sig = (get(HD_BUF + 0x1F0 + i) & 0xFF) == hd_sig[i];
      if (sig) {
         for (int i = 0; i < 16; i++)
            set(HD_BUF + 0x1F0 + i, 0);
         if (sd_start(lba, 1)) {
            st = S_FMT_WR;
            break;
         }
      }
      lba += 128;
      if (sig || lba >= blocks)
         status(0);
      else
         fmt_next();
      break;
   }

   case S_FMT_WR:
      if (!(sd & 1))
         status(0);
      break;

   case S_REASSIGN:
      if (!(ev & EV_DONE))
         break;
      set(CSR(R_EVENTS), EV_DONE);
      if (left == 0) {             // the header: the list's length, then the list
         uint32_t n = 0;
         for (int i = 0; i < 4; i++)
            n = n << 8 | (get(HD_BUF + i) & 0xFF);
         if (n) {
            left = 1;
            go(GO_DOUT, n > 508 ? 508 : n, 0, 0);
            break;
         }
      }
      status(0);
      break;
   }
}

// --- power and the front panel ------------------------------------------------------------

static int should_run(void)
{
   return there && on && rom_ok && mounted;
}

static void ctl(int bits)
{
   set(CSR(R_CTL), bits);
}

// Held buttons at a start: as VICE's, about 4 s when the RAM was cleared (the boot ROM checks
// the hardware first), a quarter second when the DOS is in it
static void start(void)
{
   static const int mode_buttons[] = { 0, HD_BTN_WP, HD_BTN_SWAP8 | HD_BTN_SWAP9 };
   held       = mode_buttons[want_mode];
   mode       = want_mode;
   want_mode  = HD_RESET_NORMAL;
   hold_for   = dos_seen ? CPU_HZ / 4 : 4 * CPU_HZ;
   held_since = MTIME_LO;
   ctl(CTL_RUN | held);
   power = P_RUN;
}

static void power_poll(void)
{
   if (!should_run()) {
      if (power != P_OFF) {
         ctl(0);
         power = P_OFF;
         st    = S_IDLE;
      }
      mode = HD_RESET_NORMAL;
      return;
   }
   switch (power) {
   case P_OFF:
      st = S_IDLE;
      if (cold) {
         set(CSR(R_EVENTS), EV_CLEAR);
         ctl(CTL_CLEAR);
         power = P_CLEAR;
      } else
         start();
      break;
   case P_CLEAR:
      if (get(CSR(R_EVENTS)) & EV_CLEAR) {
         set(CSR(R_EVENTS), EV_CLEAR);
         cold     = 0;
         dos_seen = 0;
         ctl(0);
         start();
      }
      break;
   case P_RUN:
      if (held && MTIME_LO - held_since > hold_for) {
         held = 0;
         ctl(CTL_RUN);
      }
      if (!dos_seen || mode != HD_RESET_NORMAL) {   // HD DOS's idle loop (no boot ROM code)
         uint16_t pc = get(CSR(R_PC));
         if (pc >= 0xF870 && pc < 0xF8C0) {
            dos_seen = 1;
            mode     = HD_RESET_NORMAL;
         }
      }
      break;
   }
}

void cmdhd_reset(int mode)
{
   if (!there)
      return;
   want_mode = mode;
   ctl(0);                          // held in reset; power_poll starts it again
   power = P_OFF;
   st    = S_IDLE;
}

// --- the outside --------------------------------------------------------------------------

int cmdhd_present(void)
{
   return there;
}

void cmdhd_init(void)
{
   there = get(CSR(R_MAGIC)) == MAGIC;
   if (!there)
      return;
   runs = heap_alloc((2 * RUNS + 2) * sizeof *runs);
   ctl(0);
   cold = 1;
}

int cmdhd_rom_load(void *file)
{
   FIL *f = file;
   uint8_t b[512];
   UINT got;
   if (!there)
      return -1;
   rom_ok = 0;
   ctl(0);                          // held in reset while its ROM changes
   power = P_OFF;
   st    = S_IDLE;
   busy  = 1;
   for (uint32_t at = 0; at < HD_ROM_SIZE; at += sizeof b) {
      if (f_read(f, b, sizeof b, &got) != FR_OK || got != sizeof b) {
         busy = 0;
         return -1;
      }
      for (UINT i = 0; i < got; i++)
         set(HD_ROM + at + i, b[i]);
   }
   busy   = 0;
   rom_ok = 1;
   cold   = 1;                      // a new boot ROM starts from power-up
   cmdhd_reset(HD_RESET_NORMAL);
   return 0;
}

void cmdhd_rom_clear(void)
{
   rom_ok = 0;
   cold   = 1;
}

// The first BLANK_CHECK blocks are zeros: nothing was ever installed
static int blank(FIL *f)
{
   uint8_t b[512];
   UINT got;
   if (f_lseek(f, 0) != FR_OK)
      return 0;
   for (int k = 0; k < BLANK_CHECK; k++) {
      if (f_read(f, b, sizeof b, &got) != FR_OK || got == 0)
         return 1;
      for (UINT i = 0; i < got; i++)
         if (b[i])
            return 0;
   }
   return 1;
}

int cmdhd_mount(const char *p, const char **msg)
{
   FIL f;
   FILINFO fi;
   *msg = 0;
   if (!there) {
      *msg = "\n\nThis core has no CMD HD.\n\nPress SPACE to continue.\n";
      return 1;
   }
   busy = 1;
   if (f_stat(p, &fi) != FR_OK || f_open(&f, p, FA_READ) != FR_OK) {
      busy = 0;
      *msg = "\n\nThe image could not be opened.\n\nPress SPACE to continue.\n";
      return 1;
   }
   uint32_t size = f_size(&f);
   if (size % 512 || size < SYS_BLOCKS * 512) {
      f_close(&f);
      busy = 0;
      *msg = "\n\nThis is not a CMD HD image: its size\nmust be a multiple of 512 bytes, 72 KB\n"
             "or more.\n\nPress SPACE to continue.\n";
      return 1;
   }
   runs[0] = 2 * RUNS + 2;
   f.cltbl = runs;
   FRESULT r = f_lseek(&f, CREATE_LINKMAP);
   f.cltbl = 0;
   int empty = r == FR_OK && blank(&f);
   FATFS *vol = f.obj.fs;            // f_close forgets it
   f_close(&f);
   busy = 0;
   if (r != FR_OK) {
      *msg = r == FR_NOT_ENOUGH_CORE ?
             "\n\nThis image lies in too many pieces on\nthe card. Copy it again.\n\nPress SPACE to continue.\n" :
             "\n\nThe image could not be read.\n\nPress SPACE to continue.\n";
      return 1;
   }
   cmdhd_unmount();
   fs       = vol;
   r_first  = 0;
   r_idx    = 1;
   card     = get(CSR(R_SD_CARD)) & 0xFF;
   set(CSR(R_SD_CARD), card);
   blocks   = size / 512 > MAX_BLOCKS ? MAX_BLOCKS : size / 512;
   readonly = (fi.fattrib & AM_RDO) != 0;
   strncpy(path, p, sizeof path - 1);
   path[sizeof path - 1] = 0;
   mounted   = 1;
   cold      = 1;
   want_mode = empty ? HD_RESET_INSTALL : HD_RESET_NORMAL;
   return 0;
}

void cmdhd_unmount(void)
{
   if (!mounted)
      return;
   ctl(0);
   power   = P_OFF;
   st      = S_IDLE;
   mounted = 0;
   path[0] = 0;
}

const char *cmdhd_path(void)
{
   return mounted ? path : "";
}

void cmdhd_set_on(int v)
{
   if (on != v)
      cold = 1;                     // switched on: from power-up
   on = v;
}

void cmdhd_set_id(int id)
{
   if (bus_id == id)
      return;
   bus_id = id;
   if (power == P_RUN)
      cmdhd_reset(HD_RESET_NORMAL);  // the DOS reads it at its start
}

int cmdhd_mode(void)
{
   return mode;
}

int cmdhd_running(void)
{
   return there && (get(CSR(R_STATUS)) & 1);
}

int cmdhd_create(const char *p, uint32_t size)
{
   FIL f;
   busy = 1;
   FRESULT r = f_open(&f, p, FA_WRITE | FA_CREATE_NEW);
   if (r != FR_OK) {
      busy = 0;
      return r;
   }
   r = f_expand(&f, size, 1);
   if (r == FR_OK) {
      // zeros at the start: the drive takes it as empty and starts in install mode
      static const uint8_t zero[512];
      UINT put;
      for (int k = 0; r == FR_OK && k < BLANK_CHECK && (uint32_t)k * 512 < size; k++)
         r = f_write(&f, zero, sizeof zero, &put);
   }
   f_close(&f);
   if (r != FR_OK)
      f_unlink(p);
   busy = 0;
   return r;
}

void cmdhd_poll(void)
{
   if (!there || busy)
      return;
   power_poll();
   if (power == P_RUN)
      disk_poll();
}
