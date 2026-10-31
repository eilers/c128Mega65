// The REST API's runners routes: Gideon's software/api/route_runners.cc at 4b00732 on the
// C128.
//
// load_prg and run_prg: his boot cartridge resets the machine, DMA loads the program, fixes
// BASIC's pointers and (run) types RUN. Here the firmware does it over the bus DMA: a program
// for $0801 runs in C64 mode (GO64 first), any other in C128 mode. The C128 is reset (with
// $0A00 and $2D cleared first, so old values aren't taken for new ones), then BASIC is ready
// once $0A00 is 3 (the end of C128 BASIC's cold start) or $2B-$2E are 01 08 03 08 (C64 BASIC's
// NEW); a NUL typed into the keyboard buffer is gone once the editor waits at READY. Then,
// with the CPU held: the program, the pointers his loader sets (the mode's), the lines
// relinked (his runner's LINKPRG) and RUN in the keyboard buffer. A program's .cfg (or .usr)
// next to it is loaded first, as his. 423 if BASIC doesn't come within 10 s (a cartridge, a
// boot or CP/M disk) or C64 BASIC comes for a C128 program, at once while the Z80 runs.
//
// run_crt: the .crt into the virtual cartridge, as the menu's CRT: line (the cartridge's
// source becomes the .crt, main.vhd resets the C128 into it); an upload goes straight into
// the cartridge's memory. sidplay: no player here (501); modplay: his U2 without a sampler.
#include <string.h>
#include <strings.h>
#include "c128dma.h"
#include "config_ids.h"
#include "core.h"
#include "fmt.h"
#include "heap.h"
#include "m2m_rv.h"
#include "net.h"
#include "rest_priv.h"
#include "roms.h"
#include "stores.h"
#include "uci.h"

#define CRT_DEVICE   0x0106            // C_DEV_CRT; its CSR $020: the largest .crt, in 64 KB
#define CRT_MAX_HI   0x020
#define READY_LIMIT  10000             // ms for BASIC's READY
#define POLL_MS      10

enum { MODE_C128, MODE_C64 };
enum { R_POISON, R_RESET, R_WAIT, R_NUL, R_READY, R_GO64, R_LOAD, R_POKE };

// A request's runner: what it loads, how far it is, and the pokes after the program
typedef struct {
   int      mode, run, typed, in64, at, npokes, poke;   // in64: C64 BASIC is up
   uint16_t load, end;
   int      len;
   uint8_t *prg;                       // the program's bytes after its load address
   struct { uint16_t addr; uint8_t len, d[8]; } pokes[12];
   uint8_t  scratch[8];
} runner_t;

static runner_t *runners;               // a connection's, in HyperRAM

static runner_t *runner_of(rest_t *r)
{
   if (!runners)
      runners = heap_alloc(HTTP_CLIENTS * sizeof *runners);
   return &runners[r->req->slot];
}

static void poke(runner_t *u, uint16_t addr, const char *d, int len)
{
   u->pokes[u->npokes].addr = addr;
   u->pokes[u->npokes].len  = len;
   memcpy(u->pokes[u->npokes].d, d, len);
   u->npokes++;
}

// --- the program --------------------------------------------------------------------------

// His runner's LINKPRG (C64 $A533; BASIC 7's is the same): each line's link to the byte after
// its end, from the start of BASIC's text, while a link's high byte isn't 0
static void relink(runner_t *u, uint16_t txttab)
{
   int p = txttab - u->load;
   while (p >= 0 && p + 5 < u->len && u->prg[p + 1]) {
      int y = 5;
      while (p + y < u->len && u->prg[p + y])
         y++;
      if (p + y >= u->len)
         return;
      int next = p + y + 1;
      uint16_t a = u->load + next;
      u->prg[p]     = a & 0xFF;
      u->prg[p + 1] = a >> 8;
      p = next;
   }
}

// The program in r->up (n bytes, from its load address on; a P00's header skipped by
// name), the mode, and the pointers and keys to set after it
static void prepare(rest_t *r, const char *name, int n, int run)
{
   runner_t *u = runner_of(r);
   uint8_t *data = r->up;
   const char *dot = name ? strrchr(name, '.') : NULL;
   // A P00..P99: its 26-byte header first (his x00_skip_header)
   if (dot && (dot[1] | 0x20) == 'p' && dot[2] >= '0' && dot[2] <= '9' && dot[3] >= '0' &&
       dot[3] <= '9' && !dot[4] && n >= 26 && !memcmp(data, "C64File", 8)) {
      data += 26;
      n -= 26;
   }
   memset(u, 0, sizeof *u);
   u->run = run;
   u->len = -1;                                   // fewer than 2 bytes: nothing to load
   if (n >= 2) {
      u->load = data[0] | data[1] << 8;
      u->len  = n - 2;
      if (u->len > 65536 - u->load)
         u->len = 65536 - u->load;                 // his: never past $FFFF
      u->prg  = data + 2;
      u->end  = u->load + u->len;
   }
   u->mode = u->len >= 0 && u->load == 0x0801 ? MODE_C64 : MODE_C128;

   char e[2] = { u->end & 0xFF, u->end >> 8 };
   if (u->len >= 0) {
      if (u->mode == MODE_C64) {
         relink(u, 0x0801);
         char three[6] = { e[0], e[1], e[0], e[1], e[0], e[1] };
         poke(u, 0x002D, three, 6);                // VARTAB, ARYTAB, STREND
         poke(u, 0x0035, "\x00\xA0", 2);           // FRESPC
      } else {
         relink(u, 0x1C01);
         poke(u, 0x1210, e, 2);                    // TEXT_TOP
      }
      poke(u, 0x00AE, e, 2);                       // the load's end
      poke(u, 0x0090, "\x40", 1);                  // its status: end of file
   }
   poke(u, 0x00BA, "\x08", 1);                     // the device his loader prints
   if (run) {
      poke(u, u->mode == MODE_C64 ? 0x0277 : 0x034A, "RUN\r", 4);
      poke(u, u->mode == MODE_C64 ? 0x00C6 : 0x00D0, "\x04", 1);
   }
}

// --- the steps ----------------------------------------------------------------------------

// A DMA job of the runner's: 1 once it's over; *ok whether it went through
static int dma(rest_t *r, int *ok)
{
   if (!job_step(r))
      return 0;
   *ok = r->job.pos == r->job.len;
   return 1;
}

static void dma_start_soft(rest_t *r, uint16_t addr, int len, int write, uint8_t *buf)
{
   job_begin(r, addr, len, write, buf);
   r->job.soft = 1;
}

static void answer(rest_t *r, int ss)
{
   dma_keep(0);
   subsys_response(r, ss);
}

static int run_step(rest_t *r)
{
   runner_t *u = runner_of(r);
   uint32_t now = net_millis();
   int ok;

   switch (r->phase) {
   case R_POISON:                                  // $0A00 then $002D cleared, CPU held
      if (!dma(r, &ok))
         return 0;
      if (!ok) {
         answer(r, SS_NO_LOCK);
         return 1;
      }
      if (++u->at < 2) {
         dma_start_soft(r, 0x002D, 1, 1, u->scratch);
         return 0;
      }
      uci_reboot();                                // the hold ends with the reset
      r->phase = R_RESET;
      r->t0 = now;
      return 0;

   case R_RESET:                                   // the reset happened: the hold is gone
   case R_WAIT:                                    // $0A00, $2B-$2E, a poll every 10 ms
   case R_NUL:                                     // the NUL typed: KEYD, then NDX
   case R_READY:                                   // NDX polled until the editor took it
      if (now - r->t0 > READY_LIMIT) {
         answer(r, SS_NO_LOCK);
         return 1;
      }
      if (r->phase == R_RESET) {
         if (dma_held())                           // still held: the reset is yet to come
            return 0;
         dma_keep(0);
         r->phase = R_WAIT;
         u->at = -1;
         return 0;
      }
      if (u->at < 0) {                             // a new poll, once due and the 8502 runs
         if (now - r->job.t0 < POLL_MS || dma_z80())
            return 0;
         u->at = 0;
         if (r->phase == R_WAIT)
            dma_start_soft(r, 0x0A00, 1, 0, u->scratch);
         else if (r->phase == R_NUL)
            dma_start_soft(r, u->in64 ? 0x0277 : 0x034A, 1, 1, (uint8_t *)"");
         else
            dma_start_soft(r, u->in64 ? 0x00C6 : 0x00D0, 1, 0, u->scratch);
      }
      if (!dma(r, &ok))
         return 0;
      if (!ok) {
         u->at = -1;                               // the reset's or the Z80's time: again
         return 0;
      }
      if (r->phase == R_WAIT && u->at == 0) {
         u->at = 1;
         dma_start_soft(r, 0x002B, 4, 0, u->scratch + 1);
         return 0;
      }
      if (r->phase == R_NUL && u->at == 0) {
         u->at = 1;
         static uint8_t one = 1;
         dma_start_soft(r, u->in64 ? 0x00C6 : 0x00D0, 1, 1, &one);
         return 0;
      }
      u->at = -1;
      if (r->phase == R_WAIT) {
         int c128 = u->scratch[0] == 3;
         int c64 = !memcmp(u->scratch + 1, "\x01\x08\x03\x08", 4);
         u->in64 = c64;
         if (u->mode == MODE_C64 && c64)
            r->phase = R_NUL;
         else if (c128 && !u->typed)
            r->phase = R_NUL;                      // C128 BASIC: the program, or GO64 first
         else if (u->mode == MODE_C128 && c64) {
            answer(r, SS_NO_LOCK);                 // C64 BASIC (a C64 cartridge): not ours
            return 1;
         }
         return 0;
      }
      if (r->phase == R_NUL) {
         r->phase = R_READY;
         return 0;
      }
      if (u->scratch[0])                           // R_READY: not taken yet
         return 0;
      if (u->mode == MODE_C64 && !u->in64) {
         u->typed = 1;                             // at C128 BASIC's READY: GO64, Y
         r->phase = R_GO64;
         u->at = 0;
         dma_start_soft(r, 0x034A, 7, 1, (uint8_t *)"GO64\rY\r");
         return 0;
      }
      dma_keep(1);                                 // at READY: the program, held
      r->phase = R_LOAD;
      if (u->len > 0)
         dma_start_soft(r, u->load, u->len, 1, u->prg);
      else
         r->job.pos = r->job.len = 0;
      return 0;

   case R_GO64:
      if (!dma(r, &ok))
         return 0;
      if (!ok) {
         answer(r, SS_NO_LOCK);
         return 1;
      }
      if (u->at++ == 0) {
         static uint8_t seven = 7;
         dma_start_soft(r, 0x00D0, 1, 1, &seven);
         return 0;
      }
      r->phase = R_WAIT;                           // then C64 BASIC
      u->at = -1;
      r->t0 = now;
      return 0;

   case R_LOAD:
   case R_POKE:
      if (r->job.len && !dma(r, &ok))
         return 0;
      if (r->job.len && !ok) {
         answer(r, SS_NO_LOCK);
         return 1;
      }
      if (r->phase == R_LOAD) {
         r->phase = R_POKE;
         u->poke = 0;
      } else
         u->poke++;
      if (u->poke < u->npokes) {
         dma_start_soft(r, u->pokes[u->poke].addr, u->pokes[u->poke].len, 1, u->pokes[u->poke].d);
         return 0;
      }
      answer(r, SS_OK);
      return 1;
   }
   return 1;
}

static void run_cancel(rest_t *r)
{
   job_cancel(r);
   dma_keep(0);
}

// His start_prg: the program's .cfg, or its .usr, first; then the runner
static void start_prg(rest_t *r, const char *name, int n, int run)
{
   if (dma_z80()) {
      subsys_response(r, SS_NO_LOCK);
      return;
   }
   prepare(r, name, n, run);
   runner_t *u = runner_of(r);
   r->phase = R_POISON;
   u->at = 0;
   dma_keep(1);
   dma_start_soft(r, 0x0A00, 1, 1, u->scratch);   // scratch is all 0 here
   rest_pending(r, run_step, run_cancel);
}

static void no_log(const char *fmt, ...)
{
   (void)fmt;
}

// His S_load_associated_config(_usr): NAME.cfg, else NAME.usr, next to the program. As his
// S_load_and_effectuate, its good lines take effect even if others have errors.
static void associated_config(const char *card)
{
   static const char *const ext[] = { ".cfg", ".usr" };
   char cfg[UCI_PATH_MAX + 4];
   FIL f;
   for (int i = 0; i < 2; i++) {
      fmt_snprintf(cfg, sizeof cfg, "%s", card);
      char *dot = strrchr(cfg, '.'), *slash = strrchr(cfg, '/');
      if (dot && (!slash || dot > slash))
         *dot = 0;
      strcat(cfg, ext[i]);
      if (f_open(&f, cfg, FA_READ) != FR_OK)
         continue;
      stores_begin();
      stores_read(&f, ~0u, no_log);
      f_close(&f);
      stores_apply();
      return;
   }
}

static void put_prg(rest_t *r, int run)
{
   char card[UCI_PATH_MAX];
   const char *c, *file = arg(r, "file");
   FIL f;
   UINT got;
   if (!(c = uci_card_path(file, card)) || f_open(&f, c, FA_READ) != FR_OK) {
      subsys_response(r, SS_CANNOT_OPEN_FILE);
      return;
   }
   if (!r->up)
      r->up = heap_alloc(UPLOAD_MAX + 1);
   FRESULT res = f_read(&f, r->up, UPLOAD_MAX, &got);
   f_close(&f);
   if (res != FR_OK) {
      subsys_response(r, SS_CANNOT_OPEN_FILE);
      return;
   }
   associated_config(c);
   start_prg(r, c, got, run);
}

static void post_prg(rest_t *r, int run)
{
   if (!r->up_parts) {
      subsys_response(r, SS_CANNOT_OPEN_FILE);   // his start_prg of no file
      return;
   }
   start_prg(r, r->up_name, r->up_len, run);
}

static void put_load_prg(rest_t *r) { put_prg(r, 0); }
static void put_run_prg(rest_t *r) { put_prg(r, 1); }
static void post_load_prg(rest_t *r) { post_prg(r, 0); }
static void post_run_prg(rest_t *r) { post_prg(r, 1); }

// --- run_crt ------------------------------------------------------------------------------

// The CRT:%s item's number among the manually loaded ones
static int crt_item(void)
{
   for (int k = 0; k < crtrom_count(); k++)
      if (crtrom_dev(k) == CRT_DEVICE)
         return k;
   return -1;
}

// The menu's ways: its source the .crt (core.c), then the file; main.vhd resets the C128
static int crt_answer(int err)
{
   if (!err)
      return SS_OK;
   return err == 1 ? SS_ROM_IMAGE_TOO_LARGE : SS_ERROR_IN_FILE_FORMAT;
}

static void put_run_crt(rest_t *r)
{
   char card[UCI_PATH_MAX];
   const char *c, *msg;
   FILINFO fi;
   int k = crt_item();
   if (k < 0) {
      subsys_response(r, SS_SUBSYS_NOT_PRESENT);
      return;
   }
   if (!(c = uci_card_path(arg(r, "file"), card)) || f_stat(c, &fi) != FR_OK || (fi.fattrib & AM_DIR)) {
      subsys_response(r, SS_CANNOT_OPEN_FILE);
      return;
   }
   core_sel_pre(OPTM_G_MOUNT_CRT, 0);
   int ss = crt_answer(crtrom_load_quiet(k, c, OPTM_G_MOUNT_CRT, &msg));
   shell_crtrom_named(OPTM_G_MOUNT_CRT, ss == SS_OK ? c : NULL);
   subsys_response(r, ss);
}

// The upload as it comes, into the cartridge's memory: a byte a word, as roms.c's copy_load
static void crt_sink(rest_t *r, mp_event_t ev, const uint8_t *data, int len)
{
   int k = crt_item();
   uint32_t max = (uint32_t)crtrom_csr_get(CRT_DEVICE, CRT_MAX_HI) << 16;
   if (k < 0)
      return;
   switch (ev) {
   case MP_DATA_START:
   case MP_SUBHEADER:
      core_sel_pre(OPTM_G_MOUNT_CRT, 0);
      crtrom_begin(k);
      r->up_at = 0;
      break;
   case MP_DATA:
      M2M_RAMROM_DEV = CRT_DEVICE;
      for (int i = 0; i < len && r->up_at < max; i++)
         M2M_DEV16[r->up_at++] = data[i];
      break;
   default:
      break;
   }
}

static void post_run_crt(rest_t *r)
{
   const char *msg;
   int k = crt_item();
   if (k < 0) {
      subsys_response(r, SS_SUBSYS_NOT_PRESENT);
      return;
   }
   if (!r->up_parts) {
      subsys_response(r, SS_CANNOT_OPEN_FILE);
      return;
   }
   int type, ss;
   if (core_prep_load(r->up_size, CTX_LOAD_ROM, OPTM_G_MOUNT_CRT, &type, &msg))
      ss = SS_ROM_IMAGE_TOO_LARGE;
   else
      ss = crt_answer(crtrom_parse(k, r->up_size, &msg));
   shell_crtrom_named(OPTM_G_MOUNT_CRT, ss == SS_OK ? (*r->up_name ? r->up_name : "upload.crt") : NULL);
   subsys_response(r, ss);
}

// --- no counterpart -----------------------------------------------------------------------

static void drop(rest_t *r, mp_event_t ev, const uint8_t *data, int len)
{
   (void)r, (void)ev, (void)data, (void)len;
}

static void sidplay(rest_t *r)
{
   subsys_response(r, SS_NOT_IMPLEMENTED);
}

// His U2 without a sampler
static void modplay(rest_t *r)
{
   error(r, "Sampler module not available");
   json_response(r, HTTP_NOT_IMPLEMENTED);
}

static const param_t file_params[]  = { { "file", P_REQUIRED } };
static const param_t sid_params[]   = { { "file", P_REQUIRED }, { "songnr", 0 } };
static const param_t songnr_params[] = { { "songnr", 0 } };

const call_t rest_runners_calls[] = {
   CALL(HTTP_PUT, "runners", "sidplay", sidplay, sid_params, 0),
   CALLS(HTTP_POST, "runners", "sidplay", sidplay, songnr_params, drop),
   CALL(HTTP_PUT, "runners", "load_prg", put_load_prg, file_params, 0),
   CALL(HTTP_PUT, "runners", "run_prg", put_run_prg, file_params, 0),
   CALLU(HTTP_POST, "runners", "load_prg", post_load_prg, UPLOAD_MAX),
   CALLU(HTTP_POST, "runners", "run_prg", post_run_prg, UPLOAD_MAX),
   CALL(HTTP_PUT, "runners", "run_crt", put_run_crt, file_params, 0),
   CALLS0(HTTP_POST, "runners", "run_crt", post_run_crt, crt_sink),
   CALL(HTTP_PUT, "runners", "modplay", modplay, file_params, 0),
   CALLS0(HTTP_POST, "runners", "modplay", modplay, drop),
};
const int rest_runners_count = sizeof rest_runners_calls / sizeof rest_runners_calls[0];
