// The REST API's machine routes: Gideon's software/api/route_machine.cc at 4b00732 (the
// II+L's 3.15a has the same) with his U2's answers, on the C128. His C64_DMA_RAW_READ and
// _WRITE go over the core's DMA port (c128dma.c): memory as the C128's CPU sees it then,
// with the CPU held for the whole of a request, as his stop() holds it. Reset and reboot are
// the reset button, as UCI's REBOOT; the menu is the On-Screen-Menu.
#include <string.h>
#include "c128dma.h"
#include "core.h"
#include "fmt.h"
#include "heap.h"
#include "net.h"
#include "rest_priv.h"
#include "uci.h"

#define JOB_TIMEOUT 50                 // ms: a DMA job of 2 KB takes 2 ms

static const char locked[] = "Could not obtain lock of subsystem";   // his SSRET_NO_LOCK

static struct {
   rest_t *owner;                       // the request whose job has the DMA
   int     paused;                      // machine:pause
} m;

// A C128 reset clears the hardware's hold: a pause is over then
static void synced(void)
{
   if (m.paused && !dma_held())
      m.paused = 0;
}

static void hold(void)
{
   dma_hold(m.paused || m.owner);
}

// --- his chartohex and parse_hex ----------------------------------------------------------

static int chartohex(char a)
{
   if (a >= '0' && a <= '9')
      return a - '0';
   if (a >= 'A' && a <= 'F')
      return 10 + a - 'A';
   if (a >= 'a' && a <= 'f')
      return 10 + a - 'a';
   return 0xFF;
}

// Hex digits, no larger than limit, and nothing else
static int parse_hex(const char *text, int limit, int *value)
{
   int parsed = 0;
   if (!text || !*text)
      return 0;
   for (const char *p = text; *p; p++) {
      int digit = chartohex(*p);
      if (digit == 0xFF || parsed > limit >> 4)
         return 0;
      parsed = parsed << 4 | digit;
   }
   *value = parsed;
   return 1;
}

// --- the DMA job: his dma_load_raw_buffer, a chunk a round --------------------------------

static void job_end(rest_t *r)
{
   if (m.owner == r) {
      synced();
      m.owner = NULL;
      hold();
   }
}

static void job_cancel(rest_t *r)
{
   if (m.owner == r)
      dma_stop();
   job_end(r);
}

// 1 once the job is over (r->job.pos == r->job.len, or not: it failed and answered)
static int job_step(rest_t *r)
{
   rest_job_t *j = &r->job;
   if (!m.owner) {
      if (dma_z80()) {
         error(r, locked);
         json_response(r, HTTP_LOCKED);
         return 1;
      }
      synced();
      m.owner = r;
      j->chunk = 0;
      hold();
   }
   if (m.owner != r)
      return 0;                         // another request's job first
   if (j->chunk) {
      if (dma_busy()) {
         if (net_millis() - j->t0 < JOB_TIMEOUT)
            return 0;
         job_cancel(r);
         error(r, locked);
         json_response(r, HTTP_LOCKED);
         return 1;
      }
      if (!j->write)
         for (int i = 0; i < j->chunk; i++)
            r->mem[j->pos + i] = dma_get(i);
      j->pos += j->chunk;
      j->chunk = 0;
   }
   if (j->pos == j->len) {
      job_end(r);
      return 1;
   }
   j->chunk = j->len - j->pos < DMA_MAX ? j->len - j->pos : DMA_MAX;
   if (j->write)
      for (int i = 0; i < j->chunk; i++)
         dma_put(i, r->up[j->pos + i]);
   dma_start(j->addr + j->pos, j->chunk, j->write);
   j->t0 = net_millis();
   return 0;
}

static int write_step(rest_t *r)
{
   if (!job_step(r))
      return 0;
   if (r->job.pos == r->job.len)
      json_response(r, HTTP_OK);
   return 1;
}

static int read_step(rest_t *r)
{
   if (!job_step(r))
      return 0;
   if (r->job.pos == r->job.len)
      binary_response(r, r->mem, r->job.len);
   return 1;
}

static void job(rest_t *r, int addr, int len, int write)
{
   r->job.addr  = addr;
   r->job.len   = len;
   r->job.pos   = 0;
   r->job.chunk = 0;
   r->job.write = write;
   if (!write && !r->mem)
      r->mem = heap_alloc(65536);
   rest_pending(r, write ? write_step : read_step, job_cancel);
}

// --- the routes ---------------------------------------------------------------------------

// His C64_PUSH_BUTTON: the menu opens, or closes
static void put_menu_button(rest_t *r)
{
   shell_menu_button();
   json_response(r, HTTP_OK);
}

// A pause ends with the reset; the cartridge starts again with it either way
static void put_reset(rest_t *r)
{
   m.paused = 0;
   hold();
   uci_reboot();
   json_response(r, HTTP_OK);
}

static void put_pause(rest_t *r)
{
   synced();
   m.paused = 1;
   hold();
   json_response(r, HTTP_OK);
}

static void put_resume(rest_t *r)
{
   m.paused = 0;
   hold();
   json_response(r, HTTP_OK);
}

// His U2's SSRET_NOT_IMPLEMENTED
static void put_poweroff(rest_t *r)
{
   error(r, "This command is not supported on this architecture");
   json_response(r, HTTP_NOT_IMPLEMENTED);
}

static int address(rest_t *r, int *a)
{
   if (parse_hex(arg(r, "address"), 0xFFFF, a))
      return 1;
   error(r, "Invalid address");
   json_response(r, HTTP_BAD_REQUEST);
   return 0;
}

static void range(rest_t *r, int a, int len)
{
   char text[24];
   fmt_snprintf(text, sizeof text, "%04x-%04x", a, a + len - 1);
   add_str(r, "address", text);
}

static void put_writemem(rest_t *r)
{
   const char *data = arg(r, "data");
   int a, len = strlen(data) >> 1;
   if (!address(r, &a))
      return;
   if (len > 128) {
      error(r, "Maximum length of 128 bytes exceeded. Consider using POST method with attachment.");
      json_response(r, HTTP_BAD_REQUEST);
      return;
   }
   if (len < 1) {
      error(r, "Use this API call to write at least one byte!");
      json_response(r, HTTP_BAD_REQUEST);
      return;
   }
   if (a + len > 65536) {
      error(r, "Memory write exceeds location $FFFF");
      json_response(r, HTTP_BAD_REQUEST);
      return;
   }
   if (!r->up)
      r->up = heap_alloc(UPLOAD_MAX + 1);
   for (int i = 0; i < len; i++, data += 2) {
      int hi = chartohex(data[0]), lo = chartohex(data[1]);
      if (hi == 0xFF || lo == 0xFF) {
         int at = hi == 0xFF ? 0 : 1;
         error(r, "Invalid char '%c' at position %d.", data[at], 2 * i + at);
         json_response(r, HTTP_BAD_REQUEST);
         return;
      }
      r->up[i] = hi << 4 | lo;
   }
   range(r, a, len);
   job(r, a, len, 1);
}

// The body's first part, as much as fits below $10000's 64 KB
static void post_writemem(rest_t *r)
{
   int a;
   if (!address(r, &a))
      return;
   if (!r->up_parts) {
      error(r, "Could not read data from attachment");
      json_response(r, HTTP_NOT_FOUND);
      return;
   }
   if (a + r->up_len > 65536) {
      error(r, "Memory write exceeds location $FFFF");
      json_response(r, HTTP_BAD_REQUEST);
      return;
   }
   range(r, a, r->up_len);
   job(r, a, r->up_len, 1);
}

static void get_readmem(rest_t *r)
{
   int a, len = arg_int(r, "length", 256);
   if (!address(r, &a))
      return;
   if (len < 1 || len > 65536) {
      error(r, "Invalid length");
      json_response(r, HTTP_BAD_REQUEST);
      return;
   }
   if (a + len > 65536) {
      error(r, "Memory read exceeds location $FFFF");
      json_response(r, HTTP_BAD_REQUEST);
      return;
   }
   job(r, a, len, 0);
}

// What the menu shows, as 40 x 25 screen codes and colours (shell.c)
static void get_menu_screen(rest_t *r)
{
   if (!r->mem)
      r->mem = heap_alloc(65536);
   if (!shell_menu_screen(r->mem)) {
      error(r, "Menu screen unavailable.");
      json_response(r, HTTP_NOT_FOUND);
      return;
   }
   binary_response(r, r->mem, 2000);
}

// No bus measurement in this core
static void get_measure(rest_t *r)
{
   error(r, "The current FPGA build does not support timing measurement of the cartridge bus.");
   json_response(r, HTTP_NOT_IMPLEMENTED);
}

// The firmware's HyperRAM heap: it never frees, so free is also the least it ever had
static void get_heap(rest_t *r)
{
   add_int(r, "free", heap_free());
   add_int(r, "min_ever_free", heap_free());
   add_int(r, "total", heap_total());
   json_response(r, HTTP_OK);
}

static const param_t writemem_params[] = { { "address", P_REQUIRED }, { "data", P_REQUIRED } };
static const param_t upload_params[]   = { { "address", P_REQUIRED } };
static const param_t readmem_params[]  = { { "address", P_REQUIRED }, { "length", 0 } };

const call_t rest_machine_calls[] = {
   CALL0(HTTP_PUT, "machine", "menu_button", put_menu_button),
   CALL0(HTTP_PUT, "machine", "reset", put_reset),
   CALL0(HTTP_PUT, "machine", "reboot", put_reset),
   CALL0(HTTP_PUT, "machine", "pause", put_pause),
   CALL0(HTTP_PUT, "machine", "resume", put_resume),
   CALL0(HTTP_PUT, "machine", "poweroff", put_poweroff),
   CALL(HTTP_PUT, "machine", "writemem", put_writemem, writemem_params, 0),
   CALL(HTTP_POST, "machine", "writemem", post_writemem, upload_params, UPLOAD_MAX),
   CALL(HTTP_GET, "machine", "readmem", get_readmem, readmem_params, 0),
   CALL0(HTTP_GET, "machine", "menu_screen", get_menu_screen),
   CALL0(HTTP_GET, "machine", "measure", get_measure),
   CALL0(HTTP_GET, "machine", "heap", get_heap),
};
const int rest_machine_count = sizeof rest_machine_calls / sizeof rest_machine_calls[0];
