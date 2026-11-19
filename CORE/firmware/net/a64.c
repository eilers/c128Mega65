// Assembly 64's client, on lwIP's raw API. The server answers only clients it knows, and
// "Ultimate" is the one with the whole database, so that is the Client-Id the requests
// carry. lwIP's callbacks put what comes into a ring; a64_poll, from the main loop, reads
// the answer out of it (the header, then the body by its length, in chunks, or until the
// server closes) into the buffer or onto the card. A file is written as NAME.part and
// renamed once it is whole, so a download cut short leaves nothing behind under its name.
//
// The server is far away, and it sends a whole TCP window each round trip, in one burst
// that has to wait in the MAC's receive buffer. So the window is what that buffer holds
// (ETH_RXKB): 20 segments with 32 KB, lwIP's two with the 4 KB of older cores. What has
// come is read a few KB at a time, so the window opens again while the card is written.
// Ported from the 1541 Ultimate firmware (github.com/GideonZ/1541ultimate), Copyright
// Gideon Zweijtzer and contributors, GPL v3 as this core; changed for c128Mega65 in 2026.
#include <string.h>
#include <strings.h>
#include "a64.h"
#include "eth.h"
#include "ff.h"
#include "fmt.h"
#include "heap.h"
#include "log.h"
#include "net.h"
#include "net_hw.h"
#include "lwip/dns.h"
#include "lwip/tcp.h"

#define HOSTNAME     "hackerswithstyle.se"
#define HOSTPORT     80
#define URL_SEARCH   "/leet/search/aql"
#define URL_PRESETS  "/leet/search/aql/presets"
#define URL_ENTRIES  "/leet/search/entries"
#define URL_DOWNLOAD "/leet/search/bin"

#define RING      (22 * TCP_MSS)        // what came and isn't read yet: a window at most
#define FRAME     1518                  // a full segment in the MAC's buffer
#define STEP      4096                  // read from the ring at a time, then the window opens
#define QUIET_MS  15000                 // without a byte for so long: given up
#define REQ_MAX   1536
#define HLINE_MAX  128

_Static_assert(RING >= TCP_WND && RING < 65536, "the ring holds a window, which fits 16 bits");

enum { P_STATUS, P_HEADER, P_BODY, P_CHUNK_SIZE, P_CHUNK_DATA, P_CHUNK_END };

static struct {
   int             state;
   const char     *why;
   struct tcp_pcb *pcb;
   uint8_t         up, fin, gone;       // connected; the server closed; lwIP dropped it
   unsigned        gen;                 // the request a name's answer is for
   uint16_t        head, len;           // the ring
   uint16_t        wnd;                 // the window
   uint32_t        since;               // the last progress
   int             phase, status, line_n;
   int             chunked;
   int32_t         total, left, got;    // Content-Length (-1: none); of the body, or the chunk
   char           *buf;                 // a JSON answer's
   int             size;
   int             fopen;
   ip_addr_t       addr;
} c;

// In HyperRAM
static struct aux {
   FIL     f;
   char    req[REQ_MAX], line[HLINE_MAX], file[260], part[266], text[64];
   uint8_t ring[RING];
} *x;

// --- the connection -----------------------------------------------------------------------

// Under net_lock, or in a turn
static void drop(int graceful)
{
   struct tcp_pcb *pcb = c.pcb;
   c.pcb = NULL;
   if (!pcb)
      return;
   tcp_arg(pcb, NULL);
   tcp_recv(pcb, NULL);
   tcp_err(pcb, NULL);
   if (!graceful || tcp_close(pcb) != ERR_OK)
      tcp_abort(pcb);
}

static void end(int state, const char *why)
{
   uint32_t dev = net_lock();
   drop(state == A64_DONE);
   net_unlock(dev);
   if (c.fopen) {
      c.fopen = 0;
      FRESULT r = f_close(&x->f);
      if (state == A64_DONE && r == FR_OK) {
         f_unlink(x->file);
         r = f_rename(x->part, x->file);
      }
      if (state != A64_DONE || r != FR_OK)
         f_unlink(x->part);
      if (state == A64_DONE && r != FR_OK) {
         state = A64_FAILED;
         why = "The SD card could not be written.";
      }
   }
   c.gen++;                             // a name's answer still to come isn't ours any more
   c.state = state;
   c.why = why;
   if (state == A64_FAILED)
      log_printf("a64 failed: %s", why);
}

static err_t on_recv(void *arg, struct tcp_pcb *pcb, struct pbuf *p, err_t err)
{
   (void)arg, (void)pcb, (void)err;
   if (!p) {
      c.fin = 1;
      return ERR_OK;
   }
   if (c.len + p->tot_len > RING)
      return ERR_MEM;                   // can't happen: the window is smaller
   for (struct pbuf *q = p; q; q = q->next) {
      const uint8_t *d = q->payload;
      for (int n = q->len; n;) {
         int at = (c.head + c.len) % RING, k = RING - at < n ? RING - at : n;
         memcpy(x->ring + at, d, k);
         c.len += k;
         d += k;
         n -= k;
      }
   }
   pbuf_free(p);
   return ERR_OK;
}

static void on_err(void *arg, err_t err)
{
   (void)arg, (void)err;
   c.pcb = NULL;
   c.gone = 1;
}

static err_t on_connected(void *arg, struct tcp_pcb *pcb, err_t err)
{
   (void)arg;
   if (err != ERR_OK)
      return ERR_OK;
   if (tcp_write(pcb, x->req, strlen(x->req), TCP_WRITE_FLAG_COPY) != ERR_OK) {
      drop(0);
      c.gone = 1;
      return ERR_ABRT;
   }
   tcp_output(pcb);
   c.up = 1;
   return ERR_OK;
}

static void connect_to(const ip_addr_t *addr)
{
   struct tcp_pcb *pcb = tcp_new();
   if (!pcb) {
      c.gone = 1;
      return;
   }
   c.pcb = pcb;
   tcp_rcv_wnd_max(pcb, c.wnd);
   tcp_recv(pcb, on_recv);
   tcp_err(pcb, on_err);
   if (tcp_connect(pcb, addr, HOSTPORT, on_connected) != ERR_OK) {
      drop(0);
      c.gone = 1;
   }
}

static void on_dns(const char *name, const ip_addr_t *addr, void *arg)
{
   (void)name;
   if ((unsigned)(uintptr_t)arg != c.gen || c.state != A64_BUSY)
      return;                           // an earlier request's
   if (!addr) {
      c.gone = 2;
      return;
   }
   connect_to(addr);
}

// The window: the full segments the MAC's receive buffer holds, less one for what else
// comes in meanwhile; lwIP's own where that is all it holds
static uint16_t window(void)
{
   uint32_t was = M2M_RAMROM_DEV;
   M2M_RAMROM_DEV = NET_DEVICE;
   unsigned kb = ETH_RXKB;
   M2M_RAMROM_DEV = was;
   int n = (kb ? kb * 1024 : 4096) / FRAME - 1;
   if (n > RING / TCP_MSS)
      n = RING / TCP_MSS;
   return n * TCP_MSS > TCP_WND ? n * TCP_MSS : TCP_WND;
}

// --- the answer ---------------------------------------------------------------------------

// The body's bytes, as they come
static void body(const uint8_t *d, int n)
{
   if (c.buf) {
      if (c.got + n >= c.size) {
         end(A64_FAILED, "The answer is longer than expected.");
         return;
      }
      memcpy(c.buf + c.got, d, n);
   } else {
      UINT put;
      if (f_write(&x->f, d, n, &put) != FR_OK || (int)put != n) {
         end(A64_FAILED, "The SD card could not be written.");
         return;
      }
   }
   c.got += n;
}

static void done(void)
{
   if (c.buf)
      c.buf[c.got] = 0;
   log_printf("a64 done: %d bytes", (int)c.got);
   end(A64_DONE, NULL);
}

// A header's line, or a chunk's size
static void line(const char *s)
{
   switch (c.phase) {
   case P_STATUS: {
      const char *sp = strchr(s, ' ');
      c.status = 0;
      for (sp = sp ? sp + 1 : ""; *sp >= '0' && *sp <= '9'; sp++)
         c.status = c.status * 10 + *sp - '0';
      c.phase = P_HEADER;
      break;
   }
   case P_HEADER:
      if (!strncasecmp(s, "Content-Length:", 15)) {
         const char *p = s + 15;
         while (*p == ' ')
            p++;
         for (c.total = 0; *p >= '0' && *p <= '9'; p++)
            c.total = c.total * 10 + *p - '0';
      } else if (!strncasecmp(s, "Transfer-Encoding:", 18)) {
         for (const char *p = s + 18; *p; p++)
            if (!strncasecmp(p, "chunked", 7))
               c.chunked = 1;
      } else if (!*s) {                 // the header's end
         if (c.status != 200) {
            fmt_snprintf(x->text, sizeof x->text, "Assembly 64 answered with error %d.", c.status);
            end(A64_FAILED, x->text);
         } else if (c.chunked) {
            c.total = -1;
            c.phase = P_CHUNK_SIZE;
         } else {
            c.left = c.total;           // -1: until the server closes
            c.phase = P_BODY;
            if (!c.left)
               done();
         }
      }
      break;
   case P_CHUNK_SIZE: {
      int32_t n = 0;
      for (; *s; s++) {
         int v = *s >= '0' && *s <= '9' ? *s - '0' : (*s | 0x20) >= 'a' && (*s | 0x20) <= 'f' ?
                 (*s | 0x20) - 'a' + 10 : -1;
         if (v < 0)
            break;                      // an extension after it
         n = n << 4 | v;
      }
      if (!n) {
         done();                        // the last chunk: no trailers asked for
         break;
      }
      c.left = n;
      c.phase = P_CHUNK_DATA;
      break;
   }
   case P_CHUNK_END:                    // the line end after a chunk's bytes
      c.phase = P_CHUNK_SIZE;
      break;
   }
}

// n bytes of the answer, in order
static void take(const uint8_t *d, int n)
{
   while (n && c.state == A64_BUSY) {
      if (c.phase == P_BODY || c.phase == P_CHUNK_DATA) {
         int k = c.left >= 0 && c.left < n ? c.left : n;
         body(d, k);
         if (c.state != A64_BUSY)
            return;
         d += k;
         n -= k;
         if (c.left >= 0 && !(c.left -= k)) {
            if (c.phase == P_BODY)
               done();
            else
               c.phase = P_CHUNK_END;
         }
         continue;
      }
      char ch = *d++;
      n--;
      if (ch == '\n') {
         x->line[c.line_n] = 0;
         c.line_n = 0;
         line(x->line);
      } else if (ch != '\r' && c.line_n < HLINE_MAX - 1)
         x->line[c.line_n++] = ch;
   }
}

int a64_poll(void)
{
   if (c.state != A64_BUSY)
      return c.state;
   // Two steps at most a round. Turns during a write to the card only add behind what is
   // read: the ring holds a window at most.
   for (int k = 0; k < 2 && c.len && c.state == A64_BUSY; k++) {
      int at = c.head, n = c.len < RING - at ? c.len : RING - at;
      if (n > STEP)
         n = STEP;
      take(x->ring + at, n);
      c.head = (at + n) % RING;
      c.len -= n;
      c.since = net_millis();
      uint32_t dev = net_lock();
      if (c.pcb)
         tcp_recved(c.pcb, n);
      net_unlock(dev);
   }
   if (c.state != A64_BUSY)
      return c.state;
   if (c.len)
      return c.state;                   // more of it next round
   if (c.gone == 2)
      end(A64_FAILED, "The server's name could not be resolved.");
   else if (c.gone)
      end(A64_FAILED, c.up ? "The connection was lost." : "Could not connect to Assembly 64.");
   else if (c.fin) {
      if (c.phase == P_BODY && c.left < 0)
         done();                        // a body that ends with the connection
      else
         end(A64_FAILED, "The connection was lost.");
   } else if (net_millis() - c.since > QUIET_MS)
      end(A64_FAILED, "No answer from Assembly 64.");
   return c.state;
}

void a64_cancel(void)
{
   if (c.state == A64_BUSY)
      end(A64_IDLE, NULL);
   c.state = A64_IDLE;
}

int a64_got(void)
{
   return c.got;
}

int a64_total(void)
{
   return c.total;
}

const char *a64_error(void)
{
   return c.state == A64_FAILED && c.why ? c.why : "";
}

// --- the requests -------------------------------------------------------------------------

int a64_url_encode(const char *s, char *out, int size)
{
   int n = 0;
   for (; *s; s++) {
      uint8_t ch = *s;
      int plain = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9') ||
                  ch == '_' || ch == '-' || ch == '.' || ch == '*';
      if (n + (plain ? 1 : 3) >= size)
         return 0;
      if (plain)
         out[n++] = ch;
      else
         n += fmt_snprintf(out + n, size - n, "%%%02x", ch);
   }
   out[n] = 0;
   return 1;
}

// After the path: the headers the server expects
static const char tail[] = " HTTP/1.1\r\nAccept-encoding: identity\r\nHost: " HOSTNAME "\r\n"
                           "User-Agent: Assembly Query\r\nClient-Id: Ultimate\r\n"
                           "Connection: close\r\n\r\n";
#define PATH_MAX_ (REQ_MAX - 4 - (int)sizeof tail)   // a path's room, after "GET "

static int over;                        // the path didn't fit

// A GET of the path in x->req, into buf or the file
static void start(char *buf, int size, const char *file)
{
   unsigned gen = c.gen + 1;
   int n = strlen(x->req);

   memset(&c, 0, sizeof c);
   c.gen   = gen;
   c.buf   = buf;
   c.size  = size;
   c.total = -1;
   c.wnd   = window();
   c.state = A64_BUSY;
   c.since = net_millis();
   log_printf("a64 %s", x->req);
   if (over) {
      end(A64_FAILED, "The search is too long.");
      return;
   }
   strcpy(x->req + n, tail);
   if (file) {
      fmt_snprintf(x->file, sizeof x->file, "%s", file);
      fmt_snprintf(x->part, sizeof x->part, "%s.part", file);
      if (f_open(&x->f, x->part, FA_WRITE | FA_CREATE_ALWAYS) != FR_OK) {
         end(A64_FAILED, "The SD card could not be written.");
         return;
      }
      c.fopen = 1;
   }

   uint32_t dev = net_lock();
   err_t e = ERR_OK;
   int none = ip_addr_isany_val(*netif_ip_addr4(&eth_netif));
   if (!none) {
      e = dns_gethostbyname(HOSTNAME, &c.addr, on_dns, (void *)(uintptr_t)c.gen);
      if (e == ERR_OK)
         connect_to(&c.addr);
   }
   net_unlock(dev);
   if (none)
      end(A64_FAILED, "No network: check the cable and the\nmenu's Network settings.");
   else if (e != ERR_OK && e != ERR_INPROGRESS)
      end(A64_FAILED, "The server's name could not be resolved.");
}

static char *request(void)
{
   if (!x)
      x = heap_alloc(sizeof *x);
   a64_cancel();
   over = 0;
   strcpy(x->req, "GET ");
   return x->req + 4;
}

void a64_presets(char *buf, int size)
{
   strcpy(request(), URL_PRESETS);
   start(buf, size, NULL);
}

// The server's paged form: "aql?query=" alone gives the first 20 of the same list and no more
void a64_search(const char *query, int offset, char *buf, int size)
{
   char *p = request();
   int n = fmt_snprintf(p, 64, URL_SEARCH "/%d/%d?query=", offset, A64_PAGE);
   over = !a64_url_encode(query, p + n, PATH_MAX_ - n);
   start(buf, size, NULL);
}

void a64_entries(const char *id, int cat, char *buf, int size)
{
   char *p = request(), enc[64];
   if (!a64_url_encode(id, enc, sizeof enc))
      enc[0] = 0;
   fmt_snprintf(p, 128, URL_ENTRIES "/%s/%d", enc, cat);
   start(buf, size, NULL);
}

void a64_binary(const char *id, int cat, int idx, const char *file)
{
   char *p = request(), enc[64];
   if (!a64_url_encode(id, enc, sizeof enc))
      enc[0] = 0;
   fmt_snprintf(p, 128, URL_DOWNLOAD "/%s/%d/%d", enc, cat, idx);
   start(NULL, 0, file);
}
