// The REST API's HTTP server (http.h). The protocol is a port of MicroHttpServer's
// c-version/lib/http_protocol.c and server.c at ad73d32, the server in Gideon's firmware
// (Copyright (c) 2015, StarNight; BSD licence in MicroHttpServer-LICENSE.md), so a client
// sees what an Ultimate does: the same limits, the same 15 s idle reaper, and the same
// answers to odd requests. Sockets become lwIP's raw API:
// - lwIP's callbacks, in a network turn, only put received bytes in the connection's ring
//   (its socket buffer) and note a close or an error;
// - http_poll, from the main loop, reads the rings, runs the requests and sends.
// The connections live in HyperRAM: the data RAM is full.
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "heap.h"
#include "http.h"
#include "net.h"
#include "lwip/tcp.h"
#include "lwip/priv/tcp_priv.h"

#define PORT     80
#define CLIENTS  HTTP_CLIENTS          // MAX_HTTP_CLIENT
#define BACKLOG  (CLIENTS / 2)         // his listen()
#define IDLE_MS  15000                 // HTTP_CONN_IDLE_TIMEOUT
#define RX_SIZE  TCP_WND

// http_protocol.c's states, and server.c's per connection
enum { REQ_HEADER, REQ_HEADER_TOO_BIG, REQ_HEADER_DONE, REQ_BODY };
enum { CHUNK_HEADER, CHUNK_BODY, CHUNK_TRAILER };
enum { S_IDLE, S_READING, S_WRITING, S_WRITE_END, S_CLOSE };

typedef struct {
   struct tcp_pcb *pcb;                // NULL once lwIP has dropped it
   int         work;
   bool        fin;
   uint32_t    last_active;
   size_t      windex;
   uint16_t    head, len;              // what lwIP gave and the server hasn't read
   uint8_t     rx[RX_SIZE];
   http_req_t  req;
   http_resp_t res;
} conn_t;

static conn_t         *conns[CLIENTS];
static unsigned        active;         // a bit per connection in use
static struct tcp_pcb *listener;
static struct tcp_pcb *queue[BACKLOG]; // accepted while all four were busy, oldest first
static int             queued;

// newlib's strsep, which Gideon's firmware has: an empty string gives NULL, where glibc's
// gives "". It decides whether an empty query string is an argument.
static char *sep(char **s, const char *delim)
{
   char *t = *s;
   if (!t || !*t) {
      *s = NULL;
      return NULL;
   }
   char *e = t + strcspn(t, delim);
   if (*e) {
      *e = 0;
      *s = e + 1;
   } else {
      *s = NULL;
   }
   return t;
}

// --- http_protocol.c ----------------------------------------------------------------------

static http_method_t have_method(const char *m)
{
   if (!m)
      return HTTP_GET;
   if (!strncmp(m, "GET", 3))
      return HTTP_GET;
   if (!strncmp(m, "POST", 4))
      return HTTP_POST;
   if (!strncmp(m, "PUT", 3))
      return HTTP_PUT;
   if (!strncmp(m, "DELETE", 6))
      return HTTP_DELETE;
   return HTTP_GET;
}

static void init_req(http_req_t *req, int slot)
{
   req->slot = slot;
   req->state = REQ_HEADER;
   req->content_type = "";
   req->body_cb = NULL;
   req->valid = req->used = 0;
   req->body_type = BODY_NONE;
   req->body_size = 0;
   req->hdr_valid = 0;
   req->method = HTTP_UNKNOWN;
   req->nfields = 0;
   req->uri = "";
   req->version = NULL;
}

static char *get_line(http_req_t *req)
{
   req->buf[req->valid] = 0;
   char *p = (char *)req->buf + req->used;
   char *found = strstr(p, "\r\n");
   if (found) {
      *found = 0;
      req->used += found - p + 2;
      return p;
   }
   // Not there yet: make room for more when less than 256 bytes are free
   if (req->used > 0 && HTTP_BUFFER_SIZE - req->valid < 256) {
      int avail = req->valid - req->used;
      memmove(req->buf, p, avail);
      req->used = 0;
      req->valid = avail;
   }
   return NULL;
}

static void parse_header(http_req_t *req)
{
   char *lines[32];
   int line;
   char *p = req->hdr;

   // Lines end at \r, and the \n after it is skipped
   for (line = 0; line < 32; line++)
      lines[line] = "";
   for (line = 0; line < 32; line++) {
      char *l = sep(&p, "\r");
      lines[line] = l ? l : "";
      if (!p)
         break;
      p++;
      if (!*lines[line])
         break;
   }

   // VERB path HTTP/1.1
   char *cur = lines[0];
   req->method = have_method(sep(&cur, " "));
   req->uri = "";
   req->version = NULL;
   if (cur) {
      char *uri = sep(&cur, " ");
      req->uri = uri ? uri : "";
      req->version = cur;
   }

   // KEY ":" VALUE, leading spaces trimmed
   req->nfields = 0;
   for (line = 1; line < 32; line++) {
      if (!*lines[line] || req->nfields >= HTTP_MAX_FIELDS)
         break;
      p = lines[line];
      char *key = sep(&p, ":");
      if (!p)
         break;
      while (*p == ' ')
         p++;
      req->fields[req->nfields].key = key;
      req->fields[req->nfields].value = p;
      req->nfields++;
   }

   // Only a POST has a body: a positive Content-Length, chunked, or no framing at all (his
   // until-disconnect, which get_body takes as empty)
   req->body_type = BODY_NONE;
   req->body_size = 0;
   req->content_type = NULL;
   if (req->method == HTTP_POST) {
      bool length_seen = false;
      for (unsigned i = 0; i < req->nfields; i++)
         if (!strcasecmp(req->fields[i].key, "content-length")) {
            long len = strtol(req->fields[i].value, NULL, 0);
            length_seen = true;
            if (len > 0) {
               req->body_type = BODY_SIZE;
               req->body_size = len;
            }
            break;
         }
      for (unsigned i = 0; i < req->nfields; i++)
         if (!strcasecmp(req->fields[i].key, "content-type")) {
            req->content_type = req->fields[i].value;
            break;
         }
      for (unsigned i = 0; i < req->nfields; i++)
         if (!strcasecmp(req->fields[i].key, "transfer-encoding")) {
            if (strstr(req->fields[i].value, "chunked")) {
               req->body_type = BODY_CHUNKED;
               req->chunk_state = CHUNK_HEADER;
            }
            break;
         }
      if (req->body_type == BODY_NONE && !length_seen)
         req->body_type = BODY_UNTIL_DISCONNECT;
   }
   req->body_cb = NULL;
   req->body_ctx = NULL;
}

static void body_out(http_req_t *req, const uint8_t *p, int n)
{
   if (req->body_cb && n > 0)
      req->body_cb(req->body_ctx, p, n);
}

static int handle_chunked(http_req_t *req)
{
   for (;;) {
      if (req->chunk_state == CHUNK_HEADER) {
         char *line = get_line(req);
         if (!line)
            return 1;
         long remain = strtol(line, NULL, 16);
         if (remain <= 0)
            return 0;                  // the last chunk, or a bad size
         req->chunk_remain = remain;
         req->chunk_state = CHUNK_BODY;
      }
      if (req->chunk_state == CHUNK_BODY) {
         int n = req->valid - req->used;
         if (n == 0)
            return 1;
         int avail = n > (int)req->chunk_remain ? (int)req->chunk_remain : n;
         body_out(req, req->buf + req->used, avail);
         req->used += avail;
         req->chunk_remain -= avail;
         if (!req->chunk_remain)
            req->chunk_state = CHUNK_TRAILER;
         if (req->used == req->valid) {
            req->valid = req->used = 0;
            return 1;
         }
      }
      if (req->chunk_state == CHUNK_TRAILER) {
         if (!get_line(req))
            return 1;
         req->chunk_state = CHUNK_HEADER;
      }
   }
}

static int handle_size(http_req_t *req)
{
   int n = req->valid - req->used;
   int avail = n > (int)req->body_size ? (int)req->body_size : n;
   body_out(req, req->buf + req->used, avail);
   req->used += avail;
   req->body_size -= avail;
   if (req->used == req->valid)
      req->valid = req->used = 0;
   return req->body_size ? 1 : 0;
}

// 1: more to come, 0: done
static int get_body(http_req_t *req)
{
   if (req->body_type == BODY_CHUNKED)
      return handle_chunked(req);
   if (req->body_type == BODY_SIZE)
      return handle_size(req);
   return 0;                           // until the far end closes: taken as nothing
}

static void get_header(http_req_t *req)
{
   char *p = (char *)req->buf + req->used;
   int n = req->valid - req->used;
   int valid = req->hdr_valid;
   int space = HTTP_MAX_HEADER_SIZE - valid;
   int cancopy = n > space ? space : n;

   if (n == 0) {
      req->state = REQ_HEADER_TOO_BIG;
      return;
   }
   memcpy(req->hdr + valid, p, cancopy);
   req->hdr_valid += cancopy;
   req->hdr[req->hdr_valid] = 0;

   int used;
   char *end = strstr(req->hdr, "\r\n\r\n");
   if (end) {
      int until = end - req->hdr + 4;
      parse_header(req);
      used = until - valid;
      req->state = REQ_HEADER_DONE;
   } else {
      used = cancopy;
   }
   if (used >= req->valid)
      req->valid = req->used = 0;
   else
      req->used = used;
}

static int process(http_req_t *req, http_resp_t *res)
{
   if (req->state == REQ_HEADER)
      get_header(req);
   if (req->state == REQ_HEADER_TOO_BIG)
      return S_CLOSE;
   if (req->state == REQ_HEADER_DONE) {
      rest_dispatch(req, res);         // early, so a route can take the body
      if (req->body_type == BODY_NONE)
         return S_WRITING;
      req->state = REQ_BODY;
      if (req->valid == req->used) {
         req->valid = req->used = 0;
         return S_READING;
      }
   }
   if (req->state == REQ_BODY) {
      if (get_body(req))
         return S_READING;
      if (req->body_cb) {
         req->body_cb(req->body_ctx, NULL, 0);
         req->body_cb = NULL;
         req->body_ctx = NULL;
      }
      res->buf[res->index] = 0;
      return S_WRITING;
   }
   return S_READING;
}

// --- lwIP's callbacks, from a network turn ------------------------------------------------

static err_t on_recv(void *arg, struct tcp_pcb *pcb, struct pbuf *p, err_t err)
{
   conn_t *c = arg;
   if (!p) {
      c->fin = true;
      return ERR_OK;
   }
   if (c->work != S_READING) {
      // What follows the request is never read, as by his server; it's taken here so the
      // close is a FIN, not lwIP's reset for unread data
      tcp_recved(pcb, p->tot_len);
      pbuf_free(p);
      return ERR_OK;
   }
   if (c->len + p->tot_len > RX_SIZE)
      return ERR_MEM;                  // can't happen: the window is the ring's size
   for (struct pbuf *q = p; q; q = q->next) {
      const uint8_t *d = q->payload;
      for (int i = 0; i < q->len; i++)
         c->rx[(c->head + c->len++) % RX_SIZE] = d[i];
   }
   pbuf_free(p);
   return ERR_OK;
}

static void on_err(void *arg, err_t err)
{
   ((conn_t *)arg)->pcb = NULL;
}

static void take(int i, struct tcp_pcb *pcb)
{
   conn_t *c = conns[i];
   c->pcb = pcb;
   c->work = S_READING;
   c->fin = false;
   c->head = c->len = 0;
   c->windex = 0;
   c->last_active = net_millis();
   init_req(&c->req, i);
   c->res.body_cb = NULL;
   c->res.index = 0;
   active |= 1u << i;
   tcp_arg(pcb, c);
   tcp_recv(pcb, on_recv);
   tcp_err(pcb, on_err);
}

// A queued connection's data waits in lwIP until it gets a slot. One that closes or fails
// first leaves the queue.
static void unqueue(int q)
{
   queued--;
   memmove(&queue[q], &queue[q + 1], (queued - q) * sizeof queue[0]);
   for (int i = q; i < queued; i++)
      tcp_arg(queue[i], &queue[i]);
}

static err_t on_queued_recv(void *arg, struct tcp_pcb *pcb, struct pbuf *p, err_t err)
{
   if (p)
      return ERR_MEM;
   unqueue((struct tcp_pcb **)arg - queue);
   tcp_arg(pcb, NULL);
   tcp_recv(pcb, NULL);
   tcp_err(pcb, NULL);
   if (tcp_close(pcb) != ERR_OK) {
      tcp_abort(pcb);
      return ERR_ABRT;
   }
   return ERR_OK;
}

static void on_queued_err(void *arg, err_t err)
{
   unqueue((struct tcp_pcb **)arg - queue);
}

static err_t on_accept(void *arg, struct tcp_pcb *pcb, err_t err)
{
   if (err != ERR_OK || !pcb)
      return ERR_VAL;
   for (int i = 0; i < CLIENTS; i++)
      if (!(active & 1u << i)) {
         take(i, pcb);
         return ERR_OK;
      }
   if (queued == BACKLOG) {            // can't happen: lwIP's backlog stops the SYNs
      tcp_abort(pcb);
      return ERR_ABRT;
   }
   queue[queued] = pcb;
   tcp_backlog_delayed(pcb);
   tcp_arg(pcb, &queue[queued]);
   tcp_recv(pcb, on_queued_recv);
   tcp_err(pcb, on_queued_err);
   queued++;
   return ERR_OK;
}

// --- server.c -----------------------------------------------------------------------------

void http_start(void)
{
   for (int i = 0; i < CLIENTS; i++)
      conns[i] = heap_alloc(sizeof(conn_t));
}

// Connections already open keep going; ones waiting in the backlog go with the listener
void http_listen(int on)
{
   if (on && !listener) {
      struct tcp_pcb *pcb = tcp_new_ip_type(IPADDR_TYPE_ANY);
      if (!pcb)
         return;
      if (tcp_bind(pcb, IP_ANY_TYPE, PORT) != ERR_OK) {
         tcp_close(pcb);
         return;
      }
      listener = tcp_listen_with_backlog(pcb, BACKLOG);
      if (listener)
         tcp_accept(listener, on_accept);
      else
         tcp_close(pcb);
   } else if (!on && listener) {
      while (queued) {
         struct tcp_pcb *pcb = queue[0];
         unqueue(0);
         tcp_arg(pcb, NULL);
         tcp_recv(pcb, NULL);
         tcp_err(pcb, NULL);
         tcp_abort(pcb);
      }
      tcp_close(listener);
      listener = NULL;
   }
}

// Bytes from the ring into the receive buffer, as recv(): 0 for the end, or no room
static int read_sock(conn_t *c)
{
   http_req_t *req = &c->req;
   int space = HTTP_BUFFER_SIZE - req->valid;
   int n = space < c->len ? space : c->len;
   for (int i = 0; i < n; i++)
      req->buf[req->valid + i] = c->rx[(c->head + i) % RX_SIZE];
   c->head = (c->head + n) % RX_SIZE;
   c->len -= n;
   req->valid += n;
   if (n) {
      uint32_t dev = net_lock();
      tcp_recved(c->pcb, n);
      net_unlock(dev);
   }
   return n;
}

static void write_sock(conn_t *c)
{
   http_resp_t *res = &c->res;
   if (c->windex >= res->index && res->body_cb) {
      c->windex = 0;
      int n = res->body_cb(res->body_ctx, res->buf, HTTP_BUFFER_SIZE);
      if (n < 0) {
         res->index = 0;               // the route's answer isn't ready: again next round
         return;
      }
      res->index = n;
      if (n == 0)
         res->body_cb = NULL;
   }
   int n = res->index - c->windex;
   if (n <= 0) {
      c->work = S_WRITE_END;
      return;
   }
   if (n > tcp_sndbuf(c->pcb))
      n = tcp_sndbuf(c->pcb);
   uint32_t dev = net_lock();
   err_t e = tcp_write(c->pcb, res->buf + c->windex, n, TCP_WRITE_FLAG_COPY);
   if (e == ERR_OK)
      tcp_output(c->pcb);
   net_unlock(dev);
   if (e == ERR_MEM)
      return;                          // full: the same bytes again later
   if (e != ERR_OK) {
      c->work = S_CLOSE;
      return;
   }
   c->windex += n;
   c->work = res->index > c->windex || res->body_cb ? S_WRITING : S_WRITE_END;
}

static void close_conn(int i)
{
   conn_t *c = conns[i];
   http_req_t *req = &c->req;
   // A body cut off by a close or the reaper: the route lets go of what it holds
   if (req->body_cb) {
      req->body_cb(req->body_ctx, NULL, -1);
      req->body_cb = NULL;
   }
   // And an answer that was still to come lets go of what it holds
   if (c->res.body_cb) {
      c->res.body_cb(c->res.body_ctx, NULL, 0);
      c->res.body_cb = NULL;
   }
   uint32_t dev = net_lock();
   if (c->pcb) {
      tcp_arg(c->pcb, NULL);
      tcp_recv(c->pcb, NULL);
      tcp_err(c->pcb, NULL);
      if (c->len)
         tcp_recved(c->pcb, c->len);
      if (tcp_close(c->pcb) != ERR_OK)
         tcp_abort(c->pcb);
      c->pcb = NULL;
   }
   c->work = S_IDLE;
   active &= ~(1u << i);
   if (queued) {
      struct tcp_pcb *pcb = queue[0];
      unqueue(0);
      tcp_backlog_accepted(pcb);
      take(i, pcb);
      if (pcb->refused_data)           // its request, now, not at lwIP's next 250 ms tick
         tcp_process_refused_data(pcb);
   }
   net_unlock(dev);
}

void http_poll(void)
{
   if (!active)
      return;
   uint32_t now = net_millis();
   for (int i = 0; i < CLIENTS; i++) {
      if (!(active & 1u << i))
         continue;
      conn_t *c = conns[i];
      bool busy = false;
      if (c->work == S_READING && (c->len || c->fin || !c->pcb)) {
         c->last_active = now;
         busy = true;
         if (c->pcb && read_sock(c) > 0)
            c->work = process(&c->req, &c->res);
         else
            c->work = S_CLOSE;
      }
      if (c->work == S_WRITING) {
         if (!c->pcb)
            c->work = S_CLOSE;
         else if (tcp_sndbuf(c->pcb) && tcp_sndqueuelen(c->pcb) < TCP_SND_QUEUELEN) {
            c->last_active = now;
            busy = true;
            write_sock(c);
         }
      }
      if (!busy && now - c->last_active >= IDLE_MS)
         c->work = S_CLOSE;
      if (c->work == S_WRITE_END || c->work == S_CLOSE)
         close_conn(i);
   }
}
