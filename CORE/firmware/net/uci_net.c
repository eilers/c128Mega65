// Ultimate Command Interface: the network target 3, with the commands, replies and status
// texts of Gideon's io/network/network_target.cc, on lwIP's raw API. Four sockets (his 16).
// OPEN resolves and connects, READ waits up to 40 ms (his SO_RCVTIMEO) and WRITE waits for
// room, all from work(), so the main loop goes on meanwhile. The numbers in his texts are
// errno values, which his firmware takes from newlib.
#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include "eth.h"
#include "fmt.h"
#include "net.h"
#include "uci.h"
#include "lwip/dns.h"
#include "lwip/tcp.h"
#include "lwip/udp.h"

#define NET_IDENTIFY     0x01
#define NET_GET_IF_COUNT 0x02
#define NET_GET_NETADDR  0x04
#define NET_GET_IPADDR   0x05
#define NET_SET_IPADDR   0x06
#define NET_OPEN_TCP     0x07
#define NET_OPEN_UDP     0x08
#define NET_CLOSE        0x09
#define NET_READ         0x10
#define NET_WRITE        0x11

#define SOCKETS   4
#define MAX_READ  1472                  // the largest UDP payload in one Ethernet frame
#define BLOCK     UCI_MAX_REPLY         // a reply's block
#define READ_WAIT 40                    // ms
#define RX_SIZE   TCP_WND

#define E_BADF    9                     // newlib's errno values
#define E_AGAIN   11

static const uint8_t identification[] = "C128MEGA65 NETWORK INTERFACE V1.0";
static const char ok[]           = "00,OK";
static const char unknown[]      = "21,UNKNOWN COMMAND";
static const char invalid[]      = "81,INVALID PARAMS";
static const char out_of_range[] = "82,PARAMETER(S) OUT OF RANGE";
static const char unresolved[]   = "84,UNRESOLVED HOST";
static const char no_socket[]    = "85,ERROR OPENING SOCKET";
static const char closed[]       = "01,CONNECTION CLOSED BY HOST";
static const char no_more[]      = "03,MORE DATA NOT SUPPORTED";
static char text[40];                   // a status with a number in it

// A socket, and what it received that the C128 hasn't read, in a ring: TCP's bytes, or UDP's
// datagrams, each after its length (2 bytes). TCP's window keeps its bytes within the ring;
// a datagram that doesn't fit is dropped. No lwIP pbuf is held, so a program that doesn't
// read can't empty the pool the MAC's frames go into.
typedef struct {
   struct tcp_pcb *tcp;
   struct udp_pcb *udp;
   err_t    err;                        // how the connection ended (its pcb is gone)
   uint8_t  used, up, fin;              // up: connected, fin: the far end has closed
   uint16_t head, len;
   uint8_t  rx[RX_SIZE];
} sock_t;

static sock_t socks[SOCKETS];           // the C128's handle is the index + 1

// The command under way: OPEN resolving or connecting, READ waiting for data, WRITE for room
enum { IDLE, RESOLVING, CONNECTING, READING, WRITING };
static struct {
   int       op;
   unsigned  gen;                       // a new one each command: a late DNS answer is dropped
   sock_t   *s;
   int       udp;
   uint16_t  port;
   ip_addr_t addr;
   err_t     dns;                       // ERR_INPROGRESS, then ERR_OK or ERR_VAL
   uint32_t  t0;
   const uint8_t *src;                  // WRITE: what is left of the command's bytes
   int       left, done;
   int       want;                      // READ: the length asked for
} job;

// A READ's reply, its payload still in the ring: bytes in all, handed out so far, where in
// the ring they start, and what to drop from the ring at the end
static struct {
   sock_t     *s;
   int         total, off, skip, drop;
   const char *status;
} rd;

// lwIP's err_to_errno, in newlib's numbers
static int err_no(err_t e)
{
   static const uint8_t no[] = { 0, 12, 105, 11, 118, 119, 22, 11, 112, 120, 127, 128, 0, 113, 104, 128, 5 };
   return e == ERR_IF ? -1 : e <= 0 && -e < (int)sizeof no ? no[-e] : 5;
}

// --- the ring ---------------------------------------------------------------------------

static void ring_write(sock_t *s, const void *d, int n)
{
   int tail  = (s->head + s->len) % RX_SIZE;
   int first = n < RX_SIZE - tail ? n : RX_SIZE - tail;
   memcpy(s->rx + tail, d, first);
   memcpy(s->rx, (const uint8_t *)d + first, n - first);
   s->len += n;
}

static void ring_read(const sock_t *s, int off, uint8_t *d, int n)
{
   int at    = (s->head + off) % RX_SIZE;
   int first = n < RX_SIZE - at ? n : RX_SIZE - at;
   memcpy(d, s->rx + at, first);
   memcpy(d + first, s->rx, n - first);
}

// Read bytes go; TCP's window opens by as many
static void ring_drop(sock_t *s, int n)
{
   s->head = (s->head + n) % RX_SIZE;
   s->len -= n;
   if (s->tcp)
      tcp_recved(s->tcp, n);
}

// --- lwIP's callbacks, from a network turn ------------------------------------------------

static err_t on_recv(void *arg, struct tcp_pcb *pcb, struct pbuf *p, err_t err)
{
   sock_t *s = arg;
   if (!p) {
      s->fin = 1;
      return ERR_OK;
   }
   if (s->len + p->tot_len > RX_SIZE)
      return ERR_MEM;                   // can't happen: lwIP keeps it and tries again
   for (struct pbuf *q = p; q; q = q->next)
      ring_write(s, q->payload, q->len);
   pbuf_free(p);
   return ERR_OK;
}

static void on_err(void *arg, err_t err)
{
   sock_t *s = arg;
   s->tcp = NULL;
   s->err = err;
}

static err_t on_connected(void *arg, struct tcp_pcb *pcb, err_t err)
{
   ((sock_t *)arg)->up = 1;
   return ERR_OK;
}

static void on_udp(void *arg, struct udp_pcb *pcb, struct pbuf *p, const ip_addr_t *a, u16_t port)
{
   sock_t *s = arg;
   if (s->len + 2 + p->tot_len <= RX_SIZE) {
      uint8_t n[2] = { (uint8_t)p->tot_len, (uint8_t)(p->tot_len >> 8) };
      ring_write(s, n, 2);
      for (struct pbuf *q = p; q; q = q->next)
         ring_write(s, q->payload, q->len);
   }
   pbuf_free(p);
}

static void on_dns(const char *name, const ip_addr_t *addr, void *arg)
{
   if ((uintptr_t)arg != job.gen || job.op != RESOLVING)
      return;
   job.dns = addr ? ERR_OK : ERR_VAL;
   if (addr)
      job.addr = *addr;
}

// --- sockets ----------------------------------------------------------------------------

static sock_t *owned(int h)
{
   return h >= 1 && h <= SOCKETS && socks[h - 1].used ? &socks[h - 1] : NULL;
}

static void sock_close(sock_t *s)
{
   struct tcp_pcb *pcb = s->tcp;
   if (pcb) {
      if (s->len)
         tcp_recved(pcb, s->len);       // a FIN, not lwIP's reset for unread data
      tcp_arg(pcb, NULL);
      tcp_recv(pcb, NULL);
      tcp_err(pcb, NULL);
      if (tcp_close(pcb) != ERR_OK)
         tcp_abort(pcb);
   }
   if (s->udp)
      udp_remove(s->udp);
   memset(s, 0, offsetof(sock_t, rx));
}

static void opened(sock_t *s, uci_reply_t *r)
{
   uci_data[0] = s - socks + 1;
   r->data   = uci_data;
   r->len    = 1;
   r->status = ok;
}

static void connect_failed(sock_t *s, err_t e, uci_reply_t *r)
{
   sock_close(s);
   fmt_snprintf(text, sizeof text, "11,ERROR ON CONNECT: %d", err_no(e));
   r->status = text;
}

// OPEN, once the name is an address
static void connect_socket(uci_reply_t *r)
{
   sock_t *s = NULL;
   for (int i = 0; i < SOCKETS && !s; i++)
      if (!socks[i].used)
         s = &socks[i];
   if (!s) {
      r->status = no_socket;
      return;
   }
   err_t e;
   if (job.udp) {
      if (!(s->udp = udp_new_ip_type(IPADDR_TYPE_V4))) {
         r->status = no_socket;
         return;
      }
      s->used = 1;
      udp_recv(s->udp, on_udp, s);
      e = udp_connect(s->udp, &job.addr, job.port);
   } else {
      if (!(s->tcp = tcp_new_ip_type(IPADDR_TYPE_V4))) {
         r->status = no_socket;
         return;
      }
      s->used = 1;
      tcp_arg(s->tcp, s);
      tcp_recv(s->tcp, on_recv);
      tcp_err(s->tcp, on_err);
      e = tcp_connect(s->tcp, &job.addr, job.port, on_connected);
      if (e == ERR_OK) {
         job.op = CONNECTING;
         job.s  = s;
         r->status = NULL;
         return;
      }
   }
   if (e != ERR_OK)
      connect_failed(s, e, r);
   else
      opened(s, r);
}

// OPEN_TCP, OPEN_UDP: port (2 bytes), then the host's name or address
static void open_socket(uint8_t *c, int len, int udp, uci_reply_t *r)
{
   if (len < 5) {
      r->status = invalid;
      return;
   }
   job.udp  = udp;
   job.port = c[2] | c[3] << 8;
   job.dns  = ERR_INPROGRESS;
   err_t e = dns_gethostbyname((const char *)c + 4, &job.addr, on_dns, (void *)(uintptr_t)job.gen);
   if (e == ERR_INPROGRESS) {
      job.op = RESOLVING;
      r->status = NULL;
   } else if (e != ERR_OK)
      r->status = unresolved;
   else
      connect_socket(r);
}

// --- READ: a header of 2 bytes, the bytes it carries (-1 for none), then them ------------

static void no_data(uci_reply_t *r, int e)
{
   uci_data[0] = uci_data[1] = 0xFF;
   r->data = uci_data;
   r->len  = 2;
   fmt_snprintf(text, sizeof text, "02,NO DATA: %d", e);
   r->status = text;
}

static void read_end(void)
{
   ring_drop(rd.s, rd.drop);
   rd.s = NULL;
}

// The next block of the reply: the first starts with the header, and only the last has the
// status, as his blocks
static void read_block(uci_reply_t *r)
{
   int head = rd.off == 0 ? 2 : 0;
   int n    = rd.total - rd.off;
   if (n > BLOCK - head)
      n = BLOCK - head;
   uci_data[0] = rd.total;
   uci_data[1] = rd.total >> 8;
   ring_read(rd.s, rd.skip + rd.off, uci_data + head, n);
   rd.off   += n;
   r->data   = uci_data;
   r->len    = head + n;
   r->more   = rd.off < rd.total;
   r->status = r->more ? "" : rd.status;
   if (!r->more)
      read_end();
}

static void read_now(uci_reply_t *r)
{
   sock_t *s = job.s;
   if (!s)
      no_data(r, E_BADF);
   else if (s->len) {
      int n = s->len;
      rd.skip = 0;
      if (s->udp) {
         uint8_t h[2];
         ring_read(s, 0, h, 2);
         n = h[0] | h[1] << 8;
         rd.skip = 2;
      }
      rd.s      = s;
      rd.off    = 0;
      rd.total  = n < job.want ? n : job.want;
      rd.drop   = s->udp ? 2 + n : rd.total;
      rd.status = ok;
      if (n > job.want && s->udp) {     // the rest of the datagram goes
         fmt_snprintf(text, sizeof text, "04,DATAGRAM TRUNCATED: %d", n);
         rd.status = text;
      }
      read_block(r);
   } else if (s->fin) {
      uci_data[0] = uci_data[1] = 0;
      r->data   = uci_data;
      r->len    = 2;
      r->status = closed;
      sock_close(s);
   } else if (s->err)
      no_data(r, err_no(s->err));
   else if (net_millis() - job.t0 >= READ_WAIT)
      no_data(r, E_AGAIN);
   else
      return;
   job.op = IDLE;
}

// READ: socket, length (2 bytes)
static void read_socket(uint8_t *c, int len, uci_reply_t *r)
{
   if (len != 5) {
      r->status = invalid;
      return;
   }
   job.want = c[3] | c[4] << 8;
   if (job.want > MAX_READ) {
      r->status = out_of_range;
      return;
   }
   job.s  = owned(c[2]);
   job.t0 = net_millis();
   job.op = READING;
   r->status = NULL;
   read_now(r);
}

// --- WRITE: the bytes sent, or -1, as 2 bytes --------------------------------------------

static void sent(uci_reply_t *r, int n, int e)
{
   uci_data[0] = n;
   uci_data[1] = n >> 8;
   r->data   = uci_data;
   r->len    = 2;
   r->status = ok;
   if (n < 0) {
      fmt_snprintf(text, sizeof text, "12,SEND ERROR: %d", e);
      r->status = text;
   }
   job.op = IDLE;
}

// As much as TCP takes now; the rest once acks make room
static void write_more(uci_reply_t *r)
{
   sock_t *s = job.s;
   if (!s->tcp) {
      sent(r, -1, err_no(s->err));
      return;
   }
   while (job.left) {
      int n = tcp_sndbuf(s->tcp);
      if (n > job.left)
         n = job.left;
      if (!n)
         break;
      err_t e = tcp_write(s->tcp, job.src, n, TCP_WRITE_FLAG_COPY);
      if (e == ERR_MEM)
         break;
      if (e != ERR_OK) {
         sent(r, -1, err_no(e));
         return;
      }
      job.src  += n;
      job.left -= n;
      job.done += n;
   }
   tcp_output(s->tcp);
   if (!job.left)
      sent(r, job.done, 0);
}

// WRITE: socket, then the bytes
static void write_socket(uint8_t *c, int len, uci_reply_t *r)
{
   sock_t *s = len >= 3 ? owned(c[2]) : NULL;
   if (!s) {
      sent(r, -1, E_BADF);
      return;
   }
   job.s    = s;
   job.src  = c + 3;
   job.left = len - 3;
   job.done = 0;
   if (s->udp) {
      struct pbuf *p = pbuf_alloc(PBUF_TRANSPORT, job.left, PBUF_RAM);
      err_t e = ERR_MEM;
      if (p) {
         pbuf_take(p, job.src, job.left);
         e = udp_send(s->udp, p);
         pbuf_free(p);
      }
      sent(r, e == ERR_OK ? job.left : -1, err_no(e));
      return;
   }
   job.op = WRITING;
   r->status = NULL;
   write_more(r);
}

// --- the target -------------------------------------------------------------------------

// A command may come only once the last reply is over, or given up
static void finish(void)
{
   if (rd.s)
      read_end();
   if (job.op == CONNECTING)
      sock_close(job.s);
   job.op = IDLE;
   job.gen++;
}

static void command(uint8_t *c, int len, uci_reply_t *r)
{
   uint32_t dev = net_lock();
   finish();
   r->status = ok;
   switch (len > 1 ? c[1] : -1) {
   case NET_IDENTIFY:
      r->data = identification;
      r->len  = sizeof identification - 1;
      break;
   case NET_GET_IF_COUNT:
      uci_data[0] = 1;
      r->data = uci_data;
      r->len  = 1;
      break;
   case NET_GET_NETADDR:
   case NET_GET_IPADDR:
   case NET_SET_IPADDR:
      if (len != (c[1] == NET_SET_IPADDR ? 15 : 3))
         r->status = invalid;
      else if (c[2] >= 1)
         r->status = out_of_range;
      else if (c[1] == NET_GET_NETADDR) {
         r->data = eth_netif.hwaddr;
         r->len  = 6;
      } else if (c[1] == NET_GET_IPADDR) {
         memcpy(uci_data, netif_ip4_addr(&eth_netif), 4);
         memcpy(uci_data + 4, netif_ip4_netmask(&eth_netif), 4);
         memcpy(uci_data + 8, netif_ip4_gw(&eth_netif), 4);
         r->data = uci_data;
         r->len  = 12;
      } else {
         ip4_addr_t a[3];
         memcpy(a, c + 3, sizeof a);
         netif_set_addr(&eth_netif, &a[0], &a[1], &a[2]);
      }
      break;
   case NET_OPEN_TCP:
   case NET_OPEN_UDP:
      open_socket(c, len, c[1] == NET_OPEN_UDP, r);
      break;
   case NET_CLOSE:
      if (len != 3)
         r->status = invalid;
      else if (owned(c[2]))
         sock_close(owned(c[2]));
      else {
         fmt_snprintf(text, sizeof text, "12,ERROR ON CLOSE: %d", E_BADF);
         r->status = text;
      }
      break;
   case NET_READ:
      read_socket(c, len, r);
      break;
   case NET_WRITE:
      write_socket(c, len, r);
      break;
   default:
      r->status = unknown;
   }
   net_unlock(dev);
}

static void work(uci_reply_t *r)
{
   uint32_t dev = net_lock();
   switch (job.op) {
   case RESOLVING:
      if (job.dns != ERR_INPROGRESS) {
         job.op = IDLE;
         if (job.dns == ERR_OK)
            connect_socket(r);
         else
            r->status = unresolved;
      }
      break;
   case CONNECTING:
      if (job.s->up || !job.s->tcp) {
         job.op = IDLE;
         if (job.s->up)
            opened(job.s, r);
         else
            connect_failed(job.s, job.s->err, r);
      }
      break;
   case READING:
      read_now(r);
      break;
   case WRITING:
      write_more(r);
      break;
   }
   net_unlock(dev);
}

static void more(uci_reply_t *r)
{
   if (!rd.s) {
      r->status = no_more;
      return;
   }
   uint32_t dev = net_lock();
   read_block(r);
   net_unlock(dev);
}

// The C128 gave up on the reply: what is left of a READ goes, as a datagram's rest would
static void stop(void)
{
   uint32_t dev = net_lock();
   finish();
   net_unlock(dev);
}

// A reset of the C128 closes the sockets its program opened
static void reset(void)
{
   uint32_t dev = net_lock();
   rd.s = NULL;
   finish();
   for (int i = 0; i < SOCKETS; i++)
      if (socks[i].used)
         sock_close(&socks[i]);
   net_unlock(dev);
}

const uci_target_t uci_net = { command, more, work, stop, reset };
