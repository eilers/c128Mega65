// The network for the host tests of UCI's network target (test_prg): the firmware's lwIP on
// a loopback netif at 10.0.0.1, and at the same address what test/uci/ucinet_peer.py is on
// real hardware (TCP and UDP port 6464, TCP 6465 closed), a DNS server that knows every name
// but *.INVALID (all of them 10.0.0.1), and a web server on port 80. The peer's TCP side is
// lwIP too; its UDP side and the DNS server answer at the wire, so they take none of the
// firmware's UDP PCBs. Packets for the firmware's sockets come from the pbuf pool, as eth.c's
// frames do (a pool that has run dry loses them); a packet over the MTU is lost, as its
// fragments would be (IP_REASSEMBLY 0). A packet takes 200 us. After "E n" the peer keeps
// its window shut for 1.5 s, past what the window and lwIP's send buffer hold (5840
// bytes), so the firmware's WRITEs have to wait for room.
#include "lwip/dns.h"               // first: macOS's headers define BYTE_ORDER and htons too
#include "lwip/inet_chksum.h"
#include "lwip/init.h"
#include "lwip/ip4.h"
#include "lwip/tcp.h"
#include "lwip/timeouts.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "host_net.h"
#include "m2m_rv.h"
#include "eth.h"
#include "net.h"

#define WIRE_US 200
#define PORT    6464
#define PEERS   8

extern uint64_t host_time;                // host_m2m.c's CLINT timer, in CPU clocks

struct netif eth_netif;
char eth_unique_id[7] = "C12800";
long hn_pool_drops;
static long packets, big_drops;

uint32_t net_millis(void) { return (uint32_t)(host_time / (CPU_HZ / 1000)); }
uint32_t sys_now(void) { return net_millis(); }
uint32_t net_lock(void) { return 0; }
void net_unlock(uint32_t dev) { (void)dev; }
static int net_up;
int  net_started(void) { return net_up; }

uint32_t net_rand(void)
{
   static uint32_t s = 12345;
   s ^= s << 13;
   s ^= s >> 17;
   s ^= s << 5;
   return s;
}

static uint64_t now_us(void) { return host_time / (CPU_HZ / 1000000); }
static int be16(const uint8_t *p) { return p[0] << 8 | p[1]; }

static void pattern(uint8_t *d, int n, int seed)
{
   for (int i = 0; i < n; i++)
      d[i] = (uint8_t)((i + seed) % 251);
}

// --- the wire: IP packets, in the order sent ---
static struct pkt { uint8_t *d; int n; uint64_t at; } wire[4096];
static int wire_n;

static void put(uint8_t *d, int n)
{
   if (n > 1500) {
      big_drops++;
      free(d);
      return;
   }
   if (wire_n == 4096) {
      fprintf(stderr, "host_net: wire full\n");
      exit(2);
   }
   wire[wire_n++] = (struct pkt){ d, n, now_us() + WIRE_US };
}

static err_t lo_output(struct netif *nif, struct pbuf *p, const ip4_addr_t *a)
{
   uint8_t *d = malloc(p->tot_len);
   put(d, pbuf_copy_partial(p, d, p->tot_len, 0));
   return ERR_OK;
}

static err_t lo_init(struct netif *nif)
{
   static const uint8_t mac[6] = { 0x02, 0xC1, 0x28, 0x00, 0x00, 0x01 };
   memcpy(nif->hwaddr, mac, 6);
   nif->hwaddr_len = 6;
   nif->output = lo_output;
   nif->mtu = 1500;
   nif->flags = NETIF_FLAG_UP | NETIF_FLAG_LINK_UP;
   return ERR_OK;
}

// A UDP packet back to where req came from, from the port it went to; no UDP checksum
static void udp_reply(const uint8_t *req, const void *data, int n)
{
   int ihl = (req[0] & 15) * 4;
   uint8_t *d = calloc(1, 28 + n);
   d[0] = 0x45;
   d[2] = (28 + n) >> 8;
   d[3] = 28 + n;
   d[8] = 64;
   d[9] = IP_PROTO_UDP;
   memcpy(d + 12, req + 16, 4);
   memcpy(d + 16, req + 12, 4);
   u16_t ck = inet_chksum(d, 20);
   memcpy(d + 10, &ck, 2);
   memcpy(d + 20, req + ihl + 2, 2);
   memcpy(d + 22, req + ihl, 2);
   d[24] = (8 + n) >> 8;
   d[25] = 8 + n;
   memcpy(d + 28, data, n);
   put(d, 28 + n);
}

// The peer's UDP port: "P n [seed]" gets n bytes of the pattern, "D n k" k datagrams of n
// bytes (seeds 0 to k-1); anything else comes back as it is
static void peer_udp(const uint8_t *req, const uint8_t *data, int n)
{
   static uint8_t buf[2000];
   char text[32];
   int a = 0, b = 0, t = n < 31 ? n : 31;
   memcpy(text, data, t);
   text[t] = 0;
   if (n < 31 && sscanf(text, "P %d %d", &a, &b) >= 1 && a >= 0 && a <= 1600) {
      pattern(buf, a, b);
      udp_reply(req, buf, a);
   } else if (n < 31 && sscanf(text, "D %d %d", &a, &b) == 2 && a >= 0 && a <= 1600 && b <= 8) {
      for (int k = 0; k < b; k++) {
         pattern(buf, a, k);
         udp_reply(req, buf, a);
      }
   } else
      udp_reply(req, data, n);
}

// The DNS server: an A record of 10.0.0.1 for every name but *.invalid, which doesn't exist
static void dns_reply(const uint8_t *req, const uint8_t *q, int n)
{
   static const uint8_t answer[] = { 0xC0, 0x0C, 0, 1, 0, 1, 0, 0, 0, 60, 0, 4, 10, 0, 0, 1 };
   uint8_t r[600];
   char name[300];
   int at = 12, k = 0;
   while (at < n && q[at] && k + q[at] < 290) {
      int l = q[at++];
      if (k)
         name[k++] = '.';
      memcpy(name + k, q + at, l);
      k  += l;
      at += l;
   }
   name[k] = 0;
   at += 5;                               // the name's end, type and class
   int nx = k >= 8 && !strcasecmp(name + k - 8, ".invalid");
   memcpy(r, q, at);
   r[2] = 0x81;
   r[3] = nx ? 0x83 : 0x80;
   r[6] = 0;
   r[7] = !nx;
   r[8] = r[9] = r[10] = r[11] = 0;
   if (!nx) {
      memcpy(r + at, answer, sizeof answer);
      at += sizeof answer;
   }
   udp_reply(req, r, at);
}

// --- the peer's TCP side: lines with commands on port 6464, a web server on 80 ---
typedef struct {
   struct tcp_pcb *pcb;
   int  web;
   char line[64];
   int  ln;
   int  expect, got, bad;                // "E n": bytes still to come, come, first wrong (-1)
   uint32_t hold_until, held;            // its window stays shut until then, held bytes in all
   int  send_left, sent;                 // "P n": pattern bytes still to go
   int  closing;                         // "C": close once all is sent
   uint32_t tail;                        // the web server's last 4 bytes
} peer_t;
static peer_t peers[PEERS];

static void peer_close(peer_t *c)
{
   tcp_arg(c->pcb, NULL);
   tcp_recv(c->pcb, NULL);
   tcp_sent(c->pcb, NULL);
   tcp_err(c->pcb, NULL);
   if (tcp_close(c->pcb) != ERR_OK)
      tcp_abort(c->pcb);
   c->pcb = NULL;
}

static void peer_push(peer_t *c)
{
   uint8_t buf[1460];
   while (c->send_left > 0) {
      int n = tcp_sndbuf(c->pcb);
      if (n > c->send_left)
         n = c->send_left;
      if (n > (int)sizeof buf)
         n = sizeof buf;
      if (n <= 0)
         break;
      pattern(buf, n, c->sent % 251);
      if (tcp_write(c->pcb, buf, n, TCP_WRITE_FLAG_COPY) != ERR_OK)
         break;
      c->sent      += n;
      c->send_left -= n;
   }
   tcp_output(c->pcb);
   if (c->closing && !c->send_left)
      peer_close(c);
}

static void peer_say(peer_t *c, const char *s)
{
   tcp_write(c->pcb, s, strlen(s), TCP_WRITE_FLAG_COPY);
   tcp_output(c->pcb);
}

static void peer_line(peer_t *c, const char *s)
{
   int n;
   if (sscanf(s, "P %d", &n) == 1) {
      c->send_left = n;
      c->sent = 0;
      peer_push(c);
   } else if (sscanf(s, "E %d", &n) == 1 && n > 0) {
      c->expect = n;
      c->got = 0;
      c->bad = -1;
      c->hold_until = net_millis() + 1500;
   } else if (!strcmp(s, "C")) {
      c->closing = 1;
      peer_push(c);
   } else if (*s)
      peer_say(c, "?\r");
}

static void peer_byte(peer_t *c, uint8_t b)
{
   char t[32];
   if (c->web) {
      c->tail = c->tail << 8 | b;
      if (c->tail == 0x0D0A0D0A) {
         peer_say(c, "HTTP/1.0 200 OK\r\nServer: host_net\r\n\r\n");
         c->closing = 1;
         peer_push(c);
      }
   } else if (c->expect) {
      if (b != c->got % 251 && c->bad < 0)
         c->bad = c->got;
      c->got++;
      if (!--c->expect) {
         snprintf(t, sizeof t, c->bad < 0 ? "OK %d\r" : "BAD %d\r", c->bad < 0 ? c->got : c->bad);
         peer_say(c, t);
      }
   } else if (b == 13 || b == 10) {
      c->line[c->ln] = 0;
      c->ln = 0;
      peer_line(c, c->line);
   } else if (c->ln < (int)sizeof c->line - 1)
      c->line[c->ln++] = (char)b;
}

static err_t peer_recv(void *arg, struct tcp_pcb *pcb, struct pbuf *p, err_t err)
{
   peer_t *c = arg;
   if (!p) {
      peer_close(c);
      return ERR_OK;
   }
   if (net_millis() < c->hold_until)
      c->held += p->tot_len;
   else
      tcp_recved(pcb, p->tot_len);
   for (int i = 0; i < p->tot_len && c->pcb; i++)
      peer_byte(c, pbuf_get_at(p, i));
   pbuf_free(p);
   return ERR_OK;
}

static err_t peer_sent(void *arg, struct tcp_pcb *pcb, u16_t len)
{
   peer_push(arg);
   return ERR_OK;
}

static void peer_err(void *arg, err_t err)
{
   ((peer_t *)arg)->pcb = NULL;
}

static err_t peer_accept(void *arg, struct tcp_pcb *pcb, err_t err)
{
   for (int i = 0; i < PEERS; i++)
      if (!peers[i].pcb) {
         peer_t *c = &peers[i];
         memset(c, 0, sizeof *c);
         c->pcb = pcb;
         c->web = arg != NULL;
         c->bad = -1;
         tcp_arg(pcb, c);
         tcp_recv(pcb, peer_recv);
         tcp_sent(pcb, peer_sent);
         tcp_err(pcb, peer_err);
         return ERR_OK;
      }
   tcp_abort(pcb);
   return ERR_ABRT;
}

static void listen_on(uint16_t port, void *web)
{
   struct tcp_pcb *l = tcp_new();
   tcp_bind(l, IP_ANY_TYPE, port);
   l = tcp_listen(l);
   tcp_arg(l, web);
   tcp_accept(l, peer_accept);
}

// --- running it ---

// The firmware's packets go into the pool, the peer's into RAM; UDP for the peer and DNS is
// answered here
static void deliver(struct pkt *k)
{
   uint8_t *d = k->d;
   int ihl = (d[0] & 15) * 4, proto = d[9], dport = be16(d + ihl + 2);
   packets++;
   if (proto == IP_PROTO_UDP && (dport == PORT || dport == 53)) {
      int n = be16(d + ihl + 4) - 8;
      if (dport == PORT)
         peer_udp(d, d + ihl + 8, n);
      else
         dns_reply(d, d + ihl + 8, n);
      free(d);
      return;
   }
   int peer = proto == IP_PROTO_TCP && (dport == PORT || dport == PORT + 1 || dport == 80);
   struct pbuf *p = pbuf_alloc(PBUF_RAW, k->n, peer ? PBUF_RAM : PBUF_POOL);
   if (p)
      pbuf_take(p, d, k->n);
   else
      hn_pool_drops++;
   free(d);
   if (p && eth_netif.input(p, &eth_netif) != ERR_OK)
      pbuf_free(p);
}

void host_net_poll(void)
{
   uint64_t t = now_us();
   while (wire_n && wire[0].at <= t) {
      struct pkt k = wire[0];
      memmove(wire, wire + 1, sizeof wire[0] * --wire_n);
      deliver(&k);
   }
   for (int i = 0; i < PEERS; i++)
      if (peers[i].pcb && peers[i].held && net_millis() >= peers[i].hold_until) {
         tcp_recved(peers[i].pcb, peers[i].held);
         peers[i].held = 0;
      }
   sys_check_timeouts();
}

void host_net_init(void)
{
   ip4_addr_t ip, mask, gw;
   ip_addr_t dns;
   lwip_init();
   net_up = 1;
   IP4_ADDR(&ip, 10, 0, 0, 1);
   IP4_ADDR(&mask, 255, 255, 255, 0);
   IP4_ADDR(&gw, 10, 0, 0, 254);
   netif_add(&eth_netif, &ip, &mask, &gw, NULL, lo_init, ip4_input);
   netif_set_default(&eth_netif);
   netif_set_up(&eth_netif);
   IP_ADDR4(&dns, 10, 0, 0, 1);
   dns_setserver(0, &dns);
   listen_on(PORT, NULL);
   listen_on(80, (void *)1);
}

void host_net_report(void)
{
   printf("host_net: %ld packets, %ld over the MTU, %ld lost for want of a pool pbuf\n", packets,
          big_drops, hn_pool_drops);
}
