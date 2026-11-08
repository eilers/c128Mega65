// The SwiftLink modem (CORE/firmware/net/modem.c) with the real lwIP and lwipopts.h, against
// a C128 and a server on this Mac. The C128 sends at 38400 baud into the 6551's 64-byte FIFO
// and waits while it is full, as TDRE holds it up; it reads a byte per character time from
// the other 64-byte FIFO. The network is a loopback netif: frames for the modem wait in a
// 4 KB buffer as in the MAC, which drops what doesn't fit, and go into lwIP from the pbuf
// pool as eth.c does. The firmware can be made to stall, as a long UCI call or menu redraw
// stalls the main loop.
// usage: test_modem <case>; the cases are listed in main
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "acia.h"
#include "eth.h"
#include "hog.h"
#include "modem.h"
#include "net.h"
#include "lwip/init.h"
#include "lwip/ip4.h"
#include "lwip/tcp.h"
#include "lwip/timeouts.h"

#define TICK_US   20
#define CHAR_US   260                    // 38400 baud, 10 bits
#define FIFO_SIZE 64
#define MAC_BYTES 4096                   // the MAC's receive buffer (net.vhd G_RX_ADDR_BITS)

static uint64_t now_us;
uint32_t net_millis(void) { return (uint32_t)(now_us / 1000); }
uint32_t sys_now(void) { return net_millis(); }
uint32_t net_rand(void) { static uint32_t s = 12345; s ^= s << 13; s ^= s >> 17; s ^= s << 5; return s; }
uint32_t net_gap_max(void) { return 0; }
uint32_t hog(int mode, uint32_t seconds) { (void)mode; (void)seconds; return 0; }
void *heap_alloc(uint32_t bytes) { return calloc(1, bytes); }
void uart_puts(const char *s) { fputs(s, stderr); }   // lib/fmt.c's uart_printf

// --- the C128 and its 6551 ---
static uint8_t tx[70000];                // what the C128 will send
static size_t tx_len, tx_pos;
static uint64_t tx_next;
static uint8_t txfifo[FIFO_SIZE], rxfifo[FIFO_SIZE - 1];
static int txfifo_n, rxfifo_n;
static uint8_t got[200000];              // what the C128 has read
static size_t got_len;
static uint64_t rx_next, paused_until, last_byte, max_gap;
static uint64_t pause_us;                // a disk write after every 254 bytes read
static int since_pause;
static bool reading;                     // waiting for a reply: gaps count
static long put_lost;

static void c128_tick(void)
{
   if (tx_pos < tx_len && now_us >= tx_next && txfifo_n < FIFO_SIZE) {
      txfifo[txfifo_n++] = tx[tx_pos++];
      tx_next = now_us + CHAR_US;
   }
   if (now_us < paused_until || !rxfifo_n || now_us < rx_next)
      return;
   got[got_len++] = rxfifo[0];
   memmove(rxfifo, rxfifo + 1, (size_t)--rxfifo_n);
   rx_next = now_us + CHAR_US;
   if (reading && now_us - last_byte > max_gap)
      max_gap = now_us - last_byte;
   last_byte = now_us;
   if (reading && pause_us && ++since_pause == 254) {
      since_pause = 0;
      paused_until = now_us + pause_us;
      last_byte = paused_until;          // the wait counts from when it reads again
   }
}

int acia_get(void)
{
   if (!txfifo_n)
      return -1;
   int c = txfifo[0];
   memmove(txfifo, txfifo + 1, (size_t)--txfifo_n);
   return c;
}
int acia_rx_free(void) { return (int)sizeof rxfifo - rxfifo_n; }
void acia_put(uint8_t c) { if (rxfifo_n < (int)sizeof rxfifo) rxfifo[rxfifo_n++] = c; else put_lost++; }
bool acia_dtr(void) { return true; }
bool acia_enabled(void) { return true; }
uint32_t acia_baud(void) { return 38400; }
void acia_set_lines(bool dcd, bool dsr, bool cts) { (void)dcd; (void)dsr; (void)cts; }
uint32_t acia_events(void) { return 0; }
uint32_t acia_agains(void) { return 0; }

static void send(const char *s) { memcpy(tx + tx_len, s, strlen(s)); tx_len += strlen(s); }

static bool has(const char *s)
{
   size_t n = strlen(s);
   for (size_t i = 0; i + n <= got_len; i++)
      if (!memcmp(got + i, s, n))
         return true;
   return false;
}

// --- the network ---
struct netif eth_netif;
static struct frame { uint8_t *d; uint16_t n; uint64_t at; } wire[1024], mac[64];
static int wire_n, mac_n, mac_bytes;
static uint64_t delay_us = 1000;
static long mac_drops, pool_drops;
uint32_t eth_drops(void) { return (uint32_t)mac_drops; }
uint32_t eth_tx_stuck(void) { return 0; }

static bool to_server(const uint8_t *d)
{
   uint16_t dport = (uint16_t)(d[22] << 8 | d[23]);
   return dport == 80 || dport == 23;
}

static err_t lo_output(struct netif *nif, struct pbuf *p, const ip4_addr_t *a)
{
   (void)nif; (void)a;
   if (wire_n == 1024) {
      fprintf(stderr, "wire full\n");
      exit(2);
   }
   wire[wire_n].d = malloc(p->tot_len);
   wire[wire_n].n = pbuf_copy_partial(p, wire[wire_n].d, p->tot_len, 0);
   wire[wire_n].at = now_us + delay_us;
   wire_n++;
   return ERR_OK;
}

static err_t lo_init(struct netif *nif)
{
   nif->output = lo_output;
   nif->mtu = 1500;
   nif->flags = NETIF_FLAG_UP | NETIF_FLAG_LINK_UP;
   return ERR_OK;
}

static void input(struct frame *f, pbuf_type type)
{
   struct pbuf *p = pbuf_alloc(PBUF_RAW, f->n, type);
   if (!p)
      pool_drops++;
   else
      pbuf_take(p, f->d, f->n);
   free(f->d);
   if (p && eth_netif.input(p, &eth_netif) != ERR_OK)
      pbuf_free(p);
}

// Frames arrive: the server's go straight in, the modem's into the MAC's buffer
static void wire_tick(void)
{
   while (wire_n && now_us >= wire[0].at) {
      struct frame f = wire[0];
      memmove(wire, wire + 1, sizeof wire[0] * (size_t)--wire_n);
      if (to_server(f.d)) {
         input(&f, PBUF_RAM);
      } else if (mac_n == 64 || mac_bytes + f.n > MAC_BYTES) {
         mac_drops++;
         free(f.d);
      } else {
         mac[mac_n++] = f;
         mac_bytes += f.n;
      }
   }
   sys_check_timeouts();
}

// The firmware's turn: up to four frames from the MAC (eth_poll), then the modem
static void firmware_turn(void)
{
   for (int i = 0; i < 4 && mac_n; i++) {
      struct frame f = mac[0];
      memmove(mac, mac + 1, sizeof mac[0] * (size_t)--mac_n);
      mac_bytes -= f.n;
      input(&f, PBUF_POOL);
   }
   modem_poll();
}

// --- the server ---
static uint8_t srv_in[200000];
static size_t srv_len;
static const uint8_t *srv_out;           // its reply, once the request has come
static size_t srv_out_len, srv_sent;
static const char *srv_until;            // the request ends with this
static bool answering;

static void srv_push(struct tcp_pcb *pcb)
{
   while (srv_sent < srv_out_len && tcp_sndbuf(pcb) > 0) {
      size_t n = srv_out_len - srv_sent;
      if (n > tcp_sndbuf(pcb))
         n = tcp_sndbuf(pcb);
      if (tcp_write(pcb, srv_out + srv_sent, (u16_t)n, TCP_WRITE_FLAG_COPY) != ERR_OK)
         break;
      srv_sent += n;
   }
   tcp_output(pcb);
   // Closed once all is acked: lwIP's split for a small window keeps FIN on the first piece
   if (srv_sent == srv_out_len && !pcb->unsent && !pcb->unacked) {
      tcp_sent(pcb, NULL);
      tcp_close(pcb);
   }
}

static err_t srv_sent_cb(void *arg, struct tcp_pcb *pcb, u16_t len)
{
   (void)arg; (void)len;
   srv_push(pcb);
   return ERR_OK;
}

static err_t srv_recv(void *arg, struct tcp_pcb *pcb, struct pbuf *p, err_t err)
{
   (void)arg; (void)err;
   if (!p)
      return ERR_OK;
   srv_len += pbuf_copy_partial(p, srv_in + srv_len, (u16_t)p->tot_len, 0);
   tcp_recved(pcb, p->tot_len);
   pbuf_free(p);
   size_t u = srv_until ? strlen(srv_until) : 0;
   if (srv_out && !answering && srv_len >= u && !memcmp(srv_in + srv_len - u, srv_until, u)) {
      answering = true;
      tcp_sent(pcb, srv_sent_cb);
      srv_push(pcb);
   }
   return ERR_OK;
}

static struct tcp_pcb *srv_pcb;           // the server's end of the call

static err_t srv_accept(void *arg, struct tcp_pcb *pcb, err_t err)
{
   (void)arg; (void)err;
   tcp_recv(pcb, srv_recv);
   srv_pcb = pcb;
   return ERR_OK;
}

static void listen_on(uint16_t port)
{
   struct tcp_pcb *l = tcp_new();
   tcp_bind(l, IP_ANY_TYPE, port);
   l = tcp_listen(l);
   tcp_accept(l, srv_accept);
}

// --- running it ---
static uint64_t stall_every, stall_us;   // the firmware does nothing for stall_us of every stall_every

static void run(uint64_t us)
{
   for (uint64_t until = now_us + us; now_us < until; now_us += TICK_US) {
      c128_tick();
      wire_tick();
      if (!stall_every || now_us % stall_every >= stall_us)
         firmware_turn();
   }
}

static bool dial(const char *cmd)
{
   send(cmd);
   run(500000);
   return has("CONNECT");
}

static int fails;

static void check(bool ok, const char *what)
{
   printf("  %s: %s\n", ok ? "ok  " : "FAIL", what);
   if (!ok)
      fails++;
}

// The C128 types AT commands
static void case_at(void)
{
   send("AT\r");
   run(100000);
   check(has("AT\r\r\nOK\r\n"), "AT echoes and says OK");
   send("ATI\r");
   run(100000);
   check(has("MEGA65 C128 MODEM"), "ATI names the modem");
   send("ATI1\r");
   run(100000);
   check(has("IP  10.0.0.1") && has("MISSED NMIS"), "ATI1 shows the address and counters");
   send("ATY\r");
   run(100000);
   check(has("ERROR"), "an unknown command is an ERROR");
   send("ATDT10.0.0.1:81\r");                // nobody listens there
   run(3000000);
   check(has("NO CARRIER"), "a refused call is NO CARRIER");
}

// The C128 sends; the server gets it all, in order
static void case_upload(bool telnet, size_t len)
{
   static uint8_t payload[60000], expect[130000];
   size_t elen = 0;
   for (size_t i = 0; i < len; i++) {
      payload[i] = (uint8_t)(net_rand() >> 7);
      if (payload[i] == '+')
         payload[i] = '-';               // no escapes
      expect[elen++] = payload[i];
      if (telnet && payload[i] == 0xFF)
         expect[elen++] = 0xFF;          // telnet doubles IAC
   }
   check(dial(telnet ? "ATDT10.0.0.1:23\r" : "atdt10.0.0.1:80\r"), "CONNECT");
   memcpy(tx + tx_len, payload, len);
   tx_len += len;
   run((uint64_t)len * CHAR_US * 2 + 3000000);
   check(srv_len == elen && !memcmp(srv_in, expect, elen), "the server got every byte, in order");
   printf("        C128 sent %zu of %zu, server got %zu of %zu\n", tx_pos, tx_len, srv_len, elen);
}

// An HTTP request and its short reply, then the server hangs up
static void case_http(void)
{
   static const char reply[] = "HTTP/1.0 404 Not Found\r\nContent-Length: 10\r\n\r\nnot here\r\n";
   srv_out = (const uint8_t *)reply;
   srv_out_len = sizeof reply - 1;
   srv_until = "\r\n\r\n";
   check(dial("atdt10.0.0.1:80\r"), "CONNECT");
   send("GET /c128.img HTTP/1.0\r\nHost: 192.168.1.28:8000\r\n\r\n");
   run(3000000);
   check(has("HTTP/1.0 404 Not Found\r\n"), "the reply reached the C128");
   check(has("NO CARRIER"), "then NO CARRIER");
}

// pkg fetching a file: the server answers like CloudFront (a 594-byte header, then the
// body) and hangs up; pkg gives up after about 4.2 s without a byte
static void case_download(size_t body_len)
{
   static uint8_t reply[70000];
   size_t head = (size_t)snprintf((char *)reply, sizeof reply, "HTTP/1.1 200 OK\r\nContent-Type: "
                                  "application/octet-stream\r\nContent-Length: %zu\r\nConnection: close\r\n", body_len);
   while (head < 590)
      reply[head++] = 'x';               // CloudFront's other headers
   memcpy(reply + head, "\r\n\r\n", 4);
   head += 4;
   for (size_t i = 0; i < body_len; i++)
      reply[head + i] = (uint8_t)(net_rand() >> 9);
   srv_out = reply;
   srv_out_len = head + body_len;
   srv_until = "\r\n\r\n";
   delay_us = 10000;

   check(dial("atdt10.0.0.1:80\r"), "CONNECT");
   size_t start = got_len;
   send("GET /next/73ebcebc/net.pkg HTTP/1.0\r\nHost: pkg.zerobytesfree.org:80\r\n\r\n");
   reading = true;
   last_byte = now_us;
   uint64_t t0 = now_us;
   while (got_len - start < srv_out_len && now_us - t0 < 300000000ull)
      run(10000);
   run(1000000);
   size_t n = got_len - start;
   check(n >= srv_out_len && !memcmp(got + start, reply, srv_out_len), "the C128 got every byte, in order");
   check(max_gap < 4200000, "never 4.2 s without a byte");
   check(has("NO CARRIER"), "then NO CARRIER");
   printf("        %zu of %zu in %.1f s, longest wait %.2f s\n", n < srv_out_len ? n : srv_out_len,
          srv_out_len, (now_us - t0) / 1e6, max_gap / 1e6);
}

// A BBS trickling small segments while the C128 reads nothing (a disk write): each is a pool
// pbuf, and the modem must not hold them, or every other frame finds the pool empty
static void case_trickle(void)
{
   static uint8_t msg[400];
   for (size_t i = 0; i < sizeof msg; i++)
      msg[i] = (uint8_t)(' ' + i % 64);
   check(dial("atdt10.0.0.1:80\r"), "CONNECT");
   size_t start = got_len;
   paused_until = now_us + 2000000;
   tcp_nagle_disable(srv_pcb);
   for (size_t i = 0; i < sizeof msg; ) {
      if (tcp_write(srv_pcb, msg + i, 10, TCP_WRITE_FLAG_COPY) == ERR_OK) {
         tcp_output(srv_pcb);
         i += 10;
      }
      run(5000);
   }
   run(1000000);
   check(!pool_drops, "the pool never ran dry while the C128 didn't read");
   run(3000000);
   check(got_len - start == sizeof msg && !memcmp(got + start, msg, sizeof msg),
         "the C128 got every byte, in order");
}

int main(int argc, char **argv)
{
   const char *c = argc > 1 ? argv[1] : "";
   lwip_init();
   ip4_addr_t ip, mask, gw;
   IP4_ADDR(&ip, 10, 0, 0, 1);
   IP4_ADDR(&mask, 255, 255, 255, 0);
   IP4_ADDR(&gw, 10, 0, 0, 254);
   netif_add(&eth_netif, &ip, &mask, &gw, NULL, lo_init, ip4_input);
   netif_set_default(&eth_netif);
   netif_set_up(&eth_netif);
   modem_init();
   listen_on(80);
   listen_on(23);

   // A long UCI call or a slow menu: 66 ms without a turn, every 250 ms (the longest seen on
   // the R6 without turns between sectors)
   if (strstr(c, "-stall")) {
      stall_every = 250000;
      stall_us = 66000;
   }
   if (!strcmp(c, "at"))
      case_at();
   else if (!strcmp(c, "http"))
      case_http();
   else if (!strcmp(c, "bulk") || !strcmp(c, "bulk-stall"))
      case_upload(false, 60000);
   else if (!strcmp(c, "telnet"))
      case_upload(true, 5000);
   else if (!strcmp(c, "download") || !strcmp(c, "download-stall"))
      case_download(26910);
   else if (!strcmp(c, "trickle"))
      case_trickle();
   else if (!strcmp(c, "download-pause")) {
      pause_us = 100000;                 // a disk write every 254 bytes
      case_download(26910);
   } else {
      fprintf(stderr, "usage: test_modem at|http|bulk|telnet|download|download-pause|"
                      "bulk-stall|download-stall|trickle\n");
      return 2;
   }
   check(!put_lost, "never a byte into a full FIFO");
   printf("test_modem %s: frames dropped by the MAC %ld, for want of a pool pbuf %ld; %s\n", c,
          mac_drops, pool_drops, fails ? "FAILED" : "ALL PASSED");
   return fails ? 1 : 0;
}
