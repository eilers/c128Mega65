// Hayes modem over TCP. The C128 dials host:port with ATD, or answers calls that
// arrive on MODEM_LISTEN_PORT. The command set follows the Ultimate's modem
// (1541ultimate software/io/acia/modem.cc); telnet negotiation is added, so
// binary transfers survive BBSes that speak the telnet protocol.
// Ported from the 1541 Ultimate firmware (github.com/GideonZ/1541ultimate), Copyright
// Gideon Zweijtzer and contributors, GPL v3 as this core; changed for c128Mega65 in 2026.
#include <string.h>
#include "acia.h"
#include "eth.h"
#include "fmt.h"
#include "heap.h"
#include "hog.h"
#include "log.h"
#include "modem.h"
#include "net.h"
#include "lwip/dns.h"
#include "lwip/ip_addr.h"
#include "lwip/tcp.h"

#define VERSION       "0.2"
#define CMD_MAX      80
#define OUTQ_SIZE     256     // power of 2
#define DIAL_TIMEOUT  20000   // ms
#define RING_PERIOD   2000    // ms
#define RING_MAX      10
#define FLUSH_IDLE    3       // ms without a byte from the C128 before a short segment goes out
#define FLUSH_SIZE    192     // bytes from the C128 that go out at once
#define TXBUF_SIZE    256     // FLUSH_SIZE plus room for telnet replies
#define RX_SIZE       TCP_WND // TCP's window keeps what the C128 hasn't read within it

enum state { ST_IDLE, ST_RESOLVING, ST_CONNECTING, ST_RINGING, ST_ONLINE };

enum result {
   RC_OK = 0, RC_CONNECT = 1, RC_RING = 2, RC_NO_CARRIER = 3, RC_ERROR = 4,
   RC_NO_DIALTONE = 6, RC_BUSY = 7, RC_NO_ANSWER = 8
};

static const char *const result_text[] = {
   "OK", "CONNECT", "RING", "NO CARRIER", "ERROR", "", "NO DIALTONE", "BUSY", "NO ANSWER"
};

// S-registers
#define S_AUTOANSWER  0       // answer after this many rings, 0 = never
#define S_RINGS       1       // rings so far
#define S_ESCAPE      2       // escape character, default '+'
#define S_GUARD       12      // escape guard time, 1/50 s
#define S_TELNET      50      // 0 = detect, 1 = never, 2 = always
#define S_COUNT       64

// Telnet
#define IAC   255
#define DONT  254
#define DO    253
#define WONT  252
#define WILL  251
#define SB    250
#define SE    240
#define OPT_BINARY 0
#define OPT_ECHO   1
#define OPT_SGA    3

static struct {
   enum state state;
   bool cmd_mode;              // taking AT commands
   struct tcp_pcb *pcb;        // the call
   struct tcp_pcb *incoming;   // a caller waiting for an answer
   uint32_t t_state;
   uint32_t t_ring;

   char line[CMD_MAX + 1];
   char last[CMD_MAX + 1];
   int len;
   uint8_t prev;

   bool echo, verbose, quiet;
   uint8_t dtr_mode;           // &D: 0 ignore DTR, otherwise hang up when it drops
   uint8_t dcd_mode;           // &C: 0 DCD always on, 1 DCD follows the carrier
   uint8_t s[S_COUNT];
   bool dtr;

   char host[DNS_MAX_NAME_LENGTH + 1];
   uint16_t port;
   ip_addr_t addr;

   uint8_t *rx;                // received, not yet passed to the C128: a ring in HyperRAM,
   uint16_t rx_head, rx_len;   // so no lwIP pbuf is held while the C128 doesn't read
   uint32_t rx_count;
   bool remote_closed;
   uint8_t txbuf[TXBUF_SIZE];  // for the far end, gathered into one tcp_write: a write per
   uint16_t txlen;             // byte takes a pbuf each, and TCP_SND_QUEUELEN stops it at 8
   uint32_t t_last_tx;
   int esc_count;
   uint32_t t_esc;

   bool telnet;
   uint8_t iac_state, iac_cmd;
   uint32_t we_do, we_will;    // telnet options agreed, bit per option 0..31

   uint8_t outq[OUTQ_SIZE];
   uint32_t out_head, out_tail;
   uint32_t dump_pos;          // ATI2: next log byte to print
   size_t dump_len;
} m;

// --- output to the C128 ---------------------------------------------------

static void out_byte(uint8_t c)
{
   if (m.out_head - m.out_tail < OUTQ_SIZE)
      m.outq[m.out_head++ & (OUTQ_SIZE - 1)] = c;
}

static void out_str(const char *s)
{
   while (*s)
      out_byte((uint8_t)*s++);
}

static void result(enum result r)
{
   if (m.quiet)
      return;
   char buf[24];
   if (m.verbose) {
      if (r == RC_CONNECT)
         fmt_snprintf(buf, sizeof buf, "\r\nCONNECT %u\r\n", acia_baud());
      else
         fmt_snprintf(buf, sizeof buf, "\r\n%s\r\n", result_text[r]);
   } else {
      fmt_snprintf(buf, sizeof buf, "%u\r", (uint32_t)r);
   }
   out_str(buf);
}

// --- settings -------------------------------------------------------------

static void factory(void)
{
   m.echo = true;
   m.verbose = true;
   m.quiet = false;
   m.dtr_mode = 2;
   m.dcd_mode = 1;
   memset(m.s, 0, sizeof m.s);
   m.s[S_ESCAPE] = '+';
   m.s[3] = '\r';
   m.s[4] = '\n';
   m.s[5] = 8;
   m.s[S_GUARD] = 50;
}

// --- connection handling --------------------------------------------------

static void set_state(enum state st)
{
   m.state = st;
   m.t_state = net_millis();
}

static void drop_rx(void)
{
   m.rx_head = m.rx_len = 0;
}

static void forget(struct tcp_pcb *pcb)
{
   tcp_arg(pcb, NULL);
   tcp_recv(pcb, NULL);
   tcp_err(pcb, NULL);
   tcp_sent(pcb, NULL);
}

static void close_pcb(struct tcp_pcb *pcb)
{
   forget(pcb);
   if (tcp_close(pcb) != ERR_OK)
      tcp_abort(pcb);
}

// End the call or the ringing; r is the result for the C128 (RC_OK for none).
static void hang_up(enum result r, bool say)
{
   if (m.pcb) {
      close_pcb(m.pcb);
      m.pcb = NULL;
   }
   if (m.incoming) {
      close_pcb(m.incoming);
      m.incoming = NULL;
   }
   drop_rx();
   m.remote_closed = false;
   m.cmd_mode = true;
   m.s[S_RINGS] = 0;
   set_state(ST_IDLE);
   if (say)
      result(r);
}

static err_t on_recv(void *arg, struct tcp_pcb *pcb, struct pbuf *p, err_t err)
{
   (void)arg;
   (void)err;
   if (pcb != m.pcb && pcb != m.incoming) {
      if (p)
         pbuf_free(p);
      return ERR_OK;
   }
   if (!p) {
      m.remote_closed = true;
      return ERR_OK;
   }
   if (m.rx_len + p->tot_len > RX_SIZE)
      return ERR_MEM;                   // can't happen: lwIP keeps it and tries again
   for (struct pbuf *q = p; q; q = q->next) {
      int tail  = (m.rx_head + m.rx_len) % RX_SIZE;
      int first = q->len < RX_SIZE - tail ? q->len : RX_SIZE - tail;
      memcpy(m.rx + tail, q->payload, first);
      memcpy(m.rx, (const uint8_t *)q->payload + first, q->len - first);
      m.rx_len += q->len;
   }
   pbuf_free(p);
   return ERR_OK;
}

static void on_err(void *arg, err_t err)
{
   (void)arg;
   log_printf("tcp err %d", err);
   // lwIP has already freed the pcb
   bool ringing = m.state == ST_RINGING;
   m.pcb = NULL;
   m.incoming = NULL;
   hang_up(ringing ? RC_OK : RC_NO_CARRIER, !ringing && m.state != ST_IDLE);
}

static void go_online(struct tcp_pcb *pcb)
{
   m.pcb = pcb;
   m.cmd_mode = false;
   m.rx_count = 0;
   m.telnet = m.s[S_TELNET] == 2 || (m.s[S_TELNET] == 0 && m.port == 23);
   m.iac_state = 0;
   m.we_do = m.we_will = 0;
   m.txlen = 0;
   m.esc_count = 0;
   m.t_last_tx = net_millis();
   tcp_nagle_disable(pcb);
   pcb->so_options |= SOF_KEEPALIVE;
   set_state(ST_ONLINE);
   result(RC_CONNECT);
}

static err_t on_connected(void *arg, struct tcp_pcb *pcb, err_t err)
{
   (void)arg;
   if (err != ERR_OK || m.state != ST_CONNECTING)
      return ERR_OK;
   log_printf("connected %s:%u", m.host, m.port);
   go_online(pcb);
   return ERR_OK;
}

static void connect_to(const ip_addr_t *addr)
{
   struct tcp_pcb *pcb = tcp_new();
   if (!pcb) {
      hang_up(RC_ERROR, true);
      return;
   }
   m.pcb = pcb;
   tcp_arg(pcb, NULL);
   tcp_recv(pcb, on_recv);
   tcp_err(pcb, on_err);
   set_state(ST_CONNECTING);
   if (tcp_connect(pcb, addr, m.port, on_connected) != ERR_OK)
      hang_up(RC_NO_CARRIER, true);
}

static void on_dns(const char *name, const ip_addr_t *addr, void *arg)
{
   (void)name;
   (void)arg;
   if (m.state != ST_RESOLVING)
      return;
   if (!addr) {
      log_printf("dns failed %s", m.host);
      hang_up(RC_NO_ANSWER, true);
      return;
   }
   connect_to(addr);
}

// ATD: [T|P] host[:port], with or without quotes (Zimodem style). Only one T or P
// is skipped, as on the Ultimate, so a host such as pkg.example.com keeps its P.
static void dial(const char *p)
{
   while (*p == ' ')
      p++;
   if (*p == 'T' || *p == 'P')
      p++;
   while (*p == ' ' || *p == '"')
      p++;
   int n = 0;
   while (*p && *p != ':' && *p != '"' && *p != ' ' && n < DNS_MAX_NAME_LENGTH)
      m.host[n++] = *p++;
   m.host[n] = '\0';
   m.port = 23;
   if (*p == ':') {
      uint32_t port = 0;
      for (p++; *p >= '0' && *p <= '9'; p++)
         port = port * 10 + (uint32_t)(*p - '0');
      if (port && port < 65536)
         m.port = (uint16_t)port;
   }
   if (!n) {
      result(RC_ERROR);
      return;
   }
   if (ip_addr_isany_val(*netif_ip_addr4(&eth_netif))) {
      result(RC_NO_DIALTONE);   // no IP address yet
      return;
   }
   log_printf("dial %s:%u", m.host, m.port);
   if (ipaddr_aton(m.host, &m.addr)) {
      connect_to(&m.addr);
      return;
   }
   set_state(ST_RESOLVING);
   err_t e = dns_gethostbyname(m.host, &m.addr, on_dns, NULL);
   if (e == ERR_OK)
      connect_to(&m.addr);
   else if (e != ERR_INPROGRESS)
      hang_up(RC_NO_ANSWER, true);
}

static err_t on_accept(void *arg, struct tcp_pcb *pcb, err_t err)
{
   (void)arg;
   if (err != ERR_OK || !pcb)
      return ERR_VAL;
   if (m.state != ST_IDLE || !acia_enabled()) {
      static const char busy[] = "BUSY\r\n";
      tcp_write(pcb, busy, sizeof busy - 1, 0);
      close_pcb(pcb);
      return ERR_OK;
   }
   log_printf("incoming call");
   m.incoming = pcb;
   tcp_recv(pcb, on_recv);
   tcp_err(pcb, on_err);
   m.s[S_RINGS] = 0;
   m.t_ring = net_millis() - RING_PERIOD;
   set_state(ST_RINGING);
   return ERR_OK;
}

static void answer(void)
{
   struct tcp_pcb *pcb = m.incoming;
   m.incoming = NULL;
   m.port = 0;
   go_online(pcb);
}

// --- AT commands ----------------------------------------------------------

static uint8_t upper(uint8_t c)
{
   if (c >= 'a' && c <= 'z')
      return c - 32;
   if (c >= 0xC1 && c <= 0xDA)   // shifted PETSCII letters
      return c - 0x80;
   return c;
}

static int number(const char **p)
{
   int v = 0;
   while (**p >= '0' && **p <= '9')
      v = v * 10 + (*(*p)++ - '0');
   return v;
}

static void info(int n)
{
   char buf[64];
   if (n == 0) {
      out_str("\r\nMEGA65 C128 MODEM " VERSION);
   } else if (n == 1) {
      const uint8_t *a = eth_netif.hwaddr;
      fmt_snprintf(buf, sizeof buf, "\r\nMAC %02x:%02x:%02x:%02x:%02x:%02x", a[0], a[1], a[2], a[3], a[4], a[5]);
      out_str(buf);
      out_str("\r\nIP  ");
      if (ip_addr_isany_val(*netif_ip_addr4(&eth_netif))) {
         out_str("NONE (WAITING FOR DHCP)");
      } else {
         out_str(ip4addr_ntoa(netif_ip4_addr(&eth_netif)));
         out_str("\r\nGW  ");
         out_str(ip4addr_ntoa(netif_ip4_gw(&eth_netif)));
         out_str("\r\nDNS ");
         out_str(ipaddr_ntoa(dns_getserver(0)));
      }
      fmt_snprintf(buf, sizeof buf, "\r\nLISTENING ON PORT %u\r\nFRAMES DROPPED %u",
          (unsigned)MODEM_LISTEN_PORT, (unsigned)eth_drops());
      out_str(buf);
      fmt_snprintf(buf, sizeof buf, "\r\nMISSED NMIS %u\r\nLONGEST WAIT %u MS",
          (unsigned)acia_agains(), (unsigned)net_gap_max());
      out_str(buf);
      fmt_snprintf(buf, sizeof buf, "\r\nTX TIMEOUTS %u", (unsigned)eth_tx_stuck());
      out_str(buf);
   } else if (n == 2) {
      m.dump_pos = 0;
      m.dump_len = 1;   // printed from modem_poll, a piece at a time
      out_str("\r\n");
   }
}

// Runs one command line; returns false if a result has already been given.
static bool execute(const char *p)
{
   while (*p) {
      char c = *p++;
      int n;
      switch (c) {
         case ' ':
            break;
         case 'A':
            if (m.state == ST_RINGING && m.incoming)
               answer();
            else
               result(RC_NO_CARRIER);
            return false;
         case 'D':
            if (m.state != ST_IDLE) {
               result(RC_ERROR);
               return false;
            }
            dial(p);
            return false;
         case 'E':
            m.echo = number(&p) != 0;
            break;
         case 'H':
            number(&p);
            if (m.state != ST_IDLE)
               hang_up(RC_OK, false);
            break;
         case 'I':
            n = number(&p);
            info(n);
            if (n == 2)
               return false;   // the log goes out first, then OK
            break;
         case 'O':
            number(&p);
            if (m.state == ST_ONLINE && m.pcb) {
               m.cmd_mode = false;
               result(RC_CONNECT);
            } else {
               result(RC_NO_CARRIER);
            }
            return false;
         case 'Q':
            m.quiet = number(&p) != 0;
            break;
         case 'V':
            m.verbose = number(&p) != 0;
            break;
         case 'Z':
            number(&p);
            if (m.state != ST_IDLE)
               hang_up(RC_OK, false);
            factory();
            break;
         case 'S':
            n = number(&p);
            if (n >= S_COUNT)
               goto error;
            if (*p == '=') {
               p++;
               m.s[n] = (uint8_t)number(&p);
            } else if (*p == '?') {
               char buf[8];
               p++;
               fmt_snprintf(buf, sizeof buf, "\r\n%03u", (uint32_t)m.s[n]);
               out_str(buf);
            } else {
               goto error;
            }
            break;
         case '&':
            c = *p++;
            n = number(&p);
            if (c == 'C')
               m.dcd_mode = (uint8_t)n;
            else if (c == 'D')
               m.dtr_mode = (uint8_t)n;
            else if (c == 'F')
               factory();
            else if (c != 'K' && c != 'W')
               goto error;
            break;
         case 'B': case 'L': case 'M': case 'X':
            number(&p);   // accepted, no effect
            break;
         case '*':        // tests: *T faults, *W hangs (the firmware's trap screen and watchdog);
            c = *p++;     // *H1, *H2[=seconds] load HyperRAM (hog.c)
            if (c == 'T')
               (void)*(volatile uint32_t *)0xF0100000u;   // nothing there: bus error
            else if (c == 'W')
               for (;;)
                  ;
            if (c == 'H' && (n = number(&p)) >= 1 && n <= 2) {
               uint32_t secs = 15;
               if (*p == '=') {
                  p++;
                  secs = (uint32_t)number(&p);
               }
               if (secs == 0)
                  goto error;
               char buf[40];
               fmt_snprintf(buf, sizeof buf, "\r\nHYPERRAM READS %u/S", hog(n, secs) / secs);
               out_str(buf);
               break;
            }
            goto error;
         default:
            goto error;
      }
   }
   return true;
error:
   result(RC_ERROR);
   return false;
}

static void command_line(void)
{
   m.line[m.len] = '\0';
   const char *p = m.line;
   while (*p == ' ')
      p++;
   if (p[0] == 'A' && p[1] == 'T') {
      strcpy(m.last, p);
      if (execute(p + 2))
         result(RC_OK);
   }
   m.len = 0;
}

static void command_input(void)
{
   int c;
   while (m.out_head - m.out_tail < OUTQ_SIZE / 2 && (c = acia_get()) >= 0) {
      if (m.echo)
         out_byte((uint8_t)c);
      uint8_t u = upper((uint8_t)c);
      if (u == '\r') {
         command_line();
      } else if (u == 8 || u == 0x14 || u == 0x7F) {   // BS, PETSCII DEL, ASCII DEL
         if (m.len)
            m.len--;
      } else if (m.prev == 'A' && u == '/') {   // A/ repeats the last command
         m.len = 0;
         if (m.last[0]) {
            out_str("\r");
            if (execute(m.last + 2))
               result(RC_OK);
         }
         u = 0;
      } else if (u >= ' ' && m.len < CMD_MAX) {
         m.line[m.len++] = (char)u;
      }
      m.prev = u;
   }
}

// --- online data ----------------------------------------------------------

static void telnet_send(uint8_t cmd, uint8_t opt)
{
   if (m.txlen + 3 > TXBUF_SIZE)
      return;   // 21 replies queued behind a full buffer: the far end does without
   m.txbuf[m.txlen++] = IAC;
   m.txbuf[m.txlen++] = cmd;
   m.txbuf[m.txlen++] = opt;
}

// We will do BINARY and SGA, and let the other side do those and ECHO.
static void telnet_option(uint8_t cmd, uint8_t opt)
{
   uint32_t bit = opt < 32 ? 1u << opt : 0;
   bool ok_us = opt == OPT_BINARY || opt == OPT_SGA;
   bool ok_them = ok_us || opt == OPT_ECHO;
   switch (cmd) {
      case DO:
         if (ok_us && !(m.we_will & bit)) {
            m.we_will |= bit;
            telnet_send(WILL, opt);
         } else if (!ok_us) {
            telnet_send(WONT, opt);
         }
         break;
      case DONT:
         if (m.we_will & bit) {
            m.we_will &= ~bit;
            telnet_send(WONT, opt);
         }
         break;
      case WILL:
         if (ok_them && !(m.we_do & bit)) {
            m.we_do |= bit;
            telnet_send(DO, opt);
         } else if (!ok_them) {
            telnet_send(DONT, opt);
         }
         break;
      case WONT:
         if (m.we_do & bit) {
            m.we_do &= ~bit;
            telnet_send(DONT, opt);
         }
         break;
   }
}

// Filters one received byte; returns -1 if it isn't data for the C128.
static int telnet_rx(uint8_t c)
{
   switch (m.iac_state) {
      case 0:
         if (c == IAC) {
            m.iac_state = 1;
            return -1;
         }
         if (c == '\r' && !(m.we_do & (1u << OPT_BINARY)))
            m.iac_state = 4;   // drop a NUL after CR
         return c;
      case 1:
         m.iac_state = 0;
         if (c == IAC)
            return IAC;
         if (c >= WILL) {
            m.iac_cmd = c;
            m.iac_state = 2;
         } else if (c == SB) {
            m.iac_state = 3;
         }
         return -1;
      case 2:
         m.iac_state = 0;
         telnet_option(m.iac_cmd, c);
         return -1;
      case 3:                   // inside a subnegotiation, until IAC SE
         if (c == IAC)
            m.iac_state = 5;
         return -1;
      case 5:
         m.iac_state = c == SE ? 0 : 3;
         return -1;
      case 4:
         m.iac_state = 0;
         if (c == 0)
            return -1;
         return telnet_rx(c);
   }
   return c;
}

// Passes received bytes to the C128 as fast as its FIFO takes them.
static void deliver_rx(void)
{
   if (!m.rx_len || !m.pcb || m.cmd_mode)
      return;
   int room = acia_rx_free();
   uint16_t used = 0;
   while (room > 0 && used < m.rx_len) {
      uint8_t c = m.rx[(m.rx_head + used) % RX_SIZE];
      if (m.rx_count++ == 0 && c == IAC && m.s[S_TELNET] == 0)
         m.telnet = true;   // the far end talks telnet
      int d = m.telnet ? telnet_rx(c) : c;
      if (d >= 0) {
         acia_put((uint8_t)d);
         room--;
      }
      used++;
   }
   if (used) {
      m.rx_head = (m.rx_head + used) % RX_SIZE;
      m.rx_len -= used;
      tcp_recved(m.pcb, used);
   }
}

static void data_input(void)
{
   uint32_t now = net_millis();
   uint32_t guard = (uint32_t)m.s[S_GUARD] * 20;
   int c;
   while (m.pcb && m.txlen < FLUSH_SIZE && (c = acia_get()) >= 0) {
      if (c == m.s[S_ESCAPE]) {
         if (m.esc_count == 0 ? now - m.t_last_tx >= guard : now - m.t_esc < guard) {
            m.esc_count++;
            m.t_esc = now;
         } else {
            m.esc_count = 0;
         }
      } else {
         m.esc_count = 0;
      }
      m.t_last_tx = now;
      m.txbuf[m.txlen++] = (uint8_t)c;
      if (m.telnet && c == IAC)
         m.txbuf[m.txlen++] = IAC;
   }
   if (m.esc_count >= 3 && now - m.t_esc >= guard) {
      m.esc_count = 0;
      m.cmd_mode = true;
      result(RC_OK);
   }
}

// A write lwIP can't take yet keeps the bytes for the next try; until then
// data_input leaves the C128's bytes in the ACIA, which holds the C128 up.
static void flush_tx(void)
{
   if (m.pcb && m.txlen && (m.txlen >= FLUSH_SIZE || net_millis() - m.t_last_tx >= FLUSH_IDLE) &&
       tcp_write(m.pcb, m.txbuf, m.txlen, TCP_WRITE_FLAG_COPY) == ERR_OK) {
      tcp_output(m.pcb);
      m.txlen = 0;
   }
}

// --- main loop ------------------------------------------------------------

static void timers(void)
{
   uint32_t now = net_millis();
   switch (m.state) {
      case ST_RESOLVING:
      case ST_CONNECTING:
         if (now - m.t_state > DIAL_TIMEOUT) {
            struct tcp_pcb *pcb = m.pcb;
            m.pcb = NULL;
            if (pcb) {
               forget(pcb);   // no on_err: this is NO ANSWER, not NO CARRIER
               tcp_abort(pcb);
            }
            hang_up(RC_NO_ANSWER, true);
         }
         break;
      case ST_RINGING:
         if (now - m.t_ring >= RING_PERIOD) {
            m.t_ring = now;
            if (m.s[S_RINGS] >= RING_MAX) {
               hang_up(RC_OK, false);
               break;
            }
            m.s[S_RINGS]++;
            result(RC_RING);
            if (m.s[S_AUTOANSWER] && m.s[S_RINGS] >= m.s[S_AUTOANSWER])
               answer();
         }
         break;
      default:
         break;
   }
}

static void lines(void)
{
   bool online = m.state == ST_ONLINE;
   acia_set_lines(m.dcd_mode == 0 || online, true, true);

   bool dtr = acia_dtr();
   if (m.dtr && !dtr && m.dtr_mode && online)
      hang_up(RC_NO_CARRIER, true);
   m.dtr = dtr;

   if (!acia_enabled() && m.state != ST_IDLE)
      hang_up(RC_OK, false);
}

static void drain_outq(void)
{
   int room = acia_rx_free();
   while (room > 0 && m.out_tail != m.out_head) {
      acia_put(m.outq[m.out_tail++ & (OUTQ_SIZE - 1)]);
      room--;
   }
   // ATI2 prints the log a piece at a time
   if (m.dump_len && m.out_head - m.out_tail < OUTQ_SIZE / 2) {
      char buf[OUTQ_SIZE / 2];
      size_t n = log_copy(&m.dump_pos, buf, sizeof buf);
      for (size_t i = 0; i < n; i++)
         out_byte((uint8_t)buf[i]);
      if (!n) {
         m.dump_len = 0;
         result(RC_OK);
      }
   }
}

void modem_init(void)
{
   factory();
   m.rx = heap_alloc(RX_SIZE);
   m.cmd_mode = true;
   m.state = ST_IDLE;

   struct tcp_pcb *l = tcp_new();
   if (l && tcp_bind(l, IP_ANY_TYPE, MODEM_LISTEN_PORT) == ERR_OK) {
      l = tcp_listen(l);
      tcp_accept(l, on_accept);
   }
   log_printf("modem " VERSION " ready");
}

// The C128 was reset: the program that talked to the modem is gone, so is the call.
// Settings stay, as on an external modem.
static void c128_reset(void)
{
   hang_up(RC_OK, false);
   m.out_head = m.out_tail = 0;
   m.dump_len = 0;
   m.len = 0;
   m.prev = 0;
   while (acia_get() >= 0)
      ;
}

void modem_poll(void)
{
   if (acia_events() & ACIA_EV_RESET)
      c128_reset();
   lines();
   timers();
   if (m.cmd_mode || m.state != ST_ONLINE) {
      command_input();
   } else {
      data_input();
      flush_tx();
   }
   drain_outq();
   if (m.out_tail == m.out_head && !m.dump_len) {
      deliver_rx();
      if (m.remote_closed && !m.rx_len && m.state == ST_ONLINE)
         hang_up(RC_NO_CARRIER, true);
      else if (m.remote_closed && m.state == ST_RINGING)
         hang_up(RC_OK, false);
   }
   if (m.txlen && m.cmd_mode)
      flush_tx();
}
