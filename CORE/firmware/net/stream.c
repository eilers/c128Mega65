// His data streams (software/io/network/data_streamer.cc at 4b00732): a start resolves the
// target, finds the MAC to send to, writes the header into the hardware and turns the stream
// on. His runs in one go in its own task; here the waits (DNS, ARP) are steps of the network's
// turns. His REST route (route_streams.cc) is in rest.c, his F5 menu's start and stop are the
// Network submenu's stream lines (core.c).
// Ported from the 1541 Ultimate firmware (github.com/GideonZ/1541ultimate), Copyright
// Gideon Zweijtzer and contributors, GPL v3 as this core; changed for c128Mega65 in 2026.
#include "lwip/dns.h"                  // first: macOS's headers define BYTE_ORDER and htons too
#include "lwip/etharp.h"
#include "lwip/udp.h"
#include <stdint.h>
#include <string.h>
#include "config_ids.h"
#include "core.h"
#include "eth.h"
#include "m2m_rv.h"
#include "net.h"
#include "net_hw.h"
#include "netcfg.h"
#include "rest_priv.h"
#include "stream.h"

#define ARP_TRIES 10                    // his: a probe, then 20 ticks of 5 ms, ten times
#define ARP_MS    100
#define DONE_MAX  8                     // the last starts' results, for their REST answers

enum { S_IDLE, S_DNS, S_ARP };

static struct {
   int        state, ticket, menu, on, tries, dport;
   char       port[40];                 // what came after the ':', or ""
   ip4_addr_t dst, query;
   uint32_t   t;
} st[STREAMS];
static struct { int ticket, ss; } done[DONE_MAX];
static int         done_at, tickets;
static const char *menu_msg;            // his popup for a start from the menu that failed

volatile int stream_waiting, stream_dirty;

// The hardware's words, on the network device whatever the device bus had
static void hw_ctrl(uint32_t on)
{
   uint32_t dev = M2M_RAMROM_DEV;
   M2M_RAMROM_DEV = NET_DEVICE;
   STREAM_CTRL = on;
   M2M_RAMROM_DEV = dev;
}

static uint32_t hw_on(void)
{
   uint32_t dev = M2M_RAMROM_DEV;
   M2M_RAMROM_DEV = NET_DEVICE;
   uint32_t v = STREAM_CTRL;
   M2M_RAMROM_DEV = dev;
   return v;
}

void stream_init(void)
{
   hw_ctrl(0);
}

// His calculate_udp_headers: his template, with the payload's lengths in the IP and UDP
// headers (his hardware adds them) and the IP checksum over that
void stream_header(uint8_t h[42], int id, const uint8_t *src_mac, const uint8_t *dst_mac,
                   const uint8_t *src_ip, const uint8_t *dst_ip, int port)
{
   static const uint8_t tmpl[42] = {
      0x01, 0x00, 0x5E, 0x00, 0x01, 0x40, 0x00, 0x15, 0x41, 0xAA, 0xAA, 0x01, 0x08, 0x00,
      0x45, 0x00, 0x00, 0x1C, 0xBB, 0xBB, 0x40, 0x00, 0x40, 0x11, 0x00, 0x00,
      0xEF, 0x00, 0x01, 0x40, 0xC0, 0xA8, 0x01, 0x40,
      0xD0, 0x00, 0x2A, 0xF8, 0x00, 0x08, 0x00, 0x00 };
   static const uint16_t sport[] = { 0xD000, 0xD400, 0x196E };   // 53248, 54272, 6510
   static const uint16_t payload[] = { 780, 770, 1444 };          // what follows the UDP header
   memcpy(h, tmpl, sizeof tmpl);
   memcpy(h, dst_mac, 6);
   memcpy(h + 6, src_mac, 6);
   h[16] = (20 + 8 + payload[id]) >> 8;
   h[17] = (uint8_t)(20 + 8 + payload[id]);
   memcpy(h + 26, src_ip, 4);
   memcpy(h + 30, dst_ip, 4);
   h[34] = sport[id] >> 8;
   h[35] = (uint8_t)sport[id];
   h[36] = (uint8_t)(port >> 8);
   h[37] = (uint8_t)port;
   h[38] = (8 + payload[id]) >> 8;
   h[39] = (uint8_t)(8 + payload[id]);
   uint32_t sum = 0;
   for (int i = 0; i < 10; i++)
      sum += (uint32_t)h[2 * i + 14] << 8 | h[2 * i + 15];
   while (sum & 0xFFFF0000u)
      sum = (sum >> 16) + (sum & 0xFFFF);
   h[24] = 0xFF ^ (uint8_t)(sum >> 8);
   h[25] = 0xFF ^ (uint8_t)sum;
}

// The start ends: its result kept for its ticket, the menu told
static void finish(int id, int ss, const char *popup)
{
   done[done_at].ticket = st[id].ticket;
   done[done_at].ss = ss;
   done_at = (done_at + 1) % DONE_MAX;
   if (st[id].state == S_ARP)
      stream_waiting--;
   st[id].state = S_IDLE;
   if (ss && st[id].menu && popup)
      menu_msg = popup;
   stream_dirty = 1;
}

// The header in, the stream on. As his, a running stream keeps running (its sequence and frame
// count on): the header goes in just after one of its packets, before the next reads it.
#define BUSY_READS 50000                // ~5 ms of device reads: more than audio's 4 ms period

static void send_to(int id, const uint8_t *mac)
{
   uint8_t h[42];
   stream_header(h, id, eth_netif.hwaddr, mac, (const uint8_t *)&netif_ip4_addr(&eth_netif)->addr,
                 (const uint8_t *)&st[id].dst.addr, st[id].dport);
   uint32_t dev = M2M_RAMROM_DEV;
   M2M_RAMROM_DEV = NET_DEVICE;
   if (STREAM_CTRL & STREAM_ON(id)) {
      int n = 0;
      while (!(STREAM_CTRL & STREAM_BUSY(id)) && ++n < BUSY_READS)
         ;
      while (STREAM_CTRL & STREAM_BUSY(id) && ++n < BUSY_READS)
         ;
   }
   for (int i = 0; i < 21; i++)
      STREAM_HDR(id, i) = h[2 * i] | h[2 * i + 1] << 8;
   STREAM_CTRL = (STREAM_CTRL & (STREAM_ON(0) | STREAM_ON(1))) | STREAM_ON(id);
   M2M_RAMROM_DEV = dev;
   st[id].on = 1;
   finish(id, SS_OK, NULL);
}

// His sscanf("%d"): the port, unless no number starts the text
static int port_of(int id)
{
   const char *s = st[id].port;
   int n = 0, neg;
   if (!*s)
      return 11000 + id;
   while (*s == ' ' || (*s >= '\t' && *s <= '\r'))
      s++;
   neg = *s == '-';
   if (*s == '-' || *s == '+')
      s++;
   if (*s < '0' || *s > '9')
      return 11000 + id;
   while (*s >= '0' && *s <= '9')
      n = n * 10 + *s++ - '0';
   return neg ? -n : n;
}

// His ARP probe: two zero bytes to the port, so that lwIP asks for the MAC
static void probe(int id)
{
   struct udp_pcb *pcb = udp_new();
   if (pcb) {
      struct pbuf *p = pbuf_alloc(PBUF_TRANSPORT, 2, PBUF_RAM);
      if (p) {
         memset(p->payload, 0, 2);
         ip_addr_t a;
         ip_addr_copy_from_ip4(a, st[id].query);
         udp_sendto(pcb, p, &a, (uint16_t)st[id].dport);
         pbuf_free(p);
      }
      udp_remove(pcb);
   }
   st[id].t = net_millis();
}

// The target's address: a multicast group or the broadcast address has its MAC, any other
// host its own, or its gateway's off the subnet
static void resolved(int id, const ip4_addr_t *dst)
{
   const ip4_addr_t *me = netif_ip4_addr(&eth_netif), *mask = netif_ip4_netmask(&eth_netif);
   st[id].dst = *dst;
   st[id].query = *dst;
   if ((dst->addr & mask->addr) != (me->addr & mask->addr))
      st[id].query = *netif_ip4_gw(&eth_netif);
   st[id].dport = port_of(id);           // the header takes its low 16 bits, as his
   if (!st[id].dport) {
      finish(id, SS_INVALID_PARAMETER, "Destination Port cannot be 0.");
      return;
   }
   const uint8_t *b = (const uint8_t *)&dst->addr;
   if ((b[0] & 0xF8) == 0xE8) {          // his test: 232 to 239
      uint8_t mac[6] = { 0x01, 0x00, 0x5E, b[1] & 0x7F, b[2], b[3] };
      send_to(id, mac);
   } else if (dst->addr == 0xFFFFFFFFu) {
      static const uint8_t all[6] = { 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF };
      send_to(id, all);
   } else {
      st[id].state = S_ARP;
      st[id].tries = 0;
      stream_waiting++;
      probe(id);
   }
}

static void found(const char *name, const ip_addr_t *a, void *arg)
{
   int id = (uintptr_t)arg & 1, ticket = (uintptr_t)arg >> 1;
   (void)name;
   if (st[id].ticket != ticket || st[id].state != S_DNS)
      return;
   if (!a) {
      finish(id, SS_NETWORK_RESOLVE_ERROR, "Host could not be resolved.");
      return;
   }
   st[id].state = S_IDLE;
   resolved(id, ip_2_ip4(a));
}

int stream_start(int id, const char *target, int menu)
{
   char host[40] = { 0 };
   ip_addr_t a;

   if (st[id].state == S_ARP)
      stream_waiting--;
   st[id].state = S_IDLE;
   st[id].ticket = ++tickets;
   st[id].menu = menu;
   if (!netif_is_link_up(&eth_netif) || ip4_addr_isany_val(*netif_ip4_addr(&eth_netif))) {
      finish(id, SS_NO_NETWORK, "No (valid) link");
      return -SS_NO_NETWORK;
   }
   strncpy(host, target && *target ? target : netcfg[NC_STREAM + id], 36);
   if (!*host) {
      finish(id, SS_INVALID_PARAMETER, NULL);
      return -SS_INVALID_PARAMETER;
   }
   char *colon = strchr(host, ':');
   st[id].port[0] = 0;
   if (colon) {
      *colon = 0;
      strcpy(st[id].port, colon + 1);
   }
   int ticket = st[id].ticket;
   st[id].state = S_DNS;
   err_t e = dns_gethostbyname(host, &a, found, (void *)(uintptr_t)(ticket << 1 | id));
   if (e == ERR_OK) {
      st[id].state = S_IDLE;
      resolved(id, ip_2_ip4(&a));
   } else if (e != ERR_INPROGRESS) {
      finish(id, SS_NETWORK_RESOLVE_ERROR, "Host could not be resolved.");
   }
   if (st[id].state != S_IDLE)
      return ticket;
   int ss = stream_done(id, ticket);
   return -ss;
}

int stream_done(int id, int ticket)
{
   if (st[id].ticket == ticket && st[id].state != S_IDLE)
      return -1;
   for (int i = 0; i < DONE_MAX; i++)
      if (done[i].ticket == ticket)
         return done[i].ss;
   return SS_INTERNAL_ERROR;             // too long ago
}

int stream_pending(int id)
{
   return st[id].state != S_IDLE;
}

void stream_stop(int id)
{
   if (st[id].state != S_IDLE)
      finish(id, SS_ABORTED_BY_USER, NULL);
   st[id].on = 0;
   hw_ctrl(hw_on() & (STREAM_ON(0) | STREAM_ON(1)) & ~STREAM_ON(id));
   stream_dirty = 1;
}

int stream_running(int id)
{
   return st[id].on || st[id].state != S_IDLE;
}

// The ARP table, a probe's 100 ms after it went
void stream_poll(void)
{
   for (int id = 0; id < STREAMS; id++) {
      if (st[id].state != S_ARP || net_millis() - st[id].t < ARP_MS)
         continue;
      struct eth_addr *mac;
      const ip4_addr_t *ip;
      if (etharp_find_addr(&eth_netif, &st[id].query, &mac, &ip) >= 0)
         send_to(id, mac->addr);
      else if (++st[id].tries < ARP_TRIES)
         probe(id);
      else
         finish(id, SS_NETWORK_RESOLVE_ERROR, "Cannot find MAC of specified host.");
   }
}

void stream_menu_sync(void)
{
   static const int groups[STREAMS] = { OPTM_G_NET_VSTREAM, OPTM_G_NET_ASTREAM };
   stream_dirty = 0;
   for (int id = 0; id < STREAMS; id++)
      if (shell_setting(groups[id], 0) != stream_running(id))
         shell_force(groups[id], 0, stream_running(id));
   if (menu_msg) {
      shell_message(menu_msg);
      menu_msg = NULL;
   }
}
