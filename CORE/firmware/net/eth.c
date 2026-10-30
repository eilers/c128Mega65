// lwIP netif for the RMII MAC (M2M/vhdl/controllers/ethernet). Frames go byte by
// byte through the network device (net_hw.h); the hardware buffers whole frames both ways.
#include <string.h>
#include "eth.h"
#include "fmt.h"
#include "net.h"
#include "net_hw.h"
#include "lwip/etharp.h"
#include "netif/ethernet.h"

struct netif eth_netif;
char         eth_unique_id[7];
static uint32_t no_buffer, tx_stuck;   // frames lost: no pool pbuf for one, the MAC never ready

static err_t linkoutput(struct netif *nif, struct pbuf *p)
{
   (void)nif;
   uint32_t t0 = MTIME_LO;
   while (!(ETH_STATUS & ETH_TX_ROOM))
      if (MTIME_LO - t0 > CPU_HZ / 100) {
         tx_stuck++;
         return ERR_IF;   // MAC stuck: drop the frame
      }

   // The MAC doesn't pad, so short frames get zeros up to Ethernet's 60-byte minimum
   uint32_t len = p->tot_len < 60 ? 60 : p->tot_len;
   uint32_t left = len;
   for (struct pbuf *q = p; q; q = q->next) {
      const uint8_t *d = q->payload;
      for (uint16_t i = 0; i < q->len; i++)
         ETH_TX = d[i] | (--left == 0 ? ETH_LAST : 0);
   }
   while (left)
      ETH_TX = --left == 0 ? ETH_LAST : 0;
   return ERR_OK;
}

static err_t netif_setup(struct netif *nif)
{
   // A locally administered address from the FPGA's DNA, so every board differs. The bytes
   // go into the hash low first (bits 0..31, then 32..56), which keeps each board's address.
   uint32_t lo = NET_DNA(0) | (uint32_t)NET_DNA(1) << 16;
   uint32_t hi = (NET_DNA(2) | (uint32_t)NET_DNA(3) << 16) & 0x01FFFFFF;
   uint32_t h = 2166136261u;
   for (int i = 0; i < 8; i++) {
      h ^= (i < 4 ? lo >> (8 * i) : hi >> (8 * (i - 4))) & 0xFF;
      h *= 16777619u;
   }
   // His unique_id: 24 bits from the same serial as the MAC, mixed another way
   uint32_t u = 2166136261u;
   for (int i = 7; i >= 0; i--) {
      u ^= (i < 4 ? lo >> (8 * i) : hi >> (8 * (i - 4))) & 0xFF;
      u *= 16777619u;
   }
   fmt_snprintf(eth_unique_id, sizeof eth_unique_id, "%06X", (u ^ u >> 24) & 0xFFFFFF);
   nif->hwaddr[0] = 0x02;
   nif->hwaddr[1] = 0xC1;
   nif->hwaddr[2] = 0x28;
   nif->hwaddr[3] = h >> 16;
   nif->hwaddr[4] = h >> 8;
   nif->hwaddr[5] = h;
   nif->hwaddr_len = ETH_HWADDR_LEN;
   nif->mtu = 1500;
   nif->flags = NETIF_FLAG_BROADCAST | NETIF_FLAG_ETHARP | NETIF_FLAG_ETHERNET | NETIF_FLAG_LINK_UP;
   nif->name[0] = 'e';
   nif->name[1] = '0';
   nif->output = etharp_output;
   nif->linkoutput = linkoutput;
   netif_set_hostname(nif, "c128-mega65");
   return ERR_OK;
}

void eth_init(void)
{
   ETH_CTRL = ETH_RUN;
   netif_add(&eth_netif, IP4_ADDR_ANY4, IP4_ADDR_ANY4, IP4_ADDR_ANY4, NULL, netif_setup, ethernet_input);
   netif_set_default(&eth_netif);
   netif_set_up(&eth_netif);
}

void eth_poll(void)
{
   // A few frames per call, so the modem side keeps getting turns
   for (int frames = 0; frames < 4; frames++) {
      uint32_t v = ETH_RX;
      if (!(v & ETH_RX_VALID))
         return;

      // A pool pbuf holds a whole frame (PBUF_POOL_BUFSIZE); drain the frame even without one.
      // The MAC has the whole frame before it shows the first byte.
      struct pbuf *p = pbuf_alloc(PBUF_RAW, PBUF_POOL_BUFSIZE, PBUF_POOL);
      uint8_t *d = p ? p->payload : NULL;
      uint16_t n = 0;
      for (;;) {
         if (d && n < PBUF_POOL_BUFSIZE)
            d[n++] = (uint8_t)v;
         if (v & ETH_LAST)
            break;
         do
            v = ETH_RX;
         while (!(v & ETH_RX_VALID));
      }
      if (!p) {
         no_buffer++;
         continue;
      }
      pbuf_realloc(p, n);
      if (eth_netif.input(p, &eth_netif) != ERR_OK)
         pbuf_free(p);
   }
}

uint32_t eth_drops(void)
{
   return ETH_DROPS + no_buffer;
}

uint32_t eth_tx_stuck(void)
{
   return tx_stuck;
}
