// The network's turns: from the main loop (core_net), and during long disk work between
// sectors (core_idle), so a big copy or a slow menu doesn't starve the 6551's 64-byte FIFOs
// and the MAC. lwIP's callbacks only change the network's own state; nothing here touches
// the card, so a turn in the middle of FatFs is safe. A turn never runs inside another, nor
// inside UCI's calls into lwIP (net_lock).
#include <stdbool.h>
#include "acia.h"
#include "core.h"
#include "eth.h"
#include "http.h"
#include "log.h"
#include "modem.h"
#include "net.h"
#include "net_hw.h"
#include "netcfg.h"
#include "lwip/init.h"
#include "lwip/timeouts.h"
#include "lwip/ip_addr.h"

#define IDLE_TURN (CPU_HZ / 1000)   // a turn from core_idle at most every millisecond

static bool     started, busy;
static uint32_t last_turn, gap_max;
static uint32_t seed;

uint32_t net_millis(void)
{
   uint32_t hi, lo;
   do {
      hi = MTIME_HI;
      lo = MTIME_LO;
   } while (hi != MTIME_HI);
   return (uint32_t)((((uint64_t)hi << 32) | lo) / (CPU_HZ / 1000));
}

uint32_t net_rand(void)
{
   seed ^= seed << 13;
   seed ^= seed >> 17;
   seed ^= seed << 5;
   return seed;
}

uint32_t sys_now(void)
{
   return net_millis();
}

uint32_t net_gap_max(void)
{
   uint32_t g = gap_max / (CPU_HZ / 1000);
   gap_max = 0;
   return g;
}

static void on_status(struct netif *nif)
{
   if (!ip_addr_isany_val(*netif_ip_addr4(nif)))
      log_printf("ip %s", ip4addr_ntoa(netif_ip4_addr(nif)));
}

static void start(void)
{
   seed = NET_DNA(0) ^ (uint32_t)NET_DNA(1) << 16 ^ MTIME_LO;
   if (!seed)
      seed = 1;
   log_printf("start");
   last_turn = MTIME_LO;
   lwip_init();
   eth_init();
   netif_set_status_callback(&eth_netif, on_status);
   modem_init();
   http_start();
   started = true;
   netcfg_start();
}

int net_started(void)
{
   return started;
}

void core_net(void)
{
   if (busy)
      return;
   busy = true;
   uint32_t dev = M2M_RAMROM_DEV;
   M2M_RAMROM_DEV = NET_DEVICE;
   if (!started)
      start();
   uint32_t now = MTIME_LO;
   if (now - last_turn > gap_max)
      gap_max = now - last_turn;
   last_turn = now;
   eth_poll();
   sys_check_timeouts();
   modem_poll();
   M2M_RAMROM_DEV = dev;
   busy = false;
}

void core_idle(void)
{
   if (started && MTIME_LO - last_turn >= IDLE_TURN)
      core_net();
}

uint32_t net_lock(void)
{
   uint32_t dev = M2M_RAMROM_DEV;
   M2M_RAMROM_DEV = NET_DEVICE;
   if (!started)
      start();
   busy = true;
   return dev;
}

void net_unlock(uint32_t dev)
{
   busy = false;
   M2M_RAMROM_DEV = dev;
}
