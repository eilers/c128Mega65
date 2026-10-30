// The network's settings (netcfg.h). The file holds only the strings; it is read through
// stores.c, as LOAD_CONFIG reads a file, so it can carry his other items too.
#include <string.h>
#include "config_ids.h"
#include "core.h"
#include "eth.h"
#include "ff.h"
#include "fmt.h"
#include "http.h"
#include "net.h"
#include "netcfg.h"
#include "stores.h"
#include "lwip/dhcp.h"
#include "lwip/dns.h"
#include "lwip/ip4_addr.h"

char netcfg[NC_COUNT][32];

static struct {
   int  dhcp, rest;                  // what the interface has now; -1 before the first
   char host[32];
} done = { -1, -1, "" };

static void no_log(const char *fmt, ...)
{
   (void)fmt;
}

int netcfg_load(void)
{
   FIL f;
   stores_net_defaults();
   if (f_open(&f, NETCFG_FILE, FA_READ) != FR_OK)
      return -1;
   stores_begin();
   int good = stores_read(&f, 1u << ST_ETH | 1u << ST_NET, no_log);
   f_close(&f);
   if (good)
      stores_apply();
   return 0;
}

int netcfg_save(void)
{
   FIL f;
   char line[64];
   UINT put;
   int last = -1, ok = 1;
   if (f_open(&f, NETCFG_FILE, FA_WRITE | FA_CREATE_ALWAYS) != FR_OK)
      return -1;
   for (int i = 0; i < stores_count && ok; i++) {
      const item_t *it = &stores_items[i];
      if ((it->store != ST_ETH && it->store != ST_NET) || (it->type != IT_STRING && it->type != IT_PASS))
         continue;
      int n = 0;
      if (it->store != last)
         n = fmt_snprintf(line, sizeof line, "[%s]\n", store_name(it->store));
      last = it->store;
      n += fmt_snprintf(line + n, sizeof line - n, "%s=%s\n", it->name, item_text(it));
      ok = f_write(&f, line, n, &put) == FR_OK && put == (UINT)n;
   }
   return f_close(&f) == FR_OK && ok ? 0 : -1;
}

static void addr(int k, ip4_addr_t *a)
{
   if (!ip4addr_aton(netcfg[k], a))
      ip4_addr_set_zero(a);
}

// From inside a turn, or under net_lock
static void apply_now(void)
{
   int dhcp = shell_setting(OPTM_G_NET_DHCP, 0), rest = shell_setting(OPTM_G_NET_REST, 0);
   struct netif *nif = &eth_netif;

   rest_set_password(netcfg[NC_PASS]);
   if (rest != done.rest)
      http_listen(rest);
   if (strcmp(netcfg[NC_HOST], done.host)) {
      strcpy(done.host, netcfg[NC_HOST]);
      netif_set_hostname(nif, done.host);
      if (dhcp && done.dhcp == 1)
         dhcp_renew(nif);              // the server learns the new name
   }
   if (dhcp && done.dhcp != 1) {
      netif_set_addr(nif, IP4_ADDR_ANY4, IP4_ADDR_ANY4, IP4_ADDR_ANY4);
      dhcp_start(nif);
   } else if (!dhcp) {
      ip4_addr_t ip, mask, gw, dns;
      if (done.dhcp == 1)
         dhcp_release_and_stop(nif);
      addr(NC_IP, &ip);
      addr(NC_MASK, &mask);
      addr(NC_GW, &gw);
      addr(NC_DNS, &dns);
      netif_set_addr(nif, &ip, &mask, &gw);
      ip_addr_t d;
      ip_addr_copy_from_ip4(d, dns);
      dns_setserver(0, &d);
   }
   done.dhcp = dhcp;
   done.rest = rest;
}

void netcfg_apply(void)
{
   if (!net_started())
      return;                          // the network's start applies them
   uint32_t dev = net_lock();
   apply_now();
   net_unlock(dev);
}

void netcfg_start(void)
{
   apply_now();
}

const char *netcfg_live(int k)
{
   static char text[16];
   const ip4_addr_t *a;
   if (!net_started() || ip4_addr_isany_val(*netif_ip4_addr(&eth_netif)))
      return "";
   switch (k) {
   case NC_IP:   a = netif_ip4_addr(&eth_netif); break;
   case NC_MASK: a = netif_ip4_netmask(&eth_netif); break;
   case NC_GW:   a = netif_ip4_gw(&eth_netif); break;
   default:      a = ip_2_ip4(dns_getserver(0)); break;
   }
   return ip4addr_ntoa_r(a, text, sizeof text) ? text : "";
}
