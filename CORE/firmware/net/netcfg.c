// The network's settings (netcfg.h): into effect
#include <string.h>
#include "config_ids.h"
#include "core.h"
#include "eth.h"
#include "ftp.h"
#include "http.h"
#include "net.h"
#include "netcfg.h"
#include "lwip/dhcp.h"
#include "lwip/dns.h"
#include "lwip/ip4_addr.h"

char netcfg[NC_COUNT][32];

static struct {
   int  dhcp, rest, ftp;             // what the interface has now; -1 before the first
   char host[32];
} done = { -1, -1, -1, "" };

static void addr(int k, ip4_addr_t *a)
{
   if (!ip4addr_aton(netcfg[k], a))
      ip4_addr_set_zero(a);
}

// From inside a turn, or under net_lock
static void apply_now(void)
{
   int dhcp = shell_setting(OPTM_G_NET_DHCP, 0), rest = shell_setting(OPTM_G_NET_REST, 0);
   int ftp = shell_setting(OPTM_G_NET_FTP, 0);
   struct netif *nif = &eth_netif;

   rest_set_password(netcfg[NC_PASS]);
   if (rest != done.rest)
      http_listen(rest);
   if (ftp != done.ftp)
      ftp_listen(ftp);
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
   done.ftp = ftp;
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
