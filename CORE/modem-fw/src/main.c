// Modem firmware for the C128 core's NEORV32: lwIP on the MEGA65's Ethernet port,
// a Hayes modem on the C128's emulated SwiftLink.
#include "acia.h"
#include "eth.h"
#include "log.h"
#include "modem.h"
#include "util.h"
#include "lwip/init.h"
#include "lwip/timeouts.h"
#include "lwip/ip_addr.h"

uint32_t sys_now(void)
{
   return millis();
}

static void on_status(struct netif *nif)
{
   if (!ip_addr_isany_val(*netif_ip_addr4(nif)))
      log_printf("ip %s", ip4addr_ntoa(netif_ip4_addr(nif)));
}

int main(void)
{
   log_printf("boot");
   lwip_init();
   eth_init();
   netif_set_status_callback(&eth_netif, on_status);
   modem_init();
   for (;;) {
      eth_poll();
      sys_check_timeouts();
      modem_poll();
   }
}
