#pragma once
#include "lwip/netif.h"

extern struct netif eth_netif;

void eth_init(void);   // add the netif and start DHCP
void eth_poll(void);   // hand received frames to lwIP
