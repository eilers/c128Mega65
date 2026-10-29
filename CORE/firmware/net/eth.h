#pragma once
#include <stdint.h>
#include "lwip/netif.h"

extern struct netif eth_netif;

void     eth_init(void);    // start the MAC, add the netif and start DHCP
void     eth_poll(void);    // hand received frames to lwIP
uint32_t eth_drops(void);   // frames the MAC dropped
