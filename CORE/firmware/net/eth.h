#pragma once
#include <stdint.h>
#include "lwip/netif.h"

extern struct netif eth_netif;

void     eth_init(void);    // start the MAC, add the netif and start DHCP
void     eth_poll(void);    // hand received frames to lwIP
uint32_t eth_drops(void);     // frames dropped: by the MAC, or for want of a pool pbuf
uint32_t eth_tx_stuck(void);  // frames not sent: the MAC had no room for 10 ms
