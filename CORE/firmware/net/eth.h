#pragma once
#include <stdint.h>
#include "lwip/netif.h"

extern struct netif eth_netif;
extern char         eth_unique_id[7];   // 6 hex digits, from the FPGA's DNA (REST's unique_id)

void     eth_init(void);    // start the MAC and add the netif (netcfg.c starts DHCP or not)
void     eth_poll(void);    // hand received frames to lwIP
uint32_t eth_drops(void);     // frames dropped: by the MAC, or for want of a pool pbuf
uint32_t eth_tx_stuck(void);  // frames not sent: the MAC had no room for 10 ms
