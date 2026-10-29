// The network: lwIP on the MEGA65's Ethernet port and the SwiftLink modem, with the core
// hooks core_net and core_idle (fw/core.h). Each turn selects the network device on the
// device bus and puts back the one that was there.
#pragma once
#include <stdint.h>

uint32_t net_millis(void);
uint32_t net_rand(void);
uint32_t net_gap_max(void);   // the longest time without a turn since the last call, in ms
