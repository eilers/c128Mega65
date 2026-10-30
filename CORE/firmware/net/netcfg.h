// The network's settings: the host name, REST's password and the static address. They are
// typed in the menu or set by LOAD_CONFIG and REST, and kept in NETCFG_FILE in Gideon's
// settings format (stores.c has their names and defaults). REST on or off and DHCP are
// menu items (config.vhd), saved with the menu.
#pragma once

#define NETCFG_FILE "/c128/network.cfg"

enum { NC_HOST, NC_PASS, NC_IP, NC_MASK, NC_GW, NC_DNS, NC_COUNT };

extern char netcfg[NC_COUNT][32];

int         netcfg_load(void);        // the defaults, then the card's file: 0, or -1 without it
int         netcfg_save(void);        // 0, or -1
// The settings and the menu's REST and DHCP items into effect, now or when the network starts
void        netcfg_apply(void);
void        netcfg_start(void);       // the network's start, from inside its first turn
// NC_IP to NC_DNS as the interface has them now, "" without an address
const char *netcfg_live(int k);
