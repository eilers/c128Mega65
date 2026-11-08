// The network's settings: the host name, REST's password, the static address and where his
// two streams go. They are typed in the menu or set by LOAD_CONFIG and REST, and kept in the
// settings file (stores.c has their names and defaults). REST and FTP on or off and DHCP are menu items (config.vhd), saved
// with the menu.
#pragma once

enum { NC_HOST, NC_PASS, NC_IP, NC_MASK, NC_GW, NC_DNS, NC_STREAM, NC_COUNT = NC_STREAM + 2 };
#define NC_LEN 40                     // a string's room: his stream targets have 36 characters

extern char netcfg[NC_COUNT][NC_LEN];

// The settings and the menu's REST, FTP and DHCP items into effect, now or when the network starts
void        netcfg_apply(void);
void        netcfg_start(void);       // the network's start, from inside its first turn
// NC_IP to NC_DNS as the interface has them now, "" without an address
const char *netcfg_live(int k);
