// The network for the host tests of UCI's network target (host_net.c)
#pragma once

#define HOST_PEER "10.0.0.1"   // the peer's address, and the firmware's

void host_net_init(void);
void host_net_poll(void);      // packets that are due, and lwIP's timers
void host_net_report(void);    // what the network saw, on stdout

extern long hn_pool_drops;     // frames for the firmware lost for want of a pool pbuf
// The web server's pages, for a test: the whole answer (malloc'ed, freed once sent) to a
// request's header, its length in *len; NULL for the server's own empty 200
extern unsigned char *(*hn_web)(const char *request, int *len);
