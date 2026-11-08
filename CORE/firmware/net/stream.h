// His data streams (data_streamer.cc): the VIC's picture and the sound over UDP, sent by the
// hardware (net.vhd) once the firmware has written a stream's Ethernet, IP and UDP header.
// The core has his video and audio streams, not his debug trace.
#pragma once
#include <stdint.h>

enum { STREAM_VIDEO, STREAM_AUDIO, STREAM_DEBUG };
#define STREAMS 2                       // the ones the core has

extern volatile int stream_waiting;     // streams waiting for an ARP answer: stream_poll has work
extern volatile int stream_dirty;       // the menu's ticks or a message to update (stream_menu_sync)

void stream_init(void);                 // the network's start: nothing streams
void stream_poll(void);                 // in a network turn, while stream_waiting
// His startStream: target is "host[:port]", or NULL or "" for the settings' one. In a network
// turn or under net_lock. 0 when it streams at once, -SS_* when it failed at once (rest_priv.h),
// else a ticket for stream_done while it resolves.
int  stream_start(int id, const char *target, int menu);
int  stream_done(int id, int ticket);   // -1 while it resolves, else its SS_*
int  stream_pending(int id);            // a start of it resolves
void stream_stop(int id);               // in a turn or under net_lock
int  stream_running(int id);            // it streams, or a start of it resolves
// From the main loop, while stream_dirty: the menu's ticks as the streams are, and the
// message of a start from the menu that failed
void stream_menu_sync(void);
// His calculate_udp_headers, with the lengths and checksum of the whole packet; the
// addresses' bytes in network order
void stream_header(uint8_t h[42], int id, const uint8_t *src_mac, const uint8_t *dst_mac,
                   const uint8_t *src_ip, const uint8_t *dst_ip, int port);
