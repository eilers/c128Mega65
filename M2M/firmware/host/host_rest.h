// test_http's fakes of the firmware around the REST routes (host_rest.c)
#pragma once
#include <stdint.h>

extern uint8_t c128_mem[65536];
extern int     dma_held_v, dma_z80_v, dma_polls, dma_jobs, dma_stops;
extern int     menu_buttons, menu_open, reboots, saves, net_applies, net_saves;
extern uint8_t settings[32][8], saved[32][8];   // the menu's bits by group and item, in use and in the file
extern int     drive_on[2];
extern char    reu_path[256];

void host_rest_init(void);
