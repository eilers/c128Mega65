// test_http's fakes of the firmware around the REST routes (host_rest.c)
#pragma once
#include <stdint.h>

extern uint8_t c128_mem[65536];
extern int     dma_held_v, dma_z80_v, dma_polls, dma_jobs, dma_stops;
extern int     menu_buttons, menu_open, reboots, saves, net_applies, net_saves;
extern uint8_t settings[32][8], saved[32][8];   // the menu's bits by group and item, in use and in the file
extern int     drive_on[2];
extern char    reu_path[256];

// The drives: an image (a card path), write protect, type (0 D64, 1 D71, 2 D81), the internal
// floppy (its drive, or -1), hold, remaps; a 1541/1571 acts out a disk change over polls
extern int     drv_mounted[2], drv_ro[2], drv_type[2], drv_held[2], int1581, mounts, remaps;
extern char    drv_card[2][256];
// The drive ROM device (boot1.rom's six banks) and reloads; the cartridge device and its loads
extern uint16_t *drive_rom, *crt_mem;
extern int     rom_reloads[2], crt_loads, crt_parsed;
extern char    crt_shown[256];
// The C128: a reset (taken after polls), the Z80's moment, BASIC in C128 or C64 mode at READY,
// the keyboard buffer typed into it; boot_never: BASIC never comes; c64_cart: it boots C64
extern int     c128_basic, c128_c64, boot_never, c64_cart, typed_run;
extern char    typed[64];

void host_rest_init(void);
void c128_settle(void);
int  host_rest_card(const char *img);   // the card: a FAT image
