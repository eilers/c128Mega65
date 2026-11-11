// The drives' DOS ROMs: boot1.rom's six 32 KB banks (1541, 1571, 1581, each for drive 8 and
// drive 9) in HyperRAM, at the window drive_rom_server.vhd names (C_HMAP_DRVROM). Each drive
// keeps its bank in its own block RAM and asks for it again after a new model, a reload or the
// core's reset; it stays in reset until drvrom_poll has handed it the whole bank.
#pragma once
#include <stdint.h>
#include "ff.h"

#define DRVROM_BANK     32768u
#define DRVROM_SIZE     (6 * DRVROM_BANK)
#define DRVROM_AT(d, t) (((t) * 2u + (d)) * DRVROM_BANK)   // drive d's bank, t 0 1541, 1 1571, 2 1581

int  drvrom_load(FIL *f);              // boot1.rom into the library, every drive reloaded: 0, or -1
void drvrom_put(uint32_t at, const uint8_t *src, uint32_t n);   // n bytes into the library at at
int  drvrom_ready(int d);              // drive d has its ROM
void drvrom_reload(int d);             // drive d loads its bank again, held in reset meanwhile
void drvrom_poll(void);                // the main loop: the bytes the drives ask for
