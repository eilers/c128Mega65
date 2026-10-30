// The ROMs picked in place of the stock ones: files in /c128/roms over parts of boot0.rom
// (C_DEV_SYSTEM_ROM) and boot1.rom (C_DEV_DRIVE_ROM), the internal function ROM (the system
// ROM's device from 0x18000), and the boot cartridge, a .crt in /c128/carts loaded at every
// start. stores.c has them as Gideon's items, kept in the settings file; the menu's ROMs
// submenu and Boot line pick them (core.c).
#pragma once
#include <stdint.h>

#define ROMS_DIR   "/c128/roms"
#define CARTS_DIR  "/c128/carts"
#define PICK_MAX   33                // a name, as his longest (32) and its 0

// The picks, in the order of the ROMs submenu's lines; then the boot cartridge
enum { RP_K128, RP_B128, RP_K64, RP_B64, RP_CHAR, RP_FUNC, RP_DRIVE, RP_BOOT = RP_DRIVE + 6, RP_COUNT };
#define RP_DRV(d, t) (RP_DRIVE + 3 * (d) + (t))   // drive d (0 is drive 8), t 0 1541, 1 1571, 2 1581

enum { RS_STOCK, RS_FILE, RS_FAILED };            // what a pick did, last time it was applied

const char *rompick_name(int p);                  // "" for stock; a drive's default, his 1541.rom
const char *rompick_default(int p);
int         rompick_state(int p);
const char *rompick_dir(int p);
int         rompick_fits(int p, uint32_t size);   // a file of that size is a ROM for it
// The files a pick can name, from its folder (his list_kernals, list_roms, list_crts)
void        rompick_list(int p, void (*each)(const char *name, void *ctx), void *ctx);

// A pick's new name, then the changed ones (a bit per pick) into effect: system ROMs (the
// function ROM too) with the 8502 held by DMA, then the reset button's reset; a drive's with
// a reload if the drive runs that model; the boot cartridge at the next start. Before
// rompick_boot, only the names change.
void        rompick_set(int p, const char *name);
void        rompick_unset(int p);                 // back to stock: his default name if that is
void        rompick_apply(uint32_t changed);
// At start, the C128 still in reset: every pick over the stock ROMs. The boot cartridge then
// loads from the main loop, once the C128 runs (rompick_poll).
void        rompick_boot(void);
void        rompick_poll(void);
