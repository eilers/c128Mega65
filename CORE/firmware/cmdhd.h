// The CMD HD (CORE/vhdl/cmdhd): its disk image, its power and buttons, and the SCSI disk's
// commands, which the firmware answers as VICE's scsi.c does.
#pragma once
#include <stdint.h>

#define HD_DIR      "/c128/hd"      // where the browser starts, if the card has it
#define HD_ROM_SIZE 16384

enum { HD_RESET_NORMAL, HD_RESET_CONFIG, HD_RESET_INSTALL };   // the front panel at reset
enum { HD_BTN_WP = 2, HD_BTN_SWAP8 = 4, HD_BTN_SWAP9 = 8 };     // CSR 0x008 bits

int         cmdhd_present(void);          // the bitstream has the HD
void        cmdhd_init(void);
// The boot ROM, 16 KB (a 32 KB dump is two copies): from the file f at its start
int         cmdhd_rom_load(void *f);
void        cmdhd_rom_clear(void);
// The image: 0, or why not in *msg. The drive starts cold, in install mode if the image is
// empty (shorter than the system's place, as VICE's).
int         cmdhd_mount(const char *path, const char **msg);
void        cmdhd_unmount(void);
const char *cmdhd_path(void);             // "" without one
// On the bus (the menu's Drive enabled), its bus ID (the DOS reads it from the disk's
// configuration block, which the firmware patches as it goes by, as VICE does)
void        cmdhd_set_on(int on);
void        cmdhd_set_id(int id);
int         cmdhd_running(void);
// A reset with the front panel's buttons held, as VICE's; the mode it started in, until
// HD DOS runs (the boot ROM answers at device 30 until then)
void        cmdhd_reset(int mode);
int         cmdhd_mode(void);
// New, empty image of size bytes (a multiple of 512): FR_* of FatFs
int         cmdhd_create(const char *path, uint32_t size);
// From the main loop and from FatFs's disk work: never waits
void        cmdhd_poll(void);
