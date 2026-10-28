// The ROMs that globals.vhd says to load at startup (QNICE's crts-and-roms.asm,
// CRTROM_AUTOLOAD), the ones loaded from the menu, and the CSR of their devices.
#pragma once
#include <stdint.h>

void     roms_autoload(void);   // a mandatory ROM that won't load is fatal
uint16_t crtrom_csr_get(uint16_t dev, int reg);
void     crtrom_csr_set(uint16_t dev, int reg, uint16_t v);

// Manually loaded cartridges and ROMs, numbered as their menu items (OPTM_G_LOAD_ROM)
int      crtrom_count(void);
uint16_t crtrom_dev(int k);
int      crtrom_loaded(int k);  // its device holds a file (CSR status OK)
// Load a file into it with a progress bar on the screen (LOAD_IMAGE, DMA_LOAD,
// HANDLE_CRTROM_M): 0, or an error code and *msg, the core's or the device's message
int      crtrom_load(int k, const char *path, int group, const char **msg);
