// The ROMs that globals.vhd says to load at startup (M2M/rom/crts-and-roms.asm,
// CRTROM_AUTOLOAD), and the CSR of the cartridge and ROM devices.
#pragma once
#include <stdint.h>

void     roms_autoload(void);   // a mandatory ROM that won't load is fatal
uint16_t crtrom_csr_get(uint16_t dev, int reg);
void     crtrom_csr_set(uint16_t dev, int reg, uint16_t v);
