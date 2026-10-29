// The REU's memory and its image file: the menu's REU File:%s item (core.c) and UCI's
// LOAD_REU and SAVE_REU (uci_ctrl.c, uci_dos.c). The firmware reaches the memory through
// rv_soc's windows, SDRAM on R4 and later and HyperRAM on R3 memory, where uci.vhd's word
// 0x011 says. REU byte A is byte A there, as reu_mapper.vhd lays them out.
#pragma once
#include <stdint.h>
#include "ff.h"

#define REU_DEFAULT_FILE "/c128/preload.reu"   // with none picked (Gideon's: /Usb0/preload.reu)

uint32_t    reu_size(void);              // the menu's REU size in bytes, 0 while it is off
uint32_t    reu_room(void);              // its memory: 16 MB in SDRAM, 512 KB in HyperRAM
const char *reu_file(void);              // the file picked in the menu, else REU_DEFAULT_FILE
int         reu_file_picked(void);
void        reu_file_set(const char *path);   // NULL: none picked

// A transfer between a file and the REU's memory: from f into the REU at at (a load), or
// from the REU at at into f (a save), left bytes in all. The rest starts at 0.
typedef struct {
   FIL     *f;
   int      save;
   uint32_t at, left, done;
   int      dma;                         // reu.c: 0 not looked at yet, 1 by DMA, -1 by the CPU
   uint16_t card;                        // the SD card the DMA's blocks are on
} reu_xfer_t;

// More of it: FR_OK, or FatFs's error. j->left is 0 once it is all done; a load also ends
// at the end of the file, a save on a full card (j->done says). Whole blocks of the file go
// by reu_sd.vhd's DMA, up to 32 KB a step; else up to max bytes through uci_data. Either
// way the REU's cache drops its line after a load's step, so the C128 sees the new bytes.
FRESULT reu_step(reu_xfer_t *j, uint32_t max);
