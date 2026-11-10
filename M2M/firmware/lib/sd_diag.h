// Why the SD card can't be used, in a few short lines for the boot ROM's screen and the
// Shell's (lib/sd_diag.c).
#pragma once

#define SD_DIAG_W     43
#define SD_DIAG_LINES 8     // the last one stays empty

typedef struct {
   int  n;
   char line[SD_DIAG_LINES][SD_DIAG_W + 1];
} sd_diag_t;

// fr: FatFs's result of the call that failed, 0 for none. Gives, in hex:
//   FatFs NN, SD error EEnn at BBBBBBBB|init     FatFs's FRESULT, sdhost's error, its block
//   SD mode TSSSS, now TSSSS                      sd_mode() after the last command that
//                                                 worked (0: none since the init), and now
// sd_mode(): T card type (0 unknown, 1 SD v1, 2 SD v2, 3 SDHC/SDXC); SSSS sdhost.vhd
// register 6: bits 15..8 retries, 6..5 sample point, 4 50 MHz, 3 SPI only, 2 ready,
// 1 SPI at 12.5 MHz, 0 native 4-bit bus (else SPI).
// When fr is FR_NO_FILESYSTEM it reads block 0 and the partitions through FatFs's disk
// layer: "Block 0: ..." and "Pn TT@LLLLLLLL: <likely reason>" per partition.
void sd_diag(sd_diag_t *d, int fr);
