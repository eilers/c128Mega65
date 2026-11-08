// The active SD card, in 512-byte blocks, through sdhost.vhd's registers.
#pragma once
#include <stdint.h>

#define SD_ERR_TIMEOUT 0xEEFF    // as QNICE's SD$ERR_TIMEOUT
#define SD_ERR_OTHER   0xEEFE    // error bit set without an error code

// Each returns 0, or sdhost's error code
int sd_init(void);               // (re)initialise the card
int sd_read(uint32_t block, void *buf);
int sd_write(uint32_t block, const void *buf);
