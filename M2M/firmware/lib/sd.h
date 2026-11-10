// The active SD card, in 512-byte blocks, through sdhost.vhd's registers.
#pragma once
#include <stdint.h>

#define SD_ERR_TIMEOUT 0xEEFF    // as QNICE's SD$ERR_TIMEOUT
#define SD_ERR_OTHER   0xEEFE    // error bit set without an error code

// Each returns 0, or sdhost's error code
int sd_init(void);               // (re)initialise the card
int sd_read(uint32_t block, void *buf);
int sd_write(uint32_t block, const void *buf);

// For lib/sd_diag.c: the last failure, and the controller's mode before it
typedef struct {
   uint16_t err;       // sdhost's error code, 0: none since sd_init
   uint32_t block;     // the block it failed on, SD_AT_INIT: the init
   uint32_t mode;      // sd_mode() after the last command that worked, 0: none
} sd_log_t;
#define SD_AT_INIT 0xFFFFFFFFu
extern sd_log_t sd_log;

// M2M_SD_STATUS (sdhost.vhd register 6) in bits 15..0, the card type (CSR bits 13..12) in
// bits 17..16
uint32_t sd_mode(void);
