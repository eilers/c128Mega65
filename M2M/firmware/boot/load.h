// Loading the firmware file into HyperRAM (load.c).
#pragma once
#include "fw_image.h"

typedef union {
   fw_header_t h;
   uint8_t     block[FW_HEADER_SIZE];
} fw_block_t;

// Mounts the card, loads the file at path and checks it. Returns 0, or why it failed.
// hdr gets the file's header block, as far as it was read.
const char *fw_load(const char *path, fw_block_t *hdr);

extern const char fw_err_mount[];
extern int fw_fr;             // FatFs's result of the call that failed, 0: not a FatFs failure
extern int fw_mount_failed;   // the failure was the mount
