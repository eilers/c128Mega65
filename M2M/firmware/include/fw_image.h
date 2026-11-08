// The firmware file, as tools/fw_image.py makes it: a 512-byte header block, then the
// image, which the boot ROM loads at load_addr and starts at entry.
#pragma once
#include <stdint.h>

#define FW_PATH         "/c128/firmware.bin"   // 8.3 names only: the boot ROM has no long names
#define FW_MAGIC        0x5652324Du            // "M2RV"
#define FW_HEADER_VER   1u
#define FW_HEADER_SIZE  512u

typedef struct {
   uint32_t magic;
   uint32_t header_ver;
   uint32_t hw_ver;         // the M2M_RV_HW_VERSION it was built for
   uint32_t load_addr;
   uint32_t entry;
   uint32_t size;           // of the image, in bytes
   uint32_t crc32;          // of the image (zlib's CRC-32)
   uint32_t reserved;
   char     version[32];    // shown on the screen; NUL-terminated
} fw_header_t;
