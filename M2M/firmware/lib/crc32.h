// CRC-32 as zlib's crc32(): start with 0, feed the bytes in any number of pieces.
#pragma once
#include <stddef.h>
#include <stdint.h>

uint32_t crc32_update(uint32_t crc, const void *buf, size_t len);
