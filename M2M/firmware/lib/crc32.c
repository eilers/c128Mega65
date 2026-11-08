// CRC-32, reflected polynomial 0xEDB88320. The table is built in data RAM on first use,
// which costs the boot ROM a few bytes instead of 1 KB.
#include "crc32.h"

static uint32_t table[256];

uint32_t crc32_update(uint32_t crc, const void *buf, size_t len)
{
   if (!table[1])
      for (uint32_t i = 0; i < 256; i++) {
         uint32_t c = i;
         for (int k = 0; k < 8; k++)
            c = (c >> 1) ^ (c & 1 ? 0xEDB88320u : 0);
         table[i] = c;
      }
   const uint8_t *p = buf;
   crc = ~crc;
   while (len--)
      crc = table[(crc ^ *p++) & 0xFF] ^ (crc >> 8);
   return ~crc;
}
