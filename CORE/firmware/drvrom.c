// The drives' DOS ROMs (drvrom.h)
#include "core.h"
#include "dmem.h"
#include "drvrom.h"
#include "m2m_rv.h"

#define DRIVE_ROM  0x0102u             // C_DEV_DRIVE_ROM (globals.vhd): drive_rom_server.vhd
#define CSR        (0xFFFFu << 12)     // its registers, device words:
#define R_READY    0x000               // R: bit i drive i has its ROM. W: bit i reloads drive i
#define R_WINDOW   0x001               // R: the library's HyperRAM window, in 8 KB
#define R_REQ      0x002               // R, 32 bits: bit 31 a drive asks, 18..0 the byte it asks for
#define R_DATA     0x004               // W, 32 bits: the next four bytes, from that byte's 4-byte boundary
#define CHUNK      256u                // bytes between two looks at what is asked for
#define TURN       (4 * DRVROM_BANK)   // the most a main-loop turn feeds

typedef uint32_t __attribute__((may_alias)) word_t;

static uint8_t *lib;

DMEM_CODE static uint16_t csr_get(int reg)
{
   uint32_t dev = M2M_RAMROM_DEV;
   M2M_RAMROM_DEV = DRIVE_ROM;
   uint16_t v = M2M_DEV16[CSR + reg];
   M2M_RAMROM_DEV = dev;
   return v;
}

static void csr_set(int reg, uint16_t v)
{
   uint32_t dev = M2M_RAMROM_DEV;
   M2M_RAMROM_DEV = DRIVE_ROM;
   M2M_DEV16[CSR + reg] = v;
   M2M_RAMROM_DEV = dev;
}

// The library: HyperRAM window w starts at CPU byte w * 8 KB
DMEM_CODE static uint8_t *library(void)
{
   if (!lib)
      lib = (uint8_t *)(M2M_HYPERRAM + (uint32_t)csr_get(R_WINDOW) * 8192);
   return lib;
}

// Whole words where it can: each one access through rv_soc's bridge, not four
void drvrom_put(uint32_t at, const uint8_t *src, uint32_t n)
{
   if (at >= DRVROM_SIZE)
      return;
   if (n > DRVROM_SIZE - at)
      n = DRVROM_SIZE - at;
   uint8_t *dst = library() + at;
   for (; n && ((uintptr_t)dst & 3); n--)
      *dst++ = *src++;
   for (; n >= 4; n -= 4, dst += 4, src += 4)
      *(word_t *)dst = ((uintptr_t)src & 3) ? src[0] | src[1] << 8 | src[2] << 16 | (uint32_t)src[3] << 24 :
                                              *(const word_t *)src;
   while (n--)
      *dst++ = *src++;
}

// boot1.rom into the library: a shorter file leaves zeros after it, a longer one is cut
int drvrom_load(FIL *f)
{
   static const uint8_t zeros[512];
   uint8_t b[512];
   uint32_t at = 0;
   UINT got;
   do {
      uint32_t want = DRVROM_SIZE - at < sizeof b ? DRVROM_SIZE - at : sizeof b;
      if (f_read(f, b, want, &got) != FR_OK)
         return -1;
      drvrom_put(at, b, got);
      at += got;
   } while (got == sizeof b && at < DRVROM_SIZE);
   for (; at < DRVROM_SIZE; at += sizeof zeros)
      drvrom_put(at, zeros, sizeof zeros);
   // Even a drive that stopped in the middle of its bank starts it again
   csr_set(R_READY, 3);
   return 0;
}

DMEM_CODE int drvrom_ready(int d)
{
   return csr_get(R_READY) >> d & 1;
}

void drvrom_reload(int d)
{
   csr_set(R_READY, 1u << d);
}

// Reading R_REQ starts a feed at the byte asked for; a feed that a drive stops taking (a
// reload, a new model, a reset, the other drive's turn) is dropped by the hardware, and the
// next look finds what is asked for now. Between chunks the network gets its turn.
DMEM_CODE void drvrom_poll(void)
{
   uint32_t dev = M2M_RAMROM_DEV;
   const word_t *src = (const word_t *)library();
   M2M_RAMROM_DEV = DRIVE_ROM;
   for (uint32_t fed = 0; fed < TURN; fed += CHUNK) {
      uint32_t r = M2M_DEV32[(CSR + R_REQ) / 2];
      uint32_t a = r & 0x7FFFCu;
      if (!(r >> 31) || a >= DRVROM_SIZE)
         break;
      for (uint32_t end = (a | (CHUNK - 1)) + 1; a < end; a += 4)
         M2M_DEV32[(CSR + R_DATA) / 2] = src[a / 4];
      core_idle();
      M2M_RAMROM_DEV = DRIVE_ROM;
   }
   M2M_RAMROM_DEV = dev;
}
