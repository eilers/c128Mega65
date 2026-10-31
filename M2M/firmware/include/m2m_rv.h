// The RISC-V system CPU's hardware, as M2M/vhdl/rv/rv_soc.vhd decodes it.
// Names follow QNICE's sysdef.asm, which described the same hardware.
#pragma once
#include <stdint.h>

#define CPU_HZ               100000000u

// Version of this hardware interface. Bump it when a change here would break a firmware
// built for the old one: the boot ROM only starts a firmware built for its own version.
// 2: 64 KB data RAM (was 16 KB)
// 3: every key (M2M_MATRIX), the core's bus DMA device
// 4: drive ROM reload (C_DEV_DRIVE_ROM's CSR), vd_sd's hold
// 5: the internal function ROM (C_DEV_SYSTEM_ROM from 0x18000, its CSR), 15 CRT/ROM items
// 6: drives A and B: a bus ID and a model each, the drive ROM items in their submenus
// 7: 128 KB data RAM (was 64 KB)
#define M2M_RV_HW_VERSION    7u

// The host tests (host/) bring their own REG32, device bus and HyperRAM
#ifndef REG32
#define REG32(a)             (*(volatile uint32_t *)(a))
#endif

// Registers (QNICE's 0xFFE0 and up)
#define M2M_CSR              REG32(0xE0000000u)
#define M2M_OSM_XY           REG32(0xE0000004u)   // x in the high byte, y in the low byte, in characters
#define M2M_OSM_DXDY         REG32(0xE0000008u)
#define M2M_ASCAL_MODE       REG32(0xE000000Cu)
#define M2M_SPECIAL          REG32(0xE0000010u)
#define M2M_GENERAL          REG32(0xE0000014u)
#define M2M_KEYS             REG32(0xE0000018u)   // low active, see M2M_KEY_*
#define M2M_RAMROM_DEV       REG32(0xE000001Cu)   // device the device bus window shows
#define M2M_MATRIX(i)        REG32(0xE0000020u + 4 * (i))   // every key, low active: bit n % 32 of
                                                          // word n / 32 is key_num n, i = 0..2
#define M2M_CONTROL_D(i)     REG32(0xE0000040u + 4 * (i))   // control_d bits 32i+31..32i, i = 0..7
#define M2M_CONTROL_M(i)     REG32(0xE0000060u + 4 * (i))   // control_m, ditto

#define M2M_CSR_RESET        0x0001u   // the core is held in reset
#define M2M_CSR_PAUSE        0x0002u
#define M2M_CSR_OSM          0x0004u   // the On-Screen-Menu is shown
#define M2M_CSR_KBD          0x0008u   // the keyboard reaches the core
#define M2M_CSR_JOY1         0x0010u
#define M2M_CSR_JOY2         0x0020u
#define M2M_CSR_SD_MODE      0x0040u   // 1 = the SD card in M2M_CSR_SD_FORCE, 0 = automatic
#define M2M_CSR_SD_FORCE     0x0080u   // 1 = back slot, 0 = bottom tray
#define M2M_CSR_SD_ACTIVE    0x0100u   // read only: 1 = back slot
#define M2M_CSR_SD_DET_INT   0x0200u   // read only: card in the bottom tray
#define M2M_CSR_SD_DET_EXT   0x0400u   // read only: card in the back slot
#define M2M_CSR_ASCAL_AUTO   0x0800u

#define M2M_KEY_UP           0x0001u
#define M2M_KEY_DOWN         0x0002u
#define M2M_KEY_LEFT         0x0004u
#define M2M_KEY_RIGHT        0x0008u
#define M2M_KEY_RETURN       0x0010u
#define M2M_KEY_SPACE        0x0020u
#define M2M_KEY_RUNSTOP      0x0040u
#define M2M_KEY_HELP         0x0080u
#define M2M_KEY_F1           0x0100u
#define M2M_KEY_F3           0x0200u
#define M2M_KEY_F13          0x0400u

// Device bus: 16-bit word n of the device in M2M_RAMROM_DEV. 16- and 32-bit accesses
// only; a 32-bit access is words n and n+1.
#ifndef M2M_DEV16
#define M2M_DEV16            ((volatile uint16_t *)0xC0000000u)
#define M2M_DEV32            ((volatile uint32_t *)0xC0000000u)
#endif

#define M2M_DEV_VRAM_DATA    0x0000u   // On-Screen-Menu characters, row by row
#define M2M_DEV_VRAM_ATTR    0x0001u   // their attributes, see M2M_SA_*
#define M2M_DEV_CONFIG       0x0002u   // config.vhd
#define M2M_DEV_ASCAL_PPHASE 0x0003u
#define M2M_DEV_I2C          0x0005u
#define M2M_DEV_RTC          0x0006u
#define M2M_DEV_SYS_INFO     0x00FFu

// On-Screen-Menu attributes
#define M2M_SA_INVERSE       0x80u
#define M2M_SA_DARK          0x40u
#define M2M_SA_BG_RED        0x20u
#define M2M_SA_BG_GREEN      0x10u
#define M2M_SA_BG_BLUE       0x08u
#define M2M_SA_FG_RED        0x04u
#define M2M_SA_FG_GREEN      0x02u
#define M2M_SA_FG_BLUE       0x01u
#define M2M_SA_COL_STD       0x0Bu     // cyan on blue
#define M2M_SA_COL_TTLE      0x0Eu     // yellow on blue
#define M2M_SA_COL_SEL       0x0Fu     // white on blue

// SD card controller (M2M/vhdl/sd/sdhost.vhd), one register per 32-bit word, low 16 bits
#define M2M_SD(r)            REG32(0xE0000100u + 4 * (r))
#define M2M_SD_ADDR_LO       M2M_SD(0)
#define M2M_SD_ADDR_HI       M2M_SD(1)
#define M2M_SD_POS           M2M_SD(2)   // cursor into the 512-byte buffer
#define M2M_SD_DATA          M2M_SD(3)   // the byte at the cursor
#define M2M_SD_ERROR         M2M_SD(4)
#define M2M_SD_CSR           M2M_SD(5)   // W: 0 reset, 1 read, 2 write. R: bit 15 busy, 14 error
#define M2M_SD_STATUS        M2M_SD(6)

// HyperRAM: byte address A is word A/2, low byte first
#ifndef M2M_HYPERRAM
#define M2M_HYPERRAM         0x40000000u
#endif

// SDRAM (R4 and later boards; rv_soc's G_SDRAM): the same way, 64 MB
#ifndef M2M_SDRAM
#define M2M_SDRAM            0x50000000u
#endif

#ifndef M2M_HALT
#define M2M_HALT()           do { asm volatile ("csrci mstatus, 8"); } while (1)   // stops the firmware for good, the watchdog too
#endif
#define M2M_FW_BASE          0x40400000u   // the firmware's region, C_HMAP_FIRMWARE 0x200-0x3FE
#define M2M_FW_END           0x407FE000u   // (doc/hyperram-map.md)

// NEORV32
#define UART0_CTRL           REG32(0xFFF50000u)
#define UART0_DATA           REG32(0xFFF50004u)
#define MTIME_LO             REG32(0xFFF4BFF8u)
#define MTIME_HI             REG32(0xFFF4BFFCu)
#define MTIMECMP_LO          REG32(0xFFF44000u)
#define MTIMECMP_HI          REG32(0xFFF44004u)
