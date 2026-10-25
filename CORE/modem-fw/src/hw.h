// Modem hardware as the RISC-V sees it. Must match regs_proc in CORE/vhdl/modem/modem.vhd.
#pragma once
#include <stdint.h>

#define CPU_HZ        50000000u

#define REG32(a)      (*(volatile uint32_t *)(a))

// The firmware image sits in HyperRAM behind the i-cache; data RAM is NEORV32's DMEM.
#define IMAGE_BASE    0x40000000u
#define IMAGE_MAGIC   0x314D444Du     // "MDM1"

// ACIA bridge: the C128's 6551 on the other side of two small FIFOs
#define ACIA_BASE     0xF0000000u
#define ACIA_RX       REG32(ACIA_BASE + 0x00) // W: byte for the C128. R: free RX FIFO entries
#define ACIA_TX       REG32(ACIA_BASE + 0x04) // R: next byte from the C128, bit 8 = valid (pops)
#define ACIA_STATE    REG32(ACIA_BASE + 0x08) // R: see ACIA_ST_*
#define ACIA_LINES    REG32(ACIA_BASE + 0x0C) // RW: lines the modem drives, see ACIA_LN_*
#define ACIA_EVENTS   REG32(ACIA_BASE + 0x10) // R: pending events, W: 1 clears, see ACIA_EV_*
#define ACIA_AGAINS   REG32(ACIA_BASE + 0x14) // R: receive IRQs raised again for an unread byte, since power-up

#define ACIA_ST_CMD(s)    ((s) & 0xFF)          // 6551 command register
#define ACIA_ST_CTRL(s)   (((s) >> 8) & 0xFF)   // 6551 control register
#define ACIA_ST_TURBO(s)  (((s) >> 16) & 0x03)  // Turbo232 speed register
#define ACIA_ST_ENABLED   (1u << 24)            // switched on in the menu

#define ACIA_LN_DCD       (1u << 0)
#define ACIA_LN_DSR       (1u << 1)
#define ACIA_LN_CTS       (1u << 2)

#define ACIA_EV_CTRL      (1u << 0)             // control register written
#define ACIA_EV_CMD       (1u << 1)             // command register written (DTR, RTS)
#define ACIA_EV_RESET     (1u << 2)             // C128 reset or 6551 soft reset

// Ethernet MAC
#define ETH_BASE      0xF0001000u
#define ETH_RX        REG32(ETH_BASE + 0x00) // R: data [7:0], last [8], valid [31] (pops)
#define ETH_TX        REG32(ETH_BASE + 0x04) // W: data [7:0], last [8] sends the frame
#define ETH_STATUS    REG32(ETH_BASE + 0x08) // R: TX buffer has room [0], frames dropped [31:16]

// System
#define SYS_BASE      0xF0002000u
#define SYS_DNA_LO    REG32(SYS_BASE + 0x00) // FPGA device DNA, bits 31:0
#define SYS_DNA_HI    REG32(SYS_BASE + 0x04) // bits 56:32, bit 31 = valid

// NEORV32 CLINT machine timer, CPU clock ticks
#define MTIME_LO      REG32(0xFFF4BFF8u)
#define MTIME_HI      REG32(0xFFF4BFFCu)
