// The network device (CORE/vhdl/net/net.vhd) as the firmware sees it: 16-bit words of
// device C_DEV_NET on the device bus. core_net and net_lock select it; the others take it
// as it is.
#pragma once
#include <stdint.h>
#include "m2m_rv.h"

#define NET_DEVICE     0x010B          // C_DEV_NET (globals.vhd)
#define NET_REG(w)     M2M_DEV16[w]

// The 6551's side, behind two 64-byte FIFOs
#define ACIA_RX        NET_REG(0x00)   // W: a byte for the C128. R: free entries in its FIFO
#define ACIA_TX        NET_REG(0x01)   // R: the C128's next byte, bit 8 = valid (the read takes it)
#define ACIA_CMD       NET_REG(0x02)   // R: control (15..8), command (7..0)
#define ACIA_STATE     NET_REG(0x03)   // R: switched on in the menu (8), Turbo232 rate (1..0)
#define ACIA_LINES     NET_REG(0x04)   // RW: lines the modem drives, ACIA_LN_*
#define ACIA_EVENTS    NET_REG(0x05)   // R: ACIA_EV_* since cleared, W: 1s clear them
#define ACIA_AGAINS    NET_REG(0x06)   // R: receive IRQs raised again for an unread byte, since power-up

#define ACIA_ST_ENABLED   (1u << 8)

#define ACIA_LN_DCD       (1u << 0)
#define ACIA_LN_DSR       (1u << 1)
#define ACIA_LN_CTS       (1u << 2)

#define ACIA_EV_CTRL      (1u << 0)    // control register written
#define ACIA_EV_CMD       (1u << 1)    // command register written (DTR, RTS)
#define ACIA_EV_RESET     (1u << 2)    // C128 reset or 6551 soft reset

// Ethernet MAC: whole frames buffered both ways
#define ETH_RX         NET_REG(0x08)   // R: byte (7..0), last of the frame (8), valid (15); the read takes it
#define ETH_TX         NET_REG(0x09)   // W: byte (7..0), last of the frame (8) sends it
#define ETH_STATUS     NET_REG(0x0A)   // R: a whole frame fits (0)
#define ETH_DROPS      NET_REG(0x0B)   // R: frames received with errors or without room
#define ETH_CTRL       NET_REG(0x0C)   // RW: run (0), the MAC and PHY leave reset

#define ETH_RX_VALID      0x8000u
#define ETH_LAST          0x0100u
#define ETH_TX_ROOM       0x0001u
#define ETH_RUN           0x0001u

// The FPGA's device DNA: 57 bits, 16 a word, word 3 bit 15 = valid
#define NET_DNA(i)     NET_REG(0x10 + (i))
#define NET_DNA_VALID     0x8000u
