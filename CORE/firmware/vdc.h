// The VDC's RAM and registers without a program's side effects (CORE/vhdl/vdc_peek.vhd):
// a read waits about 250 ns while it crosses to the VDC's clock, and the C128 keeps running.
#pragma once
#include <stdint.h>

#define VDC_REGS 40   // R0-R37 as a program reads them, then the status byte and the selected register

uint8_t vdc_mem(uint16_t addr);   // at the address a program sets in R18/R19
uint8_t vdc_reg(int r);           // 0 to VDC_REGS - 1
