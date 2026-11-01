// The idle main loop's code and constants go into the data RAM: run from HyperRAM, its
// i-cache misses take HyperRAM turns from the HDMI scaler. neorv32.ld puts these sections
// into .data, which crt0 copies there. The host tests build without them.
#pragma once

#ifdef __riscv
#define DMEM_CODE   __attribute__((section(".dmem.text")))
#define DMEM_CONST  __attribute__((section(".dmem.rodata")))
#else
#define DMEM_CODE
#define DMEM_CONST
#endif
