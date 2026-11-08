// The C128's bus DMA (CORE/vhdl/c128_dma.vhd): C128 memory as the CPU sees it, a job of
// up to DMA_MAX bytes at a time, and the CPU held between jobs (REST's pause). A job takes
// about 1 us a byte; nothing here waits for one.
#pragma once
#include <stdint.h>

#define DMA_MAX 2048

void dma_hold(int on);        // the CPU stays stopped between jobs
int  dma_held(void);          // hold is on (a C128 reset clears it)
int  dma_z80(void);           // the Z80 runs: no DMA then
void dma_put(int i, uint8_t b);   // the buffer, for a write job
uint8_t dma_get(int i);           // ditto, after a read job
void dma_start(uint16_t addr, int len, int write);
int  dma_busy(void);
void dma_stop(void);
