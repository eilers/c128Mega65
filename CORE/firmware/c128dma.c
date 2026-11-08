// The C128's bus DMA, device C_DEV_DMA (c128_dma.vhd).
#include "c128dma.h"
#include "m2m_rv.h"

#define DMA_DEVICE 0x010C
#define W_HOLD     0x000
#define W_ADDR     0x001
#define W_LEN      0x002
#define W_GO       0x003           // W: bit 0 write, bit 1 stop; R: bit 0 busy, bit 2 Z80
#define W_BUF      0x800

#define GO_WRITE   1
#define GO_STOP    2
#define ST_BUSY    1
#define ST_Z80     4

static volatile uint16_t *dev(void)
{
   M2M_RAMROM_DEV = DMA_DEVICE;
   return M2M_DEV16;
}

void dma_hold(int on)
{
   dev()[W_HOLD] = on != 0;
}

int dma_held(void)
{
   return dev()[W_HOLD] & 1;
}

int dma_z80(void)
{
   return (dev()[W_GO] & ST_Z80) != 0;
}

void dma_put(int i, uint8_t b)
{
   dev()[W_BUF + i] = b;
}

uint8_t dma_get(int i)
{
   return dev()[W_BUF + i];
}

void dma_start(uint16_t addr, int len, int write)
{
   volatile uint16_t *d = dev();
   d[W_ADDR] = addr;
   d[W_LEN]  = len;
   d[W_GO]   = write ? GO_WRITE : 0;
}

int dma_busy(void)
{
   return dev()[W_GO] & ST_BUSY;
}

void dma_stop(void)
{
   dev()[W_GO] = GO_STOP;
}
