// The VDC's RAM and registers, device C_DEV_VDC (vdc_peek.vhd).
#include "m2m_rv.h"
#include "vdc.h"

#define VDC_DEVICE 0x010E
#define CSR_WINDOW 0xFFFFu           // the registers, as roms.c's CSR window

static volatile uint16_t *dev(void)
{
   M2M_RAMROM_DEV = VDC_DEVICE;
   return M2M_DEV16;
}

uint8_t vdc_mem(uint16_t addr)
{
   return dev()[addr];
}

uint8_t vdc_reg(int r)
{
   return dev()[(CSR_WINDOW << 12) + r];
}
