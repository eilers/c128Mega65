// The core's configuration, through the device bus: word idx of a device's window sel is
// word (sel << 12) + idx, as QNICE's 4K window and offset.
#include "cfg.h"
#include "m2m_rv.h"

static uint16_t dev_get(uint32_t dev, uint32_t sel, uint32_t idx)
{
   M2M_RAMROM_DEV = dev;
   return M2M_DEV16[(sel << 12) + idx];
}

uint16_t cfg_get(uint32_t sel, uint32_t idx)
{
   return dev_get(M2M_DEV_CONFIG, sel, idx);
}

int cfg_str(uint32_t sel, char *buf, int size)
{
   return cfg_str_at(sel, 0, buf, size);
}

int cfg_str_at(uint32_t sel, int from, char *buf, int size)
{
   int n = 0;
   M2M_RAMROM_DEV = M2M_DEV_CONFIG;
   for (; n < size - 1 && from + n < 0x1000; n++) {
      uint16_t c = M2M_DEV16[(sel << 12) + from + n];
      if (c == 0)
         break;
      buf[n] = (char)c;
   }
   buf[n] = 0;
   return n;
}

uint16_t sysinfo_get(uint32_t sel, uint32_t idx)
{
   return dev_get(M2M_DEV_SYS_INFO, sel, idx);
}

int setting_get(int idx)
{
   return (M2M_CONTROL_M(idx / 32) >> (idx % 32)) & 1;
}

void setting_set(int idx, int on)
{
   uint32_t bit = 1u << (idx % 32);
   if (on)
      M2M_CONTROL_M(idx / 32) |= bit;
   else
      M2M_CONTROL_M(idx / 32) &= ~bit;
}
