// The HDMI filter submenu. mega65.vhd sets ascal's mode from the menu: its own scalers for
// the first three choices, its polyphase filter for the rest. Those need their coefficients,
// loaded here when the choice changes, however it changed. The choices and their tables are
// C64MEGA65's (its LOAD_HDMI_FILTER in m2m-rom.asm, sy2002, GPL v3).
#include <stdint.h>
#include "config_ids.h"
#include "core.h"
#include "dmem.h"
#include "filters.h"
#include "hdmi_filter.h"
#include "m2m_rv.h"

// The menu's order: the horizontal and the vertical table, none for ascal's own scalers
static const uint16_t *const tables[HF_COUNT][2] = {
   { 0, 0 },                                  // No Filter: nearest neighbour
   { 0, 0 },                                  // Sharp Bilinear
   { 0, 0 },                                  // Bicubic
   { GS_SHARPNESS_050, GS_SHARPNESS_050 },    // Smooth
   { LANCZOS2_12, LANCZOS2_12 },              // Lanczos
   { LANCZOS2_12, SCAN_BR_110_80 },           // Scanlines: the old CRT emulation
   { CRT_SIM_SVIDEO_H, SCAN_BR_110_80 },      // CRT (S-Video)
   { CRT_SIM_COMPOSITE_H, SCAN_BR_110_80 },   // CRT (Composite)
};

static int16_t item[HF_COUNT];   // their menu items
static int     known;            // item[] is filled in
static int     loaded = -1;      // the choice ascal has the coefficients of

static void find_items(void)
{
   for (int k = 0; k < HF_COUNT; k++)
      item[k] = shell_item(OPTM_G_HDMI_FILTER, k);
   known = 1;
}

static void load(int k)
{
   if (tables[k][0])
      shell_polyphase(tables[k][0], tables[k][1]);
   loaded = k;
}

// In the data RAM, as the idle loop is: reads the menu's bits, no HyperRAM
DMEM_CODE void hdmi_filter_poll(void)
{
   if (!known)
      find_items();
   for (int k = 0; k < HF_COUNT; k++) {
      unsigned i = (unsigned)item[k];
      if (item[k] >= 0 && (M2M_CONTROL_M(i / 32) >> (i % 32)) & 1) {
         if (k != loaded)
            load(k);
         return;
      }
   }
}
