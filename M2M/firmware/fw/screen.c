// The Shell's screen, as QNICE's screen.asm draws it.
#include "screen.h"
#include "console.h"
#include "m2m_rv.h"

void scr_clear(void)
{
   osm_clear(SCR_COL_STD);
}

int scr_print(int x, int y, int left, const char *s)
{
   M2M_RAMROM_DEV = M2M_DEV_VRAM_DATA;
   for (; *s; s++) {
      char c = *s;
      if (c == '\n' || (c == '\\' && s[1] == 'n')) {
         s += c == '\\';
         x = left;
         y++;
         continue;
      }
      if (x < OSM_DX && y < OSM_DY)
         M2M_DEV16[y * OSM_DX + x] = c == '<' ? SCR_DIR_L : c == '>' ? SCR_DIR_R : (uint8_t)c;
      x++;
   }
   return y << 8 | x;
}

void scr_frame(int x, int y, int dx, int dy)
{
   osm_frame(x, y, dx, dy, SCR_COL_STD);
}

// s from its third column (NULL: none); 200, 205 and 188 are osm_frame's corners and line
void scr_bottom(const char *s)
{
   char row[OSM_DX + 1] = { [0] = (char)200, [1 ... OSM_DX - 2] = (char)205, [OSM_DX - 1] = (char)188 };
   for (int i = 0; s && s[i] && i < OSM_DX - 4; i++)
      row[2 + i] = s[i];
   osm_print(0, OSM_DY - 1, SCR_COL_STD, row);
}

void scr_line(int x, int y, int dx)
{
   osm_hline(x, y, dx, SCR_COL_STD);
}

void scr_attr(int x, int y, int dx, uint8_t attr)
{
   M2M_RAMROM_DEV = M2M_DEV_VRAM_ATTR;
   for (int i = 0; i < dx; i++)
      M2M_DEV16[y * OSM_DX + x + i] = attr;
}

void scr_window_on(int x, int y, int dx, int dy)
{
   M2M_OSM_XY   = (x << 8) | y;
   M2M_OSM_DXDY = (dx << 8) | dy;
   M2M_CSR     |= M2M_CSR_OSM;
}

void scr_full_on(void)
{
   scr_window_on(0, 0, OSM_DX, OSM_DY);
}

void scr_off(void)
{
   M2M_CSR &= ~M2M_CSR_OSM;
}
