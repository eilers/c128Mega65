// The Shell's screen (QNICE's screen.asm): text on the On-Screen-Menu, and the two windows
// that show it, the whole screen and the options menu at the top right.
#pragma once
#include <stdint.h>

#define SCR_DIR_L      17      // the menu font's arrows, shown for < and >
#define SCR_DIR_R      16
#define SCR_SEL_MULTI  7       // selection marks of the options menu
#define SCR_SEL_SINGLE 61

#define SCR_COL_STD      0x0Bu // M2M$SA_COL_*
#define SCR_COL_STD_INV  0x8Bu
#define SCR_COL_TTLE     0x0Eu
#define SCR_COL_TTLE_INV 0x8Eu

void scr_clear(void);                                // spaces in the standard colours
// A new line ("\n" or a backslash n) goes on at left. Returns where the text ended: y << 8 | x.
int  scr_print(int x, int y, int left, const char *s);
void scr_frame(int x, int y, int dx, int dy);        // a double-line box
void scr_bottom(const char *s);                      // the whole screen's frame's bottom line, s on it
void scr_line(int x, int y, int dx);                 // a single line across a box
void scr_attr(int x, int y, int dx, uint8_t attr);
void scr_full_on(void);                              // SCR$OSM_M_ON
void scr_window_on(int x, int y, int dx, int dy);    // SCR$OSM_O_ON
void scr_off(void);
