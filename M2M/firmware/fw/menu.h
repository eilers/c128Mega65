// The options menu (M2M/rom/menu.asm): groups of items of which one is selected,
// single-select items that toggle, submenus, headlines and separator lines, as config.vhd's
// OPTM_* data describes them. Item indices count every line of the whole menu.
#pragma once
#include <stdint.h>

#define MENU_MAX       254

// Group words as config.vhd's SEL_OPTM_GROUPS gives them
#define MENU_ID        0x00FFu   // the group; 0 for text and lines
#define MENU_CLOSE     0x00FFu   // an id: closes the menu, or the submenu with MENU_SUBMENU
#define MENU_HEADLINE  0x1000u
#define MENU_SUBMENU   0x4000u   // starts or ends a submenu
#define MENU_SINGLESEL 0x8000u

enum menu_key {
   MENU_KEY_NONE, MENU_KEY_UP, MENU_KEY_DOWN, MENU_KEY_SELECT, MENU_KEY_CLOSE,
   MENU_KEY_SELALT, MENU_KEY_MENUUP
};

struct menu {
   int      n;                    // items
   int      x, y, dx, dy;         // the window, frame included
   const char *line[MENU_MAX];    // each item's text, without its "\n"
   uint16_t group[MENU_MAX];
   uint8_t  sel[MENU_MAX];        // selected
   uint8_t  hline[MENU_MAX];      // a separator line
   uint8_t  sub[MENU_MAX];        // the submenu it is in, from 1; 0 = the main menu
   int      level;                // the submenu shown, 0 = the main menu
   int      main_cursor;          // the main menu's cursor while a submenu is shown
   int      cursor;
   int      shown;                // on the screen: menu_set draws its changes

   // A selection: the group word, the item's position in its group (or 1/0 for a
   // single-select item) and the key
   void (*select)(uint16_t group, int item, enum menu_key key);
   // The text of a line with a "%s": write it to out (dx - 2 characters at most)
   void (*show)(int idx, const char *line, char *out);
};

extern struct menu menu;

int  menu_init(void);                 // after line, group, hline and n are filled; 0 = OK
void menu_show(void);                 // OPTM_SHOW
void menu_cursor_on(void);            // highlights the cursor's line
int  menu_key(enum menu_key key);     // OPTM_RUN for one key; 1 = the menu closes
int  menu_set(int idx, int on);       // OPTM_SET; returns the group item it deselected, or -1
int  menu_visible(int idx);
