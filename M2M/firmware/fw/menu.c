// The options menu, as QNICE's menu.asm runs it, one key at a time.
#include <string.h>
#include "menu.h"
#include "screen.h"

struct menu menu;

static int first_of_submenu(int i)
{
   return menu.sub[i] && (i == 0 || menu.sub[i - 1] != menu.sub[i]);
}

// The main menu shows each submenu's first line, which opens it; a submenu hides it
int menu_visible(int i)
{
   if (menu.level == 0)
      return menu.sub[i] == 0 || first_of_submenu(i);
   return menu.sub[i] == menu.level && !first_of_submenu(i);
}

static int selectable(int i)
{
   return menu_visible(i) && (menu.group[i] & (MENU_SUBMENU | MENU_ID));
}

static int screen_y(int idx)
{
   int y = menu.y + 1;
   for (int i = 0; i < idx; i++)
      y += menu_visible(i);
   return y;
}

static void mark(int idx)
{
   static const char multi[] = { SCR_SEL_MULTI, 0 }, single[] = { SCR_SEL_SINGLE, 0 };
   const char *s = !menu.sel[idx] ? " " : menu.group[idx] & MENU_SINGLESEL ? single : multi;
   scr_print(menu.x + 1, screen_y(idx), menu.x + 1, s);
}

static void highlight(int idx, uint8_t attr)
{
   scr_attr(menu.x + 1, screen_y(idx), menu.dx - 2, attr);
}

int menu_init(void)
{
   int id = 0, next = 1;
   for (int i = 0; i < menu.n; i++) {
      if ((menu.group[i] & MENU_SUBMENU) && id) {
         menu.sub[i] = id;             // the submenu's last line
         id = 0;
         continue;
      }
      if (menu.group[i] & MENU_SUBMENU)
         id = next++;
      menu.sub[i] = id;
   }
   menu.level = 0;
   return id ? -1 : 0;
}

void menu_show(void)
{
   const int x = menu.x + 1;
   char text[48];
   int y = menu.y + 1;

   scr_clear();
   scr_frame(menu.x, menu.y, menu.dx, menu.dy);
   for (int i = 0; i < menu.n; i++) {
      if (!menu_visible(i))
         continue;
      const char *s = menu.line[i];
      if (menu.show && strstr(s, "%s")) {
         menu.show(i, s, text);
         s = text;
      }
      scr_print(x, y, x, s);
      if (menu.group[i] & MENU_HEADLINE)
         highlight(i, SCR_COL_TTLE);
      if (menu.sel[i])
         mark(i);
      else if (menu.hline[i])
         scr_line(menu.x, y, menu.dx);
      y++;
   }
   // A submenu shorter than the window gets a line under it
   if (menu.level && y <= menu.y + menu.dy - 3)
      scr_line(menu.x, y, menu.dx);
   menu.shown = 1;
}

void menu_cursor_on(void)
{
   highlight(menu.cursor, SCR_COL_STD_INV);
}

static void change_level(int level, int cursor)
{
   menu.level  = level;
   menu.cursor = cursor;
   menu_show();
   menu_cursor_on();
}

int menu_set(int idx, int on)
{
   uint16_t g = menu.group[idx];
   int old = -1;

   menu.sel[idx] = on;
   if (!(g & MENU_SINGLESEL)) {
      for (int i = 0; i < menu.n && old < 0; i++)
         if (i != idx && (menu.group[i] & MENU_ID) == (g & MENU_ID) && menu.sel[i])
            old = i;
      if (old >= 0)
         menu.sel[old] = 0;
   }
   if (menu.shown && menu_visible(idx)) {
      mark(idx);
      if (old >= 0 && menu_visible(old))
         mark(old);
   }
   return old;
}

static void select_item(int c, enum menu_key key)
{
   uint16_t g = menu.group[c];
   int pos = 1;

   if (menu.sel[c]) {
      // Selecting a selected item only matters to a single-select item: it toggles
      if (g & MENU_SINGLESEL) {
         menu.sel[c] = 0;
         mark(c);
         menu.select(g, 0, key);
      }
      return;
   }
   if (!(g & MENU_SINGLESEL)) {
      pos = 0;
      for (int i = 0; i < menu.n; i++) {
         if (menu.group[i] != g)
            continue;
         pos += i < c;
         if (menu.sel[i]) {
            menu.sel[i] = 0;
            if (menu_visible(i))
               mark(i);
         }
      }
   }
   menu.sel[c] = 1;
   mark(c);
   menu.select(g, pos, key);
}

int menu_key(enum menu_key key)
{
   int c = menu.cursor;
   uint16_t g = menu.group[c];

   switch (key) {
   case MENU_KEY_UP:
   case MENU_KEY_DOWN:
      do
         c = (c + (key == MENU_KEY_UP ? menu.n - 1 : 1)) % menu.n;
      while (!selectable(c));
      highlight(menu.cursor, SCR_COL_STD);
      menu.cursor = c;
      break;
   case MENU_KEY_CLOSE:
      return 1;
   case MENU_KEY_MENUUP:
      if (menu.level == 0)
         return 1;
      change_level(0, menu.main_cursor);
      break;
   case MENU_KEY_SELECT:
   case MENU_KEY_SELALT:
      if (g & MENU_SUBMENU) {
         if ((g & MENU_ID) == MENU_CLOSE) {
            change_level(0, menu.main_cursor);
            break;
         }
         // Into the submenu, on its first item
         menu.main_cursor = c;
         do
            c++;
         while (c < menu.n && !(menu.group[c] & MENU_ID));
         change_level(menu.sub[menu.cursor], c < menu.n ? c : menu.cursor);
         break;
      }
      select_item(c, key);
      if (g == MENU_CLOSE)
         return 1;
      break;
   default:
      return 0;
   }
   if (menu.shown)          // not if a selection put up another screen
      menu_cursor_on();
   return 0;
}
