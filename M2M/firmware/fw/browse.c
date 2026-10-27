// The firmware base's screen: the SD card's folders, to try the card, FAT, long names,
// the menu text and the keys on hardware. Step 5 replaces it with the Shell's own file
// browser (M2M/rom/dirbrowse.asm: sorted, folders first, filters).
// Nothing is kept in memory: each redraw reads the folder again, up to the last row shown.
#include <string.h>
#include "browse.h"
#include "console.h"
#include "fmt.h"
#include "keys.h"
#include "m2m_rv.h"
#include "sdcard.h"
#include "version.h"

#define WIDTH     43           // inside the frame
#define LIST_Y    4
#define LIST_ROWS 24
#define DIR_L     17           // around a folder's name (M2M$DIR_L, M2M$DIR_R)
#define DIR_R     16

static char     path[256];     // "" is the root, else "/a/b"
static int      count;         // entries, ".." included
static int      top, sel;
static int      rows_drawn;
static int      started;
static uint32_t seen_changes;
static FILINFO  fi;

static int is_root(void)
{
   return path[0] == 0;
}

static int listed(const FILINFO *f)
{
   return !(f->fattrib & (AM_HID | AM_SYS)) && f->fname[0] != '.';
}

// Calls fn for entries first .. first + n - 1 (all from first if n < 0) and leaves the
// last one in fi. ".." is entry 0 below the root. Returns the number of entries read
// through, or -1 if the folder can't be read.
static int walk(int first, int n, void (*fn)(int i))
{
   DIR dir;
   int i = 0;
   if (!is_root()) {
      if (first == 0 && n) {
         strcpy(fi.fname, "..");
         fi.fattrib = AM_DIR;
         fi.fsize = 0;
         if (fn)
            fn(0);
      }
      i = 1;
   }
   if (f_opendir(&dir, is_root() ? "/" : path) != FR_OK)
      return -1;
   while (n < 0 || i < first + n) {
      if (f_readdir(&dir, &fi) != FR_OK || !fi.fname[0])
         break;
      if (!listed(&fi))
         continue;
      if (fn && i >= first)
         fn(i);
      i++;
   }
   f_closedir(&dir);
   return i;
}

static void message(const char *s)
{
   osm_printn(1, 31, M2M_SA_COL_STD, s, WIDTH);
}

static void draw_row(int i)
{
   char buf[WIDTH + 1];
   if (fi.fattrib & AM_DIR)
      fmt_snprintf(buf, sizeof buf, " %c%s%c", DIR_L, fi.fname, DIR_R);
   else
      fmt_snprintf(buf, sizeof buf, " %s", fi.fname);
   uint8_t attr = i == sel ? M2M_SA_COL_STD | M2M_SA_INVERSE : M2M_SA_COL_STD;
   osm_printn(1, LIST_Y + i - top, attr, buf, WIDTH);
   rows_drawn = i - top + 1;
}

static void draw_list(void)
{
   rows_drawn = 0;
   if (sdcard_state() == SDCARD_OK)
      walk(top, LIST_ROWS, draw_row);
   for (int r = rows_drawn; r < LIST_ROWS; r++)
      osm_printn(1, LIST_Y + r, M2M_SA_COL_STD, "", WIDTH);
}

static void draw_path(void)
{
   int len = strlen(path);
   const char *s = is_root() ? "/" : path;
   char buf[WIDTH + 1];
   if (len > WIDTH) {
      fmt_snprintf(buf, sizeof buf, "...%s", path + len - (WIDTH - 3));
      s = buf;
   }
   osm_printn(1, 2, M2M_SA_COL_STD, s, WIDTH);
}

static void draw_card(void)
{
   char buf[WIDTH + 1];
   switch (sdcard_state()) {
   case SDCARD_NONE:
      fmt_snprintf(buf, sizeof buf, " No SD card");
      break;
   case SDCARD_WAIT:
      fmt_snprintf(buf, sizeof buf, " SD card: %s ...", sdcard_slot());
      break;
   case SDCARD_OK:
      fmt_snprintf(buf, sizeof buf, " SD card: %s", sdcard_slot());
      break;
   default:
      fmt_snprintf(buf, sizeof buf, " SD card: %s, error %d", sdcard_slot(), sdcard_result());
      break;
   }
   osm_printn(1, 29, M2M_SA_COL_STD, buf, WIDTH);
}

// Read the folder afresh and show it
static void refresh(void)
{
   count = 0;
   if (sdcard_state() == SDCARD_OK && (count = walk(0, -1, 0)) < 0) {
      count = 0;
      message(" Can't read this folder");
   }
   if (sel >= count)
      sel = count ? count - 1 : 0;
   if (top > sel)
      top = sel;
   draw_path();
   draw_card();
   draw_list();
}

static void enter(int i)
{
   char buf[WIDTH + 1];
   walk(i, 1, 0);
   if (!strcmp(fi.fname, "..")) {
      *strrchr(path, '/') = 0;
   } else if (fi.fattrib & AM_DIR) {
      if (strlen(path) + 1 + strlen(fi.fname) >= sizeof path) {
         message(" The path is too long");
         return;
      }
      strcat(path, "/");
      strcat(path, fi.fname);
   } else {
      fmt_snprintf(buf, sizeof buf, " %u bytes", (unsigned)fi.fsize);
      message(buf);
      return;
   }
   top = sel = 0;
   message("");
   refresh();
}

static void start(void)
{
   osm_frame(0, 0, OSM_DX, OSM_DY, M2M_SA_COL_STD);
   osm_print(2, 1, M2M_SA_COL_TTLE, "MiSTer2MEGA65 RISC-V firmware");
   osm_hline(0, 3, OSM_DX, M2M_SA_COL_STD);
   osm_hline(0, 28, OSM_DX, M2M_SA_COL_STD);
   osm_printn(1, 30, M2M_SA_COL_STD, " Return opens, Run/Stop goes back", WIDTH);
   message(" Firmware " FW_VERSION);
   seen_changes = sdcard_changes();
   refresh();
}

void browse_poll(void)
{
   if (!started) {
      started = 1;
      start();
   }

   if (sdcard_changes() != seen_changes) {
      seen_changes = sdcard_changes();
      if (sdcard_state() != SDCARD_OK)
         path[0] = 0, top = sel = 0;
      refresh();
   }

   uint32_t k = keys_get();
   if (sdcard_state() != SDCARD_OK) {
      if (k == M2M_KEY_RETURN && sdcard_state() != SDCARD_WAIT)
         sdcard_retry();
      return;
   }
   switch (k) {
   case M2M_KEY_UP:
      sel = sel > 0 ? sel - 1 : 0;
      break;
   case M2M_KEY_DOWN:
      sel = sel < count - 1 ? sel + 1 : sel;
      break;
   case M2M_KEY_LEFT:
      sel = sel > LIST_ROWS ? sel - LIST_ROWS : 0;
      break;
   case M2M_KEY_RIGHT:
      sel = sel + LIST_ROWS < count ? sel + LIST_ROWS : (count ? count - 1 : 0);
      break;
   case M2M_KEY_RETURN:
      if (count)
         enter(sel);
      return;
   case M2M_KEY_RUNSTOP:
      if (!is_root())
         enter(0);
      return;
   default:
      return;
   }
   if (sel < top)
      top = sel;
   if (sel >= top + LIST_ROWS)
      top = sel - LIST_ROWS + 1;
   draw_list();
}
