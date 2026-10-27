// The Shell's file browser, as M2M/rom/selectfile.asm and dirbrowse.asm behave: the
// folder's entries sorted (folders first, as "<name>", case does not matter, numbers by
// their length), filtered by the core, 31 to a screen. Up and Down move, Left and Right
// page, Return opens a folder or picks a file, Run/Stop cancels, F1 and F3 choose the
// bottom tray's or the back slot's card.
//
// The entries are records in HyperRAM (fw/heap.c); the sorted order, a number per entry,
// is kept in the data RAM. QNICE kept the whole folder in its RAM.
#include <string.h>
#include "browse.h"
#include "cfg.h"
#include "console.h"
#include "core.h"
#include "ff.h"
#include "fmt.h"
#include "heap.h"
#include "m2m_rv.h"
#include "screen.h"
#include "sdcard.h"

#define ROWS        (OSM_DY - 2)
#define WIDTH       (OSM_DX - 2)
#define MAX_ENTRIES 1024
#define DEPTH       32

struct rec {
   uint16_t len;
   uint8_t  dir;
   uint8_t  num;                      // a number, as dirbrowse.asm sorts those
   char     name[FF_MAX_LFN + 3];     // folders as "<name>"
};

static const char wrn_maxfiles[] =
   "Warning: This directory contains more\nfiles than this core is able to load into\n"
   "memory.\n\nSplit the files into multiple folders.\n\nIf you continue by pressing SPACE, be\n"
   "aware that random files will be missing.\n\nPress SPACE to continue.\n";
static const char wrn_empty[] =
   "The root directory of the SD card contains\nno sub-directories that might contain any\n"
   "files that match the criteria of this core.\n\nAnd the root directory itself also does not\n"
   "contain any files that match the criteria\nof this core.\n\nNothing to browse.\n\n"
   "Press Space to continue.";

static struct rec *recs;
static uint16_t    order[MAX_ENTRIES];
static int         n, top, cur;
static char        path[256];         // "" is the root
static int         stack[DEPTH], depth;
static int         started, caller_was = -1, ctx_now, group_now;
static uint32_t    card_changes;
static enum { SHOW, WARN, NOTHING } state;
static struct rec  new_rec;
static FILINFO     fi;
static char        chosen[sizeof path + FF_MAX_LFN + 2];

// -------------------------------------------------------------------------------------
// The sorted folder
// -------------------------------------------------------------------------------------

static int key(const struct rec *r, int i)
{
   int c = (uint8_t)r->name[i];
   if (c >= 'a' && c <= 'z')
      c -= 32;
   if (r->dir)
      c = c == '<' ? 1 : c == '>' ? 2 : c;
   return c;
}

static int compare(const struct rec *a, const struct rec *b)
{
   if (a->num && b->num && a->len != b->len)
      return a->len < b->len ? -1 : 1;
   for (int i = 0;; i++) {
      int d = key(a, i) - key(b, i);
      if (d || !a->name[i])
         return d;
   }
}

static int is_number(const char *s)
{
   for (; *s; s++)
      if ((*s < '0' || *s > '9') && *s != '<' && *s != '>')
         return 0;
   return 1;
}

// new_rec into the sorted order: binary search, then the record into HyperRAM
static void insert(void)
{
   int lo = 0, hi = n;
   while (lo < hi) {
      int mid = (lo + hi) / 2;
      if (compare(&new_rec, &recs[order[mid]]) < 0)
         hi = mid;
      else
         lo = mid + 1;
   }
   memmove(&order[lo + 1], &order[lo], (n - lo) * sizeof order[0]);
   order[lo] = n;
   memcpy(&recs[n], &new_rec, offsetof(struct rec, name) + new_rec.len + 1);
   n++;
}

static void add(const char *name, int dir)
{
   char upper[sizeof new_rec.name];
   int len = strlen(name);

   new_rec.dir = dir;
   if (dir) {
      new_rec.name[0] = '<';
      memcpy(new_rec.name + 1, name, len);
      new_rec.name[len + 1] = '>';
      new_rec.name[len + 2] = 0;
      len += 2;
   } else
      memcpy(new_rec.name, name, len + 1);
   new_rec.len = len;
   new_rec.num = is_number(new_rec.name);
   for (int i = 0; i <= len; i++) {
      char c = new_rec.name[i];
      upper[i] = c >= 'a' && c <= 'z' ? c - 32 : c;
   }
   if (!core_filter_file(upper, dir, ctx_now, group_now))
      insert();
}

// 0 = read, 1 = no such folder, 2 = more entries than MAX_ENTRIES, else a FRESULT
static int read_folder(void)
{
   DIR d;
   int full = 0;
   FRESULT r;

   if (!recs)
      recs = heap_alloc(MAX_ENTRIES * sizeof *recs);
   n = 0;
   r = f_opendir(&d, path[0] ? path : "/");
   if (r == FR_NO_PATH || r == FR_NO_FILE || r == FR_INVALID_NAME)
      return 1;
   if (r != FR_OK)
      return 0x100 + r;
   if (path[0])
      add("..", 1);
   while ((r = f_readdir(&d, &fi)) == FR_OK && fi.fname[0]) {
      if (fi.fattrib & AM_HID)
         continue;
      if (n == MAX_ENTRIES) {
         full = 1;
         break;
      }
      add(fi.fname, (fi.fattrib & AM_DIR) != 0);
   }
   f_closedir(&d);
   return r != FR_OK ? 0x100 + r : full ? 2 : 0;
}

// -------------------------------------------------------------------------------------
// The screen
// -------------------------------------------------------------------------------------

static void scr_inner(void)
{
   for (int y = 1; y <= ROWS; y++) {
      static const char blank[WIDTH + 1] = { [0 ... WIDTH - 1] = ' ' };
      scr_print(1, y, 1, blank);
      scr_attr(1, y, WIDTH, SCR_COL_STD);
   }
}

static void highlight(uint8_t attr)
{
   scr_attr(1, 1 + cur - top, WIDTH, attr);
}

static void draw(void)
{
   char s[sizeof new_rec.name];
   scr_inner();
   for (int r = 0; r < ROWS && top + r < n; r++) {
      const struct rec *e = &recs[order[top + r]];
      memcpy(s, e->name, e->len + 1);
      if (e->len > WIDTH)
         strcpy(s + WIDTH - 3, "...");
      scr_print(1, 1 + r, 1, s);
   }
   highlight(SCR_COL_STD_INV);
}

// The cursor at absolute index i, on the screen it falls on (the last one full)
static void place(int i)
{
   cur = i < n ? i : n - 1;
   if (cur < 0)
      cur = 0;
   top = cur / ROWS * ROWS;
   if (top + ROWS > n)
      top = n > ROWS ? n - ROWS : 0;
}

static void message(const char *s)
{
   scr_inner();
   scr_print(1, 1, 1, s);
}

// Read the folder and show it, or what's wrong with it
static void show(int at)
{
   int r = read_folder();
   if (r == 1 && path[0]) {           // no start folder: the root
      path[0] = 0;
      depth = 0;
      r = read_folder();
   }
   if (r != 0 && r != 2) {
      char m[64];
      fmt_snprintf(m, sizeof m, "SD Card:\nUnknown error while trying to browse.\n%04X", r);
      message(m);
      state = NOTHING;
      return;
   }
   place(at);
   if (n == 0) {
      const char *m = core_custom_msg(CMSG_BROWSENOTHING, ctx_now);
      message(m ? m : wrn_empty);
      state = NOTHING;
   } else if (r == 2) {
      message(wrn_maxfiles);
      state = WARN;
   } else {
      state = SHOW;
      draw();
   }
}

// -------------------------------------------------------------------------------------
// Browsing
// -------------------------------------------------------------------------------------

void browse_reset(void)
{
   started = 0;
}

void browse_open(int ctx, int group, int caller)
{
   if (caller != caller_was)
      started = 0;
   caller_was   = caller;
   ctx_now      = ctx;
   group_now    = group;
   card_changes = sdcard_changes();
   if (!started) {
      started = 1;
      cfg_str(CFG_DIR_START, path, sizeof path);
      if (!strcmp(path, "/"))
         path[0] = 0;
      depth = 0;
      cur = 0;
   }
   show(cur);
}

static int open_folder(void)
{
   const struct rec *e = &recs[order[cur]];
   char name[sizeof new_rec.name];
   int len = e->len - 2, at = 0;

   memcpy(name, e->name + 1, len);
   name[len] = 0;
   if (!strcmp(name, "..")) {
      char *slash = strrchr(path, '/');
      if (slash)
         *slash = 0;
      at = depth ? stack[--depth] : 0;
   } else {
      if (strlen(path) + 1 + len >= sizeof path)
         return 0;
      strcat(path, "/");
      strcat(path, name);
      if (depth < DEPTH)
         stack[depth++] = cur;
   }
   show(at);
   return 1;
}

static void move_to(int c, int t)
{
   if (t != top) {
      cur = c;
      top = t;
      draw();
      return;
   }
   highlight(SCR_COL_STD);
   cur = c;
   highlight(SCR_COL_STD_INV);
}

enum browse_result browse_key(uint32_t k)
{
   int shown = n - top < ROWS ? n - top : ROWS;

   if (sdcard_changes() != card_changes)
      return BROWSE_CHANGED;

   if (state == NOTHING)
      return k == M2M_KEY_SPACE ? BROWSE_NOTHING : BROWSE_BUSY;
   if (state == WARN) {
      if (k == M2M_KEY_SPACE) {
         state = SHOW;
         draw();
      }
      return BROWSE_BUSY;
   }

   switch (k) {
   case M2M_KEY_UP:
      if (cur > 0)
         move_to(cur - 1, cur > top ? top : top - 1);
      break;
   case M2M_KEY_DOWN:
      if (cur < n - 1)
         move_to(cur + 1, cur < top + ROWS - 1 ? top : top + 1);
      break;
   case M2M_KEY_LEFT:               // a screen up, or to the top
      if (top == 0)
         move_to(0, 0);
      else {
         int by = top < ROWS ? top : ROWS;
         move_to(cur - by, top - by);
      }
      break;
   case M2M_KEY_RIGHT: {            // a screen down, or to the bottom
      int left = n - top - shown;
      if (left == 0)
         move_to(n - 1, top);
      else {
         int by = left < ROWS ? left : ROWS;
         move_to(cur + by, top + by);
      }
      break;
   }
   case M2M_KEY_RETURN: {
      const struct rec *e = &recs[order[cur]];
      if (e->dir) {
         open_folder();
         break;
      }
      int len = strlen(path);
      memcpy(chosen, path, len);
      chosen[len] = '/';
      memcpy(chosen + len + 1, e->name, e->len + 1);
      return BROWSE_FILE;
   }
   case M2M_KEY_RUNSTOP:
      return BROWSE_CANCEL;
   case M2M_KEY_F1:
   case M2M_KEY_F3: {
      uint32_t want = k == M2M_KEY_F3 ? M2M_CSR_SD_ACTIVE : 0;
      if ((M2M_CSR & M2M_CSR_SD_ACTIVE) == want)
         break;
      M2M_CSR = (M2M_CSR & ~M2M_CSR_SD_FORCE) | M2M_CSR_SD_MODE | (want ? M2M_CSR_SD_FORCE : 0);
      return BROWSE_CHANGED;
   }
   }
   return BROWSE_BUSY;
}

const char *browse_path(void)
{
   return chosen;
}

const char *browse_name(void)
{
   return strrchr(chosen, '/') + 1;
}
