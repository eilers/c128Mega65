// Assembly 64 in the menu, on the Shell's screen: the query form (four typed fields, then
// the pick lists the server's presets give), the titles found, and a title's files. A file
// is downloaded to the card, into /c128/a64/TITLE (ID-CATEGORY), and then used as the menu
// uses a file of its kind: a disk image into a drive, a program run or loaded (REST's
// runner), a .crt as the virtual cartridge. A file already there, of the size the server
// gives, is not fetched again.
//
// The pick lists start with Category, Type and Date; Return in a typed field searches; the
// titles come 50 at a time, and more follow at the list's end. The screens stay as they
// were left, so the next disk of a title is the menu's Assembly 64 line and Return.
// Ported from the 1541 Ultimate firmware (github.com/GideonZ/1541ultimate), Copyright
// Gideon Zweijtzer and contributors, GPL v3 as this core; changed for c128Mega65 in 2026.
#include <string.h>
#include <strings.h>
#include "a64.h"
#include "a64_search.h"
#include "config_ids.h"
#include "console.h"
#include "core.h"
#include "drives.h"
#include "ff.h"
#include "fmt.h"
#include "heap.h"
#include "keys.h"
#include "m2m_rv.h"
#include "net.h"
#include "runners.h"
#include "screen.h"
#include "sdcard.h"
#include "stores.h"
#define JSMN_STATIC
#include "jsmn.h"

#define COLS        (OSM_DX - 2)
#define ROWS        (OSM_DY - 2)
#define A64_DIR     "/c128/a64"
#define JSON_SIZE   (40 * 1024)        // a page of titles is 15 KB at most
#define TOKENS      2560
#define FIELDS_MAX  12
#define VALUES_MAX  192
#define RESULTS_MAX 500
#define ENTRIES_MAX 128
#define TEXTS       4
#define TEXT_MAX    26                 // a typed field's characters
#define LABEL_W     13                 // "Subcategory: "

typedef struct { char key[24], name[28]; } value_t;
typedef struct { char type[12], label[LABEL_W]; int first, n, pick; } field_t;   // pick -1: any
typedef struct { char id[16]; int16_t cat, year; char name[72], group[40]; } result_t;
typedef struct { char path[100]; int idx; uint32_t size; } entry_t;

// In HyperRAM, from the first time the screen is opened
static struct a64 {
   char      json[JSON_SIZE];
   jsmntok_t tok[TOKENS];
   value_t   values[VALUES_MAX];
   field_t   fields[FIELDS_MAX];
   char      text[TEXTS][TEXT_MAX + 1];
   result_t  results[RESULTS_MAX];
   entry_t   entries[ENTRIES_MAX];
   char      query[640];
   char      file[256];                // the file in use, on the card
   char      msg[400];
} *a;

static const char *const text_name[TEXTS] = { "name", "group", "handle", "event" };

enum { S_NONE, S_PRESETS, S_FORM, S_PICK, S_SEARCH, S_RESULTS, S_MORE, S_FILES_GET, S_FILES,
       S_ACTION, S_DOWNLOAD, S_MSG };
enum { ACT_MOUNT_A, ACT_MOUNT_B, ACT_DISK, ACT_DISK_C64, ACT_RUN, ACT_LOAD, ACT_CRT, ACT_SAVE };

struct list { int n, cur, top; };

static int         at = S_NONE, back;  // the screen, and the one a message returns to
static int         ntok, nfields, nvalues, more;
static int         f_cur;              // the form's line
static struct list l_res, l_files, l_pick, l_act, *ls;
static void      (*ls_text)(int i, char *out);
static int         acts[6], act;       // the file's choices, and the one taken
static int         shown_kb, shown_bar;
static uint32_t    dl_start;           // a download's start, ms
static int         dl_kb, dl_rate;     // the last one's size and KB/s; 0 if nothing was fetched

// --- the server's JSON, off jsmn's tokens -------------------------------------------------

static int parse(void)
{
   jsmn_parser p;
   jsmn_init(&p);
   ntok = jsmn_parse(&p, a->json, a64_got(), a->tok, TOKENS);
   return ntok > 0;
}

// The value of key in object obj, or -1
static int get(int obj, const char *key)
{
   int len = strlen(key);
   if (obj < 0 || a->tok[obj].type != JSMN_OBJECT)
      return -1;
   for (int j = obj + 1; j < ntok - 1 && a->tok[j].start < a->tok[obj].end; j++)
      if (a->tok[j].parent == obj && a->tok[j].end - a->tok[j].start == len &&
          !memcmp(a->json + a->tok[j].start, key, len))
         return j + 1;
   return -1;
}

// The next item of array arr after token j (arr itself to start), or -1
static int item(int arr, int j)
{
   for (j++; j < ntok && a->tok[j].start < a->tok[arr].end; j++)
      if (a->tok[j].parent == arr)
         return j;
   return -1;
}

// A string as the menu font and the card's names have it: code page 437, '?' for the rest
static void text_of(int t, char *out, int size)
{
   int n = 0;
   if (t >= 0) {
      const char *s = a->json + a->tok[t].start, *e = a->json + a->tok[t].end;
      while (s < e && n < size - 1) {
         uint32_t cp = (uint8_t)*s++;
         if (cp == '\\' && s < e) {
            cp = (uint8_t)*s++;
            if (cp == 'u' && e - s >= 4) {
               cp = 0;
               for (int i = 0; i < 4; i++, s++)
                  cp = cp << 4 | (*s <= '9' ? *s - '0' : (*s | 0x20) - 'a' + 10);
            } else if (cp == 'n' || cp == 't' || cp == 'r' || cp == 'b' || cp == 'f')
               cp = ' ';
         } else if (cp >= 0xC0) {            // UTF-8
            int k = cp >= 0xF0 ? 3 : cp >= 0xE0 ? 2 : 1;
            cp &= 0x3F >> k;
            while (k-- && s < e && (*s & 0xC0) == 0x80)
               cp = cp << 6 | (*s++ & 0x3F);
         } else if (cp >= 0x80)
            cp = 0x10000;
         if (cp < 32)
            cp = ' ';
         else if (cp >= 127) {
            cp = cp < 0x10000 ? ff_uni2oem(cp, 437) : 0;
            if (cp < 128)
               cp = '?';
         }
         out[n++] = cp;
      }
   }
   out[n] = 0;
}

static int int_of(int t)
{
   int v = 0, neg = 0;
   if (t < 0)
      return 0;
   const char *s = a->json + a->tok[t].start, *e = a->json + a->tok[t].end;
   if (s < e && *s == '-')
      neg = 1, s++;
   for (; s < e && *s >= '0' && *s <= '9'; s++)
      v = v * 10 + *s - '0';
   return neg ? -v : v;
}

// The presets: [ { "type": "category", "description": "Category", "values": [ { "aqlKey":
// "games", "name": "Games" } ] } ]; a value without a name shows its key. The form's order:
// what narrows a search most first, then the server's.
static int presets_read(void)
{
   static const char *const order[] = { "category", "type", "date", "repo", "subcat", "rating",
                                        "latest", "sort", "order" };
   const int known = sizeof order / sizeof order[0];
   nfields = nvalues = 0;
   if (!parse() || a->tok[0].type != JSMN_ARRAY)
      return 0;
   for (int pass = 0; pass <= known; pass++)
      for (int o = item(0, 0); o >= 0 && nfields < FIELDS_MAX; o = item(0, o)) {
         field_t *f = &a->fields[nfields];
         int vals = get(o, "values"), rank = known;
         text_of(get(o, "type"), f->type, sizeof f->type);
         for (int i = 0; i < known; i++)
            if (!strcmp(f->type, order[i]))
               rank = i;
         if (rank != pass || !*f->type || vals < 0 || a->tok[vals].type != JSMN_ARRAY)
            continue;
         text_of(get(o, "description"), f->label, sizeof f->label - 1);
         if (!*f->label) {                // none: the type, with a capital
            strcpy(f->label, f->type);
            if (f->label[0] >= 'a' && f->label[0] <= 'z')
               f->label[0] -= 32;
         }
         strcat(f->label, ":");
         f->first = nvalues;
         f->pick = -1;
         for (int v = item(vals, vals); v >= 0 && nvalues < VALUES_MAX; v = item(vals, v)) {
            value_t *val = &a->values[nvalues];
            text_of(get(v, "aqlKey"), val->key, sizeof val->key);
            text_of(get(v, "name"), val->name, sizeof val->name);
            if (!*val->name)
               strcpy(val->name, val->key);
            if (*val->key)
               nvalues++;
         }
         f->n = nvalues - f->first;
         if (f->n)
            nfields++;
      }
   return 1;
}

// A page of titles, after the ones there: how many, or -1
static int results_read(void)
{
   int n = 0;
   if (!parse() || a->tok[0].type != JSMN_ARRAY)
      return -1;
   for (int o = item(0, 0); o >= 0; o = item(0, o), n++) {
      if (l_res.n == RESULTS_MAX)
         continue;
      result_t *r = &a->results[l_res.n];
      text_of(get(o, "id"), r->id, sizeof r->id);
      text_of(get(o, "name"), r->name, sizeof r->name);
      text_of(get(o, "group"), r->group, sizeof r->group);
      r->cat  = int_of(get(o, "category"));
      r->year = int_of(get(o, "year"));
      if (*r->id)
         l_res.n++;
   }
   return n;
}

// A title's files: { "contentEntry": [ { "path": "a.d64", "id": 0, "size": 174848 } ] }
static int entries_read(void)
{
   l_files.n = l_files.cur = l_files.top = 0;
   if (!parse())
      return 0;
   int list = get(0, "contentEntry");
   if (list < 0 || a->tok[list].type != JSMN_ARRAY)
      return 0;
   for (int o = item(list, list); o >= 0 && l_files.n < ENTRIES_MAX; o = item(list, o)) {
      entry_t *e = &a->entries[l_files.n];
      int id = get(o, "id");
      text_of(get(o, "path"), e->path, sizeof e->path);
      e->idx  = id < 0 ? l_files.n : int_of(id);
      e->size = int_of(get(o, "size"));
      if (*e->path)
         l_files.n++;
   }
   return 1;
}

// --- the screen ---------------------------------------------------------------------------

// The frame, a title on its top line and a hint on its bottom line
static void frame(const char *title, const char *hint)
{
   char s[COLS];
   scr_clear();
   scr_frame(0, 0, OSM_DX, OSM_DY);
   fmt_snprintf(s, sizeof s - 4, " %s", title);
   strcat(s, " ");
   osm_print(2, 0, SCR_COL_STD, s);
   scr_bottom(hint);
}

static void cut(char *s, int max)
{
   if ((int)strlen(s) > max)
      strcpy(s + max - 3, "...");
}

static void list_draw(void)
{
   char s[200];
   for (int r = 0; r < ROWS; r++) {
      int i = ls->top + r;
      s[0] = 0;
      if (i < ls->n) {
         ls_text(i, s);
         cut(s, COLS);
      }
      osm_printn(1, 1 + r, i == ls->cur ? SCR_COL_STD_INV : SCR_COL_STD, s, COLS);
   }
}

static void list_show(struct list *l, void (*text)(int i, char *out))
{
   ls = l;
   ls_text = text;
   if (l->cur >= l->n)
      l->cur = l->n ? l->n - 1 : 0;
   if (l->cur < l->top || l->cur >= l->top + ROWS)
      l->top = l->cur / ROWS * ROWS;
   if (l->top + ROWS > l->n)
      l->top = l->n > ROWS ? l->n - ROWS : 0;
   list_draw();
}

static void list_move(int cur, int top)
{
   if (top != ls->top) {
      ls->cur = cur;
      ls->top = top;
      list_draw();
      return;
   }
   scr_attr(1, 1 + ls->cur - top, COLS, SCR_COL_STD);
   ls->cur = cur;
   scr_attr(1, 1 + ls->cur - top, COLS, SCR_COL_STD_INV);
}

// As the file browser's: Up and Down move, Left and Right page
static void list_key(uint32_t k)
{
   int n = ls->n, cur = ls->cur, top = ls->top, shown = n - top < ROWS ? n - top : ROWS;
   if (!n)
      return;
   switch (k) {
   case M2M_KEY_UP:
      if (cur > 0)
         list_move(cur - 1, cur > top ? top : top - 1);
      break;
   case M2M_KEY_DOWN:
      if (cur < n - 1)
         list_move(cur + 1, cur < top + ROWS - 1 ? top : top + 1);
      break;
   case M2M_KEY_LEFT:
      if (top == 0)
         list_move(0, 0);
      else {
         int by = top < ROWS ? top : ROWS;
         list_move(cur - by, top - by);
      }
      break;
   case M2M_KEY_RIGHT: {
      int left = n - top - shown;
      if (left == 0)
         list_move(n - 1, top);
      else {
         int by = left < ROWS ? left : ROWS;
         list_move(cur + by, top + by);
      }
      break;
   }
   }
}

static void show(int screen);

// A message, until Space: then the screen to (S_NONE: the menu)
static void message(const char *text, int to)
{
   frame("Assembly 64", " Space: continue ");
   scr_print(2, 2, 2, text);
   back = to;
   at = S_MSG;
}

static void status(const char *s)
{
   char hint[COLS];
   fmt_snprintf(hint, sizeof hint, " %s   Run/Stop: cancel ", s);
   scr_bottom(hint);
}

// --- the form -----------------------------------------------------------------------------

static int form_lines(void)
{
   return TEXTS + nfields + 1;
}

static int form_y(int i)
{
   return i < TEXTS ? 2 + i : i < TEXTS + nfields ? 3 + i : 4 + i;
}

static void form_line(int i)
{
   char s[COLS + 8], label[LABEL_W + 1];
   int on = i == f_cur;
   const char *l = "", *v = "";
   if (i < TEXTS) {
      static const char *const labels[TEXTS] = { "Name:", "Group:", "Handle:", "Event:" };
      l = labels[i];
      v = a->text[i];
   } else if (i < TEXTS + nfields) {
      const field_t *f = &a->fields[i - TEXTS];
      l = f->label;
      v = f->pick < 0 ? "Any" : a->values[f->first + f->pick].name;
   }
   memset(label, ' ', LABEL_W);
   label[LABEL_W] = 0;
   memcpy(label, l, strlen(l));
   if (i == TEXTS + nfields)
      fmt_snprintf(s, sizeof s, " Search");
   else
      fmt_snprintf(s, sizeof s, " %s%s%s", label, v, on && i < TEXTS ? "_" : "");
   osm_printn(1, form_y(i), on ? SCR_COL_STD_INV : SCR_COL_STD, s, COLS);
}

static void form_hint(void)
{
   scr_bottom(f_cur >= TEXTS && f_cur < TEXTS + nfields ? " Return: list   Space: any " :
                                                        " Return: search   Run/Stop: back ");
}

static void form_show(void)
{
   frame("Assembly 64", NULL);
   for (int i = 0; i < form_lines(); i++)
      form_line(i);
   form_hint();
}

// The query: (name:"jumpman") & (type:d64), the fields that have something
static int query_build(void)
{
   int n = 0;
   char *q = a->query;
   const int size = sizeof a->query;
   q[0] = 0;
   for (int i = 0; i < TEXTS; i++)
      if (*a->text[i])
         n += fmt_snprintf(q + n, size - n, "%s(%s:\"%s\")", n ? " & " : "", text_name[i], a->text[i]);
   for (int i = 0; i < nfields; i++) {
      const field_t *f = &a->fields[i];
      if (f->pick < 0)
         continue;
      const char *key = a->values[f->first + f->pick].key;
      n += fmt_snprintf(q + n, size - n, "%s(%s:%s%s)", n ? " & " : "", f->type,
                        !strcasecmp(f->type, "rating") && key[0] != '>' ? ">=" : "", key);
   }
   return n > 0 && n < size;
}

static void search(void)
{
   if (!query_build()) {
      scr_bottom(" Type a name or pick something first ");
      return;
   }
   l_res.n = l_res.cur = l_res.top = 0;
   more = 0;
   a64_search(a->query, 0, a->json, JSON_SIZE);
   status("Searching...");
   at = S_SEARCH;
}

static void pick_text(int i, char *out)
{
   const field_t *f = &a->fields[f_cur - TEXTS];
   fmt_snprintf(out, COLS, " %s", i ? a->values[f->first + i - 1].name : "Any");
}

static int form_key(uint32_t k, int ch)
{
   int lines = form_lines(), was = f_cur;
   if (k == M2M_KEY_RUNSTOP || ch == KEY_ESC)
      return APP_MENU;
   if (k == M2M_KEY_UP && f_cur > 0)
      f_cur--;
   else if (k == M2M_KEY_DOWN && f_cur < lines - 1)
      f_cur++;
   if (f_cur != was) {
      form_line(was);
      form_line(f_cur);
      form_hint();
      return APP_BUSY;
   }
   if (f_cur < TEXTS) {
      char *t = a->text[f_cur];
      int n = strlen(t);
      if (k == M2M_KEY_RETURN) {
         search();
         return APP_BUSY;
      }
      if (ch == KEY_DEL && n)
         t[n - 1] = 0;
      else if (ch == KEY_CLR)
         t[0] = 0;
      else if (ch >= ' ' && ch != '"' && n < TEXT_MAX) {
         t[n] = ch;
         t[n + 1] = 0;
      } else
         return APP_BUSY;
      form_line(f_cur);
      form_hint();
   } else if (f_cur < TEXTS + nfields) {
      field_t *f = &a->fields[f_cur - TEXTS];
      if (k == M2M_KEY_RETURN) {
         l_pick.n = f->n + 1;
         l_pick.cur = f->pick + 1;
         l_pick.top = 0;
         frame(f->label, " Return: pick   Run/Stop: back ");
         list_show(&l_pick, pick_text);
         at = S_PICK;
         return APP_BUSY;
      }
      if ((k == M2M_KEY_RIGHT || ch == '+') && f->pick < f->n - 1)
         f->pick++;
      else if ((k == M2M_KEY_LEFT || ch == '-') && f->pick >= 0)
         f->pick--;
      else if (k == M2M_KEY_SPACE || ch == KEY_DEL || ch == KEY_CLR)
         f->pick = -1;
      else
         return APP_BUSY;
      form_line(f_cur);
   } else if (k == M2M_KEY_RETURN)
      search();
   return APP_BUSY;
}

// --- the titles and a title's files -------------------------------------------------------

// Name (Group, Year)
static void result_text(int i, char *out)
{
   const result_t *r = &a->results[i];
   int n = fmt_snprintf(out, 100, " %s", r->name);
   if (*r->group) {
      n += fmt_snprintf(out + n, 60, " (%s", r->group);
      if (r->year)
         n += fmt_snprintf(out + n, 12, ", %d", r->year);
      strcpy(out + n, ")");
   }
}

static void results_hint(void)
{
   char s[COLS];
   fmt_snprintf(s, sizeof s, " %d%s found   Return: files ", l_res.n, more ? "+" : "");
   scr_bottom(s);
}

static void results_show(void)
{
   frame("Assembly 64: titles", NULL);
   results_hint();
   list_show(&l_res, result_text);
}

// The name, and the size at the right: 171K
static void file_text(int i, char *out)
{
   const entry_t *e = &a->entries[i];
   char size[12], name[sizeof e->path + 2];
   uint32_t kb = (e->size + 1023) >> 10;
   if (!e->size)
      size[0] = 0;
   else if (kb < 10000)
      fmt_snprintf(size, sizeof size, "%uK", (unsigned)kb);
   else
      fmt_snprintf(size, sizeof size, "%uM", (unsigned)(kb >> 10));
   fmt_snprintf(name, sizeof name, " %s", e->path);
   cut(name, COLS - 7);
   memset(out, ' ', COLS);
   memcpy(out, name, strlen(name));
   strcpy(out + COLS - 1 - strlen(size), size);
}

static void files_show(void)
{
   frame(a->results[l_res.cur].name, " Return: use   Run/Stop: back ");
   list_show(&l_files, file_text);
}

// --- a file: what to do with it, the download, and doing it -------------------------------

static int has_ext(const char *name, const char *ext)
{
   int n = strlen(name), e = strlen(ext);
   return n > e && !strcasecmp(name + n - e, ext);
}

static void act_text(int i, char *out)
{
   switch (acts[i]) {
   case ACT_MOUNT_A:
   case ACT_MOUNT_B:
      fmt_snprintf(out, COLS, " Mount in drive %c (%d)", acts[i] == ACT_MOUNT_A ? 'A' : 'B',
                   stores_drive_id(acts[i] == ACT_MOUNT_B));
      break;
   case ACT_DISK: strcpy(out, " Run, from drive A"); break;
   case ACT_DISK_C64: strcpy(out, " Run in C64 mode, from drive A"); break;
   case ACT_RUN:  strcpy(out, " Run"); break;
   case ACT_LOAD: strcpy(out, " Load, without RUN"); break;
   case ACT_CRT:  strcpy(out, " Start as cartridge"); break;
   default:       strcpy(out, " Download to the SD card only"); break;
   }
}

static void actions_show(void)
{
   const char *name = a->entries[l_files.cur].path;
   int n = 0;
   if (has_ext(name, ".d64") || has_ext(name, ".d71") || has_ext(name, ".d81")) {
      acts[n++] = ACT_MOUNT_A;
      acts[n++] = ACT_DISK;
      acts[n++] = ACT_DISK_C64;
      if (drives_count() > 1)
         acts[n++] = ACT_MOUNT_B;
   } else if (has_ext(name, ".prg") || has_ext(name, ".p00")) {
      acts[n++] = ACT_RUN;
      acts[n++] = ACT_LOAD;
   } else if (has_ext(name, ".crt"))
      acts[n++] = ACT_CRT;
   acts[n++] = ACT_SAVE;
   l_act.n = n;
   l_act.cur = l_act.top = 0;
   frame(name, " Return: go   Run/Stop: back ");
   list_show(&l_act, act_text);
}

// A name the card takes: without FAT's forbidden characters, no dot or space at its end
static void fat_name(const char *in, char *out, int max)
{
   int n = 0;
   for (; *in && n < max; in++)
      out[n++] = (uint8_t)*in < 32 || *in == 127 || strchr("\"*:/<>\\?|", *in) ? '_' : *in;
   while (n && (out[n - 1] == ' ' || out[n - 1] == '.'))
      n--;
   if (!n)
      out[n++] = '_';
   out[n] = 0;
}

// /c128/a64/TITLE (ID-CATEGORY)/NAME in a->file, its folders made
static int file_path(void)
{
   const result_t *r = &a->results[l_res.cur];
   const entry_t *e = &a->entries[l_files.cur];
   const char *base = e->path, *s;
   char title[41], id[sizeof r->id], name[81];
   for (s = e->path; *s; s++)
      if (*s == '/' || *s == '\\')
         base = s + 1;
   fat_name(r->name, title, sizeof title - 1);
   fat_name(r->id, id, sizeof id - 1);
   fat_name(base, name, sizeof name - 1);
   int n = fmt_snprintf(a->file, sizeof a->file, A64_DIR "/%s (%s-%d)", title, id, r->cat);
   f_mkdir("/c128");
   f_mkdir(A64_DIR);
   FRESULT res = f_mkdir(a->file);
   fmt_snprintf(a->file + n, sizeof a->file - n, "/%s", name);
   return res == FR_OK || res == FR_EXIST;
}

static void progress_show(void)
{
   frame("Assembly 64", " Run/Stop: cancel ");
   scr_print(2, 2, 2, "Downloading");
   osm_printn(2, 3, SCR_COL_STD, a->entries[l_files.cur].path, COLS - 2);
   shown_kb = shown_bar = -1;
   dl_start = net_millis();
}

// KB/s so far
static int rate(void)
{
   uint32_t ms = net_millis() - dl_start;
   return ms ? (int)((uint64_t)a64_got() * 1000 / ms >> 10) : 0;
}

static void progress(void)
{
   static const char block[] = { (char)254, 0 };
   int got = a64_got(), total = a64_total();
   char s[COLS];
   if (total <= 0)
      total = a->entries[l_files.cur].size;
   int kb = got >> 10, bar = total > 0 ? (int)((uint64_t)got * (COLS - 2) / total) : 0;
   if (kb != shown_kb) {
      shown_kb = kb;
      if (total > 0)
         fmt_snprintf(s, sizeof s, "%d of %d KB, %d KB/s", kb, (total + 1023) >> 10, rate());
      else
         fmt_snprintf(s, sizeof s, "%d KB, %d KB/s", kb, rate());
      osm_printn(2, 7, SCR_COL_STD, s, COLS - 2);
   }
   for (; shown_bar < bar && shown_bar < COLS - 3; shown_bar++)
      if (shown_bar >= 0)
         osm_print(2 + shown_bar, 5, SCR_COL_STD, block);
}

// The file is on the card: what it was fetched for
static int use(void)
{
   const char *msg = NULL;
   int err;
   switch (act) {
   case ACT_MOUNT_A:
   case ACT_MOUNT_B:
   case ACT_DISK:
   case ACT_DISK_C64:
      if (!(err = shell_mount(act == ACT_MOUNT_B, a->file, &msg))) {
         at = S_FILES;
         if (act == ACT_MOUNT_A || act == ACT_MOUNT_B)
            return APP_MENU;              // where the drive's line shows it
         runner_later_disk(0, act == ACT_DISK_C64);
         return APP_CLOSE;
      }
      if (!msg) {
         fmt_snprintf(a->msg, sizeof a->msg, "The disk image could not be mounted.\nError code: %04X", err);
         msg = a->msg;
      }
      message(msg, S_FILES);
      break;
   case ACT_RUN:
   case ACT_LOAD:
      runner_later_prg(a->file, act == ACT_RUN);   // once the menu is closed
      at = S_FILES;
      return APP_CLOSE;
   case ACT_CRT:
      if (!runner_crt(a->file, &msg)) {
         core_file_loaded(OPTM_G_MOUNT_CRT, a->file);
         at = S_FILES;
         return APP_CLOSE;
      }
      message(msg ? msg : "This .crt file could not be loaded.", S_FILES);
      break;
   default: {
      int n = fmt_snprintf(a->msg, sizeof a->msg, "Saved on the SD card as\n");
      for (int i = 0; a->file[i]; i++) {  // a long path on more lines
         if (i && i % (COLS - 3) == 0)
            a->msg[n++] = '\n';
         a->msg[n++] = a->file[i];
      }
      a->msg[n] = 0;
      if (dl_kb)
         fmt_snprintf(a->msg + n, sizeof a->msg - n, "\n\n%d KB at %d KB/s", dl_kb, dl_rate);
      message(a->msg, S_FILES);
      break;
   }
   }
   return APP_BUSY;
}

// Fetch the file unless the card has it: one of the server's size, or one a drive holds
static int fetch(void)
{
   const result_t *r = &a->results[l_res.cur];
   const entry_t *e = &a->entries[l_files.cur];
   FILINFO fi;
   if (sdcard_state() != SDCARD_OK) {
      message("There is no SD card to download to.", S_FILES);
      return APP_BUSY;
   }
   if (!file_path()) {
      message("The SD card could not be written.", S_FILES);
      return APP_BUSY;
   }
   int have = f_stat(a->file, &fi) == FR_OK && !(fi.fattrib & AM_DIR) && (!e->size || fi.fsize == e->size);
   for (int d = 0; d < drives_count(); d++)
      have |= !strcasecmp(drive_path(d), a->file);
   dl_kb = 0;
   if (have)
      return use();
   progress_show();
   a64_binary(r->id, r->cat, e->idx, a->file);
   at = S_DOWNLOAD;
   return APP_BUSY;
}

// --- the screens' keys --------------------------------------------------------------------

static void show(int screen)
{
   at = screen;
   switch (screen) {
   case S_FORM:    form_show(); break;
   case S_RESULTS: results_show(); break;
   case S_FILES:   files_show(); break;
   }
}

int a64_open(void)
{
   if (!a)
      a = heap_alloc(sizeof *a);
   keys_text();
   switch (at) {
   case S_NONE:
   case S_PRESETS:
      frame("Assembly 64", NULL);
      scr_print(2, 2, 2, "Connecting to Assembly 64...");
      status("Connecting...");
      a64_presets(a->json, JSON_SIZE);
      at = S_PRESETS;
      break;
   case S_RESULTS:
   case S_MORE:
   case S_FILES_GET:
      show(S_RESULTS);
      break;
   case S_FILES:
   case S_ACTION:
   case S_DOWNLOAD:
      show(S_FILES);
      break;
   default:
      show(S_FORM);
      break;
   }
   return 1;
}

int a64_key(uint32_t k)
{
   int ch = keys_char(), st;
   int stop = k == M2M_KEY_RUNSTOP || ch == KEY_ESC;

   switch (at) {
   case S_PRESETS:
      st = a64_poll();
      if (stop) {
         a64_cancel();
         at = S_NONE;
         return APP_MENU;
      }
      if (st == A64_DONE && presets_read()) {
         f_cur = 0;
         show(S_FORM);
      } else if (st == A64_DONE || st == A64_FAILED) {
         message(st == A64_DONE ? "Assembly 64's answer could not be read." : a64_error(), S_NONE);
      }
      break;

   case S_FORM:
      return form_key(k, ch);

   case S_PICK:
      if (stop)
         show(S_FORM);
      else if (k == M2M_KEY_RETURN) {
         a->fields[f_cur - TEXTS].pick = l_pick.cur - 1;
         show(S_FORM);
      } else
         list_key(k);
      break;

   case S_SEARCH:
   case S_MORE: {
      int first = at == S_SEARCH, n;
      st = a64_poll();
      if (stop) {
         a64_cancel();
         if (first)
            show(S_FORM);
         else {
            at = S_RESULTS;
            results_hint();
         }
      } else if (st == A64_FAILED) {
         message(a64_error(), first ? S_FORM : S_RESULTS);
      } else if (st == A64_DONE) {
         if ((n = results_read()) < 0)
            message("Assembly 64's answer could not be read.", first ? S_FORM : S_RESULTS);
         else if (first && !l_res.n)
            message("Nothing found.", S_FORM);
         else {
            more = n == A64_PAGE && l_res.n < RESULTS_MAX;
            if (first)
               show(S_RESULTS);
            else {
               at = S_RESULTS;
               results_hint();
               list_draw();
            }
         }
      }
      break;
   }

   case S_RESULTS:
      if (stop)
         show(S_FORM);
      else if (k == M2M_KEY_RETURN && l_res.n) {
         const result_t *r = &a->results[l_res.cur];
         a64_entries(r->id, r->cat, a->json, JSON_SIZE);
         status("Loading...");
         at = S_FILES_GET;
      } else if (k) {
         list_key(k);
         // At the list's end: the next page
         if (more && l_res.cur == l_res.n - 1) {
            a64_search(a->query, l_res.n, a->json, JSON_SIZE);
            status("More...");
            at = S_MORE;
         }
      }
      break;

   case S_FILES_GET:
      st = a64_poll();
      if (stop) {
         a64_cancel();
         at = S_RESULTS;
         results_hint();
      } else if (st == A64_FAILED)
         message(a64_error(), S_RESULTS);
      else if (st == A64_DONE) {
         if (!entries_read())
            message("Assembly 64's answer could not be read.", S_RESULTS);
         else if (!l_files.n)
            message("This title has no files.", S_RESULTS);
         else
            show(S_FILES);
      }
      break;

   case S_FILES:
      if (stop)
         show(S_RESULTS);
      else if (k == M2M_KEY_RETURN && l_files.n) {
         actions_show();
         at = S_ACTION;
      } else
         list_key(k);
      break;

   case S_ACTION:
      if (stop)
         show(S_FILES);
      else if (k == M2M_KEY_RETURN) {
         act = acts[l_act.cur];
         return fetch();
      } else
         list_key(k);
      break;

   case S_DOWNLOAD:
      st = a64_poll();
      if (stop) {
         a64_cancel();
         show(S_FILES);
      } else if (st == A64_FAILED)
         message(a64_error(), S_FILES);
      else if (st == A64_DONE) {
         dl_kb = (a64_got() + 1023) >> 10;
         dl_rate = rate();
         return use();
      } else
         progress();
      break;

   case S_MSG:
      if (k == M2M_KEY_SPACE || k == M2M_KEY_RETURN || stop) {
         if (back == S_NONE) {
            at = S_NONE;
            return APP_MENU;
         }
         show(back);
      }
      break;
   }
   return APP_BUSY;
}
