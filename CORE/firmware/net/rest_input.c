// The REST API's machine:input: Gideon's software/api/route_input.cc, input_api.h,
// route_input_menu.h and rest_mouse_queue.h at 4b00732, as his U64 answers them, on input.c
// (keys and joysticks) and mouse.c. A batch is checked whole before any of it is applied,
// with his error texts. Keys while the menu is open go to the menu.
//
// Ours on top of his: the C128's own keys (help, esc, alt, tab, line_feed, no_scroll, the
// keypad, the top-row cursor keys, and 40_80 and caps_lock, which lock as on a real C128:
// press engages, release lets go). Jeff's choice, 2026-11-03.
//
// Ported from the 1541 Ultimate firmware (github.com/GideonZ/1541ultimate), Copyright
// Gideon Zweijtzer and contributors, GPL v3 as this core; changed for c128Mega65 in 2026.
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "fmt.h"
#include "heap.h"
#include "input.h"
#include "mouse.h"
#include "rest_priv.h"
#define JSMN_STATIC
#define JSMN_STRICT 1
#include "jsmn.h"

#define MAX_EVENTS    64
#define MAX_KEYS      8
#define MAX_JOY       7
#define MAX_MOTION    127
#define MAX_DETENTS   64
#define MAX_STEPS     256
#define MIN_INTERVAL  20
#define MAX_INTERVAL  1000
#define DEF_INTERVAL  20
#define MAX_TOKENS    1024
#define BODY_MAX      4096
#define ERR_SIZE      160
#define RESTORE       0xFF         // not a matrix row: NMI

enum { E_KEYBOARD, E_JOYSTICK, E_RELEASE_ALL, E_BUTTONS, E_MOVE, E_WHEEL, E_PATH };
enum { T_PRESS, T_RELEASE, T_TAP };

static const struct { const char *name; uint8_t row, col; } key_map[] = {
   { "inst_del", 0, 0 }, { "return", 0, 1 }, { "cursor_left_right", 0, 2 }, { "f7", 0, 3 },
   { "f1", 0, 4 }, { "f3", 0, 5 }, { "f5", 0, 6 }, { "cursor_up_down", 0, 7 },
   { "3", 1, 0 }, { "w", 1, 1 }, { "a", 1, 2 }, { "4", 1, 3 },
   { "z", 1, 4 }, { "s", 1, 5 }, { "e", 1, 6 }, { "left_shift", 1, 7 },
   { "5", 2, 0 }, { "r", 2, 1 }, { "d", 2, 2 }, { "6", 2, 3 },
   { "c", 2, 4 }, { "f", 2, 5 }, { "t", 2, 6 }, { "x", 2, 7 },
   { "7", 3, 0 }, { "y", 3, 1 }, { "g", 3, 2 }, { "8", 3, 3 },
   { "b", 3, 4 }, { "h", 3, 5 }, { "u", 3, 6 }, { "v", 3, 7 },
   { "9", 4, 0 }, { "i", 4, 1 }, { "j", 4, 2 }, { "0", 4, 3 },
   { "m", 4, 4 }, { "k", 4, 5 }, { "o", 4, 6 }, { "n", 4, 7 },
   { "plus", 5, 0 }, { "p", 5, 1 }, { "l", 5, 2 }, { "minus", 5, 3 },
   { "period", 5, 4 }, { "colon", 5, 5 }, { "at", 5, 6 }, { "comma", 5, 7 },
   { "pound", 6, 0 }, { "star", 6, 1 }, { "semicolon", 6, 2 }, { "clr_home", 6, 3 },
   { "right_shift", 6, 4 }, { "equals", 6, 5 }, { "arrow_up", 6, 6 }, { "slash", 6, 7 },
   { "1", 7, 0 }, { "arrow_left", 7, 1 }, { "ctrl", 7, 2 }, { "2", 7, 3 },
   { "space", 7, 4 }, { "commodore", 7, 5 }, { "q", 7, 6 }, { "run_stop", 7, 7 },
   { "restore", RESTORE, 0 },
   // Ours: K0-K2 on PB 0-7 (Programmer's Reference Guide, the C128 keyboard table), then
   // the locking keys
   { "help", 8, 0 }, { "kp_8", 8, 1 }, { "kp_5", 8, 2 }, { "tab", 8, 3 },
   { "kp_2", 8, 4 }, { "kp_4", 8, 5 }, { "kp_7", 8, 6 }, { "kp_1", 8, 7 },
   { "esc", 9, 0 }, { "kp_plus", 9, 1 }, { "kp_minus", 9, 2 }, { "line_feed", 9, 3 },
   { "kp_enter", 9, 4 }, { "kp_6", 9, 5 }, { "kp_9", 9, 6 }, { "kp_3", 9, 7 },
   { "alt", 10, 0 }, { "kp_0", 10, 1 }, { "kp_period", 10, 2 }, { "up", 10, 3 },
   { "down", 10, 4 }, { "left", 10, 5 }, { "right", 10, 6 }, { "no_scroll", 10, 7 },
   { "40_80", 11, 0 }, { "caps_lock", 11, 1 },
};
#define NKEYS (int)(sizeof key_map / sizeof key_map[0])

static const char *const joy_names[MAX_JOY] = { "up", "down", "left", "right", "fire", "fire2", "fire3" };
static const struct { const char *name; uint8_t mask; } button_map[] = {
   { "left", 0x01 }, { "right", 0x02 }, { "middle", 0x04 },
};

typedef struct {
   uint8_t  kind, transition, port, count;
   uint8_t  index[MAX_KEYS];
   uint8_t  joy_mask, buttons;
   int16_t  x, y, wheel_v, wheel_h;
   int      path;                  // the token of a path's list
   uint16_t steps, interval;
} event_t;

// --- his JSON objects, read off jsmn's tokens ---------------------------------------------

static jsmntok_t *tok;
static char      *txt;
static int        ntok;

enum { J_OBJ, J_LIST, J_STR, J_INT, J_BOOL };

static int jtype(int t)
{
   switch (tok[t].type) {
   case JSMN_OBJECT: return J_OBJ;
   case JSMN_ARRAY:  return J_LIST;
   case JSMN_STRING: return J_STR;
   default:          return txt[tok[t].start] == 't' || txt[tok[t].start] == 'f' ? J_BOOL : J_INT;
   }
}

static int jint(int t)
{
   return strtol(txt + tok[t].start, NULL, 0);
}

static const char *jstr(int t)
{
   return txt + tok[t].start;
}

// The token after t and everything in it
static int jnext(int t)
{
   int e = t + 1;
   while (e < ntok && tok[e].start < tok[t].end)
      e++;
   return e;
}

// His JSON_Object::get: the first key, in any case; -1 without it
static int jget(int o, const char *name)
{
   for (int k = o + 1, e = jnext(o); k < e; k = jnext(k + 1))
      if (!strcasecmp(jstr(k), name))
         return k + 1;
   return -1;
}

// The list's item i
static int jitem(int l, int i)
{
   int t = l + 1;
   while (i--)
      t = jnext(t);
   return t;
}

// His convert(): a token he can't place makes the whole text unparsed (as rest_configs.c)
static int convertible(int n)
{
   for (int i = 0; i < n; i++) {
      const jsmntok_t *t = &tok[i];
      if (t->start < 0 || t->end < t->start)
         return 0;
      if (t->parent < 0)
         continue;
      const jsmntok_t *p = &tok[t->parent];
      if (p->type == JSMN_STRING && tok[p->parent].type != JSMN_OBJECT)
         return 0;
      if (p->type == JSMN_OBJECT && t->type != JSMN_STRING)
         return 0;
   }
   return 1;
}

// --- input_api.h: the checks, with his texts ----------------------------------------------

static char err[ERR_SIZE];

static void preview(char *dst, const char *src)
{
   int i = 0;
   while (src[i] && i + 1 < 32) {
      dst[i] = src[i];
      i++;
   }
   if (src[i]) {
      dst[28] = dst[29] = dst[30] = '.';
      dst[31] = 0;
      return;
   }
   dst[i] = 0;
}

static bool fail(const char *f, const char *a)
{
   fmt_snprintf(err, sizeof err, f, a);
   return false;
}

static bool reject_unknown(int o, const char *const *allowed, int n)
{
   for (int k = o + 1, e = jnext(o); k < e; k = jnext(k + 1)) {
      int ok = 0;
      for (int i = 0; i < n && !ok; i++)
         ok = !strcmp(jstr(k), allowed[i]);
      if (!ok) {
         char p[32];
         preview(p, jstr(k));
         return fail("Unknown field `%s`.", p);
      }
   }
   return true;
}

static bool string_field(int o, const char *name, const char **out)
{
   int v = jget(o, name);
   if (v < 0)
      return fail("`%s` is required.", name);
   if (jtype(v) != J_STR)
      return fail("`%s` must be a string.", name);
   *out = jstr(v);
   return true;
}

static bool transition(int o, event_t *ev)
{
   const char *name;
   if (!string_field(o, "transition", &name))
      return false;
   if (!strcmp(name, "press"))
      ev->transition = T_PRESS;
   else if (!strcmp(name, "release"))
      ev->transition = T_RELEASE;
   else if (!strcmp(name, "tap"))
      ev->transition = T_TAP;
   else
      return fail("%s", "`transition` must be one of `press`, `release`, or `tap`.");
   return true;
}

// `inputs`: required, a list of lo..hi entries
static int inputs(int o, int hi)
{
   int l = jget(o, "inputs");
   if (l < 0)
      return fail("%s", "`inputs` is required."), -1;
   if (jtype(l) != J_LIST)
      return fail("%s", "`inputs` must be an array."), -1;
   if (tok[l].size < 1 || tok[l].size > hi) {
      fmt_snprintf(err, sizeof err, "`inputs` must contain 1..%d entries.", hi);
      return -1;
   }
   return l;
}

static bool keyboard_event(int o, event_t *ev)
{
   static const char *const allowed[] = { "kind", "inputs", "transition" };
   bool seen[NKEYS], restore = false;
   if (!reject_unknown(o, allowed, 3))
      return false;
   if (jget(o, "port") >= 0)
      return fail("%s", "`port` is not allowed for keyboard events.");
   if (!transition(o, ev))
      return false;
   int l = inputs(o, MAX_KEYS);
   if (l < 0)
      return false;
   memset(seen, 0, sizeof seen);
   ev->kind = E_KEYBOARD;
   ev->count = tok[l].size;
   for (int i = 0; i < ev->count; i++) {
      int t = jitem(l, i), k;
      if (jtype(t) != J_STR)
         return fail("%s", "Keyboard inputs must be strings.");
      for (k = 0; k < NKEYS && strcmp(jstr(t), key_map[k].name); k++)
         ;
      char p[32];
      preview(p, jstr(t));
      if (k == NKEYS)
         return fail("`%s` is not a valid keyboard input.", p);
      if (seen[k])
         return fail("`%s` appears more than once in `inputs`.", p);
      seen[k] = true;
      ev->index[i] = k;
      restore |= key_map[k].row == RESTORE;
   }
   // RESTORE is an edge on NMI, not a level: only a tap does anything with it
   if (restore && ev->transition != T_TAP)
      return fail("%s", "`restore` is only valid with transition `tap`.");
   return true;
}

static bool joystick_event(int o, event_t *ev)
{
   static const char *const allowed[] = { "kind", "port", "inputs", "transition" };
   if (!reject_unknown(o, allowed, 4) || !transition(o, ev))
      return false;
   int p = jget(o, "port");
   if (p < 0)
      return fail("%s", "`port` is required.");
   if (jtype(p) != J_INT)
      return fail("%s", "`port` must be an integer.");
   if (jint(p) != 1 && jint(p) != 2)
      return fail("%s", "`port` must be 1 or 2.");
   int l = inputs(o, MAX_JOY);
   if (l < 0)
      return false;
   ev->kind = E_JOYSTICK;
   ev->port = jint(p);
   ev->joy_mask = 0;
   for (int i = 0; i < tok[l].size; i++) {
      int t = jitem(l, i), b;
      if (jtype(t) != J_STR)
         return fail("%s", "Joystick inputs must be strings.");
      for (b = 0; b < MAX_JOY && strcmp(jstr(t), joy_names[b]); b++)
         ;
      char pv[32];
      preview(pv, jstr(t));
      if (b == MAX_JOY)
         return fail("`%s` is not a valid joystick input.", pv);
      if (ev->joy_mask & 1 << b)
         return fail("`%s` appears more than once in `inputs`.", pv);
      ev->joy_mask |= 1 << b;
   }
   return true;
}

// An integer within lo..hi if it is there; errors name it obj.name, or name without obj
static bool int_member(int o, const char *obj, const char *name, int lo, int hi, int *out)
{
   int v = jget(o, name);
   if (v < 0)
      return true;
   if (jtype(v) != J_INT) {
      fmt_snprintf(err, sizeof err, "`%s%s%s` must be an integer.", obj ? obj : "", obj ? "." : "", name);
      return false;
   }
   int n = jint(v);
   if (n < lo || n > hi) {
      fmt_snprintf(err, sizeof err, "`%s%s%s` must be %d..%d.", obj ? obj : "", obj ? "." : "", name, lo, hi);
      return false;
   }
   *out = n;
   return true;
}

static int object_field(int o, const char *name, const char *const *allowed)
{
   int v = jget(o, name);
   if (jtype(v) != J_OBJ) {
      fmt_snprintf(err, sizeof err, "`%s` must be an object.", name);
      return -1;
   }
   for (int k = v + 1, e = jnext(v); k < e; k = jnext(k + 1))
      if (strcmp(jstr(k), allowed[0]) && strcmp(jstr(k), allowed[1])) {
         char p[32];
         preview(p, jstr(k));
         fmt_snprintf(err, sizeof err, "Unknown field `%s.%s`.", name, p);
         return -1;
      }
   return v;
}

static bool mouse_buttons_event(int o, event_t *ev)
{
   if (!transition(o, ev))
      return false;
   int l = jget(o, "inputs");
   if (jtype(l) != J_LIST)
      return fail("%s", "`inputs` must be an array.");
   if (tok[l].size < 1 || tok[l].size > 3)
      return fail("%s", "`inputs` must contain 1..3 entries.");
   ev->kind = E_BUTTONS;
   ev->buttons = 0;
   for (int i = 0; i < tok[l].size; i++) {
      int t = jitem(l, i);
      uint8_t mask = 0;
      if (jtype(t) != J_STR)
         return fail("%s", "Mouse inputs must be strings.");
      for (int b = 0; b < 3; b++)
         if (!strcmp(jstr(t), button_map[b].name))
            mask = button_map[b].mask;
      char p[32];
      preview(p, jstr(t));
      if (!mask)
         return fail("`%s` is not a valid mouse input.", p);
      if (ev->buttons & mask)
         return fail("`%s` appears more than once in `inputs`.", p);
      ev->buttons |= mask;
   }
   return true;
}

static bool mouse_move_event(int o, event_t *ev)
{
   static const char *const allowed[] = { "x", "y" };
   int m = object_field(o, "move", allowed), x = 0, y = 0;
   if (m < 0 || !int_member(m, "move", "x", -MAX_MOTION, MAX_MOTION, &x) ||
       !int_member(m, "move", "y", -MAX_MOTION, MAX_MOTION, &y))
      return false;
   if (jget(m, "x") < 0 && jget(m, "y") < 0)
      return fail("%s", "`move` must have `x` or `y`.");
   ev->kind = E_MOVE;
   ev->x = x;
   ev->y = y;
   return true;
}

static bool mouse_wheel_event(int o, event_t *ev)
{
   static const char *const allowed[] = { "vertical", "horizontal" };
   int w = object_field(o, "wheel", allowed), v = 0, h = 0;
   if (w < 0 || !int_member(w, "wheel", "vertical", -MAX_DETENTS, MAX_DETENTS, &v) ||
       !int_member(w, "wheel", "horizontal", -MAX_DETENTS, MAX_DETENTS, &h))
      return false;
   if (!v && !h)
      return fail("%s", "`wheel` must turn at least one wheel.");
   ev->kind = E_WHEEL;
   ev->wheel_v = v;
   ev->wheel_h = h;
   return true;
}

static bool mouse_path_event(int o, event_t *ev)
{
   int l = jget(o, "path");
   if (jtype(l) != J_LIST)
      return fail("%s", "`path` must be an array of [x, y] steps.");
   int n = tok[l].size;
   if (n < 1 || n > MAX_STEPS) {
      fmt_snprintf(err, sizeof err, "`path` must contain 1..%d steps.", MAX_STEPS);
      return false;
   }
   for (int i = 0, s = l + 1; i < n; i++, s = jnext(s)) {
      if (jtype(s) != J_LIST || tok[s].size != 2) {
         fmt_snprintf(err, sizeof err, "`path[%d]` must be an [x, y] pair.", i);
         return false;
      }
      for (int a = 0; a < 2; a++) {
         int v = jitem(s, a);
         if (jtype(v) != J_INT) {
            fmt_snprintf(err, sizeof err, "`path[%d]` must hold integers.", i);
            return false;
         }
         if (jint(v) < -MAX_MOTION || jint(v) > MAX_MOTION) {
            fmt_snprintf(err, sizeof err, "`path[%d]` values must be -%d..%d.", i, MAX_MOTION, MAX_MOTION);
            return false;
         }
      }
   }
   int interval = DEF_INTERVAL;
   if (!int_member(o, NULL, "interval_ms", MIN_INTERVAL, MAX_INTERVAL, &interval))
      return false;
   ev->kind = E_PATH;
   ev->path = l;
   ev->steps = n;
   ev->interval = interval;
   return true;
}

// A mouse event does one thing: buttons, one move, a wheel turn or a path
static bool mouse_event(int o, event_t *ev)
{
   static const char *const allowed[] = { "kind", "inputs", "transition", "move", "wheel", "path", "interval_ms" };
   if (!reject_unknown(o, allowed, 7))
      return false;
   bool in = jget(o, "inputs") >= 0, mv = jget(o, "move") >= 0;
   bool wh = jget(o, "wheel") >= 0, pa = jget(o, "path") >= 0;
   if (in + mv + wh + pa != 1)
      return fail("%s", "A mouse event needs exactly one of `inputs`, `move`, `wheel`, or `path`.");
   if (!in && jget(o, "transition") >= 0)
      return fail("%s", "`transition` is only valid with `inputs`.");
   if (!pa && jget(o, "interval_ms") >= 0)
      return fail("%s", "`interval_ms` is only valid with `path`.");
   if (in)
      return mouse_buttons_event(o, ev);
   if (mv)
      return mouse_move_event(o, ev);
   if (wh)
      return mouse_wheel_event(o, ev);
   return mouse_path_event(o, ev);
}

static bool release_all_event(int o, event_t *ev)
{
   static const char *const allowed[] = { "kind" };
   if (!reject_unknown(o, allowed, 1))
      return false;
   if (jget(o, "port") >= 0 || jget(o, "inputs") >= 0 || jget(o, "transition") >= 0)
      return fail("%s", "`release_all` events may only contain `kind`.");
   ev->kind = E_RELEASE_ALL;
   return true;
}

static bool parse_event(int o, event_t *ev)
{
   const char *kind;
   memset(ev, 0, sizeof *ev);
   if (jtype(o) != J_OBJ)
      return fail("%s", "Each event must be an object.");
   if (!string_field(o, "kind", &kind))
      return false;
   if (!strcmp(kind, "keyboard"))
      return keyboard_event(o, ev);
   if (!strcmp(kind, "joystick"))
      return joystick_event(o, ev);
   if (!strcmp(kind, "mouse"))
      return mouse_event(o, ev);
   if (!strcmp(kind, "release_all"))
      return release_all_event(o, ev);
   return fail("%s", "`kind` must be one of `keyboard`, `joystick`, `mouse`, or `release_all`.");
}

// The whole batch, before any of it is applied: its events, or -1 and err (at *at, or -1)
static int validate(event_t *events, int *at)
{
   static const char *const allowed[] = { "events" };
   *at = -1;
   if (jtype(0) != J_OBJ)
      return fail("%s", "Root must be an object."), -1;
   if (!reject_unknown(0, allowed, 1))
      return -1;
   int l = jget(0, "events");
   if (l < 0)
      return fail("%s", "`events` is required."), -1;
   if (jtype(l) != J_LIST)
      return fail("%s", "`events` must be an array."), -1;
   int n = tok[l].size;
   if (n < 1 || n > MAX_EVENTS)
      return fail("%s", "`events` must contain 1..64 entries."), -1;
   for (int i = 0, t = l + 1; i < n; i++, t = jnext(t))
      if (!parse_event(t, &events[i])) {
         *at = i;
         return -1;
      }
   return n;
}

// --- rest_mouse_queue.h -------------------------------------------------------------------

static int reports_for(const event_t *ev)
{
   switch (ev->kind) {
   case E_BUTTONS: return ev->transition == T_TAP ? 2 : 1;
   case E_MOVE:    return 1;
   case E_WHEEL:   return abs(ev->wheel_v) + abs(ev->wheel_h);
   case E_PATH:    return ev->steps;
   default:        return 0;
   }
}

// A release_all empties the queue, but what comes before it is queued too
static bool batch_fits(const event_t *ev, int n, int room, int *needed, int *left)
{
   *left = room;
   *needed = 0;
   for (int i = 0; i < n; i++) {
      if (ev[i].kind == E_RELEASE_ALL) {
         if (*needed > *left)
            return false;
         *left = MOUSE_QUEUE;
         *needed = 0;
      }
      *needed += reports_for(&ev[i]);
   }
   return *needed <= *left;
}

static void mouse_append(const event_t *ev)
{
   uint8_t tail = mouse_tail_buttons();
   switch (ev->kind) {
   case E_BUTTONS:
      if (ev->transition == T_RELEASE) {
         mouse_push(tail & ~ev->buttons, 0, 0, 0, 0, 0);
      } else {
         mouse_push(tail | ev->buttons, 0, 0, 0, 0, 0);
         if (ev->transition == T_TAP)
            mouse_push(mouse_tail_buttons() & ~ev->buttons, 0, 0, 0, 0, MOUSE_TAP_MS);
      }
      break;
   case E_MOVE:
      mouse_push(tail, ev->x, ev->y, 0, 0, 0);
      break;
   case E_WHEEL:
      for (int i = 0; i < abs(ev->wheel_v); i++)
         mouse_push(tail, 0, 0, ev->wheel_v > 0 ? 1 : -1, 0, 0);
      for (int i = 0; i < abs(ev->wheel_h); i++)
         mouse_push(tail, 0, 0, 0, ev->wheel_h > 0 ? 1 : -1, 0);
      break;
   case E_PATH:
      for (int i = 0, s = ev->path + 1; i < ev->steps; i++, s = jnext(s))
         mouse_push(tail, jint(jitem(s, 0)), jint(jitem(s, 1)), 0, 0, i ? ev->interval : 0);
      break;
   }
}

// --- route_input.cc -----------------------------------------------------------------------

// One input request at a time, as his mutex: the one applying a batch, and where it is
static struct {
   rest_t  *owner;
   event_t *events;                 // MAX_EVENTS, in HyperRAM
   int      n, next;
   uint8_t  tapped[IN_ROWS];        // the batch's taps, in the answer (RouteInputResponseKeys)
   bool     tapped_restore;
} b;

static void keys_of(const event_t *ev, uint8_t m[IN_ROWS], bool *restore)
{
   memset(m, 0, IN_ROWS);
   *restore = false;
   for (int i = 0; i < ev->count; i++) {
      int k = ev->index[i];
      if (key_map[k].row == RESTORE)
         *restore = true;
      else
         m[key_map[k].row] |= 1 << key_map[k].col;
   }
}

// His apply_keyboard_event: 0 if a tap waits for room in the queue
static bool keyboard(const event_t *ev)
{
   uint8_t m[IN_ROWS];
   bool restore;
   bool menu = input_menu_sync();
   keys_of(ev, m, &restore);
   if (ev->transition == T_TAP) {
      if (!input_tap(menu ? IN_MENU : IN_LIVE, m, menu ? false : restore))
         return false;
      for (int r = 0; r < IN_ROWS; r++)
         b.tapped[r] |= m[r];
      b.tapped_restore |= restore;
      return true;
   }
   // A release is cleanup: the C128's keys go too, even while the menu has the keyboard
   if (ev->transition == T_RELEASE)
      input_release(IN_LIVE, m);
   if (menu) {
      if (ev->transition == T_PRESS)
         input_press(IN_MENU, m);
      else
         input_release(IN_MENU, m);
      return true;
   }
   if (ev->transition == T_PRESS)
      input_press(IN_LIVE, m);
   return true;
}

static void joystick(const event_t *ev)
{
   uint8_t p = input_joy_persistent(ev->port);
   switch (ev->transition) {
   case T_PRESS:
      input_joy_set(ev->port, p & ~ev->joy_mask);
      break;
   case T_RELEASE:
      input_joy_set(ev->port, p | ev->joy_mask);
      input_joy_cancel(ev->port, ev->joy_mask);      // a tap under way can't mask it
      break;
   case T_TAP:
      input_joy_tap(ev->port, ev->joy_mask);
      break;
   }
}

static void mouse(const event_t *ev)
{
   if (!mouse_attached())
      mouse_attach();
   mouse_start();
   mouse_append(ev);
}

// Everything the API holds let go, and its mouse detached (machine:reset and reboot too)
void input_api_release(void)
{
   input_release_all();
   mouse_detach();
}

static void joy_list(rest_t *r, uint8_t active_low)
{
   int n = 0;
   list_open(r, "inputs");
   for (int i = 0; i < MAX_JOY; i++)
      if (!(active_low & 1 << i))
         list_str(r, n++, joy_names[i]);
   list_close(r, n);
}

// His emit_state_snapshot: what the API holds for the C128 and for the menu, the batch's taps
static void snapshot(rest_t *r, bool batch)
{
   uint8_t m[IN_ROWS], menu[IN_ROWS];
   bool restore;
   input_menu_sync();
   input_snapshot(IN_LIVE, m, &restore);
   input_held(IN_MENU, menu);
   if (batch) {
      for (int i = 0; i < IN_ROWS; i++)
         m[i] |= b.tapped[i];
      restore |= b.tapped_restore;
   }
   int n = 0;
   obj_open(r, "keyboard");
   list_open(r, "inputs");
   for (int k = 0; k < NKEYS; k++) {
      int row = key_map[k].row;
      bool held = row == RESTORE ? restore : ((m[row] | menu[row]) >> key_map[k].col) & 1;
      if (held)
         list_str(r, n++, key_map[k].name);
   }
   list_close(r, n);
   obj_close(r);

   list_open(r, "joysticks");
   for (int p = 1; p <= 2; p++) {
      if (p > 1)
         put(r, ", ");
      obj_open(r, NULL);
      add_int(r, "port", p);
      joy_list(r, input_joy_snapshot(p));
      obj_close(r);
   }
   list_close(r, 2);

   uint8_t buttons = mouse_attached() ? mouse_buttons() : 0;
   obj_open(r, "mouse");
   key(r, "attached");
   put(r, mouse_attached() ? "true" : "false");
   n = 0;
   list_open(r, "inputs");
   for (int i = 0; i < 3; i++)
      if (buttons & button_map[i].mask)
         list_str(r, n++, button_map[i].name);
   list_close(r, n);
   add_int(r, "pending", mouse_pending());
   obj_close(r);
}

// The batch from b.next on: 1 once it is all applied and answered
static int apply_step(rest_t *r)
{
   for (; b.next < b.n; b.next++) {
      const event_t *ev = &b.events[b.next];
      switch (ev->kind) {
      case E_KEYBOARD:
         if (!keyboard(ev))
            return 0;                 // a full tap queue: his task waits a tick and tries again
         break;
      case E_JOYSTICK:
         joystick(ev);
         break;
      case E_BUTTONS:
      case E_MOVE:
      case E_WHEEL:
      case E_PATH:
         mouse(ev);
         break;
      case E_RELEASE_ALL:
         input_release_all();
         input_release_menu();
         memset(b.tapped, 0, sizeof b.tapped);
         b.tapped_restore = false;
         mouse_detach();
         break;
      }
   }
   snapshot(r, true);
   b.owner = NULL;
   json_response(r, HTTP_OK);
   return 1;
}

static void apply_cancel(rest_t *r)
{
   if (b.owner == r)
      b.owner = NULL;
}

// His POST: the body checked, the batch checked whole, then applied in order
static int post_step(rest_t *r)
{
   if (b.owner)
      return 0;                       // another request's batch first (his mutex)
   const char *type = r->req->content_type;
   if (!type || strcasecmp(type, "application/json")) {
      error(r, "Content type should be 'application/json'.");
      json_response(r, HTTP_BAD_REQUEST);
      return 1;
   }
   if (!r->up_parts || !r->up_size) {
      error(r, "Request body is required.");
      json_response(r, HTTP_BAD_REQUEST);
      return 1;
   }
   if (r->up_size > BODY_MAX) {
      error(r, "JSON body is too large.");
      json_response(r, HTTP_BAD_REQUEST);
      return 1;
   }
   if (!tok)
      tok = heap_alloc(MAX_TOKENS * sizeof *tok);
   txt = (char *)r->up;
   txt[r->up_len] = 0;
   jsmn_parser p;
   jsmn_init(&p);
   int n = jsmn_parse(&p, txt, r->up_len, tok, MAX_TOKENS);
   if (n == JSMN_ERROR_NOMEM) {
      error(r, "The request has more than %d JSON values; send long mouse paths in several requests.",
            MAX_TOKENS);
      json_response(r, HTTP_BAD_REQUEST);
      return 1;
   }
   if (n >= 0 && (!n || !convertible(n)))
      n = JSMN_ERROR_ILLEGAL;
   if (n < 0) {
      error(r, "JSON could not be parsed. Error: %d", n);
      json_response(r, HTTP_BAD_REQUEST);
      return 1;
   }
   ntok = n;
   for (int i = 0; i < n; i++)
      txt[tok[i].end] = 0;

   int at, needed, left;
   if (!b.events)
      b.events = heap_alloc(MAX_EVENTS * sizeof *b.events);
   int count = validate(b.events, &at);
   if (count < 0) {
      if (at >= 0)
         error(r, "events[%d]: %s", at, err);
      else
         error(r, "%s", err);
      json_response(r, HTTP_BAD_REQUEST);
      return 1;
   }
   if (!batch_fits(b.events, count, mouse_room(), &needed, &left)) {
      error(r, "The batch needs %d mouse reports and the queue has room for %d; "
            "wait for `mouse.pending` to fall.", needed, left);
      json_response(r, HTTP_TOO_MANY_REQUESTS);
      return 1;
   }
   b.owner = r;
   b.n = count;
   b.next = 0;
   memset(b.tapped, 0, sizeof b.tapped);
   b.tapped_restore = false;
   if (apply_step(r))
      return 1;
   rest_pending(r, apply_step, apply_cancel);
   return 0;
}

static void post_input(rest_t *r)
{
   if (!post_step(r) && !b.owner)
      rest_pending(r, post_step, NULL);
}

static int get_step(rest_t *r)
{
   if (b.owner)
      return 0;
   snapshot(r, false);
   json_response(r, HTTP_OK);
   return 1;
}

static void get_input(rest_t *r)
{
   if (!get_step(r))
      rest_pending(r, get_step, NULL);
}

const call_t rest_input_calls[] = {
   CALL0(HTTP_GET, "machine", "input", get_input),
   CALLB(HTTP_POST, "machine", "input", post_input, BODY_MAX),
};
const int rest_input_count = sizeof rest_input_calls / sizeof rest_input_calls[0];
