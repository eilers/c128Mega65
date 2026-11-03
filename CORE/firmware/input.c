// The REST API's keys, joysticks and mouse lines as the C128 sees them, on the device
// C_DEV_INPUT (CORE/vhdl/input_inject.vhd). Ports of Gideon's Keyboard_USB rest* (held and
// tapped keys, the tap queue: 4 ticks down, 1 up, 100 ms a key), JoystickOutput (held and
// tapped lines, fire2 and fire3 on the POT lines) and MousePotPacer, on the 20 ms ticks of
// his timers. While the menu has the keyboard, keys go to it instead (keys.c).
// Ported from the 1541 Ultimate firmware (github.com/GideonZ/1541ultimate), Copyright
// Gideon Zweijtzer and contributors, GPL v3 as this core; changed for c128Mega65 in 2026.
#include <string.h>
#include "dmem.h"
#include "heap.h"
#include "input.h"
#include "m2m_rv.h"
#include "mouse.h"
#include "net.h"

#define INPUT_DEVICE 0x010D
#define W_DEBUG      12
#define W_COMMIT     15

#define TICK_MS      20            // his REST_INPUT_TIMER_TICKS and JOYSTICK_REST_TIMER_TICKS
#define TAP_HOLD     4             // his REST_KEYBOARD_TAP_HOLD_QUEUE_TICKS is 2: CP/M needs ~50 ms
                                   // down (Jeff, 2026-11-03: 80 ms in every mode)
#define TAP_GAP      1             // REST_TAP_GAP_TICKS
#define TAPS         128           // REST_TAP_QUEUE_SIZE
#define JOY_HOLD     1             // REST_JOYSTICK_TAP_HOLD_TICKS
#define JOY_BITS     7

typedef struct {
   uint8_t m[IN_ROWS];
   bool    restore;
} tap_t;

// His Keyboard_USB's REST state: one for the C128, one for the menu
typedef struct {
   uint8_t state[IN_ROWS];         // held (rest_matrix_state)
   uint8_t overlay[IN_ROWS];       // tapped
   uint8_t hold[IN_ROWS][8];       // ticks a tapped key stays down
   bool    restore, restore_overlay;
   uint8_t restore_hold;
   int     head, tail, gap;
} kbd_t;

static kbd_t kbd[2];
static tap_t *queues[2];           // TAPS each, in HyperRAM
static bool  menu_was;

// His JoystickOutput: low active, as his
static struct {
   uint8_t persistent, overlay, hold[JOY_BITS];
} joy[2] = { { IN_JOY_ALL, IN_JOY_ALL, { 0 } }, { IN_JOY_ALL, IN_JOY_ALL, { 0 } } };
static uint8_t mouse_joy = 0x1F;   // rest_mouse_p1
static bool    mouse_on;           // the mouse's lines are attached (C64_MOUSE_EN_1)
static bool    mouse_active;       // its position is on the POT lines

// His timers: the keyboard's and the joysticks' run every 20 ms while they have work
static uint32_t kbd_at, joy_at, pace_at;
static bool     kbd_run, joy_run;
static bool     awake;
static uint32_t wake_mt;

extern uint32_t keys_api, keys_api_matrix[3];

static void apply(void);
static void pacer_tick(uint32_t now);

uint32_t input_now(void)
{
   return net_millis();
}

// --- MousePotPacer ------------------------------------------------------------------------

#define PACE_STEP   63
#define PACE_BEHIND (4 * PACE_STEP)
#define PACE_WINDOW 24

typedef struct {
   int16_t position, shown, behind;
   uint8_t shown_at[PACE_WINDOW];
} axis_t;

static axis_t   pace_x, pace_y;
static uint16_t pace_latest;

static void axis_reset(axis_t *a, int16_t position, uint8_t spent)
{
   a->position = a->shown = position;
   a->behind = 0;
   memset(a->shown_at, 0, sizeof a->shown_at);
   a->shown_at[0] = spent;
}

static void axis_follow(axis_t *a, int16_t position)
{
   int behind = a->behind + (int16_t)(position - a->position);
   a->position = position;
   a->behind = behind > PACE_BEHIND ? PACE_BEHIND : behind < -PACE_BEHIND ? -PACE_BEHIND : behind;
}

static void axis_age(axis_t *a, int ms)
{
   for (int i = PACE_WINDOW - 1; i >= 0; i--)
      a->shown_at[i] = i >= ms ? a->shown_at[i - ms] : 0;
}

static bool axis_step(axis_t *a)
{
   if (!a->behind)
      return false;
   int room = PACE_STEP;
   for (int i = 0; i < PACE_WINDOW; i++)
      room -= a->shown_at[i];
   if (room <= 0)
      return false;
   int change = a->behind > room ? room : a->behind < -room ? -room : a->behind;
   a->shown += change;
   a->behind -= change;
   a->shown_at[0] += change < 0 ? -change : change;
   return true;
}

static bool pacer_advance(uint16_t now)
{
   uint16_t elapsed = now - pace_latest;
   if (elapsed) {
      int ms = elapsed < PACE_WINDOW ? elapsed : PACE_WINDOW;
      axis_age(&pace_x, ms);
      axis_age(&pace_y, ms);
      pace_latest = now;
   }
   bool x = axis_step(&pace_x);
   bool y = axis_step(&pace_y);
   return x || y;
}

bool input_mouse_behind(void)
{
   return mouse_active && (pace_x.behind || pace_y.behind);
}

// --- the keyboard: his rest* methods ------------------------------------------------------

static bool overlay_active(const kbd_t *k)
{
   if (k->restore_hold)
      return true;
   for (int r = 0; r < IN_ROWS; r++)
      for (int c = 0; c < 8; c++)
         if (k->hold[r][c])
            return true;
   return false;
}

static bool kbd_busy(const kbd_t *k)
{
   return overlay_active(k) || k->gap || k->head != k->tail;
}

static bool kbd_any(const kbd_t *k)
{
   if (k->restore || k->restore_overlay || kbd_busy(k))
      return true;
   for (int r = 0; r < IN_ROWS; r++)
      if (k->state[r] || k->overlay[r])
         return true;
   return false;
}

static void kbd_clear(kbd_t *k)
{
   memset(k, 0, sizeof *k);
}

static tap_t *queue(const kbd_t *k)
{
   int i = k - kbd;
   if (!queues[i])
      queues[i] = heap_alloc(TAPS * sizeof(tap_t));
   return queues[i];
}

// The menu's keys for keys.c: menu keys as the MEGA65's M2M_KEYS has them, and typing by
// MEGA65 key number (= the C64 matrix index; Esc is 71)
static void menu_out(void)
{
   uint8_t m[IN_ROWS];
   bool r;
   input_snapshot(IN_MENU, m, &r);
   bool shift = (m[1] & 0x80) || (m[6] & 0x10);
   uint32_t k = 0;
   if (m[0] & 0x80)
      k |= shift ? M2M_KEY_UP : M2M_KEY_DOWN;         // cursor_up_down
   if (m[0] & 0x04)
      k |= shift ? M2M_KEY_LEFT : M2M_KEY_RIGHT;      // cursor_left_right
   if (m[10] & 0x08)
      k |= M2M_KEY_UP;
   if (m[10] & 0x10)
      k |= M2M_KEY_DOWN;
   if (m[10] & 0x20)
      k |= M2M_KEY_LEFT;
   if (m[10] & 0x40)
      k |= M2M_KEY_RIGHT;
   if (m[0] & 0x02)
      k |= M2M_KEY_RETURN;
   if (m[7] & 0x10)
      k |= M2M_KEY_SPACE;
   if (m[7] & 0x80)
      k |= M2M_KEY_RUNSTOP;
   if (m[8] & 0x01)
      k |= M2M_KEY_HELP;
   if (m[0] & 0x10)
      k |= M2M_KEY_F1;
   if (m[0] & 0x20)
      k |= M2M_KEY_F3;
   keys_api_matrix[0] = m[0] | m[1] << 8 | m[2] << 16 | (uint32_t)m[3] << 24;
   keys_api_matrix[1] = m[4] | m[5] << 8 | m[6] << 16 | (uint32_t)m[7] << 24;
   keys_api_matrix[2] = (m[9] & 0x01) ? 1u << (71 - 64) : 0;
   keys_api = k;
}

static void changed(int to)
{
   if (to == IN_LIVE)
      apply();
   else
      menu_out();
}

void input_press(int to, const uint8_t m[IN_ROWS])
{
   kbd_t *k = &kbd[to];
   for (int r = 0; r < IN_ROWS; r++) {
      k->state[r] |= m[r];
      k->overlay[r] &= ~m[r];
      for (int c = 0; c < 8; c++)
         if (m[r] & 1 << c)
            k->hold[r][c] = 0;
   }
   changed(to);
   input_wake();
}

void input_release(int to, const uint8_t m[IN_ROWS])
{
   kbd_t *k = &kbd[to];
   for (int r = 0; r < IN_ROWS; r++)
      k->state[r] &= ~m[r];
   changed(to);
}

bool input_tap(int to, const uint8_t m[IN_ROWS], bool restore)
{
   kbd_t *k = &kbd[to];
   bool any = restore;
   for (int r = 0; r < IN_ROWS; r++)
      any |= m[r] != 0;
   if (!any)
      return true;
   int next = (k->head + 1) % TAPS;
   if (next == k->tail)
      return false;
   memcpy(queue(k)[k->head].m, m, IN_ROWS);
   queue(k)[k->head].restore = restore;
   k->head = next;
   input_wake();
   return true;
}

void input_snapshot(int to, uint8_t m[IN_ROWS], bool *restore)
{
   const kbd_t *k = &kbd[to];
   for (int r = 0; r < IN_ROWS; r++)
      m[r] = k->state[r] | k->overlay[r];
   *restore = k->restore || k->restore_overlay;
}

static bool same(const tap_t *t, const uint8_t m[IN_ROWS])
{
   return !t->restore && !memcmp(t->m, m, IN_ROWS);
}

int input_tap_count(const uint8_t m[IN_ROWS])
{
   const kbd_t *k = &kbd[IN_LIVE];
   int n = 0;
   for (int i = k->tail; i != k->head; i = (i + 1) % TAPS)
      n += same(&queue(k)[i], m);
   return n;
}

void input_tap_remove(const uint8_t m[IN_ROWS])
{
   kbd_t *k = &kbd[IN_LIVE];
   int to = k->tail;
   for (int i = k->tail; i != k->head; i = (i + 1) % TAPS)
      if (!same(&queue(k)[i], m)) {
         queue(k)[to] = queue(k)[i];
         to = (to + 1) % TAPS;
      }
   k->head = to;
}

void input_held(int to, uint8_t m[IN_ROWS])
{
   memcpy(m, kbd[to].state, IN_ROWS);
}

// His startRestTap, without the chord setup his routes never ask for
static void start_tap(kbd_t *k, const tap_t *t)
{
   for (int r = 0; r < IN_ROWS; r++)
      for (int c = 0; c < 8; c++)
         if (t->m[r] & 1 << c) {
            if (!(k->state[r] & 1 << c))
               k->overlay[r] |= 1 << c;
            k->hold[r][c] = TAP_HOLD;
         }
   if (t->restore) {
      k->restore_overlay = true;
      k->restore_hold = TAP_HOLD;
   }
}

// His tickRestOverlays. A tap starts only once the keys can get through (start).
static bool kbd_tick(kbd_t *k, bool start)
{
   bool change = false;
   for (int r = 0; r < IN_ROWS; r++)
      for (int c = 0; c < 8; c++) {
         if (!k->hold[r][c] || --k->hold[r][c])
            continue;
         k->overlay[r] &= ~(1 << c);
         k->gap = TAP_GAP;
         change = true;
      }
   if (k->restore_hold && !--k->restore_hold) {
      k->restore_overlay = false;
      k->gap = TAP_GAP;
      change = true;
   }
   if (!overlay_active(k)) {
      if (k->gap > 0) {
         k->gap--;
      } else if (k->head != k->tail && start) {
         start_tap(k, &queue(k)[k->tail]);
         k->tail = (k->tail + 1) % TAPS;
         change = true;
      }
   }
   return change;
}

void input_release_menu(void)
{
   kbd_clear(&kbd[IN_MENU]);
   menu_out();
}

bool input_menu_sync(void)
{
   bool active = (M2M_CSR & M2M_CSR_OSM) != 0;
   if (active != menu_was) {
      kbd_clear(&kbd[IN_MENU]);
      menu_out();
   }
   menu_was = active;
   return active;
}

// --- the joysticks: his JoystickOutput ----------------------------------------------------

static bool extra_button(uint8_t active_low)
{
   return (active_low & 0x60) != 0x60;
}

uint8_t input_joy_persistent(int port)
{
   return joy[port - 1].persistent;
}

void input_joy_set(int port, uint8_t active_low)
{
   joy[port - 1].persistent = active_low & IN_JOY_ALL;
   apply();
}

void input_joy_tap(int port, uint8_t mask)
{
   for (int i = 0; i < JOY_BITS; i++)
      if (mask & 1 << i) {
         joy[port - 1].hold[i] = JOY_HOLD;
         joy[port - 1].overlay &= ~(1 << i);
      }
   if (!joy_run) {
      joy_run = true;
      joy_at = input_now() + TICK_MS;
   }
   apply();
   input_wake();
}

void input_joy_cancel(int port, uint8_t mask)
{
   for (int i = 0; i < JOY_BITS; i++)
      if (mask & 1 << i) {
         joy[port - 1].hold[i] = 0;
         joy[port - 1].overlay |= 1 << i;
      }
   apply();
}

uint8_t input_joy_snapshot(int port)
{
   return joy[port - 1].persistent & joy[port - 1].overlay;
}

static bool joy_tick(void)
{
   bool change = false, busy = false;
   for (int p = 0; p < 2; p++)
      for (int i = 0; i < JOY_BITS; i++) {
         if (!joy[p].hold[i])
            continue;
         if (--joy[p].hold[i]) {
            busy = true;
            continue;
         }
         joy[p].overlay |= 1 << i;
         change = true;
      }
   joy_run = busy;
   return change;
}

void input_release_all(void)
{
   kbd_clear(&kbd[IN_LIVE]);
   for (int p = 0; p < 2; p++) {
      joy[p].persistent = joy[p].overlay = IN_JOY_ALL;
      memset(joy[p].hold, 0, sizeof joy[p].hold);
   }
   joy_run = false;
   apply();
}

// --- the mouse's lines --------------------------------------------------------------------

void input_mouse_lines(uint8_t active_low)
{
   mouse_on = true;
   mouse_joy = (active_low & 0x1F) | 0x60;
   apply();
}

// His setMousePosition: the first position shows at once, the rest paced
void input_mouse_position(int16_t x, int16_t y)
{
   uint16_t now = input_now();
   if (!mouse_active) {
      mouse_active = true;
      axis_reset(&pace_x, x, PACE_STEP);
      axis_reset(&pace_y, y, PACE_STEP);
      pace_latest = now;
   } else {
      axis_follow(&pace_x, x);
      axis_follow(&pace_y, y);
      pacer_advance(now);
   }
   apply();
   if (input_mouse_behind()) {
      pace_at = input_now() + 1;
      input_wake();
   }
}

// Detached: port 1 is a joystick port again
void input_mouse_off(void)
{
   mouse_on = mouse_active = false;
   mouse_joy = 0x1F;
   apply();
}

static void pacer_tick(uint32_t now)
{
   if (mouse_active && pacer_advance(now))
      apply();
}

// --- the hardware -------------------------------------------------------------------------

static volatile uint16_t *dev(void)
{
   M2M_RAMROM_DEV = INPUT_DEVICE;
   return M2M_DEV16;
}

// His applyMatrixState and JoystickOutput::apply in one: every word, then a commit, so the
// C128 sees the change whole
static void apply(void)
{
   uint8_t m[IN_ROWS];
   bool restore;
   input_snapshot(IN_LIVE, m, &restore);
   uint8_t p1 = joy[0].persistent & joy[0].overlay & (mouse_joy | 0x60);
   uint8_t p2 = joy[1].persistent & joy[1].overlay;
   uint8_t x1, y1, x2, y2;
   // A fire2 or fire3 press owns both POT lines while held; otherwise the mouse's position
   if (mouse_active && !extra_button(p1)) {
      x1 = pace_x.shown & 0x7F;
      y1 = pace_y.shown & 0x7F;
   } else {
      x1 = p1 & 0x20 ? 0x80 : 0x00;
      y1 = p1 & 0x40 ? 0x80 : 0x00;
   }
   x2 = p2 & 0x20 ? 0x80 : 0x00;
   y2 = p2 & 0x40 ? 0x80 : 0x00;
   int en = (mouse_on || extra_button(p1) ? 1 : 0) | (extra_button(p2) ? 2 : 0);

   volatile uint16_t *d = dev();
   for (int w = 0; w < 4; w++)
      d[w] = m[2 * w] | m[2 * w + 1] << 8;
   d[4] = m[8] | m[9] << 8;
   d[5] = m[10];
   d[6] = (restore ? 1 : 0) | (m[11] & IN_LOCK_4080 ? 2 : 0) | (m[11] & IN_LOCK_CAPS ? 4 : 0);
   d[7] = ~p1 & 0x1F;
   d[8] = ~p2 & 0x1F;
   d[9] = en;
   d[10] = x1 | y1 << 8;
   d[11] = x2 | y2 << 8;
   while (M2M_DEV16[W_COMMIT] & 1)   // the last commit is still on its way: ~100 ns
      ;
   d[W_COMMIT] = 1;
}

int input_debug_get(void)
{
   uint16_t v;
   dev();
   while ((v = M2M_DEV16[W_DEBUG]) & 0x100)   // a write on its way
      ;
   return v & 0xFF;
}

int input_debug_set(int value)
{
   dev()[W_DEBUG] = value & 0xFF;
   return input_debug_get();
}

// --- timing -------------------------------------------------------------------------------

void input_wake(void)
{
   uint32_t now = input_now();
   if (!kbd_run && (kbd_busy(&kbd[IN_LIVE]) || kbd_any(&kbd[IN_MENU]))) {
      kbd_run = true;
      kbd_at = now;                  // a tap from idle starts at once
   }
   awake = true;
   wake_mt = MTIME_LO;
}

static void input_run(void)
{
   uint32_t now = input_now();
   bool menu = input_menu_sync();
   if (kbd_run && (int32_t)(now - kbd_at) >= 0) {
      kbd_at += TICK_MS;
      if ((int32_t)(now - kbd_at) >= 0)
         kbd_at = now + TICK_MS;     // late: the timer doesn't catch up
      if (kbd_tick(&kbd[IN_LIVE], !menu && (M2M_CSR & M2M_CSR_KBD)))
         apply();
      if (kbd_tick(&kbd[IN_MENU], menu))
         menu_out();
      kbd_run = kbd_busy(&kbd[IN_LIVE]) || kbd_any(&kbd[IN_MENU]);
   }
   if (joy_run && (int32_t)(now - joy_at) >= 0) {
      joy_at += TICK_MS;
      if ((int32_t)(now - joy_at) >= 0)
         joy_at = now + TICK_MS;
      if (joy_tick())
         apply();
   }
   bool reports = mouse_run(now);
   if (input_mouse_behind() && (int32_t)(now - pace_at) >= 0) {
      pacer_tick(now);
      pace_at = now + 1;
   }
   awake = kbd_run || joy_run || reports || input_mouse_behind();
   wake_mt = MTIME_LO + CPU_HZ / 1000;
}

// Nothing to do but compare a time while something runs, and nothing at all otherwise
DMEM_CODE void input_poll(void)
{
   if (awake && (int32_t)(MTIME_LO - wake_mt) >= 0)
      input_run();
}
