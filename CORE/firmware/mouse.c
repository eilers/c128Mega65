// The REST API's mouse: Gideon's RestMouseQueue (reports in order, no faster than a USB
// mouse is polled, every 20 ms) and his handling of a USB mouse's report, as the REST mouse
// goes through it (UsbHidDriver::process_mouse_report and poll, HidMouseInterpreter): Mouse
// Mode, Sensitivity, Acceleration, Wheel Sensitivity and Direction from his U64 Specific
// Settings. Its buttons and position go to port 1 (input.c). His menu's mouse navigation
// is left out: the core's menu takes no mouse.
// Ported from the 1541 Ultimate firmware (github.com/GideonZ/1541ultimate), Copyright
// Gideon Zweijtzer and contributors, GPL v3 as this core; changed for c128Mega65 in 2026.
#include <stdlib.h>
#include <string.h>
#include "heap.h"
#include "input.h"
#include "mouse.h"
#include "stores.h"

#define TICK_MS        5           // his FreeRTOS tick (configTICK_RATE_HZ 200)
#define REPORT_TICKS   4           // REST_MOUSE_REPORT_TICKS: 20 ms
#define IDLE_TICKS     400         // REST_MOUSE_IDLE_TICKS: 2 s
#define PULSE_TICKS    10          // USB_HID_MOUSE_WHEEL_PULSE_TICKS: 50 ms
#define BURST_LIMIT    16          // USB_HID_MOUSE_WHEEL_BURST_LIMIT
#define CURSOR_PENDING 16          // USB_HID_CURSOR_MAX_PENDING_KEYS
#define POT_PER_STEP   2           // MOUSE_POT_COUNTS_PER_STEP

enum { MODE_CURSOR, MODE_MOUSE, MODE_MOUSE_CURSOR, MODE_MOUSE_WHEEL };
enum { PHASE_IDLE, PHASE_LOW, PHASE_HIGH };

typedef struct {
   uint8_t  buttons;
   int8_t   dx, dy, wheel, pan;
   uint16_t wait;                  // ticks after the previous report
} report_t;

static report_t *queue;            // HyperRAM
static int       head, count;
static uint8_t   tail_buttons;
static uint32_t  last_report;      // the tick the last report went out

// UsbHidDriver's state for the REST mouse
static bool    attached;
static uint8_t mouse_joy = 0x1F;
static int16_t mouse_x, mouse_y;
static int     sens_setting = -1, sens_rem_x, sens_rem_y;
static int     accel_ema_x16, accel_scale = 384;   // ADAPTIVE_ACCELERATION_FALLBACK_SCALE
static int     cursor_rem_x, cursor_rem_y;
static int     axis_rem_h, axis_rem_v, key_rem_h, key_rem_v;
static int     wheel_acc;
static int     pulse_phase, pulse_dir;
static uint8_t pulse_mask, pulse_count, base_joy = 0x1F;
static uint32_t pulse_next;
static bool    pulse_active;

static uint32_t ticks(uint32_t now)
{
   return now / TICK_MS;
}

// --- RestMouseQueue -----------------------------------------------------------------------

bool mouse_attached(void) { return attached; }
int  mouse_pending(void)  { return count; }
int  mouse_room(void)     { return MOUSE_QUEUE - count; }
uint8_t mouse_tail_buttons(void) { return tail_buttons; }

void mouse_push(uint8_t buttons, int dx, int dy, int wheel, int pan, int wait_ms)
{
   if (count >= MOUSE_QUEUE)       // the caller checked the room
      return;
   if (!queue)
      queue = heap_alloc(MOUSE_QUEUE * sizeof *queue);
   report_t *r = &queue[(head + count) % MOUSE_QUEUE];
   r->buttons = buttons;
   r->dx = dx;
   r->dy = dy;
   r->wheel = wheel;
   r->pan = pan;
   int t = (wait_ms + TICK_MS - 1) / TICK_MS;      // his ticksFor, 0 stays 0
   r->wait = t;
   count++;
   tail_buttons = buttons;
}

static void queue_clear(uint8_t buttons)
{
   head = count = 0;
   tail_buttons = buttons;
}

// His apply_mouse_event, before the append: an idle queue's first report is due at once,
// unless the last one went out less than 20 ms ago
void mouse_start(void)
{
   uint32_t now = ticks(input_now());
   if (now - last_report > IDLE_TICKS)
      last_report = now - IDLE_TICKS;
   input_wake();
}

// --- the C64 side -------------------------------------------------------------------------

static void joy1(uint8_t active_low)
{
   input_mouse_lines(active_low);
}

static void set_position(void)
{
   input_mouse_position(mouse_x * POT_PER_STEP, mouse_y * POT_PER_STEP);
}

uint8_t mouse_buttons(void)
{
   uint8_t b = 0;
   if (!(mouse_joy & 0x10))
      b |= 0x01;
   if (!(mouse_joy & 0x01))
      b |= 0x02;
   if (!(mouse_joy & 0x02))
      b |= 0x04;
   return b;
}

static void reset_state(void)
{
   mouse_joy = base_joy = 0x1F;
   pulse_phase = PHASE_IDLE;
   pulse_mask = pulse_count = 0;
   pulse_dir = 0;
   pulse_active = false;
   sens_setting = -1;
   sens_rem_x = sens_rem_y = cursor_rem_x = cursor_rem_y = 0;
   axis_rem_h = axis_rem_v = key_rem_h = key_rem_v = wheel_acc = 0;
   accel_ema_x16 = 0;
   accel_scale = 384;
}

void mouse_attach(void)
{
   if (attached)
      return;
   attached = true;
   reset_state();
   joy1(0x1F);                     // the POT lines are the mouse's now
   queue_clear(0);
}

// His release_rest_mouse and disable(): the reports dropped, port 1 a joystick port again
void mouse_detach(void)
{
   queue_clear(0);
   if (!attached)
      return;
   attached = false;
   reset_state();
   input_mouse_off();
}

// --- HidMouseInterpreter ------------------------------------------------------------------

static int clamp(int v, int limit)
{
   return v > limit ? limit : v < -limit ? -limit : v;
}

static int clamp16(int v)
{
   return v < 1 ? 1 : v > 16 ? 16 : v;
}

static int scale_rem(int delta, int mul, int div, int *rem)
{
   int scaled = delta * mul + *rem;
   int d = scaled / div;
   *rem = scaled % div;
   return d;
}

static int div_rounded(int v, int d)
{
   return v >= 0 ? (v + d / 2) / d : -((-v + d / 2) / d);
}

static int adaptive_scale(int ema_x16)
{
   int target = 32 << 4, max = 96 << 4;          // ADAPTIVE_ACCELERATION_TARGET/MAX_EMA
   if (ema_x16 <= target)
      return 384;
   if (ema_x16 >= max)
      return 576;
   int excess = ema_x16 - target, range = max - target;
   int s = 384 + (576 - 384) * excess * excess / (range * range);
   return s < 384 ? 384 : s > 576 ? 576 : s;
}

// One cursor key per 4 counts, the rest carried; a turn drops it (scaleCursorMotionKeys)
static int cursor_keys(int motion, int *rem)
{
   if (motion && (motion > 0 ? *rem < 0 : *rem > 0))
      *rem = 0;
   int total = *rem + motion;
   int keys = total / 4;
   *rem = total - keys * 4;
   return clamp(keys, 63);
}

// The cursor keys a USB mouse types (usb_hid_queue_key): into the C128's tap queue, at most
// 16 waiting each way; a turn drops the ones still waiting the other way
enum { CK_UP, CK_DOWN, CK_LEFT, CK_RIGHT };

static void cursor_matrix(int k, uint8_t m[IN_ROWS])
{
   memset(m, 0, IN_ROWS);
   m[0] = k == CK_UP || k == CK_DOWN ? 0x80 : 0x04;   // cursor_up_down, cursor_left_right
   if (k == CK_UP || k == CK_LEFT)
      m[1] = 0x80;                                     // left_shift
}

static void queue_key(int k, int repeat)
{
   uint8_t m[IN_ROWS], o[IN_ROWS];
   if (repeat <= 0)
      return;
   cursor_matrix(k ^ 1, o);
   input_tap_remove(o);
   cursor_matrix(k, m);
   int room = CURSOR_PENDING - input_tap_count(m);
   for (int i = 0; i < repeat && i < room; i++)
      input_tap(IN_LIVE, m, false);
}

static void queue_pair(int up_amount, int right_amount)
{
   if (up_amount > 0)
      queue_key(CK_UP, up_amount);
   else if (up_amount < 0)
      queue_key(CK_DOWN, -up_amount);
   if (right_amount > 0)
      queue_key(CK_RIGHT, right_amount);
   else if (right_amount < 0)
      queue_key(CK_LEFT, -right_amount);
}

// --- the wheel's pulses (Mouse + Wheel) ---------------------------------------------------

static uint8_t pulse_out(uint8_t joy)
{
   joy |= 0x0C;
   if (pulse_phase == PHASE_LOW && pulse_mask)
      joy &= ~pulse_mask;
   return joy;
}

// advanceNativeWheelBurst: whether a pulse is under way
static bool pulse_advance(uint32_t now)
{
   for (;;) {
      if (pulse_phase == PHASE_IDLE) {
         if (pulse_count && pulse_dir) {
            pulse_mask = pulse_dir > 0 ? 0x04 : 0x08;
            pulse_count--;
            pulse_phase = PHASE_LOW;
            pulse_next = now + PULSE_TICKS;
            return true;
         }
         pulse_dir = 0;
         pulse_mask = 0;
         return false;
      }
      if ((int32_t)(now - pulse_next) < 0)
         return true;
      if (pulse_phase == PHASE_LOW) {
         pulse_phase = PHASE_HIGH;
         pulse_next += PULSE_TICKS;
         continue;
      }
      pulse_phase = PHASE_IDLE;
   }
}

// service_native_wheel_timer
static void pulse_service(uint32_t now)
{
   uint8_t out = base_joy | 0x0C;
   if (stores_mouse(MS_MODE) != MODE_MOUSE_WHEEL) {
      pulse_phase = PHASE_IDLE;
      pulse_mask = pulse_count = 0;
      pulse_dir = wheel_acc = 0;
   } else if (pulse_phase != PHASE_IDLE || pulse_count) {
      pulse_advance(now);
      out = pulse_out(out);
   }
   pulse_active = pulse_phase != PHASE_IDLE || pulse_count;
   joy1(out);
}

// --- a report: process_mouse_report, then poll ---------------------------------------------

static void report(const report_t *r, uint32_t now)
{
   int mode = stores_mouse(MS_MODE);
   int sens = stores_mouse(MS_SENS);
   bool to_cursor = mode == MODE_CURSOR, to_pointer = mode != MODE_CURSOR;
   bool wheel_cursor = mode == MODE_CURSOR || mode == MODE_MOUSE_CURSOR;
   bool native = mode == MODE_MOUSE_WHEEL;

   mouse_joy = r->buttons & 1 ? mouse_joy & ~0x10 : mouse_joy | 0x10;
   mouse_joy = r->buttons & 2 ? mouse_joy & ~0x01 : mouse_joy | 0x01;
   mouse_joy = r->buttons & 4 ? mouse_joy & ~0x02 : mouse_joy | 0x02;

   // Sensitivity: 32nds of a count per setting step, the rest carried; then acceleration
   int mx = r->dx, my = r->dy;
   if (sens_setting != sens) {
      sens_rem_x = sens_rem_y = 0;
      sens_setting = sens;
   }
   mx = clamp(scale_rem(r->dx, clamp16(sens) * 32, 256, &sens_rem_x), 63);
   my = clamp(scale_rem(r->dy, clamp16(sens) * 32, 256, &sens_rem_y), 63);
   if (stores_mouse(MS_ACCEL) == 1) {
      int mag_x16 = (abs(r->dx) + abs(r->dy)) << 4;
      accel_ema_x16 += (mag_x16 - accel_ema_x16) / 4;
      int next = adaptive_scale(accel_ema_x16);
      accel_scale = next > accel_scale + 32 ? accel_scale + 32 :
                    next < accel_scale - 32 ? accel_scale - 32 : next;
      mx = clamp(mx * accel_scale / 256, 63);
      my = clamp(my * accel_scale / 256, 63);
   } else {
      accel_ema_x16 = 0;
      accel_scale = 384;
   }
   if (to_pointer) {
      mouse_x += mx;
      mouse_y -= my;
   }

   // The wheel: the REST report isn't a compact shared one, so the pan is inverted
   bool reversed = stores_mouse(MS_WHEEL_DIR) == 1;
   int sf = stores_mouse(MS_WHEEL);
   uint8_t out = mouse_joy;
   int wh = -r->pan;
   int wv = r->wheel == 1 ? 8 : r->wheel == -1 ? -8 : r->wheel;
   int wh_keys = reversed ? -wh : wh, wv_keys = reversed ? -wv : wv;

   if (to_cursor) {
      int h = cursor_keys(mx, &cursor_rem_x), v = cursor_keys(-my, &cursor_rem_y);
      queue_pair(v, h);
   }
   int steps = 0;
   if (wv || wh) {
      if (wheel_cursor) {
         axis_rem_h = axis_rem_v = wheel_acc = 0;
         key_rem_h = 0;
         int h = div_rounded(wh_keys * clamp16(sf) * 3, 2);
         int v = wv_keys ? scale_rem(wv_keys, clamp16(sf), 4, &key_rem_v) : 0;
         queue_pair(v, h);
      } else if (mode == MODE_MOUSE) {
         key_rem_h = key_rem_v = wheel_acc = 0;
         axis_rem_h = 0;
         int h = clamp(wh_keys * clamp16(sf) * 2, 63);
         int v = clamp(-wv_keys ? scale_rem(-wv_keys, clamp16(sf) * 10, 32, &axis_rem_v) : 0, 63);
         mouse_x += h;
         mouse_y -= v;
      } else if (native) {
         axis_rem_h = axis_rem_v = key_rem_h = key_rem_v = 0;
         // accumulateNativeWheelSteps: 8 = one step
         if ((wv > 0 && wheel_acc < 0) || (wv < 0 && wheel_acc > 0))
            wheel_acc = 0;
         wheel_acc += wv * clamp16(sf);
         while (wheel_acc >= 8) {
            wheel_acc -= 8;
            steps++;
         }
         while (wheel_acc <= -8) {
            wheel_acc += 8;
            steps--;
         }
         if (reversed)
            steps = -steps;
      }
   }

   set_position();
   if (native) {
      base_joy = out | 0x0C;
      if (!pulse_active)
         joy1(base_joy);
      // poll(): the steps join the burst, and the pulses start
      if (steps) {
         int dir = steps > 0 ? 1 : -1;
         if (pulse_dir != dir) {
            pulse_dir = dir;
            pulse_count = 0;
         }
         int n = pulse_count + abs(steps);
         pulse_count = n > BURST_LIMIT ? BURST_LIMIT : n;
         pulse_service(now);
      }
   } else {
      if (pulse_phase != PHASE_IDLE || pulse_count || wheel_acc)
         pulse_phase = PHASE_IDLE, pulse_count = 0, pulse_mask = 0, pulse_dir = 0, wheel_acc = 0;
      pulse_active = false;
      base_joy = out | 0x0C;
      joy1(out | 0x0C);
   }
}

// His timer: one report a tick at most, once its wait and 20 ms have passed; the wheel's
// pulses in between
bool mouse_run(uint32_t now_ms)
{
   uint32_t now = ticks(now_ms);
   if (attached && pulse_active && (int32_t)(now - pulse_next) >= 0)
      pulse_service(now);
   if (attached && count) {
      int since = now - last_report;
      report_t *r = &queue[head];
      if (since >= r->wait && since >= REPORT_TICKS) {
         report_t taken = *r;
         head = (head + 1) % MOUSE_QUEUE;
         count--;
         report(&taken, now);
         last_report = now;
      }
   }
   return attached && (count || pulse_active);
}
