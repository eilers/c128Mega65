// The REST API's keys, joysticks and mouse lines as the C128 sees them (input.c), on the
// device C_DEV_INPUT (CORE/vhdl/input_inject.vhd). Gideon's Keyboard_USB rest* and
// JoystickOutput, for net/rest_input.c.
#pragma once
#include <stdbool.h>
#include <stdint.h>

// A set of keys: rows 0-7 the C64 matrix (row = PA line, bit = PB line, his row and col),
// 8-10 the C128's K0-K2 (bit = PB line), 11 the locking keys
#define IN_ROWS       12
#define IN_LOCK_4080  0x01         // row 11
#define IN_LOCK_CAPS  0x02

// Where keys go: the C128, or the menu while it is open (keys.c reads them there)
enum { IN_LIVE, IN_MENU };

// His restPressMatrix, restReleaseMatrix, restQueueTap (0: the queue is full, try again
// later) and restSnapshot: held and tapped right now
void    input_press(int to, const uint8_t m[IN_ROWS]);
void    input_release(int to, const uint8_t m[IN_ROWS]);
bool    input_tap(int to, const uint8_t m[IN_ROWS], bool restore);
void    input_snapshot(int to, uint8_t m[IN_ROWS], bool *restore);
// Taps of exactly these keys still waiting in the C128's queue: how many, and dropping them
int     input_tap_count(const uint8_t m[IN_ROWS]);
void    input_tap_remove(const uint8_t m[IN_ROWS]);
// Only what is held, as his menu state's snapshot
void    input_held(int to, uint8_t m[IN_ROWS]);
// Whether the menu has the keyboard; a change drops what the menu held
bool    input_menu_sync(void);
void    input_release_menu(void);   // what the menu holds let go

// Joysticks: bits up 0, down 1, left 2, right 3, fire 4, fire2 5, fire3 6, low active as his
#define IN_JOY_ALL 0x7F
uint8_t input_joy_persistent(int port);                 // port 1 or 2
void    input_joy_set(int port, uint8_t active_low);
void    input_joy_tap(int port, uint8_t mask);          // held for one tick
void    input_joy_cancel(int port, uint8_t mask);
uint8_t input_joy_snapshot(int port);                   // persistent and tapped

// Everything the API holds on the keyboard and the joysticks let go (his restReleaseAll and
// releaseAllRest); the mouse is net/rest_input.c's
void    input_release_all(void);

// The mouse on port 1: its buttons as joystick lines (low active, fire 0x10 left, up 0x01
// right, down 0x02 middle), and its position on the POT lines, paced as his MousePotPacer
void    input_mouse_lines(uint8_t active_low);
void    input_mouse_position(int16_t x, int16_t y);
void    input_mouse_off(void);
bool    input_mouse_behind(void);

// The debug register at $D7FF
int     input_debug_get(void);
int     input_debug_set(int value);    // what it reads back

// The milliseconds of his timers, from the main loop
uint32_t input_now(void);
// From core_poll: cheap unless something is due
void    input_poll(void);
// Wakes the poll at once: something to time was just started
void    input_wake(void);
