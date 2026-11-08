// The REST API's mouse (mouse.c): Gideon's RestMouseQueue and his USB mouse handling
// (UsbHidDriver's process_mouse_report for the REST mouse), onto input.c's port 1 lines.
#pragma once
#include <stdbool.h>
#include <stdint.h>

#define MOUSE_QUEUE   1024         // RestMouseQueue::CAPACITY
#define MOUSE_TAP_MS  40           // REST_MOUSE_TAP_HOLD_TICKS: two PAL frames

bool    mouse_attached(void);
void    mouse_attach(void);        // his restMouseAttach and an empty queue
void    mouse_detach(void);        // his release_rest_mouse: the queue dropped, port 1 a port
uint8_t mouse_buttons(void);       // his restMouseButtons: 1 left, 2 right, 4 middle
int     mouse_pending(void);
int     mouse_room(void);
uint8_t mouse_tail_buttons(void);  // the buttons as the last queued report leaves them
// A report: buttons, motion, wheel (away from the user) and pan (right), sent wait_ms after
// the one before at the earliest, and never sooner than 20 ms after it
void    mouse_push(uint8_t buttons, int dx, int dy, int wheel, int pan, int wait_ms);
// A request starts reports: an idle queue's first one goes out at once
void    mouse_start(void);
// From input.c's timing: reports and wheel pulses that are due; whether there is more
bool    mouse_run(uint32_t now);
