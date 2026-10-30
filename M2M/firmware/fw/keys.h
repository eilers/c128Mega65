// The menu keys (M2M_KEY_*): each press counts once, Up and Down repeat while held. And
// typing from every key.
#pragma once
#include <stdint.h>

void     keys_poll(void);
uint32_t keys_get(void);   // the next key pressed (one M2M_KEY_* bit), or 0

// Typing: keys_text starts it (keys held then don't count); keys_char gives the next
// character typed, one of the KEY_* below, or 0. Return and Run/Stop come from keys_get.
#define KEY_DEL    8
#define KEY_CLR    12
#define KEY_ESC    27       // Esc
void     keys_text(void);
int      keys_char(void);
