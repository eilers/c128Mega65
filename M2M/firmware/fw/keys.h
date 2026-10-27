// The menu keys (M2M_KEY_*): each press counts once, Up and Down repeat while held.
#pragma once
#include <stdint.h>

void     keys_poll(void);
uint32_t keys_get(void);   // the next key pressed (one M2M_KEY_* bit), or 0
