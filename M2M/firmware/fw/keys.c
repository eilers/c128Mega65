// The menu keys, as QNICE's M2M/rom/keyboard.asm reads them: a key has to be let go
// before it counts again; Up and Down repeat after half a second, 12 times a second.
#include "keys.h"
#include "m2m_rv.h"

#define REPEAT_KEYS  (M2M_KEY_UP | M2M_KEY_DOWN)
#define REPEAT_DELAY (CPU_HZ / 2)
#define REPEAT_EVERY (CPU_HZ / 12)

static uint32_t held;          // high active
static uint32_t pending;       // pressed and not taken yet
static uint32_t repeat_at;

void keys_poll(void)
{
   uint32_t now = ~M2M_KEYS & 0xFFFF;
   uint32_t t = MTIME_LO;
   uint32_t rep = now & REPEAT_KEYS;

   pending |= now & ~held;
   if (rep & ~held)
      repeat_at = t + REPEAT_DELAY;
   else if (rep && (int32_t)(t - repeat_at) >= 0) {
      pending |= rep;
      repeat_at = t + REPEAT_EVERY;
   }
   held = now;
}

uint32_t keys_get(void)
{
   uint32_t k = pending & -pending;
   pending &= ~k;
   return k;
}
