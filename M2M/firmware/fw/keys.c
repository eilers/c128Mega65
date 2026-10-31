// The menu keys, as QNICE's keyboard.asm read them: a key has to be let go
// before it counts again; Up and Down repeat after half a second, 12 times a second.
// And typing, from every key (M2M_MATRIX): characters as the keys show them, held ones
// repeating the same way.
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

// --- typing -------------------------------------------------------------------------------

#define KEYS      80
#define K_SHIFT_L 15
#define K_SHIFT_R 52

// MEGA65 key numbers (m2m_keyb.vhd's key_num, the C64's matrix) to ASCII, unshifted and
// shifted, as the keycaps show them; 0 for none. Not Return and Run/Stop: they are menu keys
// (keys_get), and the two paths can see one press a round apart.
static const char plain[KEYS] = {
   [0] = KEY_DEL, [6] = KEY_F5, [51] = KEY_CLR, [71] = KEY_ESC,
   [8] = '3', [9] = 'w', [10] = 'a', [11] = '4', [12] = 'z', [13] = 's', [14] = 'e',
   [16] = '5', [17] = 'r', [18] = 'd', [19] = '6', [20] = 'c', [21] = 'f', [22] = 't', [23] = 'x',
   [24] = '7', [25] = 'y', [26] = 'g', [27] = '8', [28] = 'b', [29] = 'h', [30] = 'u', [31] = 'v',
   [32] = '9', [33] = 'i', [34] = 'j', [35] = '0', [36] = 'm', [37] = 'k', [38] = 'o', [39] = 'n',
   [40] = '+', [41] = 'p', [42] = 'l', [43] = '-', [44] = '.', [45] = ':', [46] = '@', [47] = ',',
   [48] = '\\', [49] = '*', [50] = ';', [53] = '=', [54] = '^', [55] = '/', [56] = '1', [57] = '_',
   [59] = '2', [60] = ' ', [62] = 'q',
};
static const char shifted[KEYS] = {
   [0] = KEY_DEL, [51] = KEY_CLR, [71] = KEY_ESC,
   [8] = '#', [9] = 'W', [10] = 'A', [11] = '$', [12] = 'Z', [13] = 'S', [14] = 'E',
   [16] = '%', [17] = 'R', [18] = 'D', [19] = '&', [20] = 'C', [21] = 'F', [22] = 'T', [23] = 'X',
   [24] = '\'', [25] = 'Y', [26] = 'G', [27] = '(', [28] = 'B', [29] = 'H', [30] = 'U', [31] = 'V',
   [32] = ')', [33] = 'I', [34] = 'J', [35] = '0', [36] = 'M', [37] = 'K', [38] = 'O', [39] = 'N',
   [40] = '+', [41] = 'P', [42] = 'L', [43] = '-', [44] = '>', [45] = '[', [46] = '@', [47] = '<',
   [48] = '|', [49] = '*', [50] = ']', [53] = '=', [54] = '~', [55] = '?', [56] = '!', [57] = '`',
   [59] = '"', [60] = ' ', [62] = 'Q',
};

static uint32_t down[3];       // keys held, high active, as last seen
static int      rep_key = -1;  // the key that repeats while held
static uint32_t rep_at;

static int held_now(const uint32_t *m, int k)
{
   return m[k / 32] >> (k % 32) & 1;
}

void keys_text(void)
{
   for (int i = 0; i < 3; i++)
      down[i] = ~M2M_MATRIX(i);
   rep_key = -1;
}

int keys_char(void)
{
   uint32_t now[3];
   uint32_t t = MTIME_LO;
   int key = -1;
   for (int i = 0; i < 3; i++)
      now[i] = ~M2M_MATRIX(i);
   now[2] &= 0xFFFF;
   for (int k = 0; k < KEYS && key < 0; k++)
      if (held_now(now, k) && !held_now(down, k) && plain[k])
         key = k;
   if (key >= 0) {
      rep_key = key;
      rep_at = t + REPEAT_DELAY;
   } else if (rep_key >= 0 && held_now(now, rep_key) && (int32_t)(t - rep_at) >= 0) {
      key = rep_key;
      rep_at = t + REPEAT_EVERY;
   } else if (rep_key >= 0 && !held_now(now, rep_key)) {
      rep_key = -1;
   }
   for (int i = 0; i < 3; i++)
      down[i] = now[i];
   if (key < 0)
      return 0;
   return held_now(now, K_SHIFT_L) || held_now(now, K_SHIFT_R) ? shifted[key] : plain[key];
}
