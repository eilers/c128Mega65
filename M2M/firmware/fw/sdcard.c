// The SD card's file system. sdmux.vhd switches to the back slot whenever it holds a
// card, so any change of the card bits can mean a different card: the mount is
// dropped, and made again once the new card has settled. A mount is tried even with no
// card seen, because an R3 can't see a card in its bottom tray.
#include "sdcard.h"
#include "dmem.h"
#include "m2m_rv.h"

#define CARD_BITS (M2M_CSR_SD_ACTIVE | M2M_CSR_SD_DET_INT | M2M_CSR_SD_DET_EXT)
#define PRESENT   (M2M_CSR_SD_DET_INT | M2M_CSR_SD_DET_EXT)
#define SETTLE    (CPU_HZ / 2)

static FATFS             fs;
static enum sdcard_state state = SDCARD_WAIT;
static FRESULT           result = FR_NOT_READY;
static uint32_t          cards;
static uint32_t          changed_at;
static uint32_t          changes;
static int               started;

static void set_state(enum sdcard_state s)
{
   state = s;
   changes++;
}

DMEM_CODE void sdcard_poll(void)
{
   uint32_t now = M2M_CSR & CARD_BITS;
   if (!started) {
      // The boot ROM has just read this card: mount it now
      started = 1;
      cards = now;
      changed_at = MTIME_LO - SETTLE;
      set_state(SDCARD_WAIT);
   }
   if (now != cards) {
      cards = now;
      changed_at = MTIME_LO;
      f_unmount("");
      set_state(SDCARD_WAIT);
   } else if (state == SDCARD_WAIT && MTIME_LO - changed_at >= SETTLE) {
      result = f_mount(&fs, "", 1);
      if (result == FR_OK)
         set_state(SDCARD_OK);
      else
         set_state(cards & PRESENT ? SDCARD_FAIL : SDCARD_NONE);
   }
}

void sdcard_retry(void)
{
   f_unmount("");
   changed_at = MTIME_LO - SETTLE;
   set_state(SDCARD_WAIT);
}

enum sdcard_state sdcard_state(void)
{
   return state;
}

FRESULT sdcard_result(void)
{
   return result;
}

uint32_t sdcard_changes(void)
{
   return changes;
}

const char *sdcard_slot(void)
{
   return cards & M2M_CSR_SD_ACTIVE ? "back slot" : "bottom tray";
}
