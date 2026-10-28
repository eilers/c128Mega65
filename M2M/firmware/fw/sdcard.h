// The SD card's file system: mounted when a card is in, dropped when the cards change.
#pragma once
#include <stdint.h>
#include "ff.h"

enum sdcard_state {
   SDCARD_NONE,      // no card in either slot
   SDCARD_WAIT,      // a card went in; mounted half a second later
   SDCARD_OK,        // mounted
   SDCARD_FAIL       // couldn't mount: sdcard_result() says why
};

void              sdcard_poll(void);
void              sdcard_retry(void);    // mount again
enum sdcard_state sdcard_state(void);
FRESULT           sdcard_result(void);   // of the last mount
uint32_t          sdcard_changes(void);  // counts every change of state
const char       *sdcard_slot(void);     // the card in use: "back slot" or "bottom tray"
