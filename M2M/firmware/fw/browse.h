// The Shell's file browser (QNICE's selectfile.asm and dirbrowse.asm): a folder at a
// time, sorted with the folders first, filtered by the core, on the whole screen.
#pragma once
#include <stdint.h>

enum browse_result {
   BROWSE_BUSY,       // still browsing
   BROWSE_FILE,       // a file was chosen: browse_path()
   BROWSE_CANCEL,     // Run/Stop
   BROWSE_CHANGED,    // the SD card changed: browse again once it is mounted
   BROWSE_NOTHING     // nothing to show at all; the message has been shown
};

// Start browsing for the menu item of group (ctx: CTX_* in core.h). A new caller starts
// in config.vhd's start folder; the same caller again goes on where it left off.
void               browse_open(int ctx, int group, int caller);
void               browse_reset(void);          // forget where we were (FB_RE_INIT)
enum browse_result browse_key(uint32_t key);    // a key, or 0; polled while browsing
const char        *browse_path(void);           // the chosen file, its whole path
const char        *browse_name(void);           // ditto, its name
