// HDMI Frame Sync in the menu (frame_sync.h). The hardware side: mega65.vhd's raw modes and
// digital_pipeline.vhd's genlock. Settings files and REST go through stores_frame_sync_fix.
#include <stdint.h>
#include "config_ids.h"
#include "console.h"
#include "core.h"
#include "fmt.h"
#include "frame_sync.h"
#include "m2m_rv.h"
#include "screen.h"
#include "stores.h"

#define ASK_S 10                   // seconds until a raw picture nobody kept goes back

static int      was_mode, was_fs;  // the HDMI mode and Frame Sync before the selection
static int      asking;
static uint32_t asked_at;          // MTIME_LO when the question came up
static int      shown_s;           // the seconds left that the screen shows

void fs_sel_pre(int group)
{
   if (group == OPTM_G_HDMI || group == OPTM_G_FF) {
      was_mode = stores_menu_pos(OPTM_G_HDMI, HM_COUNT);
      was_fs   = stores_menu_pos(OPTM_G_FF, FS_COUNT);
   }
}

static void show_seconds(int s)
{
   char line[40];
   fmt_snprintf(line, sizeof line, "Going back in %d s.  ", s);
   scr_print(3, 12, 3, line);
   shown_s = s;
}

static void ask(void)
{
   asking   = 1;
   asked_at = MTIME_LO;
   shell_app_start();
   scr_clear();
   scr_frame(0, 0, OSM_DX, OSM_DY);
   scr_print(3, 2, 3, "HDMI Frame Sync: Raw 50.1 Hz\n\n"
                      "HDMI now runs at the C128's own\n"
                      "frame rate. Not every display\n"
                      "can show that.\n\n"
                      "Space:    keep it\n"
                      "Run/Stop: go back now");
   show_seconds(ASK_S);
}

void fs_sel_post(int group)
{
   if (group != OPTM_G_HDMI && group != OPTM_G_FF)
      return;
   int mode = stores_menu_pos(OPTM_G_HDMI, HM_COUNT);
   int fs   = stores_menu_pos(OPTM_G_FF, FS_COUNT);
   if (!stores_hdmi_50(mode) && fs != FS_OFF) {
      if (group == OPTM_G_FF) {
         // Flicker-free or Raw picked with a 60 Hz mode: the nearest 50 Hz one
         mode = mode == HM_720P60 ? HM_720P50 : HM_576P43;
         shell_force(OPTM_G_HDMI, mode, 1);
      } else {
         // A 60 Hz mode picked: Frame Sync can't follow the C128 there
         fs = FS_OFF;
         shell_force(OPTM_G_FF, FS_OFF, 1);
      }
   }
   if (fs == FS_RAW && (was_fs != FS_RAW || was_mode != mode))
      ask();
}

int fs_asking(void)
{
   return asking;
}

int fs_key(uint32_t key)
{
   uint32_t t = MTIME_LO - asked_at;
   if (key == M2M_KEY_SPACE) {
      asking = 0;
      return APP_MENU;
   }
   if (key == M2M_KEY_RUNSTOP || t >= ASK_S * CPU_HZ) {
      shell_force(OPTM_G_HDMI, was_mode, 1);
      shell_force(OPTM_G_FF, was_fs, 1);
      asking = 0;
      return APP_MENU;
   }
   int s = ASK_S - (int)(t / CPU_HZ);
   if (s != shown_s)
      show_seconds(s);
   return APP_BUSY;
}
