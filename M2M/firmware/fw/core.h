// The core's part of the Shell: the callbacks of CORE/m2m-rom/m2m-rom.asm, written in
// CORE/firmware/core.c. Groups are config.vhd's OPTM_G_* ids (build/fw/config_ids.h).
#pragma once
#include <stdint.h>

// The submenu headline at menu index idx: a whole new line, or NULL for the standard one
const char *core_submenu_summary(int idx);
// What drive's line shows in place of its image: NULL for the standard text
const char *core_mount_label(int drive);
// Called before and after the Shell handles a selection in the menu
void        core_sel_pre(int group, int item);
void        core_sel_post(int group, int item);
// Called right before the core leaves reset: NULL, or an error message
const char *core_prep_start(void);
// The key that opens the menu: M2M_KEY_HELP, or another one for a core that uses Help
uint32_t    core_menu_key(void);

// What the Shell offers the core
int  shell_item(int group, int pos);          // menu index of a group's item, or -1
int  shell_setting(int group, int pos);       // that item's setting bit (M2M$GET_SETTING)
void shell_force(int group, int pos, int on); // M2M$FORCE_MENU
