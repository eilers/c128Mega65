// The core's part of the Shell: the callbacks of QNICE's m2m-rom.asm, written in
// CORE/firmware/core.c. Groups are config.vhd's OPTM_G_* ids (build/fw/config_ids.h).
#pragma once
#include <stdint.h>

// What the file browser is for (CTX_* in QNICE's sysdef.asm), and the situations of
// core_custom_msg (CMSG_*)
#define CTX_MOUNT_DISKIMG  0x0100
#define CTX_LOAD_ROM       0x0200
#define CMSG_BROWSENOTHING 1

// The submenu headline at menu index idx: a whole new line, or NULL for the standard one
const char *core_submenu_summary(int idx);
// What drive's line shows in place of its image: NULL for the standard text
const char *core_mount_label(int drive);
// Called before and after the Shell handles a selection in the menu
void        core_sel_pre(int group, int item);
void        core_sel_post(int group, int item);
// File browser: 1 hides a file (name in upper case; folders are "<NAME>")
int         core_filter_file(const char *name, int dir, int ctx, int group);
// A file about to be loaded: 0, or an error code and *msg. *type: the image type.
int         core_prep_load(uint32_t size, int ctx, int group, int *type, const char **msg);
// A message for a situation (CMSG_*) of the browser for a menu group, or NULL for the standard one
const char *core_custom_msg(int situation, int ctx, int group);
// A menu item of the core's own type (C_CRTROMTYPE_CORE; dev is its second word in
// C_CRTROMS_MAN): load the file picked for it, after core_prep_load, with roms.h's
// crtrom_progress for the bar: 0, or an error code and *msg. And whether it holds a file.
int         core_crtrom_load(int dev, const char *path, const char **msg);
int         core_crtrom_loaded(int dev);
// Called right before the core leaves reset: NULL, or an error message
const char *core_prep_start(void);
// The key that opens the menu: M2M_KEY_HELP, or another one for a core that uses Help
uint32_t    core_menu_key(void);
// A line with a "%s" that isn't a drive's, a CRT/ROM's or a submenu's: its text, or NULL
const char *core_item_text(int group);
// A text item, typed in the menu: 1 and its text now (at most *max characters) for one
int         core_text_item(int group, char *text, int *max);
// The text typed for it: NULL once taken, or why not
const char *core_text_done(int group, const char *text);
// 1 when lines the core gives (a submenu's summary, core_item_text) changed by themselves
int         core_menu_stale(void);
// Called from the main loop, for the core's own modules; must not wait
void        core_poll(void);
// The core's network, from the main loop after core_poll; must not wait
void        core_net(void);
// Called during long disk work, about every sector, for what must not wait long (the
// network); must not touch the card
void        core_idle(void);

// What the Shell offers the core
int  shell_group(int idx);                    // menu item idx's group (0: text, line), or -1
int  shell_item(int group, int pos);          // menu index of a group's item, or -1
int  shell_setting(int group, int pos);       // that item's setting bit (M2M$GET_SETTING)
void shell_force(int group, int pos, int on); // M2M$FORCE_MENU
// A disk image (a path on the card) into drive d, or out of it, as the menu does it: 0,
// or drive_mount's error and maybe *msg; -1 for a drive the menu doesn't have
int  shell_mount(int d, const char *path, const char **msg);
void shell_unmount(int d);
// The file of a CRT/ROM item of the group, taken without the browser (LOAD_CONFIG's REU
// image): its name on the item's line, or NULL for none
void shell_crtrom_named(int group, const char *path);
void shell_open_menu(void);                   // as the Help key does, unless it is open
void shell_menu_button(void);                 // opens or closes it (REST's menu_button)
int  shell_menu_screen(uint8_t *out);         // REST's menu_screen: 2000 bytes, or 0 if closed
int  shell_save_settings(void);               // the menu's settings into their file now: 0, or -1
int  shell_saved_setting(int group, int pos); // an item's bit in the file (the default without one)
