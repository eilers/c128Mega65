// The Shell: the start and main loop of M2M/rom/shell.asm, the menu glue and settings
// file of options.asm, the reset and ascal handling of gencfg.asm and the help pages of
// whs.asm. Nothing here waits for long: shell_poll handles one key at a time.
#include <string.h>
#include "shell.h"
#include "browse.h"
#include "cfg.h"
#include "console.h"
#include "core.h"
#include "drives.h"
#include "ff.h"
#include "filters.h"
#include "fmt.h"
#include "heap.h"
#include "keys.h"
#include "m2m_rv.h"
#include "menu.h"
#include "roms.h"
#include "screen.h"
#include "sdcard.h"

#define CSR_KBD_JOY  (M2M_CSR_KBD | M2M_CSR_JOY1 | M2M_CSR_JOY2)
#define WAIT_333MS   (CPU_HZ / 3)    // so the key that closes a screen doesn't reach the core
#define WHS_PAGES    0xFFF           // index of a help page's page count

static char   *items;                // OPTM_ITEMS in HyperRAM, each line ended where its "\n" was
static int     start_idx;            // OPTM_START
static uint8_t is_mount[MENU_MAX];   // drive number + 1, or 0
static uint8_t is_crtrom[MENU_MAX];  // CRT/ROM number + 1, or 0
static uint8_t is_help[MENU_MAX];    // help screen (WHS record) number, or 0

#define CRTROM_MAX   4               // CRT/ROM items in the menu
#define NAME_MAX     32              // a file's name as the menu shows it, cut

static enum { UI_CORE, UI_MENU, UI_HELP, UI_LOAD, UI_CLOSING } ui;
static uint32_t closing_at;
static int      help_idx, help_set, help_page, help_pages;

// Choosing and loading a file for a menu item (HANDLE_MOUNTING)
static struct {
   int      idx, k;                  // the menu item, its drive or CRT/ROM number
   int      drive;                   // 1: a drive's disk image, 0: a CRT/ROM
   uint16_t group;                   // the selection, finished once the file is in
   int      item;
   enum { L_MOUNT, L_MOUNT_ERR, L_BROWSE, L_ERROR } step;
} load;
static uint8_t crt_loaded[CRTROM_MAX];           // CRTROM_MAN_LDF
static char    crt_name[CRTROM_MAX][NAME_MAX];   // shown on its line
static char    drv_name[DRIVES_MAX][NAME_MAX];   // the image in a drive

// The settings file: one byte per menu item, 0 or 1
static struct {
   int      on;                      // good: saved to when settings change
   FIL      f;
   uint32_t card;                    // sdcard_changes() when it was opened
   uint32_t slot;                    // M2M_CSR_SD_ACTIVE then
   uint8_t  old[MENU_MAX];           // the settings when the menu opened
} cfgf;

static void wait(uint32_t t)
{
   uint32_t start = MTIME_LO;
   while (MTIME_LO - start < t)
      keys_poll();
}

void shell_fatal(const char *msg, uint32_t code)
{
   char text[512];
   int n = fmt_snprintf(text, sizeof text, "\nFATAL ERROR:\n\n%s", msg);
   if (code)
      n += fmt_snprintf(text + n, sizeof text - n, "Error code: %04X", (unsigned)code);
   fmt_snprintf(text + n, sizeof text - n, "\n\nCore stopped. Please reset the machine.\n");
   scr_full_on();
   scr_clear();
   scr_print(0, 0, 1, text);
   M2M_HALT();
}

// -------------------------------------------------------------------------------------
// The menu's data, from config.vhd
// -------------------------------------------------------------------------------------

static void load_menu(void)
{
   int n = cfg_get(CFG_OPTM_ICOUNT, 0);
   int mounts = 0, crtroms = 0, helps = 0;
   char *s = items = heap_alloc(4096);

   if (n < 1 || n > MENU_MAX)
      shell_fatal("config.vhd: Illegal menu size (OPTM_SIZE):\nMust be between 1 and 254\n", n);
   cfg_str(CFG_OPTM_ITEMS, items, 4096);
   menu.n = n;
   start_idx = -1;
   for (int i = 0; i < n; i++) {
      char *nl = strstr(s, "\\n");
      if (!nl)
         shell_fatal("config.vhd: Each line in OPTM_ITEMS needs to be terminated by a newline character.\n", i);
      *nl = 0;
      menu.line[i]  = s;
      s = nl + 2;
      menu.group[i] = cfg_get(CFG_OPTM_GROUPS, i);
      menu.hline[i] = cfg_get(CFG_OPTM_LINES, i);
      if (cfg_get(CFG_OPTM_MOUNT, i))
         is_mount[i] = ++mounts;
      if (cfg_get(CFG_OPTM_CRTROM, i))
         is_crtrom[i] = ++crtroms;
      if (cfg_get(CFG_OPTM_HELP, i))
         is_help[i] = ++helps;
      if (start_idx < 0 && cfg_get(CFG_OPTM_START, i))
         start_idx = i;
   }
   if (mounts > drives_count())
      shell_fatal("config.vhd: More menu items have the\nattribute OPTM_G_MOUNT_DRV than there are\n"
                  "virtual drives configured in globals.vhd\nusing C_VDNUM.\n", mounts);
   if (crtroms > CRTROM_MAX)
      shell_fatal("Too many CRT/ROM items in config.vhd\n", crtroms);
   if (start_idx < 0)
      shell_fatal("config.vhd: No start menu item tag (OPTM_G_START)\n", 0);
   if (menu_init())
      shell_fatal("menu.asm: One or more submenu is not\nspecified correctly:\nMissing submenu-end-flag.\n", 0);

   // Top right, as SCR$INIT places it
   menu.dx = cfg_get(CFG_OPTM_DIM, 0);
   menu.dy = cfg_get(CFG_OPTM_DIM, 1);
   menu.x  = OSM_DX - menu.dx;
   menu.y  = 0;
   menu.cursor = start_idx;
}

// -------------------------------------------------------------------------------------
// Settings: the settings file if config.vhd asks for it and it's good, else the defaults
// -------------------------------------------------------------------------------------

static int saved_item(int i)
{
   return !is_mount[i] && !is_crtrom[i];   // drives and cartridges aren't saved
}

static void load_settings(void)
{
   char name[64];
   uint8_t b[MENU_MAX];
   UINT got;

   for (int i = 0; i < 8; i++)
      M2M_CONTROL_M(i) = 0;
   for (int i = 0; i < menu.n; i++)
      setting_set(i, cfg_get(CFG_OPTM_STDSEL, i));

   if (cfg_get(CFG_GENERAL, CFG_SAVEOSDCFG) != 1 || sdcard_state() != SDCARD_OK)
      return;
   cfg_str(CFG_CFG_FILE, name, sizeof name);
   if (f_open(&cfgf.f, name, FA_READ | FA_WRITE) != FR_OK)
      return;
   // The right length, and not a new file (0xFF: keep the defaults, save later)
   if (f_size(&cfgf.f) != (FSIZE_t)menu.n || f_read(&cfgf.f, b, menu.n, &got) != FR_OK ||
       got != (UINT)menu.n) {
      f_close(&cfgf.f);
      return;
   }
   cfgf.on   = 1;
   cfgf.card = sdcard_changes();
   cfgf.slot = M2M_CSR & M2M_CSR_SD_ACTIVE;
   if (b[0] == 0xFF)
      return;
   for (int i = 0; i < menu.n; i++) {
      if (b[i] > 1)
         shell_fatal("Corrupt config file.\n", i);
      setting_set(i, b[i]);
   }
}

static void save_settings(void)
{
   uint8_t b[MENU_MAX];
   int changed = 0;
   UINT put;

   if (!cfgf.on)
      return;
   // After a card change the file is opened again, but only in the same slot: a card in
   // the other one is likely another card (ROSM_INTEGRITY)
   if (sdcard_changes() != cfgf.card) {
      char name[64];
      cfg_str(CFG_CFG_FILE, name, sizeof name);
      if ((M2M_CSR & M2M_CSR_SD_ACTIVE) != cfgf.slot || sdcard_state() != SDCARD_OK ||
          f_open(&cfgf.f, name, FA_READ | FA_WRITE) != FR_OK) {
         cfgf.on = 0;
         return;
      }
      cfgf.card = sdcard_changes();
      if (f_size(&cfgf.f) != (FSIZE_t)menu.n) {
         f_close(&cfgf.f);
         cfgf.on = 0;
         return;
      }
   }
   for (int i = 0; i < menu.n; i++) {
      b[i] = saved_item(i) && setting_get(i);
      changed |= saved_item(i) && b[i] != cfgf.old[i];
   }
   if (!changed)
      return;
   if (f_lseek(&cfgf.f, 0) != FR_OK || f_write(&cfgf.f, b, menu.n, &put) != FR_OK ||
       put != (UINT)menu.n || f_sync(&cfgf.f) != FR_OK)
      cfgf.on = 0;
}

// -------------------------------------------------------------------------------------
// Reset, pause and ascal (gencfg.asm)
// -------------------------------------------------------------------------------------

// Keyboard and joysticks on as config.vhd's general settings kb, j1 and j2 say
static void csr_flags(uint32_t csr, int kb, int j1, int j2)
{
   if (cfg_get(CFG_GENERAL, kb))
      csr |= M2M_CSR_KBD;
   if (cfg_get(CFG_GENERAL, j1))
      csr |= M2M_CSR_JOY1;
   if (cfg_get(CFG_GENERAL, j2))
      csr |= M2M_CSR_JOY2;
   M2M_CSR = csr;
}

static void ascal_init(void)
{
   // The CRT emulation's polyphase filters. ascal builds each entry from four writes in a
   // row through one register, so one filter has to be written whole before the other.
   M2M_RAMROM_DEV = M2M_DEV_ASCAL_PPHASE;
   for (int i = 0; i < 256; i++)
      M2M_DEV16[i] = LANCZOS2_12[i];
   for (int i = 0; i < 256; i++)
      M2M_DEV16[0x100 + i] = SCAN_BR_110_80[i];

   M2M_CSR &= ~M2M_CSR_ASCAL_AUTO;
   switch (cfg_get(CFG_GENERAL, CFG_ASCAL_USAGE)) {
   case CFG_AUSE_CFG:
      M2M_ASCAL_MODE = cfg_get(CFG_GENERAL, CFG_ASCAL_MODE);
      break;
   case CFG_AUSE_AUTO:
      M2M_CSR |= M2M_CSR_ASCAL_AUTO;
      break;
   }
}

// -------------------------------------------------------------------------------------
// The menu's callbacks (OPTM_CB_SEL, OPTM_CB_SHOW)
// -------------------------------------------------------------------------------------

// line with its "%s" replaced, cut to max characters with "..." (M2M$RPL_S)
static void replace_s(const char *line, const char *with, char *out, int max)
{
   const char *s = strstr(line, "%s");
   int pre = s - line, n = strlen(with);

   if (pre + n > max) {
      memcpy(out, line, pre);
      memcpy(out + pre, with, max - pre);
      strcpy(out + max - 3, "...");
      return;
   }
   memcpy(out, line, pre);
   strcpy(out + pre, with);
   n += pre;
   strncat(out + n, s + 2, max - n);
}

static void show(int idx, const char *line, char *out)
{
   const int max = menu.dx - 2;
   char with[64];
   const char *custom;

   if (menu.group[idx] & MENU_SUBMENU) {
      if ((custom = core_submenu_summary(idx))) {
         strncpy(out, custom, max);
         out[max] = 0;
         return;
      }
      // The selected item of the submenu's first group
      for (int i = idx + 1; i < menu.n && !(menu.group[i] & MENU_SUBMENU); i++) {
         uint16_t g = menu.group[i];
         if (g >= 1 && g < 255 && menu.sel[i]) {
            const char *l = menu.line[i];
            while (*l == ' ')
               l++;
            replace_s(line, l, out, max);
            return;
         }
      }
      shell_fatal("config.vhd: A submenu with a %s in its headline has no menu group.\n", idx);
   }
   if (is_mount[idx]) {
      int d = is_mount[idx] - 1;
      if (!(custom = core_mount_label(d))) {
         if (drive_mounted(d))
            custom = drv_name[d];
         else {
            cfg_str(CFG_OPTM_MSTR, with, sizeof with);
            custom = with;
         }
      }
      replace_s(line, custom, out, max);
      return;
   }
   if (is_crtrom[idx]) {
      int k = is_crtrom[idx] - 1;
      if (crt_loaded[k])
         replace_s(line, crt_name[k], out, max);
      else {
         cfg_str(CFG_OPTM_CRSTR, with, sizeof with);
         replace_s(line, with, out, max);
      }
      return;
   }
   strcpy(out, line);
}

// A page can be 4 KB, so it is printed in parts; a part never ends in the middle of a "\n"
static void show_help_page(void)
{
   uint32_t sel = CFG_WHS + (help_set << 8) + help_page;
   char part[65];
   int at = 1 << 8 | 1, n;

   help_pages = cfg_get(sel, WHS_PAGES);
   scr_clear();
   scr_frame(0, 0, OSM_DX, OSM_DY);
   for (int from = 0; (n = cfg_str_at(sel, from, part, sizeof part)) > 0; from += n) {
      if (n > 1 && part[n - 1] == '\\')
         part[--n] = 0;
      at = scr_print(at & 0xFF, at >> 8, 1, part);
   }
}

static void load_start(int idx, uint16_t group, int item);

// The settings as the menu has them, and the core's last word (the end of OPTM_CB_SEL)
static void selected(uint16_t group, int item)
{
   if ((group & MENU_ID) != MENU_CLOSE)
      for (int i = 0; i < menu.n; i++)
         setting_set(i, menu.sel[i]);
   core_sel_post(group & MENU_ID, item);
}

static void select(uint16_t group, int item, enum menu_key key)
{
   int idx = menu.cursor;

   core_sel_pre(group & MENU_ID, item);
   // Space on a mounted drive unmounts it; otherwise a drive gets an image, or another
   // one in place of its image
   if (is_mount[idx] && key == MENU_KEY_SELALT && drive_mounted(is_mount[idx] - 1)) {
      drive_unmount(is_mount[idx] - 1);
      menu_set(idx, 0);
      selected(group, item);
      menu_show();
      return;
   }
   if (is_mount[idx] || is_crtrom[idx]) {
      load_start(idx, group, item);   // finished by load_done()
      return;
   }
   if (is_help[idx]) {
      // Help items are never selected: show the pages, and the menu again after
      menu.sel[idx] = 0;
      help_idx   = idx;
      help_set   = is_help[idx];
      help_page  = 0;
      menu.shown = 0;
      scr_full_on();
      show_help_page();
      ui = UI_HELP;
   }
   selected(group, item);
}

// -------------------------------------------------------------------------------------
// Mounting a disk image, loading a cartridge or ROM: the SD card, the file browser, the
// file (HANDLE_MOUNTING)
// -------------------------------------------------------------------------------------

static void frame_full(void)
{
   scr_clear();
   scr_frame(0, 0, OSM_DX, OSM_DY);
}

static void menu_again(void)
{
   menu_show();
   scr_window_on(menu.x, menu.y, menu.dx, menu.dy);
   menu_cursor_on();
   ui = UI_MENU;
}

static void load_done(int on)
{
   menu_set(load.idx, on);
   selected(load.group, load.item);
   menu_again();
}

// Is there an image in the drive, or a file in the CRT/ROM's device
static int load_on(void)
{
   return load.drive ? drive_mounted(load.k) : crt_loaded[load.k];
}

static void browse(void)
{
   browse_open(load.drive ? CTX_MOUNT_DISKIMG : CTX_LOAD_ROM, load.group & MENU_ID,
               load.k << 8 | !load.drive);
   load.step = L_BROWSE;
}

// A file's name for the menu: one more than fits, so that the menu cuts it with "..."
static void keep_name(char *to)
{
   strncpy(to, browse_name(), menu.dx - 1);
   to[menu.dx - 1] = 0;
}

static void load_start(int idx, uint16_t group, int item)
{
   load.idx   = idx;
   load.drive = is_mount[idx] != 0;
   load.k     = load.drive ? is_mount[idx] - 1 : is_crtrom[idx] - 1;
   load.group = group;
   load.item  = item;
   load.step  = L_MOUNT;
   menu.shown = 0;
   frame_full();
   scr_full_on();
   ui = UI_LOAD;
}

static void load_poll(uint32_t key)
{
   char text[400];
   const char *msg;
   int err;

   switch (load.step) {
   case L_MOUNT:
      if (sdcard_state() == SDCARD_WAIT) {
         scr_print(1, 1, 1, "Initializing. Please wait...");
         break;
      }
      frame_full();
      if (sdcard_state() != SDCARD_OK) {
         fmt_snprintf(text, sizeof text, "Error: Cannot mount SD card!\nError code: %04X"
                      "\n\nPress Return to retry", sdcard_result());
         scr_print(1, 1, 1, text);
         load.step = L_MOUNT_ERR;
         break;
      }
      browse();
      break;
   case L_MOUNT_ERR:
      if (key == M2M_KEY_RETURN) {
         sdcard_retry();
         load.step = L_MOUNT;
      }
      break;
   case L_BROWSE:
      switch (browse_key(key)) {
      case BROWSE_BUSY:
         break;
      case BROWSE_FILE: {
         if (load.drive) {
            if (!(err = drive_mount(load.k, browse_path(), load.group & MENU_ID, &msg)))
               keep_name(drv_name[load.k]);
         } else {
            keep_name(crt_name[load.k]);
            err = crtrom_load(load.k, browse_path(), load.group & MENU_ID, &msg);
            crt_loaded[load.k] = crtrom_loaded(load.k);   // as soon as the parser started
         }
         if (!err) {
            load_done(1);
            break;
         }
         frame_full();
         int n = fmt_snprintf(text, sizeof text, "Error code: %04X", err);
         if (msg)
            fmt_snprintf(text + n, sizeof text - n, "\n%s\nPress Space to continue.", msg);
         scr_print(1, 1, 1, text);
         load.step = L_ERROR;
         break;
      }
      case BROWSE_CANCEL:
         load_done(load_on());
         break;
      case BROWSE_NOTHING:
         browse_reset();
         M2M_CSR &= ~M2M_CSR_SD_MODE;        // the card back to automatic
         load_done(load_on());
         break;
      case BROWSE_CHANGED:
         browse_reset();
         frame_full();
         load.step = L_MOUNT;
         break;
      }
      break;
   case L_ERROR:
      if (key == M2M_KEY_SPACE) {
         frame_full();
         browse();
      }
      break;
   }
}

// While the menu is open: an image, cartridge or ROM that comes or goes, as after a reset
// (VD_MNT_ST_GET, CRTROM_MLST_GET)
static void follow_loads(void)
{
   int changed = 0;
   if (drives_changed())
      for (int i = 0; i < menu.n; i++)
         if (is_mount[i] && menu.sel[i] != drive_mounted(is_mount[i] - 1)) {
            menu_set(i, !menu.sel[i]);
            setting_set(i, menu.sel[i]);
            changed = 1;
         }
   for (int i = 0; i < menu.n; i++) {
      int k = is_crtrom[i] - 1;
      if (k < 0 || k >= CRTROM_MAX)
         continue;
      int now = crtrom_loaded(k);
      if (now != crt_loaded[k]) {
         crt_loaded[k] = now;
         menu_set(i, now);
         setting_set(i, now);
         changed = 1;
      }
   }
   if (changed) {
      menu_show();
      menu_cursor_on();
   }
}

// -------------------------------------------------------------------------------------
// The menu's helpers for the core (M2M$GET_SETTING, M2M$FORCE_MENU)
// -------------------------------------------------------------------------------------

int shell_group(int idx)
{
   return idx >= 0 && idx < menu.n ? (int)(menu.group[idx] & MENU_ID) : -1;
}

int shell_item(int group, int pos)
{
   for (int i = 0; i < menu.n; i++)
      if ((int)(menu.group[i] & MENU_ID) == group && pos-- == 0)
         return i;
   return -1;
}

int shell_setting(int group, int pos)
{
   int i = shell_item(group, pos);
   return i >= 0 && setting_get(i);
}

void shell_force(int group, int pos, int on)
{
   int i = shell_item(group, pos);
   if (i < 0)
      return;
   setting_set(i, on);
   int old = menu_set(i, on);
   if (old >= 0)
      setting_set(old, 0);
}

// -------------------------------------------------------------------------------------
// Opening and closing the menu (HELP_MENU)
// -------------------------------------------------------------------------------------

static void open_menu(void)
{
   // Keyboard, joysticks and pause as config.vhd wants them while the menu is open
   M2M_CSR &= ~M2M_CSR_PAUSE;
   csr_flags(M2M_CSR & ~CSR_KBD_JOY, CFG_RP_KB_OSD, CFG_RP_J1_OSD, CFG_RP_J2_OSD);
   if (cfg_get(CFG_GENERAL, CFG_RP_PAUSE))
      M2M_CSR |= M2M_CSR_PAUSE;

   for (int i = 0; i < menu.n; i++)
      menu.sel[i] = cfgf.old[i] = setting_get(i);
   menu_show();
   scr_window_on(menu.x, menu.y, menu.dx, menu.dy);
   menu_cursor_on();
   ui = UI_MENU;
}

static void close_menu(void)
{
   scr_off();
   menu.shown = 0;
   save_settings();
   // Closed with the Close Menu item: start at the start item next time
   if (menu.group[menu.cursor] == MENU_CLOSE)
      menu.cursor = start_idx;
   closing_at = MTIME_LO;
   ui = UI_CLOSING;
}

static enum menu_key menu_key_of(uint32_t k)
{
   switch (k) {
   case M2M_KEY_UP:      return MENU_KEY_UP;
   case M2M_KEY_DOWN:    return MENU_KEY_DOWN;
   case M2M_KEY_RETURN:  return MENU_KEY_SELECT;
   case M2M_KEY_SPACE:   return MENU_KEY_SELALT;
   case M2M_KEY_HELP:
   case M2M_KEY_F13:     return MENU_KEY_CLOSE;
   case M2M_KEY_RUNSTOP: return MENU_KEY_MENUUP;
   default:              return MENU_KEY_NONE;
   }
}

// -------------------------------------------------------------------------------------
// Start and main loop
// -------------------------------------------------------------------------------------

void shell_start(void)
{
   menu.select = select;
   menu.show   = show;
   drives_init();
   load_menu();

   // The card the boot ROM read: mounted at once
   do
      sdcard_poll();
   while (sdcard_state() == SDCARD_WAIT);

   load_settings();
   roms_autoload();

   csr_flags(M2M_CSR, CFG_RP_KB_RST, CFG_RP_J1_RST, CFG_RP_J2_RST);
   ascal_init();

   const char *err = core_prep_start();
   if (err)
      shell_fatal(err, 0);

   // Out of reset, with keyboard and joysticks
   wait(WAIT_333MS);
   M2M_CSR = (M2M_CSR & ~M2M_CSR_RESET) | CSR_KBD_JOY;
   ui = UI_CORE;
}

void shell_poll(void)
{
   uint32_t k = keys_get();

   switch (ui) {
   case UI_CORE:
      if (k && k == core_menu_key())
         open_menu();
      break;
   case UI_MENU:
      follow_loads();
      if (k && menu_key(menu_key_of(k)))
         close_menu();
      break;
   case UI_LOAD:
      load_poll(k);
      break;
   case UI_HELP:
      if (k == M2M_KEY_SPACE || k == M2M_KEY_RUNSTOP) {
         menu.sel[help_idx] = 0;
         menu_again();
      } else if (k == M2M_KEY_LEFT && help_page > 0) {
         help_page--;
         show_help_page();
      } else if (k == M2M_KEY_RIGHT && help_page < help_pages - 1) {
         help_page++;
         show_help_page();
      }
      break;
   case UI_CLOSING:
      if (MTIME_LO - closing_at >= WAIT_333MS) {
         M2M_CSR = (M2M_CSR & ~M2M_CSR_PAUSE) | CSR_KBD_JOY;
         ui = UI_CORE;
      }
      break;
   }
}
