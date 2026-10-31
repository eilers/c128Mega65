// Gideon's configuration stores as the core has them (stores.h). The core is a machine with
// fewer stores, items and choices than an Ultimate. Where it has one of his items, the item
// has his name and type; its choices are the core's, under his names where he has the same.
// The C128's own ROMs are in a store of ours, C128 ROM Settings, and the menu's settings he
// has nothing like in another, MEGA65 Settings.
// Ported from the 1541 Ultimate firmware (github.com/GideonZ/1541ultimate), Copyright
// Gideon Zweijtzer and contributors, GPL v3 as this core; changed for c128Mega65 in 2026.
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "config_ids.h"
#include "core.h"
#include "drives.h"
#include "fmt.h"
#include "heap.h"
#include "net/netcfg.h"
#include "reu.h"
#include "rompick.h"
#include "stores.h"
#include "uci.h"

// I_GROUP: a menu group (item_t's group); an on_off item is its single-select line
enum { I_REU, I_REU_SIZE, I_REU_IMAGE, I_UCI, I_DRIVE, I_DRIVE_TYPE, I_DRIVE_ID, I_MODEM_IF,
       I_MODEM_HW, I_GROUP, I_NET };                        // I_NET + NC_*: netcfg's strings
#define I_ROM (I_NET + NC_COUNT)                            // I_ROM + RP_*: rompick.h's picks

static const char *const names[ST_COUNT] = {
   "C64 and Cartridge Settings", "Drive A Settings", "Drive B Settings", "Modem Settings",
   "Ethernet Settings", "Network Settings", "C128 ROM Settings", "MEGA65 Settings"
};

static const char *const en_dis[]      = { "Disabled", "Enabled", NULL };
static const char *const cart_prefs[]  = { "External", "Internal", NULL };   // the menu's Hardware, CRT file
static const char *const reu_sizes[]   = { "512 KB", "2 MB", "16 MB", NULL };   // the menu's 1-3
static const char *const drive_types[] = { "1541", "1571", "1581", "Internal 1581", NULL };   // DM_*
static const char *const modem_ifs[]   = { "ACIA / SwiftLink", NULL };
static const char *const acia_maps[]   = { "Off", "DE00/NMI", "DF00/NMI", "D700/NMI", NULL };   // the menu's 0-3
static const char *const modem_hws[]   = { "SwiftLink", NULL };
// The menu's lines
static const char *const video_outs[]  = { "Follow 40/80", "40 columns", "80 columns", "HDMI 40, VGA 80",
                                           "HDMI 80, VGA 40", "Swap with 40/80", NULL };
static const char *const jailbars[]    = { "Off", "Low", "Medium", "High", NULL };
static const char *const hdmi_modes[]  = { "720p 50 Hz 16:9", "720p 60 Hz 16:9", "576p 50 Hz 4:3", "576p 50 Hz 5:4",
                                           "640x480 60 Hz", "720x480 59.94 Hz", "800x600 60 Hz", NULL };
static const char *const vga_modes[]   = { "Standard", "15 kHz with HS/VS", "15 kHz with CSYNC", NULL };
static const char *const caps_keys[]   = { "40/80", "ASCII/DIN", NULL };
static const char *const joy_swaps[]   = { "Normal", "Swapped", NULL };   // his U64's Joystick Swapper

// An I_GROUP item on a single-select line: off, then on
static int on_off(const item_t *it)
{
   return it->choices == en_dis || it->choices == joy_swaps;
}

// In his stores' order; a string's max is his longest (the REU image's is a card path), a
// file's what the menu can pick
const item_t stores_items[] = {
   { ST_C64,     I_ROM + RP_BOOT,IT_FILE,   "Cartridge",                  NULL, 0, PICK_MAX - 1, 0, "", 0 },
   { ST_C64,     I_GROUP,        IT_ENUM,   "Cartridge Preference",       cart_prefs,  0, 0, 0, NULL, OPTM_G_CART },
   { ST_C64,     I_ROM + RP_K64, IT_FILE,   "Alternate Kernal",           NULL, 0, PICK_MAX - 1, 0, "", 0 },
   { ST_C64,     I_REU,          IT_ENUM,   "RAM Expansion Unit",         en_dis,      0, 0, 0, NULL, 0 },
   { ST_C64,     I_REU_SIZE,     IT_ENUM,   "REU Size",                   reu_sizes,   0, 0, 1, NULL, 0 },
   { ST_C64,     I_REU_IMAGE,    IT_STRING, "REU Preload Image",          NULL, 0, UCI_PATH_MAX - 1, 0, "", 0 },
   { ST_C64,     I_UCI,          IT_ENUM,   "Command Interface",          en_dis,      0, 0, 0, NULL, 0 },
   { ST_DRIVE_A, I_DRIVE,        IT_ENUM,   "Drive",                      en_dis,      0, 0, 1, NULL, 0 },
   { ST_DRIVE_A, I_DRIVE_TYPE,   IT_ENUM,   "Drive Type",                 drive_types, 0, 0, 1, NULL, 0 },
   { ST_DRIVE_A, I_DRIVE_ID,     IT_VALUE,  "Drive Bus ID",               NULL,        8, 11, 8, NULL, 0 },
   { ST_DRIVE_A, I_ROM + RP_DRV(0, 0), IT_FILE, "ROM for 1541 mode",      NULL, 0, PICK_MAX - 1, 0, "1541.rom", 0 },
   { ST_DRIVE_A, I_ROM + RP_DRV(0, 1), IT_FILE, "ROM for 1571 mode",      NULL, 0, PICK_MAX - 1, 0, "1571.rom", 0 },
   { ST_DRIVE_A, I_ROM + RP_DRV(0, 2), IT_FILE, "ROM for 1581 mode",      NULL, 0, PICK_MAX - 1, 0, "1581.rom", 0 },
   { ST_DRIVE_B, I_DRIVE,        IT_ENUM,   "Drive",                      en_dis,      0, 0, 1, NULL, 0 },
   { ST_DRIVE_B, I_DRIVE_TYPE,   IT_ENUM,   "Drive Type",                 drive_types, 0, 0, 1, NULL, 0 },
   { ST_DRIVE_B, I_DRIVE_ID,     IT_VALUE,  "Drive Bus ID",               NULL,        8, 11, 9, NULL, 0 },
   { ST_DRIVE_B, I_ROM + RP_DRV(1, 0), IT_FILE, "ROM for 1541 mode",      NULL, 0, PICK_MAX - 1, 0, "1541.rom", 0 },
   { ST_DRIVE_B, I_ROM + RP_DRV(1, 1), IT_FILE, "ROM for 1571 mode",      NULL, 0, PICK_MAX - 1, 0, "1571.rom", 0 },
   { ST_DRIVE_B, I_ROM + RP_DRV(1, 2), IT_FILE, "ROM for 1581 mode",      NULL, 0, PICK_MAX - 1, 0, "1581.rom", 0 },
   { ST_MODEM,   I_MODEM_IF,     IT_ENUM,   "Modem Interface",            modem_ifs,   0, 0, 0, NULL, 0 },
   { ST_MODEM,   I_GROUP,        IT_ENUM,   "ACIA (6551) Mapping",        acia_maps,   0, 0, 0, NULL, OPTM_G_MODEM },
   { ST_MODEM,   I_MODEM_HW,     IT_ENUM,   "Hardware Mode",              modem_hws,   0, 0, 0, NULL, 0 },
   { ST_ETH,     I_GROUP,        IT_ENUM,   "Use DHCP",                   en_dis,      0, 0, 1, NULL, OPTM_G_NET_DHCP },
   { ST_ETH,     I_NET + NC_IP,  IT_STRING, "Static IP",                  NULL, 0, 16, 0, "192.168.2.64", 0 },
   { ST_ETH,     I_NET + NC_MASK,IT_STRING, "Static Netmask",             NULL, 0, 16, 0, "255.255.255.0", 0 },
   { ST_ETH,     I_NET + NC_GW,  IT_STRING, "Static Gateway",             NULL, 0, 16, 0, "192.168.2.1", 0 },
   { ST_ETH,     I_NET + NC_DNS, IT_STRING, "Static DNS",                 NULL, 0, 16, 0, "8.8.8.8", 0 },
   { ST_NET,     I_NET + NC_HOST,IT_STRING, "Host Name",                  NULL, 0, 31, 0, "c128-mega65", 0 },
   { ST_NET,     I_NET + NC_PASS,IT_PASS,   "Network Password",           NULL, 0, 31, 0, "", 0 },
   { ST_NET,     I_GROUP,        IT_ENUM,   "FTP File Service",           en_dis,      0, 0, 1, NULL, OPTM_G_NET_FTP },
   { ST_NET,     I_GROUP,        IT_ENUM,   "Web Remote Control Service", en_dis,      0, 0, 1, NULL, OPTM_G_NET_REST },
   // Ours: empty is the stock ROM (rompick.h)
   { ST_ROMS,    I_ROM + RP_K128,IT_FILE,   "C128 Kernal ROM",            NULL, 0, PICK_MAX - 1, 0, "", 0 },
   { ST_ROMS,    I_ROM + RP_B128,IT_FILE,   "C128 Basic ROM",             NULL, 0, PICK_MAX - 1, 0, "", 0 },
   { ST_ROMS,    I_ROM + RP_B64, IT_FILE,   "C64 Basic ROM",              NULL, 0, PICK_MAX - 1, 0, "", 0 },
   { ST_ROMS,    I_ROM + RP_CHAR,IT_FILE,   "Char ROM",                   NULL, 0, PICK_MAX - 1, 0, "", 0 },
   { ST_ROMS,    I_ROM + RP_FUNC,IT_FILE,   "Internal Function ROM",      NULL, 0, PICK_MAX - 1, 0, "", 0 },
   // Ours: the menu's other settings, under its names
   { ST_MEGA65,  I_GROUP,        IT_ENUM,   "Video Output",               video_outs,  0, 0, 0, NULL, OPTM_G_VIDEO_OUT },
   { ST_MEGA65,  I_GROUP,        IT_ENUM,   "VIC-II Jailbars",            jailbars,    0, 0, 0, NULL, OPTM_G_JAILBARS },
   { ST_MEGA65,  I_GROUP,        IT_ENUM,   "HDMI Display Mode",          hdmi_modes,  0, 0, 0, NULL, OPTM_G_HDMI },
   { ST_MEGA65,  I_GROUP,        IT_ENUM,   "HDMI Flicker-free",          en_dis,      0, 0, 0, NULL, OPTM_G_FF },
   { ST_MEGA65,  I_GROUP,        IT_ENUM,   "HDMI Audio",                 en_dis,      0, 0, 1, NULL, OPTM_G_HDMI_AUDIO },
   { ST_MEGA65,  I_GROUP,        IT_ENUM,   "HDMI CRT Emulation",         en_dis,      0, 0, 0, NULL, OPTM_G_CRT },
   { ST_MEGA65,  I_GROUP,        IT_ENUM,   "VGA Display Mode",           vga_modes,   0, 0, 0, NULL, OPTM_G_VGA_MODES },
   { ST_MEGA65,  I_GROUP,        IT_ENUM,   "Caps Lock Key",              caps_keys,   0, 0, 0, NULL, OPTM_G_KEYS },
   { ST_MEGA65,  I_GROUP,        IT_ENUM,   "Help Menu on F13",           en_dis,      0, 0, 0, NULL, OPTM_G_HELP_F13 },
   { ST_MEGA65,  I_GROUP,        IT_ENUM,   "Use IEC Port",               en_dis,      0, 0, 1, NULL, OPTM_G_IEC },
   { ST_MEGA65,  I_GROUP,        IT_ENUM,   "Joystick Swapper",           joy_swaps,   0, 0, 0, NULL, OPTM_G_JOY_SWAP },
};
const int stores_count = sizeof stores_items / sizeof stores_items[0];

#define ITEMS_MAX 64
_Static_assert(sizeof stores_items / sizeof stores_items[0] <= ITEMS_MAX, "want.group holds ITEMS_MAX items");

// What a change asks for, -1 where it says nothing; strings in their own buffers. And the
// items it names.
static uint8_t wanted[ITEMS_MAX];
static struct {
   int8_t reu, size, image, uci, power[2], model[2], id[2], net[NC_COUNT], rom[RP_COUNT];
   int8_t group[ITEMS_MAX];          // an I_GROUP item's, by its index
} want;
static char    want_str[NC_COUNT][32];
static char    want_rom[RP_COUNT][PICK_MAX];
static int8_t  power_kept[2] = { -1, -1 };   // Drive as REST or LOAD_CONFIG set it, -1 never: in the file
static char    want_image[UCI_PATH_MAX];
static char    shown[UCI_PATH_MAX + 4];   // item_text's
static int8_t  reu_kept;                  // the REU's size while it is off, as his REU Size
static uint8_t stale[ST_COUNT];
static uint8_t want_store[ST_COUNT];
static int     starting;                  // stores_load: the C128 held in reset

// The items a program's or cartridge's .cfg changed (stores_apply_session): what the file
// would have kept for them, and a hash of their value then, to see if they changed since
#define KEPT_MAX   (UCI_PATH_MAX + 4)        // item_text's longest
#define KEPT_NONE  "\x01"                    // nothing: drive power never set
static uint8_t  session[ITEMS_MAX];
static uint32_t session_hash[ITEMS_MAX];
static char    *kept;                         // ITEMS_MAX of KEPT_MAX, in the heap

const char *store_name(int s)
{
   return names[s];
}

int store_find(const char *name)
{
   for (int s = 0; s < ST_COUNT; s++)
      if (!strcasecmp(name, names[s]))
         return s;
   return strcasecmp(name, "Cartridge and ROM Settings") ? -1 : ST_C64;   // his other name for it
}

// --- values now ---------------------------------------------------------------------------

// A drive's submenu (config.vhd): its bus ID 8-11, its model
static int id_group(int d)
{
   return d ? OPTM_G_DRVB_ID : OPTM_G_DRVA_ID;
}

static int model_group(int d)
{
   return d ? OPTM_G_DRVB_MODEL : OPTM_G_DRVA_MODEL;
}

int stores_drive_model(int d)
{
   for (int m = 0; m < 4; m++)
      if (shell_setting(model_group(d), m))
         return m;
   return DM_1571;
}

void stores_drive_set_model(int d, int m)
{
   if (stores_drive_model(d) != m)
      shell_force(model_group(d), m, 1);
}

int stores_drive_id(int d)
{
   for (int i = 0; i < 4; i++)
      if (shell_setting(id_group(d), i))
         return 8 + i;
   return 8 + d;
}

// The other drive with that ID takes this one's: never two drives on one ID
void stores_drive_set_id(int d, int id)
{
   int was = stores_drive_id(d);
   if (id != was && stores_drive_id(!d) == id)
      shell_force(id_group(!d), was - 8, 1);
   shell_force(id_group(d), id - 8, 1);
}

int stores_drive_of_id(int id)
{
   for (int d = 0; d < 2; d++)
      if (stores_drive_id(d) == id)
         return d;
   return -1;
}

// Should both say so, drive A is it (main.vhd)
int stores_drive_internal(int d)
{
   return stores_drive_model(d) == DM_INTERNAL && (d == 0 || stores_drive_model(0) != DM_INTERNAL);
}

int stores_drive_fit(int d, int m)
{
   int img = drive_image_type(d);
   if (m == DM_INTERNAL || img < 0 || (img == 0 && m != DM_1581))
      return m;
   return img == 2 ? DM_1581 : DM_1571;
}

// Leaving the internal floppy, an image under it takes the nearest model. One drive at a
// time is the internal floppy, as in the menu (core_sel_post).
int stores_drive_ask_model(int d, int m)
{
   if (stores_drive_internal(d) && m != DM_INTERNAL) {
      stores_drive_set_model(d, stores_drive_fit(d, m));
      return 1;
   }
   if (stores_drive_fit(d, m) != m)
      return 0;
   if (m == DM_INTERNAL && stores_drive_model(!d) == DM_INTERNAL)
      stores_drive_set_model(!d, stores_drive_fit(!d, DM_1581));
   stores_drive_set_model(d, m);
   return 1;
}

// As main.vhd runs it: the image's type where the model can't read it, a D64 in a 1581 as a
// 1571 (a C128's drive)
int stores_drive_type(int d)
{
   int img = drive_image_type(d), m = stores_drive_model(d);
   if (stores_drive_internal(d) || img == 2)
      return 2;
   if (img == 1 || m == DM_INTERNAL)
      return 1;
   if (img == 0)
      return m == DM_1541 ? 0 : 1;
   return m;
}

static int reu_now(void)
{
   for (int i = 1; i < 4; i++)
      if (shell_setting(OPTM_G_REU, i))
         return i;
   return 0;
}

static int group_pos(int group, int n)
{
   for (int i = 0; i < n; i++)
      if (shell_setting(group, i))
         return i;
   return 0;
}

int item_choices(const item_t *it)
{
   int n = 0;
   while (it->choices[n])
      n++;
   if (it->id == I_REU_SIZE)
      while (n > 1 && shell_item(OPTM_G_REU, n) < 0)
         n--;                                          // 2 and 16 MB aren't on R3 memory
   return n;
}

int item_get(const item_t *it)
{
   int d = it->store == ST_DRIVE_B, r;
   switch (it->id) {
   case I_REU:        return reu_now() != 0;
   case I_REU_SIZE:
      r = reu_now() ? reu_now() : reu_kept ? reu_kept : 1 + it->def;
      return r - 1 < item_choices(it) ? r - 1 : 0;
   case I_UCI:        return shell_setting(OPTM_G_UCI, 0);
   case I_DRIVE:      return drive_running(d) != 0;
   case I_DRIVE_TYPE: return stores_drive_model(d);
   case I_DRIVE_ID:   return stores_drive_id(d);
   case I_GROUP:
      return on_off(it) ? shell_setting(it->group, 0) : group_pos(it->group, item_choices(it));
   default:           return 0;
   }
}

int item_pick(const item_t *it)
{
   return it->id - I_ROM;
}

// The REU image as an Ultimate shows a file on its card, under /SD
const char *item_text(const item_t *it)
{
   if (it->type == IT_FILE)
      return rompick_name(item_pick(it));
   if (it->id == I_REU_IMAGE) {
      if (!reu_file_picked())
         return "";
      strcpy(shown, "/SD");
      strcat(shown, reu_file());
      return shown;
   }
   return netcfg[it->id - I_NET];
}

// --- changes ------------------------------------------------------------------------------

void stores_begin(void)
{
   memset(&want, -1, sizeof want);
   memset(want_store, 0, sizeof want_store);
   memset(wanted, 0, sizeof wanted);
}

static int is_space(char ch)
{
   return ch == ' ' || ch == '\t';
}

// A choice in any case; LOAD_CONFIG's (his equal_ignoring_outer_space) with spaces around it
static int pick(const item_t *it, const char *v, int loadcfg)
{
   int n = strlen(v), k = item_choices(it);
   if (loadcfg) {
      while (is_space(*v))
         v++, n--;
      while (n > 0 && is_space(v[n - 1]))
         n--;
   }
   for (int i = 0; i < k; i++)
      if (n == (int)strlen(it->choices[i]) && !strncasecmp(v, it->choices[i], n))
         return i;
   return -1;
}

// His sscanf("%d"): 0 if no number starts the text
static int number(const char *s, int *out)
{
   int n = 0, neg;
   while (*s == ' ' || (*s >= '\t' && *s <= '\r'))
      s++;
   neg = *s == '-';
   if (*s == '-' || *s == '+')
      s++;
   if (*s < '0' || *s > '9')
      return 0;
   while (*s >= '0' && *s <= '9')
      n = n * 10 + *s++ - '0';
   *out = neg ? -n : n;
   return 1;
}

static int want_item(const item_t *it, const char *v, int loadcfg, int *n)
{
   int d = it->store == ST_DRIVE_B, c = 0;
   const char *card;

   if (it->type == IT_ENUM && (c = pick(it, v, loadcfg)) < 0)
      return WANT_CHOICE;
   switch (it->id) {
   case I_REU:      want.reu = c;      return WANT_OK;
   case I_UCI:      want.uci = c;      return WANT_OK;
   case I_DRIVE:    want.power[d] = c; return WANT_OK;
   case I_GROUP:    want.group[it - stores_items] = c; return WANT_OK;
   case I_MODEM_IF:
   case I_MODEM_HW:                    return WANT_OK;
   case I_REU_SIZE: want.size = c + 1; return WANT_OK;
   case I_DRIVE_TYPE:
      if (!stores_drive_internal(d) && stores_drive_fit(d, c) != c)
         return WANT_CHOICE;                        // its image can't run it
      want.model[d] = c;
      return WANT_OK;
   case I_DRIVE_ID:
      if (!loadcfg)
         *n = strtol(v, NULL, 0);
      else if (!number(v, n))
         return WANT_NONUM;                         // LOAD_CONFIG's says nothing of it
      if (*n < it->min || *n > it->max)
         return loadcfg ? WANT_CHOICE : WANT_RANGE;
      want.id[d] = *n;
      return WANT_OK;
   case I_REU_IMAGE:
      if (!*v) {
         want.image = 0;                         // none: REU_DEFAULT_FILE
         return WANT_OK;
      }
      if (!(card = uci_card_path(v, want_image)))
         return WANT_CHOICE;
      memmove(want_image, card, strlen(card) + 1);   // the card's path, from the buffer's start
      want.image = 1;
      return WANT_OK;
   default: {
      // His setString: cut to the longest, its last character then a '*'
      int k = it->type == IT_FILE ? item_pick(it) : it->id - I_NET;
      char *to = it->type == IT_FILE ? want_rom[k] : want_str[k];
      strncpy(to, v, it->max);
      to[it->max] = 0;
      if ((int)strlen(v) > it->max)
         to[it->max - 1] = '*';
      if (it->type == IT_FILE)
         want.rom[k] = 1;
      else
         want.net[k] = 1;
      return WANT_OK;
   }
   }
}

int stores_want(const item_t *it, const char *v, int loadcfg, int *n)
{
   int r = want_item(it, v, loadcfg, n);
   if (r == WANT_OK) {
      want_store[it->store] = 1;
      wanted[it - stores_items] = 1;
   }
   return r;
}

static void no_log(const char *fmt, ...)
{
   (void)fmt;
}

// The settings file, or the new one that was to take its name when the power went
static FRESULT settings_open(FIL *f)
{
   FRESULT r = f_open(f, SETTINGS_FILE, FA_READ);
   return r == FR_OK ? r : f_open(f, SETTINGS_TMP, FA_READ);
}

void stores_net_defaults(void)
{
   for (int i = 0; i < stores_count; i++)
      if (stores_items[i].id >= I_NET && stores_items[i].id < I_ROM)   // not the ROMs after them
         strcpy(netcfg[stores_items[i].id - I_NET], stores_items[i].sdef);
}

// Drive power only with power: the file's says nothing unless REST or LOAD_CONFIG set it
static void want_defaults(int s, int power)
{
   int n;
   for (int i = 0; i < stores_count; i++) {
      const item_t *it = &stores_items[i];
      if (it->store != s || (it->id == I_DRIVE && !power))
         continue;
      if (it->type == IT_ENUM)
         stores_want(it, it->choices[it->id == I_REU_SIZE && item_choices(it) <= it->def ? 0 : it->def], 0, &n);
      else if (it->id == I_DRIVE_ID) {
         want.id[it->store == ST_DRIVE_B] = it->def;
         want_store[s] = 1;
      } else if (it->type != IT_VALUE)
         stores_want(it, it->sdef, 0, &n);
   }
}

void stores_want_defaults(int s)
{
   want_defaults(s, 1);
}

void stores_want_saved(int s)
{
   FIL f;
   want_defaults(s, 0);
   if (settings_open(&f) == FR_OK) {
      stores_read(&f, 1u << s, no_log);
      f_close(&f);
   }
}

static int apply(void)
{
   int now = reu_now(), on, to, done = 1, net = 0;
   if (now)
      reu_kept = now;
   if (want.size > 0)
      reu_kept = want.size;
   on = want.reu >= 0 ? want.reu : now != 0;
   if (on && !reu_kept)
      reu_kept = shell_item(OPTM_G_REU, 2) >= 0 ? 2 : 1;   // his default, 2 MB; else 512 KB
   to = on ? reu_kept : 0;
   if (to != now && starting)
      shell_force(OPTM_G_REU, to, 1);           // nothing runs that the change could reset
   else if (to != now && uci_reu_set(to))
      done = 0;

   for (int d = 0; d < 2; d++)
      if (want.power[d] >= 0) {
         drive_power(d, want.power[d] ? DRIVE_ON : DRIVE_OFF);
         power_kept[d] = want.power[d];
      }
   for (int d = 0; d < 2; d++) {
      if (want.model[d] >= 0)
         stores_drive_ask_model(d, want.model[d]);
      if (want.id[d] >= 0)
         stores_drive_set_id(d, want.id[d]);
   }
   for (int i = 0; i < stores_count; i++) {
      const item_t *it = &stores_items[i];
      int c = want.group[i];
      if (it->id != I_GROUP || c < 0)
         continue;
      if (on_off(it))
         shell_force(it->group, 0, c);
      else
         shell_force(it->group, c, 1);
      net |= it->group == OPTM_G_NET_DHCP || it->group == OPTM_G_NET_FTP || it->group == OPTM_G_NET_REST;
   }
   if (want.image >= 0) {
      reu_file_set(want.image ? want_image : NULL);
      shell_crtrom_named(OPTM_G_REU_FILE, want.image ? want_image : NULL);
   }
   for (int k = 0; k < NC_COUNT; k++)
      if (want.net[k] >= 0) {
         strcpy(netcfg[k], want_str[k]);
         net = 1;
      }
   if (net)
      netcfg_apply();
   uint32_t roms = 0;
   for (int p = 0; p < RP_COUNT; p++)
      if (want.rom[p] >= 0 && strcmp(rompick_name(p), want_rom[p])) {
         rompick_set(p, want_rom[p]);
         roms |= 1u << p;
      }
   rompick_apply(roms);
   if (want.uci >= 0)                            // last: once it is off, a UCI reply can't be read
      shell_force(OPTM_G_UCI, 0, want.uci);
   for (int s = 0; s < ST_COUNT; s++)
      stale[s] |= want_store[s];
   return done;
}

static const char *value_text(const item_t *it);

// What the settings file holds for an item: NULL for drive power never set
static const char *saved_text(const item_t *it)
{
   if (it->id == I_DRIVE && power_kept[it->store == ST_DRIVE_B] < 0)
      return NULL;
   return value_text(it);
}

static uint32_t hash(const char *s)
{
   uint32_t h = 2166136261u;                  // FNV-1a
   while (s && *s)
      h = (h ^ (uint8_t)*s++) * 16777619u;
   return h;
}

int stores_apply(void)
{
   int done = apply();
   for (int i = 0; i < stores_count; i++)
      if (wanted[i])
         session[i] = 0;
   return done;
}

int stores_apply_session(void)
{
   if (!kept)
      kept = heap_alloc(ITEMS_MAX * KEPT_MAX);
   for (int i = 0; i < stores_count; i++)
      if (wanted[i] && !session[i]) {
         const char *v = saved_text(&stores_items[i]);
         strncpy(kept + i * KEPT_MAX, v ? v : KEPT_NONE, KEPT_MAX - 1);
         session[i] = 1;
      }
   int done = apply();
   for (int i = 0; i < stores_count; i++)
      if (wanted[i])
         session_hash[i] = hash(saved_text(&stores_items[i]));
   return done;
}

int stores_stale(int s)
{
   return stale[s];
}

void stores_saved(int s)
{
   stale[s] = 0;
}

// --- his settings file --------------------------------------------------------------------

#define LINE_MAX 128                 // his line buffer

// The next line without its CR and LF: 0 at the end. As in his, a last line without an LF
// isn't read. A line longer than his buffer is cut (his reads the rest as the next line).
static int read_line(FIL *f, char *line)
{
   int n = 0;
   char ch;
   UINT got;
   while (f_read(f, &ch, 1, &got) == FR_OK && got) {
      if (ch == '\n') {
         line[n] = 0;
         return 1;
      }
      if (ch != '\r' && n < LINE_MAX - 1)
         line[n++] = ch;
   }
   return 0;
}

// An item=value line of store s: 0 after an error (his CFG_LINE_MALFORMED)
static int read_item(int s, char *line, int nr, void (*log)(const char *fmt, ...))
{
   char *name = line, *v = NULL;
   int i, k, n;

   for (i = 0; line[i] && i < 40; i++)         // his name buffer: 40 characters
      if (line[i] == '=') {
         v = line + i + 1;
         break;
      }
   line[i < 40 ? i : 39] = 0;
   for (n = strlen(name); n && is_space(name[n - 1]); n--)
      name[n - 1] = 0;
   while (is_space(*name))
      name++;
   if (!*name) {
      log("Line %d: No item name.\n", nr);
      return 0;
   }
   if (!v) {
      log("Line %d: No value given for item '%s'.\n", nr, name);
      return 0;
   }
   for (k = 0; k < stores_count; k++)
      if (stores_items[k].store == s && !strcasecmp(name, stores_items[k].name))
         break;
   if (k == stores_count) {
      log("Line %d: Item '%s' not found in this store [%s]; ignored.\n", nr, name, names[s]);
      return 1;
   }
   switch (stores_want(&stores_items[k], v, 1, &n)) {
   case WANT_OK:
      return 1;
   case WANT_NONUM:
      return 0;
   }
   log("Line %d: Value '%s' is not a valid choice for item %s\n", nr, v, stores_items[k].name);
   return 0;
}

int stores_read(FIL *f, unsigned mask, void (*log)(const char *fmt, ...))
{
   char line[LINE_MAX];
   int s = -1, skip = 0, good = 1, nr = 0;
   while (read_line(f, line)) {
      nr++;
      if (line[0] == '#' || line[0] == ';')
         continue;
      if (line[0] == '[') {
         char *end = strchr(line, ']');
         s    = -1;
         skip = 0;
         if (end) {
            *end = 0;
            s = store_find(line + 1);
            if (s >= 0 && !(mask & 1u << s)) {
               s    = -1;
               skip = 1;
            } else if (s < 0) {
               skip = 1;
               log("Line %d: Store name '%s' not found; section ignored.\n", nr, line + 1);
            }
         }
      } else if (line[0] && s >= 0) {
         good &= read_item(s, line, nr, log);
      } else if (line[0] && !skip) {
         log("Line %d: Not inside valid store.\n", nr);
         good = 0;
      }
   }
   return good;
}

int stores_associated(const char *path, int usr)
{
   static const char *const ext[] = { ".cfg", ".usr" };
   char cfg[UCI_PATH_MAX + 4];
   FIL f;
   for (int i = 0; i <= usr; i++) {
      fmt_snprintf(cfg, sizeof cfg, "%s", path);
      char *dot = strrchr(cfg, '.'), *slash = strrchr(cfg, '/');
      if (dot && (!slash || dot > slash))
         *dot = 0;
      strcat(cfg, ext[i]);
      if (f_open(&f, cfg, FA_READ) != FR_OK)
         continue;
      stores_begin();
      stores_read(&f, ~0u, no_log);
      f_close(&f);
      stores_apply_session();
      return 0;
   }
   return -1;
}

// --- the settings file --------------------------------------------------------------------

// Every item it names, wanted and in effect; what it doesn't name keeps its default. Read at
// the start, so nothing in it is a change REST's save_to_flash would write back.
int stores_load(void)
{
   FIL f;
   stores_net_defaults();
   if (settings_open(&f) != FR_OK)
      return -1;
   stores_begin();
   stores_read(&f, ~0u, no_log);
   f_close(&f);
   starting = 1;
   stores_apply();
   starting = 0;
   for (int s = 0; s < ST_COUNT; s++)
      stores_saved(s);
   return 0;
}

static const char *value_text(const item_t *it)
{
   static char num[8];
   if (it->id == I_DRIVE)
      return en_dis[power_kept[it->store == ST_DRIVE_B]];
   if (it->type == IT_ENUM)
      return it->choices[item_get(it)];
   if (it->type == IT_VALUE) {
      fmt_snprintf(num, sizeof num, "%d", item_get(it));
      return num;
   }
   return item_text(it);
}

// Into a new file that then takes the old one's name, so that a power cut leaves one whole
// file or the other
int stores_save(void)
{
   FIL f;
   char line[UCI_PATH_MAX + 80];
   UINT put;
   int last = -1, ok = 1;
   if (f_open(&f, SETTINGS_TMP, FA_WRITE | FA_CREATE_ALWAYS) != FR_OK)
      return -1;
   for (int i = 0; i < stores_count && ok; i++) {
      const item_t *it = &stores_items[i];
      const char *v = saved_text(it);
      int n = 0;
      // A program's or cartridge's value: what the file had, unless it changed since
      if (session[i] && hash(v) != session_hash[i])
         session[i] = 0;
      if (session[i])
         v = strcmp(kept + i * KEPT_MAX, KEPT_NONE) ? kept + i * KEPT_MAX : NULL;
      if (!v)
         continue;
      if (it->store != last)
         n = fmt_snprintf(line, sizeof line, "[%s]\n", names[it->store]);
      last = it->store;
      n += fmt_snprintf(line + n, sizeof line - n, "%s=%s\n", it->name, v);
      ok = f_write(&f, line, n, &put) == FR_OK && put == (UINT)n;
   }
   ok &= f_close(&f) == FR_OK;
   if (!ok) {
      f_unlink(SETTINGS_TMP);
      return -1;
   }
   FRESULT r = f_unlink(SETTINGS_FILE);
   if (r != FR_OK && r != FR_NO_FILE)
      return -1;
   return f_rename(SETTINGS_TMP, SETTINGS_FILE) == FR_OK ? 0 : -1;   // else the new one is read
}
