// Gideon's configuration stores as the core has them (stores.h). The core is a machine with
// fewer stores, items and choices than an Ultimate. Where it has one of his items, the item
// has his name and type; its choices are the core's, under his names where he has the same.
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "config_ids.h"
#include "core.h"
#include "drives.h"
#include "net/netcfg.h"
#include "reu.h"
#include "stores.h"
#include "uci.h"

enum { I_REU, I_REU_SIZE, I_REU_IMAGE, I_UCI, I_DRIVE, I_DRIVE_TYPE, I_DRIVE_ID, I_MODEM_IF,
       I_MODEM_MAP, I_MODEM_HW, I_DHCP, I_REST, I_NET };   // I_NET + NC_*: netcfg's strings

static const char *const names[ST_COUNT] = {
   "C64 and Cartridge Settings", "Drive A Settings", "Drive B Settings", "Modem Settings",
   "Ethernet Settings", "Network Settings"
};

static const char *const en_dis[]      = { "Disabled", "Enabled", NULL };
static const char *const reu_sizes[]   = { "512 KB", "2 MB", "16 MB", NULL };   // the menu's 1-3
static const char *const drive_types[] = { "1541", "1571", "1581", NULL };
static const char *const modem_ifs[]   = { "ACIA / SwiftLink", NULL };
static const char *const acia_maps[]   = { "Off", "DE00/NMI", "DF00/NMI", "D700/NMI", NULL };   // the menu's 0-3
static const char *const modem_hws[]   = { "SwiftLink", NULL };

// In his stores' order; a string's max is his longest
const item_t stores_items[] = {
   { ST_C64,     I_REU,          IT_ENUM,   "RAM Expansion Unit",         en_dis,      0, 0, 0, NULL },
   { ST_C64,     I_REU_SIZE,     IT_ENUM,   "REU Size",                   reu_sizes,   0, 0, 1, NULL },
   { ST_C64,     I_REU_IMAGE,    IT_STRING, "REU Preload Image",          NULL, 0, UCI_PATH_MAX - 1, 0, "" },
   { ST_C64,     I_UCI,          IT_ENUM,   "Command Interface",          en_dis,      0, 0, 0, NULL },
   { ST_DRIVE_A, I_DRIVE,        IT_ENUM,   "Drive",                      en_dis,      0, 0, 1, NULL },
   { ST_DRIVE_A, I_DRIVE_TYPE,   IT_ENUM,   "Drive Type",                 drive_types, 0, 0, 1, NULL },
   { ST_DRIVE_A, I_DRIVE_ID,     IT_VALUE,  "Drive Bus ID",               NULL,        8, 8, 8, NULL },
   { ST_DRIVE_B, I_DRIVE,        IT_ENUM,   "Drive",                      en_dis,      0, 0, 1, NULL },
   { ST_DRIVE_B, I_DRIVE_TYPE,   IT_ENUM,   "Drive Type",                 drive_types, 0, 0, 1, NULL },
   { ST_DRIVE_B, I_DRIVE_ID,     IT_VALUE,  "Drive Bus ID",               NULL,        9, 9, 9, NULL },
   { ST_MODEM,   I_MODEM_IF,     IT_ENUM,   "Modem Interface",            modem_ifs,   0, 0, 0, NULL },
   { ST_MODEM,   I_MODEM_MAP,    IT_ENUM,   "ACIA (6551) Mapping",        acia_maps,   0, 0, 0, NULL },
   { ST_MODEM,   I_MODEM_HW,     IT_ENUM,   "Hardware Mode",              modem_hws,   0, 0, 0, NULL },
   { ST_ETH,     I_DHCP,         IT_ENUM,   "Use DHCP",                   en_dis,      0, 0, 1, NULL },
   { ST_ETH,     I_NET + NC_IP,  IT_STRING, "Static IP",                  NULL, 0, 16, 0, "192.168.2.64" },
   { ST_ETH,     I_NET + NC_MASK,IT_STRING, "Static Netmask",             NULL, 0, 16, 0, "255.255.255.0" },
   { ST_ETH,     I_NET + NC_GW,  IT_STRING, "Static Gateway",             NULL, 0, 16, 0, "192.168.2.1" },
   { ST_ETH,     I_NET + NC_DNS, IT_STRING, "Static DNS",                 NULL, 0, 16, 0, "8.8.8.8" },
   { ST_NET,     I_NET + NC_HOST,IT_STRING, "Host Name",                  NULL, 0, 31, 0, "c128-mega65" },
   { ST_NET,     I_NET + NC_PASS,IT_PASS,   "Network Password",           NULL, 0, 31, 0, "" },
   { ST_NET,     I_REST,         IT_ENUM,   "Web Remote Control Service", en_dis,      0, 0, 1, NULL },
};
const int stores_count = sizeof stores_items / sizeof stores_items[0];

// What a change asks for, -1 where it says nothing; strings in their own buffers
static struct {
   int8_t reu, size, image, uci, power[2], model, modem, dhcp, rest, net[NC_COUNT];
} want;
static char    want_str[NC_COUNT][32];
static char    want_image[UCI_PATH_MAX];
static char    shown[UCI_PATH_MAX + 4];   // item_text's
static int8_t  reu_kept;                  // the REU's size while it is off, as his REU Size
static uint8_t stale[ST_COUNT];
static uint8_t want_store[ST_COUNT];

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

int stores_drive_type(int d)
{
   int img = drive_image_type(d);
   if (core_mount_label(d) || img == 2)
      return 2;                                        // the internal floppy, or a D81
   if (img == 1)
      return 1;                                        // a D71 makes it a 1571 (main.vhd)
   return shell_setting(OPTM_G_DRV_MODEL, 0) ? 0 : 1;  // the menu's 5.25" model
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
   case I_DRIVE_TYPE: return stores_drive_type(d);
   case I_DRIVE_ID:   return 8 + d;
   case I_MODEM_MAP:  return group_pos(OPTM_G_MODEM, 4);
   case I_DHCP:       return shell_setting(OPTM_G_NET_DHCP, 0);
   case I_REST:       return shell_setting(OPTM_G_NET_REST, 0);
   default:           return 0;
   }
}

// The REU image as an Ultimate shows a file on its card, under /SD
const char *item_text(const item_t *it)
{
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
   case I_DHCP:     want.dhcp = c;     return WANT_OK;
   case I_REST:     want.rest = c;     return WANT_OK;
   case I_MODEM_IF:
   case I_MODEM_HW:                    return WANT_OK;
   case I_REU_SIZE: want.size = c + 1; return WANT_OK;
   case I_MODEM_MAP: want.modem = c;   return WANT_OK;
   case I_DRIVE_TYPE:
      // One model for both 5.25" drives: a second, other one can't be; a drive that is a
      // 1581 (the internal floppy, a D81) stays one
      if (c == 2 ? stores_drive_type(d) != 2 : want.model >= 0 && want.model != c)
         return WANT_CHOICE;
      if (c < 2)
         want.model = c;
      return WANT_OK;
   case I_DRIVE_ID:
      if (!loadcfg)
         *n = strtol(v, NULL, 0);
      else if (!number(v, n))
         return WANT_NONUM;                         // LOAD_CONFIG's says nothing of it
      if (*n < it->min || *n > it->max)
         return loadcfg ? WANT_CHOICE : WANT_RANGE;
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
      int k = it->id - I_NET;
      strncpy(want_str[k], v, it->max);
      want_str[k][it->max] = 0;
      if ((int)strlen(v) > it->max)
         want_str[k][it->max - 1] = '*';
      want.net[k] = 1;
      return WANT_OK;
   }
   }
}

int stores_want(const item_t *it, const char *v, int loadcfg, int *n)
{
   int r = want_item(it, v, loadcfg, n);
   if (r == WANT_OK)
      want_store[it->store] = 1;
   return r;
}

static void no_log(const char *fmt, ...)
{
   (void)fmt;
}

static int saved_pos(int group, int n)
{
   for (int i = 0; i < n; i++)
      if (shell_saved_setting(group, i))
         return i;
   return 0;
}

// What the settings file and network.cfg hold; items with no home on the card (drive power,
// the REU's image) stay as they are
void stores_want_saved(int s)
{
   int n, v;
   FIL f;
   for (int i = 0; i < stores_count; i++) {
      const item_t *it = &stores_items[i];
      if (it->store != s)
         continue;
      switch (it->id) {
      case I_REU:
         v = saved_pos(OPTM_G_REU, 4);
         stores_want(it, en_dis[v != 0], 0, &n);
         if (v)
            stores_want(&stores_items[i + 1], reu_sizes[v - 1], 0, &n);
         break;
      case I_UCI:        stores_want(it, en_dis[shell_saved_setting(OPTM_G_UCI, 0)], 0, &n); break;
      case I_DRIVE_TYPE: stores_want(it, drive_types[saved_pos(OPTM_G_DRV_MODEL, 2)], 0, &n); break;
      case I_MODEM_MAP:  stores_want(it, acia_maps[saved_pos(OPTM_G_MODEM, 4)], 0, &n); break;
      case I_DHCP:       stores_want(it, en_dis[shell_saved_setting(OPTM_G_NET_DHCP, 0)], 0, &n); break;
      case I_REST:       stores_want(it, en_dis[shell_saved_setting(OPTM_G_NET_REST, 0)], 0, &n); break;
      default:
         if (it->id >= I_NET)
            stores_want(it, it->sdef, 0, &n);
      }
   }
   if ((s == ST_ETH || s == ST_NET) && f_open(&f, NETCFG_FILE, FA_READ) == FR_OK) {
      stores_read(&f, 1u << s, no_log);
      f_close(&f);
   }
}

void stores_net_defaults(void)
{
   for (int i = 0; i < stores_count; i++)
      if (stores_items[i].id >= I_NET)
         strcpy(netcfg[stores_items[i].id - I_NET], stores_items[i].sdef);
}

void stores_want_defaults(int s)
{
   int n;
   for (int i = 0; i < stores_count; i++) {
      const item_t *it = &stores_items[i];
      if (it->store != s)
         continue;
      if (it->type == IT_ENUM)
         stores_want(it, it->choices[it->id == I_REU_SIZE && item_choices(it) <= it->def ? 0 : it->def], 0, &n);
      else if (it->type != IT_VALUE)
         stores_want(it, it->sdef, 0, &n);
   }
}

int stores_apply(void)
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
   if (to != now && uci_reu_set(to))
      done = 0;

   for (int d = 0; d < 2; d++)
      if (want.power[d] >= 0)
         drive_power(d, want.power[d] ? DRIVE_ON : DRIVE_OFF);
   if (want.model >= 0)
      shell_force(OPTM_G_DRV_MODEL, want.model, 1);
   if (want.modem >= 0)
      shell_force(OPTM_G_MODEM, want.modem, 1);
   if (want.image >= 0) {
      reu_file_set(want.image ? want_image : NULL);
      shell_crtrom_named(OPTM_G_REU_FILE, want.image ? want_image : NULL);
   }
   if (want.dhcp >= 0) {
      shell_force(OPTM_G_NET_DHCP, 0, want.dhcp);
      net = 1;
   }
   if (want.rest >= 0) {
      shell_force(OPTM_G_NET_REST, 0, want.rest);
      net = 1;
   }
   for (int k = 0; k < NC_COUNT; k++)
      if (want.net[k] >= 0) {
         strcpy(netcfg[k], want_str[k]);
         net = 1;
      }
   if (net)
      netcfg_apply();
   if (want.uci >= 0)                            // last: once it is off, a UCI reply can't be read
      shell_force(OPTM_G_UCI, 0, want.uci);
   for (int s = 0; s < ST_COUNT; s++)
      stale[s] |= want_store[s];
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
// isn't read; a longer line than his buffer takes is cut.
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
