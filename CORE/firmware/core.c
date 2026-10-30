// The C128 core's part of the RISC-V Shell: the callbacks of QNICE's m2m-rom.asm.
// Menu items are found by their config.vhd group and their place in it.
#include <stddef.h>
#include <string.h>
#include "config_ids.h"
#include "core.h"
#include "fmt.h"
#include "http.h"
#include "m2m_rv.h"
#include "net/netcfg.h"
#include "reu.h"
#include "rompick.h"
#include "roms.h"
#include "stores.h"
#include "uci.h"

// Internal 1581: Off, Drive 8, Drive 9. Cartridge: Hardware, CRT file.
#define INT1581_OFF 0
#define CART_CRT    1

// C_CRTROMS_MAN's core items (globals.vhd): the REU's file, the boot cartridge, the ROMs
#define DEV_REU     0x0000
#define DEV_BOOT    0x0001
#define DEV_ROM     0x0010         // + the pick

#define CRT_DEVICE        0x0106   // C_DEV_CRT (globals.vhd), its CSR (sw_cartridge_csr.vhd):
#define CRT_CSR_MAX_HI    0x020    // the largest .crt file it takes, in 64 kB
#define CRT_CSR_STALLS    0x021    // R: bank waits, W: clear
#define CRT_CSR_STALL_MAX 0x022    // R: the longest, in C64 cycles
#define CRT_CSR_STATS     0x023    // R: 1 = show them in the menu

// In a build that asks for it (CSR $023, the 6r3 builds), while a .crt file is the source,
// the Cartridge line shows how often the C128 waited a C64 cycle or more for a bank of the
// cache since the menu was last drawn, and the longest wait. Drawing it clears both.
const char *core_submenu_summary(int idx)
{
   static char line[32];
   int g;

   // A submenu's first menu group: the Cartridge submenu's source, the ROMs submenu's C128
   // KERNAL line, the Network submenu's REST on or off
   while ((g = shell_group(++idx)) == 0)
      ;
   if (g == OPTM_G_ROM_K128) {
      int custom = 0;
      for (int p = 0; p < RP_BOOT; p++)
         custom |= rompick_state(p) != RS_STOCK;
      return custom ? " ROMs: Custom" : " ROMs: Stock";
   }
   if (g == OPTM_G_NET_REST) {
      const char *ip = netcfg_live(NC_IP);
      fmt_snprintf(line, sizeof line, " Net: %s", *ip ? ip : "no address");
      return line;
   }
   if (g != OPTM_G_CART || !shell_setting(OPTM_G_CART, CART_CRT) ||
       crtrom_csr_get(CRT_DEVICE, CRT_CSR_STATS) != 1)
      return NULL;
   unsigned waits = crtrom_csr_get(CRT_DEVICE, CRT_CSR_STALLS);
   unsigned most  = crtrom_csr_get(CRT_DEVICE, CRT_CSR_STALL_MAX);
   crtrom_csr_set(CRT_DEVICE, CRT_CSR_STALLS, 0);

   // At most 4 digits each, so the line fits. A PAL C64 cycle is 1.015 us: us = cycles +
   // cycles / 64.
   if (waits > 9999)
      waits = 9999;
   if (most > 9999)
      most = 9999;
   most += most / 64;
   if (most > 9999)
      most = 9999;
   fmt_snprintf(line, sizeof line, " Stalls %u max %uus", waits, most);
   return line;
}

// The drive that is the internal floppy says so instead of naming an image
const char *core_mount_label(int drive)
{
   if (drive <= 1 && shell_setting(OPTM_G_INT1581, drive + 1))
      return "Internal 1581";
   return NULL;
}

void core_sel_pre(int group, int item)
{
   (void)item;
   // Choosing an image for the drive that is the internal floppy gives the drive back
   // to images first
   if ((group == OPTM_G_MOUNT_D8 && shell_setting(OPTM_G_INT1581, 1)) ||
       (group == OPTM_G_MOUNT_D9 && shell_setting(OPTM_G_INT1581, 2)))
      shell_force(OPTM_G_INT1581, INT1581_OFF, 1);
   // Loading a .crt file selects the virtual cartridge. main.vhd resets the C128 when
   // the source changes, and again once the file is loaded.
   if (group == OPTM_G_MOUNT_CRT && !shell_setting(OPTM_G_CART, CART_CRT))
      shell_force(OPTM_G_CART, CART_CRT, 1);
}

void core_sel_post(int group, int item)
{
   (void)item;
   if (group == OPTM_G_NET_DHCP || group == OPTM_G_NET_REST)
      netcfg_apply();
}

// --- the Network submenu's text items -----------------------------------------------------

// Its address lines show what DHCP gave while DHCP is on, else the static address
static int net_item(int group)
{
   switch (group) {
   case OPTM_G_NET_HOST: return NC_HOST;
   case OPTM_G_NET_PASS: return NC_PASS;
   case OPTM_G_NET_IP:   return NC_IP;
   case OPTM_G_NET_MASK: return NC_MASK;
   case OPTM_G_NET_GW:   return NC_GW;
   case OPTM_G_NET_DNS:  return NC_DNS;
   default:              return -1;
   }
}

static const char *net_value(int k)
{
   return k >= NC_IP && shell_setting(OPTM_G_NET_DHCP, 0) ? netcfg_live(k) : netcfg[k];
}

const char *core_item_text(int group)
{
   static char stars[32];
   int k = net_item(group);
   if (k < 0)
      return NULL;
   const char *v = net_value(k);
   if (!*v)
      return "<none>";
   if (k != NC_PASS)
      return v;
   memset(stars, '*', strlen(v));
   stars[strlen(v)] = 0;
   return stars;
}

int core_text_item(int group, char *text, int *max)
{
   int k = net_item(group);
   if (k < 0)
      return 0;
   *max = k < NC_IP ? 31 : 15;
   strcpy(text, net_value(k));
   return 1;
}

// The Network line's address, once DHCP gives one
int core_menu_stale(void)
{
   static char last[16];
   const char *ip = netcfg_live(NC_IP);
   if (!strcmp(ip, last))
      return 0;
   strcpy(last, ip);
   return 1;
}

// Four numbers of 0-255 with dots between (lwIP's ip4addr_aton also takes "1.2")
static int dotted(const char *s)
{
   for (int part = 0; part < 4; part++) {
      int n = 0, digits = 0;
      while (*s >= '0' && *s <= '9' && digits < 4) {
         n = n * 10 + *s++ - '0';
         digits++;
      }
      if (!digits || digits > 3 || n > 255 || *s != (part < 3 ? '.' : 0))
         return 0;
      s++;
   }
   return 1;
}

// An address turns DHCP off, the others then as DHCP gave them. Typed in the menu, the
// settings are kept at once, as the menu's own are when it closes.
const char *core_text_done(int group, const char *text)
{
   int k = net_item(group);
   if (k >= NC_IP && !dotted(text))
      return "not an address";
   if (k >= NC_IP && shell_setting(OPTM_G_NET_DHCP, 0)) {
      for (int i = NC_IP; i <= NC_DNS; i++)
         if (*netcfg_live(i))
            strcpy(netcfg[i], netcfg_live(i));
      shell_force(OPTM_G_NET_DHCP, 0, 0);
   }
   strcpy(netcfg[k], text);
   netcfg_apply();
   stores_save();
   return NULL;
}

// --- the ROMs submenu and the boot cartridge (rompick.h) ----------------------------------

// The pick of a menu group's line, or of a core item's C_CRTROMS_MAN word; or -1
static int pick_of_group(int group)
{
   if (group == OPTM_G_CRT_BOOT)
      return RP_BOOT;
   return group >= OPTM_G_ROM_K128 && group <= OPTM_G_ROM_D9_81 ? group - OPTM_G_ROM_K128 : -1;
}

static int pick_of_dev(int dev)
{
   if (dev == DEV_BOOT)
      return RP_BOOT;
   return dev >= DEV_ROM && dev < DEV_ROM + RP_BOOT ? dev - DEV_ROM : -1;
}

// A pick's browser stays in its folder
const char *core_browse_root(int ctx, int group)
{
   int p = pick_of_group(group);
   return ctx == CTX_LOAD_ROM && p >= 0 ? rompick_dir(p) : NULL;
}

// A pick's line without its file: Stock, None, or <Error> for a file that wouldn't load
const char *core_crtrom_text(int dev)
{
   int p = pick_of_dev(dev);
   if (p < 0 || rompick_state(p) == RS_FILE)
      return NULL;
   if (rompick_state(p) == RS_FAILED)
      return "<Error>";
   return p == RP_BOOT ? "<None>" : p == RP_FUNC ? "None" : "Stock";
}

// Space on a pick's line: back to stock, kept at once
int core_crtrom_clear(int dev)
{
   int p = pick_of_dev(dev);
   if (p < 0)
      return 0;
   if (p == RP_BOOT)
      rompick_set(p, "");
   else
      rompick_unset(p);
   rompick_apply(1u << p);
   stores_save();
   return 1;
}

// The sizes a pick takes, for its messages
static const char *sizes(int p)
{
   static char s[24];
   int n = 0;
   for (uint32_t k = 8; k <= 32; k *= 2)
      if (rompick_fits(p, k << 10))
         n += fmt_snprintf(s + n, sizeof s - n, n ? " or %u" : "%u", (unsigned)k);
   fmt_snprintf(s + n, sizeof s - n, " KB");
   return s;
}

// Disk images for the drives, .crt files for the cartridge, .reu files for the REU, files of
// the right size for a ROM; folders always
int core_filter_file(const char *name, int dir, int ctx, int group, uint32_t size)
{
   static const char *const disk[] = { ".D64", ".D71", ".D81", NULL }, *const crt[] = { ".CRT", NULL },
                     *const reu[] = { ".REU", NULL };
   const char *const *ext = ctx == CTX_MOUNT_DISKIMG ? disk :
                            ctx == CTX_LOAD_ROM ? (group == OPTM_G_REU_FILE ? reu : crt) : NULL;
   int len = strlen(name), p = pick_of_group(group);

   if (dir || !ext)
      return 0;
   if (ctx == CTX_LOAD_ROM && p >= 0 && p != RP_BOOT)
      return !rompick_fits(p, size);
   for (; *ext; ext++)
      if (len >= 4 && !strcmp(name + len - 4, *ext))
         return 0;
   return 1;
}

// D64/D71/D81 are raw sector dumps without a header, so the file size is the only thing
// that identifies them, and it has to: the image type makes iec_drive a 1541/1571 or a
// 1581. Only the exact standard sizes are taken.
//
// A .crt file must fit the memory behind the cartridge device, which says how much that
// is: 2 MiB of HyperRAM on R3, 4 MiB of SDRAM on later boards (doc/hyperram-map.md). The
// parser in crt_parser.vhd checks the rest.
int core_prep_load(uint32_t size, int ctx, int group, int *type, const char **msg)
{
   // Image types as vdrives.vhd's img_type, which main.vhd expands for iec_drive
   static const struct { uint32_t size; int type; } disks[] = {
      { 174848, 0 },   // D64, 35 tracks: 1541
      { 196608, 0 },   // D64, 40 tracks
      { 349696, 1 },   // D71: 1571
      { 819200, 2 },   // D81: 1581
   };
   *type = 0;
   if (ctx == CTX_MOUNT_DISKIMG) {
      for (unsigned i = 0; i < sizeof disks / sizeof disks[0]; i++)
         if (size == disks[i].size) {
            *type = disks[i].type;
            return 0;
         }
      *msg = "\n\nThis is not a supported disk image. Sizes\nmust be exact: D64 174848 (35 tracks) or\n"
             "196608 (40 tracks), D71 349696, D81 819200.\n\nPress SPACE to continue.\n";
      return 1;
   }
   if (ctx == CTX_LOAD_ROM && group == OPTM_G_REU_FILE) {
      if (reu_size())
         return 0;
      *msg = "\n\nThe REU is off. Choose its size above\nfirst.\n\nPress SPACE to continue.\n";
      return 1;
   }
   int p = pick_of_group(group);
   if (ctx == CTX_LOAD_ROM && p >= 0 && p != RP_BOOT) {
      static char m[96];
      if (rompick_fits(p, size))
         return 0;
      fmt_snprintf(m, sizeof m, "\n\nThis line takes a ROM of %s.\n\nPress SPACE to continue.\n", sizes(p));
      *msg = m;
      return 1;
   }
   if (ctx == CTX_LOAD_ROM && size > (uint32_t)crtrom_csr_get(CRT_DEVICE, CRT_CSR_MAX_HI) << 16) {
      *msg = "\n\nThis .crt file is too large. A virtual\ncartridge can be up to 4 MB, or 2 MB on\n"
             "an R3 board.\n\nPress SPACE to continue.\n";
      return 1;
   }
   return 0;
}

// The folder the user browsed into holds nothing for this menu item
const char *core_custom_msg(int situation, int ctx, int group)
{
   static char m[200];
   int p = pick_of_group(group);
   if (situation != CMSG_BROWSENOTHING)
      return NULL;
   if (ctx == CTX_LOAD_ROM && p == RP_BOOT)
      return "No .crt cartridge files in /c128/carts.\n\nCopy the C64 or C128 .crt files to\n"
             "start with to /c128/carts on this SD\ncard.\n\nPress Space to continue.";
   if (ctx == CTX_LOAD_ROM && p >= 0) {
      fmt_snprintf(m, sizeof m, "No ROM files of %s in /c128/roms.\n\n"
                   "Copy ROM files to /c128/roms on this\nSD card. A line shows the files of its\n"
                   "size only.\n\nPress Space to continue.", sizes(p));
      return m;
   }
   if (ctx == CTX_LOAD_ROM && group == OPTM_G_REU_FILE)
      return "No .reu files here.\n\nCopy REU image files (.reu) to any\n"
             "folder of this SD card, for example\n/c128.\n\nPress Space to continue.";
   if (ctx == CTX_LOAD_ROM)
      return "No .crt cartridge files here.\n\nCopy C64 or C128 .crt files to any\n"
             "folder of this SD card, for example\n/c128.\n\nPress Space to continue.";
   if (ctx == CTX_MOUNT_DISKIMG)
      return "This core uses D64, D71 and D81 disk\nimages.\n\n"
             "Please copy at least one D64, D71 or D81\nfile to any sub-directory or to the root\n"
             "directory of this SD card.\n\nIf you use a folder called /c128, then\n"
             "the file browser will always start there.\n\nYou can use long file names and you can\n"
             "also use nested sub-directories to nicely\norder your collection of disk images.\n\n"
             "Nothing to browse.\n\nPress Space to continue.";
   return NULL;
}

// The REU's file: into the REU, as much as it holds, and LOAD_REU and SAVE_REU use it from then
// on. One that fails part of the way leaves no file picked. bar: the menu's progress bar.
static int reu_load(const char *path, int bar, const char **msg)
{
   FIL f;
   reu_file_set(NULL);
   FRESULT res = f_open(&f, path, FA_READ);
   if (res == FR_OK) {
      uint32_t size = f_size(&f);
      reu_xfer_t x = { .f = &f, .left = reu_size() };
      while (res == FR_OK && x.left) {
         uint32_t before = x.done;
         res = reu_step(&x, 8192);
         if (bar)
            crtrom_progress(x.done - before);
      }
      f_close(&f);
      if (bar && size > x.done)
         crtrom_progress(size - x.done);           // what the REU has no room for
   }
   if (res != FR_OK) {
      *msg = uci_fr_text(res);
      return 0x100 + res;
   }
   reu_file_set(path);
   return 0;
}

// The core's own items (C_CRTROMS_MAN): the REU's File:%s, or a pick (the Boot line, the ROMs
// submenu). Either is kept in the settings file at once.
int core_crtrom_load(int dev, const char *path, const char **msg)
{
   int p = pick_of_dev(dev), err = 0;
   if (p < 0)
      err = reu_load(path, 1, msg);
   else {
      // Its name in its folder, where the browser stayed
      const char *dir = rompick_dir(p), *name;
      int n = strlen(dir);
      name = !strncmp(path, dir, n) && path[n] == '/' ? path + n + 1 : strrchr(path, '/') + 1;
      if (strlen(name) >= PICK_MAX) {
         *msg = "\n\nThe file's name is too long: 32\ncharacters at most.\n\nPress SPACE to continue.\n";
         return 0x102;
      }
      rompick_set(p, name);
      rompick_apply(1u << p);
      if (rompick_state(p) == RS_FAILED) {
         *msg = "\n\nThe file could not be read.\n\nPress SPACE to continue.\n";
         err = 0x101;
         rompick_unset(p);
         rompick_apply(1u << p);
      }
   }
   if (!err)
      stores_save();
   return err;
}

int core_crtrom_loaded(int dev)
{
   int p = pick_of_dev(dev);
   return p < 0 ? reu_file_picked() : rompick_state(p) == RS_FILE;
}

// Before the C128 starts: the settings file's items (the network's in effect once it
// starts), the ROMs picked, and the REU's image into the REU, as his REU Preload. The boot
// cartridge loads later, once the C128 runs (rompick_poll).
const char *core_prep_start(void)
{
   const char *msg;
   stores_load();
   rompick_boot();
   if (reu_file_picked() && reu_size()) {
      static char path[UCI_PATH_MAX];
      strcpy(path, reu_file());
      if (reu_load(path, 0, &msg))
         shell_crtrom_named(OPTM_G_REU_FILE, NULL);
   }
   return NULL;
}

// "Help menu on F13" lets the C128 have its Help key
uint32_t core_menu_key(void)
{
   return shell_setting(OPTM_G_HELP_F13, 0) ? M2M_KEY_F13 : M2M_KEY_HELP;
}

// The boot cartridge's load (rompick.c), the Ultimate Command Interface (uci.c) and the REST
// API (net/http.c)
void core_poll(void)
{
   rompick_poll();
   uci_poll();
   http_poll();
}
