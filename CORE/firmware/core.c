// The C128 core's part of the RISC-V Shell: the callbacks of QNICE's m2m-rom.asm.
// Menu items are found by their config.vhd group and their place in it.
#include <stdarg.h>
#include <stddef.h>
#include <string.h>
#include "cfg.h"
#include "cmdhd.h"
#include "config_ids.h"
#include "core.h"
#include "diskimg.h"
#include "dmem.h"
#include "drives.h"
#include "fmt.h"
#include "ftp.h"
#include "hdmi_filter.h"
#include "http.h"
#include "input.h"
#include "m2m_rv.h"
#include "net/net.h"
#include "net/netcfg.h"
#include "net/stream.h"
#include "reu.h"
#include "rompick.h"
#include "roms.h"
#include "stores.h"
#include "uci.h"

// Cartridge: Hardware, CRT file
#define DISKS_DIR   "/c128/disks"   // where the drives' browser starts, if the card has it
#define CART_HW     0
#define CART_CRT    1
#define MENU_WIDTH  23             // a line's text, as the Shell shows it (config.vhd's OPTM_DX)

// C_CRTROMS_MAN's core items (globals.vhd): the REU's file, the boot cartridge, the ROMs
#define DEV_REU     0x0000
#define DEV_BOOT    0x0001
#define DEV_CFG     0x0002         // Load settings
#define DEV_HD      0x0003         // the CMD HD's Image:%s
#define DEV_ROM     0x0010         // + the pick

#define CRT_DEVICE        0x0106   // C_DEV_CRT (globals.vhd), its CSR (sw_cartridge_csr.vhd):
#define CRT_CSR_MAX_HI    0x020    // the largest .crt file it takes, in 64 kB
#define CRT_CSR_STALLS    0x021    // R: bank waits, W: clear
#define CRT_CSR_STALL_MAX 0x022    // R: the longest, in C64 cycles
#define CRT_CSR_STATS     0x023    // R: 1 = show them in the menu

// A drive's line on the main menu: its ID and what it runs as, then its image, e.g.
// " 8:1571 game.d64", cut to the menu's width (OPTM_DX) as the Shell cuts an image's name.
// A drive that is off says so in place of its model: " 9:Off", " 9:Off game.d64".
static const char *drive_line(int d)
{
   static const char *const types[] = { "1541", "1571", "1581" };
   static char line[64];
   char none[32];
   int on = stores_drive_on(d);
   const char *name = drive_path(d), *slash = strrchr(name, '/');
   if (stores_drive_internal(d))
      name = "Internal";
   else if (*name)
      name = slash ? slash + 1 : name;
   else if (on) {
      cfg_str(CFG_OPTM_MSTR, none, sizeof none);
      name = none;
   }
   int n = fmt_snprintf(line, sizeof line, " %d:%s%s%s", stores_drive_id(d),
                        on ? types[stores_drive_type(d)] : "Off", *name ? " " : "", name);
   if (n > MENU_WIDTH)
      strcpy(line + MENU_WIDTH - 3, "...");
   return line;
}

// The CMD HD's line on the main menu, as a drive's: " 12:HD image.dhd", " 12:Off"
static int hd_id(void)
{
   for (int i = 0; i < 5; i++)
      if (shell_setting(OPTM_G_HD_ID, i))
         return 8 + i;
   return 12;
}

static const char *hd_line(void)
{
   static char line[64];
   char none[32];
   int on = shell_setting(OPTM_G_HD_ON, 0);
   const char *name = cmdhd_path(), *slash = strrchr(name, '/');
   if (*name)
      name = slash ? slash + 1 : name;
   else if (on) {
      cfg_str(CFG_OPTM_MSTR, none, sizeof none);
      name = none;
   }
   int n = fmt_snprintf(line, sizeof line, " %d:%s%s%s", hd_id(), on ? "HD" : "Off", *name ? " " : "", name);
   if (n > MENU_WIDTH)
      strcpy(line + MENU_WIDTH - 3, "...");
   return line;
}

// In a build that asks for it (CSR $023, the 6r3 builds), while a .crt file is the source,
// the Cartridge line shows how often the C128 waited a C64 cycle or more for a bank of the
// cache since the menu was last drawn, and the longest wait. Drawing it clears both.
const char *core_submenu_summary(int idx)
{
   static char line[32];
   int g;

   // A submenu's first menu group: a drive's Disk line, the Cartridge submenu's source, the
   // System ROMs submenu's C128 KERNAL line, the Keyboard submenu's Caps Lock key, the
   // Network submenu's REST on or off
   while ((g = shell_group(++idx)) == 0)
      ;
   if (g == OPTM_G_MOUNT_D8 || g == OPTM_G_MOUNT_D9)
      return drive_line(g == OPTM_G_MOUNT_D9);
   if (g == OPTM_G_HD_IMAGE)
      return hd_line();
   if (g == OPTM_G_KEYS)   // Stock: Caps Lock is 40/80 and Help opens the menu
      return shell_setting(OPTM_G_KEYS, 0) && !shell_setting(OPTM_G_HELP_F13, 0) ?
             " Keyboard: Stock" : " Keyboard: Custom";
   if (g == OPTM_G_ROM_K128) {
      int custom = 0;
      for (int p = 0; p < RP_DRIVE; p++)
         custom |= rompick_state(p) != RS_STOCK;
      return custom ? " System ROMs: Custom" : " System ROMs: Stock";
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

// A drive's Disk line: the internal floppy says so instead of naming an image
const char *core_mount_text(int drive, const char *name)
{
   (void)name;
   return drive <= 1 && stores_drive_internal(drive) ? "Internal 1581" : NULL;
}

static int id_was[3];   // a drive's ID before the menu changed it: A, B, the CMD HD

void core_sel_pre(int group, int item)
{
   (void)item;
   if (group == OPTM_G_DRVA_ID || group == OPTM_G_DRVB_ID)
      id_was[group == OPTM_G_DRVB_ID] = stores_drive_id(group == OPTM_G_DRVB_ID);
   if (group == OPTM_G_HD_ID)
      id_was[2] = hd_id();
   // Choosing an image for the drive that is the internal floppy gives the drive back
   // to images first, as a 1581
   for (int d = 0; d < 2; d++)
      if (group == (d ? OPTM_G_MOUNT_D9 : OPTM_G_MOUNT_D8) && stores_drive_internal(d))
         stores_drive_set_model(d, DM_1581);
   // Loading a .crt file selects the virtual cartridge. main.vhd resets the C128 when
   // the source changes, and again once the file is loaded.
   if (group == OPTM_G_MOUNT_CRT && !shell_setting(OPTM_G_CART, CART_CRT))
      shell_force(OPTM_G_CART, CART_CRT, 1);
}

void core_sel_post(int group, int item)
{
   // Never two drives on one ID: the other one takes this one's old ID. The CMD HD has 12 as
   // well, which a floppy can't: then the floppy takes the first ID that is free.
   if (group == OPTM_G_DRVA_ID || group == OPTM_G_DRVB_ID) {
      int d = group == OPTM_G_DRVB_ID;
      if (stores_drive_id(!d) == 8 + item)
         stores_drive_set_id(!d, id_was[d]);
      if (hd_id() == 8 + item)
         shell_force(OPTM_G_HD_ID, id_was[d] - 8, 1);
   }
   if (group == OPTM_G_HD_ID)
      for (int d = 0; d < 2; d++)
         if (stores_drive_id(d) == 8 + item) {
            int to = id_was[2];
            for (int id = 8; to > 11 && id <= 11; id++)
               if (id != 8 + item && stores_drive_id(!d) != id)
                  to = id;
            stores_drive_set_id(d, to);
         }
   // The CMD HD's front panel: pressed for a moment, then the line is off again
   if (shell_setting(group, 0)) {
      int btn = group == OPTM_G_HD_SWAP8 ? HD_BTN_SWAP8 : group == OPTM_G_HD_SWAP9 ? HD_BTN_SWAP9 :
                group == OPTM_G_HD_WP ? HD_BTN_WP : 0;
      if (btn)
         cmdhd_press(btn);
      if (group == OPTM_G_HD_CONFIG || group == OPTM_G_HD_INSTALL)
         cmdhd_reset(group == OPTM_G_HD_CONFIG ? HD_RESET_CONFIG : HD_RESET_INSTALL);
      if (btn || group == OPTM_G_HD_CONFIG || group == OPTM_G_HD_INSTALL)
         shell_force(group, 0, 0);
   }
   // A drive's model is always one its image runs: a model it can't run ejects the image
   if (group == OPTM_G_DRVA_MODEL || group == OPTM_G_DRVB_MODEL) {
      int d = group == OPTM_G_DRVB_MODEL;
      if (stores_drive_fit(d, item) != item)
         shell_unmount(d);
      // One drive at a time is the internal floppy: the other one becomes a 1581 of images
      else if (item == DM_INTERNAL && stores_drive_model(!d) == DM_INTERNAL)
         stores_drive_set_model(!d, stores_drive_fit(!d, DM_1581));
      // The internal floppy is a disk in the drive: it turns the drive on, as a mount does
      if (item == DM_INTERNAL)
         stores_drive_set_on(d, 1);
   }
   // A mount moves the model to one its image runs and turns the drive on; taking the
   // image out leaves it on
   for (int d = 0; d < 2; d++)
      if (group == (d ? OPTM_G_MOUNT_D9 : OPTM_G_MOUNT_D8)) {
         stores_drive_set_model(d, stores_drive_fit(d, stores_drive_model(d)));
         if (drive_mounted(d))
            stores_drive_set_on(d, 1);
      }
   if (group == OPTM_G_NET_DHCP || group == OPTM_G_NET_REST || group == OPTM_G_NET_FTP)
      netcfg_apply();
   // His F5 menu's stream start and stop, to the target on the line below (stream.c)
   if (group == OPTM_G_NET_VSTREAM || group == OPTM_G_NET_ASTREAM) {
      int id = group == OPTM_G_NET_ASTREAM;
      uint32_t dev = net_lock();
      if (shell_setting(group, 0))
         stream_start(id, NULL, 1);
      else
         stream_stop(id);
      net_unlock(dev);
   }
}

// --- the Network submenu's text items -----------------------------------------------------

// Its address lines show what DHCP gave while DHCP is on, else the static address. The
// streams' to: lines are where they go (his Data Streams store).
static int net_item(int group)
{
   switch (group) {
   case OPTM_G_NET_HOST: return NC_HOST;
   case OPTM_G_NET_PASS: return NC_PASS;
   case OPTM_G_NET_IP:   return NC_IP;
   case OPTM_G_NET_MASK: return NC_MASK;
   case OPTM_G_NET_GW:   return NC_GW;
   case OPTM_G_NET_DNS:  return NC_DNS;
   case OPTM_G_NET_VTO:  return NC_STREAM;
   case OPTM_G_NET_ATO:  return NC_STREAM + 1;
   default:              return -1;
   }
}

static int net_addr(int k)
{
   return k >= NC_IP && k <= NC_DNS;
}

static const char *net_value(int k)
{
   return net_addr(k) && shell_setting(OPTM_G_NET_DHCP, 0) ? netcfg_live(k) : netcfg[k];
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
   *max = k >= NC_STREAM ? 36 : k < NC_IP ? 31 : 15;
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
   if (net_addr(k) && !dotted(text))
      return "not an address";
   if (net_addr(k) && shell_setting(OPTM_G_NET_DHCP, 0)) {
      for (int i = NC_IP; i <= NC_DNS; i++)
         if (*netcfg_live(i))
            strcpy(netcfg[i], netcfg_live(i));
      shell_force(OPTM_G_NET_DHCP, 0, 0);
   }
   strcpy(netcfg[k], text);
   if (k < NC_STREAM)                 // a stream's target takes effect at its next start
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
   if (group == OPTM_G_ROM_HD)
      return RP_HD;
   return group >= OPTM_G_ROM_K128 && group <= OPTM_G_ROM_D9_81 ? group - OPTM_G_ROM_K128 : -1;
}

static int pick_of_dev(int dev)
{
   if (dev == DEV_BOOT)
      return RP_BOOT;
   if (dev == DEV_ROM + RP_HD)
      return RP_HD;
   return dev >= DEV_ROM && dev < DEV_ROM + RP_BOOT ? dev - DEV_ROM : -1;
}

// A line's browser starts in the folder for its kind, if the card has it; the REU's image and
// Load settings start in config.vhd's (/c128)
const char *core_browse_start(int ctx, int group)
{
   int p = pick_of_group(group);
   if (ctx == CTX_MOUNT_DISKIMG)
      return DISKS_DIR;
   if (ctx != CTX_LOAD_ROM)
      return NULL;
   if (group == OPTM_G_MOUNT_CRT)
      return CARTS_DIR;
   if (group == OPTM_G_HD_IMAGE)
      return HD_DIR;
   return p >= 0 ? rompick_dir(p) : NULL;
}

// A pick's line without its file: Stock, None, or <Error> for a file that wouldn't load
const char *core_crtrom_text(int dev)
{
   int p = pick_of_dev(dev);
   if (p < 0 || rompick_state(p) == RS_FILE)
      return NULL;
   if (rompick_state(p) == RS_FAILED)
      return "<Error>";
   return p == RP_BOOT || p == RP_HD ? "<None>" : p == RP_FUNC ? "None" : "Stock";
}

// Space on a line: a pick back to stock and kept at once; the virtual cartridge out, the
// hardware slot the source again (both reset the C128); no REU image at the next start.
// On a CRT or REU line without a file, Space browses as Return does.
int core_crtrom_clear(int group)
{
   int p = pick_of_group(group);
   if (group == OPTM_G_MOUNT_CRT) {
      if (!crtrom_empty(CRT_DEVICE))
         return 0;
      shell_force(OPTM_G_CART, CART_HW, 1);
      return 1;
   }
   if (group == OPTM_G_REU_FILE) {
      if (!reu_file_picked())
         return 0;
      reu_file_set(NULL);
      stores_save();
      return 1;
   }
   if (group == OPTM_G_HD_IMAGE) {
      if (!*cmdhd_path())
         return 0;
      cmdhd_unmount();
      stores_save();
      return 1;
   }
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
// the right size for a ROM, settings files for Load settings; folders always
int core_filter_file(const char *name, int dir, int ctx, int group, uint32_t size)
{
   static const char *const disk[] = { ".D64", ".D71", ".D81", NULL }, *const crt[] = { ".CRT", NULL },
                     *const reu[] = { ".REU", NULL }, *const cfg[] = { ".CFG", NULL },
                     *const hd[] = { ".DHD", ".HDA", ".HDD", NULL };
   const char *const *ext = ctx == CTX_MOUNT_DISKIMG ? disk :
                            ctx != CTX_LOAD_ROM ? NULL :
                            group == OPTM_G_REU_FILE ? reu : group == OPTM_G_LOAD_CFG ? cfg :
                            group == OPTM_G_HD_IMAGE ? hd : crt;
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
   if (ctx == CTX_LOAD_ROM && (group == OPTM_G_LOAD_CFG || group == OPTM_G_HD_IMAGE))
      return 0;                        // a CMD HD image: cmdhd_mount checks it
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
   if (situation == CMSG_NEWFILE)
      return group == OPTM_G_HD_IMAGE ? "name,MB sets the size: 16 to 4000" :
             ".d64, .d71 or .d81 picks the type";
   if (situation != CMSG_BROWSENOTHING)
      return NULL;
   if (ctx == CTX_LOAD_ROM && p >= 0 && p != RP_BOOT) {
      fmt_snprintf(m, sizeof m, "No ROM files of %s here.\n\n"
                   "Copy ROM files to /c128/roms on this\nSD card. A line shows the files of its\n"
                   "size only.\n\nPress Space to continue.", sizes(p));
      return m;
   }
   if (ctx == CTX_LOAD_ROM && group == OPTM_G_REU_FILE)
      return "No .reu files here.\n\nCopy REU image files (.reu) to any\n"
             "folder of this SD card, for example\n/c128.\n\nPress Space to continue.";
   if (ctx == CTX_LOAD_ROM && group == OPTM_G_LOAD_CFG)
      return "No .cfg settings files here.\n\nA settings file has the format of\n"
             "/c128/settings.cfg, an Ultimate's.\n\nPress Space to continue.";
   if (ctx == CTX_LOAD_ROM && group == OPTM_G_HD_IMAGE)
      return "No CMD HD images here.\n\nCopy CMD HD disk images (.dhd, .hda)\nto /c128/hd on this SD card, or press\n"
             "F5 to make a new one.\n\nPress Space to continue.";
   if (ctx == CTX_LOAD_ROM)
      return "No .crt cartridge files here.\n\nCopy C64 or C128 .crt files to any\n"
             "folder of this SD card, for example\n/c128/carts.\n\nPress Space to continue.";
   if (ctx == CTX_MOUNT_DISKIMG)
      return "This core uses D64, D71 and D81 disk\nimages.\n\n"
             "Please copy at least one D64, D71 or D81\nfile to any sub-directory or to the root\n"
             "directory of this SD card.\n\nThe file browser starts in /c128/disks,\n"
             "else in /c128.\n\nYou can use long file names and you can\n"
             "also use nested sub-directories to nicely\norder your collection of disk images.\n\n"
             "Nothing to browse.\n\nPress Space to continue.";
   return NULL;
}

// --- F5 in a drive's browser: a new, empty disk, as Gideon's Create D64/D71/D81 Image ------

static const char *const disk_ext[] = { ".d64", ".d71", ".d81" };

// The drive of a Disk line, or -1
static int disk_drive(int ctx, int group)
{
   if (ctx != CTX_MOUNT_DISKIMG)
      return -1;
   return group == OPTM_G_MOUNT_D8 ? 0 : group == OPTM_G_MOUNT_D9 ? 1 : -1;
}

// What the name's extension asks for, else what the drive runs as (its line's 1541/1571/1581)
static int disk_kind(int d, const char *typed)
{
   int n = strlen(typed);
   for (int k = DISK_D64; k <= DISK_D81; k++)
      if (n > 4 && !strcasecmp(typed + n - 4, disk_ext[k]))
         return k;
   return stores_drive_type(d);
}

// --- F5 in the CMD HD's browser: a new image, "name,MB" -----------------------------------

#define HD_NEW_MB  256                  // the size of a new image, if the name says none
#define HD_MIN_MB  16
#define HD_MAX_MB  4000                 // FAT32 files stop short of 4 GB

static int hd_new(int ctx, int group)
{
   return ctx == CTX_LOAD_ROM && group == OPTM_G_HD_IMAGE;
}

// The size a typed name asks for, in MB (0: not a size), and the name's length without it
static int hd_size(const char *typed, int *len)
{
   const char *comma = strrchr(typed, ',');
   int mb = 0;
   *len = strlen(typed);
   if (!comma)
      return HD_NEW_MB;
   for (const char *c = comma + 1; *c; c++) {
      if (*c < '0' || *c > '9' || mb > HD_MAX_MB)
         return 0;
      mb = mb * 10 + *c - '0';
   }
   *len = comma - typed;
   return mb >= HD_MIN_MB && mb <= HD_MAX_MB ? mb : 0;
}

const char *core_new_file_label(int ctx, int group, const char *typed)
{
   static const char *const label[] = { "New D64 disk", "New D71 disk", "New D81 disk" };
   static char hd_label[32];
   int d = disk_drive(ctx, group), len, mb;
   if (hd_new(ctx, group)) {
      mb = hd_size(typed, &len);
      if (!mb)
         return "New CMD HD: 16 to 4000 MB";
      fmt_snprintf(hd_label, sizeof hd_label, "New CMD HD, %d MB", mb);
      return hd_label;
   }
   return d < 0 ? NULL : label[disk_kind(d, typed)];
}

// The file is the name with its type's extension, characters FAT can't have made '_' (his
// fix_filename); the disk's header has the name without the extension ("NAME,ID")
const char *core_new_file(int ctx, int group, const char *folder, const char *typed, char *name, int size)
{
   static char m[64];
   char header[32], path[UCI_PATH_MAX + 40];
   if (hd_new(ctx, group)) {
      int len, mb = hd_size(typed, &len);
      if (!mb)
         return "The size after the comma: 16 to 4000 (MB).";
      fmt_snprintf(name, size, "%.*s", len < 60 ? len : 60, typed);
      if (len < 4 || strcasecmp(name + strlen(name) - 4, ".dhd"))
         strcat(name, ".dhd");
      for (char *c = name; *c; c++)
         if (strchr("\"*:/<>\\?|,", *c))
            *c = '_';
      fmt_snprintf(path, sizeof path, "%s/%s", folder, name);
      FRESULT r = cmdhd_create(path, (uint32_t)mb << 20);
      if (r == FR_OK)
         return NULL;
      if (r == FR_EXIST)
         return "There is a file of that name here already.";
      if (r == FR_DENIED)
         return "There is no room for it on the SD card.";
      if (r == FR_WRITE_PROTECTED)
         return "The SD card is write protected.";
      fmt_snprintf(m, sizeof m, "It could not be made: %s", uci_fr_text(r));
      return m;
   }
   int kind = disk_kind(disk_drive(ctx, group), typed), n = strlen(typed);
   if (n > 4 && !strcasecmp(typed + n - 4, disk_ext[kind]))
      n -= 4;
   fmt_snprintf(header, sizeof header, "%s", typed);
   header[n < (int)sizeof header ? n : (int)sizeof header - 1] = 0;
   fmt_snprintf(name, size, "%s%s", header, disk_ext[kind]);
   for (char *c = name; *c; c++)
      if (strchr("\"*:/<>\\?|,", *c))
         *c = '_';
   fmt_snprintf(path, sizeof path, "%s/%s", folder, name);
   FRESULT r = diskimg_create(path, kind, header);
   if (r == FR_OK)
      return NULL;
   if (r == FR_EXIST)
      return "There is a file of that name here already.";
   if (r == FR_DENIED)
      return "There is no room for it on the SD card.";
   if (r == FR_WRITE_PROTECTED)
      return "The SD card is write protected.";
   fmt_snprintf(m, sizeof m, "It could not be made: %s", uci_fr_text(r));
   return m;
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

// --- Load settings ------------------------------------------------------------------------

#define MSG_W    42                  // the message screen's width, inside its frame
#define MSG_ROWS 20                  // the log's lines there, at most

static char cfg_log[640];
static int  cfg_n;

static void cfg_logf(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   int n = fmt_vsnprintf(cfg_log + cfg_n, sizeof cfg_log - cfg_n, fmt, ap);
   va_end(ap);
   cfg_n += n < (int)sizeof cfg_log - cfg_n ? n : (int)sizeof cfg_log - 1 - cfg_n;
}

// s into out, its lines broken at a space (or in a word too long) to fit MSG_W; after rows
// lines "..."
static void wrap(const char *s, char *out, int size, int rows)
{
   int n = 0, start = 0, space = -1;           // out's line starts at start, its last space
   for (; *s && rows > 0 && n < size - 5; s++) {
      out[n++] = *s;
      if (*s == '\n') {
         start = n;
         space = -1;
         rows--;
      } else if (*s == ' ')
         space = n - 1;
      else if (n - start > MSG_W) {
         if (space < start) {                 // no space: the last character onto the next line
            out[n] = out[n - 1];
            space = n++ - 1;
         }
         out[space] = '\n';
         start = space + 1;
         space = -1;
         rows--;
      }
   }
   int more = *s || (rows == 0 && n > start);
   if (rows == 0)
      n = start;
   strcpy(out + n, more ? "..." : "");
}

// As his Load Settings: the good lines take effect even if others have errors, whose log is
// the message. Kept at once, as a pick is.
static int settings_load(const char *path, const char **msg)
{
   static char text[sizeof cfg_log + MSG_ROWS + 64];
   FIL f;
   FRESULT res = f_open(&f, path, FA_READ);
   if (res != FR_OK) {
      *msg = uci_fr_text(res);
      return 0x100 + res;
   }
   stores_begin();
   cfg_n = 0;
   int good = stores_read(&f, ~0u, cfg_logf);
   f_close(&f);
   stores_apply();
   stores_save();
   if (good)
      return 0;
   int n = fmt_snprintf(text, sizeof text, "\nThere were errors; the rest is in effect.\n\n");
   wrap(cfg_log, text + n, sizeof text - n, MSG_ROWS);
   *msg = text;
   return 0x103;
}

// A .crt from the menu brings its own settings, as on an Ultimate (his FileTypeCRT)
void core_file_loaded(int group, const char *path)
{
   if (group == OPTM_G_MOUNT_CRT)
      stores_associated(path, 0);
}

// The core's own items (C_CRTROMS_MAN): the REU's File:%s, or a pick (the Boot line, the ROMs
// submenu). Either is kept in the settings file at once. And Load settings.
int core_crtrom_load(int dev, const char *path, const char **msg)
{
   int p = pick_of_dev(dev), err = 0;
   if (dev == DEV_CFG)
      return settings_load(path, msg);
   if (dev == DEV_HD) {
      // A mount turns the drive on, as a floppy's does
      if ((err = cmdhd_mount(path, msg)))
         return err;
      shell_force(OPTM_G_HD_ON, 0, 1);
      stores_save();
      return 0;
   }
   if (p < 0)
      err = reu_load(path, 1, msg);
   else {
      // Its name if it is in its folder, as his are; else its path
      const char *dir = rompick_dir(p), *name = path;
      int n = strlen(dir);
      if (!strncmp(path, dir, n) && path[n] == '/' && !strchr(path + n + 1, '/') &&
          strlen(path + n + 1) < PICK_NAME)
         name = path + n + 1;
      if (strlen(name) >= PICK_MAX) {
         *msg = "\n\nThe file's path is too long: 127\ncharacters at most.\n\nPress SPACE to continue.\n";
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
   if (dev == DEV_CFG)
      return 0;
   if (dev == DEV_HD)
      return *cmdhd_path() != 0;
   return p < 0 ? reu_file_picked() : rompick_state(p) == RS_FILE;
}

// Before the C128 starts: the settings file's items (the network's in effect once it
// starts), the ROMs picked, and the REU's image into the REU, as his REU Preload. The boot
// cartridge loads later, once the C128 runs (rompick_poll).
const char *core_prep_start(void)
{
   const char *msg;
   input_release_all();               // the input device as the firmware starts: nothing held
   cmdhd_init();
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

void core_save_settings(void)
{
   stores_save();
}

// "Help menu on F13" lets the C128 have its Help key
uint32_t core_menu_key(void)
{
   return shell_setting(OPTM_G_HELP_F13, 0) ? M2M_KEY_F13 : M2M_KEY_HELP;
}

// The menu's stream lines (net/stream.c), the boot cartridge's load (rompick.c), the Ultimate
// Command Interface (uci.c), the REST API (net/http.c), its input's timing (input.c), the
// FTP server (net/ftp.c) and the HDMI filter's coefficients (hdmi_filter.c)
// The CMD HD: the menu's Drive enabled and Bus ID (whatever set them: the menu, a settings
// file, REST), then its disk's commands
static void hd_poll(void)
{
   cmdhd_set_on(shell_setting(OPTM_G_HD_ON, 0));
   cmdhd_set_id(hd_id());
   cmdhd_poll();
}

DMEM_CODE void core_poll(void)
{
   hd_poll();
   hdmi_filter_poll();
   if (stream_dirty)
      stream_menu_sync();
   rompick_poll();
   uci_poll();
   http_poll();
   input_poll();
   ftp_poll();
}
