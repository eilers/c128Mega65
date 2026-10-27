// The C128 core's part of the RISC-V Shell: the callbacks of CORE/m2m-rom/m2m-rom.asm.
// Menu items are found by their config.vhd group and their place in it.
#include <stddef.h>
#include <string.h>
#include "config_ids.h"
#include "core.h"
#include "fmt.h"
#include "m2m_rv.h"
#include "roms.h"

// Internal 1581: Off, Drive 8, Drive 9. Cartridge: Hardware, CRT file.
#define INT1581_OFF 0
#define CART_CRT    1

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

   // The Cartridge submenu's first menu group is the cartridge source
   while ((g = shell_group(++idx)) == 0)
      ;
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
   (void)group;
   (void)item;
}

// Disk images for the drives, .crt files for the cartridge; folders always
int core_filter_file(const char *name, int dir, int ctx, int group)
{
   static const char *const disk[] = { ".D64", ".D71", ".D81", NULL }, *const crt[] = { ".CRT", NULL };
   const char *const *ext = ctx == CTX_MOUNT_DISKIMG ? disk : ctx == CTX_LOAD_ROM ? crt : NULL;
   int len = strlen(name);
   (void)group;

   if (dir || !ext)
      return 0;
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
   (void)group;
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
   if (ctx == CTX_LOAD_ROM && size > (uint32_t)crtrom_csr_get(CRT_DEVICE, CRT_CSR_MAX_HI) << 16) {
      *msg = "\n\nThis .crt file is too large. A virtual\ncartridge can be up to 4 MB, or 2 MB on\n"
             "an R3 board.\n\nPress SPACE to continue.\n";
      return 1;
   }
   return 0;
}

// The folder the user browsed into holds nothing for this menu item
const char *core_custom_msg(int situation, int ctx)
{
   if (situation != CMSG_BROWSENOTHING)
      return NULL;
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

const char *core_prep_start(void)
{
   return NULL;
}

// "Help menu on F13" lets the C128 have its Help key
uint32_t core_menu_key(void)
{
   return shell_setting(OPTM_G_HELP_F13, 0) ? M2M_KEY_F13 : M2M_KEY_HELP;
}
