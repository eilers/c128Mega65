// The C128 core's part of the RISC-V Shell: the callbacks of CORE/m2m-rom/m2m-rom.asm.
// Menu items are found by their config.vhd group and their place in it.
#include <stddef.h>
#include <string.h>
#include "config_ids.h"
#include "core.h"
#include "m2m_rv.h"
#include "roms.h"

// Internal 1581: Off, Drive 8, Drive 9. Cartridge: Hardware, CRT file.
#define INT1581_OFF 0
#define CART_CRT    1

#define CRT_DEVICE     0x0106   // C_DEV_CRT (globals.vhd)
#define CRT_CSR_MAX_HI 0x020    // the largest .crt file it takes, in 64 kB (sw_cartridge_csr.vhd)

const char *core_submenu_summary(int idx)
{
   (void)idx;   // @TODO step 5: the 6r3 build's stall line on the Cartridge headline
   return NULL;
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

// A .crt file must fit the memory behind the cartridge device, which says how much that
// is: 2 MiB of HyperRAM on R3, 4 MiB of SDRAM on later boards (doc/hyperram-map.md). The
// parser in crt_parser.vhd checks the rest.
// @TODO step 5: disk images, by their exact size
int core_prep_load(uint32_t size, int ctx, int group, int *type, const char **msg)
{
   (void)group;
   *type = 0;
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
