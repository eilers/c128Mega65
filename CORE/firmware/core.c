// The C128 core's part of the RISC-V Shell: the callbacks of CORE/m2m-rom/m2m-rom.asm.
// Menu items are found by their config.vhd group and their place in it.
#include <stddef.h>
#include "config_ids.h"
#include "core.h"
#include "m2m_rv.h"

// Internal 1581: Off, Drive 8, Drive 9. Cartridge: Hardware, CRT file.
#define INT1581_OFF 0
#define CART_CRT    1

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

const char *core_prep_start(void)
{
   return NULL;
}

// "Help menu on F13" lets the C128 have its Help key
uint32_t core_menu_key(void)
{
   return shell_setting(OPTM_G_HELP_F13, 0) ? M2M_KEY_F13 : M2M_KEY_HELP;
}
