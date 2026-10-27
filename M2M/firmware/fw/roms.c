// Auto-loaded ROMs, as CRTROM_AUTOLOAD reads them from globals.vhd through the sysinfo
// device: a record of four words per ROM (type, device or HyperRAM window, mandatory or
// optional, where its name starts in the names), then the names.
#include "roms.h"
#include "cfg.h"
#include "ff.h"
#include "m2m_rv.h"
#include "sdcard.h"
#include "shell.h"

#define CRTROM_MAN_NUM   0x000
#define CRTROM_AUT_NUM   0x001
#define CRTROM_MAN_BUF   0x100
#define CRTROM_AUT_BUF   0x200
#define CRTROM_AUT_NAMES 0x300
#define CRTROM_AUT_MAX   16

#define TYPE_DEVICE      0
#define TYPE_HYPERRAM    1
#define MODE_OPTIONAL    4

#define CSR_WINDOW       0xFFFFu
#define CSR_ST_IDLE      0

uint16_t crtrom_csr_get(uint16_t dev, int reg)
{
   M2M_RAMROM_DEV = dev;
   return M2M_DEV16[(CSR_WINDOW << 12) + reg];
}

void crtrom_csr_set(uint16_t dev, int reg, uint16_t v)
{
   M2M_RAMROM_DEV = dev;
   M2M_DEV16[(CSR_WINDOW << 12) + reg] = v;
}

// One byte per 16-bit word, at the start of a device, or of a HyperRAM window
static int load(FIL *f, int type, uint16_t target)
{
   static uint8_t buf[512];
   uint32_t at = 0;
   UINT got;

   do {
      if (f_read(f, buf, sizeof buf, &got) != FR_OK)
         return -1;
      volatile uint16_t *dst = (volatile uint16_t *)M2M_HYPERRAM + ((uint32_t)target << 12);
      if (type != TYPE_HYPERRAM) {
         M2M_RAMROM_DEV = target;
         dst = M2M_DEV16;
      }
      for (UINT i = 0; i < got; i++)
         dst[at++] = buf[i];
   } while (got == sizeof buf);
   return 0;
}

void roms_autoload(void)
{
   static const char err_mount[] = "This core needs to load one or more\n"
                                   "mandatory ROMs from SD card. But no SD\n"
                                   "card can be mounted.\n";
   static const char err_load[]  = "\n\nFile not found or file read error.\n"
                                   "The core needs this ROM to start.\n\n";
   int n = sysinfo_get(SYS_CRTSANDROMS, CRTROM_AUT_NUM);
   int mounted = sdcard_state() == SDCARD_OK;

   if (n > CRTROM_AUT_MAX)
      shell_fatal("Too many auto-load ROMs in globals.vhd\n", n);

   // The manually loaded ones' devices start idle
   for (int i = 0; i < (int)sysinfo_get(SYS_CRTSANDROMS, CRTROM_MAN_NUM); i++)
      if (sysinfo_get(SYS_CRTSANDROMS, CRTROM_MAN_BUF + 2 * i) == TYPE_DEVICE)
         crtrom_csr_set(sysinfo_get(SYS_CRTSANDROMS, CRTROM_MAN_BUF + 2 * i + 1), 0, CSR_ST_IDLE);

   for (int i = 0; i < n; i++) {
      int      base   = CRTROM_AUT_BUF + 4 * i;
      int      type   = sysinfo_get(SYS_CRTSANDROMS, base);
      uint16_t target = sysinfo_get(SYS_CRTSANDROMS, base + 1);
      int      opt    = sysinfo_get(SYS_CRTSANDROMS, base + 2) == MODE_OPTIONAL;
      int      start  = sysinfo_get(SYS_CRTSANDROMS, base + 3);
      char     name[64], msg[sizeof name + sizeof err_load];
      int      len = 0;
      FIL      f;

      if (!mounted) {
         if (!opt)
            shell_fatal(err_mount, 0);
         continue;
      }
      for (char c; len < (int)sizeof name - 1 &&
                   (c = sysinfo_get(SYS_CRTSANDROMS, CRTROM_AUT_NAMES + start + len)); len++)
         name[len] = c;
      name[len] = 0;

      FRESULT r = f_open(&f, name, FA_READ);
      int bad = r != FR_OK || load(&f, type, target) != 0;
      if (r == FR_OK)
         f_close(&f);
      if (bad && !opt) {
         int k = 0;
         for (const char *s = name; *s; s++)
            msg[k++] = *s;
         for (const char *s = err_load; *s; s++)
            msg[k++] = *s;
         msg[k] = 0;
         shell_fatal(msg, r);
      }
   }
}
