// The ROMs picked in place of the stock ones (rompick.h)
#include <string.h>
#include <strings.h>
#include "c128dma.h"
#include "config_ids.h"
#include "core.h"
#include "ff.h"
#include "fmt.h"
#include "m2m_rv.h"
#include "roms.h"
#include "rompick.h"
#include "stores.h"
#include "uci.h"

#define SYS_ROM    0x0101            // C_DEV_SYSTEM_ROM: boot0.rom, the function ROM from FUNC_AT
#define DRIVE_ROM  0x0102            // C_DEV_DRIVE_ROM: boot1.rom, six 32 KB banks
#define CRT_DEVICE 0x0106            // C_DEV_CRT, the CRT:%s item's
#define BOOT_0     "/c128/boot0.rom" // globals.vhd's BOOT_0 and BOOT_1
#define BOOT_1     "/c128/boot1.rom"
#define FUNC_AT    0x18000
#define BANK       32768
#define K8         1                 // the sizes a pick takes
#define K16        2
#define K32        4
#define SYS_PICKS  ((1u << RP_DRIVE) - 1)
#define PICK_PATH  (sizeof CARTS_DIR + PICK_MAX + 1)   // the longer folder, /, the name

// Where each system pick goes in boot0.rom (MiSTer's order): ROM1 C64 BASIC + KERNAL, ROM4 C128
// editor + Z80 BIOS + KERNAL, ROM2+3 C128 BASIC, the character ROM; the function ROM after it
static const struct { uint32_t at; uint8_t sizes; } sys_part[RP_DRIVE] = {
   { 0x04000, K16 },                 // C128 KERNAL: all of ROM4
   { 0x08000, K32 },                 // C128 BASIC
   { 0x02000, K8 | K16 },            // C64 KERNAL; 16 KB is all of ROM1, from 0x00000
   { 0x00000, K8 },                  // C64 BASIC
   { 0x10000, K8 },                  // characters, both sets
   { FUNC_AT, K16 | K32 },           // internal function ROM
};
static const char *const drive_defaults[] = { "1541.rom", "1571.rom", "1581.rom" };

static char    names[RP_COUNT][PICK_MAX] = {
   [RP_DRV(0, 0)] = "1541.rom", [RP_DRV(0, 1)] = "1571.rom", [RP_DRV(0, 2)] = "1581.rom",
   [RP_DRV(1, 0)] = "1541.rom", [RP_DRV(1, 1)] = "1571.rom", [RP_DRV(1, 2)] = "1581.rom",
};
static uint8_t state[RP_COUNT];
static int     started;              // rompick_boot has run
static int     cart_due;             // the boot cartridge, once the C128 runs (rompick_poll)

const char *rompick_name(int p)    { return names[p]; }
const char *rompick_default(int p) { return p >= RP_DRIVE && p < RP_BOOT ? drive_defaults[(p - RP_DRIVE) % 3] : ""; }
int         rompick_state(int p)   { return state[p]; }
const char *rompick_dir(int p)     { return p == RP_BOOT ? CARTS_DIR : ROMS_DIR; }

// A 1541 also takes 8 KB: its $E000 half, as JiffyDOS for the original 1541
int rompick_fits(int p, uint32_t size)
{
   int sizes = p == RP_BOOT ? 0 : p < RP_DRIVE ? sys_part[p].sizes :
               (p - RP_DRIVE) % 3 == 0 ? K8 | K16 | K32 : K16 | K32;
   return p == RP_BOOT ? size > 0 :
          (size == 8192 && (sizes & K8)) || (size == 16384 && (sizes & K16)) || (size == 32768 && (sizes & K32));
}

static int is_crt(const char *name)
{
   int n = strlen(name);
   return n > 4 && !strcasecmp(name + n - 4, ".crt");
}

void rompick_list(int p, void (*each)(const char *name, void *ctx), void *ctx)
{
   DIR d;
   FILINFO fi;
   if (f_opendir(&d, rompick_dir(p)) != FR_OK)
      return;
   while (f_readdir(&d, &fi) == FR_OK && fi.fname[0])
      if (!(fi.fattrib & (AM_DIR | AM_HID)) && strlen(fi.fname) < PICK_MAX &&
          (p == RP_BOOT ? is_crt(fi.fname) : rompick_fits(p, fi.fsize)))
         each(fi.fname, ctx);
   f_closedir(&d);
}

static void path_of(int p, char *out)
{
   fmt_snprintf(out, PICK_PATH, "%s/%s", rompick_dir(p), names[p]);
}

// Stock: no name, or a drive's default name with no such file (his 1541.rom is the stock ROM)
static int stock(int p)
{
   char path[PICK_PATH];
   FILINFO fi;
   if (!names[p][0])
      return 1;
   if (p < RP_DRIVE || p == RP_BOOT || strcasecmp(names[p], rompick_default(p)))
      return 0;
   path_of(p, path);
   return f_stat(path, &fi) != FR_OK;
}

// A drive pick's bank: boot1.rom's for drive d, model t
static uint32_t bank_at(int p)
{
   int d = (p - RP_DRIVE) / 3, t = (p - RP_DRIVE) % 3;
   return (uint32_t)(t * 2 + d) * BANK;
}

// n bytes of the file from where it is, a byte per 16-bit word of the device from at on
static int put(FIL *f, uint16_t dev, uint32_t at, uint32_t n)
{
   uint8_t b[512];
   UINT got;
   while (n) {
      UINT want = n < sizeof b ? n : sizeof b;
      if (f_read(f, b, want, &got) != FR_OK || got != want)
         return -1;
      M2M_RAMROM_DEV = dev;
      for (UINT i = 0; i < got; i++)
         M2M_DEV16[at++] = b[i];
      n -= got;
   }
   return 0;
}

// The stock ROM in a pick's part again, from boot0.rom or boot1.rom
static void put_stock(int p)
{
   int drive = p >= RP_DRIVE;
   uint32_t at = drive ? bank_at(p) : p == RP_K64 ? 0 : sys_part[p].at;
   uint32_t n = drive ? BANK : p == RP_K64 ? 16384 : sys_part[p].sizes & K32 ? 32768 : sys_part[p].sizes & K16 ? 16384 : 8192;
   FIL f;
   if (p == RP_FUNC || f_open(&f, drive ? BOOT_1 : BOOT_0, FA_READ) != FR_OK)
      return;
   if (f_lseek(&f, at) == FR_OK)
      put(&f, drive ? DRIVE_ROM : SYS_ROM, at, n);
   f_close(&f);
}

// The pick's file into its part: its size, or 0 if it won't load
static uint32_t put_file(int p)
{
   char path[PICK_PATH];
   FIL f;
   path_of(p, path);
   if (f_open(&f, path, FA_READ) != FR_OK)
      return 0;
   uint32_t size = f_size(&f), at = p >= RP_DRIVE ? bank_at(p) : sys_part[p].at;
   uint16_t dev = p >= RP_DRIVE ? DRIVE_ROM : SYS_ROM;
   if (p == RP_K64 && size == 16384)
      at = 0;
   int ok = rompick_fits(p, size);
   if (ok && p >= RP_DRIVE && size == 8192) {   // a 1541's $E000 half over the stock $C000 half
      put_stock(p);
      at += 0x2000;
   }
   ok = ok && !put(&f, dev, at, size);
   // A 16 KB drive ROM twice, as his, and so the 8 KB half; a 16 KB function ROM too, as VICE
   // and a 16 KB EPROM in the C128's 32 KB socket have it (MiSTer leaves $C000 open instead)
   if (ok && ((p >= RP_DRIVE && size <= 16384) || (p == RP_FUNC && size == 16384)))
      ok = f_lseek(&f, 0) == FR_OK && !put(&f, dev, at + 16384, size);
   f_close(&f);
   return ok ? size : 0;
}

// One pick into its part: stock, the file, or stock again for a file that won't load
static void apply_one(int p)
{
   uint32_t size = 0;
   if (stock(p))
      state[p] = RS_STOCK;
   else
      state[p] = (size = put_file(p)) ? RS_FILE : RS_FAILED;
   if (p == RP_FUNC)
      crtrom_csr_set(SYS_ROM, 0, size ? 2 : 0);          // 32 KB, or 16 KB twice
   else if (state[p] != RS_FILE && (started || state[p] == RS_FAILED))
      put_stock(p);
}

static int crt_item(void)
{
   for (int k = 0; k < crtrom_count(); k++)
      if (crtrom_dev(k) == CRT_DEVICE)
         return k;
   return -1;
}

// The boot cartridge: named here, loaded once the C128 runs by boot_cart
static void check_cart(void)
{
   char path[PICK_PATH];
   FILINFO fi;
   path_of(RP_BOOT, path);
   state[RP_BOOT] = !names[RP_BOOT][0] ? RS_STOCK :
                    f_stat(path, &fi) == FR_OK && !(fi.fattrib & AM_DIR) ? RS_FILE : RS_FAILED;
}

static void boot_cart(void)
{
   char path[PICK_PATH];
   const char *msg;
   int k = crt_item();
   check_cart();
   if (state[RP_BOOT] != RS_FILE)
      return;
   if (k < 0) {
      state[RP_BOOT] = RS_FAILED;
      return;
   }
   path_of(RP_BOOT, path);
   core_sel_pre(OPTM_G_MOUNT_CRT, 0);                  // the virtual cartridge is the source
   int err = crtrom_load_quiet(k, path, OPTM_G_MOUNT_CRT, &msg);
   shell_crtrom_named(OPTM_G_MOUNT_CRT, err ? NULL : path);
   if (err)
      state[RP_BOOT] = RS_FAILED;
}

// Its menu line: the file's name, or (core.c) Stock, None or <Error>
static void show(int p)
{
   char path[PICK_PATH];
   path_of(p, path);
   shell_crtrom_named(p == RP_BOOT ? OPTM_G_CRT_BOOT : OPTM_G_ROM_K128 + p, state[p] == RS_FILE ? path : NULL);
}

void rompick_set(int p, const char *name)
{
   strncpy(names[p], name, PICK_MAX - 1);
   names[p][PICK_MAX - 1] = 0;
}

void rompick_unset(int p)
{
   rompick_set(p, rompick_default(p));
   if (!stock(p))
      rompick_set(p, "");                              // a file has the default's name
}

// A running Z80 takes no DMA: the C128 is reset, and its Z80 boot hands the bus to the 8502.
// The 8502 must have it for 1 ms, since the Z80 also lets go of it in the VIC's bad lines.
static void reset_to_8502(void)
{
   uci_reboot();
   uint32_t start = MTIME_LO, since = start;
   while (MTIME_LO - start < CPU_HZ / 2) {
      if (dma_z80())
         since = MTIME_LO;
      else if (MTIME_LO - since >= CPU_HZ / 1000)
         return;
   }
}

void rompick_apply(uint32_t changed)
{
   static const uint8_t sys_order[] = { RP_K64, RP_B64, RP_K128, RP_B128, RP_CHAR };   // a 16 KB C64 KERNAL first
   if (!started)
      return;
   if (changed & SYS_PICKS) {
      // The bus DMA holds the 8502 (at its next read), then the reset button's reset restarts
      // the C128 and ends the hold. Not M2M_CSR's reset: that resets the whole core, the
      // command interface's registers too.
      if (dma_z80())
         reset_to_8502();
      dma_hold(1);
      if (changed & SYS_PICKS & ~(1u << RP_FUNC)) {
         FIL f;
         if (f_open(&f, BOOT_0, FA_READ) == FR_OK) {
            put(&f, SYS_ROM, 0, FUNC_AT < f_size(&f) ? FUNC_AT : f_size(&f));
            f_close(&f);
         }
         for (unsigned i = 0; i < sizeof sys_order; i++)
            if (!stock(sys_order[i]))
               apply_one(sys_order[i]);
            else
               state[sys_order[i]] = RS_STOCK;
      }
      if (changed & 1u << RP_FUNC)
         apply_one(RP_FUNC);
      uci_reboot();
   }
   for (int p = RP_DRIVE; p < RP_BOOT; p++) {
      int d = (p - RP_DRIVE) / 3, t = (p - RP_DRIVE) % 3;
      if (!(changed & 1u << p))
         continue;
      apply_one(p);
      // The drive's model: it reloads its bank and resets, or (off) does once it is on
      if (stores_drive_type(d) == t)
         crtrom_csr_set(DRIVE_ROM, 0, 1 << d);
   }
   if (changed & 1u << RP_BOOT)
      check_cart();
   for (int p = 0; p < RP_COUNT; p++)
      if (changed & 1u << p)
         show(p);
}

void rompick_boot(void)
{
   // boot0.rom and boot1.rom are fresh; the drives fetch their banks once the C128 starts
   for (int p = 0; p < RP_BOOT; p++)
      apply_one(p);
   check_cart();
   cart_due = state[RP_BOOT] == RS_FILE;
   started = 1;
   for (int p = 0; p < RP_COUNT; p++)
      show(p);
}

// Loaded as the menu loads one, with the C128 running (main.vhd restarts it into the
// cartridge): loaded while the whole core was held in the start's reset, a C64 cartridge
// could leave the C128 frozen until the next reset
void rompick_poll(void)
{
   if (!cart_due)
      return;
   cart_due = 0;
   boot_cart();
   show(RP_BOOT);
}
