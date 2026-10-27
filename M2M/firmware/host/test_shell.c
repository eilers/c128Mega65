// The Shell (fw/shell.c, fw/menu.c and the core's callbacks) run on this Mac against
// config.vhd's real menu (CORE/scripts/run_config_nvc.sh dumps it), a FAT32 image with the
// ROMs and a settings file, and keys pressed one at a time.
// Run it twice on one image: the second run starts with the settings the first one saved.
// usage: test_shell config_dump.txt fat32.img [-v]
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cfg.h"
#include "ff.h"
#include "keys.h"
#include "m2m_rv.h"
#include "menu.h"
#include "sdcard.h"
#include "shell.h"

void host_init(void);
int  disk_file_open(const char *path);
extern uint32_t host_regs[];

static int fails, runs, verbose;
static char cfg_name[64];

static void check(int ok, const char *what)
{
   runs++;
   if (!ok) {
      fails++;
      printf("FAIL %s\n", what);
   }
}

// -------------------------------------------------------------------------------------
// The screen
// -------------------------------------------------------------------------------------

static uint16_t vram(int dev, int x, int y)
{
   M2M_RAMROM_DEV = dev;
   return M2M_DEV16[y * 45 + x];
}

// One row as text: arrows as < and >, marks as * and =, lines as - and |
static void row(int y, int x0, int x1, char *out)
{
   int n = 0;
   for (int x = x0; x < x1; x++) {
      uint16_t c = vram(M2M_DEV_VRAM_DATA, x, y);
      out[n++] = c == 17 ? '<' : c == 16 ? '>' : c == 7 ? '*' : c == 61 ? '=' :
                 c == 186 || c == 199 || c == 182 ? '|' : c >= 128 ? '-' : c < 32 ? ' ' : c;
   }
   out[n] = 0;
}

static void dump(const char *title)
{
   char r[48];
   if (!verbose)
      return;
   printf("--- %s\n", title);
   for (int y = 0; y < 33; y++) {
      row(y, 0, 45, r);
      printf("%2d %s%s\n", y, r, vram(M2M_DEV_VRAM_ATTR, 21, y) == 0x8B ? "  <- cursor" : "");
   }
}

static int find_row(const char *s)
{
   char r[48];
   for (int y = 0; y < 33; y++) {
      row(y, 0, 45, r);
      if (strstr(r, s))
         return y;
   }
   return -1;
}

static int cursor_row(void)
{
   for (int y = 0; y < 33; y++)
      if (vram(M2M_DEV_VRAM_ATTR, 21, y) == 0x8B)
         return y;
   return -1;
}

static int cursor_on(const char *s)
{
   char r[48];
   int y = cursor_row();
   if (y < 0)
      return 0;
   row(y, 20, 45, r);
   return strstr(r, s) != NULL;
}

// -------------------------------------------------------------------------------------
// Keys and time
// -------------------------------------------------------------------------------------

static void poll(int n)
{
   while (n--) {
      keys_poll();
      sdcard_poll();
      shell_poll();
   }
}

static void press(uint32_t key)
{
   M2M_KEYS = ~key & 0xFFFF;
   poll(3);
   M2M_KEYS = 0xFFFF;
   poll(3);
}

static void idle(void)   // past the third of a second after a menu closes
{
   poll(40000);
}

static int go_to(const char *s)
{
   for (int i = 0; i < 40; i++) {
      if (cursor_on(s))
         return 1;
      press(M2M_KEY_DOWN);
   }
   printf("no item \"%s\"\n", s);
   return 0;
}

static int item(const char *text)   // a menu index by its text
{
   for (int i = 0; i < menu.n; i++)
      if (!strcmp(menu.line[i], text))
         return i;
   printf("no item \"%s\" in the menu\n", text);
   return 0;
}

// -------------------------------------------------------------------------------------
// The card and config.vhd
// -------------------------------------------------------------------------------------

static void load_dump(const char *path)
{
   FILE *f = fopen(path, "r");
   unsigned dev, sel, idx, val;
   if (!f) {
      perror(path);
      exit(2);
   }
   while (fscanf(f, "%x %x %x %x", &dev, &sel, &idx, &val) == 4) {
      M2M_RAMROM_DEV = dev;
      M2M_DEV16[(sel << 12) + idx] = val;
   }
   fclose(f);
}

static uint8_t pattern(int file, int i)
{
   return (uint8_t)(i * 7 + file * 31 + 1);
}

static void put_file(const char *name, int file, int size)
{
   uint8_t b[512];
   FIL f;
   UINT put;
   check(f_open(&f, name, FA_WRITE | FA_CREATE_ALWAYS) == FR_OK, name);
   for (int at = 0; at < size; at += put) {
      int n = size - at < 512 ? size - at : 512;
      for (int i = 0; i < n; i++)
         b[i] = file < 0 ? 0xFF : pattern(file, at + i);
      if (f_write(&f, b, n, &put) != FR_OK || put == 0)
         break;
   }
   f_close(&f);
}

// The ROMs, and a new settings file (all 0xFF) unless an earlier run left one: its bytes
// go to file
static void make_card(uint8_t *file)
{
   static FATFS fs;
   FIL f;
   UINT got = 0;
   f_mount(&fs, "", 1);
   f_mkdir("/c128");
   put_file("/c128/boot0.rom", 0, 4100);
   put_file("/c128/boot1.rom", 1, 600);
   put_file("/c128/modem.bin", 2, 1000);
   cfg_str(CFG_CFG_FILE, cfg_name, sizeof cfg_name);
   if (f_open(&f, cfg_name, FA_READ) == FR_OK) {
      f_read(&f, file, MENU_MAX, &got);
      f_close(&f);
   } else
      put_file(cfg_name, -1, cfg_get(CFG_OPTM_ICOUNT, 0));
   if (got == 0)
      file[0] = 0xFF;
   f_unmount("");
}

static int device_holds(int dev, int file, int size)
{
   M2M_RAMROM_DEV = dev;
   for (int i = 0; i < size; i++)
      if (M2M_DEV16[i] != pattern(file, i))
         return 0;
   return 1;
}

// The settings file's byte for a menu item, read through the firmware's mount: mounting
// again would look like a new card, and the Shell would stop saving
static int saved(int idx)
{
   uint8_t b[MENU_MAX];
   FIL f;
   UINT got = 0;
   if (f_open(&f, cfg_name, FA_READ) == FR_OK) {
      f_read(&f, b, sizeof b, &got);
      f_close(&f);
   }
   return idx < (int)got ? b[idx] : -1;
}

// -------------------------------------------------------------------------------------

int main(int argc, char **argv)
{
   if (argc < 3 || !disk_file_open(argv[2])) {
      fprintf(stderr, "usage: test_shell config_dump.txt fat32.img [-v]\n");
      return 2;
   }
   verbose = argc > 3;
   host_init();
   load_dump(argv[1]);
   uint8_t file[MENU_MAX];
   make_card(file);
   M2M_CSR |= M2M_CSR_SD_DET_EXT | M2M_CSR_SD_ACTIVE;

   // Start: ROMs loaded, defaults set, the core out of reset with keyboard and joysticks
   shell_start();
   check(!(M2M_CSR & M2M_CSR_RESET), "core left reset");
   check((M2M_CSR & (M2M_CSR_KBD | M2M_CSR_JOY1 | M2M_CSR_JOY2)) == 0x38, "keyboard and joysticks on");
   check(device_holds(0x101, 0, 4100), "boot0.rom in its device");
   check(device_holds(0x102, 1, 600), "boot1.rom in its device");
   int modem = 1;
   for (int i = 0; i < 1000; i++)
      modem &= host_hr[0x1C0 * 4096 + i] == pattern(2, i);
   check(modem, "modem.bin in HyperRAM, a byte per word");
   M2M_RAMROM_DEV = M2M_DEV_ASCAL_PPHASE;
   check(M2M_DEV16[0] == 0x03E8 && M2M_DEV16[0x100] != 0, "ascal filters loaded");
   check(M2M_CSR & M2M_CSR_ASCAL_AUTO, "ascal follows the core");
   int want = 1;
   for (int i = 0; i < menu.n; i++)
      want &= setting_get(i) == (file[0] == 0xFF ? cfg_get(CFG_OPTM_STDSEL, i) : file[i]);
   check(want, file[0] == 0xFF ? "settings are config.vhd's defaults (new settings file)" :
                                 "settings from the settings file");
   if (file[0] != 0xFF)
      check(!setting_get(item(" Use IEC port")), "IEC port off, as the first run left it");
   // Back to the defaults for the checks below
   M2M_CONTROL_M(0) = M2M_CONTROL_M(1) = M2M_CONTROL_M(2) = M2M_CONTROL_M(3) = 0;
   for (int i = 0; i < menu.n; i++)
      setting_set(i, cfg_get(CFG_OPTM_STDSEL, i));

   // The menu, top right
   press(M2M_KEY_HELP);
   dump("menu");
   check(M2M_CSR & M2M_CSR_OSM, "Help opens the menu");
   check(M2M_OSM_XY == (20u << 8) && M2M_OSM_DXDY == (25u << 8 | 30), "menu window 25x30 at 20,0");
   check(!(M2M_CSR & M2M_CSR_KBD), "keyboard off while the menu is open");
   check(find_row(" Drives") == 1 && vram(M2M_DEV_VRAM_ATTR, 22, 1) == 0x0E, "headline Drives");
   check(find_row(" 8:<Mount Drive>") == 3 && cursor_row() == 3, "cursor on drive 8 (the start item)");
   check(find_row(" Model: 1571") > 0, "drive model summary");
   check(find_row(" Video: Follow 40/80") > 0, "video summary");
   check(find_row(" Cartridge: Hardware") > 0, "cartridge summary");
   check(find_row(" REU: Off") > 0, "REU summary");

   // Into a submenu, pick, and back
   go_to("Model");
   press(M2M_KEY_RETURN);
   dump("drive model submenu");
   check(find_row("5.25 inch drive") == 1, "submenu headline");
   check(cursor_on(" 1541"), "cursor on the submenu's first item");
   check(find_row("*1571") > 0 && find_row("*1541") < 0, "1571 marked");
   press(M2M_KEY_RETURN);
   check(find_row("*1541") > 0 && find_row("*1571") < 0, "1541 marked, 1571 not");
   check(setting_get(item(" 1541")) && !setting_get(item(" 1571")), "1541 set at once");
   press(M2M_KEY_RUNSTOP);
   check(find_row(" Model: 1541") > 0 && cursor_on("Model"), "back in the main menu");

   // Close: saved, keyboard back after a moment
   press(M2M_KEY_HELP);
   check(!(M2M_CSR & M2M_CSR_OSM), "Help closes the menu");
   idle();
   check(M2M_CSR & M2M_CSR_KBD, "keyboard back after closing");
   check(saved(item(" 1541")) == 1 && saved(item(" 1571")) == 0 && saved(0) == 0, "settings file saved");
   press(M2M_KEY_HELP);
   check(cursor_on("Model"), "cursor remembered");

   // Help screen
   go_to("Credits");
   press(M2M_KEY_RETURN);
   dump("credits");
   check(M2M_OSM_XY == 0 && M2M_OSM_DXDY == (45u << 8 | 33), "credits on the whole screen");
   check(find_row("Commodore 128 for MEGA65") == 2, "credits text");
   check(cursor_row() < 0, "no menu cursor on the credits");
   press(M2M_KEY_SPACE);
   check(M2M_OSM_XY == (20u << 8) && cursor_on("Credits") && find_row("=Credits") < 0,
         "back in the menu, Credits not selected");
   check(!setting_get(item(" Credits")), "Credits not set");

   // F13 instead of Help
   go_to("Help menu on F13");
   press(M2M_KEY_RETURN);
   check(find_row("=Help menu on F13") > 0 && setting_get(item(" Help menu on F13")), "F13 option on");
   press(M2M_KEY_F13);
   idle();
   press(M2M_KEY_HELP);
   check(!(M2M_CSR & M2M_CSR_OSM), "Help no longer opens the menu");
   press(M2M_KEY_F13);
   check(M2M_CSR & M2M_CSR_OSM, "F13 opens it");
   press(M2M_KEY_RETURN);
   check(!setting_get(item(" Help menu on F13")), "F13 option off");

   // No file browser yet: the CRT and drive items change nothing
   go_to("Cartridge");
   press(M2M_KEY_RETURN);
   go_to("CRT:");
   press(M2M_KEY_RETURN);
   dump("cartridge submenu");
   check(setting_get(item(" Hardware")) && !setting_get(item(" CRT file")), "cartridge source unchanged");
   check(find_row(" CRT:<Load>") > 0 && find_row("=CRT:") < 0, "CRT item not left selected");
   press(M2M_KEY_RUNSTOP);
   go_to("8:");
   press(M2M_KEY_RETURN);
   check(find_row(" 8:<Mount Drive>") > 0 && !setting_get(item(" 8:%s")), "drive item unchanged");

   // R3-memory builds have no 2 MB and 16 MB REU
   go_to("REU");
   press(M2M_KEY_RETURN);
   int r3 = menu.n < 118;
   check(find_row(" 512 KB") > 0 && (find_row(" 2 MB") > 0) != r3 && (find_row(" 16 MB") > 0) != r3,
         r3 ? "REU: Off and 512 KB" : "REU: Off, 512 KB, 2 MB, 16 MB");
   press(M2M_KEY_RUNSTOP);

   // Close Menu: next time from the start item
   go_to("Close Menu");
   press(M2M_KEY_RETURN);
   check(!(M2M_CSR & M2M_CSR_OSM), "Close Menu closes");
   idle();
   press(M2M_KEY_HELP);
   check(cursor_on("8:"), "cursor back on the start item");

   // Leave the drive model as it was and the IEC port off, for the next run
   go_to("Model");
   press(M2M_KEY_RETURN);
   go_to("1571");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_RUNSTOP);
   go_to("Use IEC port");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_HELP);
   idle();
   check(saved(item(" Use IEC port")) == 0 && saved(item(" 1571")) == 1, "saved for the next run");

   printf("%s: %d of %d checks passed\n", argv[1], runs - fails, runs);
   return fails != 0;
}

void host_halt(void)
{
   verbose = 1;
   dump("halted");
   printf("FAIL the firmware halted\n");
   exit(1);
}
