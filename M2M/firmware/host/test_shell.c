// The Shell (fw/shell.c, fw/menu.c and the core's callbacks) run on this Mac against
// config.vhd's real menu (CORE/scripts/run_config_nvc.sh dumps it), a FAT32 image with the
// ROMs and a settings file, and keys pressed one at a time.
// Run it twice on one image: the second run starts with the settings the first one saved.
// usage: test_shell config_dump.txt fat32.img [-v]
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cfg.h"
#include "config_ids.h"
#include "core.h"
#include "ff.h"
#include "keys.h"
#include "m2m_rv.h"
#include "menu.h"
#include "reu.h"
#include "roms.h"
#include "sdcard.h"
#include "shell.h"

void host_init(void);
int  disk_file_open(const char *path);
extern uint32_t host_regs[];

static int fails, runs, verbose;
static char cfg_name[64];
static const char *image;
static int cluster;                      // bytes

// The cartridge device (C_DEV_CRT) and its CSR (sw_cartridge_csr.vhd, sd_dma.vhd)
#define CRT_DEV      0x0106
#define UCI_DEV      0x0109
#define UCI_REU_MEM  0x011         // where the REU's memory is: bit 15 SDRAM, then its 8 KiB window
#define CSR_STATUS   0x000
#define CSR_FS_LO    0x001
#define CSR_FS_HI    0x002
#define CSR_PARSEST  0x010
#define CSR_PARSEE1  0x011
#define CSR_ERR_STR  0x100
#define CSR_MAX_HI   0x020
#define CSR_STALLS   0x021
#define CSR_STALL_MAX 0x022
#define CSR_STATS    0x023
#define SDL_MAGIC    0x030
#define SDL_BLK_LO   0x032
#define SDL_BLK_HI   0x033
#define SDL_COUNT    0x034
#define SDL_LEN_LO   0x037
#define SDL_LEN_HI   0x038

// The drives: vdrives.vhd and the vd_sd.vhd of drive 8 and 9
#define VD_DEV       0x0103
#define D8_DEV       0x0104
#define D9_DEV       0x0105
#define VD_IMG_MOUNT 0x000
#define VD_SIZE_L    0x002
#define VD_SIZE_H    0x003
#define VD_TYPE      0x004
#define VD_DRV_MOUNT 0x00A
#define MAP_CTRL     0x031
#define MAP_NBLK     0x032

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

// Disk images: the first two bytes of each block are its number
static uint8_t disk_byte(int file, int i)
{
   return i % 512 < 2 ? (uint8_t)(i / 512 >> 8 * (i % 512)) : pattern(file, i);
}

// With chunk, written a chunk at a time in turn with a filler file, so that its clusters
// lie in runs of that size on the card
static void put_disk(const char *name, int file, int size, const char *filler, int chunk)
{
   uint8_t b[512];
   FIL f, g;
   UINT put;
   check(f_open(&f, name, FA_WRITE | FA_CREATE_ALWAYS) == FR_OK, name);
   if (chunk)
      f_open(&g, filler, FA_WRITE | FA_CREATE_ALWAYS);
   for (int at = 0; at < size;) {
      int n = size - at < 512 ? size - at : 512;
      for (int i = 0; i < n; i++)
         b[i] = disk_byte(file, at + i);
      f_write(&f, b, n, &put);
      at += n;
      for (int i = 0; chunk && at % chunk == 0 && i < chunk; i += 512)
         f_write(&g, b, 512, &put);
   }
   if (chunk)
      f_close(&g);
   f_close(&f);
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
   put_file("/c128/game.crt", 3, 70000);
   put_file("/c128/big.crt", 4, 140000);
   f_mkdir("/c128/Sub");
   put_file("/c128/Sub/x.crt", 5, 3000);
   put_file("/c128/Sub/notes.txt", 6, 10);
   put_file("/c128/Sub/A cartridge with a name far too long for one line.crt", 7, 100);
   put_file("/c128/ram.reu", 13, 70000);
   put_disk("/c128/game.d64", 8, 174848, NULL, 0);
   put_file("/c128/bad.d64", 9, 1000);
   put_disk("/c128/frag.d71", 10, 349696, "/c128/filler1.bin", 4096);
   put_disk("/c128/split.d64", 11, 174848, "/c128/filler2.bin", 2048);
   put_disk("/c128/Sub/disk.d81", 12, 819200, NULL, 0);
   cluster = fs.csize * 512;
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

static uint16_t vd_reg(int reg)
{
   M2M_RAMROM_DEV = VD_DEV;
   return M2M_DEV16[reg];
}

// vdrives.vhd as the core sees it: a strobed drive is mounted if the size is not 0
static void vdrives_hw(void)
{
   volatile uint16_t *vd = host_dev_at(VD_DEV);
   for (int d = 0; d < 2; d++)
      if (vd[VD_IMG_MOUNT] >> d & 1)
         vd[VD_DRV_MOUNT] = (vd[VD_DRV_MOUNT] & ~(1u << d)) | ((vd[VD_SIZE_L] | vd[VD_SIZE_H]) != 0) << d;
}

static uint32_t piece_at(int dev, int p)
{
   M2M_RAMROM_DEV = dev;
   return M2M_DEV16[2 * p] | (uint32_t)M2M_DEV16[2 * p + 1] << 16;
}

// Every block of the image on the card where the drive's table says
static int table_right(int dev, int file, int size)
{
   uint8_t blk[512];
   FILE *img = fopen(image, "rb");
   int ok = img != NULL;
   for (int at = 0; ok && at < size; at += 512) {
      long lba = piece_at(dev, at / 4096) + at % 4096 / 512;
      ok = !fseek(img, lba * 512, SEEK_SET) && fread(blk, 512, 1, img) == 1;
      for (int i = 0; ok && i < 512 && at + i < size; i++)
         ok = blk[i] == disk_byte(file, at + i);
   }
   if (img)
      fclose(img);
   return ok;
}

static int in_pieces(int dev, int size)
{
   for (int p = 0; (p + 1) * 4096 < size; p++)
      if (piece_at(dev, p + 1) != piece_at(dev, p) + 8)
         return 1;
   return 0;
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
   image = argv[2];
   host_init();
   host_dev_hook = vdrives_hw;
   load_dump(argv[1]);
   crtrom_csr_set(D8_DEV, 0x030, 0x5D4D);             // the drives map their images
   crtrom_csr_set(D9_DEV, 0x030, 0x5D4D);
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
   check(M2M_OSM_XY == (20u << 8) && M2M_OSM_DXDY == (25u << 8 | 31), "menu window 25x31 at 20,0");
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

   // A .crt file: the browser, filtered and sorted, starts in /c128; the CPU copies it in
   // (the device says it has no DMA), the device parses it, the menu shows its name
   crtrom_csr_set(CRT_DEV, CSR_MAX_HI, 2);            // takes up to 128 KB
   crtrom_csr_set(CRT_DEV, CSR_PARSEST, 2);           // its parser: OK
   go_to("Cartridge");
   press(M2M_KEY_RETURN);
   go_to("CRT:");
   press(M2M_KEY_RETURN);
   dump("browser");
   check(M2M_OSM_XY == 0 && M2M_OSM_DXDY == (45u << 8 | 33), "browser on the whole screen");
   check(find_row(" <..>") < 0 && find_row("<..>") == 1 && find_row("<Sub>") == 2 &&
         find_row("big.crt") == 3 && find_row("game.crt") == 4 && find_row(".rom") < 0,
         "browser: /c128, folders first, only .crt files");
   check(vram(M2M_DEV_VRAM_ATTR, 1, 1) == 0x8B && vram(M2M_DEV_VRAM_ATTR, 1, 2) == 0x0B,
         "browser cursor on the first line");
   press(M2M_KEY_DOWN);
   press(M2M_KEY_DOWN);
   press(M2M_KEY_DOWN);
   press(M2M_KEY_RETURN);
   dump("menu after the load");
   check(device_holds(CRT_DEV, 3, 70000), "game.crt copied into the device");
   check(crtrom_csr_get(CRT_DEV, CSR_FS_LO) == (70000 & 0xFFFF) &&
         crtrom_csr_get(CRT_DEV, CSR_FS_HI) == 1 && crtrom_csr_get(CRT_DEV, CSR_STATUS) == 3,
         "size handed to the parser, status OK");
   check(M2M_OSM_XY == (20u << 8) && find_row("=CRT:game.crt") > 0, "menu: CRT:game.crt selected");
   check(setting_get(item(" CRT file")) && !setting_get(item(" Hardware")) && setting_get(item(" CRT:%s")),
         "CRT file source and the CRT item set");

   // A build that shows the cartridge cache's waits: the Cartridge line has them, and
   // drawing it clears them
   crtrom_csr_set(CRT_DEV, CSR_STATS, 1);
   crtrom_csr_set(CRT_DEV, CSR_STALLS, 7);
   crtrom_csr_set(CRT_DEV, CSR_STALL_MAX, 128);
   press(M2M_KEY_RUNSTOP);
   dump("stall line");
   check(find_row(" Stalls 7 max 130us") > 0 && crtrom_csr_get(CRT_DEV, CSR_STALLS) == 0,
         "Cartridge line: stalls and the longest in us, then cleared");
   crtrom_csr_set(CRT_DEV, CSR_STATS, 0);
   press(M2M_KEY_RETURN);
   go_to("CRT:");

   // Again: where we left off; Run/Stop leaves it loaded
   press(M2M_KEY_RETURN);
   check(vram(M2M_DEV_VRAM_ATTR, 1, 4) == 0x8B, "browser: cursor where it was");
   press(M2M_KEY_RUNSTOP);
   check(find_row("=CRT:game.crt") > 0 && setting_get(item(" CRT:%s")), "cancelled: still loaded");

   // Too large for the device: its message, then the browser again
   press(M2M_KEY_RETURN);
   press(M2M_KEY_UP);
   press(M2M_KEY_RETURN);
   dump("too large");
   check(find_row("Error code: 0001") == 1 && find_row("This .crt file is too large") > 0, "too large");
   press(M2M_KEY_SPACE);
   check(find_row("game.crt") == 4, "browser again after the message");

   // By DMA, from a folder, with a parser error
   crtrom_csr_set(CRT_DEV, SDL_MAGIC, 0x5D4A);
   crtrom_csr_set(CRT_DEV, CSR_PARSEST, 3);
   crtrom_csr_set(CRT_DEV, CSR_PARSEE1, 0x42);
   const char *bad = "Bad chip packet";
   for (int i = 0; bad[i - (i > 0)]; i++)
      crtrom_csr_set(CRT_DEV, CSR_ERR_STR + i, bad[i]);
   press(M2M_KEY_UP);
   press(M2M_KEY_RETURN);
   check(find_row("<..>") == 1 && find_row("A cartridge with a name far too long for...") == 2 &&
         find_row("x.crt") == 3 && find_row("notes") < 0, "browser: Sub, a long name cut");
   press(M2M_KEY_DOWN);
   press(M2M_KEY_DOWN);
   press(M2M_KEY_RETURN);
   dump("parser error");
   check(find_row("Error code: 0042") == 1 && find_row("Bad chip packet") == 2, "parser error shown");
   uint32_t lba = crtrom_csr_get(CRT_DEV, SDL_BLK_LO) | (uint32_t)crtrom_csr_get(CRT_DEV, SDL_BLK_HI) << 16;
   uint8_t blk[512];
   FILE *img = fopen(image, "rb");
   int dma_ok = img && !fseek(img, (long)lba * 512, SEEK_SET) && fread(blk, 512, 1, img) == 1;
   for (int i = 0; dma_ok && i < 512; i++)
      dma_ok = blk[i] == pattern(5, i);
   if (img)
      fclose(img);
   check(dma_ok && crtrom_csr_get(CRT_DEV, SDL_COUNT) == 6 &&
         crtrom_csr_get(CRT_DEV, SDL_LEN_LO) == 3000 && crtrom_csr_get(CRT_DEV, SDL_LEN_HI) == 0,
         "DMA: x.crt's first block, 6 blocks, 3000 bytes");
   press(M2M_KEY_SPACE);
   check(vram(M2M_DEV_VRAM_ATTR, 1, 3) == 0x8B, "browser again, still on x.crt");
   crtrom_csr_set(CRT_DEV, CSR_PARSEST, 2);
   press(M2M_KEY_UP);
   press(M2M_KEY_RETURN);
   check(find_row("=CRT:A cartridge wit...") > 0, "menu: the long name cut with ...");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_UP);
   press(M2M_KEY_RETURN);                              // <..>: back to /c128, on <Sub>
   check(find_row("game.crt") == 4 && vram(M2M_DEV_VRAM_ATTR, 1, 2) == 0x8B, "up a folder, cursor on Sub");
   press(M2M_KEY_RUNSTOP);
   crtrom_csr_set(CRT_DEV, CSR_PARSEST, 2);
   crtrom_csr_set(CRT_DEV, SDL_MAGIC, 0);

   // The card changes while browsing: browsing starts over once it is mounted again
   press(M2M_KEY_RETURN);
   M2M_CSR &= ~M2M_CSR_SD_DET_EXT;
   poll(100);
   M2M_CSR |= M2M_CSR_SD_DET_EXT;
   poll(40000);
   check(find_row("<..>") == 1 && find_row("game.crt") == 4 && vram(M2M_DEV_VRAM_ATTR, 1, 1) == 0x8B,
         "new card: browsing from the start folder");
   press(M2M_KEY_RUNSTOP);

   // The device drops the file while the menu is open (a core reset)
   crtrom_csr_set(CRT_DEV, CSR_STATUS, 0);
   poll(2);
   check(find_row(" CRT:<Load>") > 0 && !setting_get(item(" CRT:%s")), "dropped file followed");
   go_to("Hardware");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_RUNSTOP);

   // A disk image for drive 8. Its drive reads it from the card: its table has the SD
   // block of each 4 KB piece, vdrives the size and type.
   go_to("8:");
   press(M2M_KEY_RETURN);
   dump("disk browser");
   check(find_row("<..>") == 1 && find_row("<Sub>") == 2 && find_row("bad.d64") == 3 &&
         find_row("frag.d71") == 4 && find_row("game.d64") == 5 && find_row("split.d64") == 6 &&
         find_row(".crt") < 0 && find_row(".bin") < 0, "disk browser: /c128, only disk images");
   for (int i = 0; i < 4; i++)
      press(M2M_KEY_DOWN);
   press(M2M_KEY_RETURN);
   dump("drive 8 mounted");
   check(find_row("=8:game.d64") > 0 && setting_get(item(" 8:%s")), "menu: 8:game.d64");
   check(vd_reg(VD_DRV_MOUNT) == 1 && vd_reg(VD_SIZE_L) == (174848 & 0xFFFF) && vd_reg(VD_SIZE_H) == 2 &&
         vd_reg(VD_TYPE) == 0, "vdrives: drive 8 mounted, D64 size and type");
   check(crtrom_csr_get(D8_DEV, MAP_CTRL) == 1 && crtrom_csr_get(D8_DEV, MAP_NBLK) == 342 &&
         table_right(D8_DEV, 8, 174848), "drive 8 on: 342 blocks, every one where the table says");

   // Drive 9: the same image is refused. frag.d71 lies in 4 KB runs.
   go_to("9:");
   press(M2M_KEY_RETURN);
   check(vram(M2M_DEV_VRAM_ATTR, 1, 1) == 0x8B, "drive 9: browsing from the top");
   for (int i = 0; i < 4; i++)
      press(M2M_KEY_DOWN);
   press(M2M_KEY_RETURN);
   dump("twice");
   check(find_row("Error code: EE42") == 1 && find_row("This disk image is already") == 2 &&
         find_row("Press Space to continue.") > 0, "the same image in drive 9 refused");
   press(M2M_KEY_SPACE);
   press(M2M_KEY_UP);
   press(M2M_KEY_RETURN);
   check(find_row("=9:frag.d71") > 0 && vd_reg(VD_DRV_MOUNT) == 3 && vd_reg(VD_SIZE_L) == 0x5600 &&
         vd_reg(VD_SIZE_H) == 5 && vd_reg(VD_TYPE) == 1, "drive 9: frag.d71, D71 size and type");
   check(crtrom_csr_get(D9_DEV, MAP_NBLK) == 683 && in_pieces(D9_DEV, 349696) &&
         table_right(D9_DEV, 10, 349696), "drive 9: an image in pieces, every block found");

   // Not a disk image, and (with clusters under 4 KB) one split on the card: refused, and
   // drive 9 keeps its image
   press(M2M_KEY_RETURN);
   press(M2M_KEY_UP);
   press(M2M_KEY_RETURN);
   check(find_row("Error code: 0001") == 1 && find_row("This is not a supported disk image") > 0,
         "wrong size refused");
   press(M2M_KEY_SPACE);
   if (cluster < 4096) {
      for (int i = 0; i < 3; i++)
         press(M2M_KEY_DOWN);
      press(M2M_KEY_RETURN);
      dump("split");
      check(find_row("Error code: EE44") == 1 && find_row("split into") == 2, "split image refused");
      press(M2M_KEY_SPACE);
   } else
      printf("(%d-byte clusters: no split image)\n", cluster);
   press(M2M_KEY_RUNSTOP);
   check(find_row("=9:frag.d71") > 0 && crtrom_csr_get(D9_DEV, MAP_CTRL) == 1 &&
         table_right(D9_DEV, 10, 349696), "drive 9 still has frag.d71");

   // Space unmounts; a D81 from a folder; the internal floppy's label
   go_to("8:");
   press(M2M_KEY_SPACE);
   check(find_row(" 8:<Mount Drive>") > 0 && !setting_get(item(" 8:%s")) && vd_reg(VD_DRV_MOUNT) == 2 &&
         crtrom_csr_get(D8_DEV, MAP_CTRL) == 0, "Space unmounts drive 8");
   press(M2M_KEY_RETURN);                              // drive 9 browsed last: from the top
   press(M2M_KEY_DOWN);
   press(M2M_KEY_RETURN);
   press(M2M_KEY_DOWN);
   press(M2M_KEY_RETURN);
   check(find_row("=8:disk.d81") > 0 && vd_reg(VD_TYPE) == 2 && crtrom_csr_get(D8_DEV, MAP_NBLK) == 1600 &&
         table_right(D8_DEV, 12, 819200), "drive 8: disk.d81 from Sub, a 1581");
   go_to("Internal 1581");
   press(M2M_KEY_RETURN);
   go_to("Drive 8");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_RUNSTOP);
   check(find_row("=8:Internal 1581") > 0, "drive 8 is the internal floppy");
   go_to("8:");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_RUNSTOP);
   check(find_row("=8:disk.d81") > 0 && find_row(" Internal 1581: Off") > 0,
         "choosing an image gives drive 8 back to images");

   // A drive unmounted behind the menu's back (a reset)
   M2M_RAMROM_DEV = VD_DEV;
   M2M_DEV16[VD_DRV_MOUNT] &= ~2;
   poll(2);
   check(find_row(" 9:<Mount Drive>") > 0 && !setting_get(item(" 9:%s")), "unmount by a reset followed");
   go_to("8:");
   press(M2M_KEY_SPACE);

   // A mount from a C128 program (the UCI's DOS target) while the menu is open: the
   // drive's line and setting follow at once
   const char *msg;
   check(shell_mount(1, "/c128/game.d64", &msg) == 0 && (vd_reg(VD_DRV_MOUNT) & 2) &&
         find_row("=9:game.d64") > 0 && setting_get(item(" 9:%s")), "shell_mount: drive 9, shown");
   shell_unmount(1);
   check(!(vd_reg(VD_DRV_MOUNT) & 2) && find_row(" 9:<Mount Drive>") > 0 && !setting_get(item(" 9:%s")),
         "shell_unmount: drive 9, shown");

   // R3-memory builds have no 2 MB and 16 MB REU
   go_to("REU");
   press(M2M_KEY_RETURN);
   int r3 = strstr(argv[1], "_r3") != NULL;
   check(find_row(" 512 KB") > 0 && (find_row(" 2 MB") > 0) != r3 && (find_row(" 16 MB") > 0) != r3,
         r3 ? "REU: Off and 512 KB" : "REU: Off, 512 KB, 2 MB, 16 MB");

   // The REU's file: only .reu files; with the REU off, a message
   M2M_RAMROM_DEV = UCI_DEV;
   M2M_DEV16[UCI_REU_MEM] = r3 ? 0x0080 : 0x8800;      // HyperRAM at 1 MB, or SDRAM at 16 MB
   uint8_t *reu = r3 ? (uint8_t *)host_hr + 0x0080 * 8192 : (uint8_t *)host_sr + 0x0800 * 8192;
   go_to("File:");
   check(find_row(" File:<Load>") > 0, "REU file: none yet");
   press(M2M_KEY_RETURN);
   check(find_row("ram.reu") > 0 && find_row("game.crt") < 0, "browser: only .reu files");
   go_to("ram.reu");
   press(M2M_KEY_RETURN);
   dump("REU off");
   check(find_row("The REU is off") > 0, "REU file with the REU off: its message");
   press(M2M_KEY_SPACE);
   press(M2M_KEY_RUNSTOP);
   check(find_row(" File:<Load>") > 0 && !reu_file_picked(), "nothing picked");

   // 512 KB: the file goes into the REU's memory at once, and LOAD_REU/SAVE_REU use it
   go_to("512 KB");
   press(M2M_KEY_RETURN);
   memset(reu, 0xEE, 80000);
   go_to("File:");
   press(M2M_KEY_RETURN);
   go_to("ram.reu");
   press(M2M_KEY_RETURN);
   dump("REU file loaded");
   int same = 1;
   for (int i = 0; i < 70000; i++)
      same &= reu[i] == pattern(13, i);
   check(same && reu[70000] == 0xEE, "ram.reu in the REU's memory, nothing past it");
   check(find_row("=File:ram.reu") > 0 && !strcmp(reu_file(), "/c128/ram.reu"), "menu: File:ram.reu");
   // LOAD_CONFIG's REU Preload Image names one without the browser (uci_ctrl.c), or none
   reu_file_set("/c128/other.reu");
   shell_crtrom_named(OPTM_G_REU_FILE, "/c128/other.reu");
   check(find_row("=File:other.reu") > 0, "a file named without the browser: on its line at once");
   reu_file_set(NULL);
   shell_crtrom_named(OPTM_G_REU_FILE, NULL);
   check(find_row(" File:<Load>") > 0, "and none");
   go_to("Off");
   press(M2M_KEY_RETURN);
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
