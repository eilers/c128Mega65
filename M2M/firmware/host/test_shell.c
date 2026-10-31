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
#include "heap.h"
#include "keys.h"
#include "m2m_rv.h"
#include "menu.h"
#include "reu.h"
#include "rompick.h"
#include "roms.h"
#include "sdcard.h"
#include "shell.h"
#include "stores.h"

void host_init(void);
int  disk_file_open(const char *path);
extern uint32_t host_regs[];

static int fails, runs, verbose;
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

// The one Model line marked in a drive's submenu
static int model_marked(const char *m)
{
   static const char *const models[] = { "1541", "1571", "1581", "Internal 1581" };
   char r[48], line[32];
   int marked = 0, want = 0;
   for (int y = 0; y < 33; y++) {
      row(y, 0, 45, r);
      for (int i = 0; i < 4; i++) {
         snprintf(line, sizeof line, "*%s ", models[i]);
         if (strstr(r, line) && !strstr(r, "ROM")) {
            marked++;
            want |= !strcmp(models[i], m);
         }
      }
   }
   return marked == 1 && want;
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

// A key of the whole keyboard (MEGA65 key number), with Shift or not
static void type_key(int k, int shift)
{
   uint32_t m[3] = { ~0u, ~0u, ~0u };
   m[k / 32] &= ~(1u << (k % 32));
   if (shift)
      m[0] &= ~(1u << 15);
   for (int i = 0; i < 3; i++)
      M2M_MATRIX(i) = m[i];
   poll(3);
   for (int i = 0; i < 3; i++)
      M2M_MATRIX(i) = ~0u;
   poll(3);
}

#define K_DEL    0
#define K_RETURN 1

static void type(const char *s)
{
   static const struct { char c; int k; } keys[] = {
      { '1', 56 }, { '2', 59 }, { '3', 8 }, { '4', 11 }, { '9', 32 }, { '.', 44 }, { 'y', 25 }, { 'x', 23 },
   };
   for (; *s; s++)
      for (unsigned i = 0; i < sizeof keys / sizeof keys[0]; i++)
         if (keys[i].c == *s)
            type_key(keys[i].k, 0);
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

// The browser's cursor onto a file, down and then up from where it is
static int browse_to(const char *name)
{
   for (int i = 0; i < 60; i++) {
      int y = find_row(name);
      if (y > 0 && vram(M2M_DEV_VRAM_ATTR, 1, y) == 0x8B)
         return 1;
      press(i < 30 ? M2M_KEY_DOWN : M2M_KEY_UP);
   }
   printf("no file \"%s\"\n", name);
   return 0;
}

// The cursor onto the line whose text (after its mark) is s, exactly
static int go_to_line(const char *s)
{
   char r[48];
   for (int i = 0; i < 60; i++) {
      int y = cursor_row();
      if (y >= 0) {
         row(y, 22, 45, r);
         for (int n = strlen(r); n > 0 && (r[n - 1] == ' ' || r[n - 1] == '|'); )
            r[--n] = 0;
         if (!strcmp(r, s))
            return 1;
      }
      press(M2M_KEY_DOWN);
   }
   printf("no line \"%s\"\n", s);
   return 0;
}

static int mount_item(int d)   // drive d's line, by its group
{
   for (int i = 0; i < menu.n; i++)
      if ((menu.group[i] & MENU_ID) == (d ? OPTM_G_MOUNT_D9 : OPTM_G_MOUNT_D8))
         return i;
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

static void put_text(const char *name, const char *text)
{
   FIL f;
   UINT put;
   check(f_open(&f, name, FA_WRITE | FA_CREATE_ALWAYS) == FR_OK, name);
   f_write(&f, text, strlen(text), &put);
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

// The ROMs; and the settings the first run saved, as it recorded them in /bits.bin: 1 if
// there are some
static int make_card(uint8_t *bits)
{
   static FATFS fs;
   FIL f;
   UINT got = 0;
   f_mount(&fs, "", 1);
   f_mkdir("/c128");
   put_file("/c128/boot0.rom", 0, 73728);
   put_file("/c128/boot1.rom", 1, 600);
   put_file("/c128/game.crt", 3, 70000);
   put_text("/c128/game.cfg", "[MEGA65 Settings]\nVGA Display Mode=15 kHz with CSYNC\n");   // game.crt's own
   put_text("/c128/mine.cfg", "[MEGA65 Settings]\nVIC-II Jailbars=High\nHDMI Display Mode=640x480 60 Hz\n");
   put_text("/c128/bad.cfg", "[MEGA65 Settings]\nVIC-II Jailbars=Huge\nCaps Lock Key=ASCII/DIN\n"
            "[A_store_name_far_too_long_for_one_line_of_the_message]\nx=1\n");
   put_text("/c128/back.cfg", "[MEGA65 Settings]\nVIC-II Jailbars=Off\nHDMI Display Mode=720p 50 Hz 16:9\n"
            "VGA Display Mode=Standard\nCaps Lock Key=40/80\n");
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
   if (f_open(&f, "/bits.bin", FA_READ) == FR_OK) {
      f_read(&f, bits, MENU_MAX, &got);
      f_close(&f);
   }
   f_unmount("");
   return got != 0;
}

static int device_holds(int dev, int file, int size)
{
   M2M_RAMROM_DEV = dev;
   for (int i = 0; i < size; i++)
      if (M2M_DEV16[i] != pattern(file, i))
         return 0;
   return 1;
}

// ROMs of 16, 8 and 32 KB, and none, and a boot cartridge. Away from /c128 while other
// browsers are tested there.
static void make_roms(void)
{
   f_rename("/roms-away", "/c128/roms");
   f_rename("/carts-away", "/c128/carts");
   f_mkdir("/c128/roms");
   put_file("/c128/roms/k16.bin", 20, 16384);
   put_file("/c128/roms/k8.bin", 21, 8192);
   put_file("/c128/roms/f32.bin", 22, 32768);
   put_file("/c128/roms/odd.bin", 23, 1000);
   f_mkdir("/c128/carts");
   put_file("/c128/carts/boot.crt", 24, 5000);
}

// A device from at holds size bytes of a file, from its offset off
static int dev_at(int dev, int at, int file, int off, int size)
{
   M2M_RAMROM_DEV = dev;
   for (int i = 0; i < size; i++)
      if (M2M_DEV16[at + i] != pattern(file, off + i))
         return 0;
   return 1;
}

// The settings file holds a line
static int setting_line(const char *line)
{
   static char text[4096];
   FIL f;
   UINT got = 0;
   if (f_open(&f, SETTINGS_FILE, FA_READ) == FR_OK) {
      f_read(&f, text, sizeof text - 1, &got);
      f_close(&f);
   }
   text[got] = 0;
   return strstr(text, line) != NULL;
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

// A menu item whose setting the settings file keeps: an item of a store is its group
static int kept(int idx)
{
   static const int own[] = { OPTM_G_REU, OPTM_G_UCI, OPTM_G_DRVA_MODEL, OPTM_G_DRVB_MODEL,
                              OPTM_G_DRVA_ID, OPTM_G_DRVB_ID };   // stores.c's own items
   int g = shell_group(idx);
   for (int k = 0; k < stores_count; k++)
      if (g > 0 && stores_items[k].group == g)
         return 1;
   for (int k = 0; k < (int)(sizeof own / sizeof own[0]); k++)
      if (own[k] == g)
         return 1;
   return 0;
}

// The stores' menu groups: a name per line in its order (the menu's words in MEGA65
// Settings), or Disabled/Enabled for a single-select line; config.vhd's default. And every
// setting of the menu kept: all but drives, files, help and text lines.
static void check_stores(void)
{
   for (int k = 0; k < stores_count; k++) {
      const item_t *it = &stores_items[k];
      if (!it->group)
         continue;
      int n = 0, def = -1, words = 1, toggle = !strcmp(it->choices[0], "Disabled") && !it->choices[2];
      for (int i = 0; i < menu.n; i++) {
         if (shell_group(i) != it->group)
            continue;
         const char *l = menu.line[i];
         while (*l == ' ')
            l++;
         if (!toggle && it->store == ST_MEGA65 && (n >= item_choices(it) || strcmp(l, it->choices[n])))
            words = 0;
         if (cfg_get(CFG_OPTM_STDSEL, i))
            def = n;
         n++;
      }
      check(toggle ? n == 1 && it->def == (def == 0) : n == item_choices(it) && it->def == def && words, it->name);
   }
   int all = 1;
   for (int i = 0; i < menu.n; i++) {
      int g = shell_group(i), max = 0;
      char text[64];
      if (g > 0 && g < 255 && !cfg_get(CFG_OPTM_MOUNT, i) && !cfg_get(CFG_OPTM_CRTROM, i) &&
          !cfg_get(CFG_OPTM_HELP, i) && !core_text_item(g, text, &max) && !kept(i)) {
         printf("not in the settings file: \"%s\"\n", menu.line[i]);
         all = 0;
      }
   }
   check(all, "every setting of the menu in the settings file");
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
   uint8_t bits[MENU_MAX];
   int second = make_card(bits);                      // the first run left its settings
   M2M_CSR |= M2M_CSR_SD_DET_EXT | M2M_CSR_SD_ACTIVE;
   crtrom_csr_set(CRT_DEV, CSR_MAX_HI, 2);            // the cartridge takes up to 128 KB, parses OK
   crtrom_csr_set(CRT_DEV, CSR_PARSEST, 2);

   // Start: ROMs loaded, defaults set, the core out of reset with keyboard and joysticks
   shell_start();
   uint8_t *fresh = heap_alloc(100);
   int zero = 1;
   for (int i = 0; i < 100; i++)
      zero &= fresh[i] == 0;
   check(zero, "heap_alloc: zeroed, on HyperRAM that held something");
   check(!(M2M_CSR & M2M_CSR_RESET), "core left reset");
   check((M2M_CSR & (M2M_CSR_KBD | M2M_CSR_JOY1 | M2M_CSR_JOY2)) == 0x38, "keyboard and joysticks on");
   check(device_holds(0x101, 0, 4100), "boot0.rom in its device");
   check(second ? dev_at(0x102, 0, 22, 0, 32768) : device_holds(0x102, 1, 600),
         second ? "drive 8's 1541 ROM picked over boot1.rom" : "boot1.rom in its device");
   M2M_RAMROM_DEV = M2M_DEV_ASCAL_PPHASE;
   check(M2M_DEV16[0] == 0x03E8 && M2M_DEV16[0x100] != 0, "ascal filters loaded");
   check(M2M_CSR & M2M_CSR_ASCAL_AUTO, "ascal follows the core");
   // The first run left ROMs picked and a boot cartridge in the settings file; the cartridge
   // loads from the main loop, once the C128 runs
   if (second) {
      check(crtrom_csr_get(CRT_DEV, CSR_STATUS) != 3, "start: the boot cartridge not loaded while the core is in reset");
      core_poll();
      check(dev_at(0x101, 0x4000, 20, 0, 16384) && dev_at(0x101, 0x18000, 22, 0, 32768) &&
            crtrom_csr_get(0x101, 0) == 2 && dev_at(0x101, 0x8000, 0, 0x8000, 0x8000),
            "start: the C128 KERNAL and the function ROM picked, the rest stock");
      check(dev_at(0x102, 2 * 32768, 20, 0, 16384) && dev_at(0x102, 2 * 32768 + 16384, 20, 0, 16384),
            "start: drive 8's 1571 ROM picked, twice");
      check(dev_at(0x102, 32768, 1, 32768, 0) && dev_at(0x101, 0x10000, 0, 0x10000, 8192),
            "start: drive 9's bank and the characters stock");
      check(device_holds(CRT_DEV, 24, 5000) && setting_get(item(" CRT file")) && setting_get(item(" CRT:%s")),
            "start: the boot cartridge loaded, the virtual cartridge the source");
      check(!setting_get(item(" Hardware")), "start: Hardware not still set beside CRT file");
      f_rename("/c128/roms", "/roms-away");
      f_rename("/c128/carts", "/carts-away");
   }
   int want = 1;
   for (int i = 0; i < menu.n; i++)
      want &= cfg_get(CFG_OPTM_CRTROM, i) || (second && (i == item(" Hardware") || i == item(" CRT file"))) ||
              setting_get(i) == (second && kept(i) ? bits[i] : cfg_get(CFG_OPTM_STDSEL, i));
   check(want, second ? "settings from the settings file" : "settings are config.vhd's defaults (no settings file)");
   if (second)
      check(!setting_get(item(" Use IEC port")), "IEC port off, as the first run left it");
   check_stores();
   // Back to the defaults for the checks below
   M2M_CONTROL_M(0) = M2M_CONTROL_M(1) = M2M_CONTROL_M(2) = M2M_CONTROL_M(3) = 0;
   for (int i = 0; i < menu.n; i++)
      setting_set(i, cfg_get(CFG_OPTM_STDSEL, i));

   // The menu, top right
   press(M2M_KEY_HELP);
   dump("menu");
   check(M2M_CSR & M2M_CSR_OSM, "Help opens the menu");
   check(M2M_OSM_XY == (20u << 8) && M2M_OSM_DXDY == (25u << 8 | 33), "menu window 25x33 at 20,0");
   check(!(M2M_CSR & M2M_CSR_KBD), "keyboard off while the menu is open");
   check(find_row(" Drives") == 1 && vram(M2M_DEV_VRAM_ATTR, 22, 1) == 0x0E, "headline Drives");
   check(find_row(" 8:1571 <Mount Drive>") == 3 && cursor_row() == 3, "cursor on drive A's line (the start item), its ID and model");
   check(find_row(" 9:1571 <Mount Drive>") == 4 && find_row("Drive A") < 0, "drive B's line; no lines of their own for the submenus");
   check(find_row(" Video: Follow 40/80") > 0, "video summary");
   check(find_row(" Cartridge: Hardware") > 0, "cartridge summary");
   check(find_row(" REU: Off") > 0, "REU summary");

   // A drive's line is its submenu: its disk, ID, model and ROMs; pick, and back
   press(M2M_KEY_RETURN);
   dump("drive A submenu");
   check(find_row(" Drive A") == 1 && find_row(" Bus ID") > 0 && find_row(" Model") > 0 &&
         find_row(" 1571 ROM:") > 0, "submenu: its headline, disk, ID, model, ROMs");
   check(cursor_on(" Disk:<Mount Drive>"), "cursor on its Disk line");
   check(find_row("*8") > 0 && find_row("*1571") > 0 && find_row("*1541") < 0, "8 and 1571 marked");
   go_to_line("1541");
   press(M2M_KEY_RETURN);
   check(find_row("*1541") > 0 && find_row("*1571") < 0, "1541 marked, 1571 not");
   check(setting_get(item(" 1541")) && !setting_get(item(" 1571")), "1541 set at once");
   press(M2M_KEY_RUNSTOP);
   check(find_row(" 8:1541 <Mount Drive>") > 0 && cursor_on("8:1541"), "back in the main menu: drive A a 1541");
   // Drive B at 10, then at drive A's 8: they swap, never on one ID
   go_to("9:");
   press(M2M_KEY_RETURN);
   go_to_line("10");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_RUNSTOP);
   check(find_row(" 10:1571 <Mount Drive>") == 4, "drive B at 10");
   press(M2M_KEY_RETURN);
   go_to_line("8");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_RUNSTOP);
   check(find_row(" 10:1541 <Mount Drive>") == 3 && find_row(" 8:1571 <Mount Drive>") == 4,
         "drive B at 8: drive A takes its 10");
   press(M2M_KEY_RETURN);
   go_to_line("9");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_RUNSTOP);
   go_to("10:");
   press(M2M_KEY_RETURN);
   go_to_line("8");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_RUNSTOP);
   check(find_row(" 8:1541 <Mount Drive>") == 3 && find_row(" 9:1571 <Mount Drive>") == 4 && cursor_on("8:1541"),
         "drives at 8 and 9 again");

   // Close: saved, keyboard back after a moment
   press(M2M_KEY_HELP);
   check(!(M2M_CSR & M2M_CSR_OSM), "Help closes the menu");
   idle();
   check(M2M_CSR & M2M_CSR_KBD, "keyboard back after closing");
   check(setting_line("[Drive A Settings]\nDrive Type=1541\nDrive Bus ID=8\n") &&
         setting_line("[MEGA65 Settings]\nVideo Output=Follow 40/80\nVIC-II Jailbars=Off\n"
                      "HDMI Display Mode=720p 50 Hz 16:9\nHDMI Flicker-free=Disabled\nHDMI Audio=Enabled\n") &&
         setting_line("Use IEC Port=Enabled\n"), "settings file saved");
   press(M2M_KEY_HELP);
   check(cursor_on("8:1541"), "cursor remembered");

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
   check(setting_get(item(" 15 kHz with CSYNC")) && !setting_get(item(" Standard")), "game.crt's game.cfg in effect");

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

   // A disk image for drive A: its line, then Disk. Its drive reads it from the card: its
   // table has the SD block of each 4 KB piece, vdrives the size and type.
   go_to("8:");
   press(M2M_KEY_RETURN);
   check(cursor_on(" Disk:<Mount Drive>"), "drive A's submenu, on Disk");
   press(M2M_KEY_RETURN);
   dump("disk browser");
   check(find_row("<..>") == 1 && find_row("<Sub>") == 2 && find_row("bad.d64") == 3 &&
         find_row("frag.d71") == 4 && find_row("game.d64") == 5 && find_row("split.d64") == 6 &&
         find_row(".crt") < 0 && find_row(".bin") < 0, "disk browser: /c128, only disk images");
   for (int i = 0; i < 4; i++)
      press(M2M_KEY_DOWN);
   press(M2M_KEY_RETURN);
   dump("drive 8 mounted");
   check(find_row("=Disk:game.d64") > 0 && setting_get(mount_item(0)), "drive A's Disk: game.d64");
   check(vd_reg(VD_DRV_MOUNT) == 1 && vd_reg(VD_SIZE_L) == (174848 & 0xFFFF) && vd_reg(VD_SIZE_H) == 2 &&
         vd_reg(VD_TYPE) == 0, "vdrives: drive 8 mounted, D64 size and type");
   check(crtrom_csr_get(D8_DEV, MAP_CTRL) == 1 && crtrom_csr_get(D8_DEV, MAP_NBLK) == 342 &&
         table_right(D8_DEV, 8, 174848), "drive 8 on: 342 blocks, every one where the table says");
   press(M2M_KEY_RUNSTOP);
   check(find_row(" 8:1541 game.d64") == 3, "drive A's line: 8:1541 game.d64");

   // Drive B: the same image is refused. frag.d71 lies in 4 KB runs.
   go_to("9:");
   press(M2M_KEY_RETURN);
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
   check(find_row("=Disk:frag.d71") > 0 && vd_reg(VD_DRV_MOUNT) == 3 && vd_reg(VD_SIZE_L) == 0x5600 &&
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
   check(find_row("=Disk:frag.d71") > 0 && crtrom_csr_get(D9_DEV, MAP_CTRL) == 1 &&
         table_right(D9_DEV, 10, 349696), "drive 9 still has frag.d71");
   press(M2M_KEY_RUNSTOP);
   check(find_row(" 9:1571 frag.d71") == 4, "drive B's line: 9:1571 frag.d71");

   // Space on Disk unmounts; a D81 from a folder; the internal floppy
   go_to("8:");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_SPACE);
   check(find_row(" Disk:<Mount Drive>") > 0 && !setting_get(mount_item(0)) && vd_reg(VD_DRV_MOUNT) == 2 &&
         crtrom_csr_get(D8_DEV, MAP_CTRL) == 0, "Space unmounts drive 8");
   press(M2M_KEY_RETURN);                              // drive 9 browsed last: from the top
   press(M2M_KEY_DOWN);
   press(M2M_KEY_RETURN);
   press(M2M_KEY_DOWN);
   press(M2M_KEY_RETURN);
   check(find_row("=Disk:disk.d81") > 0 && vd_reg(VD_TYPE) == 2 && crtrom_csr_get(D8_DEV, MAP_NBLK) == 1600 &&
         table_right(D8_DEV, 12, 819200), "drive 8: disk.d81 from Sub, a 1581");
   go_to_line("Internal 1581");
   press(M2M_KEY_RETURN);
   check(find_row("=Disk:Internal 1581") > 0, "drive A is the internal floppy");
   press(M2M_KEY_RUNSTOP);
   check(find_row(" 8:1581 Internal") == 3, "its line");
   go_to("9:");
   press(M2M_KEY_RETURN);
   go_to_line("Internal 1581");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_RUNSTOP);
   check(find_row(" 9:1581 Internal") == 4 && find_row(" 8:1581 disk.d81") == 3 && stores_drive_model(0) == DM_1581,
         "then drive B: drive A a 1581 of images again");
   press(M2M_KEY_RETURN);
   go_to("Disk:");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_RUNSTOP);
   dump("drive B, frag.d71 again");
   check(find_row("=Disk:frag.d71") > 0 && stores_drive_model(1) == DM_1571 && model_marked("1571"),
         "choosing an image gives drive B back to images: a 1571 for its D71");
   press(M2M_KEY_RUNSTOP);
   check(find_row(" 8:1581 disk.d81") == 3 && find_row(" 9:1571 frag.d71") == 4, "drive A a 1581, drive B a 1571");

   // A D64 into a 1581: the drive becomes a 1571, its line and its submenu
   go_to("8:");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_RETURN);
   for (int i = 0; i < 4; i++)
      press(M2M_KEY_DOWN);
   press(M2M_KEY_RETURN);
   dump("a D64 into the 1581");
   check(find_row("=Disk:game.d64") > 0 && stores_drive_model(0) == DM_1571 && model_marked("1571"),
         "a D64 into drive A's 1581: a 1571 in its submenu");
   press(M2M_KEY_RUNSTOP);
   check(find_row(" 8:1571 game.d64") == 3, "and on its line");
   go_to("8:");
   press(M2M_KEY_RETURN);
   go_to_line("1541");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_RUNSTOP);
   check(find_row(" 8:1541 game.d64") == 3 && stores_drive_model(0) == DM_1541, "a 1541 for its D64: yes");

   // A model its image can't run ejects the image
   go_to("8:");
   press(M2M_KEY_RETURN);
   go_to_line("1581");
   press(M2M_KEY_RETURN);
   dump("a 1581 for the D64");
   check(stores_drive_model(0) == DM_1581 && model_marked("1581") && find_row(" Disk:<Mount Drive>") > 0 &&
         !(vd_reg(VD_DRV_MOUNT) & 1) && crtrom_csr_get(D8_DEV, MAP_CTRL) == 0, "a 1581 for its D64: the D64 ejected");
   go_to_line("1541");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_RUNSTOP);
   check(find_row(" 8:1541 <Mount Drive>") == 3 && stores_drive_model(0) == DM_1541, "drive A a 1541, no disk");

   // A drive unmounted behind the menu's back (a reset)
   M2M_RAMROM_DEV = VD_DEV;
   M2M_DEV16[VD_DRV_MOUNT] &= ~2;
   poll(2);
   check(find_row(" 9:1571 <Mount Drive>") == 4 && !setting_get(mount_item(1)), "unmount by a reset followed");

   // A mount from a C128 program (the UCI's DOS target) while the menu is open: the
   // drive's line and setting follow at once
   const char *msg;
   check(shell_mount(1, "/c128/game.d64", &msg) == 0 && (vd_reg(VD_DRV_MOUNT) & 2) &&
         find_row(" 9:1571 game.d64") == 4 && setting_get(mount_item(1)), "shell_mount: drive 9, shown");
   shell_unmount(1);
   check(!(vd_reg(VD_DRV_MOUNT) & 2) && find_row(" 9:1571 <Mount Drive>") == 4 && !setting_get(mount_item(1)),
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

   // The Network submenu: no address yet (no network turn here); a host name typed in its
   // line, an address refused, then one that turns DHCP off; Run/Stop keeps the old text.
   // Typed settings go into the settings file at once.
   for (int i = 0; i < 3; i++)
      M2M_MATRIX(i) = ~0u;
   check(find_row(" Net: no address") > 0, "network summary");
   go_to("Net:");
   press(M2M_KEY_RETURN);
   dump("network");
   check(find_row("=REST API") > 0 && find_row("=DHCP") > 0 && find_row(" Host:c128-mega65") > 0 &&
         find_row(" Password:<none>") > 0 && find_row(" IP:<none>") > 0, "network submenu");
   go_to("Host:");
   press(M2M_KEY_RETURN);
   int y = find_row(" Host:c128-mega65_");
   check(y > 0 && vram(M2M_DEV_VRAM_ATTR, 21, y) == 0x8B, "host name: its field, inverted");
   for (int i = 0; i < 5; i++)
      type_key(K_DEL, 0);
   type("y");
   type_key(23, 1);                                    // Shift X
   check(find_row(" Host:c128-myX_") > 0, "host name: typed");
   type_key(K_RETURN, 0);
   check(find_row(" Host:c128-myX_") > 0, "host name: Return on the matrix alone does nothing");
   press(M2M_KEY_RETURN);
   check(find_row(" Host:c128-myX") > 0 && find_row("_") < 0 && cursor_on("Host:"), "host name: kept");
   go_to("IP:");
   press(M2M_KEY_RETURN);
   check(find_row(" IP:_") > 0, "address: empty while DHCP has none");
   type("1.2");
   press(M2M_KEY_RETURN);
   check(find_row(" IP:not an address") > 0, "address: 1.2 refused");
   type(".3.4");
   check(find_row(" IP:1.2.3.4_") > 0, "address: typing goes on");
   // One Return as the hardware gave it: the whole keyboard's copy a round before the menu
   // keys'. It ends the field once; the menu doesn't open it again.
   M2M_MATRIX(0) = ~(1u << K_RETURN);
   poll(1);
   M2M_KEYS = ~M2M_KEY_RETURN & 0xFFFF;
   poll(3);
   M2M_MATRIX(0) = ~0u;
   M2M_KEYS = 0xFFFF;
   poll(3);
   check(find_row(" IP:1.2.3.4") > 0 && find_row("_") < 0 && find_row("=DHCP") < 0 &&
         !shell_setting(OPTM_G_NET_DHCP, 0), "address: kept once, DHCP off");
   check(find_row(" Mask:255.255.255.0") > 0, "mask: the static one now");
   go_to("Mask:");
   press(M2M_KEY_RETURN);
   type("9");
   press(M2M_KEY_RUNSTOP);
   check(find_row(" Mask:255.255.255.0") > 0 && cursor_on("Mask:"), "Run/Stop: the old text");
   check(setting_line("[Ethernet Settings]\nUse DHCP=Disabled\nStatic IP=1.2.3.4\nStatic Netmask=255.255.255.0\n"
                      "Static Gateway=192.168.2.1\nStatic DNS=8.8.8.8\n[Network Settings]\n"
                      "Host Name=c128-myX\nNetwork Password=\n"), "settings file: the network");
   go_to("DHCP");
   press(M2M_KEY_RETURN);
   check(find_row("=DHCP") > 0 && find_row(" IP:<none>") > 0, "DHCP on again: its address");
   press(M2M_KEY_RUNSTOP);
   f_unlink(SETTINGS_FILE);
   stores_net_defaults();

   // The System ROMs submenu: all stock, whatever the start picked
   make_roms();
   for (int p = 0; p < RP_COUNT; p++)
      rompick_set(p, rompick_default(p));
   rompick_apply((1u << RP_COUNT) - 1);
   check(find_row(" System ROMs: Stock") > 0, "System ROMs summary: stock");
   go_to("System ROMs:");
   press(M2M_KEY_RETURN);
   dump("System ROMs");
   check(find_row(" C128 KERNAL:Stock") > 0 && find_row(" Function ROM:None") > 0 && find_row("1571 ROM:") < 0 &&
         find_row(" Space: back to stock") > 0, "System ROMs submenu: no drive ROMs");
   // A line browses /c128/roms, never above it, and shows only files of its size
   go_to("C128 KERNAL:");
   press(M2M_KEY_RETURN);
   dump("C128 KERNAL browser");
   check(find_row("<..>") < 0 && find_row("k16.bin") == 1 && find_row("k8.bin") < 0 && find_row("f32.bin") < 0 &&
         find_row("odd.bin") < 0, "C128 KERNAL browser: /c128/roms, 16 KB files only");
   M2M_RAMROM_DEV = 0x010C;                           // the bus DMA's hold, the UCI's reboot
   M2M_DEV16[0] = 0;
   M2M_RAMROM_DEV = 0x0109;
   M2M_DEV16[0x010] = 0;
   press(M2M_KEY_RETURN);
   dump("C128 KERNAL picked");
   M2M_RAMROM_DEV = 0x010C;
   int held = M2M_DEV16[0] == 1;
   M2M_RAMROM_DEV = 0x0109;
   check(held && M2M_DEV16[0x010] == 2, "C128 KERNAL picked: the CPU held by DMA, then the reset button");
   check(find_row("=C128 KERNAL:k16.bin") > 0 && dev_at(0x101, 0x4000, 20, 0, 16384) &&
         dev_at(0x101, 0, 0, 0, 0x4000) && !(M2M_CSR & M2M_CSR_RESET), "C128 KERNAL picked: in ROM4, the C128 started again");
   check(setting_line("[C128 ROM Settings]\nC128 Kernal ROM=k16.bin\n") &&
         setting_line("Host Name=c128-mega65\n"), "C128 KERNAL picked: kept at once");
   press(M2M_KEY_SPACE);
   check(find_row(" C128 KERNAL:Stock") > 0 && dev_at(0x101, 0x4000, 0, 0x4000, 16384) &&
         setting_line("C128 Kernal ROM=\n"), "Space: stock again, kept");
   // The function ROM: 16 or 32 KB, its size to the hardware
   go_to("Function ROM:");
   press(M2M_KEY_RETURN);
   check(find_row("f32.bin") == 1 && find_row("k16.bin") == 2 && find_row("k8.bin") < 0, "function ROM browser: 16 and 32 KB");
   press(M2M_KEY_RETURN);
   check(find_row("=Function ROM:f32.bin") > 0 && dev_at(0x101, 0x18000, 22, 0, 32768) && crtrom_csr_get(0x101, 0) == 2,
         "function ROM: at 0x18000, 32 KB");
   // Drive A's 1571 ROM, in its submenu: its bank twice; no reload, the drive runs a 1541 now
   press(M2M_KEY_RUNSTOP);
   go_to("8:");
   press(M2M_KEY_RETURN);
   crtrom_csr_set(0x102, 0, 0);
   go_to("1571 ROM:");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_DOWN);
   press(M2M_KEY_RETURN);
   dump("drive ROM picked");
   check(find_row("=1571 ROM:k16.bin") > 0 && dev_at(0x102, 2 * 32768, 20, 0, 16384) &&
         dev_at(0x102, 2 * 32768 + 16384, 20, 0, 16384) && crtrom_csr_get(0x102, 0) == 0,
         "drive A's 1571 ROM: its bank, twice, no reload");
   // Its 1541 ROM, which it runs: a reload
   press(M2M_KEY_UP);
   press(M2M_KEY_RETURN);
   press(M2M_KEY_RETURN);
   check(find_row("=1541 ROM:f32.bin") > 0 && dev_at(0x102, 0, 22, 0, 32768) && crtrom_csr_get(0x102, 0) == 1,
         "drive A's 1541 ROM: its bank, reloaded");
   press(M2M_KEY_RUNSTOP);
   go_to("System ROMs:");
   press(M2M_KEY_RETURN);
   // No /c128/roms: a message, then the menu
   f_rename("/c128/roms", "/c128/roms-gone");
   go_to("C128 BASIC:");
   press(M2M_KEY_RETURN);
   dump("no ROMs folder");
   check(find_row("No ROM files of 32 KB in /c128/roms.") > 0, "no ROMs folder: its message");
   press(M2M_KEY_SPACE);
   check(find_row(" C128 BASIC:Stock") > 0 && cursor_on("C128 BASIC:"), "no ROMs folder: back in the menu");
   f_rename("/c128/roms-gone", "/c128/roms");
   press(M2M_KEY_RUNSTOP);
   check(find_row(" System ROMs: Custom") > 0, "System ROMs summary: custom");
   // The boot cartridge: a .crt from /c128/carts, kept; loaded at the next start
   go_to("Cartridge");
   press(M2M_KEY_RETURN);
   go_to("Boot:");
   check(find_row(" Boot:<None>") > 0, "boot cartridge: none");
   press(M2M_KEY_RETURN);
   check(find_row("<..>") < 0 && find_row("boot.crt") == 1, "boot cartridge browser: /c128/carts");
   press(M2M_KEY_RETURN);
   check(find_row("=Boot:boot.crt") > 0 && setting_line("Cartridge=boot.crt\n"), "boot cartridge: picked, kept");
   check(find_row(" Space clears Boot") == find_row("=Boot:boot.crt") + 1, "boot cartridge: the Space hint below it");
   press(M2M_KEY_SPACE);
   check(find_row(" Boot:<None>") > 0 && setting_line("[C64 and Cartridge Settings]\nCartridge=\n"),
         "boot cartridge: Space clears it, kept");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_RETURN);
   check(find_row("=Boot:boot.crt") > 0, "boot cartridge: picked again");
   press(M2M_KEY_RUNSTOP);
   // Picked again for the next run, which checks them at its start
   go_to("System ROMs:");
   press(M2M_KEY_RETURN);
   go_to("C128 KERNAL:");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_RETURN);
   check(setting_line("C128 Kernal ROM=k16.bin\n") && setting_line("Internal Function ROM=f32.bin\n") &&
         setting_line("Drive Bus ID=8\nROM for 1541 mode=f32.bin\nROM for 1571 mode=k16.bin\n"),
         "the settings file for the next run");
   press(M2M_KEY_RUNSTOP);

   // Load settings: .cfg files; what is right in one takes effect and is kept at once. The
   // VGA mode game.crt's .cfg set lasts until the next start: the file keeps Standard.
   check(find_row("=Load settings") < 0 && find_row(" Load settings") == 29, "Load settings: the main menu's last line but two");
   go_to("Load settings");
   press(M2M_KEY_RETURN);
   dump("Load settings browser");
   check(find_row("back.cfg") > 0 && find_row("mine.cfg") > 0 && find_row("settings.cfg") > 0 &&
         find_row("game.crt") < 0 && find_row("ram.reu") < 0, "Load settings browser: .cfg files only");
   browse_to("mine.cfg");
   press(M2M_KEY_RETURN);
   check(cursor_on("Load settings") && find_row("=Load settings") < 0, "loaded: back in the menu, the line unmarked");
   check(setting_get(item(" High")) && setting_get(item(" 640x480 60 Hz")) &&
         setting_line("VIC-II Jailbars=High\nHDMI Display Mode=640x480 60 Hz\n"), "mine.cfg in effect, kept");
   check(setting_get(item(" 15 kHz with CSYNC")) && setting_line("VGA Display Mode=Standard\n"),
         "game.cfg's VGA mode in effect, not kept");
   // The VGA mode picked in the menu is kept
   go_to("VGA:");
   press(M2M_KEY_RETURN);
   go_to_line("15 kHz with HS/VS");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_RUNSTOP);
   // A file with errors: the rest in effect, the log as the message, lines cut to fit
   go_to("Load settings");
   press(M2M_KEY_RETURN);
   browse_to("bad.cfg");
   press(M2M_KEY_RETURN);
   dump("bad.cfg");
   check(find_row("There were errors; the rest is in effect.") > 0 &&
         find_row("Line 2: Value 'Huge' is not a valid choice") + 1 == find_row("for item VIC-II Jailbars") &&
         find_row("Line 4: Store name") + 1 == find_row("'A_store_name_far_too_long_for_one_line_of") &&
         find_row("'A_store_name_far_too_long_for_one_line_of") + 1 == find_row("_the_message' not found; section"),
         "bad.cfg: its log, cut to the screen");
   check(setting_get(item(" ASCII/DIN")) && setting_get(item(" High")) && setting_line("Caps Lock Key=ASCII/DIN\n") &&
         setting_line("VGA Display Mode=15 kHz with HS/VS\n"), "bad.cfg: its good line kept; the menu's VGA mode kept");
   press(M2M_KEY_SPACE);
   check(find_row("bad.cfg") > 0, "Space: the browser again");
   press(M2M_KEY_RUNSTOP);
   check(cursor_on("Load settings"), "Run/Stop: the menu");
   go_to("Load settings");
   press(M2M_KEY_RETURN);
   browse_to("back.cfg");
   press(M2M_KEY_RETURN);
   check(setting_get(item(" Standard")) && setting_get(item(" 40/80")) && setting_get(item(" 720p 50 Hz 16:9")) &&
         setting_line("VIC-II Jailbars=Off\nHDMI Display Mode=720p 50 Hz 16:9\n"), "back.cfg: as before");

   // Close Menu: next time from the start item
   go_to("Close Menu");
   press(M2M_KEY_RETURN);
   check(!(M2M_CSR & M2M_CSR_OSM), "Close Menu closes");
   idle();
   press(M2M_KEY_HELP);
   check(cursor_on("8:"), "cursor back on the start item");

   // Leave drive A's model as it was and the IEC port off, for the next run
   go_to("8:");
   press(M2M_KEY_RETURN);
   go_to_line("1571");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_RUNSTOP);
   go_to("Use IEC port");
   press(M2M_KEY_RETURN);
   press(M2M_KEY_HELP);
   idle();
   check(setting_line("[Drive A Settings]\nDrive Type=1571\n") && setting_line("Use IEC Port=Disabled\n"),
         "saved for the next run");
   // What the next run should start with
   uint8_t b[MENU_MAX];
   FIL f;
   UINT put;
   for (int i = 0; i < menu.n; i++)
      b[i] = setting_get(i);
   f_open(&f, "/bits.bin", FA_WRITE | FA_CREATE_ALWAYS);
   f_write(&f, b, menu.n, &put);
   f_close(&f);

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
