// What the REST routes use of the rest of the firmware, faked for test_http: the C128's
// memory behind a model of the bus DMA device (a job takes a few polls of its busy bit) and
// of the C128 booting to BASIC, the menu's settings bits, the drives, the drive ROM and
// cartridge devices, the REU's file and the network's settings. The card is a FAT image
// (FatFs and diskio_file.c).
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "config_ids.h"
#include "core.h"
#include "drives.h"
#include "ff.h"
#include "host_rest.h"
#include "net/netcfg.h"
#include "stores.h"
#include "roms.h"

uint8_t c128_mem[65536];
int     dma_held_v, dma_z80_v, dma_polls = 3, dma_jobs, dma_stops;
int     z80_cpm;                        // the Z80 runs CP/M until the next reset
int     drv_mounted[2], drv_ro[2], drv_type[2], drv_held[2], mounts, remaps;
char    drv_card[2][256];
uint16_t *drive_rom, *crt_mem, *sys_rom;
uint32_t  csr_reg = 0x0038;            // M2M_CSR: keyboard and joysticks, out of reset
int       func_size = -1, c128_resets;  // the function ROM's CSR; resets by a ROM change
int       rom_under_z80;                 // system ROM accesses while the Z80 runs
int     rom_reloads[2], crt_loads, crt_parsed;
char    crt_shown[256];
int     c128_basic, c128_c64, boot_never, c64_cart, typed_run;
char    typed[64];
static enum drive_power power[2];
static int ready_calls[2], rom_wait[2], reset_pend, z80_left, boot_left;
static uint32_t ramrom;
int     menu_buttons, menu_open, reboots, net_applies;
uint8_t settings[64][8];
int     drive_on[2] = { 1, 1 };
char    reu_path[256];
char    netcfg[NC_COUNT][32];

static uint8_t buf[2048];
static struct { int busy, addr, len, write; } job;

// --- the C128 -----------------------------------------------------------------------------

// The KERNAL's start: zero page and pages 2 and 3 cleared; BASIC a few jobs later
static void kernal(void)
{
   memset(c128_mem + 2, 0, 0x3FE);
   c128_basic = 0;
   boot_left = 5;
}

// A reset happens a few polls after it was asked for (main.vhd waits for the caches); the
// Z80 runs first, then the 8502 boots C128 BASIC, or C64 BASIC with a C64 cartridge
static void reset_now(void)
{
   dma_held_v = 0;
   z80_cpm = 0;                         // the Z80's boot then hands the bus to the 8502
   z80_left = 2;
   c128_c64 = c64_cart;
   kernal();
}

static int ndx_addr(void) { return c128_c64 ? 0xC6 : 0xD0; }
static int keyd_addr(void) { return c128_c64 ? 0x277 : 0x34A; }

// Time goes on: BASIC comes; at READY, with the CPU let go, the editor takes what was typed
static void c128_runs(void)
{
   if (!c128_basic && boot_left && !--boot_left && !boot_never) {
      c128_basic = 1;
      if (c128_c64) {
         memcpy(c128_mem + 0x2B, "\x01\x08\x03\x08", 4);
         memset(c128_mem + 0x800, 0, 3);
      } else {
         c128_mem[0xA00] = 3;
         memcpy(c128_mem + 0x2D, "\x01\x1C", 2);
         memcpy(c128_mem + 0x1210, "\x03\x1C", 2);
         memset(c128_mem + 0x1C00, 0, 3);
      }
   }
   int n = c128_mem[ndx_addr()];
   if (!c128_basic || dma_held_v || !n)
      return;
   char keys[16];
   memcpy(keys, c128_mem + keyd_addr(), n);
   keys[n] = 0;
   c128_mem[ndx_addr()] = 0;
   int at = strlen(typed);
   for (int i = 0; i < n && at < (int)sizeof typed - 1; i++)
      typed[at++] = keys[i] ? keys[i] : '@';         // a NUL shows as @
   typed[at] = 0;
   if (!c128_c64 && n == 7 && !memcmp(keys, "GO64\rY\r", 7)) {
      c128_c64 = 1;                                  // C64 mode: its KERNAL, its BASIC
      kernal();
   } else if (n == 4 && !memcmp(keys, "RUN\r", 4))
      typed_run++;
}

void dma_hold(int on)
{
   dma_held_v = on != 0;
   if (!on)
      c128_runs();
}

// A reset asked for earlier has happened, and its Z80 time is over
void c128_settle(void)
{
   if (reset_pend) {
      reset_pend = 0;
      reset_now();
   }
   z80_left = 0;
}

int dma_held(void)
{
   if (reset_pend && !--reset_pend)
      reset_now();
   return dma_held_v;
}

int dma_z80(void)
{
   if (z80_cpm && reset_pend && !--reset_pend)
      reset_now();
   if (z80_cpm)
      return 1;
   if (z80_left) {
      z80_left--;
      return 1;
   }
   return dma_z80_v;
}
void dma_put(int i, uint8_t b) { buf[i] = b; }
uint8_t dma_get(int i) { return buf[i]; }

void dma_start(uint16_t addr, int len, int write)
{
   c128_runs();
   job.busy = dma_polls;
   job.addr = addr;
   job.len = len;
   job.write = write;
   dma_jobs++;
}

int dma_busy(void)
{
   if (!job.busy)
      return 0;
   if (dma_z80_v || --job.busy)
      return 1;
   for (int i = 0; i < job.len; i++)
      if (job.write)
         c128_mem[(job.addr + i) & 0xFFFF] = buf[i];
      else
         buf[i] = c128_mem[(job.addr + i) & 0xFFFF];
   return 0;
}

void dma_stop(void)
{
   job.busy = 0;
   dma_stops++;
}

void shell_menu_button(void) { menu_buttons++; }

int shell_menu_screen(uint8_t *out)
{
   if (!menu_open)
      return 0;
   for (int i = 0; i < 2000; i++)
      out[i] = i;
   return 1;
}

void uci_reboot(void) { reboots++; reset_pend = 3; }
uint32_t heap_free(void) { return 3000000; }
uint32_t heap_total(void) { return 3500000; }

// The menu: REU 0 off, 1-3 a size; drive model 0 1541, 1 1571
int shell_setting(int group, int pos) { return settings[group][pos]; }
int shell_item(int group, int pos) { return pos < 4 ? pos : -1; }

void shell_force(int group, int pos, int on)
{
   if (on && group != OPTM_G_UCI && group != OPTM_G_NET_DHCP && group != OPTM_G_NET_REST && group != OPTM_G_NET_FTP)
      memset(settings[group], 0, sizeof settings[group]);
   settings[group][pos] = on;
}

void shell_crtrom_named(int group, const char *path)
{
   const char *n = path ? strrchr(path, '/') : NULL;
   (void)group;
   strcpy(crt_shown, !path ? "" : n ? n + 1 : path);
}

// --- the drives -----------------------------------------------------------------------------

int  drives_count(void) { return 2; }
int  drive_mounted(int d) { return drv_mounted[d]; }
const char *drive_path(int d) { return drv_mounted[d] ? drv_card[d] : ""; }
int  drive_image_type(int d) { return drv_mounted[d] ? drv_type[d] : -1; }
int  drive_running(int d) { return drive_on[d]; }
void drive_power(int d, enum drive_power p) { power[d] = p; drive_on[d] = p == DRIVE_ON; }
enum drive_power drive_power_of(int d) { return power[d]; }
void drive_hold(int d, int on) { drv_held[d] = on; }
void drive_flush(int d) { (void)d; }
int  drive_quiet(int d) { return drv_held[d]; }

// After a 1541/1571 mount: not ready on the next three asks, then ready
int drives_ready(void)
{
   for (int d = 0; d < 2; d++)
      if (ready_calls[d] && ready_calls[d]--)
         return 0;
   return 1;
}

// As the menu: an image for the internal floppy's drive turns it off first; a size the core
// takes; not the other drive's; then the model one the image runs
int shell_mount_ro(int d, const char *path, int ro, const char **msg)
{
   FILINFO fi;
   *msg = NULL;
   if (stores_drive_internal(d))
      stores_drive_set_model(d, DM_1581);
   if (f_stat(path, &fi) != FR_OK)
      return 0x104;
   int t = fi.fsize == 174848 || fi.fsize == 196608 ? 0 : fi.fsize == 349696 ? 1 : fi.fsize == 819200 ? 2 : -1;
   if (t < 0)
      return 1;
   if (drv_mounted[1 - d] && !strcmp(drv_card[1 - d], path))
      return 0xEE42;
   strcpy(drv_card[d], path);
   drv_mounted[d] = 1;
   drv_ro[d] = ro;
   drv_type[d] = t;
   stores_drive_set_model(d, stores_drive_fit(d, stores_drive_model(d)));
   mounts++;
   ready_calls[d] = t == 2 || power[d] == DRIVE_OFF ? 0 : 3;
   return 0;
}

void shell_unmount(int d) { drv_mounted[d] = 0; }

int drive_remap(int d, const char *path)
{
   FILINFO fi;
   if (!drv_held[d] || f_stat(path, &fi) != FR_OK)
      return 0x104;
   strcpy(drv_card[d], path);
   remaps++;
   return 0;
}

void core_sel_pre(int group, int item)
{
   (void)item;
   if (group == OPTM_G_MOUNT_CRT)
      shell_force(OPTM_G_CART, 1, 1);
}

// A .crt up to 4 MB
int core_prep_load(uint32_t size, int ctx, int group, int *type, const char **msg)
{
   (void)ctx, (void)group;
   *type = 0;
   *msg = NULL;
   return size > 4u << 20;
}

// --- the drive ROM and the cartridge --------------------------------------------------------

int      crtrom_count(void) { return 2; }
uint16_t crtrom_dev(int k) { return k ? 0x0200 : 0x0106; }

// The drive ROM's CSR: a drive has its ROM two asks after a reload; the cartridge's: 4 MB
uint16_t crtrom_csr_get(uint16_t dev, int reg)
{
   if (dev == 0x0102 && reg == 0) {
      int ready = 0;
      for (int d = 0; d < 2; d++)
         if (!rom_wait[d] || !--rom_wait[d])
            ready |= 1 << d;
      return ready;
   }
   if (dev == 0x0106 && reg == 0x20)
      return 64;
   return 0;
}

void crtrom_csr_set(uint16_t dev, int reg, uint16_t v)
{
   if (dev == 0x0101 && reg == 0)
      func_size = v;
   if (dev == 0x0102 && reg == 0)
      for (int d = 0; d < 2; d++)
         if (v >> d & 1) {
            rom_reloads[d]++;
            rom_wait[d] = 2;
         }
}

void crtrom_begin(int k) { (void)k; crt_loads++; }

// A .crt's header is all the parser looks at here
int crtrom_parse(int k, uint32_t size, const char **msg)
{
   char h[17];
   (void)k;
   for (int i = 0; i < 16; i++)
      h[i] = crt_mem[i];
   h[16] = 0;
   crt_parsed = size;
   if (!strcmp(h, "C64 CARTRIDGE   ") || !strcmp(h, "C128 CARTRIDGE  "))
      return 0;
   *msg = "Not a .crt file";
   return 0x11;
}

int crtrom_load_quiet(int k, const char *path, int group, const char **msg)
{
   FIL f;
   UINT got;
   int type;
   uint8_t b[512];
   *msg = NULL;
   crtrom_begin(k);
   if (f_open(&f, path, FA_READ) != FR_OK)
      return 0x104;
   if (core_prep_load(f_size(&f), CTX_LOAD_ROM, group, &type, msg)) {
      f_close(&f);
      return 1;
   }
   uint32_t at = 0;
   do {
      f_read(&f, b, sizeof b, &got);
      for (UINT i = 0; i < got; i++)
         crt_mem[at++] = b[i];
   } while (got == sizeof b);
   f_close(&f);
   return crtrom_parse(k, at, msg);
}

// --- registers and the device bus ---------------------------------------------------------

volatile uint32_t *host_rest_reg32(uint32_t a)
{
   // M2M_CSR: a write that holds the C128 in reset counts as a reset
   static uint32_t csr_was;
   if (a == 0xE0000000u) {
      if ((csr_reg & 1) && !(csr_was & 1))
         c128_resets++;
      csr_was = csr_reg;
      return (volatile uint32_t *)&csr_reg;
   }
   if (a == 0xFFF4BFF8u || a == 0xFFF4BFFCu) {   // MTIME: 10 us a read
      static uint64_t time;
      static uint32_t t;
      time += 1000;
      t = a == 0xFFF4BFF8u ? (uint32_t)time : (uint32_t)(time >> 32);
      return (volatile uint32_t *)&t;
   }
   if (a != 0xE000001Cu) {
      fprintf(stderr, "test_http: register %08X\n", a);
      exit(2);
   }
   return (volatile uint32_t *)&ramrom;
}

volatile uint16_t *host_rest_dev(void)
{
   if (ramrom == 0x0101) {
      if (z80_cpm || z80_left)
         rom_under_z80++;
      return sys_rom;
   }
   if (ramrom == 0x0102)
      return drive_rom;
   if (ramrom == 0x0106)
      return crt_mem;
   fprintf(stderr, "test_http: device %04X\n", ramrom);
   exit(2);
}
int  reu_file_picked(void) { return reu_path[0] != 0; }
const char *reu_file(void) { return reu_path[0] ? reu_path : "/c128/preload.reu"; }
void reu_file_set(const char *p) { strcpy(reu_path, p ? p : ""); }

int uci_reu_set(int pos)
{
   shell_force(OPTM_G_REU, pos, 1);
   return 0;
}

// Paths from the card's root, under /SD as an Ultimate shows them
const char *uci_card_path(const char *name, char *out)
{
   if (!name || strncasecmp(name, "/SD/", 4))
      return NULL;
   strcpy(out, name + 3);
   return out;
}

void netcfg_apply(void) { net_applies++; }

// The card
int disk_file_open(const char *path);
void disk_idle(void) { }

int host_rest_card(const char *img)
{
   static FATFS fs;
   return disk_file_open(img) && f_mount(&fs, "", 1) == FR_OK;
}

// The menu's defaults, in use and in the settings file
void host_rest_init(void)
{
   if (!drive_rom) {
      drive_rom = calloc(6 * 32768, sizeof *drive_rom);
      sys_rom = calloc(128 << 10, sizeof *sys_rom);
      crt_mem = calloc(4 << 20, sizeof *crt_mem);
   }
   memset(settings, 0, sizeof settings);
   settings[OPTM_G_NET_REST][0] = settings[OPTM_G_NET_FTP][0] = settings[OPTM_G_NET_DHCP][0] = 1;
   settings[OPTM_G_DRVA_MODEL][1] = settings[OPTM_G_DRVB_MODEL][1] = 1;   // 1571
   settings[OPTM_G_DRVA_ID][0] = settings[OPTM_G_DRVB_ID][1] = 1;         // 8, 9
   settings[OPTM_G_HDMI_AUDIO][0] = settings[OPTM_G_IEC][0] = 1;          // on, as config.vhd has them
}
