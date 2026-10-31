// See host_uci_model.h
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "config_ids.h"
#include "drives.h"
#include "host_uci_model.h"
#include "m2m_rv.h"
#include "sdcard.h"

#define UCI_DEVICE 0x0109u

// What the DOS targets use of the rest of the firmware
int  mounted_d = -1, mount_err;
char mounted_path[300];

enum sdcard_state sdcard_state(void) { return SDCARD_OK; }
int drives_count(void) { return 2; }
// The FTP server's: no menu settings file
int cfg_str(uint32_t sel, char *buf, int size) { buf[0] = 0; return 0; }

// iec_drive's disk_ready after a mount: still high for a few reads, low for drive_low reads
// (0: the drive never takes the disk), then high
int drive_low = 50, drive_reads;
int drives_ready(void)
{
   drive_reads++;
   return drive_reads <= 3 || drive_reads > 3 + drive_low || !drive_low;
}

// What the control target uses: vd_sd's drive power, the menu's settings, the menu
int     drive_pow[2], mounted_type, menu_opens;
uint8_t settings[64][8];
int  drive_image_type(int d) { return mounted_d == d ? mounted_type : -1; }
void drive_power(int d, enum drive_power p) { drive_pow[d] = p; }
int  drive_running(int d)
{
   return drive_pow[d] == DRIVE_ON || (drive_pow[d] == DRIVE_AUTO && mounted_d == d);
}
int  shell_setting(int group, int pos) { return settings[group][pos]; }

// The menu for LOAD_CONFIG: how many REU items it has (2 on R3 memory: Off, 512 KB), and the
// REU file's name
int  reu_items = 4, crt_named_calls;
char crt_named[300];
int  shell_item(int group, int pos) { return group == OPTM_G_REU && pos >= reu_items ? -1 : pos; }
void shell_crtrom_named(int group, const char *path)
{
   crt_named_calls += group == OPTM_G_REU_FILE;
   strcpy(crt_named, path ? path : "-");
}

// The menu's REU setting: the first size selected, 512 KB first, as main.vhd's reu_menu
unsigned reu_menu(void)
{
   for (int i = 1; i < 4; i++)
      if (settings[OPTM_G_REU][i])
         return i;
   return 0;
}

// main.vhd has a change of it three reads of word 0x012 later (or at reu_settle); one that
// lands while the keep bit is off is a reset of the C128
static unsigned reu_lag;
unsigned reu_main, reu_forces;
static void reu_clock(void)
{
   if (reu_lag && !--reu_lag && reu_main != reu_menu()) {
      reu_main = reu_menu();
      if (!um.reu_keep)
         um.reu_resets++;
   }
}
void reu_settle(void)
{
   while (reu_lag)
      reu_clock();
}
void reu_sync(void)
{
   reu_lag  = 0;
   reu_main = reu_menu();
}

void shell_force(int group, int pos, int on)
{
   if (group != OPTM_G_UCI)                    // the one single-select item it is used for
      memset(settings[group], 0, sizeof settings[group]);
   settings[group][pos] = on;
   if (group == OPTM_G_REU) {
      reu_lag = 3;
      reu_forces++;
   }
}

int ef_sector = -1, ef_starts, ef_busy;
uint16_t crtrom_csr_get(uint16_t dev, int reg)
{
   if (dev == 0x0106 && reg == 0x025 && ef_busy > 0) {
      ef_busy--;
      return 1;
   }
   return 0;
}
void crtrom_csr_set(uint16_t dev, int reg, uint16_t v)
{
   if (dev == 0x0106 && reg == 0x024)
      ef_sector = v;
   if (dev == 0x0106 && reg == 0x025) {
      ef_starts++;
      ef_busy = 20;                    // the erase takes 20 main-loop rounds
   }
}
void shell_open_menu(void) { menu_opens++; }
void shell_menu_button(void) { menu_opens++; }
int  shell_menu_screen(uint8_t *out) { (void)out; return 0; }
int  shell_save_settings(void) { return 0; }
int  shell_saved_setting(int group, int pos) { return settings[group][pos]; }
int shell_mount(int d, const char *path, const char **msg)
{
   *msg = 0;
   if (mount_err)
      return mount_err;
   mounted_d = d;
   strcpy(mounted_path, path);
   drive_reads = 0;
   return 0;
}
void shell_unmount(int d) { mounted_d = -(d + 10); }

// The REST routes link in too (http.c, through UCI's network target); UCI never calls these
int  shell_mount_ro(int d, const char *path, int ro, const char **msg) { (void)ro; return shell_mount(d, path, msg); }
int  drive_mounted(int d) { return mounted_d == d; }
const char *drive_path(int d) { return mounted_d == d ? mounted_path : ""; }
enum drive_power drive_power_of(int d) { return drive_pow[d]; }
void drive_hold(int d, int on) { (void)d, (void)on; }
void drive_flush(int d) { (void)d; }
int  drive_quiet(int d) { (void)d; return 1; }
int  drive_remap(int d, const char *path) { (void)d, (void)path; return 1; }
void core_sel_pre(int group, int item) { (void)group, (void)item; }
int  core_prep_load(uint32_t size, int ctx, int group, int *type, const char **msg)
{
   (void)size, (void)ctx, (void)group, (void)msg;
   *type = 0;
   return 1;
}
int      crtrom_count(void) { return 0; }
uint16_t crtrom_dev(int k) { (void)k; return 0; }
void     crtrom_begin(int k) { (void)k; }
int      crtrom_parse(int k, uint32_t size, const char **msg) { (void)k, (void)size, (void)msg; return 1; }
int      crtrom_load_quiet(int k, const char *path, int group, const char **msg)
{
   (void)k, (void)path, (void)group, (void)msg;
   return 1;
}
void shell_fatal(const char *msg, uint32_t code) { printf("FATAL %s %u\n", msg, code); exit(2); }
void uart_puts(const char *s) { fputs(s, stdout); }

// ------------------------------------------------------------------------------------------
// command_protocol.vhd: queue pointers are buffer offsets (command 0, response 896, status
// 1792), state 0 idle, 1 busy, 2 Data Last, 3 Data More; hs bits 0-2 new command, data
// accepted, abort
// ------------------------------------------------------------------------------------------

struct uci_model um;

static volatile uint16_t *buf(unsigned off)
{
   return host_dev_at(UCI_DEVICE) + 0x800 + off;
}

static int resp_valid(void)
{
   return um.resp - 896 < um.resp_len && (um.state & 2) && !(um.hs & 4);
}

static int stat_valid(void)
{
   return um.stat - 1792 < um.stat_len && (um.state & 2) && !(um.hs & 4);
}

unsigned uci_status(void)
{
   return resp_valid() << 7 | stat_valid() << 6 | um.state << 4 | um.error << 3 | um.hs;
}

unsigned host_uci_rd(unsigned reg)
{
   switch (reg) {
   case 0x3:  return uci_status();
   case 0xE:  return um.cmd & 0xFF;
   case 0xF:  return um.cmd >> 8;
   case 0x10: return um.c128_reset;
   case 0x11: return um.reu_mem;
   case 0x12: reu_clock(); return um.reu_keep | reu_main << 1;
   }
   return 0;
}

void host_uci_wr(unsigned reg, unsigned v)
{
   switch (reg) {
   case 0x2:
      if (v & 0x01) {
         um.hs &= ~1u;
         um.cmd = 0;
      }
      if (v & 0x02)
         um.hs &= ~2u;
      if (v & 0x04)
         um.hs &= ~4u;
      if (v & 0x10) {
         um.state = 2 | (v >> 5 & 1);
         um.resp  = 896;
         um.stat  = 1792;
      }
      if (v & 0x80) {
         um.state = 0;
         um.resp  = 896;
         um.stat  = 1792;
      }
      break;
   case 0xA:
      um.stat     = 1792;
      um.stat_len = v & 0xFF;
      break;
   case 0xC:
      um.resp     = 896;
      um.resp_len = (um.resp_len & 0x700) | (v & 0xFF);
      break;
   case 0xD:
      um.resp_len = (um.resp_len & 0xFF) | (v & 7) << 8;
      break;
   case 0x10:
      if (v & 1)
         um.c128_reset = 0;
      if (v & 2)
         um.reboots++;
      break;
   case 0x12:
      um.reu_keep = v & 1;
      break;
   }
}

// The C128's side: $DF1C control, $DF1D command, $DF1E response, $DF1F status
void c128_command(uint8_t b)
{
   *buf(um.cmd) = b;
   if (um.cmd != 895)
      um.cmd++;
}

void c128_control(unsigned v)
{
   if (v & 0x08)
      um.error = 0;
   if (v & 0x01) {
      if (um.state == 0) {
         um.state = 1;
         um.hs |= 1;
      } else {
         um.error = 1;
      }
      um.irq_en = v >> 5 & 1;
   }
   if ((v & 0x02) && (um.state & 2)) {
      um.hs    = (um.hs & ~2u) | (um.state & 1) << 1;
      um.state &= 1;
      um.irq_en = 0;
   }
   if (v & 0x04)
      um.hs |= 4;
}

uint8_t c128_ident(void)
{
   return um.irq_en && (um.state & 2) ? 0x49 : 0xC9;
}

uint8_t c128_response(void)
{
   um.irq_en = 0;
   uint8_t b = resp_valid() ? *buf(um.resp) : 0;
   if (um.resp != 1791)
      um.resp++;
   return b;
}

uint8_t c128_status_byte(void)
{
   um.irq_en = 0;
   uint8_t b = stat_valid() ? *buf(um.stat) : 0;
   if (um.stat != 2047)
      um.stat++;
   return b;
}


void uci_model_init(void)
{
   um.resp = 896;
   um.stat = 1792;
   settings[OPTM_G_REU][0]       = 1;   // REU: Off
   settings[OPTM_G_DRVA_MODEL][0] = settings[OPTM_G_DRVB_MODEL][0] = 1;   // 1541
   settings[OPTM_G_DRVA_ID][0] = settings[OPTM_G_DRVB_ID][1] = 1;         // 8, 9
   um.reu_mem = 0x8800;                 // SDRAM, window 0x0800 (C_SMAP_REU), as on the R6
}
