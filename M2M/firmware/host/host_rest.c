// What the REST routes use of the rest of the firmware, faked for test_http: the C128's
// memory behind a model of the bus DMA device (a job takes a few polls of its busy bit), the
// menu's settings bits, the drives, the REU's file and the network's settings.
#include <stdio.h>
#include <string.h>
#include "config_ids.h"
#include "drives.h"
#include "ff.h"
#include "host_rest.h"
#include "net/netcfg.h"

uint8_t c128_mem[65536];
int     dma_held_v, dma_z80_v, dma_polls = 3, dma_jobs, dma_stops;
int     menu_buttons, menu_open, reboots, saves, net_applies, net_saves;
uint8_t settings[32][8], saved[32][8];
int     drive_on[2] = { 1, 1 };
char    reu_path[256];
char    netcfg[NC_COUNT][32];

static uint8_t buf[2048];
static struct { int busy, addr, len, write; } job;

void dma_hold(int on) { dma_held_v = on != 0; }
int  dma_held(void) { return dma_held_v; }
int  dma_z80(void) { return dma_z80_v; }
void dma_put(int i, uint8_t b) { buf[i] = b; }
uint8_t dma_get(int i) { return buf[i]; }

void dma_start(uint16_t addr, int len, int write)
{
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

int  shell_save_settings(void) { saves++; return 0; }
void uci_reboot(void) { reboots++; dma_held_v = 0; }
uint32_t heap_free(void) { return 3000000; }
uint32_t heap_total(void) { return 3500000; }

// The menu: REU 0 off, 1-3 a size; drive model 0 1541, 1 1571
int shell_setting(int group, int pos) { return settings[group][pos]; }
int shell_saved_setting(int group, int pos) { return saved[group][pos]; }
int shell_item(int group, int pos) { return pos < 4 ? pos : -1; }

void shell_force(int group, int pos, int on)
{
   if (on && group != OPTM_G_UCI && group != OPTM_G_NET_DHCP && group != OPTM_G_NET_REST)
      memset(settings[group], 0, sizeof settings[group]);
   settings[group][pos] = on;
}

void shell_crtrom_named(int group, const char *path) { (void)group; (void)path; }
int  drive_image_type(int d) { (void)d; return -1; }
const char *core_mount_label(int d) { (void)d; return NULL; }
int  drive_running(int d) { return drive_on[d]; }
void drive_power(int d, enum drive_power p) { drive_on[d] = p == DRIVE_ON; }
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
   if (strncmp(name, "/SD/", 4))
      return NULL;
   strcpy(out, name + 3);
   return out;
}

void netcfg_apply(void) { net_applies++; }
int  netcfg_save(void) { net_saves++; return 0; }

// No card: network.cfg is never there
FRESULT f_open(FIL *f, const TCHAR *path, BYTE mode) { (void)f; (void)path; (void)mode; return FR_NO_FILE; }
FRESULT f_read(FIL *f, void *b, UINT n, UINT *got) { (void)f; (void)b; (void)n; *got = 0; return FR_OK; }
FRESULT f_close(FIL *f) { (void)f; return FR_OK; }

// The menu's defaults, in use and in the settings file
void host_rest_init(void)
{
   memset(settings, 0, sizeof settings);
   settings[OPTM_G_NET_REST][0] = settings[OPTM_G_NET_DHCP][0] = 1;
   settings[OPTM_G_DRV_MODEL][1] = 1;
   memcpy(saved, settings, sizeof saved);
}
