// See host_uci_model.h
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "host_uci_model.h"
#include "m2m_rv.h"
#include "sdcard.h"

#define UCI_DEVICE 0x0109u

// What the DOS targets use of the rest of the firmware
int  mounted_d = -1, mount_err;
char mounted_path[300];

enum sdcard_state sdcard_state(void) { return SDCARD_OK; }
int drives_count(void) { return 2; }

// iec_drive's disk_ready after a mount: still high for a few reads, low for drive_low reads
// (0: the drive never takes the disk), then high
int drive_low = 50, drive_reads;
int drives_ready(void)
{
   drive_reads++;
   return drive_reads <= 3 || drive_reads > 3 + drive_low || !drive_low;
}
const char *core_mount_label(int drive) { (void)drive; return NULL; }
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
}
