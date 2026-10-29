// The Ultimate Command Interface's firmware (CORE/firmware/uci*.c) against a model of
// Gideon's command_protocol.vhd, driven from the C128 side the way his
// tests/e2e/io/command_interface/uci_targets_test.py drives a real Ultimate (run_transport),
// with the replies his firmware gives. Then the DOS targets on a FAT32 image, with the
// replies and status texts of his filemanager/dos.cc, and the control target, with those of
// his io/command_interface/control_target.cc.
// usage: test_uci fat32.img   (it writes to the image)
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "config_ids.h"
#include "drives.h"
#include "ff.h"
#include "m2m_rv.h"
#include "reu.h"
#include "host_reu_sd.h"
#include "sdcard.h"
#include "uci.h"
#include "host_uci_model.h"

#define UCI_DEVICE 0x0109u

void host_init(void);
int  disk_file_open(const char *path);


static int fails, runs;

static void check(int ok, const char *what)
{
   runs++;
   if (!ok) {
      fails++;
      printf("FAIL %s\n", what);
   }
}

// ------------------------------------------------------------------------------------------
// What a C128 program does, with the firmware's main loop in between
// ------------------------------------------------------------------------------------------

static void poll(int n)
{
   while (n--)
      uci_poll();
}

static void push(const uint8_t *c, int len)
{
   for (int i = 0; i < len; i++)
      c128_command(c[i]);
   c128_control(0x01);
}

// Reads what is there of a reply: data and status
static int drain(uint8_t *data, char *text)
{
   int n = 0, k = 0;
   while (uci_status() & 0x80)
      data[n++] = c128_response();
   while (uci_status() & 0x40)
      text[k++] = (char)c128_status_byte();
   text[k] = 0;
   return n;
}

// Each block of the last reply: its length, whether more followed, its status
static int  blocks, block_len[300], block_more[300];
static char block_text[300][40];

// Poll until the command is answered: a deferred job takes many rounds
static void until_reply(void)
{
   for (int i = 0; i < 2000000 && um.state == 1; i++)
      uci_poll();
}

// The whole of a command's reply, every block, as uci.py's transact does
static int transact(const uint8_t *c, int len, uint8_t *data, char *text)
{
   int n = 0;
   push(c, len);
   for (blocks = 0; blocks < 300;) {
      until_reply();
      if (!(um.state & 2))
         break;
      int more = um.state == 3;
      int k = drain(data + n, text);
      block_len[blocks]  = k;
      block_more[blocks] = more;
      snprintf(block_text[blocks], sizeof block_text[0], "%s", text);
      blocks++;
      n += k;
      c128_control(0x02);
      if (!more)
         break;
   }
   return n;
}

static void expect(const char *what, const uint8_t *c, int len, const char *want_status,
                   const void *want, int want_len)
{
   static uint8_t data[70000];
   char text[300], msg[200];
   int n = transact(c, len, data, text);
   snprintf(msg, sizeof msg, "%s: status \"%s\", want \"%s\"", what, text, want_status);
   check(!strcmp(text, want_status), msg);
   snprintf(msg, sizeof msg, "%s: %d bytes of data, want %d", what, n, want_len);
   check(n == want_len && !memcmp(data, want, want_len), msg);
   check(uci_status() == 0, "idle afterwards");
}

// ------------------------------------------------------------------------------------------
// The DOS targets
// ------------------------------------------------------------------------------------------

static uint8_t rdata[70000];
static char    rtext[300];
static int     rlen;

// A DOS command: target, command byte, then the bytes given, and a string with its 0 if s
static void dos(int target, int cmd, const void *bytes, int n, const char *s)
{
   static uint8_t c[1000];
   int len = 2;
   c[0] = target;
   c[1] = cmd;
   if (n) {
      memcpy(c + 2, bytes, n);
      len += n;
   }
   if (s) {
      strcpy((char *)c + len, s);
      len += strlen(s);
   }
   rlen = transact(c, len, rdata, rtext);
}

static void want(const char *what, const char *st)
{
   char msg[400];
   snprintf(msg, sizeof msg, "%s: status \"%s\", want \"%s\"", what, rtext, st);
   check(!strcmp(rtext, st), msg);
}

static void want_data(const char *what, const char *st, const char *d)
{
   char msg[400];
   want(what, st);
   snprintf(msg, sizeof msg, "%s: data \"%.*s\", want \"%s\"", what, rlen, rdata, d);
   check(rlen == (int)strlen(d) && !memcmp(rdata, d, rlen), msg);
}

// The C128 resets while the clock's command register is read: it is done at once here
static volatile uint16_t *rtc;
static unsigned rtc_cmds;
static void rtc_hook(void)
{
   if (rtc[8] & 6)
      rtc_cmds = rtc[8];
   rtc[8] = 0;
}

static void dos_tests(void)
{
   static uint8_t pat[2400];
   static FATFS fs;                    // FatFs keeps it: the control target's tests use it too
   check(f_mount(&fs, "", 1) == FR_OK, "mount the image");
   for (int i = 0; i < 2400; i++)
      pat[i] = (uint8_t)(i * 13 + i / 256);

   // Folders: '/' holds SD
   dos(1, 0x12, NULL, 0, NULL);
   want_data("GET_PATH at the start", "00,OK", "/");
   dos(1, 0x13, NULL, 0, NULL);
   want("OPEN_DIR /", "00,OK");
   dos(1, 0x14, NULL, 0, NULL);
   check(blocks == 1 && rlen == 3 && rdata[0] == AM_DIR && !memcmp(rdata + 1, "SD", 2) &&
         !strcmp(rtext, "00,OK"), "READ_DIR / is SD, a folder");
   dos(1, 0x11, NULL, 0, "nope");
   want("CHANGE_DIR to nothing", "83,NO SUCH DIRECTORY");
   dos(1, 0x11, NULL, 0, "sd");
   want("CHANGE_DIR sd", "00,OK");
   dos(1, 0x12, NULL, 0, NULL);
   want_data("GET_PATH", "00,OK", "/sd/");
   dos(1, 0x11, NULL, 0, "c128/../big\\");
   dos(1, 0x12, NULL, 0, NULL);
   want_data("CHANGE_DIR with .. and \\", "00,OK", "/sd/big/");
   dos(1, 0x11, NULL, 0, "/");
   dos(1, 0x12, NULL, 0, NULL);
   want_data("CHANGE_DIR /", "00,OK", "/");
   dos(1, 0x17, NULL, 0, NULL);
   want_data("COPY_HOME_PATH", "00,OK", "/SD/");
   dos(2, 0x12, NULL, 0, NULL);
   want_data("DOS 2 has its own folder", "00,OK", "/");
   dos(1, 0x14, NULL, 0, NULL);
   want("READ_DIR without OPEN_DIR", "87,INTERNAL ERROR");

   // The card's folder: every entry, one a block, the last with 00,OK
   dos(1, 0x13, NULL, 0, NULL);
   want("OPEN_DIR /SD", "00,OK");
   dos(1, 0x14, NULL, 0, NULL);
   int seen = 0, shape = blocks >= 4 && !strcmp(rtext, "00,OK") && !block_more[blocks - 1];
   for (int b = 0, off = 0; b < blocks; off += block_len[b++]) {
      const char *name = (const char *)rdata + off + 1;
      int n = block_len[b] - 1;
      if (b < blocks - 1 && (!block_more[b] || block_text[b][0]))
         shape = 0;
      if ((n == 4 && !memcmp(name, "c128", 4)) || (n == 3 && !memcmp(name, "bad", 3)) ||
          (n == 3 && !memcmp(name, "big", 3)) || (n == 28 && !memcmp(name, "Long folder name with spaces", 28)))
         seen += rdata[off] & AM_DIR ? 1 : 0;
   }
   check(shape, "READ_DIR /SD: one entry a block, Data More up to the last");
   check(seen == 4, "READ_DIR /SD lists the image's four folders");

   // A file: write, close, stat, read in blocks, seek, info
   dos(1, 0x16, NULL, 0, "uci");
   want("CREATE_DIR uci", "00,OK");
   dos(1, 0x11, NULL, 0, "uci");
   dos(1, 0x13, NULL, 0, NULL);
   want("OPEN_DIR on an empty folder", "01,DIRECTORY EMPTY");
   dos(1, 0x02, "\x06", 1, "test.prg");
   want("OPEN_FILE for a new file", "00,OK");
   for (int i = 0; i < 3; i++) {
      uint8_t w[2 + 800] = { 0, 0 };
      memcpy(w + 2, pat + 800 * i, 800);
      dos(1, 0x05, w, sizeof w, NULL);
      want("WRITE_DATA", "00,OK");
   }
   dos(1, 0x03, NULL, 0, NULL);
   want("CLOSE_FILE", "00,OK");
   dos(1, 0x03, NULL, 0, NULL);
   want("CLOSE_FILE again", "84,NO FILE TO CLOSE");
   dos(1, 0x04, "\x10\x00", 2, NULL);
   want("READ_DATA with no file", "85,NO FILE OPEN");
   dos(1, 0x02, "\x06", 1, "test.prg");
   want("OPEN_FILE CREATE_NEW on a file there", "FILE EXISTS");
   dos(1, 0x08, NULL, 0, "test.prg");
   check(rlen == 12 + 8 && rdata[0] == 0x60 && rdata[1] == 0x09 && rdata[2] == 0 && rdata[3] == 0 &&
         !memcmp(rdata + 8, "prg", 3) && (rdata[11] & AM_ARC) && !memcmp(rdata + 12, "test.prg", 8) &&
         !strcmp(rtext, "00,OK"), "FILE_STAT: 2400 bytes, prg, archive, the name");
   dos(1, 0x08, NULL, 0, "none.prg");
   want("FILE_STAT on nothing", "82,FILE NOT FOUND");

   dos(1, 0x02, "\x01", 1, "/SD/uci/test.prg");
   want("OPEN_FILE to read, from the root", "00,OK");
   dos(1, 0x04, "\x60\x09", 2, NULL);
   int sizes = blocks == 5;
   for (int b = 0; b < blocks; b++)
      sizes &= block_len[b] == (b < 4 ? 512 : 352) && block_more[b] == (b < 4) && !block_text[b][0];
   check(sizes, "READ_DATA 2400: 4 blocks of 512 and 352, status empty");
   check(rlen == 2400 && !memcmp(rdata, pat, 2400), "READ_DATA's bytes");
   dos(1, 0x06, "\xE8\x03\x00\x00", 4, NULL);
   want("FILE_SEEK 1000", "00,OK");
   dos(1, 0x04, "\x0A\x00", 2, NULL);
   check(rlen == 10 && !memcmp(rdata, pat + 1000, 10), "READ_DATA after the seek");
   dos(1, 0x06, "\x00\x00\x00\x00", 4, NULL);
   dos(1, 0x04, "\x88\x13", 2, NULL);
   check(rlen == 2400 && blocks == 5 && !block_more[4], "READ_DATA past the end stops at it");
   dos(1, 0x07, NULL, 0, NULL);
   check(rlen == 20 && rdata[0] == 0x60 && rdata[1] == 0x09 && !memcmp(rdata + 12, "test.prg", 8),
         "FILE_INFO of the open file");
   dos(1, 0x03, NULL, 0, NULL);

   // Rename, copy (a job over many rounds), delete
   dos(1, 0x0A, NULL, 0, "test.prg");
   memcpy(rdata, "test.prg\0t2.prg", 15);
   dos(1, 0x0A, rdata, 15, NULL);
   want("RENAME_FILE", "00,OK");
   dos(1, 0x08, NULL, 0, "test.prg");
   want("the old name is gone", "82,FILE NOT FOUND");
   dos(1, 0x0B, "t2.prg\0/SD/c128", 15, NULL);
   want("COPY_FILE", "00,OK");
   dos(1, 0x08, NULL, 0, "/SD/c128/t2.prg");
   check(rlen == 18 && rdata[0] == 0x60 && rdata[1] == 0x09, "the copy is whole");
   dos(1, 0x0B, "t2.prg\0/SD/c128", 15, NULL);
   want("COPY_FILE onto a file there", "FILE EXISTS");
   dos(1, 0x09, NULL, 0, "/SD/c128/t2.prg");
   dos(1, 0x0B, "t2.prg\0SD/c128", 14, NULL);
   want("COPY_FILE's folder is named from the root", "00,OK");
   dos(1, 0x0B, "t2.prg\0c128", 11, NULL);
   want("COPY_FILE to a folder not in the root", "PATH DOESN'T EXIST");
   dos(1, 0x0B, "t2.prg\0/SD/uci", 15, NULL);
   want("COPY_FILE onto itself", "00,OK");
   // 1 MB and a bit: the copy has the size, the first block and the last of the file
   static uint8_t head[512], tail[512];
   uint8_t at[4];
   dos(1, 0x08, NULL, 0, "/SD/big/fw1m.bin");
   uint32_t size = rdata[0] | rdata[1] << 8 | rdata[2] << 16;
   at[0] = size - 512, at[1] = (size - 512) >> 8, at[2] = (size - 512) >> 16, at[3] = 0;
   dos(1, 0x02, "\x01", 1, "/SD/big/fw1m.bin");
   dos(1, 0x04, "\x00\x02", 2, NULL);
   memcpy(head, rdata, 512);
   dos(1, 0x06, at, 4, NULL);
   dos(1, 0x04, "\x00\x02", 2, NULL);
   memcpy(tail, rdata, 512);
   dos(1, 0x0B, "/SD/big/fw1m.bin\0/SD/uci", 25, NULL);
   want("COPY_FILE of 1 MB", "00,OK");
   dos(1, 0x08, NULL, 0, "fw1m.bin");
   check(size > 0x100000 && rlen == 20 && (uint32_t)(rdata[0] | rdata[1] << 8 | rdata[2] << 16) == size,
         "the 1 MB copy has the file's size");
   dos(1, 0x02, "\x01", 1, "fw1m.bin");
   dos(1, 0x04, "\x00\x02", 2, NULL);
   int same = rlen == 512 && !memcmp(rdata, head, 512);
   dos(1, 0x06, at, 4, NULL);
   dos(1, 0x04, "\x00\x02", 2, NULL);
   check(same && rlen == 512 && !memcmp(rdata, tail, 512), "and its first and last blocks");
   dos(1, 0x03, NULL, 0, NULL);
   dos(1, 0x09, NULL, 0, "fw1m.bin");
   push((uint8_t *)"\x01\x0B/SD/big/fw1m.bin\0/SD/uci", 27);
   poll(5);
   check(um.state == 1, "a long copy keeps the command busy");
   c128_control(0x04);
   poll(2);
   dos(1, 0x08, NULL, 0, "fw1m.bin");
   want("ABORT drops the unfinished copy", "82,FILE NOT FOUND");
   dos(1, 0x09, NULL, 0, "/SD/c128/t2.prg");
   want("DELETE_FILE", "00,OK");
   dos(1, 0x09, NULL, 0, "/SD/c128/t2.prg");
   want("DELETE_FILE again", "FILE DOESN'T EXIST");
   dos(1, 0x09, NULL, 0, "/nowhere");
   want("DELETE_FILE outside SD", "PATH DOESN'T EXIST");

   // A C128 reset closes the file, so what was written is on the card
   dos(2, 0x02, "\x0A", 1, "/SD/uci/reset.bin");
   dos(2, 0x05, "\0\0hello", 7, NULL);
   um.c128_reset = 1;
   poll(2);
   dos(1, 0x08, NULL, 0, "reset.bin");
   check(rlen == 21 && rdata[0] == 5, "a C128 reset closes the file, its bytes written");
   dos(2, 0x03, NULL, 0, NULL);
   want("and the file is closed", "84,NO FILE TO CLOSE");

   // Disk images: drive 8 or 9, or 0 for the last one
   dos(1, 0x23, "\x08", 1, "/SD/Long folder name with spaces/A file with a rather long name, to see.d64");
   want("MOUNT_DISK a D64 on drive 8", "00,OK");
   check(drive_reads == 3 + drive_low + 1, "MOUNT_DISK answers when disk_ready is back, after it went low");
   check(mounted_d == 0 && !strcmp(mounted_path, "/Long folder name with spaces/A file with a rather long name, to see.d64"),
         "the Shell mounts it on drive 8, the path on the card");
   dos(1, 0x23, "\x09", 1, "reset.bin");
   want("MOUNT_DISK a .bin", "89,NOT A DISK IMAGE");
   dos(1, 0x23, "\x09", 1, "none.d64");
   want("MOUNT_DISK nothing", "82,FILE NOT FOUND");
   dos(1, 0x23, "\x0A", 1, "/SD/Long folder name with spaces/A file with a rather long name, to see.d64");
   want("MOUNT_DISK on drive 10", "90,DRIVE NOT PRESENT");
   mount_err = 0xEE42;
   dos(1, 0x23, "\x09", 1, "/SD/Long folder name with spaces/A file with a rather long name, to see.d64");
   want("MOUNT_DISK of the image in the other drive", "98,FUNCTION PROHIBITED");
   mount_err = 0;
   dos(1, 0x24, "\x00", 1, NULL);
   check(mounted_d == -10 && !strcmp(rtext, "00,OK"), "UMOUNT_DISK 0: the drive last mounted");
   dos(1, 0x24, "\x09", 1, NULL);
   check(mounted_d == -11, "UMOUNT_DISK 9");
   push((uint8_t *)"\x01\x23\x09/SD/Long folder name with spaces/A file with a rather long name, to see.d64", 79);
   poll(20);
   check(um.state == 1, "and the C128 sees it busy until then");
   until_reply();
   c128_control(0x02);
   drive_low = 0;
   dos(1, 0x23, "\x09", 1, "/SD/Long folder name with spaces/A file with a rather long name, to see.d64");
   want("a drive that never takes the disk", "87,INTERNAL ERROR");
   drive_low = 50;
   dos(1, 0x02, "\x0A", 1, "/SD/uci/x.d81");
   dos(1, 0x03, NULL, 0, NULL);
   dos(1, 0x23, "\x09", 1, "/SD/uci/x.d81");
   check(drive_reads == 0 && !strcmp(rtext, "00,OK"), "a D81 answers at once: a 1581 flags the change itself");

   // The clock: BCD in rtc_controller's words 1-7 (day 0 is Monday)
   rtc = host_dev_at(M2M_DEV_RTC);
   uint16_t now[8] = { 0, 0x09, 0x05, 0x21, 0x27, 0x10, 0x26, 1 };
   for (int i = 0; i < 8; i++)
      rtc[i] = now[i];
   dos(1, 0x26, NULL, 0, NULL);
   want_data("GET_TIME", "00,OK", "2026/10/27 21:05:09");
   dos(1, 0x26, "\x01", 1, NULL);
   want_data("GET_TIME with the day", "00,OK", "TUE 2026/10/27 21:05:09");
   host_dev_hook = rtc_hook;
   dos(1, 0x27, "\x7E\x0C\x19\x08\x1E\x00", 6, NULL);
   host_dev_hook = 0;
   want_data("SET_TIME 2026-12-25 08:30", "00,OK", "FRI 2026/12/25 08:30:00");
   check(rtc[1] == 0 && rtc[2] == 0x30 && rtc[3] == 0x08 && rtc[4] == 0x25 && rtc[5] == 0x12 &&
         rtc[6] == 0x26 && rtc[7] == 4 && rtc_cmds == 0x0C, "SET_TIME writes the clock, then the chip");
   dos(1, 0x27, "\x7E\x0C\x19", 3, NULL);
   want_data("SET_TIME too short", "21,UNKNOWN COMMAND", "05");

   dos(1, 0x15, NULL, 0, NULL);
   want("COPY_UI_PATH", "99,FUNCTION NOT IMPLEMENTED");
}

static void want_bytes(const char *what, const char *st, const uint8_t *d, int n)
{
   char msg[400];
   int k = snprintf(msg, sizeof msg, "%s: %d bytes", what, rlen);
   want(what, st);
   for (int i = 0; i < rlen && i < 16 && k < 300; i++)
      k += snprintf(msg + k, sizeof msg - k, " %02X", rdata[i]);
   snprintf(msg + k, sizeof msg - k, ", want %d", n);
   check(rlen == n && !memcmp(rdata, d, n), msg);
}

static void ctrl_tests(void)
{
   static const uint8_t no_reu[] = { 0xFE, 0xFF, 0xFF, 0xFF }, zeros[8] = { 0 };
   static const uint8_t track[12] = { 0 };
   mounted_d = -1;

   dos(4, 0x01, NULL, 0, NULL);
   want_data("CONTROL IDENTIFY", "00,OK", "MEGA128 CONTROL V1.0");
   dos(4, 0x03, NULL, 0, NULL);
   want_data("FINISH_CAPTURE", "00,OK", "");
   int m = menu_opens;
   dos(4, 0x05, NULL, 0, NULL);
   want_data("FREEZE: no status", "", "");
   check(menu_opens == m + 1, "FREEZE opens the menu");
   unsigned r = um.reboots;
   dos(4, 0x06, NULL, 0, NULL);
   want_data("REBOOT: no status", "", "");
   check(um.reboots == r + 1, "REBOOT asks uci.vhd for one reset");

   // REU: at least 5 bytes; -2 while it is off
   dos(4, 0x08, "\0\0", 2, NULL);
   want_data("LOAD_REU of 4 bytes", "81,INVALID PARAMS", "");
   dos(4, 0x08, "\0\0\0", 3, NULL);
   want_bytes("LOAD_REU, REU off", "84,REU NOT ENABLED", no_reu, 4);
   dos(4, 0x09, "\0\0\0", 3, NULL);
   want_bytes("SAVE_REU, REU off", "84,REU NOT ENABLED", no_reu, 4);

   dos(4, 0x11, NULL, 0, NULL);
   want_data("DECODE_TRACK without its 12 bytes", "81,INVALID PARAMS", "");
   dos(4, 0x11, track, 12, NULL);
   want_bytes("DECODE_TRACK: no sectors", "82,ERRORS ON TRACK", zeros, 1);
   dos(4, 0x20, NULL, 0, NULL);
   want("EASYFLASH without a subcommand", "81,INVALID PARAMS");
   dos(4, 0x20, "\1\0\0", 3, NULL);
   want("EASYFLASH subcommand 1", "81,INVALID PARAMS");
   dos(4, 0x20, "\0\0", 2, NULL);
   want("EASYFLASH erase without its address", "81,INVALID PARAMS");
   push((const uint8_t[]){ 4, 0x20, 0, 0x0A, 0x20 }, 5);  // bank 10 of ROMH: its sector, 8-15
   int polls;
   for (polls = 0; um.state == 1 && polls < 1000; polls++)
      uci_poll();
   rlen = drain(rdata, rtext);
   c128_control(0x02);
   want_data("EASYFLASH erase", "00,OK", "");
   check(ef_sector == 0x48 && ef_starts == 1, "EASYFLASH erase: ROMH's sector from bank 8, started once");
   check(polls >= 20 && ef_busy == 0, "EASYFLASH erase answers once it is done");
   dos(4, 0x20, "\0\x07\0", 3, NULL);
   check(ef_sector == 0x00 && ef_starts == 2, "EASYFLASH erase: ROML's sector from bank 0");

   dos(4, 0x28, NULL, 0, NULL);
   want_data("GET_HWINFO", "00,OK", "C128 for MEGA65");
   dos(4, 0x28, "\0", 1, NULL);
   want_data("GET_HWINFO 0", "00,OK", "C128 for MEGA65");
   dos(4, 0x28, "\1", 1, NULL);
   want_bytes("GET_HWINFO 1: no SIDs of the cartridge's own", "00,OK", zeros, 1);
   dos(4, 0x28, "\2", 1, NULL);
   want_data("GET_HWINFO 2", "81,INVALID PARAMS", "");

   // Drives: type, address, power; a D71 in 9; then switched on and off
   dos(4, 0x29, NULL, 0, NULL);
   want_bytes("GET_DRVINFO, no disks", "00,OK", (uint8_t[]){ 2, 0, 8, 0, 0, 9, 0 }, 7);
   mounted_d    = 1;
   mounted_type = 1;
   settings[OPTM_G_DRV_MODEL][0] = 0;
   dos(4, 0x29, "\1", 1, NULL);
   want_bytes("GET_DRVINFO, a D71 in 9, model 1571", "00,OK", (uint8_t[]){ 2, 1, 8, 0, 1, 9, 1 }, 7);
   mounted_type = 2;
   dos(4, 0x29, NULL, 0, NULL);
   want_bytes("GET_DRVINFO, a D81 in 9", "00,OK", (uint8_t[]){ 2, 1, 8, 0, 2, 9, 1 }, 7);
   settings[OPTM_G_DRV_MODEL][0] = 1;
   dos(4, 0x34, NULL, 0, NULL);
   want_data("DISK_A_POWER, no disk", "00,OK", "off");
   dos(4, 0x35, NULL, 0, NULL);
   want_data("DISK_B_POWER, a disk", "00,OK", "on ");
   dos(4, 0x30, NULL, 0, NULL);
   want_data("ENABLE_DISK_A", "00,OK", "");
   dos(4, 0x33, NULL, 0, NULL);
   want_data("DISABLE_DISK_B", "00,OK", "");
   check(drive_pow[0] == DRIVE_ON && drive_pow[1] == DRIVE_OFF, "the drives' power set as asked");
   dos(4, 0x34, NULL, 0, NULL);
   want_data("DISK_A_POWER, switched on", "00,OK", "on ");
   dos(4, 0x35, NULL, 0, NULL);
   want_data("DISK_B_POWER, switched off", "00,OK", "off");
   dos(4, 0x29, NULL, 0, NULL);
   want_bytes("GET_DRVINFO after", "00,OK", (uint8_t[]){ 2, 0, 8, 1, 2, 9, 0 }, 7);
   dos(4, 0x31, NULL, 0, NULL);
   dos(4, 0x32, NULL, 0, NULL);
   check(drive_pow[0] == DRIVE_OFF && drive_pow[1] == DRIVE_ON, "DISABLE_DISK_A, ENABLE_DISK_B");
   drive_pow[0] = drive_pow[1] = DRIVE_AUTO;
   mounted_d = -1;

   dos(4, 0x40, NULL, 0, NULL);
   want_bytes("GET_RAMDISKINFO: none", "00,OK", zeros, 8);
   dos(4, 0x50, NULL, 0, NULL);
   want_data("LOAD_CONFIG of /temp/uci_config.cfg", "88,CANNOT OPEN CONFIG FILE", "");
   dos(4, 0x50, NULL, 0, "/SD/NONE.CFG");
   want("LOAD_CONFIG of a missing file", "88,CANNOT OPEN CONFIG FILE");

   // The Ultimate 64's, READ_RTC and ENCODE_TRACK: unknown on an Ultimate II too
   static const int u64[] = { 0x02, 0x0F, 0x12, 0x51, 0x52, 0x53, 0x54, 0x7F };
   for (unsigned i = 0; i < sizeof u64 / sizeof u64[0]; i++) {
      char what[40];
      snprintf(what, sizeof what, "control command $%02X", u64[i]);
      dos(4, u64[i], NULL, 0, NULL);
      want_data(what, "21,UNKNOWN COMMAND", "");
   }
}

// ------------------------------------------------------------------------------------------
// LOAD_CONFIG: the log and status of Gideon's ConfigIO::S_read_from_file, and the settings
// ------------------------------------------------------------------------------------------

// text as /SD/CFG.CFG, then LOAD_CONFIG of it, its name with the 0 byte a program sends
static void cfg(const char *text)
{
   FIL f;
   UINT put;
   check(f_open(&f, "/CFG.CFG", FA_WRITE | FA_CREATE_ALWAYS) == FR_OK &&
         f_write(&f, text, strlen(text), &put) == FR_OK && put == strlen(text), "write /CFG.CFG");
   f_close(&f);
   dos(4, 0x50, "/SD/CFG.CFG", 12, NULL);
   reu_settle();
}

// main.vhd's REU setting, the same as the menu's
static int reu_pos(void)
{
   return reu_main == reu_menu() ? (int)reu_main : -1;
}

static void cfg_tests(void)
{
   static char big[2000];
   const unsigned resets = um.reu_resets;
   memset(settings[OPTM_G_REU], 0, sizeof settings[OPTM_G_REU]);
   memset(settings[OPTM_G_MODEM], 0, sizeof settings[OPTM_G_MODEM]);
   settings[OPTM_G_REU][0]   = 1;                   // REU off, modem off, UCI on, 1571
   settings[OPTM_G_MODEM][0] = 1;
   settings[OPTM_G_UCI][0]   = 1;
   settings[OPTM_G_DRV_MODEL][0] = 0;
   settings[OPTM_G_DRV_MODEL][1] = 1;
   mounted_d = -1;
   drive_pow[0] = drive_pow[1] = DRIVE_AUTO;
   reu_sync();

   cfg("# a comment\r\n; another\r\n\r\n[Nonexistent Store]\r\nSomething=1\r\n"
       "[C64 and Cartridge Settings]\r\nNo Such Item=1\r\nFast Reset=Enabled\r\n");
   want_data("LOAD_CONFIG: comments, a store and items the core lacks", "00,OK",
             "Line 4: Store name 'Nonexistent Store' not found; section ignored.\n"
             "Line 7: Item 'No Such Item' not found in this store [C64 and Cartridge Settings]; ignored.\n"
             "Line 8: Item 'Fast Reset' not found in this store [C64 and Cartridge Settings]; ignored.\n");

   cfg("Orphan=1\n[C64 and Cartridge Settings]\nRAM Expansion Unit=Enabled\nREU Size=3 MB\n=5\n"
       "REU Size=1 MB\nRAM Expansion Unit=GeoRAM Mode\n[Drive A Settings]\nDrive Bus ID=x\n");
   want_data("LOAD_CONFIG: errors", "89,CONFIG FILE HAD ERRORS",
             "Line 1: Not inside valid store.\n"
             "Line 4: Value '3 MB' is not a valid choice for item REU Size\n"
             "Line 5: No item name.\n"
             "Line 6: Value '1 MB' is not a valid choice for item REU Size\n"
             "Line 7: Value 'GeoRAM Mode' is not a valid choice for item RAM Expansion Unit\n");
   check(reu_pos() == 0, "LOAD_CONFIG with errors changes nothing (the REU stays off)");
   cfg("[C64 and Cartridge Settings]\nRAM Expansion Unit\n");
   want_data("LOAD_CONFIG: no value", "89,CONFIG FILE HAD ERRORS",
             "Line 2: No value given for item 'RAM Expansion Unit'.\n");
   cfg("[C64 and Cartridge Settings\nRAM Expansion Unit=Enabled\n");
   want_data("LOAD_CONFIG: a store without its ]", "89,CONFIG FILE HAD ERRORS",
             "Line 2: Not inside valid store.\n");

   // The REU, without main.vhd's reset (uci.vhd's keep bit around the change)
   cfg("[C64 and Cartridge Settings]\nRAM Expansion Unit=Enabled\nREU Size=512 KB\nCommand Interface=Enabled\n");
   want_data("LOAD_CONFIG: REU on, 512 KB", "00,OK", "");
   check(reu_pos() == 1 && settings[OPTM_G_UCI][0], "the REU is on with 512 KB");
   check(um.reu_resets == resets && !um.reu_keep, "the REU changed with the keep bit, which is off again");
   cfg("[cartridge and rom settings]\n  reu size =  16 mb \t\n");
   want_data("LOAD_CONFIG: his other store name, any case, spaces", "00,OK", "");
   check(reu_pos() == 3, "the REU is 16 MB");
   reu_items = 2;                                   // R3 memory: Off and 512 KB
   cfg("[C64 and Cartridge Settings]\nREU Size=2 MB\n");
   want_data("LOAD_CONFIG: 2 MB on R3 memory", "89,CONFIG FILE HAD ERRORS",
             "Line 2: Value '2 MB' is not a valid choice for item REU Size\n");
   reu_items = 4;
   cfg("[C64 and Cartridge Settings]\nRAM Expansion Unit=Disabled\n");
   want("LOAD_CONFIG: REU off", "00,OK");
   check(reu_pos() == 0, "the REU is off");
   cfg("[C64 and Cartridge Settings]\nREU Size=2 MB\n");
   check(reu_pos() == 0, "REU Size while it is off: it stays off");
   cfg("[C64 and Cartridge Settings]\nRAM Expansion Unit=Enabled\n");
   check(reu_pos() == 2, "and comes on with that size");
   cfg("[C64 and Cartridge Settings]\nRAM Expansion Unit=Enabled\n");
   check(reu_pos() == 2, "Enabled again: no change");
   check(um.reu_resets == resets, "no REU change without the keep bit");
   cfg("[C64 and Cartridge Settings]\nRAM Expansion Unit=Disabled");
   check(reu_pos() == 2, "a last line without its LF isn't read, as on an Ultimate");

   // Drives: power, the one 5.25" model, their addresses
   cfg("[Drive A Settings]\nDrive=Disabled\nDrive Type=1541\nDrive Bus ID=8\n"
       "[Drive B Settings]\nDrive=Enabled\nDrive Bus ID= 9\nROM for 1541 mode=1541.rom\n");
   want_data("LOAD_CONFIG: drives", "00,OK",
             "Line 8: Item 'ROM for 1541 mode' not found in this store [Drive B Settings]; ignored.\n");
   check(drive_pow[0] == DRIVE_OFF && drive_pow[1] == DRIVE_ON, "drive A off, B on");
   check(settings[OPTM_G_DRV_MODEL][0] && !settings[OPTM_G_DRV_MODEL][1], "the model is 1541");
   cfg("[Drive A Settings]\nDrive Type=1571\n[Drive B Settings]\nDrive Type=1541\nDrive Bus ID=10\n"
       "[Drive A Settings]\nDrive Type=1581\nDrive Bus ID=x\n");
   want_data("LOAD_CONFIG: two models, address 10, a 1581 that isn't one", "89,CONFIG FILE HAD ERRORS",
             "Line 4: Value '1541' is not a valid choice for item Drive Type\n"
             "Line 5: Value '10' is not a valid choice for item Drive Bus ID\n"
             "Line 7: Value '1581' is not a valid choice for item Drive Type\n");
   check(settings[OPTM_G_DRV_MODEL][0] && drive_pow[0] == DRIVE_OFF, "nothing changed");
   mounted_d    = 1;
   mounted_type = 2;
   cfg("[Drive B Settings]\nDrive Type=1581\n[Drive A Settings]\nDrive Type=1571\nDrive=Enabled\n");
   want_data("LOAD_CONFIG: B holds a D81, A 1571", "00,OK", "");
   check(settings[OPTM_G_DRV_MODEL][1] && drive_pow[0] == DRIVE_ON, "the model is 1571, A is on");
   mounted_d = -1;
   drive_pow[0] = drive_pow[1] = DRIVE_AUTO;

   // The modem, a SwiftLink with NMI at $DE00 or $DF00
   cfg("[Modem Settings]\nModem Interface=ACIA / SwiftLink\nACIA (6551) Mapping=DF00/NMI\n"
       "Hardware Mode=SwiftLink\nListening Port=6400\n");
   want_data("LOAD_CONFIG: the modem", "00,OK",
             "Line 5: Item 'Listening Port' not found in this store [Modem Settings]; ignored.\n");
   check(settings[OPTM_G_MODEM][2] && !settings[OPTM_G_MODEM][0], "the modem is at $DF00");
   cfg("[Modem Settings]\nACIA (6551) Mapping=DE00/IRQ\nHardware Mode=Turbo232\n");
   want_data("LOAD_CONFIG: IRQ, Turbo232", "89,CONFIG FILE HAD ERRORS",
             "Line 2: Value 'DE00/IRQ' is not a valid choice for item ACIA (6551) Mapping\n"
             "Line 3: Value 'Turbo232' is not a valid choice for item Hardware Mode\n");
   cfg("[Modem Settings]\nACIA (6551) Mapping=Off\n");
   check(settings[OPTM_G_MODEM][0], "the modem is off");

   // The REU's file: a path on the card, or none
   int named = crt_named_calls;
   cfg("[C64 and Cartridge Settings]\nREU Preload Image=/SD/c128/game.reu\n");
   want_data("LOAD_CONFIG: REU Preload Image", "00,OK", "");
   check(!strcmp(reu_file(), "/c128/game.reu") && !strcmp(crt_named, "/c128/game.reu") &&
         crt_named_calls == named + 1, "the REU's file, and its name in the menu");
   cfg("[C64 and Cartridge Settings]\nREU Preload Image=/Usb0/preload.reu\n");
   want_data("LOAD_CONFIG: an image off the card", "89,CONFIG FILE HAD ERRORS",
             "Line 2: Value '/Usb0/preload.reu' is not a valid choice for item REU Preload Image\n");
   check(!strcmp(reu_file(), "/c128/game.reu"), "the file stays");
   cfg("[C64 and Cartridge Settings]\nREU Preload Image=\n");
   check(!reu_file_picked() && !strcmp(crt_named, "-"), "an empty one: none picked");

   // The command interface itself: off last
   cfg("[C64 and Cartridge Settings]\nCommand Interface=Disabled\n");
   want("LOAD_CONFIG: UCI off", "00,OK");
   check(!settings[OPTM_G_UCI][0], "UCI is off");
   settings[OPTM_G_UCI][0] = 1;

   // His reply holds 511 bytes of the log
   strcpy(big, "[C64 and Cartridge Settings]\n");
   for (int i = 0; i < 20; i++)
      strcat(big, "An item this machine does not have=1\n");
   cfg(big);
   const char *first = "Line 2: Item 'An item this machine does not have' not found";
   check(rlen == 511 && !memcmp(rdata, first, strlen(first)), "a long log: 511 bytes of it");

   memset(settings[OPTM_G_REU], 0, sizeof settings[OPTM_G_REU]);
   settings[OPTM_G_REU][0] = 1;
   reu_sync();
   f_unlink("/CFG.CFG");
}

// ------------------------------------------------------------------------------------------
// The REU's memory and its file: the control target's LOAD_REU and SAVE_REU, and the DOS
// targets'
// ------------------------------------------------------------------------------------------

static uint8_t reu_pat(uint32_t i, int pass)
{
   return (uint8_t)(i * 7 + (i >> 9) + pass * 101);
}

// The control target's reply: the result as 4 bytes, then the message
static void want_reu(const char *what, const char *st, int32_t result, const char *text)
{
   char msg[600];
   int32_t got = rlen >= 4 ? (int32_t)(rdata[0] | rdata[1] << 8 | rdata[2] << 16 | (uint32_t)rdata[3] << 24) : 0;
   want(what, st);
   snprintf(msg, sizeof msg, "%s: %d and \"%.*s\", want %d and \"%s\"", what, got, rlen > 4 ? rlen - 4 : 0,
            rdata + 4, result, text);
   check(rlen == 4 + (int)strlen(text) && got == result && !memcmp(rdata + 4, text, rlen - 4), msg);
}

// A file on the card against the pattern
static int file_is(const char *path, uint32_t size, int pass)
{
   static uint8_t b[4096];
   FIL f;
   UINT got;
   int ok = f_open(&f, path, FA_READ) == FR_OK && f_size(&f) == size;
   for (uint32_t at = 0; ok && at < size; at += got) {
      ok = f_read(&f, b, sizeof b, &got) == FR_OK && got;
      for (UINT i = 0; ok && i < got; i++)
         ok = b[i] == reu_pat(at + i, pass);
   }
   f_close(&f);
   return ok;
}

static int mem_is(const uint8_t *m, uint32_t n, int pass)
{
   for (uint32_t i = 0; i < n; i++)
      if (m[i] != reu_pat(i, pass))
         return 0;
   return 1;
}

// dma: reu_sd.vhd is there (its model, host_reu_sd.c), else the CPU moves it all
static void reu_tests(int dma)
{
   static const uint8_t no_args[3];
   uint8_t *sdram = (uint8_t *)host_sr + 0x0800 * 8192, *hram = (uint8_t *)host_hr + 0x0080 * 8192;
   int polls;
   FIL f;
   UINT put;

   host_reu_sd(dma);
   um.reu_mem = 0x8800;

   // 512 KB in SDRAM, with no file picked: /c128/preload.reu, which isn't there yet
   settings[OPTM_G_REU][0] = 0;
   settings[OPTM_G_REU][1] = 1;
   f_unlink("/c128/preload.reu");
   f_unlink("/c128/preload.bu");
   dos(4, 0x08, no_args, 3, NULL);
   want_reu("LOAD_REU with no file", "85,REU FILE CANNOT BE OPENED", -1,
            "REU LOAD: FAILED TO OPEN /SD/C128/PRELOAD.REU: FILE DOESN'T EXIST");

   for (uint32_t i = 0; i < (512u << 10); i++)
      sdram[i] = reu_pat(i, 1);
   unsigned before = um.reboots;
   dos(4, 0x09, no_args, 3, NULL);
   want_reu("SAVE_REU, 512 KB", "00,OK", 524288, "REU SAVE: SAVED 524288 BYTES FROM $     0 TO /SD/C128/PRELOAD.REU");
   check(file_is("/c128/preload.reu", 512u << 10, 1), "SAVE_REU wrote the REU to /c128/preload.reu");
   check(um.reboots == before, "SAVE_REU leaves the C128 alone");

   memset(sdram, 0, 512u << 10);
   push((const uint8_t[]){ 4, 0x08, 0, 0, 0 }, 5);
   for (polls = 0; um.state == 1 && polls < 100000; polls++)
      uci_poll();
   check(polls > 10, "LOAD_REU of 512 KB takes many main-loop rounds");
   rlen = drain(rdata, rtext);
   c128_control(0x02);
   want_reu("LOAD_REU, 512 KB", "00,OK", 524288, "REU LOAD: LOADED 524288 BYTES TO $     0 FROM /SD/C128/PRELOAD.REU");
   check(mem_is(sdram, 512u << 10, 1), "LOAD_REU put the file back into the REU");

   // Again: the old file becomes preload.bu
   for (uint32_t i = 0; i < (512u << 10); i++)
      sdram[i] = reu_pat(i, 2);
   dos(4, 0x09, no_args, 3, NULL);
   want("SAVE_REU again", "00,OK");
   check(file_is("/c128/preload.reu", 512u << 10, 2) && file_is("/c128/preload.bu", 512u << 10, 1),
         "SAVE_REU keeps the old file as preload.bu");

   // A file picked in the menu, shorter than the REU: the rest of the REU stays
   check(f_open(&f, "/c128/short.reu", FA_WRITE | FA_CREATE_ALWAYS) == FR_OK, "make /c128/short.reu");
   for (uint32_t i = 0; i < 1000; i++) {
      uint8_t b = reu_pat(i, 3);
      f_write(&f, &b, 1, &put);
   }
   f_close(&f);
   reu_file_set("/c128/short.reu");
   memset(sdram, 0xAA, 2000);
   dos(4, 0x08, no_args, 3, NULL);
   want_reu("LOAD_REU of a short file", "00,OK", 1000, "REU LOAD: LOADED 1000 BYTES TO $     0 FROM /SD/C128/SHORT.REU");
   check(mem_is(sdram, 1000, 3) && sdram[1000] == 0xAA, "LOAD_REU loads what the file has");
   reu_file_set(NULL);

   // 2 MB
   settings[OPTM_G_REU][1] = 0;
   settings[OPTM_G_REU][2] = 1;
   for (uint32_t i = 0; i < (2u << 20); i++)
      sdram[i] = reu_pat(i, 4);
   dos(4, 0x09, no_args, 3, NULL);
   want_reu("SAVE_REU, 2 MB", "00,OK", 2097152, "REU SAVE: SAVED 2097152 BYTES FROM $     0 TO /SD/C128/PRELOAD.REU");
   check(file_is("/c128/preload.reu", 2u << 20, 4), "SAVE_REU wrote 2 MB");

   // R3 memory: the REU in HyperRAM at window 0x080, 512 KB at most
   um.reu_mem = 0x0080;
   settings[OPTM_G_REU][2] = 0;
   settings[OPTM_G_REU][1] = 1;
   for (uint32_t i = 0; i < (512u << 10); i++)
      hram[i] = reu_pat(i, 5);
   dos(4, 0x09, no_args, 3, NULL);
   want("SAVE_REU from HyperRAM", "00,OK");
   check(file_is("/c128/preload.reu", 512u << 10, 5), "SAVE_REU on R3 memory reads HyperRAM's window 0x080");
   memset(hram, 0, 512u << 10);
   dos(4, 0x08, no_args, 3, NULL);
   want("LOAD_REU into HyperRAM", "00,OK");
   check(mem_is(hram, 512u << 10, 5), "LOAD_REU on R3 memory writes HyperRAM's window 0x080");

   // DOS: the open file at its position, at an address; cut off at the end of the memory
   dos(1, 0x03, NULL, 0, NULL);
   dos(1, 0x21, "\0\1\0\0\0\x10\0\0", 8, NULL);
   want_data("DOS LOAD_REU with no file open", "85,NO FILE OPEN", "");
   dos(1, 0x02, "\x01", 1, "/SD/c128/preload.reu");
   memset(hram, 0, 0x2000);
   dos(1, 0x21, "\0\1\0\0\0\x10\0\0", 8, NULL);
   want_data("DOS LOAD_REU of $1000 bytes to $100", "00,OK", "$  1000 BYTES LOADED TO REU $   100");
   check(mem_is(hram + 0x100, 0x1000, 5) && hram[0xFF] == 0 && hram[0x1100] == 0, "DOS LOAD_REU put them there");
   dos(1, 0x21, "\0\0\0\0\x10\0\0\0", 8, NULL);
   check(hram[0] == reu_pat(0x1000, 5) && hram[15] == reu_pat(0x100F, 5), "the next DOS LOAD_REU goes on in the file");
   dos(1, 0x21, "\xF0\xFF\x07\0\x20\0\0\0", 8, NULL);
   want_data("DOS LOAD_REU past 512 KB", "02,REQUEST TRUNCATED", "$    10 BYTES LOADED TO REU $ 7fff0");
   dos(1, 0x03, NULL, 0, NULL);
   dos(1, 0x02, "\x0A", 1, "/SD/c128/dos.reu");
   dos(1, 0x22, "\0\0\0\0\0\0\x01\0", 8, NULL);
   want_data("DOS SAVE_REU of 64 KB", "00,OK", "$ 10000 BYTES SAVED FROM REU $     0");
   dos(1, 0x03, NULL, 0, NULL);
   check(f_open(&f, "/c128/dos.reu", FA_READ) == FR_OK && f_size(&f) == 0x10000, "DOS SAVE_REU wrote 64 KB");
   f_close(&f);
   um.reu_mem = 0x8800;
   dos(1, 0x02, "\x01", 1, "/SD/c128/dos.reu");
   dos(1, 0x21, "\xF0\xFF\xFF\0\x20\0\0\0", 8, NULL);
   want_data("DOS LOAD_REU past 16 MB", "02,REQUEST TRUNCATED", "$    10 BYTES LOADED TO REU $fffff0");
   dos(1, 0x03, NULL, 0, NULL);

   if (dma) {
      char msg[200];
      snprintf(msg, sizeof msg, "the DMA moved the files: %u loads, %u saves, %u blocks", rs.loads, rs.saves,
               rs.blocks);
      check(rs.loads >= 8 && rs.saves >= 8 && rs.blocks > 5000, msg);
      check(rs.drops >= rs.loads, "every DMA load dropped the REU's cached line");

      // A file in more pieces than the DMA's map holds (reu.c's DMA_RUNS, 256): by the CPU, and
      // its bytes too drop the line; with the REU at 16 MB, so that it all fits
      FATFS *fs;
      DWORD free_cl;
      FIL g;
      static uint8_t cl[64 * 512];
      check(f_getfree("", &free_cl, &fs) == FR_OK && fs->csize <= 64, "the image's clusters");
      UINT csz = fs->csize * 512;
      check(f_open(&f, "/c128/pieces.reu", FA_WRITE | FA_CREATE_ALWAYS) == FR_OK &&
            f_open(&g, "/c128/gaps.bin", FA_WRITE | FA_CREATE_ALWAYS) == FR_OK, "make two files");
      for (UINT k = 0; k < 264; k++) {                 // a cluster of each in turn
         for (UINT i = 0; i < csz; i++)
            cl[i] = reu_pat(k * csz + i, 6);
         f_write(&f, cl, csz, &put);
         f_sync(&f);
         f_write(&g, cl, csz, &put);
         f_sync(&g);
      }
      f_close(&f);
      f_close(&g);
      f_unlink("/c128/gaps.bin");
      reu_file_set("/c128/pieces.reu");
      settings[OPTM_G_REU][1] = 0;
      settings[OPTM_G_REU][3] = 1;
      memset(sdram, 0, 264 * csz);
      unsigned loads = rs.loads, drops = rs.drops;
      dos(4, 0x08, no_args, 3, NULL);
      want("LOAD_REU of a file in 264 pieces", "00,OK");
      check(mem_is(sdram, 264 * csz, 6), "a file in 264 pieces loads by the CPU");
      check(rs.loads == loads && rs.drops > drops, "  without the DMA, and the CPU's writes drop the line");
      settings[OPTM_G_REU][3] = 0;
      settings[OPTM_G_REU][1] = 1;
      reu_file_set(NULL);
      f_unlink("/c128/pieces.reu");

      // FatFs's window holds a block of the file: a DMA save over it, then a read; a write
      // left in it, then a DMA load
      static uint8_t w[2 + 512];
      memset(w, 0x11, sizeof w);
      dos(1, 0x02, "\x0B", 1, "/SD/c128/win.bin");
      dos(1, 0x05, w, sizeof w, NULL);
      dos(1, 0x05, w, sizeof w, NULL);
      dos(1, 0x06, "\0\0\0\0", 4, NULL);
      dos(1, 0x04, "\x10\x00", 2, NULL);
      check(rlen == 16 && rdata[0] == 0x11, "win.bin's first block in FatFs's window");
      for (uint32_t i = 0; i < 1024; i++)
         sdram[i] = reu_pat(i, 7);
      dos(1, 0x06, "\0\0\0\0", 4, NULL);
      unsigned saves = rs.saves;
      dos(1, 0x22, "\0\0\0\0\0\x04\0\0", 8, NULL);
      want_data("DOS SAVE_REU over the window's block", "00,OK", "$   400 BYTES SAVED FROM REU $     0");
      check(rs.saves == saves + 1, "  by the DMA");
      dos(1, 0x06, "\0\0\0\0", 4, NULL);
      dos(1, 0x04, "\x10\x00", 2, NULL);
      check(rlen == 16 && rdata[0] == reu_pat(0, 7) && rdata[15] == reu_pat(15, 7),
            "  READ_DATA then sees the saved bytes, not the window's old copy");
      dos(1, 0x06, "\0\0\0\0", 4, NULL);
      memset(w, 0x22, 2 + 16);
      dos(1, 0x05, w, 2 + 16, NULL);
      dos(1, 0x06, "\0\0\0\0", 4, NULL);
      memset(sdram + 0x2000, 0, 1024);
      dos(1, 0x21, "\0\x20\0\0\0\x04\0\0", 8, NULL);
      want("DOS LOAD_REU after a WRITE_DATA", "00,OK");
      check(sdram[0x2000] == 0x22 && sdram[0x200F] == 0x22 && sdram[0x2010] == reu_pat(0x10, 7) &&
            sdram[0x23FF] == reu_pat(0x3FF, 7), "  the REU has the written bytes");
      dos(1, 0x03, NULL, 0, NULL);

      // An SD error part of the way
      rs.fail_at = 3;
      dos(4, 0x09, no_args, 3, NULL);
      want("SAVE_REU with an SD error in its third DMA step", "DISK ERROR");
      rs.fail_at = 2;
      dos(4, 0x08, no_args, 3, NULL);
      want("LOAD_REU with an SD error in its second DMA step", "DISK ERROR");
      rs.fail_at = 0;
   }

   host_reu_sd(0);
   settings[OPTM_G_REU][1] = 0;
   settings[OPTM_G_REU][0] = 1;
}

int main(int argc, char **argv)
{
   static uint8_t big[900], data[8192];
   char text[300];

   if (argc != 2 || !disk_file_open(argv[1])) {
      printf("usage: test_uci fat32.img\n");
      return 2;
   }

   host_init();
   uci_model_init();
   poll(1);

   // run_transport
   expect("an empty command", NULL, 0, "", "", 0);
   expect("unregistered target answers IDENTIFY", (uint8_t[]){ 0x0F, 0x01 }, 2, "00,OK",
          "NO TARGET", 9);
   expect("unregistered target rejects anything else", (uint8_t[]){ 0x0F, 0x7F }, 2,
          "21,UNKNOWN COMMAND", "", 0);

   push((uint8_t[]){ 0x84, 0x01 }, 2);
   poll(3);
   check(uci_status() == 0, "a no-reply command returns straight to Idle");

   push((uint8_t[]){ 0x01, 0x01 }, 2);
   poll(3);
   c128_control(0x01);
   check(uci_status() & 0x08, "pushing while a reply is pending sets ERROR");
   c128_control(0x08);
   check(!(uci_status() & 0x08), "CLR_ERR clears it");
   int n = drain(data, text);
   c128_control(0x02);
   check(n == 16 && !memcmp(data, "MEGA128 DOS V1.0", 16) && !strcmp(text, "00,OK"),
         "the pending reply survives the rejected push");
   check(uci_status() == 0, "idle after recovering from a state error");

   // The command queue starts afresh: the echo is this command alone
   expect("the next command is not prefixed by leftover bytes", (uint8_t[]){ 0x01, 0xF0, 'A' }, 3,
          "00,OK", "\x01\xF0" "A", 3);

   push((uint8_t[]){ 0x01, 0x01 }, 2);
   poll(3);
   c128_control(0x04);
   check(!(uci_status() & 0xC0), "ABORT hides the pending reply");
   poll(1);
   check(uci_status() == 0, "ABORT releases a pending reply back to Idle");

   // Ultimate DOS
   expect("DOS 1 IDENTIFY", (uint8_t[]){ 0x01, 0x01 }, 2, "00,OK", "MEGA128 DOS V1.0", 16);
   expect("DOS 2 IDENTIFY", (uint8_t[]){ 0x02, 0x01 }, 2, "00,OK", "MEGA128 DOS V1.0", 16);
   expect("DOS unknown command", (uint8_t[]){ 0x01, 0x7F }, 2, "21,UNKNOWN COMMAND", "", 0);
   expect("network target not there yet", (uint8_t[]){ 0x03, 0x01 }, 2, "00,OK", "NO TARGET", 9);
   big[0] = 0x01;
   big[1] = 0xF0;
   for (int i = 2; i < 900; i++)
      big[i] = (uint8_t)(i * 7);
   expect("ECHO of 895 bytes", big, 895, "00,OK", big, 895);
   expect("ECHO of 900 bytes stops at 895", big, 900, "00,OK", big, 895);

   // A reset of the C128 while a reply waits: idle again, flag cleared
   push((uint8_t[]){ 0x01, 0x01 }, 2);
   poll(3);
   um.c128_reset = 1;
   poll(1);
   check(uci_status() == 0 && um.c128_reset == 0, "a C128 reset ends the reply and clears the flag");
   expect("and the next command works", (uint8_t[]){ 0x0F, 0x01 }, 2, "00,OK", "NO TARGET", 9);

   dos_tests();
   ctrl_tests();
   cfg_tests();
   reu_tests(0);
   reu_tests(1);

   printf("test_uci: %d of %d checks passed\n", runs - fails, runs);
   return fails != 0;
}
