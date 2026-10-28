// The Ultimate Command Interface's firmware (CORE/firmware/uci*.c) against a model of
// Gideon's command_protocol.vhd, driven from the C128 side the way his
// tests/e2e/io/command_interface/uci_targets_test.py drives a real Ultimate (run_transport),
// with the replies his firmware gives. Then the DOS targets on a FAT32 image, with the
// replies and status texts of his filemanager/dos.cc.
// usage: test_uci fat32.img   (it writes to the image)
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ff.h"
#include "m2m_rv.h"
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
   FATFS fs;
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

   dos(1, 0x21, NULL, 0, NULL);
   want("LOAD_REU", "99,FUNCTION NOT IMPLEMENTED");
   dos(1, 0x15, NULL, 0, NULL);
   want("COPY_UI_PATH", "99,FUNCTION NOT IMPLEMENTED");
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

   printf("test_uci: %d of %d checks passed\n", runs - fails, runs);
   return fails != 0;
}
