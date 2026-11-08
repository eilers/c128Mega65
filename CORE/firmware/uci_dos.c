// Ultimate Command Interface: the DOS targets 1 and 2, C128MEGA65 DOS, with the commands,
// replies and status texts of Gideon's Ultimate DOS (filemanager/dos.cc): files and folders
// on the SD card, disk images in drives 8 and 9, the clock. Paths look like an Ultimate's:
// '/' holds one folder, SD, which is the active card. Each target has its own folder, open
// file and open folder.
// Ported from the 1541 Ultimate firmware (github.com/GideonZ/1541ultimate), Copyright
// Gideon Zweijtzer and contributors, GPL v3 as this core; changed for c128Mega65 in 2026.
#include <stddef.h>
#include <string.h>
#include <strings.h>
#include "core.h"
#include "drives.h"
#include "ff.h"
#include "fmt.h"
#include "heap.h"
#include "m2m_rv.h"
#include "reu.h"
#include "sdcard.h"
#include "stores.h"
#include "uci.h"

#define CMD_IDENTIFY       0x01
#define CMD_OPEN_FILE      0x02
#define CMD_CLOSE_FILE     0x03
#define CMD_READ_DATA      0x04
#define CMD_WRITE_DATA     0x05
#define CMD_FILE_SEEK      0x06
#define CMD_FILE_INFO      0x07
#define CMD_FILE_STAT      0x08
#define CMD_DELETE_FILE    0x09
#define CMD_RENAME_FILE    0x0A
#define CMD_COPY_FILE      0x0B
#define CMD_CHANGE_DIR     0x11
#define CMD_GET_PATH       0x12
#define CMD_OPEN_DIR       0x13
#define CMD_READ_DIR       0x14
#define CMD_COPY_UI_PATH   0x15
#define CMD_CREATE_DIR     0x16
#define CMD_COPY_HOME_PATH 0x17
#define CMD_LOAD_REU       0x21
#define CMD_SAVE_REU       0x22
#define CMD_MOUNT_DISK     0x23
#define CMD_UMOUNT_DISK    0x24
#define CMD_SWAP_DISK      0x25
#define CMD_GET_TIME       0x26
#define CMD_SET_TIME       0x27
#define CMD_ECHO           0xF0

#define PATH_MAX   UCI_PATH_MAX
#define BLOCK      512             // READ_DATA's part of a reply, and COPY_FILE's step
#define COPY_STEPS 8               // blocks a main loop round copies
// MOUNT_DISK answers once the drive has the disk, as an Ultimate's does. A core 1541/1571
// acts out a disk change after a mount (c157x_drv.sv), with drives_ready() low until it is
// over, so the reply waits to see it low, then high. The limits only catch a broken drive:
#define TAKE_LIMIT   (CPU_HZ / 10)     // low within 100 ms, or the drive didn't take it
#define CHANGE_LIMIT (CPU_HZ * 10)     // high again within 10 s (it takes 2.1)

// The clock (M2M's rtc_controller.vhd): BCD time, then its command register
#define RTC_SEC    1
#define RTC_DOW    7               // 0 is Monday
#define RTC_CMD    8
#define RTC_BUSY   0x01
#define RTC_TO_CHIP 0x0C           // copy into the chip, then run again

static const uint8_t identification[] = "C128MEGA65 DOS V1.0";
static const char ok[]           = "00,OK";
static const char dir_empty[]    = "01,DIRECTORY EMPTY";
static const char truncated[]    = "02,REQUEST TRUNCATED";
static const char unknown[]      = "21,UNKNOWN COMMAND";
static const char not_found[]    = "82,FILE NOT FOUND";
static const char no_such_dir[]  = "83,NO SUCH DIRECTORY";
static const char nothing_open[] = "84,NO FILE TO CLOSE";
static const char no_file[]      = "85,NO FILE OPEN";
static const char cant_read[]    = "86,CAN'T READ DIRECTORY";
static const char internal[]     = "87,INTERNAL ERROR";
static const char not_image[]    = "89,NOT A DISK IMAGE";
static const char no_drive[]     = "90,DRIVE NOT PRESENT";
static const char incompatible[] = "91,INCOMPATIBLE IMAGE";
static const char prohibited[]   = "98,FUNCTION PROHIBITED";
static const char not_done[]     = "99,FUNCTION NOT IMPLEMENTED";

enum { IDLE, IN_FILE, IN_DIR };

struct dos {
   char    *cwd;                   // the folder, ending in '/' (HyperRAM)
   char    *fpath;                 // the open file's path on the card, for FILE_INFO
   FIL      f;
   int      fopen;
   DIR      d;
   int      dopen;                 // OPEN_DIR opened d (or the root, dopen 2)
   int      entries, index;        // what OPEN_DIR counted, how far READ_DIR is
   int      state;
   uint32_t remaining;             // READ_DATA's bytes still to come
};

static struct dos dos[2];
static char       path[PATH_MAX], path2[PATH_MAX];
static FILINFO    fi;              // one for all: the main loop never calls back in
static int        last_drive;      // the drive last mounted, for drive id 0

// A copy (COPY_FILE) under way, for the targets' work(); one at a time, as commands are
static struct {
   FIL      src, dst;
   int      on;
   uint32_t left;
} copy;
// LOAD_REU or SAVE_REU under way, for the targets' work(); its status once it is done
static struct {
   reu_xfer_t  x;
   int         on;
   uint32_t    addr;
   const char *status;
} reu_job;
static enum { M_NONE, M_TAKE, M_CHANGE } mount_wait;   // MOUNT_DISK's reply waits for the drive
static uint32_t mount_at;

static uint32_t le32(const uint8_t *p)
{
   return p[0] | p[1] << 8 | p[2] << 16 | (uint32_t)p[3] << 24;
}

// -----------------------------------------------------------------------------------------
// Paths (dos_path.c)
// -----------------------------------------------------------------------------------------

// A name from a command, on the card: its FatFs path, or NULL
static const char *card_name(struct dos *t, const char *name, char *out)
{
   return uci_resolve(t->cwd, name, out) ? uci_on_card(out) : NULL;
}

static int is_folder(const char *p)
{
   const char *c = uci_on_card(p);
   if (!*p)
      return 1;                                        // the root
   if (!c)
      return 0;
   if (!strcmp(c, "/"))
      return sdcard_state() == SDCARD_OK;
   return f_stat(c, &fi) == FR_OK && (fi.fattrib & AM_DIR);
}

// -----------------------------------------------------------------------------------------
// The commands
// -----------------------------------------------------------------------------------------

static void close_all(struct dos *t)
{
   if (t->fopen)
      f_close(&t->f);
   if (t->dopen == 1)
      f_closedir(&t->d);
   t->fopen = t->dopen = 0;
   t->entries = 0;
   t->state = IDLE;
}

// FILE_INFO and FILE_STAT: size, date, time, extension, attributes, name (t_dos_info)
static void info(uint32_t size, uci_reply_t *r)
{
   const char *dot = strrchr(fi.fname, '.');
   int n = strlen(fi.fname);
   if (n > 63)
      n = 63;
   uci_data[0] = size;
   uci_data[1] = size >> 8;
   uci_data[2] = size >> 16;
   uci_data[3] = size >> 24;
   uci_data[4] = fi.fdate;
   uci_data[5] = fi.fdate >> 8;
   uci_data[6] = fi.ftime;
   uci_data[7] = fi.ftime >> 8;
   memset(uci_data + 8, 0, 3);                          // strncpy's padding
   if (dot)
      strncpy((char *)uci_data + 8, dot + 1, 3);
   uci_data[11] = fi.fattrib;
   memcpy(uci_data + 12, fi.fname, n);
   r->data = uci_data;
   r->len  = 12 + n;
}

static void cd(struct dos *t, const char *name, uci_reply_t *r)
{
   if (!uci_resolve(t->cwd, name, path) || !is_folder(path)) {
      r->status = no_such_dir;
      return;
   }
   if (t->dopen == 1)
      f_closedir(&t->d);
   t->dopen = 0;
   t->entries = 0;
   strcpy(t->cwd, path);
   strcat(t->cwd, "/");
}

static void open_dir(struct dos *t, uci_reply_t *r)
{
   if (t->dopen == 1)
      f_closedir(&t->d);
   t->dopen = 0;
   t->entries = 0;
   uci_resolve(t->cwd, "", path);
   const char *c = uci_on_card(path);
   if (!*path) {
      t->dopen = 2;
      t->entries = sdcard_state() == SDCARD_OK;
   } else if (!c || f_opendir(&t->d, c) != FR_OK) {
      r->status = cant_read;
      return;
   } else {
      t->dopen = 1;
      while (f_readdir(&t->d, &fi) == FR_OK && fi.fname[0])
         t->entries++;
   }
   if (!t->entries)
      r->status = dir_empty;
}

// READ_DIR's entries, one a block: attributes, then the name
static void next_entry(struct dos *t, uci_reply_t *r)
{
   if (t->index >= t->entries ||
       (t->dopen == 1 && (f_readdir(&t->d, &fi) != FR_OK || !fi.fname[0]))) {
      r->status = internal;
      t->state = IDLE;
      return;
   }
   if (t->dopen == 2) {
      fi.fattrib = AM_DIR;
      strcpy(fi.fname, "SD");
   }
   uci_data[0] = fi.fattrib;
   strcpy((char *)uci_data + 1, fi.fname);
   r->data = uci_data;
   r->len  = 1 + strlen(fi.fname);
   if (++t->index == t->entries) {
      r->status = ok;
      t->state = IDLE;
   } else {
      r->status = "";
      r->more = 1;
   }
}

// READ_DATA's blocks of up to 512 bytes
static void next_block(struct dos *t, uci_reply_t *r)
{
   UINT got = 0;
   uint32_t n = t->remaining < BLOCK ? t->remaining : BLOCK;
   FRESULT res = f_read(&t->f, uci_data, n, &got);
   t->remaining -= got;
   r->data   = uci_data;
   r->len    = got;
   r->status = res == FR_OK ? "" : uci_fr_text(res);
   if (res != FR_OK || got != n || !t->remaining)
      t->state = IDLE;
   else
      r->more = 1;
}

// COPY_FILE: the file, named from the folder, into a folder named from the root (as
// FileManager::fcopy takes it), under the same name. Folders are not copied.
static void copy_start(struct dos *t, const char *name, const char *dest, uci_reply_t *r)
{
   const char *src = card_name(t, name, path);
   const char *base = src ? strrchr(path, '/') : NULL;
   if (!src || !base[1] || !uci_resolve("/", dest, path2) || !uci_on_card(path2) ||
       strlen(path2) + strlen(base) >= PATH_MAX) {
      r->status = uci_fr_text(FR_NO_PATH);
      return;
   }
   strcat(path2, base);
   if (!strcasecmp(path, path2))
      return;                                          // onto itself (names ignore case): done
   FRESULT res = f_open(&copy.src, src, FA_READ);
   if (res == FR_OK && (res = f_open(&copy.dst, uci_on_card(path2), FA_WRITE | FA_CREATE_NEW)) != FR_OK)
      f_close(&copy.src);
   if (res != FR_OK) {
      r->status = uci_fr_text(res);
      return;
   }
   copy.on   = 1;
   copy.left = f_size(&copy.src);
   r->status = NULL;                                   // work() goes on with it
}

static void copy_end(int keep)
{
   f_close(&copy.src);
   f_close(&copy.dst);
   if (!keep)
      f_unlink(uci_on_card(path2));
   copy.on = 0;
}

// LOAD_REU, SAVE_REU (dos.cc): from the open file, at its position, into the REU's memory at
// an address, or from there into the file, one reu_step a main-loop round. The address (24
// bits) and length (25 bits) as his, 4 bytes each, little-endian; what would pass the end of
// the REU's memory (16 MB, 512 KB on R3 memory) is cut off, as his at 16 MB. The REU needn't
// be on.
static void reu_start(struct dos *t, const uint8_t *cmd, int len, uci_reply_t *r)
{
   uint32_t arg[2] = { 0, 0 }, room = reu_room();
   if (!t->fopen) {
      r->status = no_file;
      return;
   }
   for (int i = 0; i < 8 && 2 + i < len; i++)
      arg[i / 4] |= (uint32_t)cmd[2 + i] << (8 * (i % 4));
   uint32_t addr = arg[0] & 0xFFFFFF, n = arg[1] & 0x1FFFFFF;
   if (addr + n > room) {
      n = addr < room ? room - addr : 0;
      r->status = truncated;
   }
   reu_job.x      = (reu_xfer_t){ .f = &t->f, .save = cmd[1] == CMD_SAVE_REU, .at = addr, .left = n };
   reu_job.addr   = addr;
   reu_job.status = r->status;
   reu_job.on     = 1;
   r->status      = NULL;                              // work() goes on with it
}

// His reply: the same length every time, so a 16 MB one loses its last character
static void reu_done(FRESULT res, uci_reply_t *r)
{
   reu_job.on = 0;
   fmt_snprintf((char *)uci_data, UCI_MAX_REPLY, reu_job.x.save ? "$%6x BYTES SAVED FROM REU $%6x" :
                "$%6x BYTES LOADED TO REU $%6x", reu_job.x.done, reu_job.addr);
   r->data   = uci_data;
   r->len    = reu_job.x.save ? 36 : 35;
   r->status = res == FR_OK ? reu_job.status : uci_fr_text(res);
}

static void dos_work(uci_reply_t *r)
{
   if (reu_job.on) {
      FRESULT res = reu_step(&reu_job.x, COPY_STEPS * BLOCK);
      if (res != FR_OK || !reu_job.x.left)
         reu_done(res, r);
      return;
   }
   if (mount_wait) {
      uint32_t t = MTIME_LO - mount_at;
      int ready = drives_ready();
      if (mount_wait == M_TAKE && !ready)
         mount_wait = M_CHANGE;
      else if (mount_wait == M_CHANGE && ready)
         r->status = ok;
      if (!r->status && t > (mount_wait == M_TAKE ? TAKE_LIMIT : CHANGE_LIMIT))
         r->status = internal;
      if (r->status)
         mount_wait = M_NONE;
      return;
   }
   const char *err = copy.on ? NULL : internal;
   for (int i = 0; i < COPY_STEPS && copy.left && !err; i++) {
      UINT got, put = 0;
      uint32_t n = copy.left < BLOCK ? copy.left : BLOCK;
      FRESULT res = f_read(&copy.src, uci_data, n, &got);
      if (res == FR_OK && got != n)
         res = FR_INT_ERR;
      if (res == FR_OK)
         res = f_write(&copy.dst, uci_data, n, &put);
      if (res != FR_OK)
         err = uci_fr_text(res);
      else if (put != n)
         err = "DISK IS FULL";
      copy.left -= n;
   }
   if (err) {
      if (copy.on)
         copy_end(0);
      r->status = err;
   } else if (!copy.left) {
      copy_end(1);
      r->status = ok;
   }
}

// MOUNT_DISK, UMOUNT_DISK: the drive with that bus ID (his getDriveByID), or 0 for the one
// last mounted
static int drive_of(int id)
{
   int d = id == 0 ? last_drive : stores_drive_of_id(id);
   if (d < 0 || d >= drives_count() || d > 1 || stores_drive_internal(d))
      return -1;                                       // the internal floppy is no image drive
   return d;
}

static void mount(struct dos *t, int id, const char *name, uci_reply_t *r)
{
   const char *msg, *c;
   int d = drive_of(id);
   if (d < 0) {
      r->status = no_drive;
      return;
   }
   if (!(c = card_name(t, name, path)) || f_stat(c, &fi) != FR_OK || (fi.fattrib & AM_DIR)) {
      r->status = not_found;
      return;
   }
   const char *dot = strrchr(fi.fname, '.');
   char ext[4] = "";
   for (int i = 0; dot && i < 3 && dot[1 + i]; i++)
      ext[i] = dot[1 + i] | 0x20, ext[i + 1] = 0;
   if (!strcmp(ext, "g64") || !strcmp(ext, "g71")) {
      r->status = incompatible;                        // the drives take sector images only
      return;
   }
   if (strcmp(ext, "d64") && strcmp(ext, "d71") && strcmp(ext, "d81")) {
      r->status = not_image;
      return;
   }
   int err = shell_mount(d, c, &msg);
   if (err == 0xEE42)
      r->status = prohibited;                          // the image is in the other drive
   else if (err)
      r->status = incompatible;
   else {
      last_drive = d;
      if (strcmp(ext, "d81")) {                        // a 1581 flags the change at once
         mount_wait = M_TAKE;
         mount_at   = MTIME_LO;
         r->status  = NULL;                            // work() answers once it is in
      }
   }
}

// -----------------------------------------------------------------------------------------
// The clock
// -----------------------------------------------------------------------------------------

static int bcd(int v)
{
   return (v >> 4) * 10 + (v & 15);
}

static void time_text(int ext, uci_reply_t *r)
{
   static const char *const days[7] = { "SUN", "MON", "TUE", "WED", "THU", "FRI", "SAT" };
   int t[8];
   M2M_RAMROM_DEV = M2M_DEV_RTC;
   for (int i = RTC_SEC; i <= RTC_DOW; i++)
      t[i] = M2M_DEV16[i] & 0xFF;
   int n = 0;
   if (ext)
      n = fmt_snprintf((char *)uci_data, UCI_MAX_REPLY, "%s ", days[(t[RTC_DOW] + 1) % 7]);
   n += fmt_snprintf((char *)uci_data + n, UCI_MAX_REPLY - n, "%4d/%02d/%02d %02d:%02d:%02d",
                     2000 + bcd(t[6]), bcd(t[5]), bcd(t[4]), bcd(t[3]), bcd(t[2]), bcd(t[1]));
   r->data = uci_data;
   r->len  = n;
}

int uci_year(void)
{
   M2M_RAMROM_DEV = M2M_DEV_RTC;
   return 2000 + bcd(M2M_DEV16[6] & 0xFF);
}

static int rtc_wait(void)
{
   uint32_t start = MTIME_LO;
   while (M2M_DEV16[RTC_CMD] & (RTC_BUSY | 0x06))
      if (MTIME_LO - start > CPU_HZ / 50)              // the I2C write takes about 1 ms
         return 0;
   return 1;
}

static uint16_t to_bcd(int v)
{
   return (v / 10) << 4 | v % 10;
}

// SET_TIME: years since 1900, month, day, hours, minutes, seconds; into the RTC chip
static void set_time(const uint8_t *c, uci_reply_t *r)
{
   int y = c[0] + 1900, m = c[1], d = c[2];
   if (y < 2000 || y > 2099 || m < 1 || m > 12 || d < 1 || d > 31 || c[3] > 23 || c[4] > 59 ||
       c[5] > 59) {
      r->status = prohibited;
      return;
   }
   // Zeller, as dos.cc: 0 is Sunday
   int yz = y, mz = m;
   if (mz < 3) {
      yz--;
      mz += 12;
   }
   int wd = (d + (mz + 1) * 13 / 5 + yz % 100 + yz % 100 / 4 + yz / 400 - 2 * (yz / 100) + 6) % 7;
   if (wd < 0)
      wd += 7;

   M2M_RAMROM_DEV = M2M_DEV_RTC;
   M2M_DEV16[RTC_CMD] = 0;                             // stop the internal timer
   if (!rtc_wait()) {
      r->status = internal;
      return;
   }
   M2M_DEV16[0] = 0;
   for (int i = 0; i < 5; i++)
      M2M_DEV16[RTC_SEC + i] = to_bcd(c[5 - i]);
   M2M_DEV16[6] = to_bcd(y - 2000);
   M2M_DEV16[RTC_DOW] = (wd + 6) % 7;
   M2M_DEV16[RTC_CMD] = RTC_TO_CHIP;
   if (!rtc_wait()) {
      r->status = internal;
      return;
   }
   time_text(1, r);
}

// -----------------------------------------------------------------------------------------

static void dos_command(struct dos *t, uint8_t *cmd, int len, uci_reply_t *r)
{
   UINT done;
   FRESULT res;
   const char *c, *c2;
   char *name = (char *)cmd + 2;

   if (!t->cwd) {
      t->cwd   = heap_alloc(PATH_MAX);
      t->fpath = heap_alloc(PATH_MAX);
      strcpy(t->cwd, "/");
   }
   r->status = ok;
   switch (len > 1 ? cmd[1] : -1) {
   case CMD_IDENTIFY:
      r->data = identification;
      r->len  = sizeof identification - 1;
      break;
   case CMD_OPEN_FILE:
      if (t->fopen)
         f_close(&t->f);
      t->fopen = 0;
      t->state = IDLE;
      if (len < 3 || !(c = card_name(t, name + 1, path)))
         res = FR_NO_PATH;
      else if ((res = f_open(&t->f, c, cmd[2] & 0x3F)) == FR_OK) {
         t->fopen = 1;
         strcpy(t->fpath, c);
      }
      if (res != FR_OK)
         r->status = uci_fr_text(res);
      break;
   case CMD_CLOSE_FILE:
      if (!t->fopen)
         r->status = nothing_open;
      else if ((res = f_close(&t->f)) != FR_OK)
         r->status = uci_fr_text(res);
      t->fopen = 0;
      t->state = IDLE;
      break;
   case CMD_FILE_SEEK:
      if (!t->fopen)
         r->status = no_file;
      else if (len < 6)
         r->status = unknown;
      else if ((res = f_lseek(&t->f, le32(cmd + 2))) != FR_OK)
         r->status = uci_fr_text(res);
      break;
   case CMD_FILE_INFO:
      if (!t->fopen)
         r->status = no_file;
      else if (f_stat(t->fpath, &fi) != FR_OK)
         r->status = not_found;
      else
         info(f_size(&t->f), r);
      break;
   case CMD_FILE_STAT:
      if (!(c = card_name(t, name, path)) || f_stat(c, &fi) != FR_OK)
         r->status = not_found;
      else
         info(fi.fsize, r);
      break;
   case CMD_DELETE_FILE:
      res = (c = card_name(t, name, path)) ? f_unlink(c) : FR_NO_PATH;
      if (res != FR_OK)
         r->status = uci_fr_text(res);
      break;
   case CMD_RENAME_FILE:
      c  = card_name(t, name, path);
      c2 = c ? card_name(t, name + strlen(name) + 1, path2) : NULL;
      res = c2 ? f_rename(c, c2) : FR_NO_PATH;
      if (res != FR_OK)
         r->status = uci_fr_text(res);
      break;
   case CMD_COPY_FILE:
      copy_start(t, name, name + strlen(name) + 1, r);
      break;
   case CMD_CHANGE_DIR:
      cd(t, name, r);
      break;
   case CMD_COPY_HOME_PATH:
      cd(t, "/SD", r);
      r->status = ok;                                  // the reply is the folder
      // fall through
   case CMD_GET_PATH:
      strcpy((char *)uci_data, t->cwd);
      r->data = uci_data;
      r->len  = strlen(t->cwd);
      break;
   case CMD_OPEN_DIR:
      open_dir(t, r);
      break;
   case CMD_READ_DIR:
      t->index = 0;
      t->state = IN_DIR;
      if (t->dopen == 1)
         f_readdir(&t->d, NULL);                       // from the start
      next_entry(t, r);
      break;
   case CMD_CREATE_DIR:
      res = (c = card_name(t, name, path)) ? f_mkdir(c) : FR_NO_PATH;
      if (res != FR_OK)
         r->status = uci_fr_text(res);
      break;
   case CMD_READ_DATA:
      if (!t->fopen) {
         r->status = no_file;
         break;
      }
      t->remaining = len > 3 ? cmd[2] | cmd[3] << 8 : 0;
      t->state = IN_FILE;
      next_block(t, r);
      break;
   case CMD_WRITE_DATA:
      if (!t->fopen)
         r->status = no_file;
      else if (len > 4 && (res = f_write(&t->f, cmd + 4, len - 4, &done)) != FR_OK)
         r->status = uci_fr_text(res);
      else if (len > 4 && done != (UINT)(len - 4))
         r->status = "DISK IS FULL";
      break;
   case CMD_MOUNT_DISK:
      if (len < 4)
         r->status = not_found;
      else
         mount(t, cmd[2], name + 1, r);
      break;
   case CMD_UMOUNT_DISK: {
      int d = drive_of(len > 2 ? cmd[2] : 0);
      if (d < 0)
         r->status = no_drive;
      else
         shell_unmount(d);
      break;
   }
   case CMD_GET_TIME:
      if (len > 2 && cmd[2] > 1)
         r->status = unknown;
      else
         time_text(len > 2 && cmd[2] == 1, r);
      break;
   case CMD_SET_TIME:
      if (len != 8) {
         fmt_snprintf((char *)uci_data, 3, "%02d", len);
         r->data = uci_data;
         r->len  = 2;
         r->status = unknown;
      } else {
         set_time(cmd + 2, r);
      }
      break;
   case CMD_LOAD_REU:
   case CMD_SAVE_REU:
      reu_start(t, cmd, len, r);
      break;
   case CMD_COPY_UI_PATH:
   case CMD_SWAP_DISK:
      r->status = not_done;
      break;
   case CMD_ECHO:                                      // the whole command back
      r->data = cmd;
      r->len  = len;
      break;
   default:
      r->status = unknown;
   }
}

static void dos_more(struct dos *t, uci_reply_t *r)
{
   if (t->state == IN_FILE)
      next_block(t, r);
   else if (t->state == IN_DIR)
      next_entry(t, r);
   else
      r->status = "81,NOT IN DATA MODE";
}

static void dos_abort(struct dos *t)
{
   t->state = IDLE;
   mount_wait = M_NONE;
   reu_job.on = 0;
   if (copy.on)
      copy_end(0);
}

static void dos_reset(struct dos *t)
{
   dos_abort(t);
   close_all(t);                       // what was written goes to the card
}

static void command1(uint8_t *c, int len, uci_reply_t *r) { dos_command(&dos[0], c, len, r); }
static void command2(uint8_t *c, int len, uci_reply_t *r) { dos_command(&dos[1], c, len, r); }
static void more1(uci_reply_t *r)  { dos_more(&dos[0], r); }
static void more2(uci_reply_t *r)  { dos_more(&dos[1], r); }
static void abort1(void)           { dos_abort(&dos[0]); }
static void abort2(void)           { dos_abort(&dos[1]); }
static void reset1(void)           { dos_reset(&dos[0]); }
static void reset2(void)           { dos_reset(&dos[1]); }

const uci_target_t uci_dos1 = { command1, more1, dos_work, abort1, reset1 };
const uci_target_t uci_dos2 = { command2, more2, dos_work, abort2, reset2 };
