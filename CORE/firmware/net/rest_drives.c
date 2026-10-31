// The REST API's drives routes: Gideon's software/api/route_drives.cc at 4b00732 and the
// C1541 commands they run (drive/c1541.cc), on the core's two drives: a is drive 8, b is
// drive 9, softiec isn't there (his "SubSystem does not exist"). Images are read straight
// from the card (drives.c), so:
//   - readonly write protects the disk (vdrives' VD_RO), as his;
//   - unlinked mounts a copy in /c128/temp: writes go to the copy, the file stays as it was;
//     :unlink copies the mounted image and moves the drive onto the copy, held meanwhile
//     (vd_sd's hold), without a disk change;
//   - G64/G71 aren't read by the core's drives: 501.
// load_rom writes the drive's bank of boot1.rom for its current model and reloads it into the
// drive (C_DEV_DRIVE_ROM's CSR), which resets it; it lasts until boot1.rom is loaded again
// (a long reset). Uploads to mount go to /c128/temp, as his to /Temp.
// Ported from the 1541 Ultimate firmware (github.com/GideonZ/1541ultimate), Copyright
// Gideon Zweijtzer and contributors, GPL v3 as this core; changed for c128Mega65 in 2026.
#include <string.h>
#include <strings.h>
#include "config_ids.h"
#include "core.h"
#include "drives.h"
#include "fmt.h"
#include "heap.h"
#include "m2m_rv.h"
#include "net.h"
#include "rest_priv.h"
#include "rompick.h"
#include "roms.h"
#include "stores.h"
#include "uci.h"

#define SUBSYS_A     2                 // his SUBSYSID_DRIVE_A, _B, SUBSYSID_IEC
#define SUBSYS_IEC   7
#define MOUNT_D64    0x1520            // his MENU_1541_MOUNT_D64, _G64 and the mode flags
#define MOUNT_G64    0x1530
#define READ_ONLY    0x8000
#define UNLINKED     0x4000

#define DRIVE_ROM    0x0102            // C_DEV_DRIVE_ROM: boot1.rom, its CSR register 0
#define BANK_SIZE    32768             // a drive's bank: 1541 a, b, 1571 a, b, 1581 a, b
#define TEMP_DIR     "/c128/temp"
#define COPY_STEP    4096              // a main-loop round's copy
#define TAKE_LIMIT   100               // ms: a 1541/1571 starts its disk change (uci_dos.c)
#define CHANGE_LIMIT 10000             // ms: and ends it
#define ROM_LIMIT    100               // ms: a bank takes 2.6 ms to load

static const char *const types[] = { "1541", "1571", "1581" };

// What GET drives shows of each drive's image: his get_last_mounted_file, path and name
// as the mount was given them (a REST mount: "" and the whole path), for the card file
// that was mounted then. Another mount (menu, UCI) shows its own folder and name.
typedef struct {
   char card[256];                     // the drive's drive_path() when path and name were set
   char path[260], name[260];
} shown_t;

static shown_t *img;                   // two, in HyperRAM

static void img_alloc(void)
{
   if (!img)
      img = heap_alloc(2 * sizeof *img);
}

static uint32_t temp_seq;

static void card_to_sd(const char *card, char *out, int size)
{
   fmt_snprintf(out, size, "/SD%s", card);
}

// --- the drive of a request ---------------------------------------------------------------

// His driveToSubsys: a or b, any case; softiec; else his 400
static int drive_of(rest_t *r, int *subsys)
{
   const char *name = r->depth ? r->parts[0] : NULL;
   if (name && !strcasecmp(name, "softiec")) {
      *subsys = SUBSYS_IEC;
      return -1;
   }
   if (name && (!strcasecmp(name, "a") || !strcasecmp(name, "b"))) {
      *subsys = SUBSYS_A + ((name[0] | 0x20) == 'b');
      return *subsys - SUBSYS_A;
   }
   error(r, "Invalid Drive '%s'", name ? name : "(null)");
   json_response(r, HTTP_BAD_REQUEST);
   *subsys = -1;
   return -1;
}

// The drive, or an answer: his 400, or no softiec here
static int drive_or_answer(rest_t *r)
{
   int subsys, d = drive_of(r, &subsys);
   if (d < 0 && subsys == SUBSYS_IEC)
      subsys_response(r, SS_SUBSYS_NOT_PRESENT);
   return d;
}

// --- GET drives ---------------------------------------------------------------------------

// A mount from the menu or UCI: the image's folder and name
static void shown_now(int d)
{
   const char *card = drive_path(d), *slash = strrchr(card, '/');
   int n = slash ? slash - card : 0;
   strcpy(img[d].card, card);
   strcpy(img[d].path, "/SD");
   memcpy(img[d].path + 3, card, n);
   img[d].path[3 + n] = 0;
   strcpy(img[d].name, slash ? slash + 1 : card);
}

static void drive_info(rest_t *r, int d)
{
   img_alloc();
   const char *card = drive_path(d), *path = "", *name = "";
   if (*card) {
      if (strcmp(card, img[d].card))
         shown_now(d);
      path = img[d].path;
      name = img[d].name;
   }
   obj_open(r, NULL);
   obj_open(r, d ? "b" : "a");
   key(r, "enabled");
   put(r, drive_running(d) ? "true" : "false");
   add_int(r, "bus_id", stores_drive_id(d));
   add_str(r, "type", types[stores_drive_type(d)]);
   // The pick for its model, as his current_drive_rom: the stock one under his default name
   int p = RP_DRV(d, stores_drive_type(d));
   add_str(r, "rom", rompick_state(p) == RS_FAILED ? "Unable to load!" :
                     *rompick_name(p) ? rompick_name(p) : rompick_default(p));
   add_str(r, "image_file", name);
   add_str(r, "image_path", path);
   obj_close(r);
   obj_close(r);
}

static void get_drives(rest_t *r)
{
   key(r, "drives");
   put(r, "[ ");
   for (int d = 0; d < drives_count() && d < 2; d++) {
      if (d)
         put(r, ", ");
      drive_info(r, d);
   }
   put(r, " ]");
   json_response(r, HTTP_OK);
}

// --- files into /c128/temp ----------------------------------------------------------------

static int mounted_anywhere(const char *card)
{
   for (int d = 0; d < drives_count(); d++)
      if (!strcasecmp(drive_path(d), card))
         return 1;
   return 0;
}

// /c128/temp/NAME for a file named name (his temp%04x without one), as his build_temp_path;
// _1, _2 ... before the extension while that name is in a drive
static void temp_name(const char *name, char *out, int size)
{
   char base[160];
   if (*name)
      fmt_snprintf(base, sizeof base, "%s", name);
   else
      fmt_snprintf(base, sizeof base, "temp%04x", temp_seq++);
   const char *dot = strrchr(base, '.');
   int stem = dot && dot != base ? dot - base : (int)strlen(base);
   f_mkdir("/c128");
   f_mkdir(TEMP_DIR);
   for (int suffix = 0;; suffix++) {
      if (!suffix)
         fmt_snprintf(out, size, TEMP_DIR "/%s", base);
      else {
         char head[160];
         memcpy(head, base, stem);
         head[stem] = 0;
         fmt_snprintf(out, size, TEMP_DIR "/%s_%d%s", head, suffix, base + stem);
      }
      if (!mounted_anywhere(out))
         return;
   }
}

void temp_sink(rest_t *r, mp_event_t ev, const uint8_t *data, int len)
{
   UINT put;
   switch (ev) {
   case MP_DATA_START:
   case MP_SUBHEADER:
      temp_name(r->up_name, r->up_path, sizeof r->up_path);
      r->up_open = f_open(&r->up_f, r->up_path, FA_WRITE | FA_CREATE_ALWAYS) == FR_OK;
      if (!r->up_open)
         r->up_err = SS_DISK_ERROR;
      break;
   case MP_DATA:
      if (r->up_open && (f_write(&r->up_f, data, len, &put) != FR_OK || put != (UINT)len)) {
         f_close(&r->up_f);
         r->up_open = 0;
         r->up_err = SS_DISK_ERROR;
      }
      break;
   case MP_DATA_END:
   case MP_END:
   case MP_ABORT:
      if (r->up_open)
         f_close(&r->up_f);
      r->up_open = 0;
      break;
   default:
      break;
   }
}

// The upload's whole path, as REST gives paths
const char *temp_file(rest_t *r)
{
   static char sd[sizeof r->up_path + 4];
   if (!r->up_parts || r->up_err || r->up_open)
      return NULL;
   card_to_sd(r->up_path, sd, sizeof sd);
   return sd;
}

// --- a copy over main-loop rounds (mode=unlinked, :unlink) --------------------------------

typedef struct {
   FIL      src, dst;
   uint32_t left;
} copy_t;

static copy_t *copies;                 // a connection's, in HyperRAM

static copy_t *copy_of(rest_t *r)
{
   if (!copies)
      copies = heap_alloc(HTTP_CLIENTS * sizeof *copies);
   return &copies[r->req->slot];
}

// The copy of card file src into /c128/temp, its name in r->up_path: SS_OK or an SS_*
static int copy_start(rest_t *r, const char *src)
{
   copy_t *c = copy_of(r);
   const char *name = strrchr(src, '/');
   if (!r->up)
      r->up = heap_alloc(UPLOAD_MAX + 1);
   temp_name(name ? name + 1 : src, r->up_path, sizeof r->up_path);
   if (f_open(&c->src, src, FA_READ) != FR_OK)
      return SS_CANNOT_OPEN_FILE;
   if (f_open(&c->dst, r->up_path, FA_WRITE | FA_CREATE_ALWAYS) != FR_OK) {
      f_close(&c->src);
      return SS_DISK_ERROR;
   }
   c->left = f_size(&c->src);
   return SS_OK;
}

// A step: 0 while it goes on, then SS_OK or SS_DISK_ERROR (both files closed then)
static int copy_step(rest_t *r, int *done)
{
   copy_t *c = copy_of(r);
   UINT got, put;
   uint32_t n = c->left < COPY_STEP ? c->left : COPY_STEP;
   *done = 0;
   if (n && (f_read(&c->src, r->up, n, &got) != FR_OK || got != n ||
             f_write(&c->dst, r->up, n, &put) != FR_OK || put != n)) {
      f_close(&c->src);
      f_close(&c->dst);
      *done = 1;
      return SS_DISK_ERROR;
   }
   c->left -= n;
   if (c->left)
      return SS_OK;
   f_close(&c->src);
   f_close(&c->dst);
   *done = 1;
   return SS_OK;
}

static void copy_cancel(rest_t *r)
{
   copy_t *c = copy_of(r);
   f_close(&c->src);
   f_close(&c->dst);
}

// --- mount --------------------------------------------------------------------------------

enum { M_COPY, M_TAKE, M_CHANGE };

static int mount_ss(int err)
{
   if (err == 0xEE42)
      return SS_CANNOT_OPEN_FILE;              // in the other drive: his fopen would fail
   if (err == 0xEE41 || err == 0xEE44 || err >= 0x100)
      return SS_DISK_ERROR;
   return SS_ERROR_IN_FILE_FORMAT;             // not an image's size, or too large
}

// The image into the drive; then the answer waits for a 1541/1571 to have it (uci_dos.c)
static int mount_now(rest_t *r, const char *card, int ro)
{
   const char *msg;
   int d = r->d, err = shell_mount_ro(d, card, ro, &msg);
   if (err)
      return mount_ss(err);
   if (drive_image_type(d) == 2 || drive_power_of(d) == DRIVE_OFF)
      r->phase = -1;                            // a 1581 flags it at once; an off drive never
   else {
      r->phase = M_TAKE;
      r->t0 = net_millis();
   }
   return SS_OK;
}

static int mount_step(rest_t *r)
{
   int ss = SS_OK, done;
   switch (r->phase) {
   case M_COPY:
      if ((ss = copy_step(r, &done)) == SS_OK && !done)
         return 0;
      if (ss == SS_OK) {
         char card[260];
         strcpy(card, r->up_path);
         ss = mount_now(r, card, 0);
      }
      if (ss != SS_OK || r->phase == -1)
         break;
      return 0;
   case M_TAKE:
      if (!drives_ready()) {
         r->phase = M_CHANGE;
         return 0;
      }
      if (net_millis() - r->t0 > TAKE_LIMIT) {
         ss = SS_INTERNAL_ERROR;
         break;
      }
      return 0;
   case M_CHANGE:
      if (drives_ready())
         break;
      if (net_millis() - r->t0 > CHANGE_LIMIT) {
         ss = SS_INTERNAL_ERROR;
         break;
      }
      return 0;
   }
   subsys_response(r, ss);
   return 1;
}

static void mount_cancel(rest_t *r)
{
   if (r->phase == M_COPY)
      copy_cancel(r);
}

// His api_mount: the drive, the type (by default the file's extension) and the mode
static void api_mount(rest_t *r, const char *fn, const char *type, const char *mode)
{
   static const char *const t_names[] = { "d64", "d71", "d81", "g64", "g71" };
   static const int t_ftype[] = { 1541, 1571, 1581, 1541, 1571 };
   int subsys, d = drive_of(r, &subsys), t = -1, command, flags = 0;
   char card[260];
   const char *c;

   if (subsys < 0)
      return;
   for (int i = 0; i < 5 && type; i++)
      if (!strcasecmp(type, t_names[i]))
         t = i;
   if (mode && !strcasecmp(mode, "readonly"))
      flags = READ_ONLY;
   else if (mode && !strcasecmp(mode, "unlinked"))
      flags = UNLINKED;
   if (t < 0) {
      error(r, "Invalid Type '%s'", type ? type : "(null)");
      json_response(r, HTTP_BAD_REQUEST);
      return;
   }
   command = (t < 3 ? MOUNT_D64 : MOUNT_G64) | flags;
   add_int(r, "Subsys", subsys);
   add_int(r, "Ftype", t_ftype[t]);
   add_int(r, "command", command);
   add_str(r, "file", fn);
   if (d < 0) {
      subsys_response(r, SS_SUBSYS_NOT_PRESENT);
      return;
   }
   if (t >= 3) {
      subsys_response(r, SS_NOT_IMPLEMENTED);  // the core's drives read sector images only
      return;
   }

   // His C1541: the file must open (read-only for unlinked or a read-only file)
   FILINFO fi;
   if (!(c = uci_card_path(fn, card)) || f_stat(c, &fi) != FR_OK || (fi.fattrib & AM_DIR)) {
      subsys_response(r, SS_CANNOT_OPEN_FILE);
      return;
   }
   r->d = d;
   if (flags & UNLINKED) {
      int ss = copy_start(r, c);
      if (ss != SS_OK) {
         subsys_response(r, ss);
         return;
      }
      r->phase = M_COPY;
   } else {
      int ss = mount_now(r, c, (flags & READ_ONLY) || (fi.fattrib & AM_RDO));
      if (ss != SS_OK) {
         subsys_response(r, ss);
         return;
      }
   }
   // What GET drives shows: path "" and the whole name, as his SubsysCommand had them
   img_alloc();
   strcpy(img[d].card, r->phase == M_COPY ? r->up_path : c);
   img[d].path[0] = 0;
   fmt_snprintf(img[d].name, sizeof img[d].name, "%s", fn);
   if (r->phase == -1)
      json_response(r, HTTP_OK);
   else
      rest_pending(r, mount_step, mount_cancel);
}

// His get_extension: up to 3 characters after the last '.'
static const char *extension(const char *name, char *ext)
{
   const char *dot = strrchr(name, '.');
   ext[0] = 0;
   if (dot)
      fmt_snprintf(ext, 4, "%s", dot + 1);
   return ext;
}

static void put_mount(rest_t *r)
{
   char ext[4];
   const char *image = arg(r, "image"), *type = arg(r, "type");
   api_mount(r, image, type ? type : extension(image, ext), arg(r, "mode"));
}

static void post_mount(rest_t *r)
{
   char ext[4];
   const char *fn = temp_file(r), *type = arg(r, "type");
   if (!fn) {
      error(r, "Upload of file failed.");
      json_response(r, HTTP_BAD_REQUEST);
      return;
   }
   api_mount(r, fn, type ? type : extension(fn, ext), arg(r, "mode"));
}

// --- the drive commands -------------------------------------------------------------------

// The drive's ROM bank again from boot1.rom, which resets it; the answer once it has it
static void reload(int d)
{
   crtrom_csr_set(DRIVE_ROM, 0, 1 << d);
}

static int rom_step(rest_t *r)
{
   int d = r->d;
   if (!drive_running(d) || (crtrom_csr_get(DRIVE_ROM, 0) >> d & 1) ||
       net_millis() - r->t0 > ROM_LIMIT) {
      subsys_response(r, r->phase);
      return 1;
   }
   return 0;
}

static void after_reload(rest_t *r, int d, int ss)
{
   reload(d);
   r->d = d;
   r->phase = ss;
   r->t0 = net_millis();
   rest_pending(r, rom_step, NULL);
}

// His MENU_1541_RESET: as power off and on, the drive's power as it was
static void put_reset(rest_t *r)
{
   int d = drive_or_answer(r);
   if (d >= 0)
      after_reload(r, d, SS_OK);
}

static void put_remove(rest_t *r)
{
   int d = drive_or_answer(r);
   if (d < 0)
      return;
   shell_unmount(d);
   subsys_response(r, SS_OK);
}

// On; one that was on already is reset
static void put_on(rest_t *r)
{
   int d = drive_or_answer(r);
   if (d < 0)
      return;
   int was = drive_running(d);
   drive_power(d, DRIVE_ON);
   if (was)
      after_reload(r, d, SS_OK);
   else
      subsys_response(r, SS_OK);
}

static void put_off(rest_t *r)
{
   int d = drive_or_answer(r);
   if (d < 0)
      return;
   drive_power(d, DRIVE_OFF);
   subsys_response(r, SS_OK);
}

// :unlink: hold the drive, its changes to the card, copy, the copy in, let go
enum { U_QUIET, U_COPY };

static void unlink_cancel(rest_t *r)
{
   if (r->phase == U_COPY)
      copy_cancel(r);
   drive_hold(r->d, 0);
}

static int unlink_step(rest_t *r)
{
   int ss = SS_OK, done;
   if (r->phase == U_QUIET) {
      if (!drive_quiet(r->d))
         return 0;
      if ((ss = copy_start(r, drive_path(r->d))) == SS_OK) {
         r->phase = U_COPY;
         return 0;
      }
   } else {
      if ((ss = copy_step(r, &done)) == SS_OK && !done)
         return 0;
      if (ss == SS_OK && drive_remap(r->d, r->up_path))
         ss = SS_DISK_ERROR;
      if (ss == SS_OK)
         strcpy(img[r->d].card, r->up_path);     // still shown as the file it came from
   }
   drive_hold(r->d, 0);
   subsys_response(r, ss);
   return 1;
}

static void put_unlink(rest_t *r)
{
   int d = drive_or_answer(r);
   if (d < 0)
      return;
   if (!drive_mounted(d) || stores_drive_internal(d)) {
      subsys_response(r, SS_OK);               // nothing to unlink
      return;
   }
   // What GET drives showed stays: the file it came from
   img_alloc();
   if (strcmp(drive_path(d), img[d].card))
      shown_now(d);
   r->d = d;
   r->phase = U_QUIET;
   drive_hold(d, 1);
   drive_flush(d);
   rest_pending(r, unlink_step, unlink_cancel);
}

// His load_dos_from_file: up to 32 KB into the drive's memory, a 16 KB image twice; the drive
// reset once anything came; 16 and 32 KB are ROMs
static void load_rom(rest_t *r, int d, const uint8_t *rom, int n)
{
   uint32_t at = (uint32_t)(stores_drive_type(d) * 2 + d) * BANK_SIZE;
   M2M_RAMROM_DEV = DRIVE_ROM;
   for (int i = 0; i < n; i++)
      M2M_DEV16[at + i] = rom[i];
   if (n == BANK_SIZE / 2)
      for (int i = 0; i < n; i++)
         M2M_DEV16[at + n + i] = rom[i];
   int ss = n == BANK_SIZE || n == BANK_SIZE / 2 ? SS_OK : SS_INVALID_DRIVE_ROM;
   if (n)
      after_reload(r, d, ss);
   else
      subsys_response(r, ss);
}

static void put_load_rom(rest_t *r)
{
   char card[260];
   const char *c;
   FIL f;
   UINT got;
   int d = drive_or_answer(r);
   if (d < 0)
      return;
   if (!(c = uci_card_path(arg(r, "file"), card)) || f_open(&f, c, FA_READ) != FR_OK) {
      subsys_response(r, SS_CANNOT_OPEN_FILE);
      return;
   }
   if (!r->up)
      r->up = heap_alloc(UPLOAD_MAX + 1);
   FRESULT res = f_read(&f, r->up, BANK_SIZE, &got);
   f_close(&f);
   if (res != FR_OK) {
      subsys_response(r, SS_CANNOT_OPEN_FILE);
      return;
   }
   load_rom(r, d, r->up, got);
}

static void post_load_rom(rest_t *r)
{
   int d = drive_or_answer(r);
   if (d < 0)
      return;
   if (!r->up_parts) {
      error(r, "Upload of file failed.");
      json_response(r, HTTP_BAD_REQUEST);
      return;
   }
   load_rom(r, d, r->up, r->up_len);
}

// The drive's model, as his set_drive_type and its config; the drive that is the internal
// floppy stays it as a 1581. Not a model its image can't run.
static void put_set_mode(rest_t *r)
{
   const char *mode = arg(r, "mode");
   int d = drive_or_answer(r), m = -1;
   if (d < 0)
      return;
   for (int i = 0; i < 3; i++)
      if (!strcasecmp(mode, types[i]))
         m = i;
   if (m < 0) {
      error(r, "Invalid Drive Type '%s'", mode);
      json_response(r, HTTP_BAD_REQUEST);
      return;
   }
   if (!stores_drive_ask_model(d, m)) {
      error(r, "The drive's image can't run as a %s", types[m]);
      json_response(r, HTTP_BAD_REQUEST);
      return;
   }
   add_str(r, "mode", mode);
   subsys_response(r, SS_OK);
}

static const param_t mount_params[]  = { { "image", P_REQUIRED }, { "type", 0 }, { "mode", 0 } };
static const param_t upload_params[] = { { "type", 0 }, { "mode", 0 } };
static const param_t file_params[]   = { { "file", P_REQUIRED } };
static const param_t mode_params[]   = { { "mode", P_REQUIRED } };

const call_t rest_drives_calls[] = {
   CALL0(HTTP_GET, "drives", "none", get_drives),
   CALL(HTTP_PUT, "drives", "mount", put_mount, mount_params, 0),
   CALLS(HTTP_POST, "drives", "mount", post_mount, upload_params, temp_sink),
   CALL0(HTTP_PUT, "drives", "reset", put_reset),
   CALL0(HTTP_PUT, "drives", "remove", put_remove),
   CALL0(HTTP_PUT, "drives", "on", put_on),
   CALL0(HTTP_PUT, "drives", "off", put_off),
   CALL0(HTTP_PUT, "drives", "unlink", put_unlink),
   CALL(HTTP_PUT, "drives", "load_rom", put_load_rom, file_params, 0),
   CALLU(HTTP_POST, "drives", "load_rom", post_load_rom, BANK_SIZE),
   CALL(HTTP_PUT, "drives", "set_mode", put_set_mode, mode_params, 0),
};
const int rest_drives_count = sizeof rest_drives_calls / sizeof rest_drives_calls[0];
