// Ultimate Command Interface: the control target 4, with the commands, replies and status
// texts of Gideon's io/command_interface/control_target.cc, on the core: drives 8 and 9 are
// his drives A and B, REBOOT resets the C128 as the reset button does, FREEZE opens the menu.
// What only an Ultimate has (tape capture, RAM disks, extra SIDs, GCR in its memory) answers
// as an Ultimate without it does, and the Ultimate 64's commands as an Ultimate II's do.
#include <ctype.h>
#include <string.h>
#include <strings.h>
#include "config_ids.h"
#include "core.h"
#include "drives.h"
#include "ff.h"
#include "fmt.h"
#include "reu.h"
#include "roms.h"
#include "stores.h"
#include "uci.h"

#define CTRL_IDENTIFY        0x01
#define CTRL_FINISH_CAPTURE  0x03
#define CTRL_FREEZE          0x05
#define CTRL_REBOOT          0x06
#define CTRL_LOAD_REU        0x08
#define CTRL_SAVE_REU        0x09
#define CTRL_DECODE_TRACK    0x11
#define CTRL_EASYFLASH       0x20
#define CTRL_GET_HWINFO      0x28
#define CTRL_GET_DRVINFO     0x29
#define CTRL_ENABLE_DISK_A   0x30           // then DISABLE A, ENABLE B, DISABLE B
#define CTRL_DISABLE_DISK_B  0x33
#define CTRL_DISK_A_POWER    0x34           // then B
#define CTRL_DISK_B_POWER    0x35
#define CTRL_GET_RAMDISKINFO 0x40
#define CTRL_LOAD_CONFIG     0x50

#define CRT_DEVICE           0x0106         // the virtual cartridge (sw_cartridge_csr.vhd)
#define CRT_CSR_ERASE        0x024          // a sector: bit 6 ROMH's chip, bits 5-0 a bank of it
#define CRT_CSR_ERASE_GO     0x025          // W: start, R: bit 0 busy

static const uint8_t identification[] = "C128MEGA65 CONTROL V1.0";
static const uint8_t product[]        = "C128 for MEGA65";
static const char ok[]           = "00,OK";
static const char unknown[]      = "21,UNKNOWN COMMAND";
static const char invalid[]      = "81,INVALID PARAMS";
static const char track_errors[] = "82,ERRORS ON TRACK";
static const char reu_off[]      = "84,REU NOT ENABLED";
static const char reu_no_file[]  = "85,REU FILE CANNOT BE OPENED";
static const char no_config[]    = "88,CANNOT OPEN CONFIG FILE";
static const char cfg_errors[]   = "89,CONFIG FILE HAD ERRORS";
static const char internal[]     = "87,INTERNAL ERROR";

static char path[UCI_PATH_MAX];

// LOAD_REU or SAVE_REU under way, one reu_step a main-loop round (work())
#define REU_STEP (16 * 512)
static struct {
   FIL        f;
   reu_xfer_t x;
   int        on;
} reu_job;
static int ef_erasing;               // EASYFLASH's erase under way

// GET_DRVINFO: how many, then type, IEC address and power of each
static void drive_info(uci_reply_t *r)
{
   int n = drives_count() < 2 ? drives_count() : 2;
   uci_data[0] = n;
   for (int d = 0; d < n; d++) {
      uci_data[1 + 3 * d] = stores_drive_type(d);
      uci_data[2 + 3 * d] = stores_drive_id(d);
      uci_data[3 + 3 * d] = drive_running(d);
   }
   r->data = uci_data;
   r->len  = 1 + 3 * n;
}

// GET_HWINFO: 0 the product, 1 the cartridge's own SIDs (his UltiSIDs; the core has none)
static void hw_info(int device, uci_reply_t *r)
{
   if (device == 0) {
      r->data = product;
      r->len  = sizeof product - 1;
   } else if (device == 1) {
      uci_data[0] = 0;
      r->data = uci_data;
      r->len  = 1;
   } else
      r->status = invalid;
}

// LOAD_REU, SAVE_REU, as his REUPreloader: all of the REU's size from REU address 0 (there
// is no offset setting), from or to the file picked in the menu, else REU_DEFAULT_FILE. The
// reply holds his result as 4 bytes (the bytes moved; -1 no file, -2 no REU), then his
// message, in upper case. The card's path shows as the Ultimate shows it, under /SD.
static void reu_reply(uci_reply_t *r, int32_t result, const char *fmt, ...)
{
   va_list ap;
   for (int i = 0; i < 4; i++)
      uci_data[i] = (uint32_t)result >> (8 * i);
   va_start(ap, fmt);
   int n = fmt_vsnprintf((char *)uci_data + 4, UCI_MAX_REPLY - 4, fmt, ap);
   va_end(ap);
   for (int i = 4; i < 4 + n; i++)
      uci_data[i] = toupper(uci_data[i]);
   r->data = uci_data;
   r->len  = 4 + n;
}

static void reu(uint8_t *c, int len, uci_reply_t *r)
{
   int save = c[1] == CTRL_SAVE_REU;
   uint32_t size = reu_size();
   const char *p = reu_file();
   FRESULT res;

   if (len < 5) {
      r->status = invalid;
      return;
   }
   if (!size) {
      reu_reply(r, -2, "");
      r->status = reu_off;
      return;
   }
   if (save) {
      // The old file becomes .bu, after an old .bu is gone (his set_extension, in the name)
      char *dot;
      strcpy(path, p);
      if ((dot = strrchr(path, '.')) && dot > strrchr(path, '/'))
         *dot = 0;
      if (strlen(path) + 4 > sizeof path) {
         res = FR_INVALID_NAME;
      } else {
         strcat(path, ".bu");
         f_unlink(path);
         f_rename(p, path);
         res = f_open(&reu_job.f, p, FA_WRITE | FA_CREATE_ALWAYS);
      }
   } else
      res = f_open(&reu_job.f, p, FA_READ);
   if (res != FR_OK) {
      reu_reply(r, -1, "REU %s: Failed to open /SD%s: %s", save ? "Save" : "Load", p, uci_fr_text(res));
      r->status = reu_no_file;
      return;
   }
   reu_job.x  = (reu_xfer_t){ .f = &reu_job.f, .save = save, .left = size };
   reu_job.on = 1;
   r->status  = NULL;                                  // work() goes on with it
}

// EASYFLASH 0: his clears the 8 banks of the bank's sector (bank & $38) of ROMH's chip if
// the address has bit 5, else ROML's. Here crt_flash erases that sector as the chip's own
// command does, so the cartridge's cache follows; the answer comes once it is done. With
// no EasyFlash there is nothing to erase, and the answer comes at once.
static void ef_erase(uint8_t bank, uint8_t addr, uci_reply_t *r)
{
   crtrom_csr_set(CRT_DEVICE, CRT_CSR_ERASE, (addr & 0x20 ? 0x40 : 0) | (bank & 0x38));
   crtrom_csr_set(CRT_DEVICE, CRT_CSR_ERASE_GO, 1);
   ef_erasing = 1;
   r->status  = NULL;                                  // work() answers once it is done
}

static void reu_end(void)
{
   if (reu_job.on)
      f_close(&reu_job.f);
   reu_job.on = 0;
   ef_erasing = 0;
}

static void ctrl_work(uci_reply_t *r)
{
   if (ef_erasing) {
      if (!(crtrom_csr_get(CRT_DEVICE, CRT_CSR_ERASE_GO) & 1)) {
         ef_erasing = 0;
         r->status  = ok;
      }
      return;
   }
   if (!reu_job.on) {
      r->status = internal;
      return;
   }
   FRESULT res = reu_step(&reu_job.x, REU_STEP);
   if (res == FR_OK && reu_job.x.left)
      return;
   reu_end();
   if (reu_job.x.save)
      reu_reply(r, reu_job.x.done, "REU Save: Saved %d bytes from $%6x to /SD%s", reu_job.x.done, 0, reu_file());
   else
      reu_reply(r, reu_job.x.done, "REU Load: Loaded %d bytes to $%6x from /SD%s", reu_job.x.done, 0, reu_file());
   r->status = res == FR_OK ? ok : uci_fr_text(res);  // an SD error part of the way (his doesn't look)
}

// LOAD_CONFIG: a settings file in Gideon's format, read as his ConfigIO::S_read_from_file
// reads it (stores.c). A store or an item the core lacks is skipped with a line in the log,
// as his skips another machine's; a choice it lacks is an error, as a 1571 is on an
// Ultimate with 1541s only. After an error nothing changes (his effectuates only a file
// without one); else the settings change once the file is read, the REU without the reset
// a change in the menu gives. The log is the reply.

#define CFG_LOG 511                          // what his reply holds of the log
static int log_n;

static void cfg_log(const char *fmt, ...)
{
   va_list ap;
   va_start(ap, fmt);
   int n = fmt_vsnprintf((char *)uci_data + log_n, CFG_LOG + 1 - log_n, fmt, ap);
   va_end(ap);
   log_n += n < CFG_LOG - log_n ? n : CFG_LOG - log_n;
}

// By default /temp/uci_config.cfg (an Ultimate's RAM disk, not on the card)
static void load_config(uint8_t *c, int len, uci_reply_t *r)
{
   FIL f;
   const char *p = uci_card_path(len > 2 ? (const char *)c + 2 : "/temp/uci_config.cfg", path);
   if (!p || f_open(&f, p, FA_READ) != FR_OK) {
      r->status = no_config;
      return;
   }
   stores_begin();
   log_n = 0;
   int good = stores_read(&f, ~0u, cfg_log);
   f_close(&f);
   r->data   = uci_data;
   r->len    = log_n;
   r->status = !good ? cfg_errors : stores_apply() ? ok : internal;
}

static void command(uint8_t *c, int len, uci_reply_t *r)
{
   static const uint8_t no_ramdisks[8];
   r->status = ok;
   switch (len > 1 ? c[1] : -1) {
   case CTRL_IDENTIFY:
      r->data = identification;
      r->len  = sizeof identification - 1;
      break;
   case CTRL_FINISH_CAPTURE:                           // no tape capture to finish
      break;
   case CTRL_FREEZE:                                   // his cartridge button
      shell_open_menu();
      r->status = "";
      break;
   case CTRL_REBOOT:                                   // start_cartridge: our reset button
      uci_reboot();
      r->status = "";
      break;
   case CTRL_LOAD_REU:
   case CTRL_SAVE_REU:
      reu(c, len, r);
      break;
   case CTRL_DECODE_TRACK:                             // nothing decodes: no GCR here
      if (len != 14)
         r->status = invalid;
      else {
         uci_data[0] = 0;
         r->data   = uci_data;
         r->len    = 1;
         r->status = track_errors;
      }
      break;
   case CTRL_EASYFLASH:                                // 0: erase a sector of 8 banks
      if (len < 5 || c[2] != 0)
         r->status = invalid;
      else
         ef_erase(c[3], c[4], r);
      break;
   case CTRL_GET_HWINFO:
      hw_info(len < 3 ? 0 : c[2], r);
      break;
   case CTRL_GET_DRVINFO:
      drive_info(r);
      break;
   case CTRL_ENABLE_DISK_A:
   case CTRL_ENABLE_DISK_A + 1:
   case CTRL_ENABLE_DISK_A + 2:
   case CTRL_DISABLE_DISK_B:
      drive_power((c[1] - CTRL_ENABLE_DISK_A) >> 1, c[1] & 1 ? DRIVE_OFF : DRIVE_ON);
      break;
   case CTRL_DISK_A_POWER:
   case CTRL_DISK_B_POWER:
      r->data = (const uint8_t *)(drive_running(c[1] - CTRL_DISK_A_POWER) ? "on " : "off");
      r->len  = 3;
      break;
   case CTRL_GET_RAMDISKINFO:
      r->data = no_ramdisks;
      r->len  = sizeof no_ramdisks;
      break;
   case CTRL_LOAD_CONFIG:
      load_config(c, len, r);
      break;
   default:
      r->status = unknown;
   }
}

const uci_target_t uci_ctrl = { command, NULL, ctrl_work, reu_end, reu_end };
