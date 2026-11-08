// The REST API's files routes: Gideon's software/api/route_files.cc at 4b00732 on the SD
// card ("/SD/..."), the new images made by diskimg.c: zeros, then the BAM and the first
// directory block. The zeros go out a few KB a main-loop round. create_dnp answers 501:
// nothing on the core reads a CMD native partition.
// Ported from the 1541 Ultimate firmware (github.com/GideonZ/1541ultimate), Copyright
// Gideon Zweijtzer and contributors, GPL v3 as this core; changed for c128Mega65 in 2026.
#include <ctype.h>
#include <string.h>
#include "diskimg.h"
#include "ff.h"
#include "fmt.h"
#include "heap.h"
#include "rest_priv.h"
#include "uci.h"

#define ZERO_STEP 4096                 // a main-loop round's zeros

// The file a request creates, and its image (a connection's, in HyperRAM)
typedef struct {
   FIL       f;
   uint32_t  left;
   int       kind;
   diskimg_t img;
   char      name[256];                // the directory header's name (and id)
} make_t;

static make_t *mk;

// --- GET files:info -----------------------------------------------------------------------

// The card's path of a request's path ("SD/..."), or NULL
static const char *card_of(rest_t *r, char *out)
{
   char whole[sizeof r->full_path + 2];
   fmt_snprintf(whole, sizeof whole, "/%s", r->full_path);
   return uci_card_path(whole, out);
}

static void get_info(rest_t *r)
{
   char card[UCI_PATH_MAX], ext[4] = "";
   FILINFO fi;
   const char *c = card_of(r, card);
   FRESULT res = c ? f_stat(c, &fi) : FR_NO_PATH;
   if (res != FR_OK) {
      error(r, "%s", uci_fr_text(res));
      json_response(r, HTTP_NOT_FOUND);
      return;
   }
   // His FileInfo: the name cut at 127 characters, up to 3 of the extension in capitals
   const char *dot = strrchr(fi.fname, '.');
   for (int i = 0; dot && i < 3 && dot[1 + i]; i++)
      ext[i] = toupper((unsigned char)dot[1 + i]), ext[i + 1] = 0;
   fi.fname[127] = 0;
   obj_open(r, "files");
   add_str(r, "path", r->full_path);
   add_str(r, "filename", fi.fname);
   add_int(r, "size", (int)fi.fsize);
   add_str(r, "extension", ext);
   obj_close(r);
   json_response(r, HTTP_OK);
}

// --- PUT files:create_* -------------------------------------------------------------------

static void fail(rest_t *r, FRESULT res)
{
   error(r, "%s", uci_fr_text(res));
   json_response(r, HTTP_INTERNAL_ERROR);
}

// his create_file_of_size's zeros, then format: a step a round
static int create_step(rest_t *r)
{
   int k = r->req->slot;
   UINT put;
   uint32_t n = mk[k].left < ZERO_STEP ? mk[k].left : ZERO_STEP;
   FRESULT res = FR_OK;
   if (n) {
      res = f_write(&mk[k].f, r->up, n, &put);
      if (res == FR_OK && put != n)
         res = FR_DENIED;
      mk[k].left -= put;
      if (res == FR_OK && mk[k].left)
         return 0;
      add_int(r, "bytes_written", (int)(r->job.len - mk[k].left));
      if (res != FR_OK) {
         f_close(&mk[k].f);
         fail(r, res);
         return 1;
      }
   }
   diskimg_format(&mk[k].img, mk[k].kind, mk[k].name);
   res = diskimg_write(&mk[k].img, &mk[k].f);
   FRESULT cl = f_close(&mk[k].f);
   if (res == FR_OK)
      res = cl;
   if (res != FR_OK)
      fail(r, res);
   else
      json_response(r, HTTP_OK);
   return 1;
}

static void create_cancel(rest_t *r)
{
   f_close(&mk[r->req->slot].f);
}

// His enforce_diskname: the file's name without its extension
static const char *diskname(rest_t *r)
{
   if (!mk)
      mk = heap_alloc(HTTP_CLIENTS * sizeof *mk);
   char *d = mk[r->req->slot].name;
   const char *name = arg(r, "diskname");
   if (name) {
      fmt_snprintf(d, sizeof mk[0].name, "%s", name);
      return d;
   }
   const char *slash = strrchr(r->full_path, '/');
   fmt_snprintf(d, sizeof mk[0].name, "%s", slash ? slash + 1 : r->full_path);
   char *dot = strrchr(d, '.');
   if (dot)
      *dot = 0;
   return d;
}

static void create(rest_t *r, int kind, int bytes)
{
   int k = r->req->slot;
   if (!mk)
      mk = heap_alloc(HTTP_CLIENTS * sizeof *mk);
   char card[UCI_PATH_MAX];
   const char *c = card_of(r, card);
   FRESULT res = c ? f_open(&mk[k].f, c, FA_WRITE | FA_CREATE_NEW | FA_CREATE_ALWAYS) : FR_NO_PATH;
   if (res != FR_OK) {
      fail(r, res);
      return;
   }
   diskimg_expand(&mk[k].f, bytes);
   if (!r->up)
      r->up = heap_alloc(UPLOAD_MAX + 1);
   memset(r->up, 0, ZERO_STEP);
   mk[k].kind = kind;
   mk[k].left = bytes;
   r->job.len = bytes;
   rest_pending(r, create_step, create_cancel);
}

static void put_create_d64(rest_t *r)
{
   int tracks = arg_int(r, "tracks", 35);
   add_str(r, "path", r->full_path);
   add_int(r, "tracks", tracks);
   const char *name = diskname(r);
   add_str(r, "diskname", name);
   if (tracks < 35 || tracks > 41) {
      error(r, "Track count should be between 35 and 41.");
      json_response(r, HTTP_BAD_REQUEST);
      return;
   }
   create(r, DISK_D64, (17 * (tracks - 35) + 683) * 256);
}

static void put_create_d71(rest_t *r)
{
   add_str(r, "path", r->full_path);
   add_int(r, "tracks", 70);
   add_str(r, "diskname", diskname(r));
   create(r, DISK_D71, diskimg_bytes[DISK_D71]);
}

static void put_create_d81(rest_t *r)
{
   add_str(r, "path", r->full_path);
   add_str(r, "diskname", diskname(r));
   create(r, DISK_D81, diskimg_bytes[DISK_D81]);
}

// His checks, then no image: nothing on the core reads a CMD native partition
static void put_create_dnp(rest_t *r)
{
   int tracks = arg_int(r, "tracks", 0);
   add_str(r, "path", r->full_path);
   add_int(r, "tracks", tracks);
   if (tracks < 1 || tracks > 255) {
      error(r, "Invalid number of tracks (1-255).");
      json_response(r, HTTP_BAD_REQUEST);
      return;
   }
   add_str(r, "diskname", diskname(r));
   subsys_response(r, SS_NOT_IMPLEMENTED);
}

static const param_t d64_params[] = { { "tracks", 0 }, { "diskname", 0 } };
static const param_t name_params[] = { { "diskname", 0 } };
static const param_t dnp_params[] = { { "tracks", P_REQUIRED }, { "diskname", 0 } };

const call_t rest_files_calls[] = {
   CALL0(HTTP_GET, "files", "info", get_info),
   CALL(HTTP_PUT, "files", "create_d64", put_create_d64, d64_params, 0),
   CALL(HTTP_PUT, "files", "create_d71", put_create_d71, name_params, 0),
   CALL(HTTP_PUT, "files", "create_d81", put_create_d81, name_params, 0),
   CALL(HTTP_PUT, "files", "create_dnp", put_create_dnp, dnp_params, 0),
};
const int rest_files_count = sizeof rest_files_calls / sizeof rest_files_calls[0];
