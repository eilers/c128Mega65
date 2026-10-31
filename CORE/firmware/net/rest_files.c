// The REST API's files routes: Gideon's software/api/route_files.cc at 4b00732 on the SD
// card ("/SD/..."), with his FileSystemD64/D71/D81::format (filesystem/filesystem_d64.cc)
// for the new images: zeros, then the BAM and the first directory block. The zeros go out
// a few KB a main-loop round. create_dnp answers 501: nothing on the core reads a CMD native
// partition.
// Ported from the 1541 Ultimate firmware (github.com/GideonZ/1541ultimate), Copyright
// Gideon Zweijtzer and contributors, GPL v3 as this core; changed for c128Mega65 in 2026.
#include <ctype.h>
#include <string.h>
#include "ff.h"
#include "fmt.h"
#include "heap.h"
#include "rest_priv.h"
#include "uci.h"

#define ZERO_STEP 4096                 // a main-loop round's zeros

enum { D64, D71, D81 };

static const int layout_d64[] = { 17, 21, 7, 19, 6, 18, -1, 17 };
static const int layout_d71[] = { 17, 21, 7, 19, 6, 18, 5, 17, 17, 21, 7, 19, 6, 18, -1, 17 };
static const int layout_d81[] = { -1, 40 };

// The file a request creates, and its image (a connection's, in HyperRAM)
typedef struct {
   FIL       f;
   uint32_t  left;
   int       kind;
   const int *layout;
   uint8_t   root[256], bam[512], dir[256];
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

// --- his FileSystemCBM, as format needs it ------------------------------------------------

// get_abs_sector: the sectors of the tracks before it, then the sector
static int abs_sector(const int *m, int track, int sector)
{
   int abs = sector;
   for (--track; m[0] >= 1 && track >= m[0]; m += 2) {
      abs += m[0] * m[1];
      track -= m[0];
   }
   return abs + track * m[1];
}

static void track_sector(const int *m, int abs, int *track, int *sector)
{
   *track = 1;
   for (;;) {
      int t = abs / m[1];
      if (m[0] < 1 || abs < m[0] * m[1]) {
         *track += t;
         abs -= t * m[1];
         break;
      }
      *track += m[0];
      abs -= m[0] * m[1];
      m += 2;
   }
   *sector = abs;
}

// modify_allocation_bit: a free or a used sector in a track's count and bits
static void allocation(uint8_t *fr, uint8_t *bits, int sector, int alloc)
{
   int n = sector >> 3, b = sector & 7;
   if (!alloc) {
      if (bits[n] & 1 << b)
         return;
      bits[n] |= 1 << b;
      (*fr)++;
   } else {
      if (!(bits[n] & 1 << b))
         return;
      bits[n] &= ~(1 << b);
      if (*fr)
         (*fr)--;
   }
}

static void set_allocation(int k, int track, int sector, int alloc)
{
   uint8_t *fr, *m;
   if (mk[k].kind == D64) {
      fr = &mk[k].root[4 * track];
      m = fr + 1;
   } else if (mk[k].kind == D71) {
      if (track > 35) {
         fr = &mk[k].root[0xDD + track - 36];
         m = &mk[k].bam[3 * (track - 36)];
      } else {
         fr = &mk[k].root[4 * track];
         m = fr + 1;
      }
   } else {
      fr = &mk[k].bam[track > 40 ? 0x110 + 6 * (track - 41) : 0x10 + 6 * (track - 1)];
      m = fr + 1;
   }
   allocation(fr, m, sector, alloc);
}

// set_volume_name: 27 bytes of $A0, the DOS type at 21, up to 16 characters of the name and
// 2 of an id after a ',', in capitals
static void volume_name(const char *name, uint8_t *bam_name, const char *dos)
{
   memset(bam_name, 0xA0, 27);
   bam_name[21] = dos[0];
   bam_name[22] = dos[1];
   const char *p = name;
   for (int t = 0; t < 16 && *p && *p != ','; t++)
      bam_name[t] = toupper((unsigned char)*p++);
   while (*p && *p != ',')
      p++;
   if (*p == ',') {
      p++;
      for (int t = 0; t < 2 && p[t]; t++)
         bam_name[18 + t] = toupper((unsigned char)p[t]);
   }
}

// FileSystemD64/D71/D81::format
static void format(int k, const char *name)
{
   static const int sectors[] = { 683, 683 * 2, 3200 };
   int kind = mk[k].kind, t, s;
   uint8_t *root = mk[k].root, *bam = mk[k].bam;
   memset(root, 0, 256);
   memset(bam, 0, 512);
   memset(mk[k].dir, 0, 256);
   mk[k].dir[1] = 0xFF;
   root[0] = kind == D81 ? 40 : 18;
   root[1] = kind == D81 ? 3 : 1;
   root[2] = kind == D81 ? 0x44 : 0x41;
   if (kind != D81)
      root[3] = kind == D71 ? 0x80 : 0x20;
   volume_name(name, kind == D81 ? root + 4 : root + 144, kind == D81 ? "3D" : "2A");
   for (int i = 0; i < sectors[kind]; i++) {
      track_sector(mk[k].layout, i, &t, &s);
      set_allocation(k, t, s, 0);
   }
   if (kind == D64) {
      set_allocation(k, 18, 0, 1);
      set_allocation(k, 18, 1, 1);
   } else if (kind == D71) {
      set_allocation(k, 18, 0, 1);
      set_allocation(k, 18, 1, 1);
      set_allocation(k, 18, 1, 1);
      for (int i = 0; i < 19; i++)
         set_allocation(k, 53, i, 1);
   } else {
      for (int i = 0; i < 4; i++)
         set_allocation(k, 40, i, 1);
      bam[0] = 40;
      bam[1] = 2;
      bam[2] = 0x44;
      bam[3] = 0xBB;
      bam[4] = root[22];
      bam[5] = root[23];
      bam[6] = 0xC0;
      memcpy(bam + 256, bam, 6);
      bam[256] = 0x00;
      bam[257] = 0xFF;
   }
}

static FRESULT put_sectors(FIL *f, const int *layout, int track, int sector, const uint8_t *d, int n)
{
   UINT put;
   FRESULT res = f_lseek(f, (FSIZE_t)abs_sector(layout, track, sector) * 256);
   if (res == FR_OK)
      res = f_write(f, d, 256 * n, &put);
   return res == FR_OK && put != (UINT)(256 * n) ? FR_DISK_ERR : res;
}

// sync: the directory block, the header (root), and the D71's or D81's BAM blocks
static FRESULT write_image(int k)
{
   FIL *f = &mk[k].f;
   const int *l = mk[k].layout;
   int d81 = mk[k].kind == D81;
   FRESULT res = put_sectors(f, l, d81 ? 40 : 18, d81 ? 3 : 1, mk[k].dir, 1);
   if (res == FR_OK)
      res = put_sectors(f, l, d81 ? 40 : 18, 0, mk[k].root, 1);
   if (res == FR_OK && mk[k].kind == D71)
      res = put_sectors(f, l, 53, 0, mk[k].bam, 1);
   if (res == FR_OK && d81)
      res = put_sectors(f, l, 40, 1, mk[k].bam, 2);
   return res;
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
   format(k, mk[k].name);
   res = write_image(k);
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
   if (!r->up)
      r->up = heap_alloc(UPLOAD_MAX + 1);
   memset(r->up, 0, ZERO_STEP);
   mk[k].kind = kind;
   mk[k].layout = kind == D64 ? layout_d64 : kind == D71 ? layout_d71 : layout_d81;
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
   create(r, D64, (17 * (tracks - 35) + 683) * 256);
}

static void put_create_d71(rest_t *r)
{
   add_str(r, "path", r->full_path);
   add_int(r, "tracks", 70);
   add_str(r, "diskname", diskname(r));
   create(r, D71, 683 * 2 * 256);
}

static void put_create_d81(rest_t *r)
{
   add_str(r, "path", r->full_path);
   add_str(r, "diskname", diskname(r));
   create(r, D81, 256 * 3200);
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
