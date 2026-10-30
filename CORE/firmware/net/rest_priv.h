// The REST API's parts (rest.c and the rest_*.c routes): a request, its arguments and its
// answer, and the helpers that render the answer as Gideon's firmware does.
#pragma once
#include <stdbool.h>
#include <stdint.h>
#include "ff.h"
#include "http.h"
#include "multipart.h"

#define ARGS_MAX   32
#define PARTS_MAX  16                   // his path_parts[16]
#define BODY_SIZE  (64 * 1024)          // a JSON answer; a longer one answers an error
#define ERROR_SIZE 2048
#define DEPTH_MAX  4                    // objects in objects
#define UPLOAD_MAX 65536                // the most of a POST's body a route keeps

#define HTTP_OK                200
#define HTTP_BAD_REQUEST       400
#define HTTP_FORBIDDEN         403
#define HTTP_NOT_FOUND         404
#define HTTP_PRECONDITION      412
#define HTTP_LOCKED            423
#define HTTP_INTERNAL_ERROR    500
#define HTTP_NOT_IMPLEMENTED   501

// His SubsysResultCode_e (infra/return_codes.h), the ones the routes give; subsys_response
// answers one with his text and HTTP code (SubsysCommand's error_string, http_response_map)
enum {
   SS_OK, SS_INVALID_PARAMETER, SS_NO_USER_INTERFACE, SS_GENERIC_ERROR, SS_DISK_ERROR,
   SS_SUBSYS_NOT_PRESENT, SS_SUBSYS_NO_EXEC, SS_NO_LOCK, SS_DISK_MODIFIED, SS_NO_DRIVE_ROM,
   SS_INVALID_DRIVE_ROM, SS_ONLY_1541, SS_WRONG_DRIVE_TYPE, SS_CANNOT_OPEN_FILE,
   SS_FILE_SEEK_FAILED, SS_FILE_READ_FAILED, SS_WRONG_MOUNT_MODE, SS_UNDEFINED_COMMAND,
   SS_ABORTED_BY_USER, SS_SAVE_FAILED, SS_EEPROM_TOO_LARGE, SS_EEPROM_ALREADY_DEFINED,
   SS_ROM_IMAGE_TOO_LARGE, SS_ERROR_IN_FILE_FORMAT, SS_NOT_IMPLEMENTED, SS_OUT_OF_MEMORY,
   SS_MP3_INVALID_SIZE, SS_MP3_DNP_TOO_LARGE, SS_INTERNAL_ERROR
};

#define P_REQUIRED 1

typedef struct rest rest_t;

// A DMA job of a request (rest_machine.c): len bytes at addr, from or into buf
typedef struct {
   uint16_t addr;
   int      len, pos, chunk, write;
   uint32_t t0;
   uint8_t *buf;
   int      soft;                       // a failure doesn't answer: the caller looks at pos
} rest_job_t;

typedef struct {
   const char *name;
   int         flags;
} param_t;

typedef struct {
   http_method_t  method;
   const char    *route, *cmd;
   void         (*proc)(rest_t *r);
   const param_t *params;
   int            nparams;
   int            upload;               // a POST's body: the most kept of its first part, or 0
   // Or the first part as it comes (MP_DATA_START or MP_SUBHEADER, MP_DATA, MP_DATA_END):
   // into a file, a device
   void         (*sink)(rest_t *r, mp_event_t ev, const uint8_t *data, int len);
} call_t;

#define CALL(m, route, cmd, proc, params, up) { m, route, cmd, proc, params, sizeof params / sizeof params[0], up, NULL }
#define CALL0(m, route, cmd, proc)            { m, route, cmd, proc, NULL, 0, 0, NULL }
#define CALLU(m, route, cmd, proc, up)        { m, route, cmd, proc, NULL, 0, up, NULL }
#define CALLS(m, route, cmd, proc, params, sink) { m, route, cmd, proc, params, sizeof params / sizeof params[0], 1, sink }
#define CALLS0(m, route, cmd, proc, sink)        { m, route, cmd, proc, NULL, 0, 1, sink }

// A request's arguments and answer, one per connection, in HyperRAM
struct rest {
   const call_t *call;
   http_req_t   *req;
   http_resp_t  *res;
   char          url[HTTP_MAX_HEADER_SIZE + 4];
   const char   *keys[ARGS_MAX], *vals[ARGS_MAX];
   int           nargs;
   char         *parts[PARTS_MAX];
   int           depth;                 // path parts
   char          full_path[HTTP_MAX_HEADER_SIZE + 4];

   char          body[BODY_SIZE];       // a JSON or HTML answer
   int           len, pos;
   bool          over;                  // the answer didn't fit in body
   bool          first[DEPTH_MAX];      // no key in the open object yet
   int           level;
   char          errors[ERROR_SIZE];    // the errors list's items, rendered
   int           elen;
   const uint8_t *bin;                  // a binary answer instead
   int           bin_len;

   // A POST's body: his first part's bytes, up to the call's upload
   uint8_t      *up;                    // UPLOAD_MAX + 1 bytes, once needed
   int           up_len, up_size, up_parts;
   void         *mp;                    // multipart.c's
   char          up_name[128];          // the first part's file name, his fix_filename'd, or ""
   // A sink's: the file it writes (FatFs path in up_path), or how far into a device
   FIL           up_f;
   int           up_open, up_err;       // up_err: an SS_* once it went wrong
   uint32_t      up_at;
   char          up_path[256];

   // An answer that takes more than one main-loop round: step until it gives 1, having
   // answered; cancel when the connection goes first
   int         (*step)(rest_t *r);
   void        (*cancel)(rest_t *r);
   rest_job_t    job;
   uint8_t      *mem;                   // readmem's bytes, 64 KB once needed
   int           phase, d;              // a route's own steps
   uint32_t      t0;
};

// The answer
void        put(rest_t *r, const char *f, ...);
void        key(rest_t *r, const char *k);
void        add_str(rest_t *r, const char *k, const char *v);
void        add_int(rest_t *r, const char *k, int v);
void        obj_open(rest_t *r, const char *k);
void        obj_close(rest_t *r);
void        list_open(rest_t *r, const char *k);
void        list_str(rest_t *r, int n, const char *v);   // item n, from 0
void        list_close(rest_t *r, int n);                // after n items
void        error(rest_t *r, const char *f, ...);
void        json_response(rest_t *r, int code);
void        html_response(rest_t *r, int code, const char *title, const char *text);
void        binary_response(rest_t *r, const uint8_t *data, int len);
void        rest_pending(rest_t *r, int (*step)(rest_t *r), void (*cancel)(rest_t *r));
void        subsys_response(rest_t *r, int ss);   // his error text (none for SS_OK) and code

// The request
const char *arg(rest_t *r, const char *k);   // NULL without it
int         arg_int(rest_t *r, const char *k, int alt);   // his get_int: strtol, base 0
bool        pattern_match(const char *pattern, const char *fixed);

// The bus DMA (rest_machine.c): a job set up, then stepped from a route's step until it
// gives 1 (job.pos == job.len: done; else it failed and answered 423, or not if job.soft)
void        job_begin(rest_t *r, int addr, int len, int write, uint8_t *buf);
int         job_step(rest_t *r);
void        job_cancel(rest_t *r);        // the job's request went: the DMA stops
void        dma_keep(int on);             // the C128's CPU held between jobs too (a C128 reset ends it)

// A file into /c128/temp, as his TempfileWriter into /Temp (rest_drives.c): the sink, and
// the path the route then uses ("/SD/c128/temp/NAME"), or NULL if it went wrong
void        temp_sink(rest_t *r, mp_event_t ev, const uint8_t *data, int len);
const char *temp_file(rest_t *r);

extern const call_t rest_machine_calls[], rest_configs_calls[], rest_drives_calls[],
                    rest_files_calls[], rest_runners_calls[];
extern const int    rest_machine_count, rest_configs_count, rest_drives_count, rest_files_count,
                    rest_runners_count;
