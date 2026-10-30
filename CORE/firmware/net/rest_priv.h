// The REST API's parts (rest.c, rest_machine.c, rest_configs.c): a request, its arguments
// and its answer, and the helpers that render the answer as Gideon's firmware does.
#pragma once
#include <stdbool.h>
#include <stdint.h>
#include "http.h"

#define ARGS_MAX   32
#define PARTS_MAX  16                   // his path_parts[16]
#define BODY_SIZE  (16 * 1024)          // a JSON answer
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

#define P_REQUIRED 1

typedef struct rest rest_t;

// A DMA job of a request (rest_machine.c)
typedef struct {
   uint16_t addr;
   int      len, pos, chunk, write;
   uint32_t t0;
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
} call_t;

#define CALL(m, route, cmd, proc, params, up) { m, route, cmd, proc, params, sizeof params / sizeof params[0], up }
#define CALL0(m, route, cmd, proc)            { m, route, cmd, proc, NULL, 0, 0 }

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

   // An answer that takes more than one main-loop round: step until it gives 1, having
   // answered; cancel when the connection goes first
   int         (*step)(rest_t *r);
   void        (*cancel)(rest_t *r);
   rest_job_t    job;
   uint8_t      *mem;                   // readmem's bytes, 64 KB once needed
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

// The request
const char *arg(rest_t *r, const char *k);   // NULL without it
int         arg_int(rest_t *r, const char *k, int alt);   // his get_int: strtol, base 0
bool        pattern_match(const char *pattern, const char *fixed);

extern const call_t rest_machine_calls[], rest_configs_calls[];
extern const int    rest_machine_count, rest_configs_count;
