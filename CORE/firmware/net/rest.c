// The REST API on http.c: Gideon's software/api at 4b00732 (the II+L's 3.15a has the same
// core): routes.h's ArgsURI and ResponseWrapper, routes.cc's About routes, and
// MicroHttpServer's url.c and middleware.c. Answers match his to the byte: JSON as json.h
// renders it (no escaping, "{ \n", "[  ]"), his status texts (412 and 500 go out as
// "Unknown"), and a bare 404 for anything that isn't a route (no web pages here).
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "config_ids.h"
#include "eth.h"
#include "fmt.h"
#include "heap.h"
#include "http.h"
#include "m2m_rv.h"
#include "version.h"

#define ARGS_MAX   32
#define PARTS_MAX  16                   // his path_parts[16]
#define BODY_SIZE  (16 * 1024)          // a JSON answer
#define ERROR_SIZE 2048

#define HTTP_OK          200
#define HTTP_BAD_REQUEST 400
#define HTTP_FORBIDDEN   403

#define P_REQUIRED 1

typedef struct rest rest_t;

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
} call_t;

// A request's arguments and answer, one per connection, in HyperRAM
struct rest {
   const call_t *call;
   http_resp_t  *res;
   char          url[HTTP_MAX_HEADER_SIZE + 4];
   const char   *keys[ARGS_MAX], *vals[ARGS_MAX];
   int           nargs;
   char         *parts[PARTS_MAX];
   int           depth;
   char          full_path[HTTP_MAX_HEADER_SIZE + 4];
   char          body[BODY_SIZE];
   int           len, pos;
   bool          first;                 // no key in the object yet
   char          errors[ERROR_SIZE];    // the errors list's items, rendered
   int           elen;
};

static rest_t *rests[HTTP_CLIENTS];
static char    password[64];

void rest_set_password(const char *pw)
{
   fmt_snprintf(password, sizeof password, "%s", pw);
}

// newlib's strsep (see http.c's)
static char *sep(char **s, const char *delim)
{
   char *t = *s;
   if (!t || !*t) {
      *s = NULL;
      return NULL;
   }
   char *e = t + strcspn(t, delim);
   if (*e) {
      *e = 0;
      *s = e + 1;
   } else {
      *s = NULL;
   }
   return t;
}

// --- the answer: ResponseWrapper ----------------------------------------------------------

static void putv(rest_t *r, const char *f, va_list ap)
{
   int room = BODY_SIZE - r->len;
   int n = fmt_vsnprintf(r->body + r->len, room, f, ap);
   r->len += n < room ? n : room - 1;
}

static void put(rest_t *r, const char *f, ...)
{
   va_list ap;
   va_start(ap, f);
   putv(r, f, ap);
   va_end(ap);
}

static void key(rest_t *r, const char *k)
{
   put(r, r->first ? "  \"%s\" : " : ",\n  \"%s\" : ", k);
   r->first = false;
}

static void add_str(rest_t *r, const char *k, const char *v)
{
   key(r, k);
   put(r, "\"%s\"", v);
}

// An item for the errors list, cut at 199 characters as his
static void error(rest_t *r, const char *f, ...)
{
   char msg[200];
   va_list ap;
   va_start(ap, f);
   fmt_vsnprintf(msg, sizeof msg, f, ap);
   va_end(ap);
   int room = ERROR_SIZE - r->elen;
   int n = fmt_snprintf(r->errors + r->elen, room, r->elen ? ", \"%s\"" : "\"%s\"", msg);
   r->elen += n < room ? n : room - 1;
}

static int body_out(void *ctx, uint8_t *buf, int size)
{
   rest_t *r = ctx;
   int n = r->len - r->pos;
   if (n > size)
      n = size;
   memcpy(buf, r->body + r->pos, n);
   r->pos += n;
   return n;
}

static const char *code_text(int code)
{
   return code == 200 ? "OK" : code == 203 ? "No Content" : code == 400 ? "Bad Request" :
          code == 403 ? "Forbidden" : code == 404 ? "Not Found" : "Unknown";
}

static void respond(rest_t *r, int code, const char *type)
{
   http_resp_t *res = r->res;
   r->pos = 0;
   res->body_cb = body_out;
   res->body_ctx = r;
   res->index = fmt_snprintf((char *)res->buf, HTTP_BUFFER_SIZE,
                             "HTTP/1.1 %d %s\r\nConnection: close\r\nContent-Type: %s\r\n"
                             "Content-Length: %d\r\n\r\n", code, code_text(code), type, r->len);
}

static void json_response(rest_t *r, int code)
{
   key(r, "errors");
   put(r, r->elen ? "[ %s ]\n}" : "[  ]\n}", r->errors);
   respond(r, code, "application/json");
}

static void html_response(rest_t *r, int code, const char *title, const char *text)
{
   r->len = 0;
   put(r, "<html><body><h1>%s</h1>\n<p>%s</p></body></html>\r\n", title, text);
   respond(r, code, "text/html");
}

static void not_found(http_resp_t *res)
{
   res->body_cb = NULL;
   res->index = fmt_snprintf((char *)res->buf, HTTP_BUFFER_SIZE,
                             "HTTP/1.1 404 Not Found\r\nConnection: close\r\n\r\n");
}

// --- the request: url.c and ArgsURI -------------------------------------------------------

// url.c's url_decode: %XX (two hex digits, else the % stays) and + for a space
static void url_decode(char *s)
{
   char *d = s;
   for (; *s; s++) {
      if (*s == '%' && s[1] && s[2] && isxdigit((unsigned char)s[1]) && isxdigit((unsigned char)s[2])) {
         char code[3] = { s[1], s[2], 0 };
         *d++ = (char)strtoul(code, NULL, 16);
         s += 2;
      } else {
         *d++ = *s == '+' ? ' ' : *s;
      }
   }
   *d = 0;
}

static void set_arg(rest_t *r, const char *k, const char *v)
{
   for (int i = 0; i < r->nargs; i++)
      if (!strcmp(r->keys[i], k)) {
         r->vals[i] = v;
         return;
      }
   if (r->nargs < ARGS_MAX) {
      r->keys[r->nargs] = k;
      r->vals[r->nargs++] = v;
   }
}

// /v1/ROUTE/PATH:COMMAND?QUERY, as url.c's parse_url_static cuts it: the query at the
// first ?, the command at the first : before it, the path at the first / before that.
// Then the query's arguments and the path's parts, decoded. False if it isn't an API URL.
static bool parse_url(rest_t *r, const char *uri, char **route, char **cmd)
{
   static char empty[] = "";
   char *path, *query;

   fmt_snprintf(r->url, sizeof r->url, "%s", uri);
   char *token = r->url + (r->url[0] == '/');
   char *version = sep(&token, "/");
   if (!token || strcmp(version, "v1"))
      return false;
   char *start = token;
   sep(&token, "?");
   query = token ? token : empty;
   token = start;
   sep(&token, ":");
   *cmd = token && *token ? token : "none";
   token = start;
   sep(&token, "/");
   path = token ? token : empty;
   *route = start;

   char *a;
   r->nargs = 0;
   while ((a = sep(&query, "&"))) {
      char *name = sep(&a, "=");
      if (!name) {
         // An empty one ("&&", "?&"): newlib gives his url_decode a NULL name, which his
         // firmware survives (address 0 reads 0), and his errors print it as "(null)"
         set_arg(r, "(null)", "");
         continue;
      }
      url_decode(name);
      if (a)
         url_decode(a);
      set_arg(r, name, a ? a : "");
   }

   // Gideon's split_string, then each part decoded and the whole path put back together
   r->depth = 0;
   r->full_path[0] = 0;
   if (*path) {
      int len = strlen(path), n = 0;
      r->parts[n++] = path;
      for (int i = 0; i < len && n < PARTS_MAX; i++)
         if (path[i] == '/') {
            path[i] = 0;
            r->parts[n++] = path + i + 1;
         }
      int at = 0;
      for (int i = 0; i < n; i++) {
         url_decode(r->parts[i]);
         at += fmt_snprintf(r->full_path + at, sizeof r->full_path - at, i ? "/%s" : "%s", r->parts[i]);
         if (at >= (int)sizeof r->full_path)
            at = sizeof r->full_path - 1;
      }
      r->depth = n;
   }
   return true;
}

// Every argument must be one of the call's, and every required one must be there
static int validate(rest_t *r)
{
   const call_t *c = r->call;
   uint32_t matched = 0;
   int errors = 0;
   for (int i = 0; i < r->nargs; i++) {
      int j = 0;
      while (j < c->nparams && strcmp(r->keys[i], c->params[j].name))
         j++;
      if (j < c->nparams) {
         matched |= 1u << j;
      } else {
         error(r, "Function %s does not have parameter %s", c->cmd, r->keys[i]);
         errors++;
      }
   }
   for (int j = 0; j < c->nparams; j++)
      if (!(matched & 1u << j) && c->params[j].flags & P_REQUIRED) {
         error(r, "Function %s requires parameter %s", c->cmd, c->params[j].name);
         errors++;
      }
   return errors;
}

// --- the About routes (routes.cc) ---------------------------------------------------------

// A fixed page. Without its argument it never gets here: the arguments are checked first,
// so his own 400 page for that is never seen.
static void get_help(rest_t *r)
{
   html_response(r, HTTP_OK, "This function provides some help!", "Help text.");
}

static void get_version(rest_t *r)
{
   add_str(r, "version", "0.1");
   json_response(r, HTTP_OK);
}

// No core_version (his U64s only), so his tests take this for a U2: a cartridge's routes.
// The product is a name, as his; fpga_version is the hardware interface's, in his "1xx" form.
static void get_info(rest_t *r)
{
   char fpga[8], mac[18];
   const uint8_t *m = eth_netif.hwaddr;
   fmt_snprintf(fpga, sizeof fpga, "1%02x", M2M_RV_HW_VERSION);
   fmt_snprintf(mac, sizeof mac, "%02X:%02X:%02X:%02X:%02X:%02X", m[0], m[1], m[2], m[3], m[4], m[5]);
   add_str(r, "product", "C128MEGA65");
   add_str(r, "firmware_version", CORE_VERSION);
   add_str(r, "git_commit_hash", FW_VERSION);
   add_str(r, "fpga_version", fpga);
   add_str(r, "hostname", netif_get_hostname(&eth_netif));
   add_str(r, "unique_id", eth_unique_id);
   add_str(r, "ethernet_mac", mac);
   json_response(r, HTTP_OK);
}

static const param_t help_params[] = { { "command", P_REQUIRED } };

#define CALL(m, route, cmd, proc, params) { m, route, cmd, proc, params, sizeof params / sizeof params[0] }
#define CALL0(m, route, cmd, proc) { m, route, cmd, proc, NULL, 0 }

static const call_t calls[] = {
   CALL(HTTP_GET, "help", "none", get_help, help_params),
   CALL0(HTTP_GET, "version", "none", get_version),
   CALL0(HTTP_GET, "info", "none", get_info),
};

// --- middleware.c's Dispatch and routes.cc's execute_api_v1 -------------------------------

static const call_t *find_call(http_method_t m, const char *route, const char *cmd)
{
   for (unsigned i = 0; i < sizeof calls / sizeof calls[0]; i++)
      if (calls[i].method == m && !strcmp(calls[i].route, route) && !strcmp(calls[i].cmd, cmd))
         return &calls[i];
   return NULL;
}

void rest_dispatch(http_req_t *req, http_resp_t *res)
{
   if (!rests[req->slot])
      rests[req->slot] = heap_alloc(sizeof(rest_t));
   rest_t *r = rests[req->slot];
   char *route, *cmd;

   r->res = res;
   r->len = 0;
   r->elen = 0;
   r->first = true;
   r->call = parse_url(r, req->uri, &route, &cmd) ? find_call(req->method, route, cmd) : NULL;
   if (!r->call) {
      not_found(res);
      return;
   }

   // A password, once set, must come in X-Password (the last one counts)
   const char *given = NULL;
   for (unsigned i = 0; i < req->nfields; i++)
      if (!strcasecmp(req->fields[i].key, "X-Password"))
         given = req->fields[i].value;
   put(r, "{ \n");
   if (*password && (!given || strcmp(given, password))) {
      error(r, "Forbidden.");
      json_response(r, HTTP_FORBIDDEN);
      return;
   }
   if (validate(r)) {
      json_response(r, HTTP_BAD_REQUEST);
      return;
   }
   r->call->proc(r);
}
