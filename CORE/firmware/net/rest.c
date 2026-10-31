// The REST API on http.c: Gideon's software/api at 4b00732 (the II+L's 3.15a has the same
// core): routes.h's ArgsURI and ResponseWrapper, routes.cc's About routes, and
// MicroHttpServer's url.c and middleware.c. Answers match his to the byte: JSON as json.h
// renders it (no escaping, "{ \n", "[  ]"), his status texts (any code but 200, 203, 400,
// 403 and 404 goes out as "Unknown"), and a bare 404 for anything that isn't a route (no
// web pages here).
#include <ctype.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "config_ids.h"
#include "eth.h"
#include "fmt.h"
#include "heap.h"
#include "m2m_rv.h"
#include "multipart.h"
#include "rest_priv.h"
#include "version.h"

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

// --- the answer: ResponseWrapper and json.h ----------------------------------------------

static void putv(rest_t *r, const char *f, va_list ap)
{
   int room = BODY_SIZE - r->len;
   int n = fmt_vsnprintf(r->body + r->len, room, f, ap);
   if (n >= room)
      r->over = true;
   r->len += n < room ? n : room - 1;
}

void put(rest_t *r, const char *f, ...)
{
   va_list ap;
   va_start(ap, f);
   putv(r, f, ap);
   va_end(ap);
}

// His JSON_Object::render: "{ \n", each key on its own line two spaces in at any depth,
// "," between them, "\n}" after the last; an empty one is "{ \n}"
void key(rest_t *r, const char *k)
{
   put(r, r->first[r->level] ? "  \"%s\" : " : ",\n  \"%s\" : ", k);
   r->first[r->level] = false;
}

void add_str(rest_t *r, const char *k, const char *v)
{
   key(r, k);
   put(r, "\"%s\"", v);
}

void add_int(rest_t *r, const char *k, int v)
{
   key(r, k);
   put(r, "%d", v);
}

void obj_open(rest_t *r, const char *k)
{
   if (k)
      key(r, k);
   put(r, "{ \n");
   if (r->level < DEPTH_MAX - 1)
      r->level++;
   r->first[r->level] = true;
}

void obj_close(rest_t *r)
{
   put(r, r->first[r->level] ? "}" : "\n}");
   if (r->level)
      r->level--;
}

// JSON_List: "[ " items with ", " between " ]"
void list_open(rest_t *r, const char *k)
{
   key(r, k);
   put(r, "[ ");
}

void list_str(rest_t *r, int n, const char *v)
{
   put(r, n ? ", \"%s\"" : "\"%s\"", v);
}

void list_close(rest_t *r, int n)
{
   (void)n;
   put(r, " ]");
}

// An item for the errors list, cut at 199 characters as his
void error(rest_t *r, const char *f, ...)
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

// The answer's body, a buffer at a time; NULL: the connection went
static int body_out(void *ctx, uint8_t *buf, int size)
{
   rest_t *r = ctx;
   const uint8_t *from = r->bin ? r->bin : (const uint8_t *)r->body;
   int n = (r->bin ? r->bin_len : r->len) - r->pos;
   if (!buf)
      return 0;
   if (n > size)
      n = size;
   memcpy(buf, from + r->pos, n);
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
   r->bin = NULL;
   res->body_cb = body_out;
   res->body_ctx = r;
   res->index = fmt_snprintf((char *)res->buf, HTTP_BUFFER_SIZE,
                             "HTTP/1.1 %d %s\r\nConnection: close\r\nContent-Type: %s\r\n"
                             "Content-Length: %d\r\n\r\n", code, code_text(code), type, r->len);
}

void json_response(rest_t *r, int code)
{
   // His has no limit; cut-off JSON would be worse than an error
   if (r->over) {
      r->over = false;
      r->len = r->elen = 0;
      r->first[0] = true;
      put(r, "{ \n");
      error(r, "The answer is longer than %d KB.", BODY_SIZE / 1024);
      code = HTTP_INTERNAL_ERROR;
   }
   r->level = 0;
   key(r, "errors");
   put(r, r->elen ? "[ %s ]" : "[  ]", r->errors);
   obj_close(r);
   respond(r, code, "application/json");
}

void html_response(rest_t *r, int code, const char *title, const char *text)
{
   r->len = 0;
   put(r, "<html><body><h1>%s</h1>\n<p>%s</p></body></html>\r\n", title, text);
   respond(r, code, "text/html");
}

// His binary_response, with an attachment without a file name
void binary_response(rest_t *r, const uint8_t *data, int len)
{
   http_resp_t *res = r->res;
   r->pos = 0;
   r->bin = data;
   r->bin_len = len;
   res->body_cb = body_out;
   res->body_ctx = r;
   res->index = fmt_snprintf((char *)res->buf, HTTP_BUFFER_SIZE,
                             "HTTP/1.1 200 OK\r\nContent-Type: application/octet-stream\r\n"
                             "Content-Disposition: attachment\r\nContent-Length: %d\r\n"
                             "Connection: close\r\n\r\n", len);
}

// SubsysCommand's error_string and http_response_map
static const struct { const char *text; int code; } subsys[] = {
   { NULL, 200 },
   { "Invalid Parameter", 400 },
   { "Call requires user interaction, but no user interface object exists", 503 },
   { "Generic Error", 500 },
   { "Disk Error", 500 },
   { "SubSystem does not exist", 503 },
   { "SubSystem does not implement command executer", 405 },
   { "Could not obtain lock of subsystem", 423 },
   { "Disk has been modified, save first", 403 },
   { "Drive ROM not found", 412 },
   { "Drive ROM is invalid", 412 },
   { "This hardware only supports 1541", 405 },
   { "Drive is in the wrong mode", 415 },
   { "Cannot open file", 404 },
   { "Seek operation on file failed", 500 },
   { "Read operation on file failed", 500 },
   { "Illegal mount mode / drive type", 500 },
   { "Undefined subsystem command", 500 },
   { "Operation aborted by user", 500 },
   { "Save failed", 424 },
   { "EEPROM data is too large", 412 },
   { "EEPROM defined more than once", 412 },
   { "ROM image is too large", 412 },
   { "Error detected in file format", 415 },
   { "This command is not supported on this architecture", 501 },
   { "Out of memory", 507 },
   { "MegaPatch3: Invalid image size", 400 },
   { "MegaPatch3: DNP Image too large", 400 },
   { "Internal Error", 500 },
};

void subsys_response(rest_t *r, int ss)
{
   if (subsys[ss].text)
      error(r, "%s", subsys[ss].text);
   json_response(r, subsys[ss].code);
}

static void not_found(http_resp_t *res)
{
   res->body_cb = NULL;
   res->index = fmt_snprintf((char *)res->buf, HTTP_BUFFER_SIZE,
                             "HTTP/1.1 404 Not Found\r\nConnection: close\r\n\r\n");
}

// An answer that comes from a route's step, run once a main-loop round until it answers.
// http.c asks for the body; -1 says not yet.
static int pending_out(void *ctx, uint8_t *buf, int size)
{
   rest_t *r = ctx;
   (void)size;
   if (!buf) {
      if (r->cancel)
         r->cancel(r);
      return 0;
   }
   if (!r->step(r))
      return -1;
   return r->res->index;                // the step answered: its header first
}

void rest_pending(rest_t *r, int (*step)(rest_t *r), void (*cancel)(rest_t *r))
{
   r->step = step;
   r->cancel = cancel;
   r->res->body_cb = pending_out;
   r->res->body_ctx = r;
   r->res->index = 0;
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

const char *arg(rest_t *r, const char *k)
{
   for (int i = 0; i < r->nargs; i++)
      if (!strcmp(r->keys[i], k))
         return r->vals[i];
   return NULL;
}

int arg_int(rest_t *r, const char *k, int alt)
{
   const char *v = arg(r, k);
   return v ? (int)strtol(v, NULL, 0) : alt;
}

// His pattern_match: * any run of characters, ? any one, any case
bool pattern_match(const char *p, const char *f)
{
   const char *star_p = NULL, *star_f = NULL;
   for (;;) {
      if (!*f) {
         while (*p == '*')
            p++;
         if (!*p)
            return true;
      } else if (*p == '*') {
         p++;
         if (!*p)
            return true;
         star_p = p;
         star_f = f;
         continue;
      } else if (*p == '?' || toupper((unsigned char)*p) == toupper((unsigned char)*f)) {
         f++;
         p++;
         continue;
      }
      if (!star_p || !*star_f)
         return false;
      p = star_p;
      f = ++star_f;
   }
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

static const call_t about_calls[] = {
   CALL(HTTP_GET, "help", "none", get_help, help_params, 0),
   CALL0(HTTP_GET, "version", "none", get_version),
   CALL0(HTTP_GET, "info", "none", get_info),
};

// --- middleware.c's Dispatch and routes.cc's execute_api_v1 -------------------------------

static const call_t *find_in(const call_t *c, int n, http_method_t m, const char *route, const char *cmd)
{
   for (int i = 0; i < n; i++)
      if (c[i].method == m && !strcmp(c[i].route, route) && !strcmp(c[i].cmd, cmd))
         return &c[i];
   return NULL;
}

static const call_t *find_call(http_method_t m, const char *route, const char *cmd)
{
   const call_t *c = find_in(about_calls, sizeof about_calls / sizeof about_calls[0], m, route, cmd);
   if (!c)
      c = find_in(rest_machine_calls, rest_machine_count, m, route, cmd);
   if (!c)
      c = find_in(rest_configs_calls, rest_configs_count, m, route, cmd);
   if (!c)
      c = find_in(rest_drives_calls, rest_drives_count, m, route, cmd);
   if (!c)
      c = find_in(rest_files_calls, rest_files_count, m, route, cmd);
   if (!c)
      c = find_in(rest_runners_calls, rest_runners_count, m, route, cmd);
   return c;
}

static void run(rest_t *r)
{
   if (validate(r)) {
      json_response(r, HTTP_BAD_REQUEST);
      return;
   }
   r->call->proc(r);
}

// A part's file name, from its Content-Disposition's filename="..." (his TempfileWriter:
// the field's first 127 characters, no leading '.', '/' or '\\', fix_filename's '_' for
// the characters a file name can't have)
static void part_name(rest_t *r, const char *hdr)
{
   for (const char *l = hdr; *l; l += strcspn(l, "\n"), l += *l == '\n') {
      char value[128];
      while (*l == '\r' || *l == '\n')
         l++;
      int k = strcspn(l, ":\r\n");
      if (l[k] != ':' || k != 19 || strncasecmp(l, "Content-Disposition", 19))
         continue;
      const char *v = l + k + 1;
      while (*v == ' ')
         v++;
      int n = strcspn(v, "\r\n");
      if (n > (int)sizeof value - 1)
         n = sizeof value - 1;
      memcpy(value, v, n);
      value[n] = 0;
      char *f = strstr(value, "filename=\"");
      if (!f)
         continue;
      f += 10;
      char *q = strchr(f, '"');
      if (q)
         *q = 0;
      while (*f == '.' || *f == '\\' || *f == '/')
         f++;
      for (char *c = f; *c; c++)
         if (strchr("\"*:/<>\\?|,\x7F", *c))
            *c = '_';
      fmt_snprintf(r->up_name, sizeof r->up_name, "%s", f);
   }
}

// His TempfileWriter: the first part of the body, as much as the call keeps, and its size,
// or the call's sink takes it as it comes; the call runs once the body is in (writer_complete)
static void upload_event(void *ctx, mp_event_t ev, const uint8_t *data, int len)
{
   rest_t *r = ctx;
   switch (ev) {
   case MP_DATA_START:
   case MP_SUBHEADER:
      if (++r->up_parts == 1) {
         if (ev == MP_SUBHEADER)
            part_name(r, (const char *)data);
         if (r->call->sink)
            r->call->sink(r, ev, data, len);
      }
      break;
   case MP_DATA:
      if (r->up_parts == 1 && r->call->sink) {
         r->up_size += len;
         r->call->sink(r, ev, data, len);
      } else if (r->up_parts == 1) {
         int room = r->call->upload - r->up_len;
         memcpy(r->up + r->up_len, data, len < room ? len : room);
         r->up_len += len < room ? len : room;
         r->up_size += len;
      }
      break;
   case MP_DATA_END:
      if (r->up_parts == 1 && r->call->sink)
         r->call->sink(r, ev, data, len);
      break;
   case MP_ABORT:
      if (r->call->sink)
         r->call->sink(r, ev, data, len);
      break;
   case MP_END:                            // a sink's part may have no MP_DATA_END: his eTerminate
      if (r->up_parts >= 1 && r->call->sink)
         r->call->sink(r, ev, data, len);
      run(r);
      break;
   default:
      break;
   }
}

static int upload_in(void *ctx, const uint8_t *data, int size)
{
   rest_t *r = ctx;
   mp_in(r->mp, data, size);
   return size;
}

void rest_dispatch(http_req_t *req, http_resp_t *res)
{
   if (!rests[req->slot])
      rests[req->slot] = heap_alloc(sizeof(rest_t));
   rest_t *r = rests[req->slot];
   char *route, *cmd;

   r->req = req;
   r->res = res;
   r->len = 0;
   r->over = false;
   r->elen = 0;
   r->level = 0;
   r->first[0] = true;
   r->bin = NULL;
   r->step = NULL;
   r->cancel = NULL;
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
   if (!r->call->upload) {
      run(r);
      return;
   }
   if (req->body_type == BODY_NONE) {
      error(r, "Expected Body, but got none.");
      json_response(r, HTTP_PRECONDITION);
      return;
   }
   if (!r->up)
      r->up = heap_alloc(UPLOAD_MAX + 1);
   if (!r->mp)
      r->mp = mp_new();
   r->up_len = r->up_size = r->up_parts = 0;
   r->up_name[0] = 0;
   r->up_err = 0;
   r->up_at = 0;
   req->body_cb = upload_in;
   req->body_ctx = r;
   mp_start(r->mp, req->content_type, upload_event, r);
}
