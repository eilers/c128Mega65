// The REST API's configs routes: Gideon's software/api/route_configs.cc at 4b00732 (the
// II+L's 3.15a has the same, but for the order of an enumeration's values, which is the
// same here) on the core's stores (stores.c), with his texts and his JSON layout. A change
// is in effect at once and lives in memory until save_to_flash, as his: the menu's items
// then go into the menu's settings file, the rest into the settings file.
// Ported from the 1541 Ultimate firmware (github.com/GideonZ/1541ultimate), Copyright
// Gideon Zweijtzer and contributors, GPL v3 as this core; changed for c128Mega65 in 2026.
#include <stdlib.h>
#include <string.h>
#include "core.h"
#include "fmt.h"
#include "heap.h"
#include "netcfg.h"
#include "rest_priv.h"
#include "rompick.h"
#include "stores.h"
#define JSMN_STATIC
#define JSMN_STRICT 1
#include "jsmn.h"

#define JSON_MAX   32768                // his buffer_file(0, 32768)
#define TOKENS_MAX 1024                 // his convert_text_to_json_objects(..., 1024, ...)

static jsmntok_t *tokens;

// --- reading ------------------------------------------------------------------------------

static void current(rest_t *r, const item_t *it, const char *k)
{
   if (it->type == IT_ENUM)
      add_str(r, k, it->choices[item_get(it)]);
   else if (it->type == IT_VALUE)
      add_int(r, k, item_get(it));
   else
      add_str(r, k, item_text(it));
}

typedef struct { rest_t *r; int n; } presets_t;

static void preset(const char *name, void *ctx)
{
   presets_t *ps = ctx;
   list_str(ps->r, ps->n++, name);
}

// His emit_store: a store's items, or with an item pattern each matching one in full
static void emit_store(rest_t *r, int s, const char *pattern)
{
   obj_open(r, store_name(s));
   for (int i = 0; i < stores_count; i++) {
      const item_t *it = &stores_items[i];
      if (it->store != s)
         continue;
      if (!pattern) {
         current(r, it, it->name);
         continue;
      }
      if (!pattern_match(pattern, it->name))
         continue;
      obj_open(r, it->name);
      current(r, it, "current");
      if (it->type == IT_ENUM) {
         int n = item_choices(it);
         list_open(r, "values");
         for (int j = 0; j < n; j++)
            list_str(r, j, it->choices[j]);
         list_close(r, n);
         add_str(r, "default", it->choices[it->def < n ? it->def : 0]);
      } else if (it->type == IT_VALUE) {
         add_int(r, "min", it->min);
         add_int(r, "max", it->max);
         add_str(r, "format", "%d");
         add_int(r, "default", it->def);
      } else if (it->type == IT_FILE) {
         // His preset functions: the files of the right kind, after an empty one where that
         // is the default
         presets_t ps = { r, 0 };
         list_open(r, "presets");
         if (!*it->sdef)
            list_str(r, ps.n++, "");
         rompick_list(item_pick(it), preset, &ps);
         list_close(r, ps.n);
         add_str(r, "default", it->sdef);
      } else {
         add_str(r, "default", it->sdef);
      }
      obj_close(r);
   }
   obj_close(r);
}

static void get_configs(rest_t *r)
{
   if (!r->depth) {
      list_open(r, "categories");
      for (int s = 0; s < ST_COUNT; s++)
         list_str(r, s, store_name(s));
      list_close(r, ST_COUNT);
      json_response(r, HTTP_OK);
      return;
   }
   int matched = 0;
   for (int s = 0; s < ST_COUNT; s++)
      if (pattern_match(r->parts[0], store_name(s))) {
         emit_store(r, s, r->depth < 2 ? NULL : r->parts[1]);
         matched = 1;
      }
   if (!matched) {
      error(r, "No configuration category matches '%s'.", r->parts[0]);
      json_response(r, HTTP_NOT_FOUND);
      return;
   }
   json_response(r, HTTP_OK);
}

// --- changing -----------------------------------------------------------------------------

// His set_item, wanted: 0 with an error
static int set_item(rest_t *r, const item_t *it, const char *v)
{
   int n;
   switch (stores_want(it, v, 0, &n)) {
   case WANT_OK:
      return 1;
   case WANT_RANGE:
      error(r, "Value %d is outside of the allowable range (%d-%d)", n, it->min, it->max);
      return 0;
   }
   error(r, "Value '%s' is not a valid choice for item %s", v, it->name);
   return 0;
}

static void put_configs(rest_t *r)
{
   const char *v = arg(r, "value");
   int s;
   if (v && r->depth != 2) {
      error(r, "When using the 'value' argument, the path should have two elements; the config "
               "category and the config item name. Given: '%s'", r->full_path);
      json_response(r, HTTP_BAD_REQUEST);
      return;
   }
   if (!v && r->depth != 3) {
      error(r, "When not using the 'value' argument, the path should have three elements; the "
               "config category and the config item name, followed by the setting. Given: '%s'",
            r->full_path);
      json_response(r, HTTP_BAD_REQUEST);
      return;
   }
   if (!v)
      v = r->parts[2];
   for (s = 0; s < ST_COUNT && !pattern_match(r->parts[0], store_name(s)); s++)
      ;
   if (s == ST_COUNT) {
      error(r, "There is no config category that matches '%s'", r->parts[0]);
      json_response(r, HTTP_NOT_FOUND);
      return;
   }
   const item_t *it = NULL;
   for (int i = 0; i < stores_count && !it; i++)
      if (stores_items[i].store == s && pattern_match(r->parts[1], stores_items[i].name))
         it = &stores_items[i];
   if (!it) {
      error(r, "Item '%s' not found in this configuration category [%s].", r->parts[1], store_name(s));
      json_response(r, HTTP_NOT_FOUND);
      return;
   }
   stores_begin();
   if (!set_item(r, it, v)) {
      json_response(r, HTTP_BAD_REQUEST);
      return;
   }
   stores_apply();
   json_response(r, HTTP_OK);
}

// A JSON value as his convert() makes it: a string's text as it stands (no escapes undone),
// true and false, a number by strtol, base 0 (null is 0)
static const char *literal(char *text, const jsmntok_t *t, char *num, int *kind)
{
   const char *v = text + t->start;
   *kind = t->type;
   if (t->type == JSMN_PRIMITIVE) {
      if (*v == 't' || *v == 'f')
         return NULL;
      fmt_snprintf(num, 16, "%d", (int)strtol(v, NULL, 0));
      return num;
   }
   return v;
}

// The tokens after t and all inside it
static int skip(const jsmntok_t *t, int n, int i)
{
   int end = i + 1;
   while (end < n && tokens[end].start < t[i].end)
      end++;
   return end;
}

// His convert(): a token he can't place makes the whole text unparsed
static int convertible(int n)
{
   for (int i = 0; i < n; i++) {
      const jsmntok_t *t = &tokens[i];
      if (t->start < 0 || t->end < t->start)
         return 0;
      if (t->parent < 0)
         continue;
      const jsmntok_t *p = &tokens[t->parent];
      if (p->type == JSMN_STRING && tokens[p->parent].type != JSMN_OBJECT)
         return 0;
      if (p->type == JSMN_OBJECT && t->type != JSMN_STRING)
         return 0;
   }
   return 1;
}

// His apply_config: every store and item in the body is tried, whatever the others do
static int apply_config(rest_t *r, char *text, int n)
{
   char num[16];
   int good = 1, kind;
   if (tokens[0].type != JSMN_OBJECT) {
      error(r, "Root is not an object.");
      return 0;
   }
   for (int i = 1; i < n; ) {
      const jsmntok_t *k = &tokens[i], *v = &tokens[i + 1];
      const char *name = text + k->start;
      int s = store_find(name), next = skip(tokens, n, i + 1);
      if (s < 0) {
         error(r, "'%s' is not a valid configuration category name.", name);
         good = 0;
      } else if (v->type != JSMN_OBJECT) {
         error(r, "Element '%s' should be an object.", name);
         good = 0;
      } else {
         stores_begin();
         for (int j = i + 2; j < next; ) {
            const jsmntok_t *ik = &tokens[j], *iv = &tokens[j + 1];
            const char *iname = text + ik->start;
            const item_t *it = NULL;
            for (int x = 0; x < stores_count && !it; x++)
               if (stores_items[x].store == s && !strcasecmp(iname, stores_items[x].name))
                  it = &stores_items[x];
            const char *val = literal(text, iv, num, &kind);
            if (!it) {
               error(r, "'%s' is not a valid item name in category '%s'.", iname, store_name(s));
               good = 0;
            } else if (kind == JSMN_OBJECT || kind == JSMN_ARRAY) {
               error(r, "Value given for '%s' should be a literal.", iname);
               good = 0;
            } else if (!val) {
               error(r, "Value given for '%s' cannot be of type boolean.", iname);
               good = 0;
            } else {
               good &= set_item(r, it, val);
            }
            j = skip(tokens, n, j + 1);
         }
         stores_apply();
      }
      i = next;
   }
   return good;
}

static void post_configs(rest_t *r)
{
   const char *type = r->req->content_type;
   if (!type || strcasecmp(type, "application/json")) {
      error(r, "Content type should be 'application/json'.");
      json_response(r, HTTP_BAD_REQUEST);
      return;
   }
   if (!r->up_parts || !r->up_size || r->up_size > JSON_MAX) {
      error(r, "Could not buffer attachment.");
      json_response(r, HTTP_INTERNAL_ERROR);
      return;
   }
   if (!tokens)
      tokens = heap_alloc(TOKENS_MAX * sizeof *tokens);
   char *text = (char *)r->up;
   text[r->up_len] = 0;
   jsmn_parser p;
   jsmn_init(&p);
   int n = jsmn_parse(&p, text, r->up_len, tokens, TOKENS_MAX);
   if (n >= 0 && (!n || !convertible(n)))
      n = JSMN_ERROR_ILLEGAL;
   if (n < 0) {
      error(r, "JSON content could not be parsed. Error: %d", n);
      json_response(r, HTTP_BAD_REQUEST);
      return;
   }
   for (int i = 0; i < n; i++)
      text[tokens[i].end] = 0;
   json_response(r, apply_config(r, text, n) ? HTTP_OK : HTTP_BAD_REQUEST);
}

// --- flash --------------------------------------------------------------------------------

static int depth_ok(rest_t *r)
{
   if (r->depth <= 1)
      return 1;
   error(r, "Path depth exceeds 1.");
   json_response(r, HTTP_BAD_REQUEST);
   return 0;
}

static int matches(rest_t *r, int s)
{
   return !r->depth || pattern_match(r->parts[0], store_name(s));
}

static void put_load(rest_t *r)
{
   if (!depth_ok(r))
      return;
   if (r->depth)
      list_open(r, "loaded");
   int n = 0;
   for (int s = 0; s < ST_COUNT; s++)
      if (matches(r, s)) {
         stores_begin();
         stores_want_saved(s);
         stores_apply();
         stores_saved(s);
         if (r->depth)
            list_str(r, n, store_name(s));
         n++;
      }
   if (r->depth)
      list_close(r, n);
   json_response(r, HTTP_OK);
}

// Only stores with changes, as his is_flash_stale; then the menu's settings file and the
// settings file, both written whole
static void put_save(rest_t *r)
{
   if (!depth_ok(r))
      return;
   int n = 0;
   list_open(r, "written");
   for (int s = 0; s < ST_COUNT; s++)
      if (matches(r, s) && stores_stale(s)) {
         stores_saved(s);
         list_str(r, n++, store_name(s));
      }
   list_close(r, n);
   if (n) {
      shell_save_settings();
      stores_save();
   }
   json_response(r, HTTP_OK);
}

static void put_reset(rest_t *r)
{
   if (!depth_ok(r))
      return;
   if (r->depth)
      list_open(r, "reset");
   int n = 0;
   for (int s = 0; s < ST_COUNT; s++)
      if (matches(r, s)) {
         stores_begin();
         stores_want_defaults(s);
         stores_apply();
         if (r->depth)
            list_str(r, n, store_name(s));
         n++;
      }
   if (r->depth)
      list_close(r, n);
   json_response(r, HTTP_OK);
}

static const param_t value_params[] = { { "value", 0 } };

const call_t rest_configs_calls[] = {
   CALL0(HTTP_GET, "configs", "none", get_configs),
   CALL(HTTP_PUT, "configs", "none", put_configs, value_params, 0),
   CALLU(HTTP_POST, "configs", "none", post_configs, JSON_MAX),
   CALL0(HTTP_PUT, "configs", "load_from_flash", put_load),
   CALL0(HTTP_PUT, "configs", "save_to_flash", put_save),
   CALL0(HTTP_PUT, "configs", "reset_to_default", put_reset),
};
const int rest_configs_count = sizeof rest_configs_calls / sizeof rest_configs_calls[0];
