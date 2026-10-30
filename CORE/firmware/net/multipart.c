// A port of MicroHttpServer's c-version/lib/multipart.c at ad73d32 (Gideon's software/httpd;
// Copyright (c) 2015, StarNight; BSD licence in MicroHttpServer-LICENSE.md): the same state
// machine, so a body splits into the same parts. Its part headers aren't parsed here: the
// core keeps no file names.
//
// A multipart form is (--, SEPARATOR, \r\n, (HEADER FIELD, \r\n)+, \r\n, DATA)+, --,
// SEPARATOR, \r\n. With the separator searched for as \r\n--SEP, that is (SEPARATOR,
// HEADER, EMPTY LINE, DATA)+, SEPARATOR, searched for on the fly.
#include <stddef.h>
#include <string.h>
#include <strings.h>
#include "heap.h"
#include "multipart.h"

typedef enum { S_INIT, S_BINARY, S_DITCH, S_HEADER, S_DATA, S_TERMINATED } state_t;

typedef struct {
   state_t  state;
   char     boundary[80];
   int      boundary_length;
   int      match_state;
   int      header_size;
   uint8_t  data[4096];
   int      data_size;
   mp_cb_t  cb;
   void    *ctx;
} stream_t;

void *mp_new(void)
{
   return heap_alloc(sizeof(stream_t));
}

static void event(stream_t *s, mp_event_t ev, const uint8_t *data, int len)
{
   s->cb(s->ctx, ev, data, len);
}

static void expunge(stream_t *s)
{
   event(s, MP_DATA, s->data, s->data_size);
   s->data_size = 0;
}

static void keep(stream_t *s, uint8_t b)
{
   s->data[s->data_size++] = b;
   if (s->data_size == (int)sizeof s->data)
      expunge(s);
}

void mp_start(void *mp, const char *type, mp_cb_t cb, void *ctx)
{
   stream_t *s = mp;
   const char *b;
   memset(s, 0, offsetof(stream_t, data));
   s->data_size = 0;
   s->cb = cb;
   s->ctx = ctx;
   if (!type)
      type = "";
   event(s, MP_START, (const uint8_t *)type, strlen(type));
   // "boundary=" after "multipart"; a boundary too long for his malloc'd copy can't happen
   // here, so a longer one than ours is taken as no boundary: the body is one raw part
   if (!strncasecmp(type, "multipart", 9) && (b = strstr(type, "boundary=")) &&
       strlen(b + 9) + 5 <= sizeof s->boundary) {
      strcpy(s->boundary, "\r\n--");
      strcpy(s->boundary + 4, b + 9);
      s->boundary_length = strlen(s->boundary);
      s->match_state = 2;                 // the body starts with a boundary, without the \r\n
      s->state = S_DITCH;
      return;
   }
   event(s, MP_DATA_START, (const uint8_t *)type, strlen(type));
   s->state = S_BINARY;
}

void mp_in(void *mp, const uint8_t *buf, int len)
{
   static const char header_end[5] = "\r\n\r\n";
   stream_t *s = mp;

   if (len < 0) {
      event(s, MP_ABORT, NULL, 0);
      s->state = S_TERMINATED;
      return;
   }
   switch (s->state) {
   case S_BINARY:
      if (len == 0) {
         event(s, MP_DATA_END, NULL, 0);
         event(s, MP_END, NULL, 0);
         s->state = S_TERMINATED;
      } else {
         event(s, MP_DATA, buf, len);
      }
      break;

   case S_DITCH:
   case S_DATA:
   case S_HEADER:
      for (int i = 0; i < len; i++) {
         if (s->state != S_HEADER) {
            if (s->match_state == s->boundary_length) {
               s->match_state = 0;
               if (s->data_size)
                  expunge(s);
               if (s->state == S_DATA)
                  event(s, MP_DATA_END, NULL, 0);
               // After a separator there is always a header
               s->state = S_HEADER;
               s->header_size = 0;
            } else if (buf[i] == (uint8_t)s->boundary[s->match_state]) {
               s->match_state++;
            } else if (s->match_state != 0) {
               // Not a separator after all: its bytes so far were data
               if (s->state == S_DATA)
                  for (int j = 0; j < s->match_state; j++)
                     keep(s, s->boundary[j]);
               s->match_state = buf[i] == (uint8_t)s->boundary[0];
            }
         } else {
            if (s->match_state == 4) {
               // After the header's empty line there is always data
               s->match_state = 0;
               event(s, MP_SUBHEADER, NULL, 0);
               s->state = S_DATA;
               s->header_size = 0;
            } else if (buf[i] == (uint8_t)header_end[s->match_state]) {
               s->match_state++;
            } else {
               s->match_state = 0;
            }
         }
         if (s->state == S_DATA && s->match_state == 0)
            keep(s, buf[i]);
      }
      if (len == 0) {
         s->state = S_TERMINATED;
         if (s->data_size) {
            expunge(s);
            event(s, MP_DATA_END, NULL, 0);
         }
         event(s, MP_END, NULL, 0);
      }
      break;

   default:
      break;
   }
}
