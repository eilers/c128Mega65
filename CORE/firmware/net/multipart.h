// A POST's body as MicroHttpServer's multipart.c splits it (Gideon's software/httpd at
// ad73d32): a multipart/form-data body into its parts, anything else as one raw part.
#pragma once
#include <stdint.h>

typedef enum { MP_START, MP_DATA_START, MP_SUBHEADER, MP_DATA, MP_DATA_END, MP_END, MP_ABORT } mp_event_t;

typedef void (*mp_cb_t)(void *ctx, mp_event_t ev, const uint8_t *data, int len);

void *mp_new(void);                       // a parser's state, about 5 KB of HyperRAM
void  mp_start(void *mp, const char *content_type, mp_cb_t cb, void *ctx);
// The body's bytes; len 0 ends it, len < 0 says the connection went first
void  mp_in(void *mp, const uint8_t *buf, int len);
