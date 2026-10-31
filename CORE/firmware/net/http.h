// The REST API's HTTP server: MicroHttpServer's rules (Gideon's software/httpd at ad73d32),
// on lwIP's raw API. Four connections, a 2 KB header, a body only on POST, Connection:
// close. The requests run from the main loop, so a route may use the card.
#pragma once
#include <stddef.h>
#include <stdint.h>

#define HTTP_CLIENTS         4
#define HTTP_MAX_HEADER_SIZE 2048
#define HTTP_BUFFER_SIZE     1024
#define HTTP_MAX_FIELDS      20

typedef enum { HTTP_UNKNOWN, HTTP_GET, HTTP_POST, HTTP_PUT, HTTP_DELETE } http_method_t;
typedef enum { BODY_NONE, BODY_UNTIL_DISCONNECT, BODY_SIZE, BODY_CHUNKED } http_body_t;

// A body's bytes for the route: size 0 ends it, -1 says the connection went first
typedef int (*http_in_t)(void *ctx, const uint8_t *data, int size);
// The response's body, a buffer at a time: 0 when there is no more, -1 for nothing yet (the
// route's work goes on: asked again next round). buf NULL: the connection went, let go.
typedef int (*http_out_t)(void *ctx, uint8_t *buf, int size);

typedef struct {
   const char *key, *value;
} http_field_t;

typedef struct {
   int           slot;                     // the connection, 0..HTTP_CLIENTS-1
   int           state;
   char          hdr[HTTP_MAX_HEADER_SIZE + 4];
   int           hdr_valid;
   http_method_t method;
   const char   *uri, *version;
   http_field_t  fields[HTTP_MAX_FIELDS];
   unsigned      nfields;
   const char   *content_type;
   http_in_t     body_cb;                  // set by the route, or the body is dropped
   void         *body_ctx;
   size_t        body_size;
   http_body_t   body_type;
   int           chunk_state;
   size_t        chunk_remain;
   uint8_t       buf[HTTP_BUFFER_SIZE + 4];
   int           valid, used;
} http_req_t;

typedef struct {
   http_out_t body_cb;
   void      *body_ctx;
   size_t     index;                       // bytes in buf: the status line and the header
   uint8_t    buf[HTTP_BUFFER_SIZE + 4];
} http_resp_t;

void http_start(void);   // from the network's start
void http_listen(int on); // port 80 open or closed (the menu's REST API item), in a turn or under net_lock
void http_poll(void);    // from the main loop

// The application (rest.c), called once a request's header is in: fills in resp, and maybe
// a body_cb in req for a POST's body (then resp is filled in when the body ends)
void rest_dispatch(http_req_t *req, http_resp_t *resp);
// X-Password, "" for none
void rest_set_password(const char *pw);
