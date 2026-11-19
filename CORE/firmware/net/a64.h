// Assembly 64's server (hackerswithstyle.se): the search form's presets, a search, a
// title's files, and a file. One request at a time; a64_poll, from the main loop, moves it
// on. A JSON answer ends up in buf with a 0 after it, a file on the card under its name
// once all of it is there.
#pragma once
#include <stdint.h>

enum { A64_IDLE, A64_BUSY, A64_DONE, A64_FAILED };

#define A64_PAGE 50                     // results a search asks for at a time

void a64_presets(char *buf, int size);
// query as the server's AQL has it, "(name:"x") & (type:d64)"; results from number offset on
void a64_search(const char *query, int offset, char *buf, int size);
void a64_entries(const char *id, int cat, char *buf, int size);
void a64_binary(const char *id, int cat, int idx, const char *file);

int         a64_poll(void);             // the request's state
void        a64_cancel(void);
int         a64_got(void);              // the answer's bytes so far
int         a64_total(void);            // of how many, or -1 if the server didn't say
const char *a64_error(void);            // why it failed, a line for the screen

// Letters, digits and _ - . * as they are, the rest %xx. 0 if out is too short.
int         a64_url_encode(const char *s, char *out, int size);
