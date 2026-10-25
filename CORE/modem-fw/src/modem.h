#pragma once

#define MODEM_LISTEN_PORT 6400

void modem_init(void);
void modem_poll(void);
void modem_say(const char *s);   // a line for the C128
