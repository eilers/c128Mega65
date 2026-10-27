// Text on the On-Screen-Menu and the serial port.
#pragma once
#include <stdint.h>

#define OSM_DX 45
#define OSM_DY 33

void osm_clear(uint8_t attr);
void osm_print(int x, int y, uint8_t attr, const char *s);
void osm_hex(int x, int y, uint8_t attr, uint32_t v, int digits);
void osm_printn(int x, int y, uint8_t attr, const char *s, int width);   // cut or padded to width
void osm_frame(int x, int y, int dx, int dy, uint8_t attr);              // a double-line box
void osm_hline(int x, int y, int dx, uint8_t attr);                      // a single line across a box

void uart_init(uint32_t baud);
void uart_putc(char c);
int  uart_getc(void);   // -1 when nothing came
void uart_puts(const char *s);
void uart_hex(uint32_t v, int digits);

void trap_init(void);   // lib/trap.c
