// Text on the On-Screen-Menu and the serial port.
#pragma once
#include <stdint.h>

#define OSM_DX 45
#define OSM_DY 33

void osm_clear(uint8_t attr);
void osm_print(int x, int y, uint8_t attr, const char *s);
void osm_hex(int x, int y, uint8_t attr, uint32_t v, int digits);

void uart_init(uint32_t baud);
void uart_putc(char c);
void uart_puts(const char *s);
void uart_hex(uint32_t v, int digits);

void trap_init(void);   // lib/trap.c
