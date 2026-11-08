// A small printf: %c %s %d %u %x %X and %%, with a width and the flags '-' and '0'.
#pragma once
#include <stdarg.h>
#include <stddef.h>

int fmt_vsnprintf(char *buf, size_t size, const char *f, va_list ap);
int fmt_snprintf(char *buf, size_t size, const char *f, ...);
void uart_printf(const char *f, ...);   // up to 160 characters
