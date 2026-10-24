#pragma once
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// Small printf: %s %c %d %u %x with optional width and zero padding. Returns the length.
int vfmt(char *buf, size_t size, const char *f, va_list ap);
int fmt(char *buf, size_t size, const char *f, ...);

uint32_t millis(void);
uint32_t rand32(void);
