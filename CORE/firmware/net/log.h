#pragma once
#include <stddef.h>
#include <stdint.h>

// Debug log in a ring buffer. ATI2 prints it on the C128.
void log_printf(const char *f, ...);

// Copies log text from *pos on (0 = oldest kept) and advances *pos. Returns the length.
size_t log_copy(uint32_t *pos, char *out, size_t size);
