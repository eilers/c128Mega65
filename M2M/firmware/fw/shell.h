// The Shell (QNICE's shell.asm): starts the core with its ROMs and settings, then runs
// the options menu when Help (or F13) is pressed.
#pragma once
#include <stdint.h>

void shell_start(void);    // returns once the core runs
void shell_poll(void);
void shell_fatal(const char *msg, uint32_t code) __attribute__((noreturn));
