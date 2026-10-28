// HyperRAM after the firmware's image, for what the 16 KB data RAM can't hold: handed out
// once and kept. No cache: slow, but fine for the menu's text and the file browser.
#pragma once
#include <stdint.h>

void *heap_alloc(uint32_t bytes);
