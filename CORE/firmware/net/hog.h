#pragma once
#include <stdint.h>

// HyperRAM load test (AT*H): mode 1 misses the i-cache, mode 2 reads constants.
// Runs for the given seconds with nothing else going on (the whole firmware waits);
// returns the reads made: cache lines (mode 1) or 32-bit words (mode 2).
uint32_t hog(int mode, uint32_t seconds);
