#pragma once
#include <stdint.h>

// HyperRAM load test (AT*H): mode 1 misses the i-cache, mode 2 reads constants.
// Runs for the given seconds with nothing else going on (the whole firmware waits);
// returns HyperRAM reads made.
uint32_t hog(int mode, uint32_t seconds);
