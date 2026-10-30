// A model of reu_sd.vhd for the host tests: the REU's SD card DMA (the card's blocks
// straight to and from the REU's memory, at once) and its cache drop, counted.
#pragma once

struct reu_sd_model {
   unsigned loads, saves, blocks;  // transfers done, and their blocks
   unsigned drops;                 // the cache's line dropped: the CPU asked, or a load
   unsigned fail_at;               // the n-th transfer from now fails (0: none)
};
extern struct reu_sd_model rs;

void host_reu_sd(int on);          // the device there (its magic reads 0x5D52) or not
