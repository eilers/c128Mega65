// Forced into boot/load.c on the host: the firmware's HyperRAM region is a buffer here.
#include <stdint.h>

extern uint8_t host_hr[];
#define FW_MEM(a) ((void *)(host_hr + ((a) - 0x40400000u)))
