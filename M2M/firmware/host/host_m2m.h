// Forced into the firmware's sources for test_shell: the registers, the device bus and
// HyperRAM are memory here (host_m2m.c), and a halted firmware ends the test.
#pragma once
#include <stdint.h>

volatile uint32_t *host_reg32(uint32_t a);
volatile uint16_t *host_dev(void);       // the device in M2M_RAMROM_DEV, 2^28 words
volatile uint16_t *host_dev_at(uint32_t d);
extern void      (*host_dev_hook)(void); // called on every device access, to play hardware
void               host_halt(void) __attribute__((noreturn));
extern volatile uint16_t *host_hr;       // HyperRAM, 2^23 words

#define REG32(a)     (*host_reg32(a))
#define M2M_DEV16    (host_dev())
#define M2M_DEV32    ((volatile uint32_t *)host_dev())
#define M2M_HYPERRAM ((uintptr_t)host_hr)
#define M2M_HALT()   host_halt()
#define FW_HEAP      ((uintptr_t)host_hr + 0x500000)   // in the firmware's region (fw/heap.c)
#define FW_HEAP_END  ((uintptr_t)host_hr + 0x7FE000)
