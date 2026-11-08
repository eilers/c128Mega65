// test_http's registers and device bus (host_rest.c): M2M_RAMROM_DEV picks the device that
// M2M_DEV16 shows, the drive ROM or the cartridge's memory
#pragma once
#include <stdint.h>

volatile uint32_t *host_rest_reg32(uint32_t a);
volatile uint16_t *host_rest_dev(void);
#define REG32(a)  (*host_rest_reg32(a))
#define M2M_DEV16 (host_rest_dev())
