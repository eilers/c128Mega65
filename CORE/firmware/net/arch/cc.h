// lwIP port glue for the system firmware
#pragma once
#include <stdint.h>
#include "../log.h"
#include "../net.h"

#define BYTE_ORDER LITTLE_ENDIAN

#define LWIP_RAND() net_rand()

// Asserts say where, not what: the message strings would cost about 6 KB
#define LWIP_PLATFORM_DIAG(x)   do { log_printf x; } while (0)
#define LWIP_PLATFORM_ASSERT(x) do { log_printf("assert %s:%d", __FILE_NAME__, __LINE__); } while (0)
