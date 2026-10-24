// lwIP port glue for the modem's NEORV32
#pragma once
#include <stdint.h>
#include "../log.h"
#include "../util.h"

#define BYTE_ORDER LITTLE_ENDIAN

#define LWIP_RAND() rand32()

#define LWIP_PLATFORM_DIAG(x)   do { log_printf x; } while (0)
#define LWIP_PLATFORM_ASSERT(x) do { log_printf("assert %s %s:%d", x, __FILE__, __LINE__); } while (0)
