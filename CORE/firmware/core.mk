# Included by M2M/firmware/Makefile: the C128 core's sources beyond CORE/firmware/*.c,
# relative to CORE/firmware. The network (net/) with lwIP 2.2.1 (net/lwip).
CORE_SRC    := $(patsubst $(CORE_DIR)/%,%,$(wildcard $(CORE_DIR)/net/*.c) \
               $(wildcard $(CORE_DIR)/net/lwip/core/*.c) $(wildcard $(CORE_DIR)/net/lwip/core/ipv4/*.c) \
               $(CORE_DIR)/net/lwip/netif/ethernet.c)
CORE_CFLAGS := -I$(CORE_DIR)/net -I$(CORE_DIR)/net/lwip/include -Wno-unused-parameter
