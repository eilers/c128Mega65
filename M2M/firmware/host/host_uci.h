// Forced into the UCI firmware's sources for test_uci: its register accesses go to
// test_uci.c's model of command_protocol.vhd; the buffer is host_m2m.c's device memory.
#pragma once
#include "host_m2m.h"

unsigned host_uci_rd(unsigned reg);
void     host_uci_wr(unsigned reg, unsigned v);

#define UCI_RD(n)    host_uci_rd(n)
#define UCI_WR(n, v) host_uci_wr(n, v)
