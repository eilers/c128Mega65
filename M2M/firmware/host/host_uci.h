// Forced into the UCI firmware's sources for test_uci and test_prg: its register accesses go
// to host_uci_model.c's model of uci.vhd; the buffer is host_m2m.c's device memory.
#pragma once
#include "host_m2m.h"

unsigned host_uci_rd(unsigned reg);
void     host_uci_wr(unsigned reg, unsigned v);

#define UCI_RD(n)    host_uci_rd(n)
#define UCI_RD16(n)  host_uci_rd(n)
#define UCI_WR(n, v) host_uci_wr(n, v)
