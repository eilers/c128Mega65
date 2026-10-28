// A model of Gideon's command_protocol.vhd for the host tests (test_uci, test_prg): the
// firmware's side through host_uci.h, the C128's side as functions; and what the UCI
// firmware uses of the rest of the firmware, faked.
#pragma once
#include <stdint.h>

// Queue pointers are buffer offsets (command 0, response 896, status 1792); state 0 idle,
// 1 busy, 2 Data Last, 3 Data More; hs bits 0-2 new command, data accepted, abort
struct uci_model {
   unsigned cmd, resp, stat;
   unsigned resp_len, stat_len;
   unsigned state, hs, error;
   unsigned c128_reset;            // uci.vhd's flag
   unsigned irq_en;                // the push's IRQ bit, until the reply is read
};
extern struct uci_model um;

void     uci_model_init(void);
unsigned uci_status(void);         // $DF1C
void     c128_control(unsigned v); // $DF1C
void     c128_command(uint8_t b);  // $DF1D
uint8_t  c128_response(void);      // $DF1E
uint8_t  c128_status_byte(void);   // $DF1F
uint8_t  c128_ident(void);         // $DF1D: $C9, or $49 while the IRQ is on

// shell_mount's last drive and path (shell_unmount: -(drive + 10)); mount_err fails it
extern int  mounted_d, mount_err;
// drives_ready() after a mount: high for 3 reads, low for drive_low (0: never), then high
extern int  drive_low, drive_reads;
extern char mounted_path[300];
