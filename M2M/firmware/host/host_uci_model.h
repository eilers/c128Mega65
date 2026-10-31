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
   unsigned reboots;               // resets the firmware asked uci.vhd for
   unsigned reu_mem;               // word 0x011: where the REU's memory is
   unsigned reu_keep;              // word 0x012 bit 0
   unsigned reu_resets;            // REU setting changes without it: main.vhd's resets
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
// The control target's: each drive's power (enum drive_power), the mounted image's type
// (drive_image_type), how often the menu was opened, shell_setting's bits by group and item
extern int     drive_pow[2], mounted_type, menu_opens;
// The cartridge's CSR (crtrom_csr_get/set) for EASYFLASH's erase: the last sector written,
// the starts, and how many more reads of its busy bit read 1
extern int     ef_sector, ef_starts, ef_busy;
extern uint8_t settings[64][8];
// LOAD_CONFIG's: the REU group's items in the menu (4, or 2 on R3 memory), shell_crtrom_named's
// calls for the REU file and its last path ("-" for none). The menu's REU setting (0-3), and
// main.vhd's, which follows a change a few reads of word 0x012 later, or at reu_settle;
// reu_sync makes it the menu's at once (a test that sets the menu itself); reu_forces counts
// the firmware's changes of it
extern int      reu_items, crt_named_calls;
extern char     crt_named[300];
extern unsigned reu_main, reu_forces;
unsigned        reu_menu(void);
void            reu_settle(void);
void            reu_sync(void);
