// Ultimate Command Interface: commands from a C128 program at $DF1C (CORE/vhdl/uci/uci.vhd),
// served from the main loop as Gideon's command_intf.cc serves them. A command's first byte
// picks the target (bits 0-3) and can ask for no reply (bit 7); its second is the command.
#pragma once
#include <stdint.h>

#define UCI_MAX_COMMAND 895   // the command queue holds 896, but stops one short
#define UCI_MAX_REPLY   896
#define UCI_MAX_STATUS  255

// A target's answer: data, a status text such as "00,OK", and whether more data follows
// once the C128 has taken this part (Data More). A status of NULL means not yet: the C128
// sees the command busy, and the target's work() is called from the main loop until it
// sets one, so that a long job never stops the loop.
typedef struct {
   const uint8_t *data;
   int            len;
   const char    *status;
   int            more;
} uci_reply_t;

typedef struct {
   // cmd[len] is 0, so a name at the end of a command is a C string
   void (*command)(uint8_t *cmd, int len, uci_reply_t *r);
   void (*more)(uci_reply_t *r);     // the next part of a Data More reply
   void (*work)(uci_reply_t *r);     // a bit more of a job whose reply is not ready
   void (*abort)(void);              // the C128 gave up on the reply
   void (*reset)(void);              // the C128 was reset
} uci_target_t;

extern uint8_t uci_data[UCI_MAX_REPLY];   // for a target's reply data

void uci_poll(void);

// The targets: MEGA128 DOS 1 and 2 (uci_dos.c)
extern const uci_target_t uci_dos1, uci_dos2;
