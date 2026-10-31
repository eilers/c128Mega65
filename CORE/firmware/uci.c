// Ultimate Command Interface, the transport and the dispatcher: Gideon's command_intf.cc
// (run_task, copy_result), polled. Replies and status texts match his firmware's, which
// his tests/e2e/io/command_interface/uci_targets_test.py checks against real hardware.
// Ported from the 1541 Ultimate firmware (github.com/GideonZ/1541ultimate), Copyright
// Gideon Zweijtzer and contributors, GPL v3 as this core; changed for c128Mega65 in 2026.
#include <stddef.h>
#include <string.h>
#include "config_ids.h"
#include "core.h"
#include "m2m_rv.h"
#include "uci.h"

#define UCI_DEVICE    0x0109u        // C_DEV_UCI (globals.vhd)

// Device words (uci.vhd): the protocol's registers, the reset flag and the buffer
#define R_SLOT_BASE   0x0
#define R_SLOT_ENABLE 0x1
#define R_HANDSHAKE   0x2
#define R_STATUS      0x3
#define R_IRQ_CLEAR   0x5
#define R_STAT_LEN    0xA
#define R_RESP_LEN_L  0xC
#define R_RESP_LEN_H  0xD
#define R_CMD_LEN_L   0xE
#define R_CMD_LEN_H   0xF
#define W_FLAGS       0x010
#define W_REU_MEM     0x011
#define W_REU_KEEP    0x012          // bit 0 keep the C128 running; R bits 2-1 main.vhd's REU
#define W_COMMAND     0x800
#define W_RESPONSE    (0x800 + 896)
#define W_STATUS      (0x800 + 1792)

// What the C128 has done (R_STATUS), and what R_HANDSHAKE does
#define ST_NEW_COMMAND 0x01
#define ST_DATA_ACC    0x02
#define ST_ABORT       0x04
#define HS_RESET       0x87         // back to idle, all three of those cleared
#define HS_ACCEPT      0x01
#define HS_NEXT_DATA   0x02
#define HS_LAST        0x10
#define HS_MORE        0x30

// Registers and the reset flag; the host test puts a model of the protocol here
#ifndef UCI_RD
#define UCI_RD(n)    (M2M_DEV16[n] & 0xFFu)
#define UCI_RD16(n)  (M2M_DEV16[n])
#define UCI_WR(n, v) (M2M_DEV16[n] = (uint16_t)(v))
#endif

uint8_t        uci_data[UCI_MAX_REPLY] __attribute__((aligned(4)));
static uint8_t cmd[UCI_MAX_COMMAND + 1];
static int     ready;
static int     target = -1;         // the target whose reply is under way, or -1
static int     pending;             // its reply isn't ready yet: work() goes on
static uci_reply_t job;

// command_target.h's base class: a target that nobody registered
static void none_command(uint8_t *c, int len, uci_reply_t *r)
{
   static const uint8_t name[] = "NO TARGET";
   r->data   = name;
   r->len    = len > 1 && c[1] == 0x01 ? 9 : 0;
   r->status = r->len ? "00,OK" : "21,UNKNOWN COMMAND";
}

static const uci_target_t none = { none_command, NULL, NULL, NULL, NULL };

static const uci_target_t *targets[16] = {
   [1] = &uci_dos1,
   [2] = &uci_dos2,
   [3] = &uci_net,
   [4] = &uci_ctrl,
};

static const uci_target_t *get(int t)
{
   return targets[t] ? targets[t] : &none;
}

// copy_result: the reply into its queues, then Data Last or Data More
static void send(const uci_reply_t *r)
{
   int n = r->len < UCI_MAX_REPLY ? r->len : UCI_MAX_REPLY;
   int s = strlen(r->status);
   if (s > UCI_MAX_STATUS)
      s = UCI_MAX_STATUS;

   M2M_RAMROM_DEV = UCI_DEVICE;      // the target may have used another device
   for (int i = 0; i < n; i++)
      M2M_DEV16[W_RESPONSE + i] = r->data[i];
   for (int i = 0; i < s; i++)
      M2M_DEV16[W_STATUS + i] = (uint8_t)r->status[i];
   UCI_WR(R_RESP_LEN_H, n >> 8);
   UCI_WR(R_RESP_LEN_L, n & 0xFF);
   UCI_WR(R_STAT_LEN, s);
   UCI_WR(R_HANDSHAKE, r->more ? HS_MORE : HS_LAST);
}

static void reply_more(void)
{
   uci_reply_t r = { NULL, 0, "00,OK", 0 };
   if (target >= 0 && get(target)->more)
      get(target)->more(&r);
   send(&r);
}

static void done(void);

static void new_command(void)
{
   int len = UCI_RD(R_CMD_LEN_L) | UCI_RD(R_CMD_LEN_H) << 8;
   if (len > UCI_MAX_COMMAND)
      len = UCI_MAX_COMMAND;

   // A command with no bytes gets an empty reply, from no target
   if (len == 0) {
      UCI_WR(R_RESP_LEN_H, 0);
      UCI_WR(R_RESP_LEN_L, 0);
      UCI_WR(R_STAT_LEN, 0);
      UCI_WR(R_HANDSHAKE, HS_ACCEPT);
      UCI_WR(R_HANDSHAKE, HS_LAST);
      return;
   }

   for (int i = 0; i < len; i++)
      cmd[i] = M2M_DEV16[W_COMMAND + i];
   cmd[len] = 0;
   target = cmd[0] & 0x0F;

   job = (uci_reply_t){ NULL, 0, "00,OK", 0 };
   get(target)->command(cmd, len, &job);
   M2M_RAMROM_DEV = UCI_DEVICE;
   UCI_WR(R_HANDSHAKE, HS_ACCEPT);   // the command queue is free again
   if (!job.status && get(target)->work)
      pending = 1;
   else
      done();
}

// The command's reply, or none for a no-reply command
static void done(void)
{
   pending = 0;
   if (!job.status)
      job.status = "";
   if (cmd[0] & 0x80)
      UCI_WR(R_HANDSHAKE, HS_RESET);
   else
      send(&job);
}

// uci.vhd resets the C128 (word 0x010 bit 1), which then sets the reset flag
void uci_reboot(void)
{
   M2M_RAMROM_DEV = UCI_DEVICE;
   UCI_WR(W_FLAGS, 2);
}

uint16_t uci_reu_mem(void)
{
   M2M_RAMROM_DEV = UCI_DEVICE;
   return UCI_RD16(W_REU_MEM);
}

// The keep bit is on the C128's side once its write returns, and goes once main.vhd has the
// new setting; the menu's bits get there in microseconds
int uci_reu_set(int pos)
{
   int seen;
   M2M_RAMROM_DEV = UCI_DEVICE;
   UCI_WR(W_REU_KEEP, 1);
   shell_force(OPTM_G_REU, pos, 1);
   M2M_RAMROM_DEV = UCI_DEVICE;
   uint32_t start = MTIME_LO;
   while (!(seen = (UCI_RD(W_REU_KEEP) >> 1 & 3) == (unsigned)pos) && MTIME_LO - start < CPU_HZ / 100)
      ;
   UCI_WR(W_REU_KEEP, 0);
   return seen ? 0 : -1;
}

static void start(void)
{
   M2M_RAMROM_DEV = UCI_DEVICE;
   UCI_WR(R_SLOT_BASE, 0x47);         // $DF1C, which main.vhd decodes
   UCI_WR(R_SLOT_ENABLE, 1);
   UCI_WR(R_HANDSHAKE, HS_RESET);
   UCI_WR(R_IRQ_CLEAR, 7);            // the firmware polls; the mask stays open
   UCI_WR(W_FLAGS, 1);
}

void uci_poll(void)
{
   if (!ready) {
      start();
      ready = 1;
   }
   M2M_RAMROM_DEV = UCI_DEVICE;

   // A reset of the C128: the program that talked to the targets is gone
   if (UCI_RD(W_FLAGS) & 1) {
      UCI_WR(W_FLAGS, 1);
      for (int t = 0; t < 16; t++)
         if (get(t)->reset)
            get(t)->reset();
      target  = -1;
      pending = 0;
      M2M_RAMROM_DEV = UCI_DEVICE;
      UCI_WR(R_HANDSHAKE, HS_RESET);
      return;
   }

   unsigned st = UCI_RD(R_STATUS);
   if (st & ST_ABORT) {
      if (target >= 0 && get(target)->abort)
         get(target)->abort();
      target  = -1;
      pending = 0;
      M2M_RAMROM_DEV = UCI_DEVICE;
      UCI_WR(R_HANDSHAKE, HS_RESET);
      return;
   }
   if (pending) {
      get(target)->work(&job);
      M2M_RAMROM_DEV = UCI_DEVICE;
      if (job.status)
         done();
      return;
   }
   if (st & ST_DATA_ACC) {
      reply_more();
      UCI_WR(R_HANDSHAKE, HS_NEXT_DATA);
   }
   if (st & ST_NEW_COMMAND)
      new_command();
}
