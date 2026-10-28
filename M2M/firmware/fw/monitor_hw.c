// The serial monitor's hardware side: the serial port, whose interrupt keeps what arrives
// while the main loop is busy, and memory accesses that survive a bus error.
#include "console.h"
#include "m2m_rv.h"
#include "monitor.h"

#define UART_RX_NEMPTY (1u << 16)
#define UART_TX_NFULL  (1u << 19)
#define UART_IRQ_RX    (1u << 20)   // interrupt while the receive FIFO isn't empty
#define MCAUSE_UART0   0x80000012u  // UART0 is fast interrupt 2
#define MIE_UART0      (1u << 18)

#define RX_SIZE  128u   // a whole command line
#define OUT_SIZE 512u

static volatile uint8_t  rx[RX_SIZE];
static volatile uint32_t rx_head, rx_tail;
static char              out[OUT_SIZE];
static uint32_t          out_head, out_tail;

static volatile bool guard, fault;   // mon_access is under way; it hit a bus error
static uint32_t      trap_vec;       // lib/trap.c's handler

static void __attribute__((interrupt("machine"), aligned(4))) mon_trap(void)
{
   uint32_t cause, epc;
   asm volatile ("csrr %0, mcause" : "=r"(cause));
   if (cause == MCAUSE_UART0) {
      while (UART0_CTRL & UART_RX_NEMPTY) {
         uint8_t c = UART0_DATA;
         if (rx_head - rx_tail < RX_SIZE)
            rx[rx_head++ % RX_SIZE] = c;
      }
      return;
   }
   // A misaligned or failed load or store in mon_access: skip it (2 or 4 bytes long)
   if (guard && cause >= 4 && cause <= 7) {
      fault = true;
      asm volatile ("csrr %0, mepc" : "=r"(epc));
      epc += (*(volatile uint16_t *)epc & 3) == 3 ? 4 : 2;
      asm volatile ("csrw mepc, %0" :: "r"(epc));
      return;
   }
   // Anything else is for trap.c, which stops the firmware: the instruction runs again
   // and traps there
   asm volatile ("csrw mtvec, %0" :: "r"(trap_vec));
}

void monitor_init(void)
{
   asm volatile ("csrr %0, mtvec" : "=r"(trap_vec));
   asm volatile ("csrw mtvec, %0" :: "r"(mon_trap));
   UART0_CTRL |= UART_IRQ_RX;
   asm volatile ("csrs mie, %0" :: "r"(MIE_UART0));
   asm volatile ("csrsi mstatus, 8");
   mon_start();
}

void monitor_poll(void)
{
   while (out_tail != out_head && (UART0_CTRL & UART_TX_NFULL))
      UART0_DATA = (uint8_t)out[out_tail++ % OUT_SIZE];
   while (rx_tail != rx_head) {
      char c = rx[rx_tail % RX_SIZE];
      if (mon_busy() && c != 3)   // the next command waits for the read, Ctrl-C doesn't
         break;
      rx_tail++;
      mon_input(c);
   }
   mon_step();
}

void mon_putc(char c)
{
   if (out_head - out_tail < OUT_SIZE)
      out[out_head++ % OUT_SIZE] = c;
}

int mon_room(void)
{
   return OUT_SIZE - (out_head - out_tail);
}

bool mon_access(uint32_t addr, int width, bool write, uint32_t *v)
{
   fault = false;
   guard = true;
   if (write) {
      if (width == 1)
         *(volatile uint8_t *)addr = *v;
      else if (width == 2)
         *(volatile uint16_t *)addr = *v;
      else
         *(volatile uint32_t *)addr = *v;
   } else {
      if (width == 1)
         *v = *(volatile uint8_t *)addr;
      else if (width == 2)
         *v = *(volatile uint16_t *)addr;
      else
         *v = *(volatile uint32_t *)addr;
   }
   guard = false;
   return !fault;
}
