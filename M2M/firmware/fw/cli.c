// The serial console. It takes one line at a time and never waits for one, so the main
// loop runs on while you type. It replaces the QNICE monitor for debugging.
#include <string.h>
#include "cli.h"
#include "console.h"
#include "fmt.h"
#include "m2m_rv.h"
#include "sdcard.h"
#include "version.h"

static char line[80];
static int  len;
static int  last;
static int  started;

static uint32_t hex(const char *s, uint32_t dflt)
{
   if (!s || !*s)
      return dflt;
   uint32_t v = 0;
   for (; *s; s++) {
      int c = *s | 0x20;
      if (*s >= '0' && *s <= '9')
         v = v * 16 + *s - '0';
      else if (c >= 'a' && c <= 'f')
         v = v * 16 + c - 'a' + 10;
      else
         break;
   }
   return v;
}

static void cmd_ls(const char *arg)
{
   DIR dir;
   FILINFO fi;
   if (sdcard_state() != SDCARD_OK) {
      uart_puts("no SD card mounted\n");
      return;
   }
   FRESULT r = f_opendir(&dir, arg && *arg ? arg : "/");
   if (r != FR_OK) {
      uart_printf("can't open: error %d\n", r);
      return;
   }
   while (f_readdir(&dir, &fi) == FR_OK && fi.fname[0]) {
      if (fi.fattrib & AM_DIR)
         uart_puts("     <dir>  ");
      else
         uart_printf("%10u  ", (unsigned)fi.fsize);
      uart_puts(fi.fname);
      uart_puts("\n");
   }
   f_closedir(&dir);
}

// Words at an address: memory, HyperRAM, the device bus or the registers
static void cmd_md(const char *a, const char *n)
{
   uint32_t addr = hex(a, 0) & ~3u;
   uint32_t words = hex(n, 16);
   for (uint32_t i = 0; i < words; i++) {
      if (i % 4 == 0)
         uart_printf("%s%08X:", i ? "\n" : "", addr + 4 * i);
      uart_printf(" %08X", REG32(addr + 4 * i));
   }
   uart_puts("\n");
}

static void cmd_sd(void)
{
   uint32_t csr = M2M_CSR;
   uart_printf("card %s, %s%s\n", sdcard_slot(),
               csr & M2M_CSR_SD_DET_EXT ? "back slot has a card " : "",
               csr & M2M_CSR_SD_DET_INT ? "bottom tray has a card" : "");
   uart_printf("state %d, last mount %d\n", sdcard_state(), sdcard_result());
   uart_printf("sdhost CSR %04X error %04X status %04X\n", M2M_SD_CSR, M2M_SD_ERROR, M2M_SD_STATUS);
}

static void run(char *s)
{
   char *argv[3] = { 0 };
   int argc = 0;
   while (argc < 3) {
      while (*s == ' ')
         s++;
      if (!*s)
         break;
      argv[argc++] = s;
      while (*s && *s != ' ')
         s++;
      if (*s)
         *s++ = 0;
   }
   if (!argc)
      return;

   if (!strcmp(argv[0], "help"))
      uart_puts("ls [path]        list a folder on the SD card\n"
                "md addr [words]  show memory, in hex\n"
                "sd               the SD cards\n"
                "ver              versions\n"
                "boot             load the firmware again (the boot ROM)\n");
   else if (!strcmp(argv[0], "ls"))
      cmd_ls(argv[1]);
   else if (!strcmp(argv[0], "md"))
      cmd_md(argv[1], argv[2]);
   else if (!strcmp(argv[0], "sd"))
      cmd_sd();
   else if (!strcmp(argv[0], "ver"))
      uart_printf("firmware %s, hardware interface %u\n", FW_VERSION, M2M_RV_HW_VERSION);
   else if (!strcmp(argv[0], "boot"))
      ((void (*)(void))0)();
   else
      uart_printf("%s? Try help\n", argv[0]);
}

void cli_poll(void)
{
   int c;
   if (!started) {
      started = 1;
      uart_puts("> ");
   }
   while ((c = uart_getc()) >= 0) {
      if (c == '\n' && last == '\r') {
         last = c;
         continue;                         // CR LF is one Return
      }
      last = c;
      if (c == '\r' || c == '\n') {
         uart_puts("\n");
         line[len] = 0;
         run(line);
         len = 0;
         uart_puts("> ");
      } else if ((c == 8 || c == 127) && len) {
         len--;
         uart_puts("\b \b");
      } else if (c >= ' ' && c < 127 && len < (int)sizeof line - 1) {
         line[len++] = c;
         uart_putc(c);
      }
   }
}
