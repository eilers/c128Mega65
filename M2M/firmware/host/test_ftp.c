// The FTP server (CORE/firmware/net/ftp.c) with the real lwIP and lwipopts.h, on a loopback
// netif at 10.0.0.1 as test_http has it: the clients are lwIP pcbs in the same stack, the card
// a FAT image. The network also takes turns inside FatFs's disk work, as core_idle does. The
// expected replies are Gideon's (software/network/ftpd.cc at 4b00732), from his code.
// usage: test_ftp IMG
#include "lwip/init.h"            // first: macOS's headers define BYTE_ORDER and htons too
#include "lwip/ip4.h"
#include "lwip/tcp.h"
#include "lwip/timeouts.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "drives.h"
#include "ff.h"
#include "ftp.h"
#include "net.h"
#include "netcfg.h"
#include "sdcard.h"
#include "uci.h"

#define STEP_US   100                  // a main-loop round
#define WIRE_US   200                  // a packet's time on the wire
#define SECTOR_US 20                   // a sector's time on the card, with a turn after it
#define SETTINGS  "/c128/c128mega65-V1.1_Beta1.cfg"

static uint64_t now_us;
uint32_t net_millis(void) { return (uint32_t)(now_us / 1000); }
uint32_t sys_now(void) { return net_millis(); }
uint32_t net_rand(void) { static uint32_t s = 12345; s ^= s << 13; s ^= s >> 17; s ^= s << 5; return s; }
uint32_t net_lock(void) { return 0; }
void     net_unlock(uint32_t dev) { (void)dev; }
void    *heap_alloc(uint32_t bytes) { return calloc(1, bytes); }
void     uart_puts(const char *s) { fputs(s, stderr); }
int      disk_file_open(const char *path);

// --- what ftp.c uses of the firmware ---
char netcfg[NC_COUNT][32];
static int  year_now = 2026;
static bool card_in = true;
static char mounted[2][256];
int uci_year(void) { return year_now; }
enum sdcard_state sdcard_state(void) { return card_in ? SDCARD_OK : SDCARD_NONE; }
int drives_count(void) { return 2; }
int drive_mounted(int d) { return mounted[d][0] != 0; }
const char *drive_path(int d) { return mounted[d]; }
int cfg_str(uint32_t sel, char *buf, int size) { return snprintf(buf, size, "%s", SETTINGS); }

// --- the loopback ---
struct netif eth_netif;
static struct pkt { uint8_t *d; int n; uint64_t at; } wire[8192];
static int  wire_n;
static bool in_turn;
static int  nested;                    // turns taken inside FatFs

static err_t lo_output(struct netif *nif, struct pbuf *p, const ip4_addr_t *a)
{
   if (wire_n == 8192) {
      fprintf(stderr, "wire full\n");
      exit(2);
   }
   wire[wire_n].d = malloc(p->tot_len);
   wire[wire_n].n = pbuf_copy_partial(p, wire[wire_n].d, p->tot_len, 0);
   wire[wire_n].at = now_us + WIRE_US;
   wire_n++;
   return ERR_OK;
}

static err_t lo_init(struct netif *nif)
{
   nif->output = lo_output;
   nif->mtu = 1500;
   nif->flags = NETIF_FLAG_UP | NETIF_FLAG_LINK_UP;
   return ERR_OK;
}

// A network turn: packets that are due, lwIP's timers
static void turn(void)
{
   if (in_turn)
      return;
   in_turn = true;
   while (wire_n && now_us >= wire[0].at) {
      struct pkt k = wire[0];
      memmove(wire, wire + 1, sizeof wire[0] * --wire_n);
      struct pbuf *p = pbuf_alloc(PBUF_RAW, k.n, PBUF_RAM);
      pbuf_take(p, k.d, k.n);
      free(k.d);
      if (eth_netif.input(p, &eth_netif) != ERR_OK)
         pbuf_free(p);
   }
   sys_check_timeouts();
   in_turn = false;
}

void disk_idle(void)
{
   now_us += SECTOR_US;
   if (wire_n && now_us >= wire[0].at)
      nested++;
   turn();
}

// A main-loop round
static void step(void)
{
   now_us += STEP_US;
   turn();
   ftp_poll();
}

static void run_for(uint64_t us)
{
   uint64_t end = now_us + us;
   while (now_us < end)
      step();
}

// Long waits a millisecond a round
static void idle_for(uint64_t us)
{
   uint64_t end = now_us + us;
   while (now_us < end) {
      now_us += 1000;
      turn();
      ftp_poll();
   }
}

// --- clients ---
typedef struct {
   struct tcp_pcb *pcb;
   uint8_t *got;
   int      n, cap, read;              // read: how far the replies have been taken
   bool     up, closed, reset, hold;   // hold: don't read (the window fills)
} client_t;

static err_t cl_recv(void *arg, struct tcp_pcb *pcb, struct pbuf *p, err_t err)
{
   client_t *c = arg;
   if (!p) {
      c->closed = true;
      tcp_arg(pcb, NULL);
      tcp_recv(pcb, NULL);
      tcp_err(pcb, NULL);
      tcp_close(pcb);
      c->pcb = NULL;
      return ERR_OK;
   }
   if (c->hold)
      return ERR_MEM;
   if (c->n + p->tot_len + 1 > c->cap) {
      c->cap = (c->n + p->tot_len + 1) * 2;
      c->got = realloc(c->got, c->cap);
   }
   c->n += pbuf_copy_partial(p, c->got + c->n, p->tot_len, 0);
   c->got[c->n] = 0;
   tcp_recved(pcb, p->tot_len);
   pbuf_free(p);
   return ERR_OK;
}

static void cl_err(void *arg, err_t err)
{
   client_t *c = arg;
   c->pcb = NULL;
   c->reset = err == ERR_RST;
   c->closed = true;
}

static err_t cl_connected(void *arg, struct tcp_pcb *pcb, err_t err)
{
   ((client_t *)arg)->up = true;
   return ERR_OK;
}

static client_t *cl_new(struct tcp_pcb *pcb)
{
   client_t *c = calloc(1, sizeof *c);
   c->cap = 4096;
   c->got = calloc(1, c->cap);
   c->pcb = pcb;
   tcp_arg(pcb, c);
   tcp_recv(pcb, cl_recv);
   tcp_err(pcb, cl_err);
   return c;
}

static client_t *cl_connect(uint16_t port)
{
   ip_addr_t a;
   IP4_ADDR(ip_2_ip4(&a), 10, 0, 0, 1);
   client_t *c = cl_new(tcp_new());
   tcp_connect(c->pcb, &a, port, cl_connected);
   for (int i = 0; i < 1000 && !c->up && !c->closed; i++)
      step();
   return c;
}

static void cl_free(client_t *c)
{
   if (c->pcb) {
      tcp_arg(c->pcb, NULL);
      tcp_recv(c->pcb, NULL);
      tcp_err(c->pcb, NULL);
      tcp_abort(c->pcb);
   }
   free(c->got);
   free(c);
}

static void cl_send(client_t *c, const void *d, int n)
{
   const uint8_t *p = d;
   while (n > 0 && c->pcb) {
      int k = tcp_sndbuf(c->pcb);
      if (k > n)
         k = n;
      if (k && tcp_write(c->pcb, p, k, TCP_WRITE_FLAG_COPY) == ERR_OK) {
         tcp_output(c->pcb);
         p += k;
         n -= k;
      }
      step();
   }
}

static void cl_close(client_t *c)
{
   if (c->pcb)
      tcp_shutdown(c->pcb, 0, 1);
}

// Until the server has closed it, or 20 s
static void run_until_closed(client_t *c)
{
   for (uint64_t end = now_us + 20000000; now_us < end && !c->closed;)
      step();
}

// The next whole reply, up to its line "ddd " (20 s at most); "" if none came
static const char *reply(client_t *c)
{
   static char r[8192];
   for (uint64_t end = now_us + 20000000; now_us < end;) {
      const char *s = (const char *)c->got + c->read;
      for (const char *l = s; *l;) {
         const char *e = strstr(l, "\r\n");
         if (!e)
            break;
         if (l[0] >= '0' && l[0] <= '9' && l[3] == ' ') {
            int n = e + 2 - s;
            memcpy(r, s, n);
            r[n] = 0;
            c->read += n;
            return r;
         }
         l = e + 2;
      }
      if (c->closed)
         break;
      step();
   }
   return "";
}

static const char *cmd(client_t *c, const char *line)
{
   char b[4096];
   int n = snprintf(b, sizeof b, "%s\r\n", line);
   cl_send(c, b, n);
   return reply(c);
}

// --- checks ---
static int failures, checks;

static void check(const char *name, bool ok, const char *got)
{
   checks++;
   if (ok)
      return;
   failures++;
   printf("FAIL %s\n", name);
   if (got)
      printf("  got: \"%s\"\n", got);
}

static void expect(const char *name, const char *got, const char *want)
{
   check(name, !strcmp(got, want), got);
}

static void ask(client_t *c, const char *line, const char *want)
{
   expect(line, cmd(c, line), want);
}

static client_t *login(void)
{
   client_t *c = cl_connect(21);
   reply(c);
   cmd(c, "USER anonymous");
   cmd(c, "PASS x");
   return c;
}

static client_t *pasv(client_t *c)
{
   int a, b, cc, d, p1, p2;
   const char *r = cmd(c, "PASV");
   if (sscanf(r, "227 Entering Passive Mode (%d,%d,%d,%d,%d,%d).", &a, &b, &cc, &d, &p1, &p2) != 6) {
      check("PASV", false, r);
      return NULL;
   }
   return cl_connect(p1 << 8 | p2);
}

static void put_file(const char *path, const void *d, int n)
{
   FIL f;
   UINT put;
   if (f_open(&f, path, FA_WRITE | FA_CREATE_ALWAYS) != FR_OK || f_write(&f, d, n, &put) != FR_OK ||
       (int)put != n) {
      printf("can't write %s\n", path);
      exit(2);
   }
   f_close(&f);
}

static int get_file(const char *path, uint8_t *d, int max)
{
   FIL f;
   UINT got = 0;
   if (f_open(&f, path, FA_READ) != FR_OK)
      return -1;
   f_read(&f, d, max, &got);
   f_close(&f);
   return got;
}

static uint8_t *pattern(int n, int seed)
{
   uint8_t *d = malloc(n);
   uint32_t x = seed * 2654435761u + 1;
   for (int i = 0; i < n; i++) {
      x ^= x << 13;
      x ^= x >> 17;
      x ^= x << 5;
      d[i] = x;
   }
   return d;
}

// --- the cases ---

static void session(void)
{
   client_t *c = cl_connect(21);
   char want[128];
   snprintf(want, sizeof want, "220 C128MEGA65 FTP: IP = 10.0.0.1. Hello 10.0.0.1:%d\r\n", c->pcb->local_port);
   expect("greeting", reply(c), want);
   ask(c, "PWD", "530 Not logged in.\r\n");
   ask(c, "EPSV", "530 Not logged in.\r\n");
   ask(c, "USER anonymous", "331 User name okay, need password.\r\n");
   ask(c, "PASS", "230 User logged in, proceed.\r\n");       // no password set: any
   ask(c, "SYST", "214 UNIX system type.\r\n");
   ask(c, "FEAT", "211-Features:\r\n MLST size*;type*;modify*\r\n MLSD\r\n211 End\r\n");
   ask(c, "NOOP", "200 Command okay.\r\n");
   ask(c, "noop", "200 Command okay.\r\n");
   ask(c, "TYPE I", "200 Command okay.\r\n");
   ask(c, "TYPE A", "200 Command okay.\r\n");
   ask(c, "MODE S", "502 Command not implemented.\r\n");
   ask(c, "EPSV", "502 Command not implemented.\r\n");
   ask(c, "OPTS UTF8 ON", "502 Command not implemented.\r\n");
   ask(c, "ABOR", "226 Closing data connection.\r\n");
   cl_send(c, "NOOP\nNOOP\r\n\r\nNOOP\r", 18);                // LF, CR LF, CR; empty lines skipped
   expect("three commands at once", reply(c), "200 Command okay.\r\n");
   reply(c);
   expect("the third", reply(c), "200 Command okay.\r\n");
   ask(c, "QUIT", "221 Goodbye.\r\n");
   run_until_closed(c);
   check("QUIT: closed", c->closed && !c->reset, NULL);
   cl_free(c);

   // A password: a wrong one is answered after a second, and logs nobody in
   strcpy(netcfg[NC_PASS], "pw");
   c = cl_connect(21);
   reply(c);
   cmd(c, "USER u");
   uint64_t t = now_us;
   ask(c, "PASS wrong", "530 Not logged in.\r\n");
   check("a wrong password: a second", now_us - t >= 1000000, NULL);
   ask(c, "PWD", "530 Not logged in.\r\n");
   ask(c, "PASS pw", "230 User logged in, proceed.\r\n");
   ask(c, "PWD", "257 \"/\" is current directory.\r\n");
   cl_free(c);
   netcfg[NC_PASS][0] = 0;
}

static void paths(void)
{
   client_t *c = login();
   ask(c, "PWD", "257 \"/\" is current directory.\r\n");
   ask(c, "CWD SD", "250 Requested file action okay, completed.\r\n");
   ask(c, "XPWD", "257 \"/SD/\" is current directory.\r\n");
   ask(c, "MKD ftp", "257 \"/SD/ftp\" created.\r\n");
   ask(c, "MKD ftp", "553 Requested action not taken.\r\n");
   ask(c, "MKD", "501 Syntax error in parameters or arguments.\r\n");
   ask(c, "XMKD /SD/ftp/sub", "257 \"/SD/ftp/sub\" created.\r\n");
   ask(c, "CWD ftp/sub", "250 Requested file action okay, completed.\r\n");
   ask(c, "PWD", "257 \"/SD/ftp/sub/\" is current directory.\r\n");
   ask(c, "CDUP", "250 Requested file action okay, completed.\r\n");
   ask(c, "PWD", "257 \"/SD/ftp/\" is current directory.\r\n");
   ask(c, "CWD nothing", "550 Requested action not taken.\r\n");
   ask(c, "CWD \\SD\\FTP", "250 Requested file action okay, completed.\r\n");   // either slash, any case
   ask(c, "CWD /", "250 Requested file action okay, completed.\r\n");
   ask(c, "CDUP", "250 Requested file action okay, completed.\r\n");            // the root stays
   ask(c, "PWD", "257 \"/\" is current directory.\r\n");
   ask(c, "MKD x", "553 Requested action not taken.\r\n");                      // not on the card
   ask(c, "RMD /SD/ftp", "550 Requested action not taken.\r\n");                // not empty
   ask(c, "RMD /SD/ftp/sub", "250 Requested file action okay, completed.\r\n");
   ask(c, "RMD /SD/ftp/sub", "550 Requested action not taken.\r\n");
   ask(c, "RMD", "501 Syntax error in parameters or arguments.\r\n");
   card_in = false;
   ask(c, "CWD SD", "550 Requested action not taken.\r\n");
   card_in = true;
   cl_free(c);
}

static void listings(void)
{
   put_file("/ftp/a.prg", "0123456789", 10);
   f_mkdir("/ftp/dir");
   put_file("/ftp/b.d64", "x", 1);
   client_t *c = login(), *d;
   cmd(c, "CWD /SD/ftp");

   d = pasv(c);
   ask(c, "LIST", "150 File status okay; about to open data connection.\r\n");
   expect("LIST: its end", reply(c), "226 Closing data connection.\r\n");
   run_until_closed(d);
   expect("LIST: the lines", (char *)d->got,
          "-rw-rw-rw-   1 user     ftp           10 Jan 01 00:00 a.prg\r\n"
          "drw-rw-rw-   1 user     ftp            0 Jan 01 00:00 dir\r\n"
          "-rw-rw-rw-   1 user     ftp            1 Jan 01 00:00 b.d64\r\n");
   check("LIST: a FIN", d->closed && !d->reset, NULL);
   cl_free(d);

   year_now = 2027;                    // another year's dates show it
   d = pasv(c);
   ask(c, "LIST -la *.PRG", "150 File status okay; about to open data connection.\r\n");
   reply(c);
   run_until_closed(d);
   expect("LIST a pattern, options stripped, another year", (char *)d->got,
          "-rw-rw-rw-   1 user     ftp           10 Jan 01  2026 a.prg\r\n");
   cl_free(d);
   year_now = 2026;

   d = pasv(c);
   ask(c, "NLST /SD/ftp", "150 File status okay; about to open data connection.\r\n");
   reply(c);
   run_until_closed(d);
   expect("NLST", (char *)d->got, "a.prg\r\ndir\r\nb.d64\r\n");
   cl_free(d);

   d = pasv(c);
   ask(c, "MLSD ?.d64", "150 File status okay; about to open data connection.\r\n");
   reply(c);
   run_until_closed(d);
   expect("MLSD", (char *)d->got, "type=file;size=1;modify=20260101000000; b.d64\r\n");
   cl_free(d);

   d = pasv(c);
   ask(c, "LIST /", "150 File status okay; about to open data connection.\r\n");
   reply(c);
   run_until_closed(d);
   expect("LIST of the root", (char *)d->got, "drw-rw-rw-   1 user     ftp            0 Jan 01  1980 SD\r\n");
   cl_free(d);

   d = pasv(c);
   ask(c, "LIST a.prg", "150 File status okay; about to open data connection.\r\n");
   reply(c);
   run_until_closed(d);
   check("LIST of a file: its line", strstr((char *)d->got, " a.prg\r\n") && d->n < 80, (char *)d->got);
   cl_free(d);

   ask(c, "LIST", "425 Can't open data connection, use PORT or PASV first.\r\n");   // no PASV again
   ask(c, "LIST /SD/none/x", "451 Requested action aborted: local error in processing.\r\n");
   ask(c, "MLST a.prg", "250- Listing a.prg\r\ntype=file;size=10;modify=20260101000000; a.prg\r\n250 End\r\n");
   ask(c, "MLST /SD/ftp/dir", "250- Listing dir\r\ntype=dir;modify=20260101000000; dir\r\n250 End\r\n");
   ask(c, "MLST", "250- Listing ftp\r\ntype=cdir;modify=20260101000000; ftp\r\n250 End\r\n");
   ask(c, "MLST none", "550 Requested action not taken.\r\n");
   ask(c, "SIZE a.prg", "213 10\r\n");
   ask(c, "SIZE none", "550 Requested action not taken.\r\n");
   ask(c, "SIZE", "501 Syntax error in parameters or arguments.\r\n");
   cl_free(c);
}

// PORT's data connections, accepted at 10.0.0.1:40000
static client_t *accepted;

static err_t on_port_accept(void *arg, struct tcp_pcb *pcb, err_t err)
{
   accepted = cl_new(pcb);
   return ERR_OK;
}

static void transfers(void)
{
   client_t *c = login(), *d;
   cmd(c, "CWD /SD/ftp");

   // 300 KB up and back down
   int n = 300000;
   uint8_t *data = pattern(n, 1), *back = malloc(n + 1);
   d = pasv(c);
   uint64_t t = now_us;
   ask(c, "STOR big.bin", "150 Opening BINARY mode data connection for big.bin.\r\n");
   cl_send(d, data, n);
   cl_close(d);
   expect("STOR: its end", reply(c), "226 Closing data connection.\r\n");
   printf("test_ftp: STOR of %d bytes: %.2f s\n", n, (now_us - t) / 1e6);
   check("STOR: on the card", get_file("/ftp/big.bin", back, n + 1) == n && !memcmp(back, data, n), NULL);
   cl_free(d);
   ask(c, "SIZE big.bin", "213 300000\r\n");

   d = pasv(c);
   t = now_us;
   ask(c, "RETR big.bin", "150 Opening BINARY mode data connection for big.bin (300000 bytes).\r\n");
   expect("RETR: its end", reply(c), "226 Closing data connection.\r\n");
   run_until_closed(d);
   printf("test_ftp: RETR of %d bytes: %.2f s\n", n, (now_us - t) / 1e6);
   check("RETR: the file", d->n == n && !memcmp(d->got, data, n) && !d->reset, NULL);
   cl_free(d);

   // A whole number of the server's buffers
   uint8_t *even = pattern(8192, 2);
   put_file("/ftp/even.bin", even, 8192);
   d = pasv(c);
   ask(c, "RETR even.bin", "150 Opening BINARY mode data connection for even.bin (8192 bytes).\r\n");
   reply(c);
   run_until_closed(d);
   check("RETR of 8192 bytes", d->n == 8192 && !memcmp(d->got, even, 8192), NULL);
   cl_free(d);

   // Empty: a file all the same
   d = pasv(c);
   ask(c, "STOR empty.bin", "150 Opening BINARY mode data connection for empty.bin.\r\n");
   cl_close(d);
   expect("STOR, nothing: its end", reply(c), "226 Closing data connection.\r\n");
   ask(c, "SIZE empty.bin", "213 0\r\n");
   cl_free(d);

   // Before the 150
   ask(c, "RETR none", "550 Requested action not taken.\r\n");
   ask(c, "RETR /SD/ftp/dir", "550 Requested action not taken.\r\n");
   ask(c, "RETR big.bin", "425 Can't open data connection, use PORT or PASV first.\r\n");
   ask(c, "STOR x.bin", "425 Can't open data connection, use PORT or PASV first.\r\n");
   d = pasv(c);
   ask(c, "RETR none", "550 Requested action not taken.\r\n");   // the connection stays for the next
   ask(c, "RETR a.prg", "150 Opening BINARY mode data connection for a.prg (10 bytes).\r\n");
   reply(c);
   run_until_closed(d);
   check("RETR after a 550", d->n == 10, (char *)d->got);
   cl_free(d);

   // A PASV nobody connects to: 425 after 5 s
   cmd(c, "PASV");
   t = now_us;
   ask(c, "RETR a.prg", "150 Opening BINARY mode data connection for a.prg (10 bytes).\r\n");
   expect("no data connection", reply(c), "425 Can't open data connection, use PORT or PASV first.\r\n");
   check("no data connection: 5 s", now_us - t >= 5000000 && now_us - t < 5100000, NULL);

   // Outside the card, the upload fails after its 150
   cmd(c, "CWD /");
   d = pasv(c);
   ask(c, "STOR x.bin", "150 Opening BINARY mode data connection for x.bin.\r\n");
   expect("STOR outside the card", reply(c), "452 Requested action not taken.\r\n");
   cl_free(d);
   cmd(c, "CWD /SD/ftp");

   // A stalled upload: 426 after 5 s, what came is kept
   d = pasv(c);
   ask(c, "STOR stall.bin", "150 Opening BINARY mode data connection for stall.bin.\r\n");
   cl_send(d, data, 1000);
   t = now_us;
   expect("STOR stalled", reply(c), "426 Connection closed; transfer aborted.\r\n");
   check("STOR stalled: 5 s", now_us - t >= 4900000, NULL);
   check("STOR stalled: what came", get_file("/ftp/stall.bin", back, n) == 1000, NULL);
   cl_free(d);

   // A download the client resets
   d = pasv(c);
   d->hold = true;
   ask(c, "RETR big.bin", "150 Opening BINARY mode data connection for big.bin (300000 bytes).\r\n");
   run_for(100000);
   tcp_abort(d->pcb);
   d->pcb = NULL;
   expect("RETR reset", reply(c), "426 Connection closed; transfer aborted.\r\n");
   free(d->got);
   free(d);

   // A download the client doesn't take: 426 after 5 s without progress
   d = pasv(c);
   d->hold = true;
   ask(c, "RETR big.bin", "150 Opening BINARY mode data connection for big.bin (300000 bytes).\r\n");
   expect("RETR not taken", reply(c), "426 Connection closed; transfer aborted.\r\n");
   cl_free(d);

   // Commands wait while a transfer runs, as his do
   d = pasv(c);
   ask(c, "RETR big.bin", "150 Opening BINARY mode data connection for big.bin (300000 bytes).\r\n");
   cl_send(c, "NOOP\r\n", 6);
   expect("NOOP waits", reply(c), "226 Closing data connection.\r\n");
   expect("NOOP after it", reply(c), "200 Command okay.\r\n");
   run_until_closed(d);
   check("RETR with a command behind it", d->n == n, NULL);
   cl_free(d);

   // PORT: the server connects to the client
   ask(c, "PORT 10,0,0,1,256", "501 Syntax error in parameters or arguments.\r\n");
   struct tcp_pcb *l = tcp_new();
   tcp_bind(l, IP_ANY_TYPE, 40000);
   l = tcp_listen(l);
   accepted = NULL;
   tcp_accept(l, on_port_accept);
   ask(c, "PORT 10,0,0,1,156,64", "200 Command okay.\r\n");
   ask(c, "RETR a.prg", "150 Opening BINARY mode data connection for a.prg (10 bytes).\r\n");
   expect("PORT: RETR's end", reply(c), "226 Closing data connection.\r\n");
   for (int i = 0; i < 10000 && !(accepted && accepted->closed); i++)
      step();
   check("PORT: RETR", accepted && accepted->n == 10 && !memcmp(accepted->got, "0123456789", 10), NULL);
   if (accepted)
      cl_free(accepted);
   accepted = NULL;
   cmd(c, "PORT 10,0,0,1,156,64");
   ask(c, "STOR port.bin", "150 Opening BINARY mode data connection for port.bin.\r\n");
   for (int i = 0; i < 10000 && !accepted; i++)
      step();
   if (accepted) {
      cl_send(accepted, data, 5000);
      cl_close(accepted);
   }
   expect("PORT: STOR's end", reply(c), "226 Closing data connection.\r\n");
   check("PORT: STOR", get_file("/ftp/port.bin", back, n) == 5000 && !memcmp(back, data, 5000), NULL);
   if (accepted)
      cl_free(accepted);
   tcp_close(l);
   cmd(c, "PORT 10,0,0,1,156,65");     // nobody there
   ask(c, "RETR a.prg", "150 Opening BINARY mode data connection for a.prg (10 bytes).\r\n");
   expect("PORT: refused", reply(c), "425 Can't open data connection, use PORT or PASV first.\r\n");

   // Two sessions at once: one up, one down
   client_t *c2 = login();
   cmd(c2, "CWD /SD/ftp");
   client_t *d1 = pasv(c), *d2 = pasv(c2);
   uint8_t *data2 = pattern(n, 3);
   ask(c, "STOR two.bin", "150 Opening BINARY mode data connection for two.bin.\r\n");
   ask(c2, "RETR big.bin", "150 Opening BINARY mode data connection for big.bin (300000 bytes).\r\n");
   cl_send(d1, data2, n);
   cl_close(d1);
   expect("two at once: STOR", reply(c), "226 Closing data connection.\r\n");
   expect("two at once: RETR", reply(c2), "226 Closing data connection.\r\n");
   run_until_closed(d2);
   check("two at once: both files", get_file("/ftp/two.bin", back, n + 1) == n && !memcmp(back, data2, n) &&
         d2->n == n && !memcmp(d2->got, data, n), NULL);
   cl_free(d1);
   cl_free(d2);
   cl_free(c2);
   free(data2);

   check("the network took turns inside FatFs", nested > 0, NULL);
   free(data);
   free(back);
   free(even);
   cl_free(c);
}

static void files(void)
{
   client_t *c = login(), *d;
   cmd(c, "CWD /SD/ftp");
   ask(c, "RNTO b.d64", "503 Bad sequence of commands.\r\n");
   ask(c, "RNFR", "501 Syntax error in parameters or arguments.\r\n");
   ask(c, "RNFR b.d64", "350 Requested file action pending further information.\r\n");
   ask(c, "RNTO", "501 Syntax error in parameters or arguments.\r\n");
   ask(c, "RNTO c.d64", "503 Bad sequence of commands.\r\n");     // the RNFR is used up
   cmd(c, "RNFR b.d64");
   ask(c, "RNTO /SD/ftp/dir/c.d64", "250 Requested file action okay, completed.\r\n");
   cmd(c, "RNFR none");
   ask(c, "RNTO x", "450 Requested file action not taken.\r\n");
   cmd(c, "RNFR a.prg");
   ask(c, "RNTO big.bin", "450 Requested file action not taken.\r\n");   // it's there
   ask(c, "DELE empty.bin", "250 Requested file action okay, completed.\r\n");
   ask(c, "DELE empty.bin", "550 Requested action not taken.\r\n");
   ask(c, "DELE dir", "550 Requested action not taken.\r\n");
   ask(c, "DELE", "501 Syntax error in parameters or arguments.\r\n");

   // What the core uses in place: mounted disk images, the menu's settings file
   put_file("/ftp/disk.d64", "disk", 4);
   strcpy(mounted[1], "/ftp/disk.d64");
   ask(c, "DELE disk.d64", "450 File in use.\r\n");
   ask(c, "DELE /sd/FTP/DISK.D64", "450 File in use.\r\n");
   cmd(c, "RNFR disk.d64");
   ask(c, "RNTO other.d64", "450 File in use.\r\n");
   ask(c, "STOR disk.d64", "450 File in use.\r\n");
   d = pasv(c);
   ask(c, "RETR disk.d64", "150 Opening BINARY mode data connection for disk.d64 (4 bytes).\r\n");
   reply(c);
   run_until_closed(d);
   check("a mounted image: RETR", d->n == 4, NULL);
   cl_free(d);
   f_mkdir("/c128");
   put_file(SETTINGS, "\xFF", 1);
   ask(c, "STOR " "/SD" SETTINGS, "450 File in use.\r\n");
   mounted[1][0] = 0;
   ask(c, "DELE disk.d64", "250 Requested file action okay, completed.\r\n");
   cl_free(c);
}

static void limits(void)
{
   client_t *s[5];
   for (int i = 0; i < 4; i++)
      s[i] = login();
   s[4] = cl_connect(21);
   expect("a fifth session", reply(s[4]), "421 Too many FTP connections, try again later.\r\n");
   run_until_closed(s[4]);
   check("a fifth session: closed", s[4]->closed && !s[4]->reset, NULL);
   cl_free(s[4]);
   ask(s[0], "QUIT", "221 Goodbye.\r\n");
   run_until_closed(s[0]);
   cl_free(s[0]);
   s[0] = cl_connect(21);
   check("a session again", !strncmp(reply(s[0]), "220 ", 4), NULL);

   // Idle: 300 s without a command
   idle_for(299000000);
   check("idle 299 s: still open", !s[1]->closed, NULL);
   idle_for(2000000);
   check("idle 300 s: closed", s[1]->closed, NULL);
   for (int i = 0; i < 4; i++)
      cl_free(s[i]);
   idle_for(1000000);

   // The menu's item: off for new sessions only
   client_t *c = login();
   ftp_listen(0);
   client_t *x = cl_connect(21);
   check("off: refused", x->closed && x->reset, NULL);
   cl_free(x);
   ask(c, "NOOP", "200 Command okay.\r\n");
   ftp_listen(1);
   x = cl_connect(21);
   check("on again", !strncmp(reply(x), "220 ", 4), NULL);
   cl_free(x);
   cl_free(c);

   // A line longer than his buffer is cut
   c = login();
   char line[1500] = "NOOP ";
   memset(line + 5, 'x', sizeof line - 6);
   line[sizeof line - 1] = 0;
   ask(c, line, "200 Command okay.\r\n");
   cl_free(c);
}

int main(int argc, char **argv)
{
   static FATFS fs;
   setvbuf(stdout, NULL, _IONBF, 0);
   if (argc != 2 || !disk_file_open(argv[1]) || f_mount(&fs, "", 1) != FR_OK) {
      printf("usage: test_ftp IMG\n");
      return 2;
   }
   lwip_init();
   ip4_addr_t ip, mask, gw;
   IP4_ADDR(&ip, 10, 0, 0, 1);
   IP4_ADDR(&mask, 255, 255, 255, 0);
   IP4_ADDR(&gw, 10, 0, 0, 254);
   netif_add(&eth_netif, &ip, &mask, &gw, NULL, lo_init, ip4_input);
   netif_set_default(&eth_netif);
   netif_set_up(&eth_netif);
   ftp_start();
   ftp_listen(1);

   session();
   paths();
   listings();
   transfers();
   files();
   limits();
   printf("test_ftp: %d checks, %d failed\n", checks, failures);
   return failures != 0;
}
