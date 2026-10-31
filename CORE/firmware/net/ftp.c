// The FTP server (ftp.h): a port of software/network/ftpd.cc of the 1541 Ultimate firmware
// (github.com/GideonZ/1541ultimate at 4b00732; Copyright Gideon Zweijtzer and contributors,
// GPL v3 as this core), changed for c128Mega65 in 2026. On lwIP's raw API, as http.c is his
// web server, so a client sees what an Ultimate does: his replies, four sessions, passive
// ports from 51000, 5 s for a data connection, 300 s for an idle session. Paths are UCI DOS's:
// '/' holds SD, the active card. lwIP's callbacks only move bytes; the commands and the card
// work run from the main loop (ftp_poll), a step a round.
// Not his: a file the core uses in place (a mounted disk image, the menu's settings file)
// can't be overwritten, deleted or renamed (450), as the drives write it behind FatFs. An
// empty upload makes an empty file, and ABOR has a reply.
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <string.h>
#include "cfg.h"
#include "drives.h"
#include "ff.h"
#include "fmt.h"
#include "ftp.h"
#include "heap.h"
#include "net.h"
#include "netcfg.h"
#include "sdcard.h"
#include "uci.h"
#include "lwip/tcp.h"

#define PORT       21
#define SESSIONS   4                   // FTPD_MAX_SESSIONS
#define CMD_MAX    1024                // COMMAND_BUFFER_SIZE
#define IDLE_MS    (300 * 1000)        // FTPD_CONTROL_IDLE_MS
#define DATA_MS    5000                // his data sockets' timeouts
#define PASV_FIRST 51000
#define PASV_LAST  61000
#define RX_SIZE    TCP_WND             // the control connection's ring, as http.c's
#define BUF_SIZE   4096                // a transfer's: STOR's ring (more than TCP_WND), or what goes out
#define OUT_SIZE   1024                // replies lwIP hasn't taken yet
#define LINE_ROOM  320                 // a listing's longest line

#define MSG150     "150 File status okay; about to open data connection."
#define MSG150RETR "150 Opening BINARY mode data connection for %s (%u bytes)."
#define MSG150STOR "150 Opening BINARY mode data connection for %s."
#define MSG200     "200 Command okay."
#define MSG211     "211-Features:\r\n MLST size*;type*;modify*\r\n MLSD\r\n211 End"
#define MSG213     "213 %u"
#define MSG214SYST "214 UNIX system type."
#define MSG220     "220 C128MEGA65 FTP: IP = %d.%d.%d.%d. Hello %d.%d.%d.%d:%d"
#define MSG221     "221 Goodbye."
#define MSG226     "226 Closing data connection."
#define MSG227     "227 Entering Passive Mode (%d,%d,%d,%d,%d,%d)."
#define MSG230     "230 User logged in, proceed."
#define MSG250     "250 Requested file action okay, completed."
#define MSG257PWD  "257 \"%s\" is current directory."
#define MSG257MKD  "257 \"%s\" created."
#define MSG331     "331 User name okay, need password."
#define MSG350     "350 Requested file action pending further information."
#define MSG421     "421 Too many FTP connections, try again later.\r\n"
#define MSG425     "425 Can't open data connection, use PORT or PASV first."
#define MSG426     "426 Connection closed; transfer aborted."
#define MSG450     "450 Requested file action not taken."
#define MSG450USE  "450 File in use."
#define MSG451     "451 Requested action aborted: local error in processing."
#define MSG452     "452 Requested action not taken."
#define MSG501     "501 Syntax error in parameters or arguments."
#define MSG502     "502 Command not implemented."
#define MSG503     "503 Bad sequence of commands."
#define MSG530     "530 Not logged in."
#define MSG550     "550 Requested action not taken."
#define MSG553     "553 Requested action not taken."

enum { X_NONE, X_LIST, X_NLST, X_MLSD, X_RETR, X_STOR };
enum { D_NONE, D_PASV, D_PORT };                // the data connection PASV or PORT set up
enum { T_OK, T_SETUP, T_ABORT, T_STORAGE };     // his FTPTransferResult

typedef struct {
   struct tcp_pcb *pcb;                // control; NULL once lwIP has dropped it
   bool      greet, fin, quit, broken, authed, wrong_pass, rnfr;
   uint32_t  last_active, wait_until;  // wait_until: a wrong password's second
   uint16_t  head, len;                // what lwIP gave and the server hasn't read
   uint16_t  out_n;                    // replies lwIP hasn't taken
   int       line_n;
   char      cwd[UCI_PATH_MAX];        // ends in '/'
   char      from[UCI_PATH_MAX];       // RNFR's name
   char      target[UCI_PATH_MAX];     // STOR's file on the card, "" outside it
   char      pattern[UCI_PATH_MAX];    // LIST's, "" for all
   // The data connection: PASV's listener and what it accepted, or PORT's
   int       mode;
   struct tcp_pcb *lsn, *data;
   bool      data_up, data_fin, data_gone;
   ip_addr_t port_ip;
   uint16_t  port_port;
   // A transfer: after its 150, until the data connection is up, then a step a round
   int       xfer;
   bool      started, sending, eof, root;   // root: the listing of '/', SD still to come
   uint32_t  since;                    // its start, then its last progress
   int       year;                     // LIST's: this year's dates show the time
   uint8_t  *buf;                      // STOR's ring, or what goes out next
   uint16_t  bhead, blen;
   FIL       f;
   DIR       d;
   bool      fopen, dopen;
   uint8_t   rx[RX_SIZE];
   char      line[CMD_MAX];
   char      out[OUT_SIZE];
} sess_t;

static sess_t         *sess[SESSIONS];
static unsigned        active;          // a bit per session in use
static struct tcp_pcb *listener;
static uint16_t        next_port = PASV_FIRST;
static uint8_t         bufs[SESSIONS][BUF_SIZE];   // in the data RAM: transfers go fast
static char            path[UCI_PATH_MAX], path2[UCI_PATH_MAX];
static FILINFO         fi;              // one for all: one command at a time

static const char *const months[13] = { "Nul", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                                        "Aug", "Sep", "Oct", "Nov", "Dec" };

// The time from t to now in ms. A round's now is taken at its start, so t may be later: a
// turn inside the card's work moved it on.
static int32_t since(uint32_t now, uint32_t t)
{
   return (int32_t)(now - t);
}

// --- replies, from the main loop ----------------------------------------------------------

static void flush(sess_t *s)
{
   if (!s->out_n || !s->pcb)
      return;
   uint32_t dev = net_lock();
   int n = s->out_n < tcp_sndbuf(s->pcb) ? s->out_n : tcp_sndbuf(s->pcb);
   err_t e = n ? tcp_write(s->pcb, s->out, n, TCP_WRITE_FLAG_COPY) : ERR_MEM;
   if (e == ERR_OK)
      tcp_output(s->pcb);
   net_unlock(dev);
   if (e == ERR_OK) {
      memmove(s->out, s->out + n, s->out_n - n);
      s->out_n -= n;
   } else if (e != ERR_MEM) {
      s->broken = true;
   }
}

// A reply, \r\n added (his send_msg)
static void say(sess_t *s, const char *f, ...)
{
   char line[600];
   va_list ap;
   va_start(ap, f);
   int n = fmt_vsnprintf(line, sizeof line - 2, f, ap);
   va_end(ap);
   if (n > (int)sizeof line - 3)
      n = sizeof line - 3;
   line[n++] = '\r';
   line[n++] = '\n';
   if (s->out_n + n > OUT_SIZE) {
      s->broken = true;                // the client doesn't take its replies
      return;
   }
   memcpy(s->out + s->out_n, line, n);
   s->out_n += n;
   flush(s);
}

// --- the card -----------------------------------------------------------------------------

// A name from the folder, on the card: its FatFs path (in path), or NULL
static const char *card(sess_t *s, const char *name)
{
   return uci_resolve(s->cwd, name, path) ? uci_on_card(path) : NULL;
}

static bool is_dir(const char *whole)
{
   const char *c = uci_on_card(whole);
   if (!*whole)
      return true;                     // the root
   if (!c)
      return false;
   if (!strcmp(c, "/"))
      return sdcard_state() == SDCARD_OK;
   return f_stat(c, &fi) == FR_OK && (fi.fattrib & AM_DIR);
}

static DWORD first_cluster(const char *c)
{
   FIL f;
   DWORD cl = 0;
   if (f_open(&f, c, FA_READ) == FR_OK) {
      cl = f.obj.sclust;
      f_close(&f);
   }
   return cl;
}

// The core writes these behind FatFs: a mounted disk image, the menu's settings file. By
// first cluster, so any spelling of the name counts.
static bool in_use(const char *c)
{
   char name[64];
   DWORD cl = first_cluster(c);
   if (!cl)
      return false;
   for (int d = 0; d < drives_count(); d++)
      if (drive_mounted(d) && first_cluster(drive_path(d)) == cl)
         return true;
   cfg_str(CFG_CFG_FILE, name, sizeof name);
   return first_cluster(name) == cl;
}

// Wildcards * and ?, case ignored (his pattern_match)
static bool match(const char *p, const char *f)
{
   const char *star_p = NULL, *star_f = NULL;
   for (;;) {
      if (*p == '*') {
         star_p = ++p;
         star_f = f;
         continue;
      }
      if (!*f)
         return !*p;
      char a = *p >= 'A' && *p <= 'Z' ? *p + 32 : *p, b = *f >= 'A' && *f <= 'Z' ? *f + 32 : *f;
      if (*p && (*p == '?' || a == b)) {
         p++;
         f++;
      } else if (star_p) {
         p = star_p;
         f = ++star_f;
      } else {
         return false;
      }
   }
}

// --- the data connection ------------------------------------------------------------------

static err_t on_data_recv(void *arg, struct tcp_pcb *pcb, struct pbuf *p, err_t err)
{
   sess_t *s = arg;
   if (!p) {
      s->data_fin = true;
      return ERR_OK;
   }
   if (s->sending) {                   // RETR's and LIST's: nothing comes back
      tcp_recved(pcb, p->tot_len);
      pbuf_free(p);
      return ERR_OK;
   }
   if (s->blen + p->tot_len > BUF_SIZE)
      return ERR_MEM;                  // can't happen: the window is smaller
   for (struct pbuf *q = p; q; q = q->next) {
      const uint8_t *d = q->payload;
      for (int n = q->len; n;) {
         int at = (s->bhead + s->blen) % BUF_SIZE, k = BUF_SIZE - at < n ? BUF_SIZE - at : n;
         memcpy(s->buf + at, d, k);
         s->blen += k;
         d += k;
         n -= k;
      }
   }
   pbuf_free(p);
   return ERR_OK;
}

static void on_data_err(void *arg, err_t err)
{
   sess_t *s = arg;
   s->data = NULL;
   s->data_gone = true;
}

static void data_take(sess_t *s, struct tcp_pcb *pcb)
{
   s->data = pcb;
   tcp_arg(pcb, s);
   tcp_recv(pcb, on_data_recv);
   tcp_err(pcb, on_data_err);
}

static err_t on_data_accept(void *arg, struct tcp_pcb *pcb, err_t err)
{
   sess_t *s = arg;
   if (err != ERR_OK || !pcb)
      return ERR_VAL;
   if (s->data) {                      // one data connection a PASV
      tcp_abort(pcb);
      return ERR_ABRT;
   }
   data_take(s, pcb);
   s->data_up = true;
   return ERR_OK;
}

static err_t on_data_connected(void *arg, struct tcp_pcb *pcb, err_t err)
{
   ((sess_t *)arg)->data_up = true;
   return ERR_OK;
}

// Under net_lock
static void drop(struct tcp_pcb *pcb, bool graceful)
{
   tcp_arg(pcb, NULL);
   tcp_recv(pcb, NULL);
   tcp_err(pcb, NULL);
   if (!graceful || tcp_close(pcb) != ERR_OK)
      tcp_abort(pcb);
}

// His destroy_connection: the next transfer needs another PASV or PORT
static void data_reset(sess_t *s, bool graceful)
{
   uint32_t dev = net_lock();
   if (s->lsn) {
      tcp_arg(s->lsn, NULL);
      tcp_close(s->lsn);
      s->lsn = NULL;
   }
   if (s->data) {
      drop(s->data, graceful);
      s->data = NULL;
   }
   net_unlock(dev);
   s->mode = D_NONE;
   s->data_up = s->data_fin = s->data_gone = false;
   s->bhead = s->blen = 0;
}

// --- transfers ----------------------------------------------------------------------------

static void finish(sess_t *s, int result)
{
   static const char *const msgs[] = { MSG226, MSG425, MSG426, MSG452 };
   if (s->fopen)
      f_close(&s->f);
   if (s->dopen)
      f_closedir(&s->d);
   s->fopen = s->dopen = s->root = false;
   data_reset(s, result == T_OK);
   s->xfer = X_NONE;
   s->sending = s->started = false;
   s->last_active = net_millis();
   say(s, msgs[result]);
}

// After its 150; PORT's connection is made now
static void xfer_start(sess_t *s, int x)
{
   s->xfer = x;
   s->sending = x != X_STOR;
   s->started = s->eof = false;
   s->since = net_millis();
   if (s->sending)
      s->bhead = s->blen = 0;          // what the client sent first is dropped
   if (s->mode == D_PORT) {
      uint32_t dev = net_lock();
      struct tcp_pcb *pcb = tcp_new_ip_type(IPADDR_TYPE_V4);
      if (pcb) {
         data_take(s, pcb);
         if (tcp_connect(pcb, &s->port_ip, s->port_port, on_data_connected) != ERR_OK) {
            drop(pcb, false);
            s->data = NULL;
         }
      }
      net_unlock(dev);
      if (!s->data)
         s->data_gone = true;
   }
}

// The listing's next entry, in fi: false at the end
static bool next_entry(sess_t *s)
{
   if (s->root) {
      s->root = false;
      fi.fattrib = AM_DIR;
      fi.fsize = fi.fdate = fi.ftime = 0;
      strcpy(fi.fname, "SD");
      return sdcard_state() == SDCARD_OK && (!s->pattern[0] || match(s->pattern, fi.fname));
   }
   while (s->dopen) {
      if (f_readdir(&s->d, &fi) != FR_OK || !fi.fname[0])
         return false;
      if (!s->pattern[0] || match(s->pattern, fi.fname))
         return true;
   }
   return false;
}

// fi's date and time, made sane as his vfs_stat does
static void when(int t[6])
{
   t[0] = (fi.fdate >> 9) + 1980;
   t[1] = fi.fdate >> 5 & 15;
   t[2] = fi.fdate & 31;
   t[3] = fi.ftime >> 11;
   t[4] = fi.ftime >> 5 & 63;
   t[5] = (fi.ftime & 31) * 2;
   t[1] = t[1] < 1 ? 1 : t[1] > 12 ? 12 : t[1];
   t[2] = t[2] < 1 ? 1 : t[2];
   t[3] = t[3] > 23 ? 23 : t[3];
   t[4] = t[4] > 59 ? 59 : t[4];
}

static int entry_line(sess_t *s, char *o)
{
   int t[6], n;
   bool dir = fi.fattrib & AM_DIR;
   when(t);
   if (s->xfer == X_NLST)
      n = fmt_snprintf(o, LINE_ROOM, "%s\r\n", fi.fname);
   else if (s->xfer == X_MLSD)
      n = fmt_snprintf(o, LINE_ROOM, "type=%s;size=%u;modify=%04d%02d%02d%02d%02d%02d; %s\r\n",
                       dir ? "dir" : "file", (unsigned)fi.fsize, t[0], t[1], t[2], t[3], t[4], t[5],
                       fi.fname);
   else if (t[0] == s->year)
      n = fmt_snprintf(o, LINE_ROOM, "-rw-rw-rw-   1 user     ftp  %11u %s %02d %02d:%02d %s\r\n",
                       (unsigned)fi.fsize, months[t[1]], t[2], t[3], t[4], fi.fname);
   else
      n = fmt_snprintf(o, LINE_ROOM, "-rw-rw-rw-   1 user     ftp  %11u %s %02d %5d %s\r\n",
                       (unsigned)fi.fsize, months[t[1]], t[2], t[0], fi.fname);
   if (dir && s->xfer == X_LIST)
      o[0] = 'd';
   return n < LINE_ROOM ? n : LINE_ROOM - 1;
}

// More to send: RETR's next part of the file into an empty buffer, LIST's next lines
static int fill(sess_t *s)
{
   if (s->xfer == X_RETR) {
      UINT got;
      if (s->blen || s->eof)
         return T_OK;
      if (f_read(&s->f, s->buf, BUF_SIZE, &got) != FR_OK)
         return T_ABORT;
      s->bhead = 0;
      s->blen = got;
      s->eof = got < BUF_SIZE;
      return T_OK;
   }
   if (s->bhead && s->bhead + s->blen > BUF_SIZE - LINE_ROOM) {
      memmove(s->buf, s->buf + s->bhead, s->blen);
      s->bhead = 0;
   }
   while (!s->eof && s->bhead + s->blen <= BUF_SIZE - LINE_ROOM) {
      if (next_entry(s))
         s->blen += entry_line(s, (char *)s->buf + s->bhead + s->blen);
      else
         s->eof = true;
   }
   return T_OK;
}

static void send_step(sess_t *s, uint32_t now)
{
   int r = fill(s);
   if (r != T_OK) {
      finish(s, r);
      return;
   }
   if (s->blen) {
      if (!s->data) {
         finish(s, T_ABORT);
         return;
      }
      uint32_t dev = net_lock();
      int n = s->blen < tcp_sndbuf(s->data) ? s->blen : tcp_sndbuf(s->data);
      err_t e = n ? tcp_write(s->data, s->buf + s->bhead, n, TCP_WRITE_FLAG_COPY) : ERR_MEM;
      if (e == ERR_OK)
         tcp_output(s->data);
      net_unlock(dev);
      if (e == ERR_OK) {
         s->bhead = s->blen == n ? 0 : s->bhead + n;
         s->blen -= n;
         s->since = s->last_active = now;
      } else if (e != ERR_MEM) {
         finish(s, T_ABORT);
         return;
      }
   }
   if (s->eof && !s->blen)
      finish(s, T_OK);
   else if (since(now, s->since) >= DATA_MS)
      finish(s, T_ABORT);                // his 5 s send timeout
}

// STOR: the ring onto the card, its two parts at most a round. Callbacks during f_write only
// add behind what is written: the ring holds at most a window.
static void recv_step(sess_t *s, uint32_t now)
{
   for (int k = 0; k < 2 && s->blen; k++) {
      int at = s->bhead, n = s->blen < BUF_SIZE - at ? s->blen : BUF_SIZE - at;
      UINT put;
      if (f_write(&s->f, s->buf + at, n, &put) != FR_OK || (int)put != n) {
         finish(s, T_STORAGE);
         return;
      }
      s->bhead = (at + n) % BUF_SIZE;
      s->blen -= n;
      uint32_t dev = net_lock();
      if (s->data)
         tcp_recved(s->data, n);
      net_unlock(dev);
      s->since = s->last_active = now;
   }
   if (s->data_fin && !s->blen) {
      FRESULT r = f_close(&s->f);
      s->fopen = false;
      finish(s, r == FR_OK ? T_OK : T_STORAGE);
   } else if (s->data_gone) {
      finish(s, T_ABORT);
   } else if (since(now, s->since) >= DATA_MS) {
      finish(s, T_ABORT);                // his 5 s receive timeout
   }
}

static void transfer(sess_t *s, uint32_t now)
{
   if (!s->started) {
      if (s->data && s->data_up) {
         s->started = true;
         s->since = now;
         if (s->xfer == X_STOR) {        // his opens it at the first data
            if (!s->target[0] || f_open(&s->f, s->target, FA_WRITE | FA_CREATE_ALWAYS) != FR_OK) {
               finish(s, T_STORAGE);
               return;
            }
            s->fopen = true;
         }
      } else if (s->data_gone || since(now, s->since) >= DATA_MS) {
         finish(s, T_SETUP);
         return;
      } else {
         return;
      }
   }
   if (s->sending)
      send_step(s, now);
   else
      recv_step(s, now);
}

// --- the commands -------------------------------------------------------------------------

static void cmd_user(sess_t *s, const char *arg)
{
   say(s, MSG331);
}

static void cmd_pass(sess_t *s, const char *arg)
{
   if (!netcfg[NC_PASS][0] || !strcmp(arg, netcfg[NC_PASS])) {
      s->authed = true;
      say(s, MSG230);
   } else {
      s->wrong_pass = true;            // 530 after a second, his throttle
      s->wait_until = net_millis() + 1000;
   }
}

static void cmd_quit(sess_t *s, const char *arg)
{
   say(s, MSG221);
   s->quit = true;
}

static void cmd_port(sess_t *s, const char *arg)
{
   int v[6], n = 0;
   while (n < 6) {
      while (*arg == ' ')
         arg++;
      if (*arg < '0' || *arg > '9')
         break;
      for (v[n] = 0; *arg >= '0' && *arg <= '9'; arg++)
         v[n] = v[n] * 10 + *arg - '0';
      if (++n < 6 && *arg++ != ',')
         break;
   }
   if (n != 6) {
      say(s, MSG501);
      return;
   }
   data_reset(s, true);
   IP_ADDR4(&s->port_ip, v[0] & 255, v[1] & 255, v[2] & 255, v[3] & 255);
   s->port_port = (v[4] & 255) << 8 | (v[5] & 255);
   s->mode = D_PORT;
   say(s, MSG200);
}

static void cmd_pasv(sess_t *s, const char *arg)
{
   struct tcp_pcb *pcb, *l = NULL;
   uint16_t port = 0;
   data_reset(s, true);
   uint32_t dev = net_lock();
   if ((pcb = tcp_new_ip_type(IPADDR_TYPE_ANY))) {
      err_t e = ERR_USE;
      ip_set_option(pcb, SOF_REUSEADDR);
      for (int i = 0; i < 100 && e == ERR_USE; i++) {
         port = next_port++;
         if (next_port >= PASV_LAST)
            next_port = PASV_FIRST;
         e = tcp_bind(pcb, IP_ANY_TYPE, port);
      }
      if (e == ERR_OK)
         l = tcp_listen_with_backlog(pcb, 1);
      if (!l)
         tcp_close(pcb);
   }
   if (l) {
      tcp_arg(l, s);
      tcp_accept(l, on_data_accept);
      s->lsn = l;
      s->mode = D_PASV;
   }
   net_unlock(dev);
   if (!l) {
      say(s, MSG425);
      return;
   }
   const ip4_addr_t *me = ip_2_ip4(&s->pcb->local_ip);
   say(s, MSG227, ip4_addr1(me), ip4_addr2(me), ip4_addr3(me), ip4_addr4(me), port >> 8, port & 255);
}

static void cd(sess_t *s, const char *name)
{
   if (!uci_resolve(s->cwd, name, path) || !is_dir(path) || strlen(path) + 2 > UCI_PATH_MAX) {
      say(s, MSG550);
      return;
   }
   strcpy(s->cwd, path);
   strcat(s->cwd, "/");
   say(s, MSG250);
}

static void cmd_cwd(sess_t *s, const char *arg)  { cd(s, arg); }
static void cmd_cdup(sess_t *s, const char *arg) { cd(s, ".."); }
static void cmd_pwd(sess_t *s, const char *arg)  { say(s, MSG257PWD, s->cwd); }

// LIST, NLST, MLSD: a folder, or the files in one that a name or pattern matches
static void list(sess_t *s, const char *arg, int x)
{
   while (*arg == '-' || *arg == ' ')  // his strips options; their arguments too
      while (*arg && *arg++ != ' ')
         ;
   s->pattern[0] = 0;
   if (!uci_resolve(s->cwd, arg, path)) {
      say(s, MSG451);
      return;
   }
   if (!is_dir(path)) {
      char *slash = strrchr(path, '/');
      strcpy(s->pattern, slash + 1);
      *slash = 0;
      if (!is_dir(path)) {
         say(s, MSG451);
         return;
      }
   }
   if (!*path)
      s->root = true;
   else if (f_opendir(&s->d, uci_on_card(path)) == FR_OK)
      s->dopen = true;
   else {
      say(s, MSG451);
      return;
   }
   if (s->mode == D_NONE) {
      if (s->dopen)
         f_closedir(&s->d);
      s->dopen = s->root = false;
      say(s, MSG425);
      return;
   }
   say(s, MSG150);
   s->year = uci_year();
   xfer_start(s, x);
}

static void cmd_list(sess_t *s, const char *arg) { list(s, arg, X_LIST); }
static void cmd_nlst(sess_t *s, const char *arg) { list(s, arg, X_NLST); }
static void cmd_mlsd(sess_t *s, const char *arg) { list(s, arg, X_MLSD); }

static void cmd_mlst(sess_t *s, const char *arg)
{
   int t[6];
   if (!uci_resolve(s->cwd, *arg ? arg : ".", path)) {
      say(s, MSG550);
      return;
   }
   const char *c = uci_on_card(path);
   if (!*path || (c && !strcmp(c, "/"))) {             // the root and SD have no entry
      if (*path && sdcard_state() != SDCARD_OK) {
         say(s, MSG550);
         return;
      }
      fi.fattrib = AM_DIR;
      fi.fsize = fi.fdate = fi.ftime = 0;
      strcpy(fi.fname, *path ? "SD" : "/");
   } else if (!c || f_stat(c, &fi) != FR_OK) {
      say(s, MSG550);
      return;
   }
   when(t);
   const char *type = !*arg ? "cdir" : fi.fattrib & AM_DIR ? "dir" : "file";
   if (fi.fattrib & AM_DIR)
      say(s, "250- Listing %s\r\ntype=%s;modify=%04d%02d%02d%02d%02d%02d; %s\r\n250 End", fi.fname,
          type, t[0], t[1], t[2], t[3], t[4], t[5], fi.fname);
   else
      say(s, "250- Listing %s\r\ntype=%s;size=%u;modify=%04d%02d%02d%02d%02d%02d; %s\r\n250 End",
          fi.fname, type, (unsigned)fi.fsize, t[0], t[1], t[2], t[3], t[4], t[5], fi.fname);
}

static void cmd_retr(sess_t *s, const char *arg)
{
   const char *c = card(s, arg);
   if (!c || f_stat(c, &fi) != FR_OK || (fi.fattrib & AM_DIR) || f_open(&s->f, c, FA_READ) != FR_OK) {
      say(s, MSG550);
      return;
   }
   if (s->mode == D_NONE) {
      f_close(&s->f);
      say(s, MSG425);
      return;
   }
   s->fopen = true;
   say(s, MSG150RETR, arg, (unsigned)fi.fsize);
   xfer_start(s, X_RETR);
}

static void cmd_stor(sess_t *s, const char *arg)
{
   const char *c = card(s, arg);
   if (c && in_use(c)) {
      say(s, MSG450USE);
      return;
   }
   if (s->mode == D_NONE) {
      say(s, MSG425);
      return;
   }
   strcpy(s->target, c ? c : "");
   say(s, MSG150STOR, arg);
   xfer_start(s, X_STOR);
}

static void cmd_noop(sess_t *s, const char *arg) { say(s, MSG200); }
static void cmd_syst(sess_t *s, const char *arg) { say(s, MSG214SYST); }
static void cmd_feat(sess_t *s, const char *arg) { say(s, MSG211); }
static void cmd_type(sess_t *s, const char *arg) { say(s, MSG200); }   // binary, always
static void cmd_mode(sess_t *s, const char *arg) { say(s, MSG502); }

static void cmd_abor(sess_t *s, const char *arg)
{
   data_reset(s, false);
   s->rnfr = false;
   say(s, MSG226);
}

static void cmd_rnfr(sess_t *s, const char *arg)
{
   if (!*arg || strlen(arg) >= UCI_PATH_MAX) {
      say(s, MSG501);
      return;
   }
   strcpy(s->from, arg);
   s->rnfr = true;
   say(s, MSG350);
}

static void cmd_rnto(sess_t *s, const char *arg)
{
   if (!s->rnfr) {
      say(s, MSG503);
      return;
   }
   s->rnfr = false;
   if (!*arg) {
      say(s, MSG501);
      return;
   }
   const char *from = card(s, s->from);
   const char *to = uci_resolve(s->cwd, arg, path2) ? uci_on_card(path2) : NULL;
   if (from && in_use(from))
      say(s, MSG450USE);
   else if (!from || !to || f_rename(from, to) != FR_OK)
      say(s, MSG450);
   else
      say(s, MSG250);
}

static void cmd_mkd(sess_t *s, const char *arg)
{
   const char *c;
   if (!*arg)
      say(s, MSG501);
   else if (!(c = card(s, arg)) || f_mkdir(c) != FR_OK)
      say(s, MSG553);
   else
      say(s, MSG257MKD, path);
}

static void cmd_rmd(sess_t *s, const char *arg)
{
   const char *c;
   if (!*arg)
      say(s, MSG501);
   else if (!(c = card(s, arg)) || f_stat(c, &fi) != FR_OK || !(fi.fattrib & AM_DIR) || f_unlink(c) != FR_OK)
      say(s, MSG550);
   else
      say(s, MSG250);
}

static void cmd_dele(sess_t *s, const char *arg)
{
   const char *c;
   if (!*arg)
      say(s, MSG501);
   else if (!(c = card(s, arg)) || f_stat(c, &fi) != FR_OK || (fi.fattrib & AM_DIR))
      say(s, MSG550);
   else if (in_use(c))
      say(s, MSG450USE);
   else if (f_unlink(c) != FR_OK)
      say(s, MSG550);
   else
      say(s, MSG250);
}

static void cmd_size(sess_t *s, const char *arg)
{
   const char *c;
   if (!*arg)
      say(s, MSG501);
   else if (!(c = card(s, arg)) || f_stat(c, &fi) != FR_OK)
      say(s, MSG550);
   else
      say(s, MSG213, (unsigned)fi.fsize);
}

static const struct {
   const char *name;
   void (*fn)(sess_t *s, const char *arg);
} cmds[] = {
   { "USER", cmd_user }, { "PASS", cmd_pass }, { "QUIT", cmd_quit },   // before a login too
   { "PORT", cmd_port }, { "CWD", cmd_cwd },   { "CDUP", cmd_cdup }, { "PWD", cmd_pwd },
   { "XPWD", cmd_pwd },  { "NLST", cmd_nlst }, { "LIST", cmd_list }, { "RETR", cmd_retr },
   { "STOR", cmd_stor }, { "NOOP", cmd_noop }, { "SYST", cmd_syst }, { "ABOR", cmd_abor },
   { "TYPE", cmd_type }, { "MODE", cmd_mode }, { "RNFR", cmd_rnfr }, { "RNTO", cmd_rnto },
   { "MKD", cmd_mkd },   { "XMKD", cmd_mkd },  { "RMD", cmd_rmd },   { "XRMD", cmd_rmd },
   { "DELE", cmd_dele }, { "SIZE", cmd_size }, { "PASV", cmd_pasv }, { "MLST", cmd_mlst },
   { "MLSD", cmd_mlsd }, { "FEAT", cmd_feat },
};
#define CMDS      (int)(sizeof cmds / sizeof cmds[0])
#define OPEN_CMDS 3

// His dispatch_command: up to four letters, in any case; the argument starts one character on
static void dispatch(sess_t *s, const char *text)
{
   char cmd[5];
   int k = 0, i = 0;
   for (; k < 4 && ((text[k] | 32) >= 'a' && (text[k] | 32) <= 'z'); k++)
      cmd[k] = text[k] & ~32;
   cmd[k] = 0;
   while (i < CMDS && strcmp(cmds[i].name, cmd))
      i++;
   if (!s->authed && i >= OPEN_CMDS) {
      say(s, MSG530);
      return;
   }
   const char *arg = (int)strlen(text) < k + 1 ? "" : text + k + 1;
   if (i < CMDS)
      cmds[i].fn(s, arg);
   else
      say(s, MSG502);
}

// A command from the ring: true once a line is whole (CR or LF ends it)
static bool take_line(sess_t *s)
{
   int used = 0;
   bool whole = false;
   while (used < s->len && !whole) {
      char c = s->rx[(s->head + used++) % RX_SIZE];
      if (c == '\r' || c == '\n')
         whole = s->line_n > 0;
      else if (s->line_n < CMD_MAX - 1)
         s->line[s->line_n++] = c;
      else
         s->line[CMD_MAX - 2] = c;     // his: the last character is overwritten
   }
   if (used) {
      s->head = (s->head + used) % RX_SIZE;
      s->len -= used;
      s->last_active = net_millis();
      uint32_t dev = net_lock();
      if (s->pcb)
         tcp_recved(s->pcb, used);
      net_unlock(dev);
   }
   s->line[s->line_n] = 0;
   return whole;
}

// --- sessions -----------------------------------------------------------------------------

static err_t on_recv(void *arg, struct tcp_pcb *pcb, struct pbuf *p, err_t err)
{
   sess_t *s = arg;
   if (!p) {
      s->fin = true;
      return ERR_OK;
   }
   if (s->len + p->tot_len > RX_SIZE)
      return ERR_MEM;                  // can't happen: the window is the ring's size
   for (struct pbuf *q = p; q; q = q->next) {
      const uint8_t *d = q->payload;
      for (int i = 0; i < q->len; i++)
         s->rx[(s->head + s->len++) % RX_SIZE] = d[i];
   }
   pbuf_free(p);
   return ERR_OK;
}

static void on_err(void *arg, err_t err)
{
   ((sess_t *)arg)->pcb = NULL;
}

static err_t on_accept(void *arg, struct tcp_pcb *pcb, err_t err)
{
   int i = 0;
   if (err != ERR_OK || !pcb)
      return ERR_VAL;
   while (i < SESSIONS && (active & 1u << i))
      i++;
   if (i == SESSIONS) {                // his: 421 and closed
      tcp_write(pcb, MSG421, sizeof MSG421 - 1, TCP_WRITE_FLAG_COPY);
      if (tcp_close(pcb) != ERR_OK) {
         tcp_abort(pcb);
         return ERR_ABRT;
      }
      return ERR_OK;
   }
   sess_t *s = sess[i];
   memset(s, 0, offsetof(sess_t, rx));
   s->pcb = pcb;
   s->greet = true;                    // from the main loop
   s->buf = bufs[i];
   s->last_active = net_millis();
   strcpy(s->cwd, "/");
   active |= 1u << i;
   tcp_arg(pcb, s);
   tcp_recv(pcb, on_recv);
   tcp_err(pcb, on_err);
   return ERR_OK;
}

static void end_session(int i)
{
   sess_t *s = sess[i];
   if (s->fopen)
      f_close(&s->f);                  // an upload cut off keeps what came
   if (s->dopen)
      f_closedir(&s->d);
   s->fopen = s->dopen = false;
   s->xfer = X_NONE;
   s->sending = false;
   data_reset(s, false);
   uint32_t dev = net_lock();
   if (s->pcb) {
      tcp_arg(s->pcb, NULL);
      tcp_recv(s->pcb, NULL);
      tcp_err(s->pcb, NULL);
      if (s->len)
         tcp_recved(s->pcb, s->len);
      if (tcp_close(s->pcb) != ERR_OK)
         tcp_abort(s->pcb);
      s->pcb = NULL;
   }
   net_unlock(dev);
   active &= ~(1u << i);               // last: a turn in f_close may not take it before
}

void ftp_start(void)
{
   for (int i = 0; i < SESSIONS; i++)
      sess[i] = heap_alloc(sizeof(sess_t));
}

// Sessions already open keep going
void ftp_listen(int on)
{
   if (on && !listener) {
      struct tcp_pcb *pcb = tcp_new_ip_type(IPADDR_TYPE_ANY);
      if (!pcb)
         return;
      ip_set_option(pcb, SOF_REUSEADDR);
      if (tcp_bind(pcb, IP_ANY_TYPE, PORT) != ERR_OK) {
         tcp_close(pcb);
         return;
      }
      listener = tcp_listen_with_backlog(pcb, 2);   // his listen()
      if (listener)
         tcp_accept(listener, on_accept);
      else
         tcp_close(pcb);
   } else if (!on && listener) {
      tcp_close(listener);
      listener = NULL;
   }
}

void ftp_poll(void)
{
   if (!active)
      return;
   uint32_t now = net_millis();
   for (int i = 0; i < SESSIONS; i++) {
      if (!(active & 1u << i))
         continue;
      sess_t *s = sess[i];
      if (s->pcb && s->greet) {
         const ip4_addr_t *me = ip_2_ip4(&s->pcb->local_ip), *you = ip_2_ip4(&s->pcb->remote_ip);
         s->greet = false;
         say(s, MSG220, ip4_addr1(me), ip4_addr2(me), ip4_addr3(me), ip4_addr4(me), ip4_addr1(you),
             ip4_addr2(you), ip4_addr3(you), ip4_addr4(you), s->pcb->remote_port);
      }
      if (s->lsn && s->data) {         // PASV's one connection is in
         uint32_t dev = net_lock();
         tcp_arg(s->lsn, NULL);
         tcp_close(s->lsn);
         net_unlock(dev);
         s->lsn = NULL;
      }
      if (s->pcb && !s->broken && !s->quit) {
         if (s->xfer)
            transfer(s, now);
         else if (s->wrong_pass) {
            if ((int32_t)(now - s->wait_until) >= 0) {
               s->wrong_pass = false;
               say(s, MSG530);
            }
         } else {
            while (!s->xfer && !s->quit && !s->broken && !s->wrong_pass && s->pcb && take_line(s)) {
               dispatch(s, s->line);
               s->line_n = 0;
            }
         }
      }
      flush(s);
      bool idle = !s->xfer && since(now, s->last_active) >= IDLE_MS;
      if (!s->pcb || s->broken || (s->quit && !s->out_n) || (s->fin && !s->len && !s->xfer) || idle)
         end_session(i);
   }
}
