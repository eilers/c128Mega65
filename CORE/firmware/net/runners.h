// The runners without a request, for the menu (a drive's file browser, Assembly 64): a
// program or a cartridge from the card, as REST's run_prg, load_prg and run_crt start them
// (rest_runners.c), and a disk. Results are REST's SS_* codes: 0 is fine.
#pragma once

// Start a program (run: with RUN typed). The C128 is reset, so the menu must be closed.
int runner_prg(const char *card, int run);
// Start the disk mounted in drive d (0: A): its .cfg, then the reset. A boot sector boots, a
// first program for C128 BASIC gets RUN"*", any other disk C64 mode, LOAD"*",8,1 and RUN, at
// the drive's bus ID. c64: C64 mode whatever the disk holds.
int runner_disk(int d, int c64);
// From the main loop: -1 while one runs or there is none, its result once it has ended
int runner_poll(void);
// A .crt into the virtual cartridge, which starts it: *msg may say why not
int runner_crt(const char *card, const char **msg);

// For the menu, which the C128's reset needs closed: a program (card stays the caller's) or
// the disk in drive d, started once the menu has closed. If it fails, the menu opens again
// and says so. runner_pending: one waits or runs, and runner_later moves it on (core_poll).
void runner_later_prg(const char *card, int run);
void runner_later_disk(int d, int c64);
extern int runner_pending;
void runner_later(void);
