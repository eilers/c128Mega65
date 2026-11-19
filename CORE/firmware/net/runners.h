// The runners without a request, for the menu (Assembly 64): a program or a cartridge from
// the card, as REST's run_prg, load_prg and run_crt start them (rest_runners.c). Results are
// REST's SS_* codes: 0 is fine.
#pragma once

// Start a program (run: with RUN typed). The C128 is reset, so the menu must be closed.
int runner_prg(const char *card, int run);
// From the main loop: -1 while one runs or there is none, its result once it has ended
int runner_poll(void);
// A .crt into the virtual cartridge, which starts it: *msg may say why not
int runner_crt(const char *card, const char **msg);
