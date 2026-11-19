// Assembly 64 in the menu (a64_search.c): the search form, the titles, a title's files
#pragma once
#include <stdint.h>

int  a64_open(void);            // the menu's Assembly 64 line: its screen, as it was left
int  a64_key(uint32_t key);     // a round with the screen shown: core.h's APP_*
