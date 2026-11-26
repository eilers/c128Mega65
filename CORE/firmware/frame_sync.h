// HDMI Frame Sync in the menu: Flicker-free and Raw 50.1 Hz need a 50 Hz HDMI mode, and a new
// raw picture is kept only when Space says so; otherwise it goes back after 10 s, since a
// display may show nothing at the C128's own frame rate.
#pragma once
#include <stdint.h>

void fs_sel_pre(int group);        // core_sel_pre: the HDMI mode and Frame Sync before
void fs_sel_post(int group);       // core_sel_post: the rules, and the question
int  fs_asking(void);              // the question is on the screen
int  fs_key(uint32_t key);         // a round of it, from core_app_key: core.h's APP_*
