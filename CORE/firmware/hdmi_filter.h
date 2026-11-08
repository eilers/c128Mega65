// The HDMI filter: the polyphase coefficients for the menu's choice (hdmi_filter.c)
#pragma once

// The submenu's choices, in its order (config.vhd). From HF_SCANLINES on they have scan
// lines: his HDMI Scan lines is Enabled for them. HF_DEFAULT is config.vhd's default.
enum { HF_NONE, HF_SHARP, HF_BICUBIC, HF_SMOOTH, HF_LANCZOS, HF_SCANLINES, HF_SVIDEO,
       HF_COMPOSITE, HF_COUNT, HF_DEFAULT = HF_SHARP };

// Loads them when the choice changed, by the menu, a settings file or REST; from the main loop
void hdmi_filter_poll(void);
