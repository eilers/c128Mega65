# Known Bugs
* C128-Mode: Core does not show a READY prompt when device is on IEC bus without power. It is stuck on booting from disk
* C128-Mode: Joystick fire button was reported as not working (discord)
* CP/M mode: Input and/or disk mailbox path misbehaves on MEGA65 (keys wrong/dead at `A>`; one image spuriously asks for Disk L though VICE does not). Start at [`HANDOVER-z80-cpm.md`](HANDOVER-z80-cpm.md). Treat [`C128_MiSTer`](../CORE/C128_MiSTer) as stable — investigate MEGA65 glue first.


# Missing Features
* Video:
    * VDC Position / palette / variant menu options
    * HDMI resolution / Zoom menu bits still hardwired in `mega65.vhd`
    * ...
* Virtual devices (IEC)
* Cartridge support

# Fixed
* Reset Button is now working.
* Dedicated 32.000 MHz VDC MMCM (`clk_vdc.vhd`) wired; Help menu Video Out
  (Follow 40/80 / VIC / VDC), CRT emulation, and VIC-II Jailbars.
* VDC HDMI horizontal drift (~10 px): was CDC of VDC pixels into `main_clk`
  (~31.53 vs 32 MHz beat). Now native-domain mux + BUFGMUX of MMCM raw
  clocks onto `video_clk`. Retest:
  `CORE/artifacts/mega65_r6_vic_vdc.bit` (2026-07-29).
