# What works

* Sound
* Mega65 Keyboard
* Joystick Port
* IEC (Serial) bus (inclusive Burst Mode)
* Real cartridges in the expansion port (C64 and C128 cartridges)
* 40/70 Column mode (HDMI, audio: untested - please report)
* Go64 and native C128 Mode
* Reset-Button
* Added various screen resolutions.
* Added support of physical cartridges. Please do not expect the compatibilty that is provided by the C64Mega65 core! Just use it
  for Cartridges that are for the C128 only (as set by default).

# Known Bugs
Please note that this is an early alpha version! So please report any issues [on github](https://github.com/csoren/c128cpm/issues)
* C128-Mode: Core does not show a READY prompt when a device is on the IEC bus without power. This might be a normal behavior
  with a (or my) 1571. It blocks the check for a bootable disk.
* Software compatibility:
  * CP/M is not booting properly or the keyboard is broken.
  * GEOS for C128 does not boot and is crashing.
  * Microsoft Multiplan v1.06 is reported to crash.

# Missing Features

* Video:
  * "Flicker-free" has no effect in 80-column mode.
  * "HDMI: Zoom-in"
* "Audio improvements"
* Virtual devices (IEC)
* Expansion port: only real cartridges are supported. Emulated cartridges (`.crt`
  files), a simulated 1750 REU and cartridges that want to become bus master
  (`/DMA` is ignored) are not implemented.
* Supporting the internal drive as 1581.

# Fixed

* VDC timing closure: `vdc_signals.sv` computed the interlace field-1 vsync column
  with a **modulo by a runtime register** (`(hp + (reg_ht>>1) - 1) % reg_ht`), which
  Vivado turned into a combinational divider — 133 logic levels, 92 `CARRY4`, 54 ns —
  on the `vsCount` clock enable.
* "Flicker-free" (HDMI submenu, default OFF).
* HDMI resolution switching via the Help menu in order to support 4:3 screens.
* Reset Button is now working.
* Dedicated 32.000 MHz VDC MMCM (`clk_vdc.vhd`) wired; Help menu Video Out
  (Follow 40/80 / VIC / VDC), CRT emulation, and VIC-II Jailbars.
* C128-Mode: Joystick fire button was reported as not working (discord)
* Fixing keyboard layout. See <https://github.com/eilers/c128Mega65/issues/1>
