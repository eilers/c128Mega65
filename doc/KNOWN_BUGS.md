# What works
* Sound
* Mega65 Keyboard
* Joystick Port
* IEC (Serial) bus (inclusive Burst Mode)
* 40/70 Column mode (HDMI, audio: untested - please report)
* Go64 and native C128 Mode
* Reset-Button

# Known Bugs
* C128-Mode: Core does not show a READY prompt when a device is on the IEC bus without power. This might be a normal behavior 
  with a (or my) 1571. It blocks the check for a bootable disk.   
* CP/M mode: Keyboard broken (`1` → `↑D` or dead, `Z` dead; C128 OK).
  - **Verify on 40-col VIC only.** 80-col uses the VDC; its HDMI path is
    menu-selectable but still early.
  - Fix 1 (ILA-proven): live CIA PRA/PRB in `mos6526_8520.v` — `cpuDi` now
    matches matrix on `$DC01` reads (see `iladata12` / `iladata14`).
  - Fix 2 (testing): Z80 `WAIT_n` while IORQ && !cpuHasBus so IN cannot sample
    a floated bus during VIC AEC (`cpu_z80.vhd` / `fpga64_sid_iec.vhd`).
  - Fix 3 (testing): `alt_crsr` from `mmu_z80_n` (not BUSAK); VIC `$D02F`
    `k_reg` resets to idle `111` (active-low K lines).
  - Fix 4 (did not fix symptom): level-sensitive port writes without `phi2_n`.
  - Fix 5 (testing): port writes = `phi2_n` write **or** 2nd+ consecutive
    CS+write cycle (avoid re-sampling `db_in` every clk). ILA depth 64k for
    press-edge captures. Still: col7 `cpuDi=FE` proven (`iladata14`/`17`);
    suspect missed `OUT FF` / VIC-phase mis-index if `↑D` persists.
  - Keymap (CXKYCODE/CXINTR): `1` = col7/bit0 → code `$38` → ASCII `$31`.
    Visible `↑D` is `$5E` + `D` — **not** the normal `1` mapping.
* GEOS for C128 does not boot and is crashing. 

# Missing Features
* Video:
    * "Flicker-free" has no effect in 80-column mode. 
    * "HDMI: Zoom-in": removed from the menu, `qnice_zoom_crop_o` is hardwired to
      '0' in `mega65.vhd`. `crop.vhd` uses fixed VIC-II geometry (border 33/35,
      image 320x200), so it has to become source-aware before the menu item can
      come back and be wired for the VDC.
    * "Audio improvements": removed from the menu, `qnice_audio_filter_o` is
      hardwired to '0' in `mega65.vhd`
    * ...
* Virtual devices (IEC)
* Cartridge support
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
* Fixing keyboard layout. See https://github.com/eilers/c128Mega65/issues/1