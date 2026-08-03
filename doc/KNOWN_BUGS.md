# Known Bugs
* C128-Mode: Core does not show a READY prompt when device is on IEC bus without power. It is stuck on booting from disk
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
* GEOS for C128 does not boot an is crashing. 


# Missing Features
* Video:
    * "HDMI: Zoom-in" menu bit still hardwired in `mega65.vhd`. `crop.vhd` uses
      fixed VIC-II geometry (border 33/35, image 320x200), so it has to become
      source-aware before the bit can be wired for the VDC.
    * "Audio improvements" menu bit still hardwired in `mega65.vhd`
    * ...
* Virtual devices (IEC)
* Cartridge support

# Fixed
* HDMI resolution switching via the Help menu (720p 50/60 Hz, 576p 4:3 and 5:4,
  640x480, 720x480, 800x600). Works for both video sources because ascal
  auto-detects the input geometry. The VDC delivers ~760 active pixels per
  line, which is wider than the 720 and 640 pixel modes, so ascal now runs
  with `DOWNSCALE => true` (`digital_pipeline.vhd`).
* Reset Button is now working.
* Dedicated 32.000 MHz VDC MMCM (`clk_vdc.vhd`) wired; Help menu Video Out
  (Follow 40/80 / VIC / VDC), CRT emulation, and VIC-II Jailbars.
* VDC HDMI horizontal drift (~10 px): was CDC of VDC pixels into `main_clk`
  (~31.53 vs 32 MHz beat). Now native-domain mux + BUFGMUX of MMCM raw
  clocks onto `video_clk`. Retest:
  `CORE/artifacts/mega65_r6_vic_vdc.bit` (2026-07-29).
* C128-Mode: Joystick fire button was reported as not working (discord)
* Fixing keyboard layout. See https://github.com/eilers/c128Mega65/issues/1