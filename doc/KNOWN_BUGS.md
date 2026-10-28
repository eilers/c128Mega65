# What works

* Sound
* Mega65 Keyboard
* Joystick Port
* IEC (Serial) bus (inclusive Burst Mode). The Help menu **Use IEC port** item
  defaults to on; turn it off to isolate the physical DIN from the virtual drives.
* The Help menu occupies the MEGA65 Help key, which is also the C128 Help key.
  Enable **Help menu on F13** (default off) to open the menu with F13 instead,
  so C128 software can use Help. Close the menu with F13, Help, or Close Menu.
* Virtual drives: device 8 and 9, mountable from `/c128` on the SD card. Standard linear
  `.D64`, `.D71`, and `.D81` images have FPGA-side sector paths. The menu picks 1541 or
  1571 for the 5.25" formats; mounting a `.D81` turns that drive into a 1581 regardless
  of the menu. Raw `.G64`/`.G71` and MFM/CP-M images are not supported.
* The MEGA65's internal 3.5" drive as a 1581 on device 8 or 9 (**Internal 1581** in
  the menu): reads, writes and formats DD disks at the speed of a real 1581. See
  [internal-floppy.md](internal-floppy.md).
* Real cartridges in the expansion port (C64 and C128 cartridges), including ones that
  take the bus with `/DMA`, such as the Ultimate II+ (menu, REU and command interface)
* Virtual cartridges: a `.crt` file of up to 2 MB, loaded with **Cartridge > CRT:** in
  the menu, runs as a cartridge in the expansion port. C128 function ROMs (generic) and
  these C64 types: generic 8K/16K/Ultimax, Action Replay, Final Cartridge III, Simons'
  BASIC, Ocean, Fun Play, Super Games, C64 Game System, Dinamic, Magic Desk (8K and 16K),
  Super Snapshot 5, COMAL 80, Structured BASIC, Mikro Assembler, EasyFlash, GMod2 and
  Turbo 2000. Other types are refused with "Unsupported type". Action Replay, Final
  Cartridge III, Super Games, Mikro Assembler and EasyFlash use the REU's registers at
  $DF00-$DF1F, so the REU is off while one of them runs, as on an Ultimate.
* REU (RAM Expansion Unit) of 512 KB or 2 MB, set in the menu (off by default)
* 40/70 Column mode (HDMI, audio: untested - please report)
* Dual screen: **Display Settings > Video** chooses what HDMI and VGA show. Follow 40/80
  (default), 40 columns and 80 columns put the same picture on both ports. HDMI 40, VGA 80
  and HDMI 80, VGA 40 show both chips at once, like a C128 with two monitors; the 40/80
  key then only picks the screen BASIC starts on. Swap with 40/80 puts the chip the key
  selects on HDMI and the other chip on VGA.
* Go64 and native C128 Mode
* Reset-Button

# Known Bugs

* C128-Mode: Core does not show a READY prompt when a device is on the IEC bus without power. This might be a normal behavior
  with a (or my) 1571. It blocks the check for a bootable disk.
* Expansion port on R3/R3A/R4 boards: those boards drive the slot's RESET line
  output-only, so a cartridge cannot reset the C128. Freezer/menu buttons that work
  by pulling RESET therefore do nothing. The EasyFlash 3 is detected by
  `cartridge_heuristics.vhd` and gets a synthesized reset, but only for its Menu,
  EasyFlash, and C64 modes — Kernal mode is refused on purpose, because the EF3
  drives A14 itself and would fight the MEGA65's address transceiver. R5/R6 sense
  the real RESET and need none of this.
* During a cartridge DMA, BA and R/W share a level shifter, so the core drives R/W
  high while it shows the cartridge BA: in PHI1, and while BA is low, until ~190 ns
  into PHI2. An Ultimate II+ DMA write while BA is low fights it for ~130 ns, and a
  cartridge that drives R/W low in PHI1 fights it there.
* A DMA cartridge gets only the first two cycles after BA falls, as the Ultimate II+
  uses them. A VIC-II leaves the CPU three, but the core ignores a cartridge's access
  in the third and drives R/W high through it.

# Planned

* VIA T1 IFR on `iecdrv_via6522`: native 1541/1571 serial currently masks a stale
  timer-1 flag in `c157x_logic.sv` (`via1_t1_guard`) because the vendored 6522 can
  keep IFR bit 6 set after a T1 load under Vivado. That bandage should move into
  the VIA itself so DOS reads the real flag. Do not remove the mask until a VIA
  unit test and a hardware 1541 EOI/LOAD"$" pass replace it.

# Missing Features

* Video:
  * "Flicker-free" has no effect in 80-column mode.
  * A smooth HDMI filter without scanlines for 80 columns. CRT emulation's polyphase
      filter does the smoothing, but its coefficients are hard-coded in the framework.
  * "HDMI: Zoom-in": removed from the menu, `qnice_zoom_crop_o` is hardwired to
      '0' in `mega65.vhd`. `crop.vhd` uses fixed VIC-II geometry (border 33/35,
      image 320x200), so it has to become source-aware before the menu item can
      come back and be wired for the VDC.
  * "Audio improvements": removed from the menu, `qnice_audio_filter_o` is
      hardwired to '0' in `mega65.vhd`
  * ...
* Virtual drives: raw GCR images (`.G64` / `.G71`) and `.T64` tape images. The 2-bit
  image type the M2M framework carries is fully used by D64, D71 and D81, so raw GCR
  would need a framework change. Also missing: the MiSTer "Always" and "Never" drive
  enable modes (a drive is enabled exactly while an image is mounted) and the
  track-number overlay (`drv_overlay.sv`).
* Virtual cartridges: freezers (Action Replay, Final Cartridge III, Super Snapshot 5)
  run, but there is no freeze button yet. EasyFlash saves to its flash last until the
  cartridge is loaded again or the MEGA65 is switched off; they are not written back to
  the SD card yet. The banked C128 types (Warp Speed 128, COMAL 80 128, Magic Desk 128,
  GMod2-C128) are not supported.
* Expansion port: a cartridge can take the bus with `/DMA` (for example the Ultimate
  II+ menu) only while the 8502 runs. A real C128 stops its Z80 (CP/M mode) for `/DMA`
  too (PRG p. 636), but the core's Z80 keeps running and the cartridge's accesses are
  ignored.
  DMA to another cartridge's I/O area ($DE00-$DFFF) does not reach it, because
  IO1/IO2 share a level shifter with R/W, which the cartridge drives.
