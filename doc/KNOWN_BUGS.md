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
* Real cartridges in the expansion port (C64 and C128 cartridges)
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
* Expansion port on R3/R3A/R4 boards: those boards drive the slot's RESET line
  output-only, so a cartridge cannot reset the C128. Freezer/menu buttons that work
  by pulling RESET therefore do nothing. The EasyFlash 3 is detected by
  `cartridge_heuristics.vhd` and gets a synthesized reset, but only for its Menu,
  EasyFlash, and C64 modes — Kernal mode is refused on purpose, because the EF3
  drives A14 itself and would fight the MEGA65's address transceiver. R5/R6 sense
  the real RESET and need none of this.
* Saved settings live at `/c128/c128mega65-<version>.cfg` now, and the filename carries
  the core version, so a settings file from an older release is simply not found instead
  of being found with the wrong length. Copy the `.cfg` shipped with the release to the
  SD card to get saved settings back after upgrading.
* Virtual drives are newly added and took four rounds of fixes, all invisible from the
  outside:
  1. "Device not present" while mounting worked: the drive-ROM handover in `mega65.vhd`
     re-read its address in the same cycle as the write strobe, shifting the whole DOS
     image by one byte. `iecdrv_rom` reports `rom_valid` for a corrupted image, so nothing
     complained. Covered by `CORE/sim/tb_drive_rom.vhd`.
  2. `DS$` empty, no directory output, and the IEC bus stalled so that a real drive on the
     physical port stopped working too: `iecdrv_mem` (the drives' work RAM, and the same
     mistake in `iecdrv_trackmem`) registered the address and write enable one clock ahead
     of the data. Every caller strobes `wren` for a single cycle (`ena_r`, `ph2_f`,
     `buff_we`), so each write stored the byte that followed it — in practice the idle bus
     value. The DOS runs from ROM and therefore still answered ATN, which is why this
     looked like a disk-data problem rather than dead RAM. Covered by
     `CORE/sim/tb_iecdrv_mem.vhd`.
  3. A mounted `.D81` answered `73, CBM DOS V3.0 1571`, i.e. it stayed a 1571 and behaved
     exactly like a `.D64`. The image type never reached the core: `LOAD_IMAGE` in
     `M2M/rom/shell.asm` takes the type from `PREP_LOAD_IMAGE` into `R7`, then reuses `R7`
     as the high word of the progress bar counter and clears it again at end of file, so
     the type it returns — and that `VD_STROBE_IM` writes into `vdrives` — was always 0,
     which decodes to D64. The type now travels in the `LI_IMGTYPE` variable instead.
     This is framework code and affects every M2M core that uses more than one image
     type; C64MEGA65 never hit it because its 1581 is commented out in `iec_drive.sv`.
     `CORE/sim/tb_vdrive_mount.vhd` covers the hardware half of this path (it confirmed
     `vdrives` and the `img_type` decode were correct, which is what pointed at the
     firmware).
  4. With the `.D81` finally arriving as a 1581, `DIRECTORY` still returned nothing and
     `DS$` answered `74, DRIVE NOT READY` with the drive motor never running. The DOS
     polls that state through `floppy_ready = fd_ready && fd_present`, and `fd_ready`
     requires the disk to have span up. In `c1581_fdc1772.v` the generate block that
     instantiates the `floppy` models connects `.select(fd_any && fdn == i)` and
     `.motor_on(fd_motor)`, but `fd_any`, `fdn` and `fd_motor` are all declared *below*
     that generate block. Quartus resolves such references against module scope, so this
     works on MiSTer; Verilog instead creates an implicit net where an undeclared
     identifier is used in a port connection, and inside a generate block that net is
     local to the block. On Vivado every `floppy` therefore ran with its own private,
     undriven `select` and `motor_on` (xsim shows them as `x` and `z`), the disk never
     span up and `ready` could never assert. Moving the declarations above the generate
     block fixes it. Confirmed on hardware: with the ready chain colour-coded onto the
     drive LED, mounting a `.D81` now walks red (no disk) to blue (no motor) to yellow
     (spinning up) to green (ready), and back to blue when the DOS lets the motor time
     out. Covered by `CORE/sim/tb_c1581_ready.sv`, which also had to give
     `floppy.v`'s registers explicit power-up values and turn the sensitivity-less
     `always` block driving `fdn` into `always @*` — the latter is an infinite
     zero-delay loop that hangs any simulator at time zero, which is why this module had
     never been simulated.

  5. The `.D81` was then ready, identified correctly and could read a sector — the
     colour-coded drive LED went green, meaning a full 512-byte block reached the drive
     CPU without `RECORD NOT FOUND` — yet `DIRECTORY` returned nothing, `DS$` answered
     `74, DRIVE NOT READY`, and `LOAD"$",8` in C64 mode answered `?FILE NOT FOUND`. The
     cause is the same Verilog rule as in round 4, in the same file, but this time it hit
     `data_in`: the FDC's data register, declared as `reg [7:0]` *below* the `fifo`
     instance whose `.data_b(data_in)` connection names it first. Vivado created a 1-bit
     implicit net and ignored the 8-bit declaration, so every byte the drive CPU wrote to
     the FDC data register kept only bit 0 — including `step_to <= data_in`, the target of
     a SEEK. The drive could physically only reach track 0 and 1. Everything the DOS does
     on the disk lives on track 40 (header, BAM, directory), so the drive answered ATN,
     reported its DOS version out of ROM and could read the one track it was already
     sitting on, while every real access failed. Three more truncated vectors came from the
     same rule and are fixed with it: `track_fdc` in `c1581_drv.sv` (the FDC's track
     number, 8 bits), and `gcr_do` plus `track` in the 157x (see the `.D64` entry), and the
     SID's combined-waveform table outputs in `sid_top.sv`.

     What made this expensive: the drive-LED diagnostic was sticky since mount, so it
     saturated at green on the drive's very first read and then reported nothing about the
     failing directory access. The synthesis log had been naming every one of these signals
     all along, as `WARNING: [Synth 8-8895] '<name>' is already implicitly declared`.
     `rtl_elab_check.tcl` now promotes that message to an error and `build_bitstream.tcl`
     refuses to package a bitstream whose synthesis log contains it, because no simulation
     can catch this class of bug: xsim resolves names against the whole scope, so the
     testbenches saw correct 8-bit signals and passed.

  Ruled out along the way, all by reading the RTL rather than by measurement:

  * The request geometry. `iec_drive.sv` shifts the 1581's 512-byte LBA left by one for our
    `BLKSZ=1` 256-byte blocks and asks for `sd_blk_cnt=1`, which `vdrives` turns into
    `VD_SIZEB` = 512 bytes, transferred in a single acknowledge cycle.
  * The width of the sector-buffer address, which is 9 bits from `iec_drive.sv` down to the
    FDC, i.e. wide enough for 512 bytes.
  * The host-side signal mux in `iec_drive.sv`, which routes `sd_rd`, `sd_lba` and the
    buffer from either the 157x or the 1581 depending on `img_hd`. A `.D81` decodes to
    `img_hd = 1`, which also holds the 157x in reset, so the two cannot compete for the
    host.
  * The QNICE-to-core clock crossing on the sector buffer. `sd_buff_wr` stays asserted for
    hundreds of core cycles with address and data stable, so the core clock simply writes
    the same byte to the same address several times.
  * The whole path from the FDC's request to the byte arriving at the drive CPU, now
    covered by `CORE/sim/tb_c1581_ready.sv` with a faithful model of the QNICE side (see
    below). It passes: one request, 512 correct bytes in the sector buffer, 512 correct
    bytes fetched by the CPU, no `RECORD NOT FOUND`, command completed.

  Two things found along the way that are worth knowing but were not the 1581 bug:

  * `HANDLE_DRV_RD` in `M2M/rom/shell.asm` means to lower the buffer write enable again
    after each byte, but wrote `XOR 0, R9`, which computes `R9 xor 0` and therefore left it
    at 1. The write enable went high on the first byte of a block and stayed high until the
    acknowledge dropped. For the 1581 that is harmless — the firmware writes the address
    before the data, so the RAM briefly stores the previous byte at the new address and then
    the correct one — but it does mean that counting write strobes is not a way to measure
    how much of a block arrived, and an earlier drive-LED diagnostic did exactly that and
    reported "short block" for a transfer that was in fact complete. It is *not* harmless
    for the 157x: `c157x_heads.sv` holds the head machine's bit counters in reset while
    `sd_buff_wr` is high, which is meant to park the head during a track load but with a
    stuck strobe parks it forever. Now fixed to `XOR R9, R9`. This is framework code, so it
    affects every M2M core whose drive keys anything off the write strobe.
  * Vivado's *simulator* treats a variable declared inside an `always` block as automatic
    and re-initialises it on every clock edge, so `c1581_fdc1772.v`'s delayed copies and
    state variables never retained anything: `sd_ackD` could not see the falling edge of
    `sd_ack`, the FDC stayed in `SD_READ` forever, and `data_transfer_state` re-requested
    the same sector endlessly. Vivado's *synthesiser* does not do this — the flashed
    bitstream contains real `sd_ackD_reg`, `data_transfer_state_reg` and `seek_state_reg`
    flip-flops — so this was a simulation artefact that made the gate lie. The
    declarations are now at module scope, which is what the code always meant, and it is
    what makes the data-path gate above possible at all.

  Still unverified on hardware: write back to the SD card, whether an unmounted (and
  therefore reset-held) emulated drive really stays electrically silent on the IEC bus, and
  coexistence with a real drive on the physical port.
* `.D64`/`.D71` directory listing is hardware-qualified on MEGA65 R6: 1541 and 1571
  modes, devices 8 and 9, with the drive LED on during the read. Two hardware-only
  defects had to be bypassed in `c157x_logic.sv` without touching vendored VIA VHDL:
  a stale VIA1 T1 flag on `$180D` (1541 serial receive took the EOI path too early)
  and an unlatched sector-GCR byte-ready pulse that the 1571 DOS never sampled at
  2 MHz. Writes back to the SD card, an unmounted drive staying silent on the bus,
  and coexistence with a real IEC drive are still unverified. Deterministic images
  and the remaining checklist are under `test/157x-rw/`. Do not interpret this as
  support for `.G64`, `.G71`, or 1571 MFM/CP-M images.

# Planned
* D71 `disk_present` vs `ch_timeout`: `c157x_drv.sv` raises `disk_present` when
  `ch_timeout[24:23] = 00` so the 1571 side-1 probe does not run while GCR is still
  forced busy. Dual-head track cache may already make that unnecessary. Confirm on
  hardware (mount D71, `U0>M1`, `LOAD"$"` immediately) before deleting the special case.
* VIA T1 IFR on `iecdrv_via6522`: native 1541/1571 serial currently masks a stale
  timer-1 flag in `c157x_logic.sv` (`via1_t1_guard`) because the vendored 6522 can
  keep IFR bit 6 set after a T1 load under Vivado. That bandage should move into
  the VIA itself so DOS reads the real flag. Do not remove the mask until a VIA
  unit test and a hardware 1541 EOI/LOAD"$" pass replace it.

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
* Virtual drives: raw GCR images (`.G64` / `.G71`) and `.T64` tape images. The 2-bit
  image type the M2M framework carries is fully used by D64, D71 and D81, so raw GCR
  would need a framework change. Also missing: the MiSTer "Always" and "Never" drive
  enable modes (a drive is enabled exactly while an image is mounted) and the
  track-number overlay (`drv_overlay.sv`).
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
* Fixing keyboard layout. See https://github.com/eilers/c128Mega65/issues/1