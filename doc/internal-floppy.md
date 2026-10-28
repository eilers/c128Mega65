# The internal floppy drive as a 1581

The MEGA65's built-in 3.5" drive can be drive 8 or drive 9 of the C128, as a
Commodore 1581. It reads, writes and formats standard 1581 disks (DD, 80
tracks, 2 sides, 10 sectors of 512 bytes) at the speed of a real 1581.

Tested on an R6 against a real 1581 (October 2026): a disk formatted and
written on the MEGA65 reads on the 1581 and the other way round, and
`HEADER"NAME",I01,U8` takes 1 min 30 s on both.

## Using it

Help menu > **Internal 1581** > **Drive 8** or **Drive 9**. The drive's line in
the menu then shows `Internal 1581`, and any disk image mounted on it waits
until the setting goes back to **Off**. Choosing an image for that drive also
switches the setting back to **Off**. Changing the setting resets that drive,
as mounting does, so wait until its LED is off.

The write-protect tab is the only guard: with the hole open the drive answers
`26, WRITE PROTECT ON`, with it closed the C128 can write and format. Use DD
disks; HD disks with a taped hole are not supported.

Not supported: Read Track (it finishes without data, as with disk images),
copy-protected or deliberately malformed disks, HD/ED disks, CMD FD-2000 disks.

## Testing it on hardware

With a DD disk in the internal drive set to Drive 8, mount
`test/1581-rw/1581-rw-test.d81` on drive 9, `LOAD"RWTEST",9`, `RUN` and answer
8. Then format a disk in each drive (`HEADER"CORE",I02,U8`), save a program on
it and read it back in the other drive.

## How it works

The work is C64MEGA65's (issues #90 and #244, sy2002). At their commit 61fdbb2
reading had been in test builds since July 2026 and sector writing since
October 2026; formatting had only been tested in simulation. The 1581
emulation stays as it is; only its WD1772 talks to the real drive instead of
the image:

- `CORE/vhdl/physical_1581/` runs on the QNICE clock and owns the floppy pins:
  input conditioning, MFM decoder and encoder with CRC, head stepping, the
  writer with its safety interlocks. Copied unchanged.
- `c1581_fdc1772.v` got the physical-mode branch of C64MEGA65's `fdc1772.v`:
  each WD command becomes a request to the controller, read bytes go to the
  drive CPU as they come off the disk, writes go through a 4-byte FIFO. Image
  mode is unchanged.
- One difference from C64MEGA65: they hold each sector until its CRC is known
  and then hand it over at disk speed, so the DOS misses the next sector and
  every sector costs a revolution (a track side about 2 s instead of 0.2 s,
  a format about 6 min instead of 1:30).
  We stream the bytes as the real WD1772 does; a bad sector still ends with a
  CRC error and the DOS retries it, as on a real 1581. A read starts only once
  the read FIFO is empty, so bytes left by a cancelled read can't slip in.
- `c1581_drv.sv` takes the CIA's disk change and write protect from the real
  drive, `c1581_multi.sv` lets either drive own the one controller, and
  `iec_drive.sv` turns that drive into a 1581 and stops its image requests.
- `main.vhd` decodes the menu, resets a drive whose mode changes, and holds
  the controller, its two FIFOs and the diagnostic device.

Two test benches:

- `CORE/sim/tb_c1581_physical.sv` is C64MEGA65's bench for the WD side with a
  mock controller, pointed at our module and adapted to streaming
  (`CORE/scripts/run_c1581_physical_sim.tcl`).
- `CORE/sim/tb_c1581_track.vhd` runs our WD with the real controller and
  C64MEGA65's spinning disk model (`CORE/sim/physical_1581/`) and reads a track
  side the way the DOS does; it must be byte-exact and take about one
  revolution (`CORE/scripts/run_c1581_track_sim.tcl`).

## Diagnostics

`physical_1581_diag` is QNICE device `0x0108`, as in C64MEGA65, so their
register map (`doc/1581_dd_debug_device.md` there) applies. It shows pin
levels, the controller state, the last sector result, index timing and error
counters, and can be read from the QNICE monitor over the JTAG UART.
