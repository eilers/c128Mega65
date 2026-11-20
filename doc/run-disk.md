# Run Disk

In a drive's file browser, **F7** mounts the disk image under the cursor, closes the menu,
resets the C128 and starts the disk. **F8** (Shift+F7) does the same in C64 mode. Return
only mounts, as before. Assembly 64's file screen has the same two as "Run, from drive A"
and "Run in C64 mode, from drive A" (`doc/assembly64.md`).

- `CORE/firmware/net/rest_runners.c`: `runner_disk`, on the steps of REST's `run_prg`.
- `CORE/firmware/diskimg.c`: `diskimg_start` reads what decides the mode.
- `M2M/firmware/fw/browse.c`, `fw/core.h`: the two keys (`core_run_label`, `core_file_run`).
- `M2M/firmware/host/test_http.c` has the runner on a model of the C128, `test_shell.c` the
  keys and the menu.

## Which mode

The disk decides. C64 mode only when nothing on it says C128:

| The image has | What happens after the reset |
|---|---|
| A boot sector (`CBM` at track 1, sector 0) | Nothing is typed: the C128's KERNAL boots it. CP/M and most commercial C128 disks |
| A first program that loads at `$1C01` (C128 BASIC; also `$4001`) | C128 mode, `RUN"*"` |
| Anything else | C64 mode, `LOAD"*",8,1` and `RUN`, as the Ultimate's Run Disk |

A 1581's CP/M disk has no boot sector on track 1: CP/M's directory is there. It has the
1581's autoboot file (`COPYRIGHT CBM 86`, type USR), code for the drive that hands the C128
track 40, sector 5 when it asks for the boot sector. An image with that file and `CBM` in
that sector counts as one with a boot sector.

F8 is for a disk that carries both: a boot sector for the C128 and a C64 program first
(Zork I). It goes to C64 mode whatever the disk holds.

Checked on 131 disk images from Assembly 64 (every fifth title of the first pages of a
search, 2026-11-19):

| Disks | Boot sector | First program at `$1C01` | at `$0801` | Other |
|---|---|---|---|---|
| 31 of the C128 category | 12 | 16 more | 1 | 2 |
| 100 C64 games and demos | 1 | 0 | 92 | 7 |

The rule starts 28 of the 31 as C128 disks. The three others: one has its C64 version
first, one a picture, on one no program was found. No C64 disk is taken for a C128 one.

## How it starts

- The disk's `NAME.cfg`, next to the image, is loaded first and stays until the next start,
  as a `.crt`'s and `run_prg`'s (`doc/rest.md`). A plain mount loads none: a setting such
  as the REU's size resets the C128.
- The reset waits until the drive has the disk. If CP/M runs, a reset comes first: the
  steps need the 8502.
- C64 mode: the C= key is held over the reset, so the C128 comes up in C64 mode and nothing
  boots first. If C128 BASIC comes up all the same, `GO64` is typed.
- C64 mode's keys: `LOAD"*",8,1` is 12 characters and the keyboard buffer holds 10. So the
  line is written on the screen at the cursor, and the buffer gets Return, `RUN`, Return, as
  the Ultimate's boot cartridge does it. `RUN` waits in the buffer while the disk loads. A
  program that starts itself never gets to it.
- The device number is the drive's bus ID. In C128 mode a drive that isn't 8 gets
  `RUN"*",U9`, or `BOOT U9` for a boot sector.
- If BASIC doesn't come within 10 s the menu opens and says so: a cartridge, or a boot disk
  in drive 8 when the other drive's disk was to run.

REST has no call for it: the Ultimate's API has none.
