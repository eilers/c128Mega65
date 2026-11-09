# CMD HD

The core has a CMD HD on the IEC bus, next to drives A and B. It is emulated as a whole
drive, as VICE does it: the real boot ROM runs on the drive's own 6502, and HD DOS loads from
the disk image. One SCSI disk, at ID 0, is the image file on the SD card.

## What it needs

| File | Where | Notes |
|---|---|---|
| CMD HD boot ROM | `/c128/roms` | 16 KB, or a 32 KB dump (two copies). Version 2.80 is the last. Not included: CMD's. |
| A disk image | `/c128/hd` | `.dhd` (VICE) or `.hda`/`.hdd` (ZuluSCSI, BlueSCSI): a raw SCSI disk, up to 4000 MB |
| HD UTILITIES disk | anywhere | Only to set up a new image: CREATE SYS, HD-TOOLS. Not included: CMD's. |

## The menu

**CMD HD** on the main menu shows the bus ID and the image, as the drive lines do. Its submenu:

- **Image**: pick an image; Space takes it out; F5 in the browser makes a new one (below).
  Picking an image turns the drive on.
- **Drive enabled**: on the bus or not (default on; without an image or boot ROM it stays off).
- **Bus ID**: 8 to 12 (default 12, as CMD's). The DOS keeps its device number on the disk; the
  core puts the menu's into it as the DOS reads it, so the menu is always right. Two drives never
  share an ID: the menu moves the other one.
- **Boot ROM**: the boot ROM file; Space takes it out.
- **Press SWAP 8 / SWAP 9 / WRITE PROTECT**: the front panel's buttons, pressed for a moment.
- **Reset: config mode** (WRITE PROTECT held) and **Reset: install mode** (SWAP 8 and SWAP 9
  held): both put the drive at device 30 for CMD's tools.

The settings file keeps all of it in `CMD HD Settings` (`Drive`, `Drive Bus ID`, `Disk Image`,
`Boot ROM`).

## A new image

1. In the Image browser press F5 and type a name; `name,128` makes 128 MB (16 to 4000, default
   256). The core makes the file and mounts it. It is empty, so the drive starts in install mode
   at device 30.
2. Mount the HD UTILITIES disk in drive A, `LOAD"CREATE SYS",8` and RUN. Press RETURN twice
   (the system's start block, 384, is the usual), then Y to clear the area below it. CREATE SYS
   writes the system and HD DOS to the disk.
3. Reset the drive (the menu's Drive enabled off and on, or a C128 reset), then run HD-TOOLS to
   make partitions.

An image from a real CMD HD (dd, ddrescue, or a ZuluSCSI in initiator mode) works as it is.

## How it is built

- `CORE/vhdl/cmdhd/`: the controller board on the core clock (`cmdhd_drive.vhd`: T65 at 2 MHz,
  64 KB RAM, two VIAs, the 8255, the RTC-72421, the U20 latch), the SCSI bus phases
  (`cmdhd_scsi.vhd`), and the device for the firmware with the SD controller's engine client
  (`cmdhd.vhd`). The RTC shows the MEGA65's own clock.
- `CORE/firmware/cmdhd.c`: the disk's SCSI commands (VICE's `scsi.c`), block lookup through
  FatFs's cluster map, the bus ID patch, power and the front panel. The firmware answers each
  command between other work; the DOS waits on REQ as long as it takes.
- No parallel cable: the 8255's cable lines read as VICE's do with nothing attached, so a
  RAMLink could be added later.

Tests:

- `CMDHD_ROM=<boot ROM> CMDHD_IMG=<image with a system> CORE/scripts/run_cmdhd_nvc.sh` runs the
  real boot ROM to HD DOS's idle loop (about 7 minutes).
- `make -C M2M/firmware host-test` includes `test_cmdhd`: the firmware against a model of the
  device, on a card image.
