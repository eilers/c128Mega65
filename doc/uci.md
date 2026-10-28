# Ultimate Command Interface (UCI)

The command interface of Gideon's Ultimate cartridges, at `$DF1C`-`$DF1F`. A C128 or C64
program writes a command into a queue, the system CPU's firmware answers with data and a
status text, and the program reads them back. Programs written for an Ultimate II+ or an
Ultimate 64 (for example those built on xlar54's cc65 `ultimateii-dos-lib`) use it for
files on the card, disk mounting and, on an Ultimate, the network.

Switch it on with **UCI at $DF1C** in the menu (off by default, as on the Ultimate).

## The C128's side

As Gideon's *Ultimate Command Interface Register API* describes, protocol for protocol:

| Address | Read | Write |
|---|---|---|
| `$DF1B` | SoftIEC bus id (0) | |
| `$DF1C` | status: DATA_AV, STAT_AV, state, ERROR, ABORT_P, DATA_ACC, CMD_BUSY | control: DMA, TRIGGER, IRQ, CLR_ERR, ABORT, DATA_ACC, PUSH_CMD |
| `$DF1D` | identification, `$C9` (`$49` while its IRQ is on) | command queue (896 bytes; 895 are kept) |
| `$DF1E` | response queue (896 bytes) | |
| `$DF1F` | status queue (256 bytes) | |

The interface claims `$DF18`-`$DF1F`, as Gideon's does; `$DF18`-`$DF1A` read `$FF`. That
range is the REU's unused registers, which read `$FF` as well, so the REU behaves the same
with the interface on. The interface answers ahead of the REU and of a cartridge in the slot,
so turn it off to reach a real Ultimate's own. It is off while the virtual cartridge uses IO2
(EasyFlash and others), as the Ultimate's firmware does for a cartridge that rules it out.

Its IRQ goes to the C128's IRQ; its DMA bit (and TRIGGER, at the next write to `$FF00`)
holds the 8502 the way a cartridge's /DMA does, until the reply is there.

## Hardware

`CORE/vhdl/uci/`: Gideon's `command_protocol.vhd` and its packages, unchanged (README.md
there names his commit), on the core's clock. `uci.vhd` takes the place of his
`command_interface.vhd`: the 2 KB queue buffer is a dual-clock block RAM, and the firmware
reaches it and the protocol's registers as device `C_DEV_UCI` (`0x0109`) on the device bus.
Register accesses cross to the core's clock as toggles, so they wait about 200 ns; the buffer
needs no wait. A reset of the C128 sets a flag the firmware sees. `main.vhd` decodes the
addresses and lays the interface over the REU and the cartridge port; `mega65.vhd` holds it.

## Firmware

`CORE/firmware/uci.c` polls the interface from the main loop, as Gideon's `command_intf.cc`
serves it: a new command goes to the target its first byte names (bits 0-3; bit 7 asks for no
reply), the reply goes into the queues as Data Last or Data More, DATA_ACC asks the target for
the next part, ABORT and a C128 reset end the reply. Targets nobody has registered answer
`IDENTIFY` with `NO TARGET` and everything else with `21,UNKNOWN COMMAND`, as his do. A
target can answer later: it leaves the status empty, and its `work()` is called from the main
loop until the reply is ready, while the C128 sees the command busy. A long job, such as a
copy, so never stops the loop.

| Target | What | State |
|---|---|---|
| 1, 2 | MEGA128 DOS (`uci_dos.c`, identifies as `MEGA128 DOS V1.0`), with the commands, replies and status texts of Gideon's Ultimate DOS | see below |
| 3 | Network | with the network (step 8) |
| 4 | Control: identify, reboot, drive information | to do |

### MEGA128 DOS

The card looks as an Ultimate shows its storage: `/` holds one folder, `SD`, the active card.
Each target has its own folder, open file and open folder. A C128 reset closes their files,
so what was written is on the card.

| Command | |
|---|---|
| `$01` IDENTIFY, `$F0` ECHO | |
| `$02` OPEN_FILE, `$03` CLOSE_FILE, `$04` READ_DATA, `$05` WRITE_DATA, `$06` FILE_SEEK | READ_DATA answers in blocks of 512 bytes. Mode bits as FatFs's (`FA_READ`, `FA_WRITE`, `FA_CREATE_NEW`, ...). |
| `$07` FILE_INFO, `$08` FILE_STAT | Size, FAT date and time, the extension's 3 characters, attributes, the name (up to 63 characters). FILE_STAT's name ends with the command, or at a 0 byte. Gideon's FILE_STAT doesn't end it (his other commands do), so on an Ultimate it runs on into what earlier commands left in the buffer, and a program has to send the 0 itself. |
| `$09` DELETE_FILE, `$0A` RENAME_FILE, `$0B` COPY_FILE | DELETE takes an empty folder too. COPY copies a file into a folder named from the root, as Gideon's `fcopy` does (not from the current folder), under the same name, a few KB per main-loop round; onto itself it is done at once. Folders are not copied. |
| `$11` CHANGE_DIR, `$12` GET_PATH, `$13` OPEN_DIR, `$14` READ_DIR, `$16` CREATE_DIR, `$17` COPY_HOME_PATH | READ_DIR answers an entry a block: attributes, then the name. The home folder is `/SD/`. |
| `$23` MOUNT_DISK, `$24` UMOUNT_DISK | Drive 8 or 9, or 0 for the one mounted last; D64, D71, D81 (G64 and G71: `91,INCOMPATIBLE IMAGE`). The menu shows the image at once. A drive that is the internal floppy answers `90,DRIVE NOT PRESENT`. MOUNT_DISK answers once the drive has the disk, as an Ultimate's answers after its disk insertion. The core's 1541/1571 acts out a disk change after a mount (`c157x_drv.sv`: no disk for 1.6 s, over at 2.1 s), and a read before then got no disk while the DOS retried for about 15 s. `iec_drive`'s `disk_ready`, in bit 5 of the drives' CSR (`vd_sd.vhd`), is low while that lasts; the reply waits to see it low and then high (a drive that never goes low didn't take the disk: `87,INTERNAL ERROR`). A D81 answers at once: the 1581 flags the change itself. Not yet measured: whether a drive that was empty, and so starts from reset at the mount, has its DOS running by then. |
| `$26` GET_TIME, `$27` SET_TIME | The MEGA65's clock; SET_TIME writes the RTC chip too. |
| `$15` COPY_UI_PATH, `$21` LOAD_REU, `$22` SAVE_REU, `$25` SWAP_DISK | `99,FUNCTION NOT IMPLEMENTED` (the REU ones come with the control target's) |

Not checked: that a file deleted or renamed is not an image in a drive. The drive keeps the
SD blocks it was given, so deleting a mounted image and writing other files can let the drive
write into them.

## Status

7a (the transport, DOS `IDENTIFY` and `ECHO`) passes on the R6 (Jeff, 2026-10-27): every
UCITEST line. On his Ultimate II+L (firmware 3.14d) every line matched the core's except IRQ
BIT: `$DF1D` read `$C9` both times, because the IRQ came in 3.14e.

7b (MEGA128 DOS) passes on the R6 (Jeff, 2026-10-28): every UCIDOS line, on the core and on
the II+L (drive B on), with core UCI2-CB59269 and firmware `cb59269`. On the core also in C64
mode (UCITEST64, UCIDOS64), in FAST mode, and with the 6r3 core. With drive 9 empty, DRIVE 9
HEADER needed no retries: the drive starts at the mount and is ready when MOUNT answers.
GET_TIME showed 2000-01-01 until the R6's clock chip was set; the II+L answers 2145-12-31,
its clock registers all reading `$FF`.

## Tests

- `CORE/scripts/run_uci_nvc.sh`: `tb_uci` drives `uci.vhd` from both sides, the C128's bus
  and the device bus, the way the core and `rv_soc.vhd` do: replies in one and two blocks,
  the 895-byte limit, no-reply commands, ERROR, ABORT, IRQ, DMA and TRIGGER, the reset flag.
- `make -C M2M/firmware host-test`: `test_uci` runs the firmware against a C model of the
  protocol, with Gideon's `uci_targets_test.py` transport cases and the replies his firmware
  gives.
- `make -C M2M/firmware host-test`: also `test_uci`'s DOS part, on a FAT32 image (every
  command, with Gideon's replies and statuses), and `test_prg`, which runs `test/uci`'s
  programs on a 6502 against the firmware. It fails a program that sends FILE_STAT without
  the name's 0 byte, which an Ultimate needs.
- `test/uci`: `UCITEST` and `UCIDOS` (and their C64 versions) for the hardware, to run on an
  Ultimate and on the core and compare.
