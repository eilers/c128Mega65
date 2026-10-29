# Ultimate Command Interface (UCI)

The command interface of Gideon's Ultimate cartridges, at `$DF1C`-`$DF1F`. A C128 or C64
program writes a command into a queue, the system CPU's firmware answers with data and a
status text, and the program reads them back. Programs written for an Ultimate II+ or an
Ultimate 64 (for example those built on xlar54's cc65 `ultimateii-dos-lib`) use it for
files on the card, disk mounting and, on an Ultimate, the network.

Switch it on with **UCI at $DF1C** in the menu (off by default, as on the Ultimate).

The REU submenu's **File** item picks the REU's image file (`.reu`), the Ultimate's "REU
Preload Image": picking one loads it into the REU at once, as much as the REU holds, and
LOAD_REU and SAVE_REU use it from then on. The REU must be on first. The core forgets it at
power-off, as it does disks and cartridges.

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
needs no wait. A reset of the C128 sets a flag the firmware sees (word `0x010` bit 0), and
the firmware can ask for one (bit 1, for REBOOT): `main.vhd` runs it as it runs the reset
after a changed REU or cartridge setting, once no drive is writing, as the reset button's.

The REU's memory, for LOAD_REU and SAVE_REU: the system CPU reaches SDRAM at `0x50000000`
(`rv_soc.vhd`, R4 and later), as a third master of `sdram_arbit.vhd` after the cartridge and
the REU, in the REU's window; HyperRAM it already reaches at `0x40000000`. `uci.vhd`'s word
`0x011` says where the REU's memory starts: bit 15 SDRAM, bits 12-0 its 8 KiB window
(`C_SMAP_REU`, or `C_HMAP_REU` on R3 memory). The firmware's side is `CORE/firmware/reu.c`.

The files go by DMA where they can: `reu_sd.vhd` (device `C_DEV_REU_SD`, `0x010A`) moves SD
card blocks straight between the card and the REU's memory through the SD controller's
engine port, with multi-block commands, behind the drives and a .crt load. Its memory port
is a fourth master of `sdram_arbit.vhd` (in the REU's window) or, on R3 memory, shares the
modem's turn at HyperRAM, so the REU's own path keeps its timing. The firmware hands it
the file's clusters from FatFs's fast-seek map, 32 KB a step; a file in more than 32
pieces, an odd REU address, a position inside a block or a save's last bytes of a block go
through the CPU as before. A save takes its clusters first (as far as the card has room).
On the R6, 16 MB took about 18 s to save and 14 s to load through the CPU, over UCIREU's
13 s.

The REU's read cache (`avm_cache`, one line of 16 bytes) doesn't see the system CPU's or
the DMA's writes, so after them it drops its line (`inval_i`, from `reu_sd.vhd` on the core's
clock): a DMA load does it before it reports done, once a read behind its writes was
answered; the firmware asks for it after its own writes (word `0x009`). Before this the
first REU fetch after a LOAD_REU could get 16 old bytes (UCIREU's "REU $2000 HOLDS $1000'S"
on the R6).

A REU setting LOAD_CONFIG makes doesn't reset the C128 (on an Ultimate the REU changes under
the running program; `main.vhd` resets for a change in the menu). `uci.vhd`'s word `0x012`
goes through the same crossing as the protocol's registers: bit 0 (keep) is on the C128's
side once the firmware's write returns, and `main.vhd` passes REU changes while it is on; a
read gives `main.vhd`'s REU setting in bits 2-1. The firmware sets keep, changes the menu's
REU item, waits until `main.vhd` has the new setting, and clears keep. The setting is the
menu's bits, priority-encoded, so once it shows the new one the bits still on their way
can't change it again.

Drive power, for the control target: `vd_sd.vhd`'s CSR word `0x034`, one per drive, holds
0 automatic (on while it has a disk or is the internal floppy, as before), 1 on (with no disk
too, MiSTer's "Always") or 2 off (with a disk too). It survives a C128 reset and goes back to
automatic with an M2M reset. `main.vhd` brings it into the drives' clock and their reset;
bit 2 reads whether the drive runs. `main.vhd` decodes the
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
| 1, 2 | C128MEGA65 DOS (`uci_dos.c`, identifies as `C128MEGA65 DOS V1.0`), with the commands, replies and status texts of Gideon's Ultimate DOS | see below |
| 3 | Network (`net/uci_net.c`, identifies as `C128MEGA65 NETWORK INTERFACE V1.0`), with the commands, replies and status texts of Gideon's `network_target.cc` | see below |
| 4 | Control (`uci_ctrl.c`, identifies as `C128MEGA65 CONTROL V1.0`), with the commands, replies and status texts of Gideon's `control_target.cc` | see below |

### C128MEGA65 DOS

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
| `$21` LOAD_REU, `$22` SAVE_REU | The open file, from where it is, into the REU's memory at an address, or from there into the file: 3 bytes of address and 4 of length, little-endian, as his (24 and 25 bits). What would pass the end of the REU's memory is cut off with `02,REQUEST TRUNCATED`: at 16 MB as on an Ultimate, at 512 KB on R3 memory. The data is his, `$  1000 BYTES LOADED TO REU $   100` (`SAVED FROM`), always 35 (36) characters. No file open: `85,NO FILE OPEN`. The REU needn't be on. |
| `$15` COPY_UI_PATH, `$25` SWAP_DISK | `99,FUNCTION NOT IMPLEMENTED` |

Not checked: that a file deleted or renamed is not an image in a drive. The drive keeps the
SD blocks it was given, so deleting a mounted image and writing other files can let the drive
write into them.

### Network

The MEGA65's Ethernet port, through the firmware's lwIP (the one the SwiftLink modem uses):
four sockets, TCP or UDP, each to one host and port (Gideon's: 16, less what his firmware
opens itself). A fifth gets `85,ERROR OPENING SOCKET`. Sockets are numbered 1 to 4. A C128
reset closes them. The numbers in his status texts are errno values, as newlib numbers
them (his firmware prints newlib's); the core uses the same numbers.

| Command | |
|---|---|
| `$01` IDENTIFY | |
| `$02` GET_INTERFACE_COUNT | 1 |
| `$04` GET_NETADDR, `$05` GET_IPADDR, `$06` SET_IPADDR | Interface 0: the MAC address (6 bytes); address, netmask and gateway (4 bytes each, as on the wire). SET_IPADDR sets them as his does, with DHCP still running. A wrong length: `81,INVALID PARAMS`; interface 1 or more: `82,PARAMETER(S) OUT OF RANGE`. `$03` SET_INTERFACE is `21,UNKNOWN COMMAND`, as his is commented out. |
| `$07` OPEN_TCP, `$08` OPEN_UDP | Port (2 bytes), then the host: a name or an address. The reply is the socket. The name is looked up and the connection made while the main loop goes on (the C128 sees the command busy). `84,UNRESOLVED HOST`, `85,ERROR OPENING SOCKET`, `11,ERROR ON CONNECT: n` (104 refused, 113 no answer, 118 no route). |
| `$09` CLOSE_SOCKET | A socket it didn't open: `12,ERROR ON CLOSE: 9`. |
| `$10` READ_SOCKET | Socket, length (2 bytes, up to 1472: `82` above). The reply is 2 bytes, the bytes it carries, then them; over 893 bytes it goes on in a second block (Data More). With nothing there it waits 40 ms (his `SO_RCVTIMEO`), then `02,NO DATA: 11` and -1 (`$FF $FF`). TCP gives what has come, up to the length; then `01,CONNECTION CLOSED BY HOST` (and 0) once the far end has closed, which closes the socket. UDP gives a datagram a read, and one longer than the length is cut, with `04,DATAGRAM TRUNCATED: n` (n its whole length); the rest is gone. A reply given up after its first block (ABORT) drops the rest too. |
| `$11` WRITE_SOCKET | Socket, then the bytes (up to 892). The reply is the bytes sent (2 bytes). TCP takes them all, waiting for room as acks come in; a socket it didn't open: `12,SEND ERROR: 9` and -1. |

What each socket received and the C128 hasn't read waits in a buffer of its own in the data
RAM (TCP's window, 2920 bytes; UDP datagrams that don't fit are dropped), so a program that
doesn't read never holds lwIP's buffers, which the modem and every other socket need.
One difference: a TCP READ of 0 bytes answers `00,OK` with 0; Gideon's takes lwIP's 0 as a
closed connection and closes the socket.

### Control

| Command | |
|---|---|
| `$01` IDENTIFY | `C128MEGA65 CONTROL V1.0` |
| `$03` FINISH_CAPTURE | `00,OK`: there is no tape capture to finish. |
| `$05` FREEZE | Opens the menu, as the Help key does; no status. |
| `$06` REBOOT | Resets the C128 as the reset button does (his `start_cartridge`: a cartridge starts again); no status. |
| `$08` LOAD_REU, `$09` SAVE_REU | As his REU preload: the REU's whole size from REU address 0 (there is no offset setting), from or to the file picked in the menu's REU submenu (File), else `/c128/preload.reu` (his default is `/Usb0/preload.reu`), so a first SAVE_REU makes it. SAVE_REU keeps the old file as `.bu`. The data is the result as 4 bytes (the bytes moved), then his message in upper case, for example `REU LOAD: LOADED 524288 BYTES TO $     0 FROM /SD/C128/PRELOAD.REU`. At least 5 bytes, or `81,INVALID PARAMS`; REU off: `84,REU NOT ENABLED` and -2; no file: `85,REU FILE CANNOT BE OPENED`, -1 and why. Done a step per main-loop round (32 KB by DMA, a few KB by the CPU). |
| `$11` DECODE_TRACK | 14 bytes, or `81,INVALID PARAMS`; then no sectors and `82,ERRORS ON TRACK`: the GCR it decodes is in an Ultimate's memory, and the core's drives take sector images. |
| `$20` EASYFLASH | Subcommand 0 with a bank and an address, or `81,INVALID PARAMS`. His clears the 8 banks of the bank's sector (bank & `$38`) of ROMH's chip if the address has bit 5, else of ROML's. Here the virtual EasyFlash's flash model erases that sector as the chip's own erase command does (`crt_flash.vhd`, through the cartridge's CSR words `0x024` and `0x025`), so its cache follows; `00,OK` once it is done (about 2 ms). With no EasyFlash in, nothing is erased and the answer comes at once. |
| `$28` GET_HWINFO | 0: `C128 for MEGA65`; 1: the cartridge's own SIDs (an Ultimate's UltiSIDs): none; else `81,INVALID PARAMS`. |
| `$29` GET_DRVINFO | 2, then type (0 1541, 1 1571, 2 1581), address (8, 9) and power of each drive. A D81 or the internal floppy makes a 1581, a D71 a 1571, else the menu's model. |
| `$30`-`$33` ENABLE/DISABLE_DISK_A/B | Drive power on or off (see Hardware); there is no command back to automatic. |
| `$34`, `$35` DISK_A/B_POWER | `on ` while the drive runs, else `off`. |
| `$40` GET_RAMDISKINFO | 8 zeros: no RAM disks. |
| `$50` LOAD_CONFIG | A settings file named from the root (`/temp/uci_config.cfg` without a name, which isn't on the card), read as his `ConfigIO::S_read_from_file` reads it: see below. `88,CANNOT OPEN CONFIG FILE` if it can't be opened. |
| Others: READ_RTC, ENCODE_TRACK, the Ultimate 64's palettes and SAVEMEM | `21,UNKNOWN COMMAND`, as on an Ultimate II. |

LOAD_CONFIG's file has `[store]` lines, each followed by its `item=value` lines; a line that
starts with `#` or `;` is a comment. Store and item names and choices are his, in any case,
with spaces around the name and choice taken off. The core is a machine with fewer stores,
items and choices than an Ultimate:

| Store | Item | Choices here |
|---|---|---|
| `C64 and Cartridge Settings` (or `Cartridge and ROM Settings`) | `RAM Expansion Unit` | `Disabled`, `Enabled` (no `GeoRAM Mode`) |
| | `REU Size` | `512 KB`, `2 MB`, `16 MB` (R3 memory: `512 KB`); while the REU is off it is kept for an `Enabled` later, as his item is; `Enabled` with no size known is his default, 2 MB (512 KB on R3 memory) |
| | `REU Preload Image` | a path under `/SD/`: the REU's file, as one picked in the menu (not loaded); empty: none, so `/c128/preload.reu` |
| | `Command Interface` | `Disabled`, `Enabled`; switched last, as once it is off the C128 can't read the reply |
| `Drive A Settings`, `Drive B Settings` | `Drive` | `Disabled`, `Enabled`: drive power off or on |
| | `Drive Type` | `1541`, `1571`: the menu's one model for both drives, so a second, other one in the file is an error; `1581` only for a drive that is one (the internal floppy, a D81), which stays one |
| | `Drive Bus ID` | `8` for A, `9` for B |
| `Modem Settings` | `ACIA (6551) Mapping` | `Off`, `DE00/NMI`, `DF00/NMI` (a SwiftLink: no IRQ choices) |
| | `Modem Interface`, `Hardware Mode` | `ACIA / SwiftLink`, `SwiftLink`: nothing to change |

As in his firmware: a store the machine doesn't have is skipped with `Line n: Store name '...'
not found; section ignored.`, an item with `Line n: Item '...' not found in this store [...];
ignored.`; neither is an error. Errors are a choice the machine doesn't have (`Line n: Value
'...' is not a valid choice for item ...`), a line outside a store, a line with no item name
or no `=`, and a Drive Bus ID that isn't a number (no log line, as in his). The reply's data
is the log, at most 511 bytes, empty when every line went in; the status `00,OK`, or `89,CONFIG
FILE HAD ERRORS`. After an error nothing changes: his firmware effectuates only a file
without one (it keeps the values, though, and they take effect with the next file that has
none; here they are dropped). Like his, a last line without its line end isn't read. The
changes are the menu's: saved with the settings when the menu next closes, as his are saved
only when asked. LOAD_CONFIG doesn't reset the C128 for a REU change (Hardware). An
Ultimate takes the interface off the bus for a few ms while a change of its C64 store takes
effect, before the reply (`set_emulation_flags`); the core keeps it on. A program has to
allow for that gap on an Ultimate: `test/uci`'s library checks `$DF1D` around each read of
`$DF1C` (test/uci/README.md, UCICFG).

## Status

7a (the transport, DOS `IDENTIFY` and `ECHO`) passes on the R6 (Jeff, 2026-10-27): every
UCITEST line. On his Ultimate II+L (firmware 3.14d) every line matched the core's except IRQ
BIT: `$DF1D` read `$C9` both times, because the IRQ came in 3.14e.

7b (C128MEGA65 DOS) passes on the R6 (Jeff, 2026-10-28): every UCIDOS line, on the core and on
the II+L (drive B on), with core UCI2-CB59269 and firmware `cb59269`. On the core also in C64
mode (UCITEST64, UCIDOS64), in FAST mode, and with the 6r3 core. With drive 9 empty, DRIVE 9
HEADER needed no retries: the drive starts at the mount and is ready when MOUNT answers.
GET_TIME showed 2000-01-01 until the R6's clock chip was set; the II+L answers 2145-12-31,
its clock registers all reading `$FF`.

7c's control commands and drive power pass on the R6 (Jeff, 2026-10-28): every UCICTRL line,
with core UCI3-44AAD93 and firmware `44aad93`. The II+L (3.14d) failed seven, each where his
firmware has changed since (`test/uci/README.md`); updated to 3.15a, it fails one. FREEZE
opens the core's menu and REBOOT resets the C128. Its first run hung at DRIVE B ON THE BUS:
a 1541 holds DATA for about a second from its reset, and an OPEN then waits for ever in the
KERNAL; UCICTRL now waits for DATA first.

7c's LOAD_CONFIG passes on the R6 and on the II+L (3.15a) (Jeff, 2026-10-28): every UCICFG
line, with core UCI7-DB34589 and firmware `8e6d6bc`, including STILL RUNNING after the REU
comes on. The II+L's first run failed four lines: the program wasn't waiting out the moment
an Ultimate takes the interface off the bus while LOAD_CONFIG's REU change takes effect
(`test/uci/README.md`, UCICFG); with `ea35e30` it passes too. Step 7 (UCI) is done; the
network target 3 comes with step 8.

8d (the network target) passes the host tests: UCINET against `host_net.c`'s peer, twice
with a reset between, in both modes, 11 firmware mutations caught. On the R6 (net8 core,
firmware `a800c60`, 2026-10-29): UCINET and UCINET64 pass every line against
`ucinet_peer.py` on the Mac; a second run after a reset finds 4 sockets again, without one 0.
The first run failed 17 lines on the program, not the firmware: its buffers past `$4000` read
back BASIC's ROM in C128 mode. Jeff's II+L (3.15a) passes UCINET and UCINET64 too, a reset
closing its sockets as the core's does; what differs is shown, not failed: two interfaces (the
first without an address), 5 sockets free, higher socket numbers, a shorter 40 ms wait. The
rates are the same on both, about 7.7 KB/s writing and 7.1 KB/s reading: the C128 is the limit.
ZBF OS's `pkg` uses the core's target 3 (Jeff, 2026-10-29). 8d done.

## Tests

- `CORE/scripts/run_uci_nvc.sh`: `tb_uci` drives `uci.vhd` from both sides, the C128's bus
  and the device bus, the way the core and `rv_soc.vhd` do: replies in one and two blocks,
  the 895-byte limit, no-reply commands, ERROR, ABORT, IRQ, DMA and TRIGGER, the reset flag,
  and word `0x012` (keep there when its write returns, the REU setting).
- `test_uci`'s LOAD_CONFIG part: his log texts and statuses for every item, error and
  skipped line, nothing changed after an error, the REU changed only under keep (its model
  of `main.vhd` takes the change a few reads later and counts one without keep as a reset).
  `test_prg` runs UCICFG with the REU off, 512 KB, 16 MB and R3's menu, its REU at `$DF00`
  following `main.vhd`'s setting, and fails a run the core would have reset.
- `make -C M2M/firmware host-test`: `test_uci` runs the firmware against a C model of the
  protocol, with Gideon's `uci_targets_test.py` transport cases and the replies his firmware
  gives.
- `make -C M2M/firmware host-test`: also `test_uci`'s DOS part, on a FAT32 image (every
  command, with Gideon's replies and statuses), and `test_prg`, which runs `test/uci`'s
  programs on a 6502 against the firmware. It fails a program that sends FILE_STAT without
  the name's 0 byte, which an Ultimate needs.
- `test/uci`: `UCITEST` and `UCIDOS` (and their C64 versions) for the hardware, to run on an
  Ultimate and on the core and compare.
- `test_prg` runs UCINET against `host_net.c`: the firmware's lwIP on a loopback, the peer
  `ucinet_peer.py` is on hardware, a DNS server and a web server, at 10.0.0.1. It runs the
  program twice with a C128 reset between (the sockets it left open must be gone), and fails
  a run in which a frame for the firmware found lwIP's pool empty.
