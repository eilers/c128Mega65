# UCI tests

`uci-test.d64` holds six tests of the Ultimate Command Interface at `$DF1C`-`$DF1F`, each
for C128 mode and for C64 mode: `UCITEST` / `UCITEST64` (the transport), `UCIDOS` /
`UCIDOS64` (the DOS target), `UCICTRL` / `UCICTRL64` (the control target), `UCIREU` /
`UCIREU64` (LOAD_REU and SAVE_REU), `UCICFG` / `UCICFG64` (LOAD_CONFIG) and `UCINET` /
`UCINET64` (the network target). They talk to the interface the way a program does. Each line
ends in PASS or FAIL, and names, paths and times are shown, so a run on a real Ultimate and
a run on the core can be compared line by line.

On the core: switch on **UCI at $DF1C** in the menu, mount the disk on drive 8,
`LOAD"UCITEST",8` and `RUN` (or `UCITEST64` after `GO64`), and the same for the others.

On an Ultimate II+ in the slot: the core's REU and UCI off (both answer at `$DF00`-`$DF1F`
ahead of a cartridge), **Cartridge: Hardware**, and the Ultimate's own Command Interface on.

| Line | What it checks |
|------|----------------|
| EMPTY COMMAND | A push with no bytes gets an empty reply and an empty status. |
| DOS 1 / DOS 2 IDENTIFY | The DOS's name, shown: `C128MEGA65 DOS V1.0` on the core, `ULTIMATE-II DOS V1.2` on an Ultimate. Status `00,OK`, and target 2 gives target 1's name. |
| CONTROL IDENTIFY | Target 4's name, shown: `C128MEGA65 CONTROL V1.0` on the core, `CONTROL TARGET V1.1` on an Ultimate. Any status starting `00`. |
| TARGET $0F IDENTIFY | A target nobody registered says `NO TARGET`, `00,OK`. |
| UNKNOWN COMMAND | `$01 $7F`: no data, `21,UNKNOWN COMMAND`. |
| ECHO 895 BYTES | DOS `ECHO` of the largest command the queue holds comes back whole. |
| NO-REPLY COMMAND | A command with bit 7 of its target byte set goes straight back to idle. |
| PUSH WHILE BUSY | A push while a reply waits sets ERROR; CLR_ERR clears it, and the reply (DOS 1's name) survives. |
| ABORT | ABORT ends a waiting reply. |
| IRQ BIT | With the IRQ bit, `$DF1D` reads `$49` once the reply is there and `$C9` after a response byte is read. It came with Ultimate firmware 3.14e; an older one reads `$C9` both times and fails this line. |
| DOS 1 IDENTIFY AGAIN | How many polls of `$DF1C` the reply took (about 60 us each at 1 MHz). |

## UCIDOS

Works on the first folder in the root whose name starts with S or U (`SD` on the core,
`Usb0` or `SD` on an Ultimate), or else on the first folder. It makes a folder `UCITEST`
there and removes it again at the end; a run that stops half way leaves it behind, and the
next run first deletes the files it left there. The C128 spends about 2 seconds at 1 MHz,
most of it writing the D64, plus whatever the card takes to store it.

| Line | What it checks |
|------|----------------|
| CD / , OPEN_DIR / | The root, and its folders, listed. |
| CD TO THE FIRST, GET_PATH | Into the chosen folder: `/SD/` on the core. |
| MKDIR UCITEST, CD UCITEST | A folder of its own (`FILE EXISTS` from an earlier run is fine). |
| OPEN TEST.BIN TO WRITE, WRITE 3 X 300 BYTES, CLOSE | A new file in three WRITE_DATA commands. |
| FILE_STAT: 900 BYTES | Its size from the folder. FILE_STAT names go with their 0 byte: an Ultimate's FILE_STAT doesn't end the name, so without it the name runs into older bytes in its buffer and the file isn't found. |
| OPEN TO READ, READ 900: 2 BLOCKS | Back in two replies, 512 and 388 bytes (Data More, then Data Last), byte for byte. |
| SEEK 700, READ 10 THERE | A seek, and the bytes there. |
| FILE_INFO: 900 BYTES | The open file's size. |
| RENAME TO TEST2.BIN, OLD NAME GONE | A rename; FILE_STAT of the old name says `82,FILE NOT FOUND`. |
| MKDIR SUB, COPY TO SUB: FROM THE ROOT | COPY_FILE names its folder from the root, as Gideon's `FileManager::fcopy` does, so a bare `SUB` is `/SUB`: `PATH DOESN'T EXIST`. |
| COPY INTO /.../UCITEST/SUB, THE COPY: 900 BYTES | A copy into the folder, by its whole path. |
| DELETE THE COPY, DELETE SUB | Deleting a file, and an empty folder. |
| OPEN_DIR, READ_DIR: TEST2.BIN | The folder lists the one file. |
| DELETE TEST2.BIN, EMPTY FOLDER | Then `01,DIRECTORY EMPTY`. |
| WRITE UCITEST.D64 | An empty, formatted D64 (174848 bytes, the disk name UCI MOUNT), written over the interface. |
| MOUNT ON 9, DRIVE 9 HEADER, UMOUNT 9 | Mounted on drive 9 (on the core the reply comes once the drive has the disk, about 2 s); `LOAD"$",9`'s first line, read over the IEC bus, names UCI MOUNT. It tries for about 4 seconds and says how many retries it took (a third of a second each), or why it failed: OPEN FAILED (nothing answered as device 9: the C128 KERNAL sends the name at OPEN), DEVICE NOT PRESENT (nothing answered at CHKIN), or the name it saw. |
| MOUNT ON 10, MOUNT NOTHING | `90,DRIVE NOT PRESENT`, `82,FILE NOT FOUND`. An Ultimate whose drive B is off answers `90` to both (it checks the drive first), and MOUNT ON 9 fails there too. |
| CD .., DELETE UCITEST | Tidying up. |
| GET_TIME | The clock, shown, with the day. |

## UCICTRL

The control target, 4. It leaves things as they were: each drive's power goes back to what
it was, nothing is erased, and SAVE_REU runs only while the REU is off (it would overwrite
the REU's image file). Drives A and B are the core's 8 and 9; on an Ultimate, its own.

| Line | What it checks |
|---|---|
| IDENTIFY | The target's name, shown: `C128MEGA65 CONTROL V1.0` on the core, `CONTROL TARGET V1.1` on an Ultimate. |
| GET_HWINFO: THE PRODUCT | The product, shown: `C128 for MEGA65` on the core. |
| GET_HWINFO 1: ITS SIDS | The cartridge's own SIDs (an Ultimate's UltiSIDs): a count, then 5 bytes each, shown in hex. The core has none. |
| GET_HWINFO 2 | `81,INVALID PARAMS`. |
| GET_DRVINFO | A count, then type (0 1541, 1 1571, 2 1581), address and power of each drive, shown in hex. An Ultimate adds its software IEC devices. |
| GET_RAMDISKINFO | 8 bytes, shown; zeros where there are no RAM disks. |
| DRIVE A POWER, DISABLE, OFF, ENABLE, ON | `on ` or `off`, shown; then off and on again, and back as it was. |
| DRIVE B ..., GONE FROM THE BUS, ON THE BUS | The same for drive B, and an OPEN of a buffer (`#`) at its address (from GET_DRVINFO) must fail with it off and work with it on. A drive switched on starts from reset, so it tries for about 4 seconds and says how many retries it took (a third of a second each). While a drive holds DATA it doesn't OPEN: a 1541 or 1571 holds it from its reset (about a second) until its DOS listens for ATN, and an OPEN then waits for ever in the KERNAL. |
| DECODE_TRACK, EASYFLASH ..., LOAD_REU OF 4 BYTES | Wrong parameters: `81,INVALID PARAMS`. Nothing is erased. |
| LOAD_CONFIG, NO FILE | `88,CANNOT OPEN CONFIG FILE`. The name goes with its 0 byte: Gideon's LOAD_CONFIG doesn't end it, as his FILE_STAT doesn't. |
| FINISH_CAPTURE | `00,OK`, no data. |
| UNKNOWN $02, $51, $12, $7F | READ_RTC, the Ultimate 64's GET_PALETTE, ENCODE_TRACK and one nobody has: `21,UNKNOWN COMMAND` on an Ultimate II and on the core. |
| LOAD_REU, SAVE_REU, REU OFF | With the REU off: `84,REU NOT ENABLED`, the data `$FE $FF $FF $FF` (-2). With it on, LOAD_REU's answer is shown and SAVE_REU is skipped. |

The core answers as Gideon's current source does. Older Ultimate firmware fails these lines
(3.14d all seven, 3.15a only GET_DRVINFO):

- GET_HWINFO: THE PRODUCT, GET_HWINFO 2 and the three EASYFLASH lines: `21,UNKNOWN COMMAND`
  before his "Bring control target in line with documentation" (`e97fb2e6`, 3.14e), which
  made a missing device mean the product and wrong parameters `81,INVALID PARAMS`.
- LOAD_CONFIG, NO FILE: `21,UNKNOWN COMMAND` before 3.15a, which added the command.
- GET_DRVINFO: its software IEC devices count, but the reply leaves out their bytes, before
  `f9e2391a` (2026-10, after 3.15a).

At the end, F runs FREEZE (the core opens its menu, an Ultimate its own; the line passes when
the reply comes, with no status), R runs REBOOT (the C128 resets as with the reset button),
and any other key ends it.

## UCIREU

LOAD_REU and SAVE_REU, of the control target (the REU's file) and of the DOS target (a file of
its own). The REU must be on: on the core 512 KB or more in the menu; on an Ultimate its own
REU, with the core's off. SAVE_REU overwrites the REU's file (the core's REU File, else
`/c128/preload.reu`; an Ultimate's REU Preload Image), keeping the old one as `.bu`, so the
program asks for a key first. It leaves the REU's first 64 KB holding its pattern.

| Line | What it checks |
|---|---|
| LOAD_REU: THE REU IS ON | Any answer but `84,REU NOT ENABLED`; shown: the result as 4 bytes, the message, the status (`85` and -1 if the file isn't there yet). |
| PATTERN INTO 64 KB OF REU | Written by the REU's own DMA (STASH from `$3000`, 4 KB at a time). |
| SAVE_REU, LOAD_REU | `00,OK` and a result of at least 64 KB (the REU's size), with the 64 KB cleared in between; both shown. The message names the file, so it differs between machines. |
| LOAD_REU: AS MANY AS SAVED, THE PATTERN IS BACK | The same result, and the 64 KB as they were (FETCH and compare). |
| CD /, OPEN_DIR /, CD TO ITS FIRST FOLDER | As UCIDOS: the first folder starting with S or U. |
| DOS LOAD_REU, NO FILE OPEN | `85,NO FILE OPEN`. |
| DOS SAVE_REU, DOS LOAD_REU, REU $2000 HOLDS $1000'S | REU `$1000-$1FFF` into `UCIREU.BIN`, then back to `$2000`: his `$  1000 BYTES SAVED FROM REU $  1000` (36 characters) and `$  1000 BYTES LOADED TO REU $  2000` (35); the pattern then at `$2000`. |
| DOS LOAD_REU PAST THE TOP | 32 bytes to `$FFFFF0`: 16 load, `02,REQUEST TRUNCATED`, shown (the address in lower case, as his). On the core's R3 memory the REU's memory ends at 512 KB, so nothing loads there. |
| DELETE UCIREU.BIN | The file goes again. |

## UCICFG

LOAD_CONFIG, the control target's `$50`, of files it writes as `UCICFG.CFG` into the first
folder in the root starting with S or U, then deletes. It finds the REU's size with the REU's
own DMA, a byte in each 64 KB bank it tries, and puts those bytes back. It starts by showing
the REU's size and drive A's type and B's power, and ends by putting them back with one more
file. On the core a drive's power comes back as on or off, not as automatic, until the core
restarts (as with UCICTRL). The logs are compared byte for byte with what Gideon's firmware
gives (3.15a on, unchanged since); a log that differs is shown in upper case under the line.

| Line | What it checks |
|---|---|
| NO SUCH FILE | `/NOSUCH/UCICFG.CFG`: `88,CANNOT OPEN CONFIG FILE`. |
| A STORE, AN ITEM IT LACKS | A comment, `[No Such Store]` with an item, `No Such Item=1` in the C64 store: `00,OK` and his two log lines, one for each. |
| REU ON, 512 KB, STILL RUNNING | `RAM Expansion Unit=Enabled`, `REU Size=512 KB`: `00,OK`, no log, and then a 512 KB REU at `$DF00` (it wraps at bank 8). The program is still running: the core resets the C128 for a REU change in its menu, not for this one. |
| REU SIZE 16 MB, 16 MB AT $DF00 | `00,OK` and a 16 MB REU; on the core's R3 memory (no 16 MB) `89,CONFIG FILE HAD ERRORS` and his line for a choice it lacks, shown as (NOT HERE). |
| REU OFF, NO REU AT $DF00 | `Disabled`: nothing answers at `$DF02`. |
| ERRORS, NOTHING CHANGED | A line outside a store, `REU Size=3 MB`, `=5` and a good `RAM Expansion Unit=Enabled`: `89` and his three log lines, and still no REU. (His firmware keeps the good value and switches it in with the next file without errors; the core drops it.) |
| DRIVE B OFF / ON, GET_DRVINFO | `[Drive B Settings]` `Drive=Disabled`, then `Enabled`: GET_DRVINFO's power byte for B. |
| DRIVE A 1571 / 1541, GET_DRVINFO | `Drive Type`, GET_DRVINFO's type for A. Skipped if drive A is a 1581 (the internal floppy, a D81). On the core this is the one model of both drives. |
| BACK AS IT WAS, REU, DRIVES AS BEFORE | One file with the REU, A's type and B's power from the start: `00,OK`, and the three as they were. |
| DELETE UCICFG.CFG | `00,OK`. |

An Ultimate takes the command interface off the bus for a few ms when LOAD_CONFIG changes a
setting of its C64 store, such as the REU: his `set_emulation_flags` switches the interface's
slot off, prints to its UART, and switches it on again, all before the reply. `$DF1C`-`$DF1F`
read `$FF` meanwhile (on the MEGA65), which a program would take for a reply of `$FF` bytes,
then push its next command while the real reply waits and lose it. So every program's
`wait_reply` (`ucilib.s`) takes `$DF1C` only while `$DF1D` reads `$C9` (`$49` with its IRQ)
before and after it, and UCICFG shows `NO INTERFACE FOR $n POLLS` under a line where that
happened. The core keeps the interface on, and shows no such line.

## UCINET

The network target, 3. Its second half needs `ucinet_peer.py` running on a computer on the
same network (`python3 test/uci/ucinet_peer.py`; it shows the computer's address, listens
on TCP and UDP port 6464, and logs what it does). Port 6465 must be closed there, and a
firewall must let 6464 in. UCINET asks for the peer's address after the first half; RETURN
alone skips the peer. The two host-name lines need the internet (a name server and
`example.com`). It ends by leaving four UDP sockets open, which a reset must close: after a
reset, a second run's UDP SOCKETS line must show as many as the first run's did (with no
reset in between, the core's shows 0 and fails). `GO64` is no reset here: it doesn't pull the
C128's reset line, so neither the core nor an Ultimate sees it. In C128 mode its buffers reach
past `$4000`, so it maps RAM over BASIC's ROM there while it runs (`$FF00` = `$0E`).

| Line | What it checks |
|---|---|
| IDENTIFY | The name, shown: `C128MEGA65 NETWORK INTERFACE V1.0` on the core, `ULTIMATE-II NETWORK INTERFACE V1.0` on an Ultimate. |
| GET_INTERFACE_COUNT | At least one: 1 on the core, 2 on the II+L. |
| GET_IPADDR, EACH | Each interface's address, netmask and gateway, shown; one must have an address (none means no DHCP lease yet). The II+L's interface 0 has none: its Ethernet is 1. |
| GET_IPADDR PAST THE LAST, GET_NETADDR, THAT ONE | `82,PARAMETER(S) OUT OF RANGE`; the MAC address of the interface with the address. |
| GET_IPADDR, NO INTERFACE, SET_IPADDR, NO ADDRESSES, SET_INTERFACE | `81,INVALID PARAMS`, `81`, `21,UNKNOWN COMMAND`. |
| SET_IPADDR, THE SAME; GET_IPADDR: UNCHANGED | That interface's three addresses, set again: `00,OK`, and nothing changed. |
| READ $7F, NEVER OPENED; READ OF 1473 BYTES; READ, NO LENGTH | A socket nobody opened: -1 (`$FF $FF`) and `02,NO DATA: 9` (EBADF); a length over 1472 (a whole UDP datagram in one Ethernet frame): `82` and no data; `81`. |
| WRITE $7F, CLOSE $7F, OPEN_TCP, NO NAME | `12,SEND ERROR: 9` with -1, `12,ERROR ON CLOSE: 9`, `81`. |
| UDP SOCKETS | UDP sockets to 127.0.0.1 opened until one fails, then closed: how many, and the failure (4 and `85,ERROR OPENING SOCKET` on the core, 5 on the II+L: 16 less its own). |
| OPEN_TCP PEER:6465, CLOSED | `11,ERROR ON CONNECT: 104`, refused. |
| OPEN_TCP PEER:6464 | The socket, shown: 1 on the core, a higher number on an Ultimate. |
| 10 READS, NOTHING SENT | Each `02,NO DATA: 11` (EAGAIN) after waiting for data: at least 10 jiffies for ten. The core waits 40 ms (22-25 jiffies for ten); the II+L's 40 ms comes out shorter (17-20). |
| TCP 200 BYTES, READS OF 64 | Asks the peer for 200 bytes of its pattern (byte i is i mod 251), read 64 at a time: each reply `00,OK`, its 2-byte header the bytes it carries, the pattern unbroken. How many reads, and the most blocks a reply took. |
| TCP 1420 BYTES, READS OF 1472 | The same, in replies of two blocks (893 bytes after the header, then the rest). |
| TCP WRITE 8000 BYTES | The pattern in WRITEs of 892 bytes; each answers its length. The peer reads them only after 1.5 s, with a small receive buffer, so the last WRITEs wait for room; then it answers `OK 8000`. The rate, shown, is mostly that wait. |
| TCP READ 32768 BYTES | The pattern, read 1472 at a time, and the rate. |
| CLOSED BY THE PEER, READ AFTER THAT | The peer closes: `01,CONNECTION CLOSED BY HOST` with 0, which closes the socket, so the next READ is `02,NO DATA: 9`. |
| OPEN_UDP PEER:6464, UDP 64 BYTES | A datagram of 64 bytes, whole. |
| UDP 200 BYTES, READ OF 64, THE REST IS GONE | 64 bytes and `04,DATAGRAM TRUNCATED: 200`; the next READ finds nothing (`02,NO DATA: 11`). |
| UDP 1420, 1472 BYTES | Whole, in two blocks (the first 895 bytes with its header). |
| UDP 1473 BYTES: DROPPED | It comes in two fragments, and neither the core nor an Ultimate puts fragments together: ten READs find nothing. |
| READ GIVEN UP AFTER A BLOCK, THE NEXT COMMAND, THE NEXT DATAGRAM | Two datagrams of 1420 bytes; the first READ takes its first block (895 bytes, Data More) and ABORTs: the interface is idle, IDENTIFY answers, and the next READ is the second datagram whole, not the first one's rest. |
| UDP WRITE 892 BYTES, ECHOED | The peer sends it back, whole. |
| CLOSE | `00,OK`. |
| OPEN_TCP NOSUCH.INVALID | `84,UNRESOLVED HOST` (`.invalid` never resolves). |
| OPEN_TCP EXAMPLE.COM:80, HEAD | A name looked up, a connection, a HEAD request: the reply's first line, shown, starts with `HTTP/`. |
| LEAVE 4 UDP SOCKETS OPEN | For the reset (above). |

If `$DF1D` doesn't read `$C9`, a program says so and stops: that is all VICE (no command
interface) shows of them, in `x128` and `x64sc`. Parts that need no interface build alone
for VICE with ca65's `-D`: `PROBETEST` (UCICFG's REU probe: no REU and 128 KB to 16 MB found
in `x128`, 256 KB in `x64sc`, its bytes put back), `BUSTEST` (UCICTRL's bus check of drive 9,
also while VICE's monitor resets the drive), `HDRTEST` (UCIDOS's drive 9 header) and
`REUTEST` (UCIREU's REU DMA and pattern check).

The whole of each runs in the firmware's host tests (`make -C M2M/firmware host-test`):
`host/test_prg.c` is a 6502 that runs the program against the firmware's UCI code, a model
of the protocol and a FAT32 image, with the KERNAL faked where the programs call it. It logs
each program's time at 1 MHz and fails a run that would go wrong on hardware: an OPEN while a
drive just switched on holds DATA (for a second), a FILE_STAT or LOAD_CONFIG name without its
0 byte, a REU change the core would reset the C128 for, a frame for the firmware that finds
lwIP's pool empty. In C128 mode BASIC's ROM is at `$4000`-`$BFFF` unless `$FF00` maps RAM
there, so data kept up there reads back wrong, as on hardware. The REU's STASH and FETCH run
at once (`-reu` 512 KB, `-reu16` 16 MB, `-r3` R3's menu); it answers at `$DF00` only while
it is on, and wraps at its size. `-blink` takes the interface off the bus for 5 ms after
each REU change, as an Ultimate does. UCIDOS also runs again after a run cut short once the
D64 is written, as by a reset. UCINET runs against `host_net.c`: the firmware's lwIP on a
loopback with the same peer, a DNS server and a web server at 10.0.0.1, twice with a reset
of the C128 between, and once without a peer.

Rebuild with `python3 test/uci/make_image.py`. It needs `ca65` and `ld65` from cc65.
`ucilib.inc` and `ucilib.s` hold what the tests share.
