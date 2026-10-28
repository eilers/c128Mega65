# UCI tests

`uci-test.d64` holds two tests of the Ultimate Command Interface at `$DF1C`-`$DF1F`, each
for C128 mode and for C64 mode: `UCITEST` / `UCITEST64` (the transport) and `UCIDOS` /
`UCIDOS64` (the DOS target). They talk to the interface the way a program does. Each line
ends in PASS or FAIL, and names, paths and times are shown, so a run on a real Ultimate and
a run on the core can be compared line by line.

On the core: switch on **UCI at $DF1C** in the menu, mount the disk on drive 8,
`LOAD"UCITEST",8` and `RUN` (or `UCITEST64` after `GO64`), and the same for `UCIDOS`.

On an Ultimate II+ in the slot: the core's REU and UCI off (both answer at `$DF00`-`$DF1F`
ahead of a cartridge), **Cartridge: Hardware**, and the Ultimate's own Command Interface on.

| Line | What it checks |
|------|----------------|
| EMPTY COMMAND | A push with no bytes gets an empty reply and an empty status. |
| DOS 1 / DOS 2 IDENTIFY | The DOS's name, shown: `MEGA128 DOS V1.0` on the core, `ULTIMATE-II DOS V1.2` on an Ultimate. Status `00,OK`, and target 2 gives target 1's name. |
| CONTROL IDENTIFY | Target 4's answer, shown. The core has no control target yet, so it says `NO TARGET`. |
| TARGET $0F IDENTIFY | A target nobody registered says `NO TARGET`, `00,OK`. |
| UNKNOWN COMMAND | `$01 $7F`: no data, `21,UNKNOWN COMMAND`. |
| ECHO 895 BYTES | DOS `ECHO` of the largest command the queue holds comes back whole. |
| NO-REPLY COMMAND | A command with bit 7 of its target byte set goes straight back to idle. |
| PUSH WHILE BUSY | A push while a reply waits sets ERROR; CLR_ERR clears it, and the reply (DOS 1's name) survives. |
| ABORT | ABORT ends a waiting reply. |
| IRQ BIT | With the IRQ bit, `$DF1D` reads `$49` once the reply is there and `$C9` after a response byte is read. Gideon added it in March 2026 (firmware 3.14e and later); an older Ultimate reads `$C9` both times and fails this line. |
| DOS 1 IDENTIFY AGAIN | How many polls of `$DF1C` the reply took (about 10 us each at 1 MHz). |

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

If `$DF1D` doesn't read `$C9`, a program says so and stops. That is all it can show in
VICE, which has no command interface; all four get there in `x128` and `x64sc`. The whole of
each runs in the firmware's host tests instead (`make -C M2M/firmware host-test`):
`host/test_prg.c` is a 6502 that runs the program against the firmware's UCI code, a model
of the protocol and a FAT32 image, with the KERNAL faked where the programs call it. It
counts the 6502's cycles and logs each program's time at 1 MHz, and it also runs UCIDOS
again after a run cut short once the D64 is written, as a reset would.

Rebuild with `python3 test/uci/make_image.py`. It needs `ca65` and `ld65` from cc65.
`ucilib.inc` and `ucilib.s` hold what the two tests share.
