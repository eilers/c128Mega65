# SwiftLink modem test

`modem-test.d64` holds two builds of [CCGMS Future](https://github.com/mist64/ccgmsterm)
(commit 4c4a202), both set up for a SwiftLink at `$DE00` at 38400 baud:

- `CCGMS`: the terminal program, for calling BBSes by hand.
- `CCGMS AUTO`: the same with CCGMS's test automation on. `xfer/` is its test server,
  which drives the terminal through Punter and every XMODEM variant over the modem.

They are CCGMS's regular build (`DEFAULT_DRIVER=2 DEFAULT_BAUDRATE=38400`, plus
`AUTOMATION=1` for the second); `src/config.s` only knows 2400 and 9600 as the default
rate, so 38400 was added there. CCGMS's EasyFlash build (the "CCGMS Ultimate" cartridge,
for instance) has no SwiftLink driver at all.

Both passed in VICE 3.9 `x64sc` with its own SwiftLink at `$DE00`, the automated test at
9600 and at 38400 baud.

`ccgms-d700.crt` and `ccgms-auto-d700.crt` are the same two programs as cartridges, which
start in seconds instead of loading from disk. They are built for a SwiftLink at `$D700`
(`DEFAULT_DRIVER=4`): they are Magic Desk cartridges, whose bank register takes every
write to `$DE00`-`$DEFF`, so a SwiftLink there would switch banks. `cart/` is the loader:
bank 0 copies the program from the banks after it to `$0801`, switches the cartridge off
and RUNs it (`cart/build.sh <prg> <crt> <name>`). The automated test passed from the
cartridge in VICE 3.9 `x128`, C64 mode, with its SwiftLink at `$D700` at 38400 baud.

## Setup

1. The MEGA65 on a network with DHCP. The modem is part of `/c128/firmware.bin`.
2. In the menu, set **Modem** to `$DE00`.
3. CCGMS is a C64 program: `GO64`, mount `modem-test.d64` on drive 8, `LOAD"CCGMS",8`, `RUN`.
   Or set **Modem** to `$D700` and load `ccgms-d700.crt` as the cartridge: the C128
   starts in C64 mode and runs CCGMS.
4. Type `ATI1`. The modem shows its MAC and IP address (or that it still waits for DHCP).

## Calling a BBS

`ATDT afterlife.dynu.com:6400` (a C64 BBS from CCGMS's phone book). `CONNECT 38400`,
then the BBS's screen. `+++`, wait a second, `ATH` hangs up; so does leaving CCGMS.

## HyperRAM load and HDMI

The system firmware runs from HyperRAM, which HDMI's scaler shares. Two test commands load
it for 15 seconds (`AT*H1=60` makes it a minute), with the whole firmware waiting meanwhile
(no network, no menu), then print `HYPERRAM READS n/S`:

- `AT*H1`: every instruction misses the cache, so the CPU reads cache line after cache line.
- `AT*H2`: uncached reads of the firmware image, 4 bytes at a time.

Run them on the 80-column screen and watch HDMI: it must stay clean. When a cache line was
one 64-word HyperRAM burst, `AT*H1` filled HDMI with sparkles.

## A fault and a hang

`AT*T` reads from an address with nothing there; the firmware stops on its trap screen
(`trap` and the cause in the menu's bottom row). `AT*W` loops for good; a second later the
watchdog stops the firmware the same way, marked `stuck`, with the address of the loop.
Either way the core needs a reset (a long press).

## Automated transfer test

On a computer on the same network (macOS or Linux, with `cc` and `socat`):

    cd test/modem/xfer
    sh xfer.sh 2>&1 | grep TEST_XFER

It listens on TCP port 25232 for one call. On the C128, `LOAD"CCGMS AUTO",8`, `RUN`
(or the `ccgms-auto-d700.crt` cartridge with the modem at `$D700`, and a writable disk on
drive 8), and type `ATDT <that computer's IP>:25232`. Four seconds after `CONNECT` the server
starts typing for you. It takes a few minutes, and the computer prints:

    ***** TEST_XFER: PUNTER OK
    ***** TEST_XFER: XMODEM; client: XMODEM, server: 512B/CHKSUM: OK
    ***** TEST_XFER: XMODEM; client: XMODEM, server: 1KB/CRC16:   OK
    ***** TEST_XFER: XMODEM; client: XMODEM-CRC, server: 512B/CHKSUM: OK
    ***** TEST_XFER: XMODEM; client: XMODEM-CRC, server: 1KB/CRC16:   OK
    ***** TEST_XFER: XMODEM; client: XMODEM-1K, server: 512B/CHKSUM: OK
    ***** TEST_XFER: XMODEM; client: XMODEM-1K, server: 1KB/CRC16:   OK
    ***** TEST_XFER: Succeeded!

The downloads go to drive 8, so the disk must be writable. A firewall may have to let
`socat` accept the call.

CCGMS is Copyright (c) 2016,2022 Craig Smith, alwyz and Michael Steil, under the BSD
3-Clause license in `xfer/LICENSE.txt`. That covers the programs on `modem-test.d64`, the
two cartridges, and `xfer/`, which is CCGMS Future's `test/` server, unchanged.
