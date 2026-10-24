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

## Setup

1. `/c128/modem.bin` on the SD card (from `sdcard/c128/`), the MEGA65 on a network with DHCP.
2. In the menu, set **Modem** to `$DE00`.
3. CCGMS is a C64 program: `GO64`, mount `modem-test.d64` on drive 8, `LOAD"CCGMS",8`, `RUN`.
4. Type `ATI1`. The modem shows its MAC and IP address (or that it still waits for DHCP).

## Calling a BBS

`ATDT afterlife.dynu.com:6400` (a C64 BBS from CCGMS's phone book). `CONNECT 38400`,
then the BBS's screen. `+++`, wait a second, `ATH` hangs up; so does leaving CCGMS.

## Automated transfer test

On a computer on the same network (macOS or Linux, with `cc` and `socat`):

    cd test/modem/xfer
    sh xfer.sh 2>&1 | grep TEST_XFER

It listens on TCP port 25232 for one call. On the C128, `LOAD"CCGMS AUTO",8`, `RUN`,
and type `ATDT <that computer's IP>:25232`. Four seconds after `CONNECT` the server
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

`xfer/` is CCGMS Future's `test/` server, unchanged, under its BSD 3-Clause license
(`xfer/LICENSE.txt`).
