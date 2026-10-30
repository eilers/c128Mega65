C128 Core for Mega65
====================

This port is using the c128 core from MiSTer and uses the MiSTer2MEGA65 framework to port it to the MEGA65.
MiSTer2MEGA65 is a framework to simplify porting MiSTer cores to the MEGA65.

This core is not yet fully functional. Please see the [Project Blog](https://wiki.eilers-online.net/c128Mega65) to get more information about the current status.

Current target hardware
-----------------------

- MEGA65 R6 board

Build prerequisites
-------------------

1. Initialize submodules:
   - `git submodule update --init --recursive`
2. Build the bitstreams: `CORE/scripts/build_boards.sh` (see [doc/build-and-sim.md](doc/build-and-sim.md)).

The on-screen menu, the drives and the ROM loading run on a RISC-V system CPU (see
[doc/riscv-system-cpu.md](doc/riscv-system-cpu.md)). Its boot ROM is in the bitstream; its
firmware is `sdcard/c128/firmware.bin`. Both are committed, so a core builds without the RISC-V
toolchain. After changing the firmware, rebuild it with `make -C M2M/firmware` (xPack
`riscv-none-elf-gcc`) and copy `M2M/firmware/build/firmware.bin` to `sdcard/c128/`.

Required SD card files
----------------------

Place the following files on the active SD card:

- `/c128/firmware.bin` (firmware of the system CPU, from `sdcard/c128/` or the release zip)
- `/c128/boot0.rom` (system ROM bundle, 72 KiB)
- `/c128/boot1.rom` (drive ROM bundle, 512 KiB)

Without these files, the core cannot boot correctly. Without `firmware.bin`, or with one that
does not fit the core, the core says so on the screen.

Optional:

- `/c128/modem.bin` (firmware of the SwiftLink modem, see [doc/swiftlink-modem.md](doc/swiftlink-modem.md))

Contributing
------------

Help is welcome: see [CONTRIBUTING.md](CONTRIBUTING.md). Beta cores are shared and discussed in
`#c128-core` on the MEGA65 Discord. Report bugs as
[issues](https://github.com/eilers/c128Mega65/issues); known ones are in
[doc/KNOWN_BUGS.md](doc/KNOWN_BUGS.md).
