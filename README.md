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
2. Build QNICE toolchain:
   - `cd M2M/QNICE/tools && ./make-toolchain.sh`
3. Build core shell ROM:
   - `cd CORE/m2m-rom && ./make_rom.sh`

Required SD card files
----------------------

Place the following files on the active SD card:

- `/c128/boot0.rom` (system ROM bundle, 72 KiB)
- `/c128/boot1.rom` (drive ROM bundle, 512 KiB)

Without these files, the core cannot boot correctly.
