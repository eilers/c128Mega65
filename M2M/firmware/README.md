# RISC-V system CPU firmware

Runs on the NEORV32 that replaces QNICE in a `G_RISCV` build (`M2M/vhdl/rv/rv_soc.vhd`).
See `doc/riscv-system-cpu.md` for the plan, the hardware and how booting works.

## Build

Needs the [xPack RISC-V toolchain](https://github.com/xpack-dev-tools/riscv-none-elf-gcc-xpack)
(`riscv-none-elf-gcc`, 14.3.0-1 was used) on `PATH`, or `CROSS=<prefix>`.

    make

builds

- `boot.rom`, which `rv_soc.vhd` reads into its boot ROM at synthesis
- `build/firmware.bin`, the firmware: copy it to `/c128/firmware.bin` on the SD card
- `test/selftest.rom` and `test/fwboot.rom`, boot ROMs of `CORE/sim/tb_rv_soc.vhd`

The `.rom` files are committed, so a core builds without the toolchain. The firmware's
version is `git describe` of the tree it was built from; `make FW_VERSION=...` sets
another.

    make host-test

runs the boot ROM's loader and the firmware's FatFs setup on the Mac, against FAT images
that `hdiutil` makes (`host/`), and the serial monitor's commands.

## Serial monitor

The firmware answers commands on the serial port of the MEGA65 JTAG adapter (2 Mbaud,
8N1; the boot ROM's messages come at 115200). They read and write the C128's memory, the
core's devices, HyperRAM and the registers while the core runs (`fw/monitor.c` lists
them). `tools/mon.py` is the Mac's side:

    tools/mon.py peek ram:1C01 40         # C128 RAM, bank 0, $1C01
    tools/mon.py poke ram:400 8 9         # "HI" at the top left of the 40-column screen
    tools/mon.py save ram:0 40000 c128.bin
    tools/mon.py load prog.prg            # at its load address in bank 0
    tools/mon.py pause                    # and resume
    tools/mon.py term                     # type commands; h lists them

`ram:` is device 0100 (bank N at N0000), `sysrom:` 0101, `drvrom:` 0102, `osm:` the
menu's characters, `hr:` HyperRAM; `DEV:WORD` names any device. A device's byte is the low
byte of its 16-bit word. `ram:` is the RAM itself: at D000-DFFF it's the RAM under the I/O
chips. A bus error ends the command with an error, not the firmware. Opening or closing
the menu sets the C128's pause as the menu wants it.

## Layout

| Path                | What                                                                 |
|---------------------|----------------------------------------------------------------------|
| `include/m2m_rv.h`  | the hardware, as `rv_soc.vhd` decodes it                              |
| `include/fw_image.h`| the firmware file's header                                           |
| `boot/`             | the boot ROM: loads the firmware from the SD card (`load.c`)          |
| `fw/`               | the firmware: main loop, keys, SD card, screen, serial monitor       |
| `lib/`              | shared: SD card, FatFs disk layer, CRC-32, menu text, serial port, `printf`, trap handler |
| `fatfs/`            | [FatFs](http://elm-chan.org/fsw/ff/) R0.16 with patches 1 and 2 (line ends made LF); `boot/` and `fw/` each have an `ffconf.h` |
| `test/`             | boot ROMs for the testbench                                          |
| `host/`             | tests that run on the Mac                                            |
| `tools/`            | `fw_image.py` makes the firmware file; `rom_txt.py` the boot ROM's text; `mon.py` talks to the serial monitor |
| `neorv32/`          | NEORV32 v1.13.6's `crt0.S` and linker script                         |
