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
that `hdiutil` makes (`host/`).

## Layout

| Path                | What                                                                 |
|---------------------|----------------------------------------------------------------------|
| `include/m2m_rv.h`  | the hardware, as `rv_soc.vhd` decodes it                              |
| `include/fw_image.h`| the firmware file's header                                           |
| `boot/`             | the boot ROM: loads the firmware from the SD card (`load.c`)          |
| `fw/`               | the firmware: main loop, keys, SD card, screen, serial console        |
| `lib/`              | shared: SD card, FatFs disk layer, CRC-32, menu text, serial port, `printf`, trap handler |
| `fatfs/`            | [FatFs](http://elm-chan.org/fsw/ff/) R0.16 with patches 1 and 2 (line ends made LF); `boot/` and `fw/` each have an `ffconf.h` |
| `test/`             | boot ROMs for the testbench                                          |
| `host/`             | tests that run on the Mac                                            |
| `tools/`            | `fw_image.py` makes the firmware file; `rom_txt.py` the boot ROM's text |
| `neorv32/`          | NEORV32 v1.13.6's `crt0.S` and linker script                         |
