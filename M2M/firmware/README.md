# RISC-V system CPU firmware

Runs on the NEORV32 that replaces QNICE in a `G_RISCV` build (`M2M/vhdl/rv/rv_soc.vhd`).
See `doc/riscv-system-cpu.md` for the plan and the hardware.

## Build

Needs the [xPack RISC-V toolchain](https://github.com/xpack-dev-tools/riscv-none-elf-gcc-xpack)
(`riscv-none-elf-gcc`, 14.3.0-1 was used) on `PATH`, or `CROSS=<prefix>`.

    make

builds `boot.rom`, which `rv_soc.vhd` reads into its boot ROM at synthesis, and
`test/selftest.rom`, the boot ROM of `CORE/sim/tb_rv_soc.vhd`. Both are committed.

## Layout

| Path                | What                                                                |
|---------------------|---------------------------------------------------------------------|
| `include/m2m_rv.h`  | the hardware, as `rv_soc.vhd` decodes it                             |
| `lib/`              | text on the menu and the serial port; the trap handler              |
| `boot/boot.c`       | the boot ROM: for now a bring-up check                              |
| `test/selftest.c`   | checks for the testbench                                            |
| `tools/rom_txt.py`  | turns a binary into the text the boot ROM is read from               |
| `neorv32/`          | NEORV32 v1.13.6's `crt0.S` and linker script                        |
