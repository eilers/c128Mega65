# NEORV32

The RISC-V CPU of the SwiftLink modem (`CORE/vhdl/modem/modem.vhd`). These are the
`rtl/core` files of [NEORV32](https://github.com/stnolting/neorv32) **v1.13.6**,
unchanged, under its BSD-3-Clause license (`LICENSE`). They compile into the VHDL
library `neorv32`, in the order of `file_list.txt` (NEORV32's `rtl/file_list_core.f`).

One file is left out: `neorv32_bootrom_image.vhd`, NEORV32's bootloader. In its place
`CORE/vhdl/modem/neorv32_bootrom_image.vhd` holds our boot code, generated from
`CORE/modem-fw/boot/boot.S` by `make -C CORE/modem-fw bootrom`. NEORV32's boot ROM
isn't used (it took a RAMB18 for 104 bytes): the CPU starts at `0xF0003000`
(`BOOT_MODE_SELECT` 1), where `modem.vhd` serves the same image from LUTs. The
package is still compiled because `neorv32_bootrom.vhd` refers to it.

To move to another release: copy the files listed in its `rtl/file_list_core.f`
here, except the bootloader image, and regenerate `file_list.txt` the same way.
`CORE/scripts/add_modem_sources.tcl` adds them to the Vivado project.
