# NEORV32

The RISC-V system CPU (`M2M/vhdl/rv/rv_soc.vhd`). These are the
`rtl/core` files of [NEORV32](https://github.com/stnolting/neorv32) **v1.13.6**,
unchanged, under its BSD-3-Clause license (`LICENSE`). They compile into the VHDL
library `neorv32`, in the order of `file_list.txt` (NEORV32's `rtl/file_list_core.f`).

One file is left out: `neorv32_bootrom_image.vhd`, NEORV32's bootloader. NEORV32's boot
ROM isn't used (`rv_soc` starts in its own, `BOOT_MODE_SELECT` 1), but
`neorv32_bootrom.vhd` refers to the package, so `M2M/vhdl/rv/neorv32_bootrom_image.vhd`
provides an empty one.

To move to another release: copy the files listed in its `rtl/file_list_core.f`
here, except the bootloader image, and regenerate `file_list.txt` the same way.
`CORE/scripts/add_rv_sources.tcl` adds them to the Vivado project.
