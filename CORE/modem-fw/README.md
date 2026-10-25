# Modem firmware

Runs on the NEORV32 inside the core (`CORE/vhdl/modem`): lwIP on the MEGA65's
Ethernet port and a Hayes command set on the C128's emulated SwiftLink. See
`doc/swiftlink-modem.md` for what the modem does and how it is built.

## Build

Needs the [xPack RISC-V toolchain](https://github.com/xpack-dev-tools/riscv-none-elf-gcc-xpack)
(`riscv-none-elf-gcc`, 14.3.0-1 was used) on `PATH`, or `CROSS=<prefix>`.

    make            # build/modem.bin
    make bootrom    # ../vhdl/modem/neorv32_bootrom_image.vhd, only after changing boot/boot.S

Copy `build/modem.bin` to the SD card as `/c128/modem.bin`. The Shell loads it at
startup; a new firmware needs no new core.

## Layout

| Path            | What                                                                   |
|-----------------|------------------------------------------------------------------------|
| `src/hw.h`      | the hardware's registers, as `CORE/vhdl/modem/modem.vhd` decodes them   |
| `src/modem.c`   | Hayes commands, dialling and answering, telnet, the data path          |
| `src/acia.c`    | the 6551's FIFOs and lines                                             |
| `src/eth.c`     | lwIP netif for the RMII MAC                                            |
| `src/log.c`     | debug log; `ATI2` prints it on the C128                                |
| `src/lwipopts.h`| lwIP: raw API, no OS, sized for the 16 KB data RAM                     |
| `boot/boot.S`   | boot code (at 0xF0003000): waits for a checksummed image in HyperRAM, jumps to it |
| `tools/`        | `mkimage.py` puts the header the boot code checks on the image; `bootrom_vhd.py` turns the boot code into VHDL |
| `lwip/`         | lwIP 2.2.1 (STABLE-2_2_1_RELEASE): `src/core`, `src/include`, `src/netif/ethernet.c` |
| `neorv32/`      | NEORV32 v1.13.6's `crt0.S` and linker script                           |

Code and constants link at `0x40000010` (HyperRAM: instructions through the cache, constants a word at a time), data and stack
at `0x80000000` (16 KB). `make` prints the sizes; `.data + .bss` plus the stack must
stay below 16 KB.

## Test

`CORE/sim/tb_modem.vhd` boots `sdcard/c128/modem.bin` from a HyperRAM model and types
AT commands through the 6551: `vivado -mode batch -source CORE/scripts/run_modem_sim.tcl`
(also part of `run_all_sims.sh`). After a firmware change, copy `build/modem.bin` to
`sdcard/c128/` first.
