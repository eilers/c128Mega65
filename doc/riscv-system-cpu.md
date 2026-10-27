# RISC-V system CPU

Work in progress on branch `riscv-shell`: a NEORV32 replaces QNICE, the M2M framework's
menu CPU. The goal is an Ultimate-style firmware: the menu, then Gideon's command
interface (UCI) and REST API, protocol compatible.

## Plan

1. **Hardware** (done in simulation): `M2M/vhdl/rv/rv_soc.vhd`, a drop-in for the
   `QNICE` entity with the same ports. A build switch, `G_RISCV`, picks one or the
   other, so `config.vhd`, the core's devices and the framework stay as they are.
2. **Firmware base**: the boot ROM loads the firmware from the SD card into HyperRAM and
   runs it there. FreeRTOS and a C++ subset, SD card, FAT32, menu text, keys.
3. **The Shell in C++**, item by item, until it does everything the QNICE Shell does:
   the menu from `config.vhd`, the callbacks in `m2m-rom.asm`, mounting drives, loading
   cartridges and ROMs (`modem.bin` too), the settings file, the help screens. Each step
   is tested on hardware against a QNICE build.
4. **Drop QNICE**: make RISC-V the default and delete QNICE.

Then UCI at `$DF1C` (Gideon's REU also decodes `$DF00`–`$DF1F`, so the two need
sorting out), then networking. The modem's own NEORV32 moves into the system CPU at
that stage.

Decisions (2026-10-26): FreeRTOS and C++ from the start; the firmware runs from HyperRAM
behind the i-cache, loaded from the SD card by a small boot ROM (no card: a message on
the screen; the `.cor` flash slot may come later); the modem keeps its CPU until the
network stage.

## Hardware

`rv_soc.vhd` runs on the QNICE clock (50 MHz). NEORV32 v1.13.6 (`CORE/vhdl/neorv32`,
shared with the modem): rv32imc, 8 KB i-cache, 16 KB data RAM, no d-cache yet.

| Address      | What                                                                     |
|--------------|--------------------------------------------------------------------------|
| `0x00000000` | boot ROM, 8 KB of block RAM, read at synthesis from `M2M/firmware/boot.rom` |
| `0x40000000` | HyperRAM, 16 MB: byte A is word A/2, low byte first (`rv_hr_bridge.vhd`)  |
| `0x80000000` | data RAM                                                                 |
| `0xC0000000` | QNICE's device bus: word N of the device in `RAMROM_DEV` at `+2N`          |
| `0xE0000000` | registers: CSR, OSM, ascal, keys, control flags                           |
| `0xE0000100` | SD card controller registers 0–7 (`sdhost.vhd`)                           |
| `0xFFE00000` | NEORV32: UART0 (the serial port), CLINT timer                             |

`M2M/firmware/include/m2m_rv.h` has the details.

- **Device bus.** The devices keep QNICE's timing: they act on the falling edge, and the
  CPU reads at the next rising edge without wait. A 16-bit access is one device access,
  a 32-bit access two. Between accesses the select is low for at least one falling
  edge, which `qnice2hyperram`-style devices need to see a new access. There's no
  4K window: the whole 28-bit address is in the CPU's address.
- **HyperRAM.** The CPU has its own Avalon port in place of `qnice2hyperram`, so the
  device bus's HyperRAM device answers nothing in a RISC-V build. Cache lines are read
  in 16-word bursts, one after the other, as the modem does since the HDMI sparkles
  (`doc/swiftlink-modem.md`).
- **Registers.** The same bits as QNICE's `0xFFE0` and up (`M2M/rom/sysdef.asm`). The
  256 control flags are eight 32-bit registers each, not address/data pairs.

## Status

- `tb_rv_soc` passes: registers, keys, the menu RAM, a device with wait states, the SD
  registers, HyperRAM bytes, halves and words, and code run from HyperRAM.
- The boot ROM is a bring-up check: it shows a banner, the keys, the CSR, and whether
  block 0 of the SD card ends in `55AA`. The core stays in reset.
- `qnice_wrapper` elaborates both ways in nvc. Not yet built in Vivado.

## Building and testing

- Firmware: `make -C M2M/firmware` (xPack `riscv-none-elf-gcc`). The `.rom` files are
  committed, so a core builds without the toolchain.
- Bitstream: `CORE/scripts/build_boards.sh 6rv`, an R6 with `G_RISCV`.
- Simulation: `CORE/scripts/run_rv_soc_nvc.sh` (nvc, a few seconds on the Mac).
