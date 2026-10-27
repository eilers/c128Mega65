# RISC-V system CPU

Work in progress on branch `riscv-shell`: a NEORV32 replaces QNICE, the M2M framework's
menu CPU. The goal is an Ultimate-style firmware: the menu, then Gideon's command
interface (UCI) and REST API, protocol compatible.

## Plan

1. **Hardware** (done in simulation): `M2M/vhdl/rv/rv_soc.vhd`, a drop-in for the
   `QNICE` entity with the same ports. A build switch, `G_RISCV`, picks one or the
   other, so `config.vhd`, the core's devices and the framework stay as they are.
2. **Firmware base**: the boot ROM loads the firmware from the SD card into HyperRAM and
   runs it there, from `C_HMAP_FIRMWARE` (`0x200`–`0x3FE`, CPU address `0x40400000`,
   `doc/hyperram-map.md`). A main loop in C, SD card, FAT32, menu text, keys.
3. **The Shell in C**, item by item, until it does everything the QNICE Shell does:
   the menu from `config.vhd`, the callbacks in `m2m-rom.asm`, mounting drives, loading
   cartridges and ROMs (`modem.bin` too), the settings file, the help screens. Each step
   is tested on hardware against a QNICE build.
4. **Drop QNICE**: make RISC-V the default and delete QNICE.

Then UCI at `$DF1C` (Gideon's REU also decodes `$DF00`–`$DF1F`, so the two need
sorting out), then networking. The modem's own NEORV32 moves into the system CPU at
that stage.

Decisions: the firmware runs from HyperRAM behind the i-cache, loaded from the SD card by
a small boot ROM (no card: a message on the screen; the `.cor` flash slot may come
later); the modem keeps its CPU until the network stage (2026-10-26). No RTOS: a main
loop of small modules that never wait, as in the QNICE Shell, written in C
(2026-10-27; this replaces the first plan of FreeRTOS and C++).

## Hardware

NEORV32 v1.13.6 (`CORE/vhdl/neorv32`, shared with the modem): rv32imc, 8 KB i-cache,
16 KB data RAM, no d-cache yet.

Clocks: the CPU, the boot ROM and the HyperRAM bridge run at 100 MHz, on the HyperRAM
clock, so the bridge talks to the HyperRAM arbiter directly; the framework leaves out the
FIFO that QNICE's port crosses through. The device bus, the registers and the SD
controllers stay at 50 MHz, on the QNICE clock. Both clocks come from one MMCM with phase
0, so the paths between them are timed: a request crosses as a toggle with its address and
data beside it, the answer the same way back. Gideon's U64E-II CPU also runs at 100 MHz.

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

## Firmware

`M2M/firmware`, in C, built with xPack `riscv-none-elf-gcc`.

- **Boot ROM** (`boot/`, 8 KB): reads `/c128/firmware.bin` from the active SD card with
  a read-only FatFs (8.3 names), loads it into HyperRAM, reads it back to check its
  CRC-32, and jumps to it. If there's no card, no file, or a bad one, it says why on the
  menu and the serial port, and tries again when the cards change or Return is pressed.
- **The file** (`include/fw_image.h`, made by `tools/fw_image.py`): a 512-byte header
  block, then the image. The header holds a magic number, the load address (inside
  `0x40400000`–`0x407FE000`), the entry, the size, the image's CRC-32, a version string,
  and the version of the hardware interface it was built for. The boot ROM starts only a
  firmware with its own `M2M_RV_HW_VERSION` (`include/m2m_rv.h`): bump it when a change
  there would break an older firmware.
- **The firmware** (`fw/`): code and constants in HyperRAM at `0x40400000`; data and stack
  in the 16 KB data RAM. `main.c` polls each module in turn: the keys (Up and Down repeat,
  as in QNICE's `keyboard.asm`), the SD card (mounted when a card is in, dropped when the
  cards change) and the screen. FatFs R0.16 with its two patches, long names in code page
  437, the menu font's. Everything a user needs to see goes on the screen: the serial
  port needs a JTAG adapter, so it only gets a banner line, for the testbench.
- **The Shell** (`fw/shell.c`, step 5, in progress): loads the menu from `config.vhd`, the
  settings file (or the defaults) and the auto-load ROMs from `globals.vhd`, sets up ascal and
  its filters, and lets the core out of reset; then Help (or F13) opens the options menu.
  `fw/menu.c` is QNICE's `menu.asm` (groups, single-select items, submenus, headlines,
  lines); `fw/cfg.c` reads `config.vhd` and the sysinfo device. The core's callbacks
  (`m2m-rom.asm`'s) are `CORE/firmware/core.c`, which finds menu items by their group
  (`config_ids.h`, made from `config.vhd`). Not yet: the file browser, drives, `.crt` files,
  the 6r3 stall line.
- **Shared** (`lib/`): the SD card through `sdhost`'s registers (512-byte blocks, a byte
  at a time through its buffer, as QNICE did), FatFs's disk layer, CRC-32, menu text, the
  serial port, a small `printf`, and the trap handler.

## Status

- `tb_rv_soc` passes with the CPU at 100 MHz: registers, keys, the menu RAM, a device with
  wait states, the SD registers, HyperRAM bytes, halves and words, and code run from
  HyperRAM. Its second run boots the firmware from its HyperRAM model: the firmware
  prints its banner and draws its screen.
- The host tests pass: the boot ROM's loader against FAT32 and FAT16 images made by macOS,
  with an MBR (good files, a 1 MB one, and eight kinds of broken ones), and the
  firmware's long names and `printf`.
- Step 1's boot ROM was a bring-up check (a banner, the keys, the CSR, the SD card's
  block 0), which passed on the R6. Step 4's loader replaces it.
- The firmware base passes on the R6 (`6rv` build: WNS +0.397 ns, WHS +0.036 ns, 265.5
  block RAM tiles): the boot ROM loads `/c128/firmware.bin`, and the firmware lists the
  card's folders with long names, takes every key (Up and Down repeat), opens folders and
  goes back, and follows card changes: with the back card out it lists the bottom tray's.
  Without the file the boot ROM says so, and loads it once the card is right.
- `qnice_wrapper` elaborates both ways in nvc and Vivado.
- R6 build (`build_boards.sh 6rv`): timing met at 100 MHz (the CPU's clock +1.0 ns, the
  50/100 MHz handshakes +1.9 ns), and 25 fewer block RAM tiles than a QNICE build
  (265.5 against 290.5). On the R6 the boot ROM shows its banner, the keys, the CSR and the
  SD card's block 0.
- The HyperRAM map with the firmware region (`doc/hyperram-map.md`) passes on the R6 in
  `6` and `6r3` builds: the REU at 512 KB and 2 MB, virtual cartridges, the modem, HDMI,
  and the R3 menu and settings file.
- The REU in SDRAM on R4 and later, up to 16 MB (`doc/hyperram-map.md`), passes on the R6:
  REUTEST at all three sizes and beside a virtual cartridge, the test cartridges with the REU
  on, and 512 KB only on `6r3`.

## Building and testing

- Firmware: `make -C M2M/firmware` (xPack `riscv-none-elf-gcc`). The `.rom` files are
  committed, so a core builds without the toolchain. Copy `build/firmware.bin` to
  `/c128/firmware.bin` on the SD card.
- Host tests: `make -C M2M/firmware host-test` (macOS: `hdiutil` makes the FAT images).
  `test_shell` runs the Shell against `config.vhd` as `run_config_nvc.sh` dumps it (full
  and R3 menus), with the registers and devices faked in `host/host_m2m.c`. Both nvc
  scripts need `QNICE_DIR` in a worktree without the QNICE submodule.
- Bitstream: `CORE/scripts/build_boards.sh 6rv`, an R6 with `G_RISCV`.
- Simulation: `CORE/scripts/run_rv_soc_nvc.sh` (nvc, about 30 seconds on the Mac): the
  HyperRAM bridge, the selftest, then the firmware, which reads the real `config.vhd` and
  stops on its no-SD-card screen. Build the firmware first.
