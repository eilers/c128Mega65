# RISC-V system CPU

A NEORV32 has replaced QNICE, the M2M framework's menu CPU, on every board. The goal is an
Ultimate-style firmware: the menu, then Gideon's command interface (UCI), REST API and FTP
server, protocol compatible. The QNICE Shell's sources were removed in `a626c1c`; its parent
has them, for reference.

## Plan

1. **Hardware** (done): `M2M/vhdl/rv/rv_soc.vhd`, a drop-in for the `QNICE` entity with
   the same ports, so `config.vhd`, the core's devices and the framework stayed as they
   were. Until step 4 a build switch, `G_RISCV`, picked one or the other.
2. **Firmware base**: the boot ROM loads the firmware from the SD card into HyperRAM and
   runs it there, from `C_HMAP_FIRMWARE` (`0x200`–`0x3FE`, CPU address `0x40400000`,
   `doc/hyperram-map.md`). A main loop in C, SD card, FAT32, menu text, keys.
3. **The Shell in C**, item by item, until it does everything the QNICE Shell does:
   the menu from `config.vhd`, the callbacks in `m2m-rom.asm`, mounting drives, loading
   cartridges and ROMs, the settings file, the help screens. Each step
   is tested on hardware against a QNICE build.
4. **Drop QNICE** (`a626c1c`): RISC-V on every board, QNICE deleted.

Then UCI at `$DF1C` (`doc/uci.md`), then networking: the modem's own NEORV32 moved into
the system CPU in step 8 (`CORE/firmware/net`, `doc/swiftlink-modem.md`), with a 1-second
main-loop watchdog (`fw/watchdog.c`) that stops on the trap screen.

Decisions: the firmware runs from HyperRAM behind the i-cache, loaded from the SD card by
a small boot ROM (no card: a message on the screen; the `.cor` flash slot may come
later); the modem kept its own CPU until the network stage. No RTOS: a main
loop of small modules that never wait, as in the QNICE Shell, written in C
(2026-10-27; this replaces the first plan of FreeRTOS and C++).

## Hardware

NEORV32 v1.13.6 (`CORE/vhdl/neorv32`): rv32imc, 8 KB i-cache, 128 KB data RAM (16 KB until
step 8, hardware interface 2; 64 KB until hardware interface 7), no d-cache yet.

Clocks: the CPU, the boot ROM and the HyperRAM and SDRAM bridges run at 100 MHz, on the
HyperRAM clock, so the bridges talk to their arbiters directly; the framework leaves out the
FIFO that QNICE's port crossed through. The device bus, the registers and the SD
controllers stay at 50 MHz, on the QNICE clock. Both clocks come from one MMCM with phase
0, so the paths between them are timed: a request crosses as a toggle with its address and
data beside it, the answer the same way back. Gideon's U64E-II CPU also runs at 100 MHz.

| Address      | What                                                                     |
|--------------|--------------------------------------------------------------------------|
| `0x00000000` | boot ROM, 8 KB of block RAM, read at synthesis from `M2M/firmware/boot.rom` |
| `0x40000000` | HyperRAM, 8 MB in a 16 MB window: byte A is word A/2, low byte first (`rv_hr_bridge.vhd`) |
| `0x50000000` | SDRAM, 64 MB, the same way (R4 and later; a bus error on R3 memory)       |
| `0x80000000` | data RAM                                                                 |
| `0xC0000000` | QNICE's device bus: word N of the device in `RAMROM_DEV` at `+2N`          |
| `0xE0000000` | registers: CSR, OSM, ascal, keys (and every key), control flags          |
| `0xE0000100` | SD card controller registers 0–7 (`sdhost.vhd`)                           |
| `0xFFE00000` | NEORV32: UART0 (the serial port), CLINT timer                             |

`M2M/firmware/include/m2m_rv.h` has the details.

- **Device bus.** The devices keep QNICE's timing: they act on the falling edge, and the
  CPU reads at the next rising edge without wait. A 16-bit access is one device access,
  a 32-bit access two. Between accesses the select is low for at least one falling
  edge, which `qnice2hyperram`-style devices need to see a new access. There's no
  4K window: the whole 28-bit address is in the CPU's address.
- **HyperRAM.** The CPU has its own Avalon port in place of QNICE's `qnice2hyperram`
  device, which is gone from the device bus (the core's devices still use the entity). Cache lines are read
  in 16-word bursts, one after the other, so HDMI's scaler gets its turns
  (`doc/swiftlink-modem.md`). The port also yields to the scaler (`avm_yield.vhd` in
  `framework.vhd`): a CPU access starts only once the scaler has been idle for 320 ns, or
  after 164 us of waiting. In 80 columns the scaler's reads at the top of a frame otherwise
  ran late with the CPU's turns between them, and HDMI showed noise. From SDRAM they are
  read in 2-word bursts, never longer than the REU's accesses (`doc/hyperram-map.md`).
- **Registers.** The same bits as QNICE's `0xFFE0` and up (QNICE's `sysdef.asm`). The
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
  in the 128 KB data RAM. `main.c` polls each module in turn: the keys (Up and Down repeat,
  as in QNICE's `keyboard.asm`), the SD card (mounted when a card is in, dropped when the
  cards change) and the screen. FatFs R0.16 with its two patches, long names in code page
  437, the menu font's. Everything a user needs to see goes on the screen: the serial
  port needs a JTAG adapter. It gets a banner line, and a monitor (`fw/monitor.c`,
  `tools/mon.py`, see `M2M/firmware/README.md`) that reads and writes memory, devices and
  registers while the core runs.
  The idle main loop's code (`DMEM_CODE`, `include/dmem.h`) runs from the data RAM: from
  HyperRAM, its i-cache misses (the cache is direct-mapped) took HyperRAM turns from the
  HDMI scaler, which then missed the start of a frame in 80 columns and showed noise.
- **The Shell** (`fw/shell.c`, step 5): loads the menu from `config.vhd`, its default
  settings (the core's settings file changes them) and the auto-load ROMs from `globals.vhd`, sets up ascal and
  its filters, and lets the core out of reset; then Help (or F13) opens the options menu.
  `fw/menu.c` is QNICE's `menu.asm` (groups, single-select items, submenus, headlines,
  lines); `fw/cfg.c` reads `config.vhd` and the sysinfo device. The core's callbacks
  (`m2m-rom.asm`'s) are `CORE/firmware/core.c`, which finds menu items by their group
  (`config_ids.h`, made from `config.vhd`). `fw/browse.c` is the file browser
  (`selectfile.asm`, `dirbrowse.asm`). `fw/roms.c` loads `.crt` files, by the device's DMA
  where it has one, and hands the core's own items (`C_CRTROMTYPE_CORE`: the REU's file,
  the ROM picks) to `core.c`. `fw/drives.c` mounts disk images: drives 8 and 9 read them straight
  from the card (`vd_sd.vhd`), so mounting only hands the drive the SD block of each 4 KB
  piece. In builds with R3's memory map, the Cartridge line shows the cartridge cache's
  waits, as QNICE's does.
- **Shared** (`lib/`): the SD card through `sdhost`'s registers (512-byte blocks, a byte
  at a time through its buffer, as QNICE did), FatFs's disk layer, CRC-32, menu text, the
  serial port, a small `printf`, and the trap handler.

## Status

- `tb_rv_soc` passes with the CPU at 100 MHz: registers, keys, the menu RAM, a device with
  wait states, the SD registers, HyperRAM bytes, halves and words, and code run from
  HyperRAM, SDRAM and the data RAM. Its second run boots the firmware from its HyperRAM model: the firmware
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
- The Shell's first part passes on the R6 (`6rv` build of `ac67764` with the HyperRAM
  bridge fix, firmware `8b8d593`): the C128 starts with its ROMs, and the options menu
  works as QNICE's does, submenus, summaries, help page, F13 and CRT emulation included.
  The first firmware (`d6e2b46`) drew CRT emulation as a crosshatch: ascal builds each
  polyphase filter entry from four writes in a row, so the two filters have to be written
  one after the other. Neither the host test nor `tb_rv_soc` can see that order.
- `.crt` files from the file browser (firmware `eb2858f`) pass on the R6.
- Disk images in drives 8 and 9 (firmware `e0704c2`) pass on the R6, except that a reset
  with the menu open froze it. A short press of the reset button resets the HyperRAM side
  with the core; the bridge lost the read it was waiting for, and the CPU waited for ever.
  Since `b7928f4` the bridge finishes the read, or writes again, once that reset is over,
  and the CPU's bus timeout outlasts it (`tb_rv_hr_bridge` checks 3 and 4). That passes on
  the R6 (`6rv` build of `b7928f4`: WNS +0.323 ns, WHS +0.032 ns, 265.5 block RAM tiles).
- The stall line (`09b9d2e`) passes on the R6 in a `6r3rv` build (WNS +0.246 ns, WHS
  +0.019 ns, 297.5 block RAM tiles): a `.crt` game's longest bank wait was 7 us, against
  5 us in the QNICE `6r3` build of the cache fix. So with R3's memory map, the CPU fetching
  its code from HyperRAM barely adds to the cartridge's waits. With that, the Shell does
  what the QNICE Shell does (step 5).
- Without QNICE (`a626c1c`), every board meets timing (WNS / WHS: R6 +0.282 / +0.035 ns,
  `6r3` +0.376 / +0.017, R3 +0.275 / +0.010, R4 +0.202 / +0.036, R5 +0.391 / +0.023), with
  265.5 block RAM tiles, or 297.5 with R3's memory map, of 365. The boot simulation gate
  passes, and the R6 and `6r3` builds pass on the R6: menu, `.crt` files, drives, a reset
  with the menu open, the stall line. R3, R4 and R5 are untested on hardware.

## Building and testing

- Firmware: `make -C M2M/firmware` (xPack `riscv-none-elf-gcc`). The `.rom` files and
  `sdcard/c128/firmware.bin` are committed, so a core builds without the toolchain.
  After a change, copy `build/firmware.bin` to `sdcard/c128/` (from a clean tree: the
  release scripts check it is not older than its sources) and to `/c128/` on the card.
- Host tests: `make -C M2M/firmware host-test` (macOS: `hdiutil` makes the FAT images).
  `test_shell` runs the Shell against `config.vhd` as `run_config_nvc.sh` dumps it (full
  and R3 menus), with the registers and devices faked in `host/host_m2m.c`.
- Bitstream: `CORE/scripts/build_boards.sh`, any board; `6r3` is an R6 with R3's memory
  map (`G_R3_MEMORY`), where the CPU shares HyperRAM with everything else.
- Simulation: `CORE/scripts/run_rv_soc_nvc.sh` (nvc, about 30 seconds on the Mac): the
  HyperRAM bridge, the selftest, then the firmware, which reads the real `config.vhd` and
  stops on its no-SD-card screen. Build the firmware first.
