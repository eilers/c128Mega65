# HyperRAM Memory Map

The MEGA65's 8 MiB of HyperRAM is shared by the HDMI framebuffer, the REU, virtual cartridges and the RISC-V system CPU's firmware. This file fixes which part each one owns, so the REU and a virtual cartridge can run at the same time without colliding. Every board has the same map. R4 and later boards also have SDRAM, which holds the REU and the virtual cartridge there ([SDRAM](#sdram-r4-and-later)).

Addresses are in the units of the `C_HMAP_*` constants in `CORE/vhdl/globals.vhd`: windows of 4 kW (8 KiB). `0x000`–`0x3FF` covers all 8 MiB.

## Layout

| Windows       | Size        | Bytes       | Owner                            | Constant                         |
|---------------|-------------|-------------|----------------------------------|----------------------------------|
| `0x000–0x07F` | 1 MiB       | 0–1 MiB     | ascal framebuffer (M2M)          | `C_HMAP_M2M`                     |
| `0x080–0x0BF` | 512 KiB     | 1–1.5 MiB   | REU (R3; idle on R4+)            | `C_HMAP_REU`                     |
| `0x0C0–0x1BF` | 2 MiB       | 1.5–3.5 MiB | Virtual cartridge (R3; idle on R4+) | `C_HMAP_CRT`                  |
| `0x1C0–0x1FF` | 512 KiB     | 3.5–4 MiB   | Free (was the modem's firmware) |                                  |
| `0x200–0x3FE` | ~4 MiB      | 4–8 MiB     | RISC-V system CPU's firmware     | `C_HMAP_FIRMWARE`                |
| `0x3FF`       | 8 KiB       | at 8 MiB    | Unassigned (keep free, below)    |                                  |

On R4 and later the virtual cartridge and the REU are in SDRAM, so their HyperRAM regions are idle there.

Disk images take no HyperRAM. The virtual drives read and write them in place on the SD card (`CORE/vhdl/vd_sd.vhd`), which freed the two D81 buffers that used to sit at `0x300–0x3C9`.

`main.vhd` masks the REU's address to its size before `reu_mapper`, so every access stays inside its region, in HyperRAM or SDRAM. The one exception is the read cache's look-ahead, which can read up to 8 words past the end of the REU: on R3 into the cartridge's region, which is harmless because it only reads.

### R3-memory builds: no REU over 512 KiB

R3 has no SDRAM, so its REU and cartridge share HyperRAM, and the REU is 512 KiB at most. The `6r3` build (an R6 with R3's memory map, `G_R3_MEMORY`) is the same. In these builds:

- **The menu leaves out "2 MB" and "16 MB".** `config.vhd` gets `G_R3_MEMORY` from the framework and drops the `C_MENU_R3_DROP` items from `C_MENU_REU_2M` on (94 and 95) from `OPTM_ITEMS_ALL` and `OPTM_GROUPS_ALL`, so the menu has `OPTM_SIZE_ALL − 2` items and every item after them moves up by two.
- **The core can't see them.** `menu_bits` in `mega65.vhd` puts a `'0'` back at bits 94 and 95, so `main.vhd` uses the same `C_MENU_*` indices on every board, and the REU can't be set above 512 KiB.
- **The Shell finds the "CRT file" item** by its menu group (`shell_item` in the firmware, `CORE/firmware/core.c`), so one firmware serves every board.
- **Settings have their own file,** `/c128/c128mega65-<version>-r3.cfg`, two bytes shorter. The Shell only uses a settings file whose length matches the menu, so R3 can't share the R4+ file. Make it with `M2M/tools/make_config.sh <file> auto-r3`; `build_release.sh` ships it with the R3 core.

`CORE/sim/tb_config.vhd` (`CORE/scripts/run_config_nvc.sh`, nvc) checks that the R3 menu is the full one without items 94 and 95, in every selector the Shell reads.

### Free: `0x1C0–0x1FF`

The SwiftLink modem's own CPU ran its firmware from here (`/c128/modem.bin`). Since the modem moved into the RISC-V system firmware (RISC-V step 8), nothing uses this window.

### RISC-V firmware: `0x200–0x3FE`

The RISC-V system CPU (`doc/riscv-system-cpu.md`) sees all of HyperRAM at `0x40000000`, two bytes per word, so this region starts at CPU address `0x40400000`. Its boot ROM loads the firmware here from the SD card (`/c128/firmware.bin`).

Keep the top window (`0x3FF`) unassigned. A read cache can fetch past the address it was asked for (the REU's does, up to 8 words), and on the HyperRAM chips that need the write workaround a single-word write becomes a 2-word burst. At the top of HyperRAM those extra words run past the end of the chip. C64MEGA65 hit this with its REU in the last 512 KiB (its issue #214). Elsewhere the extra read words and the masked second write word are harmless.

## SDRAM (R4 and later)

R4, R5 and R6 boards have 64 MiB of SDRAM, the same chip on all three. C64MEGA65's controller (`M2M/vhdl/controllers/SDRAM/sdram.vhd`) drives it at 100 MHz, on the HyperRAM clock. Its map, in the same units (`C_SMAP_*`, `0x0000–0x1FFF`):

| Windows         | Size    | Bytes     | Owner             | Constant     |
|-----------------|---------|-----------|-------------------|--------------|
| `0x0200–0x03FF` | 4 MiB   | 4–8 MiB   | Virtual cartridge | `C_SMAP_CRT` |
| `0x0800–0x0FFF` | 16 MiB  | 16–32 MiB | REU, every size   | `C_SMAP_REU` |

The other 44 MiB are unassigned.

**Sharing.** `sdram_arbit.vhd` (in `mega65.vhd`) serves the cartridge's reader first, and the REU starts an access only in a window that `main.vhd` opens: clocks 7–15 of PHI2 low (`C_REU_FROM`, `C_REU_TO`). That comes after FAST mode's cartridge read at the start of PHI2 low, and the access is done before the refreshes in the middle of PHI2 high, so it never makes a cartridge read late. The REU still moves a byte per cycle, and SWAP two: its writes are posted, and its read cache fetches ahead. A window that ended at clock 11 cost SWAP a cycle every 8 bytes, because a refill that just missed it held up the next write. With no virtual cartridge running, the REU may start at any time. The system CPU comes third (its window at `0x50000000`, for UCI's LOAD_REU and SAVE_REU and the menu's REU file): it takes the same window as the REU, after it, and its accesses are 2 words, shorter than the REU's 8. `CORE/sim/tb_reu_sdram.vhd` (`CORE/scripts/run_reu_sdram_sim.tcl`) checks every cartridge read against its deadline with the REU running, at 1 MHz, in FAST mode and in Ultimax mode with the VIC fetching, at all three REU sizes, and again with the CPU writing and reading as fast as it can.

**Row ends.** The controller sets the chip to full-page bursts, which wrap within a 1,024-word row. The REU's read cache reads 8 words from any address, so a burst could run past a row's end and read the start of the same row instead of the next. The controller now ends such a read at the row's end and reads the rest from the next row. This is the likely cause of the glitches C64MEGA65 saw with its REU in SDRAM (its issue #206), which it worked around by moving the REU back to HyperRAM. `tb_reu_sdram` FETCHes across a row end, and with the old controller gets 4 of the 64 bytes wrong. Write bursts must still stay in one row, which the controller checks in simulation; the REU writes single words.

## Decisions

### The REU and a cartridge can run together

The Cartridge setting chooses the hardware slot or a virtual cartridge. The REU is a separate setting that works with either, and it is off by default. The MiSTer C128 core allows the same. C64MEGA65 V5.2 makes the three choices one radio group, but its V6 work lets the REU run beside a cartridge too.

The REU and the cartridge each own a region, so they never share memory. One C64MEGA65 bug came from the two overlapping (#198).

When both run:

- **IO2 (`$DF00–$DFFF`).** The REU answers only `$DF00–$DF1F`. `$DF20–$DFFF` belongs to the cartridge, which lets FC3 101% and EasyFlash's cartridge RAM work beside an REU (C64MEGA65 #208). A hardware cartridge that uses `$DF00–$DF1F` still collides with the REU, and a program writing there for its cartridge can start an REU transfer. A virtual cartridge of a type that uses that range (Action Replay, Final Cartridge III, Super Games, Mikro Assembler, EasyFlash) switches the REU off while it runs, as the Ultimate does for its own REU; subtype 1 of Final Cartridge III and EasyFlash, made to share IO2, keeps it. EasyFlash menus such as CCGMS's run code from `$DF00`.
- **DMA and IRQ.** Both DMA requests are OR'd onto the C128, and both IRQs reach the CPU.
- **Memory.** Both ports can be busy at once. On R3 an arbiter takes turns between them in HyperRAM; on R4 and later the cartridge goes first in SDRAM (above).
- **Default off.** C64MEGA65 found that an REU left switched on is a common cause of cartridges and games misbehaving (its open issue #213).

### REU sizes: 512 KiB, 2 MiB and 16 MiB

- **512 KiB** is the Commodore 1750, which most REU software expects.
- **2 MiB** and **16 MiB** serve newer software that uses larger REUs; 16 MiB is the Ultimate's largest. This departs from C64MEGA65, which offers only 512 KiB. R4 and later only: R3 has no SDRAM.
- **128 KiB (1700) and 256 KiB (1764) are left out.** Neither the C64 nor the C128 MiSTer core offers them.

MiSTer's `reu.sv` has four size settings on its 2-bit `cfg` input: none, 512 KiB, 2 MiB and 16 MiB. We use all four unchanged. `reu_cfg` in `CORE/vhdl/main.vhd` uses that encoding.

### No 512 KiB wrap at 2 MiB

`reu.sv` also has a `wrap` input. It makes a 2 MiB REU wrap its addresses at 512 KiB like a 1750 modified to 2 MiB. MiSTer offers it as the menu item "REU wrap" and turns it on by default. Here `main.vhd` ties it to `0`, and the menu doesn't offer it:

- **512 KiB** behaves like C64MEGA65's REU, which is a 1750: a transfer wraps at the end of the 512 KiB, and `$DF06` reads `$F8` with the bank in the low bits. `wrap` makes no difference at this size.
- **2 MiB** and **16 MiB** count straight through all their memory, as the Ultimate and VICE do.

At every size `$DF06` reads its top 5 bits as 1 and the bank in the low 3 bits, like the 8726 controller in a 1750, VICE, and (by OS128's author's account) the Ultimate II+. MiSTer's `reu.sv` reads only the bits its size lacks as 1 (`$E0` at 2 MiB) unless `wrap` is on, so `reu.sv` here is changed to match; the discussion is in MiSTer C128 issue #46.

### Virtual cartridge region: 2 MiB

This covers the common large cartridges: EasyFlash at 1 MiB and Ocean at up to 512 KiB. As in C64MEGA65, the raw `.crt` file is stored, headers included, so the 2 MiB limit applies to the whole file and a cartridge with a full 2 MiB of ROM won't fit. The Shell refuses a larger file before loading it (`core_prep_load` in `CORE/firmware/core.c`). On R4 and later the file goes to SDRAM instead, at `C_SMAP_CRT` (see below), and the limit there is 4 MiB, enough for a full-size Magic Desk 16K with its 2 MiB of ROM. The cartridge device reports its limit in CSR `$020`, and the Shell checks the file against that.

On R3 the C128 never reads this region directly. `crt_cacher.vhd` copies the banks a cartridge has switched in into block RAM: 8 slots of 8 KiB each for ROML and for ROMH (a C128 cartridge's 16 KiB windows take two slots), 32 RAMB36 tiles in all, plus 8 for the 32 KiB of cartridge RAM some types have. A slot fills 256 bytes (one HyperRAM burst) at a time, and the C128 waits only when the CPU reads a burst that is not in yet, as it does for DMA. That burst is loaded next, so a wait lasts a few microseconds rather than the whole bank's 100 or so. Whole-bank waits stopped the CPU for several raster lines while the VIC ran on, which broke games' raster effects (Eye of the Beholder's glitches when the party moves). The cartridge device counts the waits of a C64 cycle or more and the longest (CSR $021–$022), which a build can show on the menu's Cartridge line (CSR $023).

On R4 and later the file goes to SDRAM instead, at `C_SMAP_CRT`, and this HyperRAM region is idle. `crt_reader.vhd` reads each byte the CPU or the VIC asks for straight from the file, as the Ultimate does from its own memory. The request starts as soon as the address is out, and the byte is back about 140 ns later, ahead of FAST mode's 8-clock deadline. Nothing is cached, so the C128 never waits and bank switching costs nothing. SDRAM refreshes are kept to the middle of PHI2, where no request is due, and the REU's accesses to early PHI2 low ([SDRAM](#sdram-r4-and-later)). The cartridge RAM stays in block RAM.

An EasyFlash's flash programs and erases change the stored file (`crt_flash.vhd`; `crt_reader.vhd` on R4 and later, `crt_cacher.vhd` on R3, which also updates its cached copy). In SDRAM they use the same mid-PHI2 window as the refreshes, one access per window, so they never hold up a read. Many EasyFlash files leave out banks that are all `$FF`, save areas included. Each such bank has a spare 8 KiB bank in the region's second MiB: base + 1 MiB + (chip × 64 + bank) × 8 KiB, 1 MiB for both chips. The spares are filled with `$FF` right after the file is parsed, while the C128 is still held in reset, and a bank's first program maps its spare in, which is quick: EAPI waits less than 0.5 ms for a byte. An EasyFlash file is at most 1 MiB + 2 KiB, and only when it has all 128 banks, so a file that lacks one always fits below the spares. Changes last until the cartridge is loaded again or the MEGA65 is switched off.

### The framebuffer region and triple-buffering

ascal reserves 1 MiB at address 0 (`RAMSIZE` in `M2M/vhdl/av_pipeline/digital_pipeline.vhd`). It stores the core's own picture, not the HDMI output: at most about 760 pixels a line (the VDC's) at 3 bytes, 2,304 bytes once rounded up to its 128-byte bursts, so the C128's 312 lines take 0.72 MB. It used to reserve 2 MiB, `2^ceil(log2(720 × 540 × 3))`, sized for the output. ascal masks every write address with `RAMSIZE − 1`, so a picture that doesn't fit wraps inside its megabyte and never writes over the next region (the REU on R3). Its reads aren't masked, so the lines past the end would show that region.

Triple-buffering would need three buffers. It is switched off in `CORE/vhdl/mega65.vhd` (`qnice_ascal_triplebuf_o <= '0'`). `ASCAL_USAGE = 2` in `config.vhd` keeps ascal's mode in sync with that signal, so ascal only uses buffer 0. This map gives up triple-buffering for good. C64MEGA65 V5.2 makes the same trade: with the same ascal settings, it puts its REU and cartridge at `0x200`, inside the triple-buffer area.

**Unexplained:** during virtual-drive bring-up, drive 8's buffer sat at `0x200` and read back as zeros after a mount. With `Empty.d81`, whose only nonzero bytes are on track 40, that shows up as the directory reading `00 00 00 00`. The buffers were then moved above 6 MiB. The comment at the time blamed triple-buffering, but triple-buffering was already off, so the cause is still unknown. `0x200` was the cartridge's base until the RISC-V map, and nothing went wrong there; it is now the RISC-V firmware's. If data there ever reads back as zeros, suspect this first.

## Shared code both features touch

These are the places where the REU and cartridge work meet. They are already set up for both, so each feature only edits its own blocks.

- **Core HyperRAM port.** In `CORE/vhdl/mega65.vhd`, `i_core_hr_arbit` (`avm_arbit`, 2 ports) shares the core's HyperRAM master between the REU (`reu_hr_*`) and the virtual cartridge (`crt_hr_*`). A second `avm_arbit`, `i_dma_hr_arbit`, puts the REU's SD card DMA on R3 memory (`dma_hm_*`) beside the two; elsewhere its second port idles. On R4 and later the REU's port goes to `i_sdram_arbit` instead, beside the cartridge's reader (`reu_sdram_gen`), and the first arbiter idles.
- **Expansion port.** In `CORE/vhdl/main.vhd`, `cart_source` decodes the Cartridge submenu and `reu_cfg` the REU submenu. `handle_cores_expansion_port_signals_proc` has one `case` arm per cartridge source; the virtual cartridge fills its arm, including its DMA request in `v_dma`. The REU is laid over whichever source is active: it drives `reu_dma_req`, `reu_irq` and `reu_dout`, and `reu_io` selects its registers. The SwiftLink modem goes over both: at its base (`$DE00`, `$DF00` or `$D700`) its 8 registers answer, and its 6551 pulls NMI. At `$DF00` it would share the REU's registers, so `reu_cfg` turns the REU off then.
- **Menu.** New items are best added after the Modem submenu, the last one with `C_MENU_*` indices, so they shift no index. An item added before it moves every later index in `globals.vhd`, and the settings files must be made again (`M2M/tools/make_config.sh`, as for the REU's File item). R3-memory builds leave out the 2 MB and 16 MB REU items (above), so keep `C_MENU_*` indices in the full menu's numbering.
- **Device IDs** on the system CPU's device bus. The virtual cartridge is `0x0106`; the next free ID is `0x0107` (`globals.vhd`).
- **The CPU's write signal.** The core's `ram_we` only fires for writes to RAM (`ramWE <= systemWe and cs_ram` in `fpga64_sid_iec.vhd`, since 657f411). Writes to `$DFxx` are I/O, and in C128 mode `$FF00` is the MMU, so neither shows on `ram_we`. `fpga64_sid_iec` therefore also brings out the unfiltered `systemWe`, which is `system_we` in `main.vhd`. The REU takes it for its registers at `$DF00` and for the `$FF00` writes that BASIC 7's `STASH`, `FETCH` and `SWAP` use to start a transfer. The hardware slot takes it too, for R/W, the data bus direction and the EasyFlash 3 checks. A virtual cartridge's bank switching needs it as well.
- **Reset on change.** `exp_change_reset_proc` in `main.vhd` resets the C128 and a hardware cartridge for 50 ms when the REU or Cartridge setting changes, as C64MEGA65 does since its issues #225 and #226, and when a `.crt` file is loaded. If a write-back cache is dirty, the reset waits until it is clean. A virtual cartridge's reset also lasts until the cache holds the banks it starts in. C64MEGA65 resets from the ROM through `M2M$CSR_RESET`, but here that reaches `main.vhd` as `reset_hard_i` and would unmount the virtual drives.
- **Cartridge ROM reads.** ROML/ROMH reads reach the CPU through `cpu_data_in_proc` (`main.vhd`), which has one branch for the hardware slot and one for the virtual cartridge.

## Later

The SDRAM's unassigned 44 MiB could hold the RISC-V firmware's data on R4 and later (`doc/riscv-system-cpu.md`). R3 stays as it is: its REU is 512 KiB at most.
