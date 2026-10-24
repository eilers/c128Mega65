# HyperRAM Memory Map

The MEGA65's 8 MiB of HyperRAM is shared by the HDMI framebuffer, the virtual drives, the REU and virtual cartridges. This file fixes which part each one owns, so the REU and a virtual cartridge can run at the same time without colliding.

Addresses are in the units of the `C_HMAP_*` constants in `CORE/vhdl/globals.vhd`: windows of 4 kW (8 KiB). `0x000`–`0x3FF` covers all 8 MiB.

## Layout

| Windows       | Size        | Bytes       | Owner                            | Constant                         |
|---------------|-------------|-------------|----------------------------------|----------------------------------|
| `0x000–0x0FF` | 2 MiB       | 0–2 MiB     | ascal framebuffer (M2M)          | `C_HMAP_M2M`                     |
| `0x100–0x1FF` | up to 2 MiB | 2–4 MiB     | REU                              | `C_HMAP_REU`                     |
| `0x200–0x2FF` | 2 MiB       | 4–6 MiB     | Virtual cartridge                | `C_HMAP_CRT`                     |
| `0x300–0x364` | 808 KiB     | 6–6.8 MiB   | Drive 8 D81 image + guard window | `C_HMAP_VD0`, `C_HMAP_VD0_GUARD` |
| `0x365–0x3C9` | 808 KiB     | 6.8–7.6 MiB | Drive 9 D81 image + guard window | `C_HMAP_VD1`, `C_HMAP_VD1_GUARD` |
| `0x3CA–0x3FF` | 432 KiB     | 7.6–8 MiB   | Unassigned                       |                                  |

The D81 buffers live in HyperRAM because one D81 (819,200 bytes, exactly 100 windows) is more block RAM than the design has left. Each guard window absorbs a write one byte past the end of the image.

A 512 KiB REU uses `0x100–0x13F`, a 2 MiB REU `0x100–0x1FF`. `main.vhd` masks the REU's address to its size before `reu_mapper`, so every access stays inside that range. The one exception is the read cache's look-ahead, which can read up to 8 words past the end of a 2 MiB REU, into the cartridge's region.

Keep the top window (`0x3FF`) unassigned. The REU's read cache fetches up to 8 words past the address it was asked for, and on the HyperRAM chips that need the write workaround a single-word write becomes a 2-word burst. At the top of HyperRAM those extra words run past the end of the chip. C64MEGA65 hit this with its REU in the last 512 KiB (its issue #214). Elsewhere the extra read words and the masked second write word are harmless.

## Decisions

### The REU and a cartridge can run together

The Cartridge setting chooses the hardware slot or a virtual cartridge. The REU is a separate setting that works with either, and it is off by default. The MiSTer C128 core allows the same. C64MEGA65 V5.2 makes the three choices one radio group, but its V6 work lets the REU run beside a cartridge too.

The REU and the cartridge each own a region, so they never share memory. One C64MEGA65 bug came from the two overlapping (#198).

When both run:

- **IO2 (`$DF00–$DFFF`).** The REU answers only `$DF00–$DF1F`. `$DF20–$DFFF` belongs to the cartridge, which lets FC3 101% and EasyFlash's cartridge RAM work beside an REU (C64MEGA65 #208). A hardware cartridge that uses `$DF00–$DF1F` still collides with the REU, and a program writing there for its cartridge can start an REU transfer. A virtual cartridge of a type that uses that range (Action Replay, Final Cartridge III, Super Games, Mikro Assembler, EasyFlash) switches the REU off while it runs, as the Ultimate does for its own REU; subtype 1 of Final Cartridge III and EasyFlash, made to share IO2, keeps it. EasyFlash menus such as CCGMS's run code from `$DF00`.
- **DMA and IRQ.** Both DMA requests are OR'd onto the C128, and both IRQs reach the CPU.
- **HyperRAM.** Both ports can be busy at once, and the arbiter shares between them.
- **Default off.** C64MEGA65 found that an REU left switched on is a common cause of cartridges and games misbehaving (its open issue #213).

### REU sizes: 512 KiB and 2 MiB

- **512 KiB** is the Commodore 1750, which most REU software expects.
- **2 MiB** serves newer software that uses larger REUs. This departs from C64MEGA65, which offers only 512 KiB. Larger REUs wait for SDRAM (see Later).
- **128 KiB (1700) and 256 KiB (1764) are left out.** Neither the C64 nor the C128 MiSTer core offers them.

MiSTer's `reu.sv` has four size settings on its 2-bit `cfg` input: none, 512 KiB, 2 MiB and 16 MiB. We use the first three unchanged. `reu_cfg` in `CORE/vhdl/main.vhd` already uses that encoding.

### No 512 KiB wrap at 2 MiB

`reu.sv` also has a `wrap` input. It makes a 2 MiB REU wrap its addresses at 512 KiB like a 1750 modified to 2 MiB. MiSTer offers it as the menu item "REU wrap" and turns it on by default. Here `main.vhd` ties it to `0`, and the menu doesn't offer it:

- **512 KiB** behaves like C64MEGA65's REU, which is a 1750: a transfer wraps at the end of the 512 KiB, and `$DF06` reads `$F8` with the bank in the low bits. `wrap` makes no difference at this size.
- **2 MiB** counts straight through all 2 MiB, as the Ultimate and VICE do.

At both sizes `$DF06` reads its top 5 bits as 1 and the bank in the low 3 bits, like the 8726 controller in a 1750, VICE, and (by OS128's author's account) the Ultimate II+. MiSTer's `reu.sv` reads only the bits its size lacks as 1 (`$E0` at 2 MiB) unless `wrap` is on, so `reu.sv` here is changed to match; the discussion is in MiSTer C128 issue #46.

### Virtual cartridge region: 2 MiB

This covers the common large cartridges: EasyFlash at 1 MiB and Ocean at up to 512 KiB. As in C64MEGA65, the raw `.crt` file is stored, headers included, so the 2 MiB limit applies to the whole file and a cartridge with a full 2 MiB of ROM won't fit. The Shell refuses a larger file before loading it (`PREP_LOAD_IMAGE` in `m2m-rom.asm`). On R4 and later the file goes to SDRAM instead (see below), and the limit there is 4 MiB, enough for a full-size Magic Desk 16K with its 2 MiB of ROM. The cartridge device reports its limit in CSR `$020`, and the Shell checks the file against that.

On R3 the C128 never reads this region directly. `crt_cacher.vhd` copies the banks a cartridge has switched in into block RAM: 8 slots of 8 KiB each for ROML and for ROMH (a C128 cartridge's 16 KiB windows take two slots), 32 RAMB36 tiles in all, plus 8 for the 32 KiB of cartridge RAM some types have. While a bank is being fetched, the C128 waits, as it does for DMA.

On R4 and later the file goes to SDRAM instead, at the same address, and this HyperRAM region stays unused. `crt_reader.vhd` reads each byte the CPU or the VIC asks for straight from the file, as the Ultimate does from its own memory. The request starts as soon as the address is out, and the byte is back about 140 ns later, ahead of FAST mode's 8-clock deadline. Nothing is cached, so the C128 never waits and bank switching costs nothing. SDRAM refreshes are kept to the middle of PHI2, where no request is due. The cartridge RAM stays in block RAM.

### The framebuffer region and triple-buffering

ascal reserves 2 MiB at address 0: `RAMSIZE = 2^ceil(log2(720 × 540 × 3))` (`M2M/vhdl/av_pipeline/digital_pipeline.vhd`). Triple-buffering would need three times that, 0–6 MiB. It is switched off in `CORE/vhdl/mega65.vhd` (`qnice_ascal_triplebuf_o <= '0'`). `ASCAL_USAGE = 2` in `config.vhd` keeps ascal's mode in sync with that signal, so ascal only uses buffer 0. This map gives up triple-buffering for good. C64MEGA65 V5.2 makes the same trade: with the same ascal settings, it puts its REU and cartridge at `0x200`, inside the triple-buffer area.

**Unexplained:** during virtual-drive bring-up, drive 8's buffer sat at `0x200` and read back as zeros after a mount. With `Empty.d81`, whose only nonzero bytes are on track 40, that shows up as the directory reading `00 00 00 00`. The buffers were then moved above 6 MiB. The comment at the time blamed triple-buffering, but triple-buffering was already off, so the cause is still unknown. `0x200` is now the cartridge's base. If REU or cartridge data ever reads back as zeros, suspect this first.

## Shared code both features touch

These are the places where the REU and cartridge work meet. They are already set up for both, so each feature only edits its own blocks.

- **Core HyperRAM port.** In `CORE/vhdl/mega65.vhd`, `i_core_hr_arbit` (`avm_arbit_general`, 4 ports, its maximum) shares the core's HyperRAM master between disk-image staging, the drive sector fill, the REU (`reu_hr_*`) and the virtual cartridge (`crt_hr_*`). Each feature replaces its own tie-off block above the arbiter.
- **Expansion port.** In `CORE/vhdl/main.vhd`, `cart_source` decodes the Cartridge submenu and `reu_cfg` the REU submenu. `handle_cores_expansion_port_signals_proc` has one `case` arm per cartridge source; the virtual cartridge fills its arm, including its DMA request in `v_dma`. The REU is laid over whichever source is active: it drives `reu_dma_req`, `reu_irq` and `reu_dout`, and `reu_io` selects its registers.
- **Menu.** New items go below `CRT file`, the last item with a `C_MENU_*` index, so they shift no index. The cartridge's `CRT:%s` load line is there.
- **QNICE device IDs.** The virtual cartridge is `0x0106`; the next free ID is `0x0107` (`globals.vhd`).
- **The CPU's write signal.** The core's `ram_we` only fires for writes to RAM (`ramWE <= systemWe and cs_ram` in `fpga64_sid_iec.vhd`, since 657f411). Writes to `$DFxx` are I/O, and in C128 mode `$FF00` is the MMU, so neither shows on `ram_we`. `fpga64_sid_iec` therefore also brings out the unfiltered `systemWe`, which is `system_we` in `main.vhd`. The REU takes it for its registers at `$DF00` and for the `$FF00` writes that BASIC 7's `STASH`, `FETCH` and `SWAP` use to start a transfer. The hardware slot takes it too, for R/W, the data bus direction and the EasyFlash 3 checks. A virtual cartridge's bank switching needs it as well.
- **Reset on change.** `exp_change_reset_proc` in `main.vhd` resets the C128 and a hardware cartridge for 50 ms when the REU or Cartridge setting changes, as C64MEGA65 does since its issues #225 and #226, and when a `.crt` file is loaded. If a write-back cache is dirty, the reset waits until it is clean. A virtual cartridge's reset also lasts until the cache holds the banks it starts in. C64MEGA65 resets from the ROM through `M2M$CSR_RESET`, but here that reaches `main.vhd` as `reset_hard_i` and would unmount the virtual drives.
- **Cartridge ROM reads.** ROML/ROMH reads reach the CPU through `cpu_data_in_proc` (`main.vhd`), which has one branch for the hardware slot and one for the virtual cartridge.

## Later

REUs larger than 2 MiB don't fit in HyperRAM beside a cartridge. R4, R5 and R6 boards have 64 MiB of SDRAM with the same chip and pinout on all three. The framework now drives it (C64MEGA65's controller) for the virtual cartridge, which uses a few MiB at most. An REU there would have to share it without delaying the cartridge's reads.
