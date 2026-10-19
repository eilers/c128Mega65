# HyperRAM Memory Map

The MEGA65's 8 MiB of HyperRAM is shared by the HDMI framebuffer, the virtual drives, the REU and virtual cartridges. This file fixes which part each one owns, so the REU and the virtual cartridge can be built in parallel without colliding.

Addresses are in the units of the `C_HMAP_*` constants in `CORE/vhdl/globals.vhd`: windows of 4 kW (8 KiB). `0x000`–`0x3FF` covers all 8 MiB.

## Layout

| Windows       | Size        | Bytes       | Owner                            | Constant                         |
|---------------|-------------|-------------|----------------------------------|----------------------------------|
| `0x000–0x0FF` | 2 MiB       | 0–2 MiB     | ascal framebuffer (M2M)          | `C_HMAP_M2M`                     |
| `0x100–0x2FF` | up to 4 MiB | 2–6 MiB     | REU                              | `C_HMAP_REU`                     |
| `0x200–0x2FF` | 2 MiB       | 4–6 MiB     | Virtual cartridge                | `C_HMAP_CRT`                     |
| `0x300–0x364` | 808 KiB     | 6–6.8 MiB   | Drive 8 D81 image + guard window | `C_HMAP_VD0`, `C_HMAP_VD0_GUARD` |
| `0x365–0x3C9` | 808 KiB     | 6.8–7.6 MiB | Drive 9 D81 image + guard window | `C_HMAP_VD1`, `C_HMAP_VD1_GUARD` |
| `0x3CA–0x3FF` | 432 KiB     | 7.6–8 MiB   | Unassigned                       |                                  |

The D81 buffers live in HyperRAM because one D81 (819,200 bytes, exactly 100 windows) is more block RAM than the design has left. Each guard window absorbs a write one byte past the end of the image.

The REU's footprint depends on its size:

| REU size | Windows       |
|----------|---------------|
| 512 KiB  | `0x100–0x13F` |
| 2 MiB    | `0x100–0x1FF` |
| 4 MiB    | `0x100–0x2FF` (covers the cartridge region) |

## Decisions

### REU and virtual cartridge are mutually exclusive in 1.1

The expansion port runs in one mode at a time: hardware slot, REU, or virtual cartridge. C64MEGA65 does the same: the three choices form one radio group in its menu. Real hardware has the same limit, because a 1750 has no pass-through port. We start with the simplest version and can relax it later.

Because only one runs at a time, a 4 MiB REU may use the cartridge region. The REU and the cartridge still get different base addresses, so a 512 KiB or 2 MiB REU never overlaps the cartridge. If we later allow both at once with an REU of up to 2 MiB, no memory has to move. That change would still need:

- a rule for the IO2 page (`$DF00–$DFFF`). The REU's registers live there, and so do parts of many cartridges, such as EasyFlash's RAM. A program writing to its cartridge's IO2 address can also start an REU transfer.
- both DMA requests OR'd together onto the CPU.
- both HyperRAM ports active at the same time.

### REU sizes: 512 KiB, 2 MiB, 4 MiB

- **512 KiB** is the Commodore 1750, which most REU software expects.
- **2 MiB and 4 MiB** serve newer software that uses larger REUs. This departs from C64MEGA65, which offers only 512 KiB. We favor flexibility over strict original hardware, as long as the REU fits in HyperRAM.
- **4 MiB is the most that fits.** The framebuffer and both D81 buffers have to stay while the REU runs, which leaves 4 MiB in one block. Software finds an REU's size from where its addresses wrap, so the size must be a power of two. 4 MiB was never a Commodore size, but the 1541 Ultimate and VICE offer it.
- **128 KiB (1700) and 256 KiB (1764) are left out.** Neither the C64 nor the C128 MiSTer core offers them.

MiSTer's `reu.sv` has four size settings on its 2-bit `cfg` input: none, 512 KiB, 2 MiB and 16 MiB. A 16 MiB REU can't fit in HyperRAM, so for 4 MiB we change that setting's address mask to `24'h3FFFFF`. This is a change in our fork of the MiSTer core (`eilers/C128_MiSTer`, the `CORE/C128_MiSTer` submodule).

**Open:** `reu.sv` also has a `wrap` input. It makes a larger REU wrap its addresses at 512 KiB like a real 1750, for old software that depends on that. MiSTer offers it as the menu item "REU wrap". We haven't decided whether to offer it.

### Virtual cartridge region: 2 MiB

This covers the common large cartridges: EasyFlash at 1 MiB and Ocean at up to 512 KiB. C64MEGA65 stores the raw `.crt` file, headers included. If we do the same, the 2 MiB limit applies to the whole file, so a cartridge with a full 2 MiB of ROM won't fit.

### The framebuffer region and triple-buffering

ascal reserves 2 MiB at address 0: `RAMSIZE = 2^ceil(log2(720 × 540 × 3))` (`M2M/vhdl/av_pipeline/digital_pipeline.vhd`). Triple-buffering would need three times that, 0–6 MiB. It is switched off in `CORE/vhdl/mega65.vhd` (`qnice_ascal_triplebuf_o <= '0'`), and with it off ascal only uses buffer 0. This map gives up triple-buffering for good. C64MEGA65 makes the same trade: its released cores (V5.2) keep the REU and cartridge at 4 MiB with the same ascal settings.

**Unexplained:** during virtual-drive bring-up, drive 8's buffer sat at `0x200` and read back as zeros after a mount. With `Empty.d81`, whose only nonzero bytes are on track 40, that shows up as the directory reading `00 00 00 00`. The buffers were then moved above 6 MiB. The comment at the time blamed triple-buffering, but triple-buffering was already off, so the cause is still unknown. If REU or cartridge data ever reads back as zeros, suspect this first.

## Shared code both features touch

These are the places where the REU and cartridge work meet. They are already set up for both, so each feature only edits its own blocks.

- **Core HyperRAM port.** In `CORE/vhdl/mega65.vhd`, `i_core_hr_arbit` (`avm_arbit_general`, 4 ports, its maximum) shares the core's HyperRAM master between disk-image staging, the drive sector fill, the REU (`reu_hr_*`) and the virtual cartridge (`crt_hr_*`). Each feature replaces its own tie-off block above the arbiter.
- **Expansion port mode.** The Expansion Port submenu chooses Hardware, REU 512 KB, REU 2 MB, REU 4 MB or CRT file. In `CORE/vhdl/main.vhd`, `exp_port_mode` decodes it, and `handle_cores_expansion_port_signals_proc` has one `case` arm per source. Each feature fills its own arm, including `core_dma`, the C128's DMA request. The REU reads its size from `C_MENU_EXP_PORT_REU_*`.
- **Menu.** New items go below `CRT file`, the last item with a `C_MENU_*` index, so they shift no index. The cartridge's `CRT:%s` load line goes there. `MENU_HEAP_SIZE` in `CORE/m2m-rom/m2m-rom.asm` has room for it.
- **QNICE device IDs.** The next free ID is `0x0106` (`globals.vhd`).

## Later

8 MiB and 16 MiB REUs don't fit in HyperRAM. R4, R5 and R6 boards have 64 MiB of SDRAM with the same chip and pinout on all three. M2M doesn't drive it yet, so it needs a controller first.
