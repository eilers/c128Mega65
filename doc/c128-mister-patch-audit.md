# C128_MiSTer local patch audit

Audit of [`CORE/C128_MiSTer`](../CORE/C128_MiSTer) at `1840dd1` versus `upstream/master` (`df74a23`).

**Policy:** Treat upstream C128_MiSTer as stable. Prefer fixes in MEGA65 glue ([`CORE/vhdl/main.vhd`](../CORE/vhdl/main.vhd)). Submodule edits need a clear MEGA65-integration reason (BRAM latency, Vivado, resource use) or should be reverted / moved out.

## Commits ahead of upstream

| Commit | Summary | Class |
|--------|---------|--------|
| `db2f8b0` | Vivado 2022.2 compatibility | Hygiene |
| `8ed0a5b` | Boot process seems successful | Mixed / historical |
| `e703360` | VDC RAM → Block RAM (drop Altera `altsyncram`) | Vivado / resources |
| `b8cb1a0` | ILA test probe ports | Debug (MEGA65) |
| `53431aa` | VIC snow: dedicated `vicRamDin` port | MEGA65 BRAM timing |
| `88e3c73` | Merge upstream/master | Merge |
| `24c88e6` | More Vivado compat after merge | Hygiene |
| `9859ddc` | Z80 first-fetch: prime `$0000` in reset | MEGA65 BRAM timing |
| `9a4b493` | Z80 reset held for full core reset | MEGA65 BRAM / warm reset |
| `1840dd1` | Reset assert unconditional (warm reset) | MEGA65 reset glue |

## Functional diffs (worth tracking)

### 1. Z80 address prime during reset — `rtl/cpu_z80.vhd`

While `reset=1`, latched `addr` is forced to `$0000` so the ROM/MMU pipeline settles before the first T80 fetch.

- **Why:** MEGA65 system ROM is 1-cycle BRAM; MiSTer SDRAM path hides the same latency.
- **Risk:** Changes reset-time bus behaviour vs upstream. Required for boot sim / cold boot today.
- **Action:** Keep until a glue-only solution (e.g. longer ROM setup before releasing `reset_t80`) is proven. Re-validate with boot sim when Vivado is available. Do **not** extend this file with keyboard/`WAIT_n` experiments.

### 2. Reset / `reset_t80` — `rtl/fpga64_sid_iec.vhd`

- Assert `reset`/`reset_t80` for the whole `reset_n=0` pulse (not only on a lucky `preCycle` edge).
- After release, do **not** keep tying `reset_t80` to BUSAK (upstream did `not reset_n and cpuBusAkT80_n`).
- Also freezes `sysEnable`/`rfsh_cycle` while reset is held (sim POR hygiene).

- **Why:** Warm reset was a no-op; Z80 prime needs a full reset pulse.
- **Risk:** Z80 reset timing diverges from MiSTer; could affect post-boot Z80/CP/M if anything assumes upstream reset/BUSAK coupling.
- **Action:** High-priority re-check with Z80 diag T1–T4 once hardware is back. Candidate to narrow (glue-only reset stretch) if CP/M issues correlate.

### 3. VIC `vicRamDin` — `rtl/fpga64_sid_iec.vhd` + `main.vhd`

Submodule adds a separate VIC display data port so VIC can see the byte one cycle later than the CPU (`vicDiAec <= vicRamDin when aec=1`).

**Important glue status:** [`main.vhd`](../CORE/vhdl/main.vhd) currently wires:

```vhdl
vicRamDin => ram_data, -- TEMP: live data (snow-fix timing to be reworked after boot)
```

So the snow-fix port exists in the core, but MEGA65 still feeds **live** `ram_data`, not a delayed `vic_data_r`. The behavioural split is incomplete on the wrapper side.

- **Action:** Either finish delayed VIC data in `main.vhd`, or drop the port and stay byte-identical to upstream until snow is retackled. Incomplete wiring is a footgun for “core patched but glue TEMP”.

### 4. ILA / debug ports — `fpga64_sid_iec.vhd`

Exports `z80_we_o`, `dbg_vic_has_bus_o`, `dbg_enable_vic_o`, `dbg_aec_o`, `dbg_vicdi_o`. In `main.vhd` these are tied to `open`.

- **Action:** Harmless. When debugging, tap them in the wrapper only; no need for further core changes.

### 5. VDC RAM rewrite — `rtl/vdc856x/vdcram.v`

Replaces Altera `altsyncram` with a Vivado Block-RAM template (`ram_style="block"`). Read-during-write same address returns **old** data (upstream write-first bypass removed).

- **Why:** Distributed RAM exhausted SLICEM; placement/boot fragility.
- **Risk:** 80-col / VDC-only edge cases. 40-col CP/M debug should be unaffected.
- **Action:** Keep for Vivado. Retest VDC path separately later.

### 6. Vivado case / cast hygiene

- `fpga64_buslogic.vhd`, `mmu8722.vhd`: `when others`, `crBank` cast
- SID / `cpu_6510` / `fpga64_rgbcolor`: mostly `input`→`input wire`, line-ending / formatting churn

- **Action:** Keep as Vivado noise. No functional CP/M hypothesis.

## Not present (and should stay absent)

Per project policy, do **not** add submodule patches for:

- Z80 `WAIT_n` during IORQ
- `alt_crsr` source changes
- CIA PRA/PRB “live read” experiments
- VIC `$D02F` `k_reg` idle tweaks

Those belong in ignored historical notes only; see [`z80-cpm-debug.md`](z80-cpm-debug.md).

## MEGA65 glue suspects (outside submodule)

Investigate here before touching C128_MiSTer again:

| Area | File | Notes |
|------|------|--------|
| Keyboard→`ps2_key` bridge | `CORE/vhdl/main.vhd` | Edge-only events; Caps Lock / Restore special-cased |
| RAM/ROM mux | `main.vhd` | Live 1-cycle BRAM; comment about rejected hold-bridge |
| `ram_we_o <= ram_we` | `main.vhd` | “Z80 latch writes miss CE” if ANDed with CE |
| `vicRamDin` TEMP | `main.vhd` | Snow port not actually delayed |
| Virtual IEC | `globals.vhd` `C_VDNUM=0` | CP/M disk tests need real IEC today |
| Cart/DMA forced inactive | `main.vhd` | Correct for no-cart; don’t float EXP DMA |

## Recommended offline next steps

1. Keep this audit updated when submodule pin moves.
2. Use [`CORE/diag/z80`](../CORE/diag/z80) on VICE as behavioural golden.
3. When Vivado returns: boot sim gate, then decide whether reset/prime can move entirely into glue.
