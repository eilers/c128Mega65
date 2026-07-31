# Building the core and running simulations

This guide describes how to build the C128 MEGA65 FPGA bitstream and run the
boot-path simulation gate used before flashing hardware.

Target board: **MEGA65 R6**. Vivado project:
`CORE/CORE-R6-vivado2022.xpr` (Vivado 2022).

## Quick start

From the repository root:

```bash
# One-time setup (submodules, QNICE toolchain, M2M shell ROM)
git submodule update --init --recursive
cd M2M/QNICE/tools && ./make-toolchain.sh
cd ../../../CORE/m2m-rom && ./make_rom.sh

# Boot simulation (run before flashing)
CORE/scripts/run_boot_sim.sh

# FPGA bitstream (long-running)
CORE/scripts/vivado.sh -mode batch \
  -source CORE/scripts/build_bitstream.tcl \
  -tclargs CORE/CORE-R6-vivado2022.xpr
```

## Prerequisites

### Repository and submodules

```bash
git clone <repo-url> c128Mega65
cd c128Mega65
git submodule update --init --recursive
```

Submodules include:

| Path | Purpose |
|------|---------|
| `M2M/QNICE` | QNICE assembler and monitor (M2M shell ROM) |
| `CORE/C128_MiSTer` | MiSTer C128 core RTL |

### QNICE toolchain

The M2M menu/shell ROM is assembled with the QNICE toolchain:

```bash
cd M2M/QNICE/tools
./make-toolchain.sh
```

Press Enter at the prompts. When finished, `M2M/QNICE/assembler/qasm` must exist
and be executable.

### M2M shell ROM

```bash
cd CORE/m2m-rom
./make_rom.sh
```

This generates `m2m-rom.rom` and related artifacts used by the Vivado project.
Re-run after changing `globals.vhd` drive/CRT counts or editing `m2m-rom.asm`.

### C128 boot ROM files

Simulation and hardware both need the system ROM bundle on the SD card path.
For simulation, the file must exist in the working tree:

| File | Size | SD card path |
|------|------|--------------|
| `sdcard/c128/boot0.rom` | 73 728 bytes (72 KiB) | `/c128/boot0.rom` |
| `sdcard/c128/boot1.rom` | 196 608 bytes | `/c128/boot1.rom` |

`boot0.rom` is required for the boot simulation. `run_boot_sim.tcl` checks
that it exists before starting xsim.

Copy both files to the MEGA65 SD card before testing on hardware. Without them
the core cannot boot correctly.

### Vivado

Scripts invoke Vivado through the Flatpak package `com.github.corna.Vivado`,
with the repository mounted read/write:

```bash
flatpak install flathub com.github.corna.Vivado   # once
CORE/scripts/vivado.sh -version
```

If Vivado 2022 is installed natively, you can call `vivado` directly with the
same `-mode batch -source … -tclargs …` arguments instead of `vivado.sh`.

## Building the FPGA bitstream

### Command

From the repository root:

```bash
CORE/scripts/vivado.sh -mode batch \
  -source CORE/scripts/build_bitstream.tcl \
  -tclargs CORE/CORE-R6-vivado2022.xpr
```

Equivalent from `CORE/scripts`:

```bash
cd CORE/scripts
./vivado.sh -mode batch -source build_bitstream.tcl -tclargs ../CORE-R6-vivado2022.xpr
```

### What the build does

`build_bitstream.tcl`:

1. Opens the Vivado project (creates a writable `*-vivado2022.xpr` copy if the
   checked-in project file is read-only).
2. Normalizes SystemVerilog file types (Vivado 2022 compatibility).
3. Runs synthesis (`synth_1`, 14 jobs).
4. Runs implementation through bitstream generation (`impl_1`, 14 jobs).

A full build typically takes on the order of **30–60 minutes**, depending on
the machine.

### Output

On success, the bitstream is written under the project run directory, for example:

```
CORE/CORE-R6-vivado2022.runs/impl_1/mega65_r6.bit
```

The build script prints the path at the end. Non-zero exit code `2` indicates
synthesis or implementation failure.

### GUI workflow

To open the project interactively:

```bash
CORE/scripts/vivado.sh CORE/CORE-R6-vivado2022.xpr
```

Use **Generate Bitstream** from the Vivado GUI, or run synthesis/implementation
steps manually.

### Query project without building

```bash
CORE/scripts/vivado.sh -mode batch \
  -source CORE/scripts/vivado_query.tcl \
  -tclargs CORE/CORE-R6-vivado2022.xpr
```

## Running simulations

The repository ships one focused simulation: **C128 boot path** (`CORE/sim/tb_c128_boot.vhd`).
It instantiates `main` with BRAM-backed RAM/ROM preloaded from `boot0.rom` — no
M2M menu, HDMI, or SD-card model.

### Boot simulation

```bash
CORE/scripts/run_boot_sim.sh
```

Optional: pass a different project file:

```bash
CORE/scripts/run_boot_sim.sh CORE/CORE-R6-vivado2022.xpr
```

Exit codes:

| Code | Meaning |
|------|---------|
| `0` | Gate passed — safe to proceed to hardware flash |
| non-zero | Gate failed — do not flash until the boot path is fixed |

### Regression wrapper

```bash
CORE/scripts/run_boot_sim_regression.sh
```

Currently runs the same boot sim once and fails if the gate does not pass.

### Pass criteria

The testbench runs for a fixed simulation time, releases the core from reset,
then checks:

| Check | Requirement |
|-------|-------------|
| Z80 → 8502 handoff | Rising edge on `boot_z80_n` observed |
| Final CPU mode | `boot_z80_n = '1'` (8502 active) |
| RAM activity | `ram_we_count > 0` |

On success, xsim reports:

```
PASS: Z80 handed off to 8502 (ram_we_count=…)
```

On failure:

```
FAIL: no Z80->8502 handoff within …
```

See `doc/z80-first-fetch-boot-hang.md` for background on a boot regression this
gate is designed to catch.

### Logs and generated files

| Output | Location |
|--------|----------|
| NDJSON debug log | `.cursor/debug-boot.log` (auto-generated absolute path) |
| xsim console | Printed to stdout; Vivado may also write under `CORE/CORE-R6-vivado2022.sim/` |
| Path package | `CORE/sim/boot_paths_pkg.vhd` (regenerated each run from repo root) |

`boot_paths_pkg.vhd` is generated by `gen_boot_paths_pkg.tcl` — do not edit it
by hand. It embeds absolute paths to `boot0.rom` and the log file for the
current checkout.

### Simulation internals (reference)

| File | Role |
|------|------|
| `CORE/sim/tb_c128_boot.vhd` | Testbench top |
| `CORE/sim/sim_support_pkg.vhd` | ROM loader and JSON log helper |
| `CORE/scripts/run_boot_sim.tcl` | Vivado xsim driver and exit-code gate |
| `CORE/scripts/gen_boot_paths_pkg.tcl` | Regenerates `boot_paths_pkg.vhd` |

## Release check (optional)

`CORE/scripts/release-check.sh` runs a subset of validation without a full
bitstream build:

```bash
CORE/scripts/release-check.sh
```

Checks: submodules initialized, QNICE assembler present, M2M ROM build, boot
sim gate, and ROM file sizes.

Add `--with-vivado` to include a full Vivado batch build (very long-running).

## Troubleshooting

### `boot0.rom` missing

```
ERROR: boot ROM not found: …/sdcard/c128/boot0.rom
```

Place a 73 728-byte `boot0.rom` at `sdcard/c128/boot0.rom`.

### QNICE assembler not found

Run `git submodule update --init --recursive`, then build the toolchain under
`M2M/QNICE/tools`.

### Flatpak Vivado cannot see the repo

`vivado.sh` passes `--filesystem="$REPO_DIR"` to Flatpak. Run scripts from a
checkout on a mounted filesystem Flatpak can access.

### Boot sim passes but hardware fails

Simulation covers the Z80 boot opcode fetch and MMU handoff, not HDMI, SD
access, or full M2M integration. Use `doc/r6-boot-validation-checklist.md` for
on-board checks. Timing must also close (WNS ≥ 0); see
`doc/r6-constraints-rationale.md`.

### Writable project copy

If `CORE-R6-vivado2022.xpr` is read-only, `build_bitstream.tcl` creates
`CORE-R6-vivado2022-vivado2022.xpr` in the same directory. Prefer pointing
`-tclargs` at the project you intend to modify; the script handles the copy
automatically when needed.

## Related documentation

- [README.md](../README.md) — project overview and SD card layout
- [doc/r6-constraints-rationale.md](r6-constraints-rationale.md) — timing constraints
- [doc/HANDOVER-z80-cpm.md](HANDOVER-z80-cpm.md) — machine handoff for Z80/CP/M debug
- [doc/plans/z80-cpm-test-env.md](plans/z80-cpm-test-env.md) — full test-env plan
- [doc/z80-cpm-debug.md](z80-cpm-debug.md) — CP/M / Z80 integration debug (MEGA65 glue)
- [doc/c128-mister-patch-audit.md](c128-mister-patch-audit.md) — local C128_MiSTer diff audit
- [CORE/diag/z80/README.md](../CORE/diag/z80/README.md) — offline VICE Z80 diagnostic
