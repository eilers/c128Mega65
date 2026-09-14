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

### Saved settings file

The on-screen menu only persists its settings if a file of exactly `OPTM_SIZE`
bytes exists at the `CFG_FILE` path from `CORE/vhdl/config.vhd`. The filename
carries `CORE_VERSION`, so it changes with every release:

| File | Size | SD card path |
|------|------|--------------|
| `CORE/m2m-rom/c128mega65-<version>.cfg` | one byte per menu line | `/c128/c128mega65-<version>.cfg` |

Regenerate it whenever `OPTM_SIZE` or `CORE_VERSION` changes, and delete the
file belonging to the previous version:

```bash
cd M2M/tools
./make_config.sh ../../CORE/m2m-rom/c128mega65-<version>.cfg auto
```

`build_release.sh` ships this file with the release zip.

### Disk images

Disk images are mounted from `/c128` on the SD card. The core accepts `.D64`
(1541), `.D71` (1571) and `.D81` (1581) for both drive 8 and drive 9; the
mounted image decides whether that drive behaves as a 1581, while the menu
chooses between 1541 and 1571 for the 5.25" formats.

Hardware qualification images are in `test/157x-rw/` and `test/1581-rw`. 

The physical six-pin IEC connector is disconnected by default. Enable
`IEC: Use hardware port` in the on-screen menu to use real drives, printers,
or other IEC devices. When disabled, the core releases all physical outputs
and ignores external CLK, DATA, and SRQ levels; virtual drives continue to
work normally. Do not toggle the option during disk access.

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
4. Rejects the run if synthesis created any implicit net (see below).
5. Runs implementation through bitstream generation (`impl_1`, 14 jobs).

A full build typically takes on the order of **30–60 minutes**, depending on
the machine.

### Why the build fails on implicit nets

Verilog creates a one-bit net when a port connection names an identifier that has
not been declared yet, and Vivado then ignores the wider declaration further down
the file, reporting only `WARNING: [Synth 8-8895] '<name>' is already implicitly
declared`. An eight-bit signal silently becomes one bit, and the design still
builds and mostly runs.

This cost several days of debugging on the virtual drives. It is how the 1581 lost
the seek target in its FDC data register, so the drive could only ever reach track
0 and 1 while looking healthy in every other respect; how the 1541/1571 lost the
byte its read head hands to VIA2 and its track number; and how the SID lost its
combined waveforms. Quartus resolves these references against module scope, so
none of it is visible upstream on MiSTer.

No simulation can catch this, which is the important part: xsim resolves names
against the whole scope, so the testbenches see correct full-width signals and
pass while the bitstream is broken. The synthesis log is the only place the
problem shows up, so the build now treats it as a hard error, and
`rtl_elab_check.tcl` promotes the same message with `set_msg_config` for a check
that takes about two minutes instead of a full build.

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

### Inspecting clock-domain-crossing violations

The timing summary only prints the ten worst paths per clock pair, which is not
enough to tell a handful of genuinely unconstrained crossings from hundreds of
paths belonging to a single missing exception. To list *every* violating path
between two clocks, from an already routed checkpoint:

```bash
CORE/scripts/vivado.sh -mode batch \
  -source CORE/scripts/report_cdc_violations.tcl \
  -tclargs CORE/CORE-R6-vivado2022.runs/impl_1/mega65_r6_routed.dcp qnice_clk main_clk
```

Grouping the resulting `PATH` lines by source and destination module usually
points straight at the module whose exception is missing or no longer matches.
Note that a `set_false_path` naming cells by hierarchical path fails *silently*
when a module moves — Vivado only prints a critical warning that is easy to lose
in a build log. Grep the log for `12-4739` after changing the hierarchy.

## Running simulations

The repository ships focused boot, memory, 1581, GCR, geometry, mixed-language,
and LED-policy simulations. Run every existing testbench with:

```bash
CORE/scripts/run_all_sims.sh
```

The first failing gate stops the suite. `build_release.sh` asks whether to run
this suite before synthesizing any board; a failure aborts the core build.
`RUN_TESTBENCHES=y` or `RUN_TESTBENCHES=n` answers that prompt non-interactively.

Every runner script goes through `CORE/scripts/sim_launch.tcl` instead of calling
`launch_simulation` itself. It verifies that the requested top actually took effect,
because Vivado silently falls back to the synthesis top when it cannot parse a
testbench, and it retries once against a wiped `*.sim` directory, because xvhdl
compiles incrementally and leaves units stale when a VHDL package changes.

| Simulation | Testbench | Covers |
|------------|-----------|--------|
| C128 boot path | `CORE/sim/tb_c128_boot.vhd` | `main` with BRAM-backed RAM/ROM preloaded from `boot0.rom` — no M2M menu, HDMI, or SD-card model |
| Drive DOS ROM handover | `CORE/sim/tb_drive_rom.vhd` | `drive_rom_server.vhd` feeding the real `iecdrv_rom.sv`, byte for byte, across a bank switch |
| Drive RAM | `CORE/sim/tb_iecdrv_mem.vhd` | `iecdrv_mem` and `iecdrv_trackmem` storing what the drive CPU and the head actually wrote |
| 1581 drive ready | `CORE/sim/tb_c1581_ready.sv` | `c1581_fdc1772.v` plus `floppy.v` from mounting a D81 through spin-up to the first `sd_rd` |
| GCR codec | `CORE/sim/tb_c1541_gcr_codec.sv` | all valid/invalid 5-bit codes, exhaustive byte round-trip, checksums and D64/D71 geometry |
| D64/D71 request contract | `CORE/sim/tb_c157x_track.sv` | all density zones, D71 side offset, read and write LBA/block-count handshakes |
| 1541/1571 GCR path | `CORE/sim/tb_c157x_gcr_path.sv` | sector-buffer load, sync/data generation and valid write decode on tracks 18 and 53 |
| Drive index bridge | `CORE/sim/tb_vdrive_index.vhd` | drive 0/1 across ascending SystemVerilog and descending VHDL arrays |
| Authentic drive LED | `CORE/sim/tb_drive_led_policy.vhd` | green access/error pulses and delayed yellow dirty-cache indication |
| 1541/1571 bring-up | `CORE/sim/tb_c157x_boot.sv` | mounting a `.D64`, the real `boot1.rom` handover, and the drive CPU from reset to answering the serial bus |

### 1541/1571 bring-up simulation

```bash
CORE/scripts/vivado.sh -mode batch \
  -source CORE/scripts/run_c157x_boot_sim.tcl \
  -tclargs CORE/CORE-R6-vivado2022.xpr
```

Every other drive gate starts from a drive whose CPU is already running. This one
starts before that: it mounts a `.D64`, streams the real `sdcard/c128/boot1.rom`
into `iec_drive` over the same pull handshake the FPGA uses, and then checks that
the drive leaves reset, fetches its reset vector, reaches the DOS entry point at
`$EAA0`, writes its VIA registers and finally drives the IEC DATA line.

The last check is what "the C128 sees a device" means. Reaching it takes about
0.6 ms of drive time, because the DOS runs a 256x256 zero-page test at `$EAB2`
before it ever looks at the serial bus — the same reason a real 1541 needs a
second or two after power-on. The gate therefore runs for seconds of simulated
time and takes roughly 20 minutes; `run_all_sims.sh` keeps it last.

`run_c157x_boot_sim.tcl` regenerates `CORE/sim/sim_boot1_path_pkg.sv` with the
absolute path to `boot1.rom` for the current checkout — do not edit it by hand.

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

### Drive DOS ROM simulation

```bash
CORE/scripts/vivado.sh -mode batch \
  -source CORE/scripts/run_drive_rom_sim.tcl \
  -tclargs CORE/CORE-R6-vivado2022.xpr
```

Run this after touching `drive_rom_server.vhd`, the drive-ROM block RAM in
`mega65.vhd`, or `iecdrv_rom.sv`. It loads two banks of a synthetic `boot1.rom`
through the real pull handshake and compares all 65536 bytes, then checks that
switching the bank (what changing the drive model in the menu does) invalidates
the old image.

This matters because a broken handover is completely silent: `iecdrv_rom` raises
`rom_valid` after 32768 write strobes regardless of *which* bytes arrived, so the
drive leaves reset and executes garbage. The only symptom on hardware is
"device not present", with the mount itself appearing to work.

### Drive RAM simulation

```bash
CORE/scripts/vivado.sh -mode batch \
  -source CORE/scripts/run_drive_mem_sim.tcl \
  -tclargs CORE/CORE-R6-vivado2022.xpr
```

Run this after touching `iecdrv_misc.sv`. Both `iecdrv_mem` (the drives' work RAM)
and `iecdrv_trackmem` (the GCR track buffer) were `altsyncram` instances upstream
and had to be re-inferred for Vivado, and there is one way to get that wrong that
neither synthesis nor a boot test will show you: every caller strobes `wren` for a
single cycle (`ena_r` and `ph2_f` for the CPU, `buff_we` per bit-cell for the head)
while address and data are only valid during that strobe. A version that registers
the address a cycle ahead of the data stores the byte that *follows* the write.

The drive then still boots — its DOS runs from ROM, so it answers ATN and does not
report "device not present" — but every RAM-held variable is wrong. On hardware that
looks like a disk-data problem: `DS$` comes back empty, `DIRECTORY` produces no
output, and the drive stalls the IEC bus so a real drive on the physical port stops
working too. The testbench drives the same bus pattern the 6502 does, including the
bus activity right after the strobe, and writes the array twice (ascending, then
descending) so that an off-by-one address cannot look self-consistent.

Exit code `0` means all bytes matched; non-zero means the image was corrupted or
the handshake stalled.

### 1581 drive ready and sector data path simulation

```bash
CORE/scripts/vivado.sh -mode batch \
  -source CORE/scripts/run_c1581_ready_sim.tcl \
  -tclargs CORE/CORE-R6-vivado2022.xpr
```

Run this after touching `c1581_fdc1772.v`, `floppy.v`, or the drive clock enables in
`main.vhd`. It mounts a D81-sized image, checks the geometry the FDC derives from the
image size, turns the motor on and waits for `floppy_ready`. It then issues a READ SECTOR
and follows the sector the whole way: the request must appear on `sd_rd` with the right
LBA, all 512 bytes must land in the sector buffer, and the drive CPU must be able to fetch
those same 512 bytes back out of the data register with no `RECORD NOT FOUND` and no lost
data. In short, it reproduces both the chain the DOS walks before it can read a byte and
the path that byte then takes.

The host side of it is modelled on what the QNICE Shell really does rather than on what
the protocol comment says: it runs on a 50 MHz clock, spends many cycles per register
store, writes the buffer address before the data, and — like `HANDLE_DRV_RD` — never
lowers the write enable between bytes. That last detail matters, because it is why the
write strobe cannot be used to count how many bytes arrived.

This exists because that chain broke in a way no boot test could show. The generate
block instantiating the `floppy` models connects `.select(fd_any && fdn == i)` and
`.motor_on(fd_motor)`, but all three signals are declared *below* it in the upstream
source. Quartus binds those to module scope; Verilog instead creates an implicit net
where an undeclared identifier appears in a port connection, and inside a generate block
that net is local to the block. Vivado said so — `Net fdd[0].fd_any ... does not have
driver` — but only as a warning among hundreds. The drive booted, answered the IEC bus
and reported the right DOS version; it simply could never spin its disk, so every
command came back `74, DRIVE NOT READY`.

Three things had to change before the module could be simulated at all. `fdn` was driven
from an `always` block with neither a sensitivity list nor a timing control, which
synthesis reads as combinational logic but a simulator has to execute as an infinite
zero-delay loop — xsim hangs at time zero. The registers in both files had no
initialisers, so xsim starts them at X where a Xilinx FPGA powers them up at 0; the X on
`motor_spin_up_sequence` alone is enough to stop every command from ever executing. And
every variable that the upstream source declares inside an `always` block had to move to
module scope, because xsim treats such a variable as automatic and re-initialises it on
every clock edge. Nothing that has to remember anything survives that: `sd_ackD` never saw
the falling edge of `sd_ack` so the FDC stayed in `SD_READ` forever, and
`data_transfer_state` kept re-requesting the same sector. Vivado's synthesiser does *not*
do this — the netlist has real `sd_ackD_reg` and `data_transfer_state_reg` flip-flops — so
this was purely the simulator disagreeing with the hardware, and until it was fixed the
gate blamed the design for a fault that only existed in xsim.

The testbench tells the FDC its clock enable is ten times slower than it really is. Every
modelled floppy time is derived from that parameter, so a 500 ms spin-up and a 200 ms
revolution compress to a tenth without changing any logic, which is the difference
between a 15 second gate and a 30 minute one.

Exit code `0` means the drive reached ready and read a complete, correct sector.

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
- [doc/r6-boot-validation-checklist.md](r6-boot-validation-checklist.md) — hardware validation
- [doc/r6-constraints-rationale.md](r6-constraints-rationale.md) — timing constraints
- [doc/z80-first-fetch-boot-hang.md](z80-first-fetch-boot-hang.md) — boot hang post-mortem
