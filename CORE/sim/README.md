# C128 boot simulation

Focused testbench for `main` + BRAM RAM + `boot0.rom` (no M2M menu/HDMI).

## Run (required before hardware flash)

```bash
cd CORE/scripts
./run_boot_sim.sh
```

Exit code 0 = gate passed. Exit code 2 = **do not flash** — boot path is broken.

Full regression (baseline + rejected H-V23 bridge model):

```bash
./run_boot_sim_regression.sh
```

Logs:

- NDJSON: `.cursor/debug-b576b7.log`
- Console: `CORE/sim/boot_sim_console.log`

## Pass criteria (sim-boot-41+)

| Check | Requirement |
|-------|-------------|
| Boot ladder | `final_boot_stage` = 7 |
| MMU handoff | `final_z80_n` = `'1'` (8502 mode) |
| Z80 RAM reads | `z80_ram_rd_mismatch` = 0 (H15 + 1-cycle BRAM model) |
| RAM writes | `ram_we_count` > 0 |

The old gate only counted `ramWE` edges. That missed H-V10/H-V22/H-V23 regressions where
simulation still reached stage 7 in some builds but **hardware stuck at magenta stage 101**.

## Z80 RAM read model

During Z80 boot (`boot_z80_n='0'`), each `ramCE` read compares `ram_data_i` against
`shadow_ram(bram_addr_reg)` where `bram_addr_reg` tracks the BRAM port address with
1-cycle latency (matches `tdp_ram`).

## Rejected memory bridge regression

`boot_paths_pkg` constant `C_MEM_BRIDGE`:

- `false` (default): baseline H15 path — must pass
- `true`: models rejected H-V23 mega65 BRAM bridge — must **fail** gate

```bash
./run_boot_sim.sh ../CORE-R6-vivado2022.xpr --mem-bridge
```

## Files

| File | Role |
|------|------|
| `tb_c128_boot.vhd` | Testbench top |
| `sim_support_pkg.vhd` | ROM loader + debug log helper |
| `boot_paths_pkg.vhd` | Auto-generated absolute paths (do not edit) |
| `../scripts/run_boot_sim.tcl` | Vivado xsim driver + gate |
| `../scripts/gen_boot_paths_pkg.tcl` | Regenerates `boot_paths_pkg.vhd` |
