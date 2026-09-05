#!/usr/bin/env bash
# Run every existing simulation testbench against the given Vivado project.
# Fail-fast: the first failing gate stops the suite with a non-zero exit.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
PROJECT="${1:-CORE/CORE-R6-vivado2022.xpr}"

cd "$ROOT_DIR"

VIVADO=(CORE/scripts/vivado.sh -mode batch)

run_step() {
  local title="$1"
  shift
  echo
  echo "=== $title ==="
  "$@"
}

run_tcl() {
  local title="$1"
  local tcl="$2"
  shift 2
  run_step "$title" "${VIVADO[@]}" -source "$tcl" -tclargs "$PROJECT" "$@"
}

echo "Simulation testbench suite"
echo "Project: $PROJECT"
echo "Repository: $ROOT_DIR"

run_step "GCR fixture self-test" python3 CORE/scripts/gen_gcr_fixtures.py --self-test

# Fast, isolated unit gates first.
run_tcl "Drive index bridge (tb_vdrive_index)" CORE/scripts/run_vdrive_index_sim.tcl
run_tcl "GCR codec (tb_c1541_gcr_codec)" CORE/scripts/run_c1541_gcr_codec_sim.tcl
run_tcl "D64/D71 track geometry (tb_c157x_track)" CORE/scripts/run_c157x_track_sim.tcl
run_tcl "1541 GCR path (tb_c1541_read_write)" CORE/scripts/run_c157x_gcr_path_sim.tcl tb_c1541_read_write
run_tcl "1571 GCR path (tb_c1571_read_write)" CORE/scripts/run_c157x_gcr_path_sim.tcl tb_c1571_read_write
run_tcl "Drive LED policy (tb_drive_led_policy)" CORE/scripts/run_drive_led_sim.tcl
run_tcl "VIA Timer 1 (tb_via_timer)" CORE/scripts/run_via_timer_sim.tcl
run_tcl "Drive RAM (tb_iecdrv_mem)" CORE/scripts/run_drive_mem_sim.tcl
run_tcl "Drive DOS ROM handover (tb_drive_rom)" CORE/scripts/run_drive_rom_sim.tcl
run_tcl "1581 ready / sector path (tb_c1581_ready)" CORE/scripts/run_c1581_ready_sim.tcl
run_tcl "D81 mount / 1581 selection (tb_vdrive_mount)" CORE/scripts/run_vdrive_mount_sim.tcl
run_tcl "157x GCR stream (tb_c157x_stream)" CORE/scripts/run_c157x_stream_sim.tcl
run_tcl "157x DOS job queue (tb_c157x_job)" CORE/scripts/run_c157x_job_sim.tcl
run_tcl "1581 IEC control (tb_c1581_iec)" CORE/scripts/run_c1581_iec_sim.tcl

# Long-running bring-up gates last.
run_step "C128 boot path (tb_c128_boot)" CORE/scripts/run_boot_sim.sh "$PROJECT"
run_tcl "1541/1571 bring-up on D64 (tb_c157x_boot)" CORE/scripts/run_c157x_boot_sim.tcl "" d64
if [[ -f "$ROOT_DIR/test/157x-rw/157x-rw-test.d71" ]]; then
  run_tcl "1571 bring-up on D71 (tb_c157x_boot)" CORE/scripts/run_c157x_boot_sim.tcl "" d71
fi

echo
echo "All simulation testbenches passed."
