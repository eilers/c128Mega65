#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
PROJECT="${1:-CORE/CORE-R6-vivado2022.xpr}"
cd "$ROOT_DIR"

VIVADO=(CORE/scripts/vivado.sh -mode batch)

python3 CORE/scripts/gen_gcr_fixtures.py --self-test
"${VIVADO[@]}" -source CORE/scripts/run_vdrive_index_sim.tcl -tclargs "$PROJECT"
"${VIVADO[@]}" -source CORE/scripts/run_c1541_gcr_codec_sim.tcl -tclargs "$PROJECT"
"${VIVADO[@]}" -source CORE/scripts/run_c157x_track_sim.tcl -tclargs "$PROJECT"
"${VIVADO[@]}" -source CORE/scripts/run_c157x_gcr_path_sim.tcl -tclargs "$PROJECT" tb_c1541_read_write
"${VIVADO[@]}" -source CORE/scripts/run_c157x_gcr_path_sim.tcl -tclargs "$PROJECT" tb_c1571_read_write
"${VIVADO[@]}" -source CORE/scripts/run_drive_led_sim.tcl -tclargs "$PROJECT"
"${VIVADO[@]}" -source CORE/scripts/run_c1581_ready_sim.tcl -tclargs "$PROJECT"
# The read head as it is actually wired into c157x_drv: sync marks and a $52 header
# mark off a real .D64 track. Cheap, and it fails first when the GCR join breaks.
"${VIVADO[@]}" -source CORE/scripts/run_c157x_stream_sim.tcl -tclargs "$PROJECT"
# The DOS disk controller, driven through its own job queue rather than the serial
# bus, so a failure points at the head or the geometry instead of at IEC timing.
"${VIVADO[@]}" -source CORE/scripts/run_c157x_job_sim.tcl -tclargs "$PROJECT"
# Slowest gate by far: the drive has to finish its power-up self test, which is about
# a second of drive time, before it answers the serial bus. Keep it last.
"${VIVADO[@]}" -source CORE/scripts/run_c157x_boot_sim.tcl -tclargs "$PROJECT"
"${VIVADO[@]}" -source CORE/scripts/rtl_elab_check.tcl -tclargs "$PROJECT"

echo "All virtual-drive simulation gates passed."
