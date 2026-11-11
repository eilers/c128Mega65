#!/usr/bin/env bash
# Run tb_mouse_input (Amiga mouse detection, M2M/vhdl/controllers/M65/mouse_input.vhdl)
# with nvc (brew install nvc). Takes seconds; no Vivado needed.
#
# Usage: CORE/scripts/run_mouse_input_nvc.sh [work dir]

set -euo pipefail

repo=$(cd "$(dirname "$0")/../.." && pwd)
work=${1:-"$repo/CORE/mouse_input_nvc"}
mkdir -p "$work"
cd "$work"

nvc --std=2008 -a \
    "$repo/M2M/vhdl/controllers/M65/mouse_input.vhdl" \
    "$repo/CORE/sim/tb_mouse_input.vhd"
nvc --std=2008 --ieee-warnings=off -e tb_mouse_input -r --exit-severity=error 2>&1 | tee tb_mouse_input.log

grep -q "ALL CHECKS PASSED" tb_mouse_input.log && echo "MOUSE INPUT SIM: PASSED" || { echo "MOUSE INPUT SIM: FAILED"; exit 1; }
