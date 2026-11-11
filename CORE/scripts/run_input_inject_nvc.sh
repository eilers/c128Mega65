#!/usr/bin/env bash
# Run tb_input_inject (REST machine:input's device, CORE/vhdl/input_inject.vhd) with nvc.
# Takes a second; no Vivado needed.
#
# Usage: CORE/scripts/run_input_inject_nvc.sh [work dir]

set -euo pipefail

repo=$(cd "$(dirname "$0")/../.." && pwd)
work=${1:-"$repo/CORE/input_inject_nvc"}
mkdir -p "$work"
cd "$work"

nvc --std=2008 -a \
  "$repo/CORE/vhdl/input_inject_pkg.vhd" \
  "$repo/CORE/vhdl/input_inject.vhd" \
  "$repo/CORE/sim/tb_input_inject.vhd"
nvc --std=2008 --ieee-warnings=off -e tb_input_inject -r --exit-severity=error 2>&1 | tee tb_input_inject.log

grep -q "ALL CHECKS PASSED" tb_input_inject.log && echo "INPUT INJECT SIM: PASSED" || { echo "INPUT INJECT SIM: FAILED"; exit 1; }
