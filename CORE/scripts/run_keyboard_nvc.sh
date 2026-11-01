#!/usr/bin/env bash
# Run tb_keyboard (the MEGA65 arrows in the C128 matrix, 8502 and Z80) with nvc.
# Takes a second; no Vivado needed.
#
# Usage: CORE/scripts/run_keyboard_nvc.sh [work dir]

set -euo pipefail

repo=$(cd "$(dirname "$0")/../.." && pwd)
work=${1:-"$repo/CORE/keyboard_nvc"}
mkdir -p "$work"
cd "$work"

nvc --std=2008 -a "$repo/CORE/vhdl/keyboard.vhd" "$repo/CORE/sim/tb_keyboard.vhd"
nvc --std=2008 --ieee-warnings=off -e tb_keyboard -r --exit-severity=error 2>&1 | tee tb_keyboard.log

grep -q "ALL CHECKS PASSED" tb_keyboard.log && echo "KEYBOARD SIM: PASSED" || { echo "KEYBOARD SIM: FAILED"; exit 1; }
