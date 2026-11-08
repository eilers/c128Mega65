#!/usr/bin/env bash
# Run tb_avm_yield (the system CPU giving HyperRAM turns to the HDMI scaler,
# M2M/vhdl/memory/avm_yield.vhd) with nvc (brew install nvc). Takes seconds; no Vivado needed.
#
# Usage: CORE/scripts/run_avm_yield_nvc.sh [work dir]

set -euo pipefail

repo=$(cd "$(dirname "$0")/../.." && pwd)
work=${1:-"$repo/CORE/avm_yield_nvc"}
mkdir -p "$work"
cd "$work"

nvc --std=2008 -a \
    "$repo/M2M/vhdl/memory/avm_yield.vhd" \
    "$repo/CORE/sim/tb_avm_yield.vhd"
nvc --std=2008 --ieee-warnings=off -e tb_avm_yield -r --exit-severity=error 2>&1 | tee tb_avm_yield.log

grep -q "ALL CHECKS PASSED" tb_avm_yield.log && echo "AVM YIELD SIM: PASSED" || { echo "AVM CACHE SIM: FAILED"; exit 1; }
