#!/usr/bin/env bash
# Run tb_reu_sd (the REU's SD card DMA, CORE/vhdl/reu_sd.vhd) with nvc (brew install nvc).
# Takes a few seconds; no Vivado needed.
#
# Usage: CORE/scripts/run_reu_sd_nvc.sh [work dir]

set -euo pipefail

repo=$(cd "$(dirname "$0")/../.." && pwd)
work=${1:-"$repo/CORE/reu_sd_nvc"}
mkdir -p "$work"
cd "$work"

nvc --std=2008 -a \
    "$repo/M2M/vhdl/sd/sdhost.vhd" \
    "$repo/CORE/vhdl/reu_sd.vhd" \
    "$repo/CORE/sim/tb_reu_sd.vhd"
nvc --std=2008 --ieee-warnings=off -e tb_reu_sd -r --exit-severity=error 2>&1 | tee tb_reu_sd.log

grep -q "ALL CHECKS PASSED" tb_reu_sd.log && echo "REU SD SIM: PASSED" || { echo "REU SD SIM: FAILED"; exit 1; }
