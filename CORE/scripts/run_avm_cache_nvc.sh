#!/usr/bin/env bash
# Run tb_avm_cache (the REU's cache line and its drop, M2M/vhdl/memory/avm_cache.vhd) with
# nvc (brew install nvc). Takes a second; no Vivado needed.
#
# Usage: CORE/scripts/run_avm_cache_nvc.sh [work dir]

set -euo pipefail

repo=$(cd "$(dirname "$0")/../.." && pwd)
work=${1:-"$repo/CORE/avm_cache_nvc"}
mkdir -p "$work"
cd "$work"

nvc --std=2008 -a \
    "$repo/M2M/vhdl/memory/avm_cache.vhd" \
    "$repo/CORE/sim/tb_avm_cache.vhd"
nvc --std=2008 --ieee-warnings=off -e tb_avm_cache -r --exit-severity=error 2>&1 | tee tb_avm_cache.log

grep -q "ALL CHECKS PASSED" tb_avm_cache.log && echo "AVM CACHE SIM: PASSED" || { echo "AVM CACHE SIM: FAILED"; exit 1; }
