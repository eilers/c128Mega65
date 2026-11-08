#!/usr/bin/env bash
# Run tb_c128_dma (the system CPU's bus DMA, CORE/vhdl/c128_dma.vhd) with nvc
# (brew install nvc). Takes a few seconds; no Vivado needed.
#
# Usage: CORE/scripts/run_c128_dma_nvc.sh [work dir]

set -euo pipefail

repo=$(cd "$(dirname "$0")/../.." && pwd)
work=${1:-"$repo/CORE/c128_dma_nvc"}
mkdir -p "$work"
cd "$work"

nvc --std=2008 -a \
    "$repo/M2M/vhdl/tdp_ram.vhd" \
    "$repo/M2M/vhdl/2port2clk_ram.vhd" \
    "$repo/CORE/vhdl/c128_dma.vhd" \
    "$repo/CORE/sim/tb_c128_dma.vhd"
nvc --std=2008 --ieee-warnings=off -e tb_c128_dma -r --exit-severity=error 2>&1 | tee tb_c128_dma.log

grep -q "ALL CHECKS PASSED" tb_c128_dma.log && echo "C128 DMA SIM: PASSED" || { echo "C128 DMA SIM: FAILED"; exit 1; }
