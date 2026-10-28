#!/usr/bin/env bash
# Run tb_uci (the Ultimate Command Interface, CORE/vhdl/uci) with nvc (brew install nvc).
# Takes a few seconds; no Vivado needed.
#
# Usage: CORE/scripts/run_uci_nvc.sh [work dir]

set -euo pipefail

repo=$(cd "$(dirname "$0")/../.." && pwd)
work=${1:-"$repo/CORE/uci_nvc"}
mkdir -p "$work"
cd "$work"

nvc --std=2008 -a \
    "$repo/M2M/vhdl/tdp_ram.vhd" \
    "$repo/M2M/vhdl/2port2clk_ram.vhd" \
    "$repo/CORE/vhdl/uci/io_bus_pkg.vhd" \
    "$repo/CORE/vhdl/uci/slot_bus_pkg.vhd" \
    "$repo/CORE/vhdl/uci/command_if_pkg.vhd" \
    "$repo/CORE/vhdl/uci/command_protocol.vhd" \
    "$repo/CORE/vhdl/uci/uci.vhd" \
    "$repo/CORE/sim/tb_uci.vhd"
nvc --std=2008 --ieee-warnings=off -e tb_uci -r --exit-severity=error 2>&1 | tee tb_uci.log

grep -q "ALL CHECKS PASSED" tb_uci.log && echo "UCI SIM: PASSED" || { echo "UCI SIM: FAILED"; exit 1; }
