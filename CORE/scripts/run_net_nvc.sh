#!/usr/bin/env bash
# Run tb_net (the network device, CORE/vhdl/net) with nvc (brew install nvc): the 6551's
# FIFOs and IRQs against a firmware model's echo, two Ethernet frames through an RMII
# loopback, the DNA. Takes a few seconds; no Vivado needed.
#
# Usage: CORE/scripts/run_net_nvc.sh [work dir]

set -euo pipefail

repo=$(cd "$(dirname "$0")/../.." && pwd)
work=${1:-"$repo/CORE/net_nvc"}
mkdir -p "$work"
cd "$work"

nvc --std=2008 --work=unisim -a "$repo/CORE/sim/nvc_unisim.vhd"
nvc --std=2008 -L . -a \
    "$repo/M2M/vhdl/axis_fifo_async.vhd" \
    "$repo/M2M/vhdl/axis_dropper.vhd" \
    "$repo/M2M/vhdl/controllers/ethernet/eth_rmii.vhd" \
    "$repo/M2M/vhdl/controllers/ethernet/eth_wrapper.vhd" \
    "$repo/CORE/vhdl/net/acia6551.vhd" \
    "$repo/CORE/vhdl/net/net.vhd" \
    "$repo/CORE/sim/tb_net.vhd"
nvc --std=2008 -L . --ieee-warnings=off -e tb_net -r --exit-severity=error 2>&1 | tee tb_net.log
grep -q "ALL CHECKS PASSED" tb_net.log && echo "NET SIM: PASSED" || { echo "NET SIM: FAILED"; exit 1; }
