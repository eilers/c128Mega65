#!/usr/bin/env bash
# Run tb_stream_tx (his video and audio streams, CORE/vhdl/net/stream_tx.vhd) with nvc
# (brew install nvc): a VIC model and audio through the network device, the RMII output
# decoded and checked, firmware frames in between, and a stalled streamer's drops. About
# 35 ms of simulated time; no Vivado needed.
#
# Usage: CORE/scripts/run_stream_nvc.sh [work dir]

set -euo pipefail

repo=$(cd "$(dirname "$0")/../.." && pwd)
work=${1:-"$repo/CORE/stream_nvc"}
mkdir -p "$work"
cd "$work"

nvc --std=2008 --work=unisim -a "$repo/CORE/sim/nvc_unisim.vhd"
nvc --std=2008 -L . -a \
    "$repo/M2M/vhdl/axis_fifo_async.vhd" \
    "$repo/M2M/vhdl/axis_dropper.vhd" \
    "$repo/M2M/vhdl/controllers/ethernet/eth_rmii.vhd" \
    "$repo/M2M/vhdl/controllers/ethernet/eth_wrapper.vhd" \
    "$repo/CORE/vhdl/net/acia6551.vhd" \
    "$repo/CORE/vhdl/net/stream_tx.vhd" \
    "$repo/CORE/vhdl/net/net.vhd" \
    "$repo/CORE/sim/tb_stream_tx.vhd"
nvc --std=2008 -L . --ieee-warnings=off -e tb_stream_tx -r --exit-severity=error 2>&1 | tee tb_stream_tx.log
grep -q "ALL CHECKS PASSED" tb_stream_tx.log && echo "STREAM SIM: PASSED" || { echo "STREAM SIM: FAILED"; exit 1; }
