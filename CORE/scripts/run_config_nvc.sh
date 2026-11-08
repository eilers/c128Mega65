#!/usr/bin/env bash
# Run tb_config (config.vhd's menu, full and R3-memory) with nvc (brew install nvc), then
# tb_config_dump for the RISC-V firmware's host test.
# Takes a few seconds; no Vivado needed.
#
# Usage: CORE/scripts/run_config_nvc.sh [work dir]

set -euo pipefail

repo=$(cd "$(dirname "$0")/../.." && pwd)
work=${1:-"$repo/CORE/config_nvc"}
mkdir -p "$work"
cd "$work"

nvc --std=2008 -a \
    "$repo/M2M/vhdl/qnice_tools.vhd" \
    "$repo/M2M/vhdl/av_pipeline/video_modes_pkg.vhd" \
    "$repo/CORE/vhdl/globals.vhd" \
    "$repo/CORE/vhdl/config.vhd" \
    "$repo/CORE/sim/tb_config.vhd" \
    "$repo/CORE/sim/tb_config_dump.vhd"
nvc --std=2008 --ieee-warnings=off -e tb_config -r --exit-severity=error 2>&1 | tee tb_config.log

grep -q "ALL CHECKS PASSED" tb_config.log && echo "CONFIG SIM: PASSED" || { echo "CONFIG SIM: FAILED"; exit 1; }

# What the RISC-V firmware's host test reads (M2M/firmware/host)
for r3 in false true; do
  out=config_dump.txt; [ $r3 = true ] && out=config_dump_r3.txt
  nvc --std=2008 --ieee-warnings=off -e tb_config_dump -gG_R3_MEMORY=$r3 -gG_OUT="$work/$out" \
      -r --exit-severity=error > tb_config_dump.log 2>&1 || { cat tb_config_dump.log; exit 1; }
done
echo "CONFIG DUMP: $work/config_dump.txt $work/config_dump_r3.txt"
