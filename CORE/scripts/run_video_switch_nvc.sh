#!/usr/bin/env bash
# Run tb_video_switch (HDMI black, not garbage, while 40/80 columns switch:
# M2M/vhdl/av_pipeline/video_switch_blank.vhd) with nvc (brew install nvc), with and
# without scanline_fit and with a downscaled wide picture. About a minute; no Vivado.
#
# Usage: CORE/scripts/run_video_switch_nvc.sh [work dir]

set -euo pipefail

repo=$(cd "$(dirname "$0")/../.." && pwd)
work=${1:-"$repo/CORE/video_switch_nvc"}
mkdir -p "$work"
cd "$work"

# ascal leaves two integers out of range before its first frame; nvc stops on that
sed -e 's/o_pshift<=o_off(0) -1;/o_pshift<=(o_off(0) -1) MOD 16;/' \
    -e 's/o_ihsizem<=o_ihsize + o_off(0) - 2;/o_ihsizem<=(o_ihsize + o_off(0) - 2) MOD 4096;/' \
    "$repo/M2M/vhdl/av_pipeline/ascal.vhd" > ascal_nvc.vhd

nvc --std=2008 -a --relaxed \
    ascal_nvc.vhd \
    "$repo/M2M/vhdl/cdc_stable.vhd" \
    "$repo/M2M/vhdl/av_pipeline/scanline_fit.vhd" \
    "$repo/M2M/vhdl/av_pipeline/video_switch_blank.vhd" \
    "$repo/CORE/sim/tb_video_switch.vhd"

status=0
for fit in false true; do
  for bw in 128 192; do
    log="tb_video_switch_${fit}_${bw}.log"
    nvc --std=2008 --ieee-warnings=off -e -gG_FIT=$fit -gG_BW=$bw tb_video_switch -r > "$log" 2>&1 || true
    echo "== scanline_fit $fit, picture B $bw wide"
    grep -E "^(raw|blanked):|switch to" "$log" || true
    grep -q "ALL CHECKS PASSED" "$log" || status=1
  done
done

[ $status = 0 ] && echo "VIDEO SWITCH SIM: PASSED" || { echo "VIDEO SWITCH SIM: FAILED"; exit 1; }
