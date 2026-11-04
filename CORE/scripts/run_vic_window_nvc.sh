#!/usr/bin/env bash
# Run tb_vic_window with nvc (brew install nvc): the core's VIC on its own, PAL, 25 rows and
# 40 columns; the display must sit at x 32..351, raster 51..250 of the colorX/colorY that his
# video stream (stream_tx) takes, with no blanking inside its 384x272 window. Seconds.
#
# Usage: CORE/scripts/run_vic_window_nvc.sh [work dir]

set -euo pipefail

repo=$(cd "$(dirname "$0")/../.." && pwd)
work=${1:-"$repo/CORE/vic_window_nvc"}
mkdir -p "$work"
cd "$work"

nvc --std=2008 -a \
    "$repo/CORE/C128_MiSTer/rtl/video_vicII_656x.vhd" \
    "$repo/CORE/sim/tb_vic_window.vhd"
nvc --std=2008 --ieee-warnings=off -e tb_vic_window -r --exit-severity=error 2>&1 | tee tb_vic_window.log
grep -q "ALL CHECKS PASSED" tb_vic_window.log && echo "VIC WINDOW SIM: PASSED" || { echo "VIC WINDOW SIM: FAILED"; exit 1; }
