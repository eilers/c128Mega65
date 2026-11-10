#!/usr/bin/env bash
# Run tb_via_sr (iecdrv_via6522's shift register sends bit 7 first) with nvc. Seconds.
set -euo pipefail
repo=$(cd "$(dirname "$0")/../.." && pwd)
work=${1:-"$repo/CORE/via_sr_nvc"}
mkdir -p "$work"
cd "$work"
nvc --std=2008 -a --relaxed "$repo/CORE/C128_MiSTer/rtl/iec_drive/iecdrv_via6522.vhd"
nvc --std=2008 -a "$repo/CORE/sim/tb_via_sr.vhd"
nvc --std=2008 --ieee-warnings=off -e tb_via_sr -r --exit-severity=failure 2>&1 | tee tb_via_sr.log
grep -q "ALL CHECKS PASSED" tb_via_sr.log && echo "VIA SR SIM: PASSED" || { echo "VIA SR SIM: FAILED"; exit 1; }
