#!/usr/bin/env bash
# HDMI genlock (HDMI Frame Sync = Raw 50.1 Hz) against a cycle model of ascal's output timing:
# C64MEGA65's bench (CORE/sim/hdmi_genlock, README.md there) with nvc, in the raw PAL modes
# following the VIC-II (pal720, pal576) and the VDC (vdc720, vdc576).
#
# Usage: CORE/scripts/run_hdmi_genlock_nvc.sh [quick|full] [mode ...] [--work DIR]
#   quick (default): the bench's reduced set (near starts, 31 locked frames)
#   full:            every start phase and disturbance, 311 locked frames
#   modes:           pal720 pal576 vdc720 vdc576 (default: all four)
# Ends with "GENLOCK SIM: PASSED" or "GENLOCK SIM: FAILED"; four simulations run at once.

set -uo pipefail

repo=$(cd "$(dirname "$0")/../.." && pwd)
work="$repo/CORE/hdmi_genlock_nvc"
set_args="-gG_SET=near -gG_LOCKED=31"
modes=()
while [ $# -gt 0 ]; do
   case "$1" in
      quick) set_args="-gG_SET=near -gG_LOCKED=31" ;;
      full)  set_args="" ;;
      pal720|pal576|vdc720|vdc576) modes+=("$1") ;;
      --work) shift; work=$1 ;;
      *) echo "usage: $0 [quick|full] [pal720|pal576|vdc720|vdc576 ...] [--work DIR]"; exit 2 ;;
   esac
   shift
done
[ ${#modes[@]} -gt 0 ] || modes=(pal720 pal576 vdc720 vdc576)
mkdir -p "$work"
cd "$work"

sim="$repo/CORE/sim/hdmi_genlock"
nvc --std=2008 -a "$repo/M2M/vhdl/av_pipeline/hdmi_genlock.vhd" \
    "$sim/tb_ascal_timing_pkg.vhd" "$sim/tb_hdmi_genlock_pkg.vhd" "$sim/tb_hdmi_genlock.vhd" \
    > build.log 2>&1 || { cat build.log; echo "GENLOCK SIM: FAILED (build)"; exit 1; }

mode_no() {
   case $1 in pal720) echo 0 ;; pal576) echo 1 ;; vdc720) echo 4 ;; vdc576) echo 5 ;; esac
}

pids=()
for m in "${modes[@]}"; do
   # shellcheck disable=SC2086
   ( nvc --std=2008 --ieee-warnings=off -e tb_hdmi_genlock -gG_MODE="$(mode_no "$m")" $set_args \
        -r --exit-severity=failure > "$m.log" 2>&1; echo $? > "$m.rc" ) &
   pids+=($!)
done
wait "${pids[@]}"

fail=0
for m in "${modes[@]}"; do
   echo "=== $m"
   grep -E 'TB: ' "$m.log" | sed -E 's/^.*(TB: )/    \1/' | tail -12
   if [ "$(cat "$m.rc")" != 0 ] || ! grep -q 'TB: RESULT PASS' "$m.log"; then
      grep -i -E 'error|failure' "$m.log" | head -5 | sed 's/^/    /'
      fail=1
   fi
done
[ $fail = 0 ] && echo "GENLOCK SIM: PASSED" || { echo "GENLOCK SIM: FAILED (logs in $work)"; exit 1; }
