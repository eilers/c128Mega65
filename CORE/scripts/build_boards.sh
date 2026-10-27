#!/usr/bin/env bash
# Build the bitstreams of several boards at the same time.
#
# The M2M shell ROM is assembled once up front; each board's synthesis then skips
# that step (M2M_ROM_PREBUILT, see CORE/m2m-rom/synth_pre.tcl), so boards building
# side by side never rewrite the same files. Each board gets its own log.
#
# Usage: CORE/scripts/build_boards.sh [board ...]      (default: 6 3)
#   board       3, 4, 5, 6, 6r3: an R6 bitstream with R3's memory map (SDRAM unused),
#               or 6rv: an R6 bitstream with the RISC-V system CPU instead of QNICE
#   JOBS=2      boards building at once; synthesis takes about 8 GB per board
#   LOG_DIR=... where the logs go (default: artifacts/build-logs)

set -uo pipefail

if (( BASH_VERSINFO[0] < 5 || (BASH_VERSINFO[0] == 5 && BASH_VERSINFO[1] < 1) )); then
  echo "ERROR: needs bash 5.1 or newer (wait -n -p)" >&2
  exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CORE_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$CORE_DIR/.." && pwd)"

BOARDS=("$@")
[[ ${#BOARDS[@]} -gt 0 ]] || BOARDS=(6 3)
JOBS="${JOBS:-2}"
LOG_DIR="${LOG_DIR:-$ROOT_DIR/artifacts/build-logs}"
mkdir -p "$LOG_DIR"

for b in "${BOARDS[@]}"; do
  # 6r3 and 6rv build from their own copies of the R6 project, so they can run beside 6
  if [[ "$b" == 6r3 || "$b" == 6rv ]]; then
    sed "s/CORE-R6-vivado2022/CORE-R${b^^}-vivado2022/g" "$CORE_DIR/CORE-R6-vivado2022.xpr" \
      > "$CORE_DIR/CORE-R${b^^}-vivado2022.xpr" || exit 1
  fi
  [[ -f "$CORE_DIR/CORE-R${b^^}-vivado2022.xpr" ]] || { echo "ERROR: no project for R$b" >&2; exit 1; }
done

# The QNICE assembler needs a C preprocessor, which the Vivado Flatpak has (same app as vivado.sh).
echo "Assembling the M2M shell ROM..."
stamp="$(mktemp)"
flatpak run --filesystem="$ROOT_DIR" --command=bash com.github.corna.Vivado \
  -c "cd '$CORE_DIR/m2m-rom' && ./make_rom.sh" > "$LOG_DIR/m2m-rom.log" 2>&1
if [[ ! "$CORE_DIR/m2m-rom/m2m-rom.rom" -nt "$stamp" ]]; then
  rm -f "$stamp"
  echo "ERROR: the ROM was not assembled, see $LOG_DIR/m2m-rom.log" >&2
  exit 1
fi
rm -f "$stamp"
tail -1 "$LOG_DIR/m2m-rom.log"

declare -A board_of start_of rc_of
build() {
  local b="$1"
  cd "$ROOT_DIR" || exit 1
  M2M_ROM_PREBUILT=1 exec CORE/scripts/vivado.sh -mode batch \
    -source CORE/scripts/build_bitstream.tcl \
    -tclargs "CORE/CORE-R${b^^}-vivado2022.xpr" > "$LOG_DIR/build-r${b}.log" 2>&1
}

finish_one() {
  local pid
  wait -n -p pid
  local rc=$?
  local b="${board_of[$pid]}"
  rc_of[$b]=$rc
  echo "R$b done: exit $rc after $(( $(date +%s) - ${start_of[$b]} ))s"
}

running=0
for b in "${BOARDS[@]}"; do
  if (( running >= JOBS )); then
    finish_one
    running=$(( running - 1 ))
  fi
  echo "R$b building, log $LOG_DIR/build-r${b}.log"
  build "$b" &
  board_of[$!]=$b
  start_of[$b]=$(date +%s)
  running=$(( running + 1 ))
done
while (( running > 0 )); do
  finish_one
  running=$(( running - 1 ))
done

# One line per board: setup and hold come from the routed timing summary.
echo
failed=0
for b in "${BOARDS[@]}"; do
  impl="$CORE_DIR/CORE-R${b^^}-vivado2022.runs/impl_1"
  top="mega65_r${b:0:1}"
  timing="$(awk '/^\| Design Timing Summary/ {f=1} f && $1 ~ /^-?[0-9.]+$/ {
                   print "setup WNS " $1 " ns (" $3 " failing), hold WHS " $5 " ns (" $7 " failing)"; exit}' \
            "$impl/${top}_timing_summary_routed.rpt" 2>/dev/null)"
  bram="$(awk -F'|' '/^\| Block RAM Tile/ {gsub(/ /, "", $3); print $3; exit}' \
          "$impl/${top}_utilization_placed.rpt" 2>/dev/null)"
  echo "R$b: exit ${rc_of[$b]}, ${timing:-no timing report}, block RAM ${bram:-?}"
  [[ "${rc_of[$b]}" == 0 ]] || failed=1
done
exit $failed
