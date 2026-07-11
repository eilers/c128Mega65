#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TS="$(date +%Y%m%d-%H%M%S)"
ARTIFACT_DIR="$ROOT_DIR/artifacts/release-check-$TS"
mkdir -p "$ARTIFACT_DIR"
SUMMARY="$ARTIFACT_DIR/summary.log"

WITH_VIVADO=0
if [[ "${1:-}" == "--with-vivado" ]]; then
  WITH_VIVADO=1
fi

log() {
  echo "$1" | tee -a "$SUMMARY"
}

fail() {
  log "FAIL: $1"
  exit 1
}

pass() {
  log "PASS: $1"
}

check_file_size() {
  local path="$1"
  local expected="$2"
  if [[ ! -f "$path" ]]; then
    fail "Missing required ROM file: $path"
  fi
  local size
  size="$(stat -c%s "$path")"
  if [[ "$size" != "$expected" ]]; then
    fail "Unexpected size for $path: got $size bytes, expected $expected bytes"
  fi
  pass "ROM size check: $path ($size bytes)"
}

log "Release check started: $TS"
log "Artifact directory: $ARTIFACT_DIR"

log "CHECK: Submodule status"
git -C "$ROOT_DIR" submodule status --recursive > "$ARTIFACT_DIR/submodules.txt"
while IFS= read -r line; do
  if [[ -n "$line" && "${line:0:1}" == "-" ]]; then
    fail "Uninitialized submodule detected: $line"
  fi
done < "$ARTIFACT_DIR/submodules.txt"
pass "All submodules initialized"

log "CHECK: QNICE assembler availability"
if [[ ! -x "$ROOT_DIR/M2M/QNICE/assembler/qasm" ]]; then
  fail "QNICE assembler missing. Run: cd M2M/QNICE/tools && ./make-toolchain.sh"
fi
pass "QNICE assembler is available"

log "CHECK: Build M2M ROM"
(
  cd "$ROOT_DIR/CORE/m2m-rom"
  ./make_rom.sh
) > "$ARTIFACT_DIR/m2m-rom-build.log" 2>&1 || {
  cat "$ARTIFACT_DIR/m2m-rom-build.log" >> "$SUMMARY"
  fail "M2M ROM build failed"
}
pass "M2M ROM build succeeded"

log "CHECK: C128 boot simulation gate"
(
  cd "$ROOT_DIR/CORE/scripts"
  ./run_boot_sim.sh "$ROOT_DIR/CORE/CORE-R6-vivado2022.xpr"
) > "$ARTIFACT_DIR/boot-sim.log" 2>&1 || {
  cat "$ARTIFACT_DIR/boot-sim.log" >> "$SUMMARY"
  fail "C128 boot simulation gate failed (run ./CORE/scripts/run_boot_sim.sh)"
}
pass "C128 boot simulation gate passed"

check_file_size "$ROOT_DIR/sdcard/c128/boot0.rom" "73728"
check_file_size "$ROOT_DIR/sdcard/c128/boot1.rom" "196608"

if [[ "$WITH_VIVADO" == "1" ]]; then
  log "CHECK: Vivado batch build"
  (
    cd "$ROOT_DIR"
    CORE/scripts/vivado.sh -mode batch -source CORE/scripts/build_bitstream.tcl -tclargs CORE/CORE-R6.xpr
  ) > "$ARTIFACT_DIR/vivado-build.log" 2>&1 || {
    cat "$ARTIFACT_DIR/vivado-build.log" >> "$SUMMARY"
    fail "Vivado batch build failed"
  }
  pass "Vivado batch build succeeded"
else
  log "SKIP: Vivado batch build (use --with-vivado to enable)"
fi

log "Release check finished successfully."
log "See artifacts in: $ARTIFACT_DIR"
