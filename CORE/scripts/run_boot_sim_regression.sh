#!/usr/bin/env bash
# Boot sim regression: baseline must pass; rejected H-V23 bridge model must fail.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT="${1:-$SCRIPT_DIR/../CORE-R6-vivado2022.xpr}"

echo "=== Boot sim baseline (must pass) ==="
"$SCRIPT_DIR/run_boot_sim.sh" "$PROJECT"

echo "=== Boot sim H-V23 bridge (must fail) ==="
"$SCRIPT_DIR/run_boot_sim.sh" "$PROJECT" --mem-bridge

echo "All boot sim regression checks passed."
