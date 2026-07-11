#!/usr/bin/env bash
# Boot sim regression: the C128 boot must reach the Z80 -> 8502 handoff (pass=true).
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT="${1:-$SCRIPT_DIR/../CORE-R6-vivado2022.xpr}"

echo "=== Boot sim (must pass) ==="
"$SCRIPT_DIR/run_boot_sim.sh" "$PROJECT"

echo "Boot sim regression passed."
