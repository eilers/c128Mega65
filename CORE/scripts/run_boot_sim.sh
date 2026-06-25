#!/usr/bin/env bash
# Run C128 boot simulation via Vivado xsim.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CORE_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_DIR="$(cd "$CORE_DIR/.." && pwd)"
PROJECT="${1:-$CORE_DIR/CORE-R6-vivado2022.xpr}"
shift || true

exec "$SCRIPT_DIR/vivado.sh" -mode batch \
  -source "$SCRIPT_DIR/run_boot_sim.tcl" \
  -tclargs "$PROJECT" "$@"
