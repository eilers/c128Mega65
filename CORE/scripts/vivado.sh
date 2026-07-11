#!/usr/bin/env bash
# Run Vivado inside the Flatpak container (com.github.corna.Vivado).
# Examples:
#   ./vivado.sh -version
#   ./vivado.sh -mode batch -source scripts/vivado_query.tcl -tclargs CORE/CORE-R6.xpr
#   ./vivado.sh -mode batch -source scripts/build.tcl

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CORE_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_DIR="$(cd "$CORE_DIR/.." && pwd)"

exec flatpak run --filesystem="$REPO_DIR" com.github.corna.Vivado \
    -nojournal -nolog "$@"
