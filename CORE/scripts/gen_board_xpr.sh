#!/usr/bin/env bash
# Regenerate CORE-R3/R4/R5 Vivado project files from the maintained R6 template.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CORE_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
TEMPLATE="$CORE_DIR/CORE-R6-vivado2022.xpr"

if [[ ! -f "$TEMPLATE" ]]; then
  echo "ERROR: template not found: $TEMPLATE" >&2
  exit 1
fi

# Drop machine-specific duplicate sim paths Vivado may have added to the R6 project.
scrub_template() {
  grep -v '/var/home/' "$TEMPLATE" | grep -v '/home/bazzite/' | grep -v '/home/mike/'
}

board_variant() {
  local rev="$1"
  local out_name="$2"
  local project_base="${out_name%.xpr}"
  local out_path="$CORE_DIR/$out_name"

  scrub_template \
    | sed \
      -e "s/CORE-R6-vivado2022/${project_base}/g" \
      -e "s/CORE-R6/${project_base}/g" \
      -e "s/mega65_r6/mega65_r${rev}/g" \
      -e "s/top_mega65-r6/top_mega65-r${rev}/g" \
      -e "s/MEGA65-R6/MEGA65-R${rev}/g" \
    > "$out_path"

  echo "Wrote $out_path"
}

for rev in 3 4 5; do
  board_variant "$rev" "CORE-R${rev}-vivado2022.xpr"
  board_variant "$rev" "CORE-R${rev}.xpr"
done
