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
  awk '
    /^      <File Path="\$PPRDIR\/\.\.\/\.\.\/\.\.\/\.\.\/\.\.\/\.\.\// { skip = 1; next }
    skip && /^      <\/File>/ { skip = 0; next }
    skip { next }
    { print }
  ' "$TEMPLATE"
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

add_r3_sources() {
  local out_path="$1"
  local tmp
  tmp="$(mktemp)"
  awk '
    /<File Path="\$PPRDIR\/\.\.\/M2M\/vhdl\/top_mega65-r3\.vhd">/ && !done {
      print "      <File Path=\"$PPRDIR/../M2M/vhdl/controllers/M65/max10.vhdl\">"
      print "        <FileInfo SFType=\"VHDL2008\">"
      print "          <Attr Name=\"UsedIn\" Val=\"synthesis\"/>"
      print "          <Attr Name=\"UsedIn\" Val=\"simulation\"/>"
      print "        </FileInfo>"
      print "      </File>"
      print "      <File Path=\"$PPRDIR/../M2M/vhdl/controllers/M65/pcm_to_pdm.vhdl\">"
      print "        <FileInfo SFType=\"VHDL2008\">"
      print "          <Attr Name=\"UsedIn\" Val=\"synthesis\"/>"
      print "          <Attr Name=\"UsedIn\" Val=\"simulation\"/>"
      print "        </FileInfo>"
      print "      </File>"
      done = 1
    }
    { print }
  ' "$out_path" > "$tmp"
  mv "$tmp" "$out_path"
}

add_r3_sources "$CORE_DIR/CORE-R3-vivado2022.xpr"
add_r3_sources "$CORE_DIR/CORE-R3.xpr"
echo "Added R3-specific sources to CORE-R3 project files"
