#!/usr/bin/env bash
# Build C128 MEGA65 cores for R3–R6, convert to .cor files, and package a release zip.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CORE_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$CORE_DIR/.." && pwd)"

CORETOOL="${CORETOOL:-/var/home/bazzite/Desktop/m65tools-develo-207-c5bf0c-linux/coretool}"
BOARDS=(6 3 5 4)
# JOBS=2 builds two boards at a time with build_boards.sh (about 8 GB each during synthesis).
JOBS="${JOBS:-1}"

# Corefile capabilities and active boot flags (coretool --caps / --flags).
#
# MEGAFLASH looks at these on power-on to decide which core to start. Two separate sets:
#   --caps  : what this core is able to do. Only these can be switched on inside MEGAFLASH.
#   --flags : which of those capabilities are active out of the box.
#
# The MEGA65's slot is electrically a C64 expansion port and the C128 runs both cartridge
# families (a C64 cartridge pulls EXROM/GAME low and the C128 comes up in C64 mode; a C128
# cartridge is found by the boot ROM as External Function ROM), so we claim both cartridge
# types as capabilities.
#
# Only c128cart is active by default, though. A C64 cartridge belongs to the C64 core, and
# whichever core sits in the lower slot wins the auto-start, so claiming c64cart out of the box
# would let the C128 hijack cartridges the user expects to boot the C64 core. Anyone who does
# want that can switch c64cart on inside MEGAFLASH, or build with CORE_FLAGS=c64cart,c128cart.
# "default" is left inactive for the same reason: becoming the machine's default core is the
# user's decision, not the release's.
#
# Note for whoever wonders about "--flags 3": coretool takes names, not numbers. It rejects a
# numeric argument with 'unknown capability "3"'. Run `coretool --caps list` for valid values.
# Also note that every value in --flags must also appear in --caps, otherwise `coretool
# --verify` reports "Flags without Caps".
CORE_CAPS="${CORE_CAPS:-default,c64cart,c128cart}"
CORE_FLAGS="${CORE_FLAGS:-c128cart}"

die() {
  echo "ERROR: $*" >&2
  exit 1
}

sanitize_version_slug() {
  local raw="$1"
  local slug
  slug="$(printf '%s' "$raw" | sed -E 's/^[[:space:]]+//; s/[[:space:]]+$//; s/[[:space:]]+/_/g; s/[^A-Za-z0-9._-]+/_/g; s/_+/_/g; s/^_|_$//g')"
  [[ "$slug" =~ [0-9A-Za-z] ]] || die "CORE_VERSION must contain at least one letter or digit."
  printf '%s' "$slug"
}

CONFIG_VHD_PATH="$CORE_DIR/vhdl/config.vhd"
CORE_VERSION_RE='constant[[:space:]]+CORE_VERSION[[:space:]]*:[[:space:]]*string[[:space:]]*:=[[:space:]]*"([^"]+)"[[:space:]]*;'

# The release name is CORE_VERSION from config.vhd. Synthesis bakes that string into the
# bitstream (the credits screen) and the firmware build bakes it into firmware.bin (the
# network info page), so the zip, the core file and those two screens stay the same text.
read_core_version() {
  [[ -f "$CONFIG_VHD_PATH" ]] || die "config.vhd not found: $CONFIG_VHD_PATH"

  local matches
  mapfile -t matches < <(grep -oEi "$CORE_VERSION_RE" "$CONFIG_VHD_PATH" | sed -E "s/$CORE_VERSION_RE/\1/I")

  case ${#matches[@]} in
    0) die "No 'constant CORE_VERSION : string := \"...\";' found in $CONFIG_VHD_PATH." ;;
    1) ;;
    *) die "Found ${#matches[@]} CORE_VERSION assignments in $CONFIG_VHD_PATH; expected exactly one." ;;
  esac

  CORE_VERSION_VHDL="${matches[0]}"
  VERSION_SLUG="$(sanitize_version_slug "$CORE_VERSION_VHDL")"
  [[ "$CORE_VERSION_VHDL" == "$VERSION_SLUG" ]] || die "CORE_VERSION \"$CORE_VERSION_VHDL\" is not a usable release name.
     Use only letters, digits, dots, underscores and hyphens, with no spaces."
}

# firmware.bin stores CORE_VERSION as its own C string in the payload after the 512-byte
# header (include/fw_image.h): a zero byte, then the text, then a zero byte. The byte before
# the text has to be zero too, so a shorter name cannot match the tail of a longer one.
firmware_has_core_version() {
  local image="$1"
  python3 -I - "$image" "$CORE_VERSION_VHDL" <<'PY'
import sys
path, version = sys.argv[1:]
try:
    data = open(path, "rb").read()
except OSError:
    sys.exit(1)
needle = version.encode("ascii") + b"\0"
start = 512
while True:
    i = data.find(needle, start)
    if i < 0:
        sys.exit(1)
    if data[i - 1] == 0:
        sys.exit(0)
    start = i + 1
PY
}

# Same paths as check_firmware_fresh.sh. A dirty tree elsewhere (Vivado project files,
# for example) does not change this image, so it must not mark the firmware dirty.
firmware_source_paths=(
  M2M/firmware/fw M2M/firmware/lib M2M/firmware/include
  M2M/firmware/fatfs M2M/firmware/neorv32 M2M/firmware/Makefile M2M/firmware/tools
  CORE/firmware M2M/video_filters CORE/vhdl/config.vhd
)

# Commit recorded in the image. check_image.py refuses a version that ends in -dirty,
# which here means uncommitted edits under the firmware's own sources.
firmware_version_string() {
  local base
  base="$(git -C "$ROOT_DIR" describe --always --match=NONE 2>/dev/null || echo unknown)"
  if [[ -n "$(git -C "$ROOT_DIR" status --porcelain -- "${firmware_source_paths[@]}")" ]]; then
    printf '%s-dirty' "$base"
  else
    printf '%s' "$base"
  fi
}

# xPack GNU RISC-V Embedded GCC 14.3.0-1. A shell that has not loaded ~/.bashrc.d
# still finds the copy installed under ~/.local/xpack. RISCV_TOOLCHAIN_BIN overrides it.
use_riscv_toolchain() {
  if command -v riscv-none-elf-gcc >/dev/null 2>&1; then
    return 0
  fi
  local bin="${RISCV_TOOLCHAIN_BIN:-$HOME/.local/xpack/xpack-riscv-none-elf-gcc-14.3.0-1/bin}"
  [[ -x "$bin/riscv-none-elf-gcc" ]] || die "riscv-none-elf-gcc not found.
     Install xPack GNU RISC-V Embedded GCC 14.3.0-1, or set RISCV_TOOLCHAIN_BIN to its bin directory."
  export PATH="$bin:$PATH"
}

# Check the rebuilt image before replacing the committed one, so a firmware that is not
# fit to ship (for example from uncommitted firmware sources) leaves sdcard/c128/firmware.bin alone.
rebuild_firmware() {
  local why="$1"
  local built="$ROOT_DIR/M2M/firmware/build/firmware.bin"
  echo "$why"
  use_riscv_toolchain
  make -C "$ROOT_DIR/M2M/firmware" build/firmware.bin \
    FW_VERSION="$(firmware_version_string)" || die "Firmware rebuild failed."
  firmware_has_core_version "$built" \
    || die "Rebuilt firmware still has no CORE_VERSION \"$CORE_VERSION_VHDL\"."
  python3 -I "$ROOT_DIR/M2M/firmware/tools/check_image.py" "$built" \
    "$ROOT_DIR/M2M/firmware/include/m2m_rv.h" >/dev/null \
    || die "Rebuilt firmware.bin is not fit to ship."
  cp -f "$built" "$FIRMWARE"
  echo "Updated $FIRMWARE"
}

firmware_version() {
  python3 -I "$ROOT_DIR/M2M/firmware/tools/check_image.py" "$FIRMWARE" \
    "$ROOT_DIR/M2M/firmware/include/m2m_rv.h"
}

read_core_version
PREFIX="C128Core_${VERSION_SLUG}"
TS="$(date +%Y%m%d-%H%M%S)"
STAGING_DIR="$ROOT_DIR/artifacts/release-${VERSION_SLUG}-${TS}"
ZIP_PATH="$ROOT_DIR/artifacts/${PREFIX}.zip"

echo "Release: $CORE_VERSION_VHDL"
echo "Artifact prefix: ${PREFIX}_R<n>"
echo

[[ -x "$CORETOOL" ]] || die "coretool not found or not executable: $CORETOOL"

# Rebuild the firmware if it does not contain the CORE_VERSION string or if it is older than its sources.
FIRMWARE="$ROOT_DIR/sdcard/c128/firmware.bin"
rebuild_reason=""
if ! firmware_has_core_version "$FIRMWARE"; then
  rebuild_reason="Firmware does not contain CORE_VERSION \"$CORE_VERSION_VHDL\"; rebuilding."
else
  FW_VERSION="$(firmware_version)" || die "firmware.bin is not fit to ship"
  if ! "$SCRIPT_DIR/check_firmware_fresh.sh"; then
    rebuild_reason="Firmware is older than its sources; rebuilding."
  fi
fi
if [[ -n "$rebuild_reason" ]]; then
  rebuild_firmware "$rebuild_reason"
  FW_VERSION="$(firmware_version)" || die "firmware.bin is not fit to ship"
  "$SCRIPT_DIR/check_firmware_fresh.sh" || die "firmware.bin is older than its sources"
fi
echo "Firmware: $FW_VERSION (CORE_VERSION $CORE_VERSION_VHDL)"

ask_run_testbenches() {
  local answer="${RUN_TESTBENCHES:-}"
  if [[ -z "$answer" ]]; then
    read -r -p "Run all simulation testbenches before building cores? [Y/n] " answer
  fi
  case "${answer:-Y}" in
    [Yy]|[Yy][Ee][Ss]|"") return 0 ;;
    *) return 1 ;;
  esac
}

if ask_run_testbenches; then
  echo "Running all simulation testbenches..."
  "$SCRIPT_DIR/run_all_sims.sh" "CORE/CORE-R6-vivado2022.xpr" \
    || die "A simulation testbench failed; core build aborted."
else
  echo "Skipping simulation testbenches."
fi

mkdir -p "$STAGING_DIR"
echo "Staging directory: $STAGING_DIR"

cp -f "$FIRMWARE" "$STAGING_DIR/"
echo "Firmware: $STAGING_DIR/firmware.bin"

find_bitstream() {
  local rev="$1"
  local project_base="CORE-R${rev}-vivado2022"
  local impl_dir="$CORE_DIR/${project_base}.runs/impl_1"
  local expected="$impl_dir/mega65_r${rev}.bit"

  if [[ -f "$expected" ]]; then
    printf '%s\n' "$expected"
    return 0
  fi

  local matches=()
  mapfile -t matches < <(find "$impl_dir" -maxdepth 1 -name '*.bit' -type f 2>/dev/null || true)
  if [[ ${#matches[@]} -eq 1 ]]; then
    printf '%s\n' "${matches[0]}"
    return 0
  fi

  die "Bitstream not found for R${rev} (expected $expected)"
}

if (( JOBS > 1 )); then
  echo "=== Building R${BOARDS[*]} bitstreams, $JOBS at a time ==="
  JOBS="$JOBS" "$SCRIPT_DIR/build_boards.sh" "${BOARDS[@]}" || die "A board failed to build."
fi

for rev in "${BOARDS[@]}"; do
  project_xpr="$CORE_DIR/CORE-R${rev}-vivado2022.xpr"
  [[ -f "$project_xpr" ]] || die "Vivado project missing: $project_xpr"

  if (( JOBS <= 1 )); then
    echo "=== Building R${rev} bitstream ==="
    (
      cd "$ROOT_DIR"
      CORE/scripts/vivado.sh -mode batch \
        -source CORE/scripts/build_bitstream.tcl \
        -tclargs "$project_xpr"
    )
  fi

  bit_src="$(find_bitstream "$rev")"
  bit_out="$STAGING_DIR/${PREFIX}_R${rev}.bit"
  core_out="$STAGING_DIR/${PREFIX}_R${rev}.cor"

  cp -f "$bit_src" "$bit_out"
  echo "Bitstream: $bit_out"

  echo "=== Converting R${rev} to core file ==="
  "$CORETOOL" -B "$core_out" \
    -b "$bit_out" \
    -n "C128" \
    -v "$CORE_VERSION_VHDL" \
    -t "mega65r${rev}" \
    --caps "$CORE_CAPS" \
    --flags "$CORE_FLAGS"
  echo "Core file: $core_out"

  # Fail the build if the corefile does not come out the way we asked for it, so that a
  # mistyped capability cannot silently ship a core MEGAFLASH will not auto-start. coretool
  # still exits 0 when it only marks a problem (e.g. "Flags without Caps"), so inspect its
  # report rather than trusting the exit code alone.
  verify_out="$("$CORETOOL" -V "$core_out" 2>&1)" || die "coretool could not read $core_out"
  if grep -q '✗' <<<"$verify_out"; then
    printf '%s\n' "$verify_out" >&2
    die "coretool reported problems in $core_out"
  fi
  echo
done

echo "=== Creating release zip ==="
(
  cd "$STAGING_DIR"
  zip -q -j "$ZIP_PATH" ./*
)

echo
echo "Release build complete."
echo "Zip: $ZIP_PATH"
echo "Artifacts: $STAGING_DIR"
