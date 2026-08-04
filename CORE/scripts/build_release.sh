#!/usr/bin/env bash
# Build C128 MEGA65 cores for R3–R6, convert to .cor files, and package a release zip.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CORE_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
ROOT_DIR="$(cd "$CORE_DIR/.." && pwd)"

CORETOOL="${CORETOOL:-/var/home/bazzite/Desktop/m65tools-develo-207-c5bf0c-linux/coretool}"
BOARDS=(6 5 4 3)

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
  [[ -n "$slug" ]] || die "Release name must contain at least one letter or digit."
  printf '%s' "$slug"
}

read -r -p "Release name (e.g. Alpha 1): " RELEASE_NAME
[[ -n "${RELEASE_NAME//[[:space:]]/}" ]] || die "Release name cannot be empty."

VERSION_SLUG="$(sanitize_version_slug "$RELEASE_NAME")"
PREFIX="C128Core_${VERSION_SLUG}"
TS="$(date +%Y%m%d-%H%M%S)"
STAGING_DIR="$ROOT_DIR/artifacts/release-${VERSION_SLUG}-${TS}"
ZIP_PATH="$ROOT_DIR/artifacts/${PREFIX}.zip"

mkdir -p "$STAGING_DIR"

echo "Release: $RELEASE_NAME"
echo "Artifact prefix: ${PREFIX}_R<n>"
echo "Staging directory: $STAGING_DIR"
echo

[[ -x "$CORETOOL" ]] || die "coretool not found or not executable: $CORETOOL"

if [[ ! -x "$ROOT_DIR/M2M/QNICE/assembler/qasm" ]]; then
  die "QNICE assembler missing. Run: cd M2M/QNICE/tools && ./make-toolchain.sh"
fi

echo "Building M2M shell ROM..."
(
  cd "$CORE_DIR/m2m-rom"
  ./make_rom.sh
) >/dev/null

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

for rev in "${BOARDS[@]}"; do
  project_xpr="$CORE_DIR/CORE-R${rev}-vivado2022.xpr"
  [[ -f "$project_xpr" ]] || die "Vivado project missing: $project_xpr"

  echo "=== Building R${rev} bitstream ==="
  (
    cd "$ROOT_DIR"
    CORE/scripts/vivado.sh -mode batch \
      -source CORE/scripts/build_bitstream.tcl \
      -tclargs "$project_xpr"
  )

  bit_src="$(find_bitstream "$rev")"
  bit_out="$STAGING_DIR/${PREFIX}_R${rev}.bit"
  core_out="$STAGING_DIR/${PREFIX}_R${rev}.cor"

  cp -f "$bit_src" "$bit_out"
  echo "Bitstream: $bit_out"

  echo "=== Converting R${rev} to core file ==="
  "$CORETOOL" -B "$core_out" \
    -b "$bit_out" \
    -n "C128" \
    -v "$RELEASE_NAME" \
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
