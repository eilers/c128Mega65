#!/usr/bin/env bash
# Fails if sdcard/c128/firmware.bin was built from an older commit than the last change to
# the firmware's sources. Rebuild it with: make -C M2M/firmware, then copy
# M2M/firmware/build/firmware.bin to sdcard/c128/ and commit it.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

built="$(python3 -I M2M/firmware/tools/check_image.py sdcard/c128/firmware.bin M2M/firmware/include/m2m_rv.h)"
changed="$(git log -1 --format=%h -- M2M/firmware/fw M2M/firmware/lib M2M/firmware/include \
  M2M/firmware/fatfs M2M/firmware/neorv32 M2M/firmware/Makefile M2M/firmware/tools \
  CORE/firmware M2M/video_filters)"

if ! git merge-base --is-ancestor "$changed" "$built" 2>/dev/null; then
  echo "sdcard/c128/firmware.bin was built from $built, but its sources changed in $changed" >&2
  exit 1
fi
echo "sdcard/c128/firmware.bin built from $built, after the last source change ($changed)"
