#!/usr/bin/env bash
# Makes FAT images with macOS's own formatter (hdiutil, no root needed), with an MBR as
# on a MEGA65 card, puts the test files on them, and runs test_boot and test_fw.
# usage: run.sh build-dir   (holds test_boot and test_fw; ../build/firmware.bin must exist)
set -euo pipefail

here=$(cd "$(dirname "$0")" && pwd)
out=$(cd "$1" && pwd)
files=$out/files
mnt=$out/mnt
rm -rf "$files" && mkdir -p "$files" "$mnt"
trap 'hdiutil detach -quiet "$mnt" 2>/dev/null || true' EXIT

python3 -I "$here/make_cases.py" "$here/../build/firmware.bin" "$files"

make_img() {   # image name, file system
   local img=$files/$1
   hdiutil create -quiet -size 64m -fs "$2" -volname TESTFAT -layout MBRSPUD "$img.dmg"
   hdiutil attach -quiet -nobrowse -mountpoint "$mnt" "$img.dmg"
   mkdir -p "$mnt/c128" "$mnt/bad" "$mnt/big" "$mnt/Long folder name with spaces"
   cp -X "$files/firmware.bin" "$mnt/c128/"
   cp -X "$files/fw1m.bin" "$mnt/big/"
   for f in crc hwver short hdrshort magic big entry align; do
      cp -X "$files/$f.bin" "$mnt/bad/"
   done
   head -c 174848 /dev/zero > "$mnt/Long folder name with spaces/A file with a rather long name, to see.d64"
   hdiutil detach -quiet "$mnt"
   mv "$img.dmg" "$img"
   [ "$(xxd -s 510 -l 2 -p "$img")" = 55aa ] || { echo "$img: no MBR"; exit 1; }
}

make_img fat32.img "MS-DOS FAT32"
make_img fat16.img "MS-DOS FAT16"
head -c 1048576 /dev/zero > "$files/blank.img"

"$out/test_boot" "$files"
"$out/test_fw" "$files/fat32.img"
echo "HOST TESTS: PASSED"
