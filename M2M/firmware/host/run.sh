#!/usr/bin/env bash
# Makes FAT images with macOS's own formatter (hdiutil, no root needed), with an MBR as
# on a MEGA65 card, puts the test files on them, and runs test_boot, test_fw, test_mon,
# test_uci and test/uci's programs (test_prg); then test_shell against config.vhd as
# CORE/scripts/run_config_nvc.sh dumps it (nvc).
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
"$out/test_mon"
cp "$files/fat32.img" "$files/uci.img"
"$out/test_uci" "$files/uci.img"
for prg in ucitest ucitest64 ucidos ucidos64; do   # test/uci's programs, on a 6502
   cp "$files/fat32.img" "$files/prg.img"
   "$out/test_prg" "$files/prg.img" "$here/../../../test/uci/$prg.prg" > "$out/$prg.log" ||
      { cat "$out/$prg.log"; exit 1; }
   t=$(tail -n 1 "$out/$prg.log")
   echo "test_prg $prg: ALL PASSED, ${t#test_prg: }"
done
for prg in ucidos ucidos64; do   # a run cut short after the D64, as by a reset, then a whole one
   cp "$files/fat32.img" "$files/prg.img"
   "$out/test_prg" "$files/prg.img" "$here/../../../test/uci/$prg.prg" "WRITE UCITEST.D64 PASS" \
      > "$out/$prg-cut.log" && "$out/test_prg" "$files/prg.img" \
      "$here/../../../test/uci/$prg.prg" >> "$out/$prg-cut.log" || { cat "$out/$prg-cut.log"; exit 1; }
   echo "test_prg $prg after a run cut short: ALL PASSED"
done
"$here/../../../CORE/scripts/run_config_nvc.sh" "$out/config_nvc" > "$out/config_nvc.log" 2>&1 ||
   { cat "$out/config_nvc.log"; exit 1; }
for dump in config_dump.txt config_dump_r3.txt; do
   cp "$files/fat32.img" "$files/shell.img"
   "$out/test_shell" "$out/config_nvc/$dump" "$files/shell.img"
   "$out/test_shell" "$out/config_nvc/$dump" "$files/shell.img"   # with the saved settings
done
echo "HOST TESTS: PASSED"
