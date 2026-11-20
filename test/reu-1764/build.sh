#!/usr/bin/env bash
# Builds reu-tests6.d64 with 17-start for issue #9: the BASIC driver with C64MEGA65's REU check
# (reucheck.bas, from their doc/fix-issue-259/tests/round5) appended, and the machine code at
# $2000 glued behind it, as their round-5 build.sh does. Tools: VICE petcat + c1541, cc65.
set -eu
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd -P)"
cd "$HERE"
VICE="${VICE:-/Applications/vice-arm64-gtk3-3.9/bin}"
ML_ADDR=8192   # $2000

cat 17-start.bas reucheck.bas > 17-start.tmp.bas
"$VICE/petcat" -w2 -o 17-start.prg -- 17-start.tmp.bas
rm -f 17-start.tmp.bas
ca65 -t none -o 17-start.o 17-start.s
ld65 -t none -o 17-start.bin 17-start.o
rm -f 17-start.o
python3 - 17-start "$ML_ADDR" <<'PY'
import sys
name, ml_addr = sys.argv[1], int(sys.argv[2])
bas = open(name + ".prg", "rb").read()
ml  = open(name + ".bin", "rb").read()
load = bas[0] | (bas[1] << 8)
end  = load + len(bas) - 2
assert load == 0x0801, hex(load)
assert end <= ml_addr, f"{name}: BASIC text ends at {end:#x}, beyond {ml_addr:#x}"
open(name + ".prg", "wb").write(bas + bytes(ml_addr - end) + ml)
print(f"{name}.prg: BASIC {len(bas)-2} bytes, ML {len(ml)} bytes at {ml_addr:#x}")
PY
rm -f 17-start.bin
rm -f reu-tests6.d64
"$VICE/c1541" -format "reu tests 6,r6" d64 reu-tests6.d64 >/dev/null
"$VICE/c1541" -attach reu-tests6.d64 -write 17-start.prg 17-start >/dev/null
"$VICE/c1541" -attach reu-tests6.d64 -list
