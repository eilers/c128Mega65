#!/usr/bin/env bash
# Run tb_rv_soc (the RISC-V system CPU, M2M/vhdl/rv) with nvc, the open-source VHDL
# simulator (brew install nvc). Takes a few seconds; no Vivado needed.
#
# Usage: CORE/scripts/run_rv_soc_nvc.sh [work dir]

set -euo pipefail

repo=$(cd "$(dirname "$0")/../.." && pwd)
work=${1:-"$repo/CORE/rv_soc_nvc"}
mkdir -p "$work"
cd "$work"

neo=("$repo/CORE/vhdl/modem/neorv32_bootrom_image.vhd")
while read -r f; do neo+=("$repo/CORE/vhdl/neorv32/$f"); done < "$repo/CORE/vhdl/neorv32/file_list.txt"

nvc --std=2008 --work=neorv32 -a "${neo[@]}"
nvc --std=2008 -L . -a \
    "$repo/M2M/vhdl/QNICE/sdhost.vhd" \
    "$repo/M2M/vhdl/QNICE/sdmux.vhd" \
    "$repo/M2M/vhdl/rv/rv_hr_bridge.vhd" \
    "$repo/M2M/vhdl/rv/rv_soc.vhd" \
    "$repo/CORE/sim/tb_rv_soc.vhd"
nvc --std=2008 -L . --ieee-warnings=off -e tb_rv_soc -gG_BOOT_ROM="$repo/M2M/firmware/test/selftest.rom" \
    -r --exit-severity=error 2>&1 | tee tb_rv_soc.log

grep -q "ALL CHECKS PASSED" tb_rv_soc.log && echo "RV SOC SIM: PASSED" || { echo "RV SOC SIM: FAILED"; exit 1; }
