#!/usr/bin/env bash
# Run tb_cmdhd (the CMD HD boots its DOS from a disk image, CORE/vhdl/cmdhd) with nvc.
# Needs the CMD HD boot ROM and an image with the system on it, which can't be in the repo:
#   CMDHD_ROM=<boot ROM, 16 or 32 KB> CMDHD_IMG=<.dhd> CORE/scripts/run_cmdhd_nvc.sh [work dir]
# About 7 minutes: the boot ROM's own checks take 4.4 s of drive time.
#
# T65's registers are given 0 at power-up here, as the FPGA's flip-flops have: the boot ROM
# runs a delay loop on Y before it sets it, which never ends on 'U'.

set -euo pipefail

repo=$(cd "$(dirname "$0")/../.." && pwd)
work=${1:-"$repo/CORE/cmdhd_nvc"}
: "${CMDHD_ROM:?the CMD HD boot ROM}"
: "${CMDHD_IMG:?a CMD HD image with a system}"
mkdir -p "$work"
cd "$work"

t65="$repo/CORE/C128_MiSTer/rtl/t65"
sed -e "s/signal ABC, X, Y          : std_logic_vector(15 downto 0);/signal ABC, X, Y          : std_logic_vector(15 downto 0) := (others => '0');/" \
    "$t65/T65.vhd" > T65_sim.vhd

nvc --std=2008 -a "$t65/T65_Pack.vhd" "$t65/T65_ALU.vhd" "$t65/T65_MCode.vhd" T65_sim.vhd
nvc --std=2008 -a --relaxed "$repo/CORE/C128_MiSTer/rtl/iec_drive/iecdrv_via6522.vhd"
nvc --std=2008 -a \
    "$repo/M2M/vhdl/sd/sdhost.vhd" \
    "$repo/M2M/vhdl/tdp_ram.vhd" \
    "$repo/CORE/vhdl/cmdhd/cmdhd_i8255.vhd" \
    "$repo/CORE/vhdl/cmdhd/cmdhd_rtc.vhd" \
    "$repo/CORE/vhdl/cmdhd/cmdhd_scsi.vhd" \
    "$repo/CORE/vhdl/cmdhd/cmdhd_drive.vhd" \
    "$repo/CORE/vhdl/cmdhd/cmdhd.vhd"
nvc --std=2008 -a --relaxed "$repo/CORE/sim/tb_cmdhd.vhd"
nvc --std=2008 --ieee-warnings=off -e tb_cmdhd -gG_ROM="$CMDHD_ROM" -gG_IMG="$CMDHD_IMG" \
    -r --exit-severity=failure 2>&1 | tee tb_cmdhd.log

grep -q "ALL CHECKS PASSED" tb_cmdhd.log && echo "CMD HD SIM: PASSED" || { echo "CMD HD SIM: FAILED"; exit 1; }
