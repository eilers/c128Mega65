#!/usr/bin/env bash
# Run tb_drive_rom (the firmware's feed of the drives' DOS ROMs, drive_rom_server.vhd) with nvc
# (brew install nvc), against iecdrv_rom_model.vhd. Takes seconds; no Vivado needed.
# run_drive_rom_sim.tcl runs the same testbench in xsim against the real iecdrv_rom.sv.
#
# Usage: CORE/scripts/run_drive_rom_nvc.sh [work dir]

set -euo pipefail

repo=$(cd "$(dirname "$0")/../.." && pwd)
work=${1:-"$repo/CORE/drive_rom_nvc"}
mkdir -p "$work"
cd "$work"

nvc --std=2008 -a \
    "$repo/CORE/sim/iecdrv_rom_model.vhd" \
    "$repo/CORE/vhdl/drive_rom_server.vhd" \
    "$repo/CORE/sim/tb_drive_rom.vhd"
nvc --std=2008 --ieee-warnings=off -e tb_drive_rom -r --exit-severity=error 2>&1 | tee tb_drive_rom.log

grep -q "ALL CHECKS PASSED" tb_drive_rom.log && echo "DRIVE ROM SIM: PASSED" || { echo "DRIVE ROM SIM: FAILED"; exit 1; }
