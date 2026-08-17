# Run the disk-image mount path simulation (tb_vdrive_mount).
#
# Checks that a mounted .D81 actually turns the drive into a 1581. vdrives hands the
# 2-bit image type to the core clock domain, main.vhd expands it into iec_drive's
# {img_hd, img_mfm, img_gcr, img_ds} and iec_drive latches it to pick the DOS ROM
# bank. If that latch misses, the drive silently stays a 1571 and a .D81 answers
# "73, CBM DOS V3.0 1571,00,00" instead of identifying as a 1581.
#
# Usage:
#   vivado -mode batch -source CORE/scripts/run_vdrive_mount_sim.tcl -tclargs CORE/CORE-R6-vivado2022.xpr

set project_file [lindex $argv 0]
if {$project_file eq ""} {
    set project_file [file normalize [file join [file dirname [info script]] .. CORE-R6-vivado2022.xpr]]
}

set core_dir [file normalize [file join [file dirname [info script]] ..]]
set repo_dir [file normalize [file join $core_dir ..]]
set tb_file  [file join $core_dir sim tb_vdrive_mount.vhd]

open_project $project_file
update_compile_order -fileset sources_1

set sv_files [get_files -all -quiet -filter {NAME =~ "*.sv"}]
foreach sv_file $sv_files {
    set_property file_type {SystemVerilog} $sv_file
}

if {[llength [get_filesets -quiet sim_1]] == 0} {
    create_fileset -simset sim_1
}
if {[llength [get_files -quiet $tb_file]] == 0} {
    add_files -fileset sim_1 -norecurse $tb_file
}
set_property file_type {VHDL 2008} [get_files $tb_file]

# Nothing here is inspected in a waveform viewer, and iec_drive is large enough that
# logging every signal dominates the run time.
set_property -name {xsim.elaborate.debug_level} -value {off} -objects [get_filesets sim_1]

set_property top tb_vdrive_mount [get_filesets sim_1]
set_property top_lib xil_defaultlib [get_filesets sim_1]
update_compile_order -fileset sim_1

puts "Launching disk-image mount simulation (top=tb_vdrive_mount)..."
if {[llength [get_runs -quiet sim_1]] > 0} {
    catch {reset_simulation -simset sim_1 -force}
}
launch_simulation -simset sim_1

set sim_dir [file join \
    [file dirname [file normalize $project_file]] \
    "[file rootname [file tail $project_file]].sim" sim_1 behav xsim]

# The whole stimulus takes about 21 us of simulated time. iec_drive is a big design
# (two 6502 cores plus their peripherals) and every clock edge costs wall time, so the
# ceiling is kept just above what the testbench actually needs.
run 200us

# xsim flushes simulate.log when the simulation is closed, so reading it any earlier
# only sees a partial file.
close_sim -force

set gate_ok 0
set sim_log [file join $sim_dir simulate.log]
if {[file exists $sim_log]} {
    set fh [open $sim_log r]
    while {[gets $fh line] >= 0} {
        if {[string match *PASS* $line] || [string match *FAIL* $line] \
            || [string match *---* $line] || [string match *Failure* $line]} {
            puts $line
        }
        if {[string match *ALL\ CHECKS\ PASSED* $line]} {
            set gate_ok 1
        }
    }
    close $fh
} else {
    puts "ERROR: Simulator log not found: $sim_log"
}
if {!$gate_ok} {
    puts "ERROR: Mount path gate failed"
}

close_project
if {!$gate_ok} {
    exit 2
}
exit 0
