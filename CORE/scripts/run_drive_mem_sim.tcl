# Run the drive RAM simulation (tb_iecdrv_mem).
#
# Checks that iecdrv_mem and iecdrv_trackmem store what the drive CPU and the read/write
# head actually wrote. Both were altsyncram upstream; a Vivado rewrite that registers the
# address ahead of the data passes synthesis and lets the drive boot, but corrupts every
# byte the DOS puts into RAM. On hardware that shows up only as an empty DS$, no directory
# output and a stalled IEC bus.
#
# Usage:
#   vivado -mode batch -source CORE/scripts/run_drive_mem_sim.tcl -tclargs CORE/CORE-R6-vivado2022.xpr

set project_file [lindex $argv 0]
if {$project_file eq ""} {
    set project_file [file normalize [file join [file dirname [info script]] .. CORE-R6-vivado2022.xpr]]
}

set core_dir [file normalize [file join [file dirname [info script]] ..]]
set repo_dir [file normalize [file join $core_dir ..]]
set tb_file  [file join $core_dir sim tb_iecdrv_mem.vhd]

open_project $project_file
update_compile_order -fileset sources_1

foreach required_file [list \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/iecdrv_misc.sv"] \
] {
    if {[llength [get_files -quiet $required_file]] == 0} {
        puts "Adding missing source file: $required_file"
        add_files -norecurse -fileset [get_filesets sources_1] $required_file
    }
}

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

set_property top tb_iecdrv_mem [get_filesets sim_1]
set_property top_lib xil_defaultlib [get_filesets sim_1]
update_compile_order -fileset sim_1

puts "Launching drive RAM simulation (top=tb_iecdrv_mem)..."
if {[llength [get_runs -quiet sim_1]] > 0} {
    catch {reset_simulation -simset sim_1 -force}
}
launch_simulation -simset sim_1

# xsim writes everything the testbench prints to simulate.log in the run directory.
# "run" itself returns nothing, so the gate has to read that file rather than its result.
set sim_dir [file join \
    [file dirname [file normalize $project_file]] \
    "[file rootname [file tail $project_file]].sim" sim_1 behav xsim]

# The testbench stops its own clock when it is done, so a generous ceiling is safe.
run 200ms

# xsim flushes simulate.log when the simulation is closed, so reading it any earlier
# only sees a partial file.
close_sim -force

set gate_ok 0
set sim_log [file join $sim_dir simulate.log]
if {[file exists $sim_log]} {
    set fh [open $sim_log r]
    while {[gets $fh line] >= 0} {
        if {[string match *MISMATCH* $line] || [string match *simulation\ finished* $line] \
            || [string match *Failure* $line]} {
            puts $line
        }
        if {[string match *simulation\ finished* $line]} {
            if {[string match *\"pass\":true* $line]} {
                set gate_ok 1
            } else {
                puts "ERROR: Drive RAM gate failed (pass=false)"
            }
        }
    }
    close $fh
} else {
    puts "ERROR: Simulator log not found: $sim_log"
}
if {!$gate_ok} {
    puts "ERROR: Drive RAM summary line missing or failed"
}

close_project
if {!$gate_ok} {
    exit 2
}
exit 0
