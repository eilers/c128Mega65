# Run the 1581 "drive ready" simulation (tb_c1581_ready).
#
# A mounted .D81 identifies as a 1581 on hardware but DIRECTORY returns nothing and
# DS$ then reports "74, DRIVE NOT READY". The DOS polls that state through
# floppy_ready = fd_ready && fd_present, so this gate drives the FDC and the floppy
# model with the exact clock-enable chain the core generates (the 16 MHz DDA from
# main.vhd feeding the divider from c1581_multi.sv), mounts a D81-sized image, turns
# the motor on and checks that the drive actually reaches "ready".
#
# Usage:
#   vivado -mode batch -source CORE/scripts/run_c1581_ready_sim.tcl -tclargs CORE/CORE-R6-vivado2022.xpr

set project_file [lindex $argv 0]
if {$project_file eq ""} {
    set project_file [file normalize [file join [file dirname [info script]] .. CORE-R6-vivado2022.xpr]]
}

set core_dir [file normalize [file join [file dirname [info script]] ..]]
set repo_dir [file normalize [file join $core_dir ..]]
set tb_file  [file join $core_dir sim tb_c1581_ready.sv]

source [file join [file dirname [info script]] sim_launch.tcl]

open_project $project_file
update_compile_order -fileset sources_1

foreach required_file [list \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/c1581_fdc1772.v"] \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/floppy.v"] \
] {
    if {[llength [get_files -quiet $required_file]] == 0} {
        puts "Adding missing source file: $required_file"
        add_files -norecurse -fileset [get_filesets sources_1] $required_file
    }
    # Both are Verilog-2001 by extension but use SystemVerilog constructs.
    set_property file_type {SystemVerilog} [get_files $required_file]
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
set_property file_type {SystemVerilog} [get_files $tb_file]


# This gate only reads $display output and the bit-level floppy model has to run for
# tens of milliseconds of simulated time, so there is no reason to record waveforms.
set_property -name {xsim.elaborate.debug_level} -value {off} -objects [get_filesets sim_1]

update_compile_order -fileset sim_1

puts "Launching 1581 ready simulation (top=tb_c1581_ready)..."
sim_launch tb_c1581_ready

set sim_dir [file join \
    [file dirname [file normalize $project_file]] \
    "[file rootname [file tail $project_file]].sim" sim_1 behav xsim]

# The testbench calls $finish itself; the ceiling only bounds a runaway run.
run 1s

# xsim flushes simulate.log when the simulation is closed, so read it afterwards.
close_sim -force

set gate_ok 0
set sim_log [file join $sim_dir simulate.log]
if {[file exists $sim_log]} {
    set fh [open $sim_log r]
    while {[gets $fh line] >= 0} {
        if {[string match *tb_c1581_ready* $line] || [string match *MISMATCH* $line] \
            || [string match *Failure* $line]} {
            puts $line
        }
        if {[string match *simulation\ finished* $line]} {
            if {[string match *\"pass\":true* $line]} {
                set gate_ok 1
            } else {
                puts "ERROR: 1581 ready gate failed (pass=false)"
            }
        }
    }
    close $fh
} else {
    puts "ERROR: Simulator log not found: $sim_log"
}
if {!$gate_ok} {
    puts "ERROR: 1581 ready summary line missing or failed"
}

close_project
if {!$gate_ok} {
    exit 2
}
exit 0
