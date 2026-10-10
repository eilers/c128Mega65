# Run the hardware HyperRAM sector-buffer fill simulation (tb_vd_hr_pump).
#
# Checks that vd_hr_pump copies image bytes from a mock Avalon slave into the
# drive buffer with the MiSTer sd_rd / sd_ack / sd_buff_wr handshake, for both
# a 256-byte 1571 block and a 512-byte 1581-style request.
#
# Usage:
#   vivado -mode batch -source CORE/scripts/run_vd_hr_pump_sim.tcl \
#     -tclargs CORE/CORE-R6-vivado2022.xpr

set project_file [lindex $argv 0]
if {$project_file eq ""} {
    set project_file [file normalize [file join [file dirname [info script]] .. CORE-R6-vivado2022.xpr]]
}

set core_dir [file normalize [file join [file dirname [info script]] ..]]
set repo_dir [file normalize [file join $core_dir ..]]
set tb_file  [file join $core_dir sim tb_vd_hr_pump.vhd]
set dut_file [file join $core_dir vhdl vd_hr_pump.vhd]

source [file join [file dirname [info script]] sim_launch.tcl]

open_project $project_file
update_compile_order -fileset sources_1

if {[llength [get_files -quiet $dut_file]] == 0} {
    puts "Adding missing source file: $dut_file"
    add_files -norecurse -fileset [get_filesets sources_1] $dut_file
}
set_property file_type {VHDL 2008} [get_files $dut_file]

if {[llength [get_filesets -quiet sim_1]] == 0} {
    create_fileset -simset sim_1
}
if {[llength [get_files -quiet $tb_file]] == 0} {
    add_files -fileset sim_1 -norecurse $tb_file
}
set_property file_type {VHDL 2008} [get_files $tb_file]
set_property -name {xsim.elaborate.debug_level} -value {off} -objects [get_filesets sim_1]

update_compile_order -fileset sim_1

puts "Launching HyperRAM pump simulation (top=tb_vd_hr_pump)..."
sim_launch tb_vd_hr_pump

set sim_dir [file join \
    [file dirname [file normalize $project_file]] \
    "[file rootname [file tail $project_file]].sim" sim_1 behav xsim]

run 20ms
close_sim -force

set gate_ok 0
set sim_log [file join $sim_dir simulate.log]
if {[file exists $sim_log]} {
    set fh [open $sim_log r]
    while {[gets $fh line] >= 0} {
        if {[string match *ALL\ CHECKS\ PASSED* $line] || \
            [string match *simulation\ finished* $line] || \
            [string match *mismatch* $line] || \
            [string match *Failure* $line] || \
            [string match *ERROR* $line]} {
            puts $line
        }
        if {[string match *ALL\ CHECKS\ PASSED* $line]} { set gate_ok 1 }
        if {[string match *simulation\ finished* $line] && [string match *\"pass\":true* $line]} {
            set gate_ok 1
        }
    }
    close $fh
} else {
    puts "ERROR: Simulator log not found: $sim_log"
}
if {!$gate_ok} {
    puts "ERROR: vd_hr_pump gate failed"
}

close_project
if {!$gate_ok} {
    exit 2
}
exit 0
