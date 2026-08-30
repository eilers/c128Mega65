# Focused regression for the 6522 Timer 1 load/read sequence used by 1571 DOS.
set project_file [lindex $argv 0]
if {$project_file eq ""} {
    set project_file [file normalize [file join [file dirname [info script]] .. CORE-R6-vivado2022.xpr]]
}

set core_dir [file normalize [file join [file dirname [info script]] ..]]
set tb_file [file join $core_dir sim tb_via_timer.sv]

open_project $project_file
if {[llength [get_filesets -quiet sim_1]] == 0} { create_fileset -simset sim_1 }
if {[llength [get_files -quiet $tb_file]] == 0} {
    add_files -fileset sim_1 -norecurse $tb_file
}
set_property file_type {SystemVerilog} [get_files $tb_file]
set_property top tb_via_timer [get_filesets sim_1]
set_property top_lib xil_defaultlib [get_filesets sim_1]
update_compile_order -fileset sim_1
launch_simulation -simset sim_1

set sim_dir [file join [file dirname [file normalize $project_file]] \
    "[file rootname [file tail $project_file]].sim" sim_1 behav xsim]
set sim_log [file join $sim_dir simulate.log]
close_sim -force
set gate_ok 0
if {[file exists $sim_log]} {
    set fh [open $sim_log r]
    while {[gets $fh line] >= 0} {
        if {[string first "PASS: T1 high byte after one count" $line] >= 0} { set gate_ok 1 }
        if {[string match *PASS* $line] || [string match *FAIL* $line]} { puts $line }
    }
    close $fh
}
close_project
if {!$gate_ok} { puts "ERROR: tb_via_timer failed"; exit 2 }
exit 0
