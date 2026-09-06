set project_file [lindex $argv 0]
set top_name [lindex $argv 1]
if {$project_file eq ""} {
    set project_file [file normalize [file join [file dirname [info script]] .. CORE-R6-vivado2022.xpr]]
}
if {$top_name eq ""} { set top_name tb_c1541_read_write }

set core_dir [file normalize [file join [file dirname [info script]] ..]]
set tb_file [file join $core_dir sim tb_c157x_gcr_path.sv]
source [file join [file dirname [info script]] sim_launch.tcl]

open_project $project_file
if {[llength [get_filesets -quiet sim_1]] == 0} { create_fileset -simset sim_1 }
if {[llength [get_files -quiet $tb_file]] == 0} {
    add_files -fileset sim_1 -norecurse $tb_file
}
set_property file_type {SystemVerilog} [get_files $tb_file]
set_property -name {xsim.elaborate.debug_level} -value {off} -objects [get_filesets sim_1]
update_compile_order -fileset sim_1
sim_launch $top_name
run 40 ms

set sim_dir [file join [file dirname [file normalize $project_file]] \
    "[file rootname [file tail $project_file]].sim" sim_1 behav xsim]
close_sim -force
set gate_ok 0
set sim_log [file join $sim_dir simulate.log]
if {[file exists $sim_log]} {
    set fh [open $sim_log r]
    while {[gets $fh line] >= 0} {
        if {[string match *ALL\ CHECKS\ PASSED* $line]} { set gate_ok 1 }
        if {[string match *PASS* $line] || [string match *FAIL* $line]} { puts $line }
    }
    close $fh
}
close_project
if {!$gate_ok} { puts "ERROR: $top_name failed"; exit 2 }
exit 0
