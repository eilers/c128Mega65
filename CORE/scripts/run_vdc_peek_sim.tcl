# The system CPU's reads of the VDC (vdc_peek.vhd) against the real vdc_top (tb_vdc_peek).
set project_file [lindex $argv 0]
if {$project_file eq ""} {
    set project_file [file normalize [file join [file dirname [info script]] .. CORE-R6-vivado2022.xpr]]
}
set core_dir [file normalize [file join [file dirname [info script]] ..]]
set tb_file [file join $core_dir sim tb_vdc_peek.vhd]
set dut_file [file join $core_dir vhdl vdc_peek.vhd]
source [file join [file dirname [info script]] sim_launch.tcl]

open_project $project_file
if {[llength [get_filesets -quiet sim_1]] == 0} { create_fileset -simset sim_1 }
# The device goes where build_bitstream.tcl puts it, so a build after this sim still has it
if {[llength [get_files -quiet $dut_file]] == 0} {
    add_files -fileset sources_1 -norecurse $dut_file
}
if {[llength [get_files -quiet $tb_file]] == 0} {
    add_files -fileset sim_1 -norecurse $tb_file
}
set_property file_type {VHDL 2008} [get_files [list $dut_file $tb_file]]
set_property -name {xsim.elaborate.debug_level} -value {off} -objects [get_filesets sim_1]
update_compile_order -fileset sim_1
sim_launch tb_vdc_peek
run 50 ms
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
if {!$gate_ok} { puts "ERROR: tb_vdc_peek failed"; exit 2 }
exit 0
