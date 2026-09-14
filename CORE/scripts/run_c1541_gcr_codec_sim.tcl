# Run the independent Commodore 1541 GCR codec unit test.
#
# Usage:
#   vivado -mode batch -source CORE/scripts/run_c1541_gcr_codec_sim.tcl \
#     -tclargs CORE/CORE-R6-vivado2022.xpr

set project_file [lindex $argv 0]
if {$project_file eq ""} {
    set project_file [file normalize [file join [file dirname [info script]] .. CORE-R6-vivado2022.xpr]]
}

set core_dir   [file normalize [file join [file dirname [info script]] ..]]
set codec_file [file join $core_dir C128_MiSTer rtl iec_drive c1541_gcr_codec.sv]
set tb_file    [file join $core_dir sim tb_c1541_gcr_codec.sv]

source [file join [file dirname [info script]] sim_launch.tcl]

open_project $project_file

if {[llength [get_files -quiet $codec_file]] == 0} {
    puts "Adding missing source file: $codec_file"
    add_files -norecurse -fileset [get_filesets sources_1] $codec_file
}
set_property file_type {SystemVerilog} [get_files $codec_file]

if {[llength [get_filesets -quiet sim_1]] == 0} {
    create_fileset -simset sim_1
}
if {[llength [get_files -quiet $tb_file]] == 0} {
    add_files -fileset sim_1 -norecurse $tb_file
}
set_property file_type {SystemVerilog} [get_files $tb_file]

set_property -name {xsim.elaborate.debug_level} -value {off} -objects [get_filesets sim_1]
update_compile_order -fileset sources_1
update_compile_order -fileset sim_1

puts "Launching 1541 GCR codec simulation (top=tb_c1541_gcr_codec)..."
sim_launch tb_c1541_gcr_codec

set sim_dir [file join \
    [file dirname [file normalize $project_file]] \
    "[file rootname [file tail $project_file]].sim" sim_1 behav xsim]

run 10us
close_sim -force

set gate_ok 0
set sim_log [file join $sim_dir simulate.log]
if {[file exists $sim_log]} {
    set fh [open $sim_log r]
    while {[gets $fh line] >= 0} {
        if {[string match *tb_c1541_gcr_codec* $line] \
            || [string match *MISMATCH* $line] \
            || [string match *Failure* $line]} {
            puts $line
        }
        if {[string match *simulation\ finished* $line]} {
            if {[string match *\"pass\":true* $line]} {
                set gate_ok 1
            } else {
                puts "ERROR: 1541 GCR codec gate failed (pass=false)"
            }
        }
    }
    close $fh
} else {
    puts "ERROR: Simulator log not found: $sim_log"
}
if {!$gate_ok} {
    puts "ERROR: 1541 GCR codec summary line missing or failed"
}

close_project
if {!$gate_ok} {
    exit 2
}
exit 0
