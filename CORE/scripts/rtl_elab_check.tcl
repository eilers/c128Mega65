# Fast syntax/elaboration gate: opens the project, applies the same file-type
# fixups as build_bitstream.tcl, then elaborates the RTL without synthesizing.
# Usage:
#   vivado -mode batch -source rtl_elab_check.tcl -tclargs <project.xpr>

set project_file [lindex $argv 0]
if {$project_file eq ""} {
    puts "Usage: vivado -mode batch -source rtl_elab_check.tcl -tclargs <project.xpr>"
    exit 1
}

set core_dir [file normalize [file join [file dirname [info script]] ..]]
set repo_dir [file normalize [file join $core_dir ..]]

open_project $project_file
update_compile_order -fileset sources_1

set required_source_files [list \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/video_vicIIe_jb.sv"] \
    [file join $repo_dir "M2M/vhdl/controllers/MiSTer/video_sync.vhd"] \
    [file join $repo_dir "CORE/vhdl/video_sync_c128.sv"] \
    [file join $repo_dir "CORE/vhdl/clk_vdc.vhd"] \
]
foreach required_file $required_source_files {
    if {[llength [get_files -quiet $required_file]] == 0} {
        puts "Adding missing source file: $required_file"
        add_files -norecurse -fileset [get_filesets sources_1] $required_file
    }
}

set sv_files [get_files -all -quiet -filter {NAME =~ "*.sv"}]
foreach sv_file $sv_files {
    set_property file_type {SystemVerilog} $sv_file
}
set v_file [file join $repo_dir "CORE/C128_MiSTer/rtl/mos6526_8520.v"]
if {[llength [get_files -quiet $v_file]] > 0} {
    set_property file_type {SystemVerilog} [get_files $v_file]
}

puts "TOP: [get_property top [current_fileset]]"
puts "=== Elaborating RTL ==="
if {[catch {synth_design -rtl -name rtl_elab_check -no_iobuf} err]} {
    puts "ELAB_RESULT: FAIL"
    puts $err
    exit 2
}
puts "ELAB_RESULT: PASS"
exit 0
