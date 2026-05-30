# Generate bitstream in batch mode.
# Usage:
#   vivado -mode batch -source build_bitstream.tcl -tclargs <project.xpr>

set project_file [lindex $argv 0]
if {$project_file eq ""} {
    puts "Usage: vivado -mode batch -source build_bitstream.tcl -tclargs <project.xpr>"
    exit 1
}

# Resolve repository root from this script location to avoid hard-coded absolute paths.
set core_dir [file normalize [file join [file dirname [info script]] ..]]
set repo_dir [file normalize [file join $core_dir ..]]

open_project $project_file

set readonly 0
if {![catch {set readonly [get_property IS_READONLY [current_project]]}]} {
    if {$readonly} {
        set project_dir [file dirname $project_file]
        set project_base [file rootname [file tail $project_file]]
        set compat_project "${project_dir}/${project_base}-vivado2022.xpr"
        puts "Project opened read-only, saving compatibility copy to: $compat_project"
        save_project_as -force $compat_project
        close_project
        open_project $compat_project
    }
}

update_compile_order -fileset sources_1

# Ensure files referenced by instantiated components exist in sources_1.
set required_sv_files [list \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/video_vicIIe_jb.sv"] \
]
foreach required_file $required_sv_files {
    if {[llength [get_files -quiet $required_file]] == 0} {
        puts "Adding missing source file: $required_file"
        add_files -norecurse -fileset [get_filesets sources_1] $required_file
    }
}

# Vivado 2022 may import .sv files as plain Verilog from newer project files.
# Force all .sv sources to SystemVerilog before synthesis.
set sv_files [get_files -all -quiet -filter {NAME =~ "*.sv"}]
if {[llength $sv_files] > 0} {
    puts "Setting SystemVerilog file type on [llength $sv_files] .sv files"
    foreach sv_file $sv_files {
        set_property file_type {SystemVerilog} $sv_file
    }
}

# Some C128 MiSTer files use SystemVerilog constructs despite .v extension.
set sv_compat_v_files [list \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/mos6526_8520.v"] \
]
foreach v_file $sv_compat_v_files {
    if {[llength [get_files -quiet $v_file]] > 0} {
        puts "Setting SystemVerilog file type on $v_file"
        set_property file_type {SystemVerilog} [get_files $v_file]
    }
}

if {[llength [get_runs -quiet synth_1]] > 0} {
    reset_run synth_1
}
if {[llength [get_runs -quiet impl_1]] > 0} {
    reset_run impl_1
}

puts "Starting synthesis (synth_1)..."
launch_runs synth_1 -jobs 8
wait_on_run synth_1
set synth_status [get_property STATUS [get_runs synth_1]]
puts "synth_1 status: $synth_status"

puts "Starting implementation and bitstream (impl_1)..."
launch_runs impl_1 -to_step write_bitstream -jobs 8
wait_on_run impl_1
set impl_status [get_property STATUS [get_runs impl_1]]
puts "impl_1 status: $impl_status"

# Vivado does not always expose BITSTREAM.FILE on run objects.
# Read the generated bit file directly from the impl run directory.
set impl_dir [get_property DIRECTORY [get_runs impl_1]]
set bit_files [glob -nocomplain -directory $impl_dir "*.bit"]
if {[llength $bit_files] > 0} {
    puts "Bitstream: [lindex $bit_files 0]"
}

if {[string match "*ERROR*" $synth_status] || [string match "*ERROR*" $impl_status]} {
    exit 2
}

exit 0
