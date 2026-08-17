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
set required_source_files [list \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/video_vicIIe_jb.sv"] \
    [file join $repo_dir "M2M/vhdl/controllers/MiSTer/video_sync.vhd"] \
    [file join $repo_dir "CORE/vhdl/video_sync_c128.sv"] \
    [file join $repo_dir "CORE/vhdl/clk_vdc.vhd"] \
    [file join $repo_dir "CORE/vhdl/cartridge_heuristics.vhd"] \
    [file join $repo_dir "CORE/vhdl/mount_buf_wrapper.vhd"] \
    [file join $repo_dir "CORE/vhdl/drive_rom_server.vhd"] \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/iec_drive.sv"] \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/iecdrv_misc.sv"] \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/iecdrv_rom.sv"] \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/iecdrv_via6522.vhd"] \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/c157x_multi.sv"] \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/c157x_drv.sv"] \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/c157x_logic.sv"] \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/c157x_h156.sv"] \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/c157x_heads.sv"] \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/c157x_track.sv"] \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/c157x_fdc1772.sv"] \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/c1581_multi.sv"] \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/c1581_drv.sv"] \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/c1581_fdc1772.v"] \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/floppy.v"] \
]
foreach required_file $required_source_files {
    if {[llength [get_files -quiet $required_file]] == 0} {
        puts "Adding missing source file: $required_file"
        add_files -norecurse -fileset [get_filesets sources_1] $required_file
    }
    # A freshly added .vhd defaults to VHDL-93, and every VHDL source in this design needs
    # 2008. Vivado also drops the type back to the default whenever it rewrites the .xpr,
    # so set it on every run rather than only when the file was missing.
    if {[file extension $required_file] in {.vhd .vhdl}} {
        set_property file_type {VHDL 2008} [get_files $required_file]
    }
}

# Board-specific sources not present in the shared R6-derived project template.
set project_base [file rootname [file tail $project_file]]
if {[regexp {CORE-R3} $project_base]} {
    foreach required_file [list \
        [file join $repo_dir "M2M/vhdl/controllers/M65/max10.vhdl"] \
        [file join $repo_dir "M2M/vhdl/controllers/M65/pcm_to_pdm.vhdl"] \
    ] {
        if {[llength [get_files -quiet $required_file]] == 0} {
            puts "Adding R3-specific source file: $required_file"
            add_files -norecurse -fileset [get_filesets sources_1] $required_file
        }
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
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/c1581_fdc1772.v"] \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/floppy.v"] \
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
if {[string match "*ERROR*" $synth_status]} {
    exit 2
}

# Refuse to build a bitstream that contains implicit nets. When a port connection names
# an identifier before its declaration, Vivado silently creates a 1-bit net and ignores
# the wider declaration that follows, so an 8-bit signal quietly becomes 1 bit. That is
# how the 1541/1571 lost its GCR read byte and its track number, and how the 1581 lost
# its seek target. Simulation cannot catch it, because xsim resolves names across the
# whole scope. This is a log scan rather than a set_msg_config promotion because
# launch_runs synthesizes in a separate Vivado process.
set synth_log [file join [get_property DIRECTORY [get_runs synth_1]] runme.log]
if {[file exists $synth_log]} {
    set fh [open $synth_log r]
    set log_text [read $fh]
    close $fh
    set implicit_nets [lsearch -all -inline [split $log_text "\n"] "*Synth 8-8895*"]
    if {[llength $implicit_nets] > 0} {
        puts "ERROR: synthesis created implicit nets, which silently truncate vectors."
        puts "       Declare each of these before the port connection that names it:"
        foreach line $implicit_nets {
            puts "       [string trim $line]"
        }
        exit 2
    }
}

puts "Starting implementation and bitstream (impl_1)..."
launch_runs impl_1 -to_step write_bitstream -jobs 14
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
