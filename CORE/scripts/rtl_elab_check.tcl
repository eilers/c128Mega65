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
    # See the comment in build_bitstream.tcl: VHDL sources need 2008 on every run, not
    # only when they were missing, because a Vivado save resets the type to the default.
    if {[file extension $required_file] in {.vhd .vhdl}} {
        set_property file_type {VHDL 2008} [get_files $required_file]
    }
}

set sv_files [get_files -all -quiet -filter {NAME =~ "*.sv"}]
foreach sv_file $sv_files {
    set_property file_type {SystemVerilog} $sv_file
}
set sv_compat_v_files [list \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/mos6526_8520.v"] \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/c1581_fdc1772.v"] \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/iec_drive/floppy.v"] \
]
foreach v_file $sv_compat_v_files {
    if {[llength [get_files -quiet $v_file]] > 0} {
        set_property file_type {SystemVerilog} [get_files $v_file]
    }
}

puts "TOP: [get_property top [current_fileset]]"

# Promote implicit net declarations to errors. When a port connection names an
# identifier before its declaration, Vivado silently creates a 1-bit net and ignores
# the wider declaration that follows. That cost us several days of debugging: it is
# how the 1541/1571 lost its GCR read byte (gcr_do) and its track number, how the 1581
# lost its seek target (data_in), and how the SID lost its combined waveforms. None of
# it shows up in simulation, because xsim resolves names across the whole scope.
set_msg_config -id {Synth 8-8895} -new_severity ERROR

puts "=== Elaborating RTL ==="
if {[catch {synth_design -rtl -name rtl_elab_check -no_iobuf} err]} {
    puts "ELAB_RESULT: FAIL"
    puts $err
    exit 2
}
puts "ELAB_RESULT: PASS"
exit 0
