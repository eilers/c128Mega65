# Run the C128 boot behavioural simulation (tb_c128_boot).
# Usage:
#   vivado -mode batch -source CORE/scripts/run_boot_sim.tcl -tclargs CORE/CORE-R6-vivado2022.xpr

set project_file [lindex $argv 0]
if {$project_file eq ""} {
    set project_file [file normalize [file join [file dirname [info script]] .. CORE-R6-vivado2022.xpr]]
}

set core_dir  [file normalize [file join [file dirname [info script]] ..]]
set repo_dir  [file normalize [file join $core_dir ..]]
set tb_file   [file join $core_dir sim tb_c128_boot.vhd]
set pkg_file  [file join $core_dir sim sim_support_pkg.vhd]
set boot_rom  [file join $repo_dir sdcard c128 boot0.rom]
set log_file  [file join $repo_dir .cursor debug-boot.log]

if {![file exists $boot_rom]} {
    puts "ERROR: Missing boot ROM: $boot_rom"
    exit 2
}

if {[file exists $log_file]} {
    file delete -force $log_file
}

source [file join [file dirname [info script]] sim_launch.tcl]

open_project $project_file
update_compile_order -fileset sources_1

# Ensure new core sources exist in the project (same list as build_bitstream.tcl).
foreach required_file [list \
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
] {
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

# Vivado 2022 may import .sv files as plain Verilog; force SystemVerilog so the
# simulator (xvlog) parses them correctly. Mirrors build_bitstream.tcl.
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

if {[llength [get_filesets -quiet sim_1]] == 0} {
    create_fileset -simset sim_1
}

if {[llength [get_files -quiet $pkg_file]] == 0} {
    add_files -fileset sources_1 -norecurse $pkg_file
}
if {[llength [get_files -quiet $tb_file]] == 0} {
    add_files -fileset sim_1 -norecurse $tb_file
}

set paths_pkg [file join $core_dir sim boot_paths_pkg.vhd]
source [file join $core_dir scripts gen_boot_paths_pkg.tcl]
if {[llength [get_files -quiet $paths_pkg]] == 0} {
    add_files -fileset sources_1 -norecurse $paths_pkg
}
puts "ROM path: $boot_rom"
puts "Log path: $log_file"
update_compile_order -fileset sim_1
set_property -name {xsim.simulate.runtime} -value {25ms} -objects [get_filesets sim_1]

puts "Launching boot simulation (top=tb_c128_boot)..."
sim_launch tb_c128_boot

set sim_log [file join $core_dir sim boot_sim_console.log]
set sim_out [open $sim_log w]
puts $sim_out [run 25ms]
close $sim_out
puts "Simulator console log: $sim_log"

set gate_ok 1
if {[file exists $log_file]} {
    set summary_line ""
    set fh [open $log_file r]
    while {[gets $fh line] >= 0} {
        puts $line
        if {[string match *simulation\ finished* $line]} {
            set summary_line $line
        }
    }
    close $fh

    if {$summary_line eq ""} {
        puts "ERROR: Boot sim summary line missing in $log_file"
        set gate_ok 0
    } elseif {[string match *\"pass\":false* $summary_line]} {
        puts "ERROR: Boot sim gate failed (pass=false)"
        set gate_ok 0
    }
} else {
    puts "WARNING: Debug log not written: $log_file"
    set gate_ok 0
}

close_sim -force
close_project
if {!$gate_ok} {
    exit 2
}
exit 0
