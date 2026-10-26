# Run the FAST-mode I/O simulation (tb_fast_io): main boots as in tb_c128_boot with the REU and
# the SwiftLink at $DE00, then 8502 code from /NMI reads the REU's status at 1 and 2 MHz and
# touches the 6551 at both speeds. Each access must strobe the device once.
# Usage:
#   vivado -mode batch -source CORE/scripts/run_fast_io_sim.tcl -tclargs CORE/CORE-R6-vivado2022.xpr

set project_file [lindex $argv 0]
if {$project_file eq ""} {
    set project_file [file normalize [file join [file dirname [info script]] .. CORE-R6-vivado2022.xpr]]
}

set core_dir  [file normalize [file join [file dirname [info script]] ..]]
set repo_dir  [file normalize [file join $core_dir ..]]
set tb_file   [file join $core_dir sim tb_fast_io.vhd]
set pkg_file  [file join $core_dir sim sim_support_pkg.vhd]
set boot_rom  [file join $repo_dir sdcard c128 boot0.rom]

if {![file exists $boot_rom]} {
    puts "ERROR: Missing boot ROM: $boot_rom"
    exit 2
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
    [file join $repo_dir "CORE/vhdl/vd_hr_pump.vhd"] \
    [file join $repo_dir "M2M/vhdl/QNICE/sdhost.vhd"] \
    [file join $repo_dir "CORE/vhdl/sd_dma.vhd"] \
    [file join $repo_dir "CORE/vhdl/sd_eng_arb.vhd"] \
    [file join $repo_dir "CORE/vhdl/vd_sd.vhd"] \
    [file join $repo_dir "CORE/vhdl/physical_1581/physical_1581_pkg.vhd"] \
    [file join $repo_dir "CORE/vhdl/physical_1581/physical_1581_crc.vhd"] \
    [file join $repo_dir "CORE/vhdl/physical_1581/physical_1581_inputs.vhd"] \
    [file join $repo_dir "CORE/vhdl/physical_1581/physical_1581_mfm_gaps.vhd"] \
    [file join $repo_dir "CORE/vhdl/physical_1581/physical_1581_mfm_quantise.vhd"] \
    [file join $repo_dir "CORE/vhdl/physical_1581/physical_1581_mfm_gaps_to_bits.vhd"] \
    [file join $repo_dir "CORE/vhdl/physical_1581/physical_1581_mfm_bits_to_bytes.vhd"] \
    [file join $repo_dir "CORE/vhdl/physical_1581/physical_1581_mfm_decoder.vhd"] \
    [file join $repo_dir "CORE/vhdl/physical_1581/physical_1581_mfm_encoder.vhd"] \
    [file join $repo_dir "CORE/vhdl/physical_1581/physical_1581_writer.vhd"] \
    [file join $repo_dir "CORE/vhdl/physical_1581/physical_1581_wfifo.vhd"] \
    [file join $repo_dir "CORE/vhdl/physical_1581/physical_1581_rdfifo.vhd"] \
    [file join $repo_dir "CORE/vhdl/physical_1581/physical_1581_controller.vhd"] \
    [file join $repo_dir "CORE/vhdl/physical_1581/physical_1581_diag.vhd"] \
    [file join $repo_dir "CORE/vhdl/drive_rom_server.vhd"] \
    [file join $repo_dir "CORE/vhdl/reu_mapper.vhd"] \
    [file join $repo_dir "M2M/vhdl/memory/avm_cache.vhd"] \
    [file join $repo_dir "CORE/vhdl/cartridge.vhd"] \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/reu.sv"] \
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
foreach sim_file [list \
    [file join $repo_dir "M2M/vhdl/memory/avm_memory.vhd"] \
    [file join $repo_dir "M2M/vhdl/memory/avm_pause.vhd"] \
    [file join $repo_dir "M2M/vhdl/memory/avm_memory_pause.vhd"] \
    $tb_file \
] {
    if {[llength [get_files -quiet $sim_file]] == 0} {
        add_files -fileset sim_1 -norecurse $sim_file
    }
    set_property file_type {VHDL 2008} [get_files $sim_file]
}

set paths_pkg [file join $core_dir sim boot_paths_pkg.vhd]
source [file join $core_dir scripts gen_boot_paths_pkg.tcl]
if {[llength [get_files -quiet $paths_pkg]] == 0} {
    add_files -fileset sources_1 -norecurse $paths_pkg
}
puts "ROM path: $boot_rom"
update_compile_order -fileset sim_1

puts "Launching FAST-mode I/O simulation (top=tb_fast_io)..."
sim_launch tb_fast_io

set sim_dir [file join \
    [file dirname [file normalize $project_file]] \
    "[file rootname [file tail $project_file]].sim" sim_1 behav xsim]

run 120ms
close_sim -force

set gate_ok 0
set sim_log [file join $sim_dir simulate.log]
if {[file exists $sim_log]} {
    set fh [open $sim_log r]
    while {[gets $fh line] >= 0} {
        if {[string match *ALL\ CHECKS\ PASSED* $line] || \
            [string match *CHECKS\ FAILED* $line] || \
            [string match *REU\ status* $line] || \
            [string match *select\ rose* $line] || \
            [string match */NMI\ code* $line] || \
            [string match *8502\ has* $line] || \
            [string match *Error:* $line] || \
            [string match *Failure* $line] || \
            [string match *ERROR* $line]} {
            puts $line
        }
        if {[string match *ALL\ CHECKS\ PASSED* $line]} { set gate_ok 1 }
    }
    close $fh
} else {
    puts "ERROR: Simulator log not found: $sim_log"
}
if {!$gate_ok} {
    puts "ERROR: FAST-mode I/O gate failed"
}

close_project
if {!$gate_ok} {
    exit 2
}
exit 0
