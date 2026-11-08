# Run the REU-in-SDRAM simulation (tb_reu_sdram).
#
# The REU (reu.sv, reu_mapper, avm_cache, avm_fifo) and the virtual cartridge's crt_reader
# share the SDRAM controller through sdram_arbit, with a model of the chip. Checks that every
# cartridge read is there before the C128 latches it (1 MHz, FAST, Ultimax with the VIC
# fetching ROMH), that the REU still moves a byte per cycle, and that a FETCH across an SDRAM
# row end reads the right data.
#
# Usage:
#   vivado -mode batch -source CORE/scripts/run_reu_sdram_sim.tcl \
#     -tclargs CORE/CORE-R6-vivado2022.xpr

set project_file [lindex $argv 0]
if {$project_file eq ""} {
    set project_file [file normalize [file join [file dirname [info script]] .. CORE-R6-vivado2022.xpr]]
}

set core_dir [file normalize [file join [file dirname [info script]] ..]]
set repo_dir [file normalize [file join $core_dir ..]]
set tb_file  [file join $core_dir sim tb_reu_sdram.vhd]

source [file join [file dirname [info script]] sim_launch.tcl]

open_project $project_file
update_compile_order -fileset sources_1

foreach dut_file [list \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/reu.sv"] \
    [file join $repo_dir "CORE/vhdl/reu_mapper.vhd"] \
    [file join $repo_dir "M2M/vhdl/memory/avm_cache.vhd"] \
    [file join $repo_dir "CORE/vhdl/crt_reader.vhd"] \
    [file join $repo_dir "M2M/vhdl/controllers/SDRAM/sdram.vhd"] \
    [file join $repo_dir "CORE/vhdl/sdram_arbit.vhd"] \
    [file join $repo_dir "M2M/vhdl/axi_fifo_small.vhd"] \
] {
    if {[llength [get_files -quiet $dut_file]] == 0} {
        puts "Adding missing source file: $dut_file"
        add_files -norecurse -fileset [get_filesets sources_1] $dut_file
    }
    if {[file extension $dut_file] eq ".sv"} {
        set_property file_type {SystemVerilog} [get_files $dut_file]
    } else {
        set_property file_type {VHDL 2008} [get_files $dut_file]
    }
}

if {[llength [get_filesets -quiet sim_1]] == 0} {
    create_fileset -simset sim_1
}
foreach sim_file [list \
    $tb_file \
] {
    if {[llength [get_files -quiet $sim_file]] == 0} {
        add_files -fileset sim_1 -norecurse $sim_file
    }
    set_property file_type {VHDL 2008} [get_files $sim_file]
}
set_property -name {xsim.elaborate.debug_level} -value {off} -objects [get_filesets sim_1]

update_compile_order -fileset sim_1

puts "Launching REU-in-SDRAM simulation (top=tb_reu_sdram)..."
sim_launch tb_reu_sdram

set sim_dir [file join \
    [file dirname [file normalize $project_file]] \
    "[file rootname [file tail $project_file]].sim" sim_1 behav xsim]

run 40ms
close_sim -force

set gate_ok 0
set sim_log [file join $sim_dir simulate.log]
if {[file exists $sim_log]} {
    set fh [open $sim_log r]
    while {[gets $fh line] >= 0} {
        if {[string match *ALL\ CHECKS\ PASSED* $line] || \
            [string match *CHECKS\ FAILED* $line] || \
            [string match ---* $line] || \
            [string match *C128\ cycles* $line] || \
            [string match *row\ crossing* $line] || \
            [string match *cartridge\ reads:* $line] || \
            [string match *longest\ waits* $line] || \
            [string match SDRAM:* $line] || \
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
    puts "ERROR: REU-in-SDRAM gate failed"
}

close_project
if {!$gate_ok} {
    exit 2
}
exit 0
