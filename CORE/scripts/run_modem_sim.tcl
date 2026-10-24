# Run the SwiftLink modem simulation (tb_modem): the firmware in sdcard/c128/modem.bin
# boots on the NEORV32 from a HyperRAM model and answers AT commands through the 6551.
#
# Standalone xsim, because NEORV32 compiles into its own library; the project argument
# that run_all_sims.sh passes is ignored.
#
# Usage:
#   vivado -mode batch -source CORE/scripts/run_modem_sim.tcl

set core_dir [file normalize [file join [file dirname [info script]] ..]]
set repo_dir [file normalize [file join $core_dir ..]]
set bin_dir  [file join $::env(XILINX_VIVADO) bin]
set work_dir [file join $core_dir modem_sim.sim]
file mkdir $work_dir
cd $work_dir

set neo_dir [file join $repo_dir CORE/vhdl/neorv32]
set neo [list [file join $repo_dir CORE/vhdl/modem/neorv32_bootrom_image.vhd]]
set fh [open [file join $neo_dir file_list.txt] r]
foreach name [split [string trim [read $fh]] "\n"] {
    lappend neo [file join $neo_dir [string trim $name]]
}
close $fh

set work [list \
    [file join $repo_dir M2M/vhdl/axis_fifo_async.vhd] \
    [file join $repo_dir M2M/vhdl/axis_dropper.vhd] \
    [file join $repo_dir M2M/vhdl/controllers/ethernet/eth_rmii.vhd] \
    [file join $repo_dir M2M/vhdl/controllers/ethernet/eth_wrapper.vhd] \
    [file join $repo_dir CORE/vhdl/modem/acia6551.vhd] \
    [file join $repo_dir CORE/vhdl/modem/modem_hr_bridge.vhd] \
    [file join $repo_dir CORE/vhdl/modem/modem.vhd] \
    [file join $core_dir sim/tb_modem.vhd] \
]

proc step {log args} {
    if {[catch {exec -ignorestderr {*}$args > $log} err]} {
        puts [exec cat $log]
        puts "MODEM SIM: FAILED ($err)"
        exit 1
    }
}

step xvhdl_neorv32.log [file join $bin_dir xvhdl] --2008 -work neorv32 {*}$neo
step xvhdl.log         [file join $bin_dir xvhdl] --2008 {*}$work
step xelab.log         [file join $bin_dir xelab] --debug off --relax -L neorv32 -L unisim tb_modem \
                       -generic_top "G_IMAGE=[file join $repo_dir sdcard c128 modem.bin]" -s tb_modem_snap
step xsim.log          [file join $bin_dir xsim] tb_modem_snap -R

set fh [open xsim.log r]
set out [read $fh]
close $fh
foreach line [split $out "\n"] {
    if {[regexp {Note: |Error: |Failure: } $line]} {
        puts $line
    }
}
if {[string first "ALL CHECKS PASSED" $out] < 0} {
    puts "MODEM SIM: FAILED"
    exit 1
}
puts "MODEM SIM: PASSED"
