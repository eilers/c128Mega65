# Report what the synthesized netlist actually does with iec_drive's drive-model
# registers. A .D81 that answers as a 1571 means img_hd stayed 0 on hardware, so this
# checks whether the register survived synthesis and what feeds its data pin.
#
# Usage:
#   vivado -mode batch -source CORE/scripts/query_img_hd.tcl -tclargs <synth dcp>

set dcp [lindex $argv 0]
if {$dcp eq ""} {
    set dcp [file normalize [file join [file dirname [info script]] .. \
        CORE-R6-vivado2022.runs synth_1 mega65_r6.dcp]]
}

open_checkpoint $dcp

foreach pattern {*img_hd* *img_gcr* *img_ds* *img_mfm* *rom_bank*} {
    set cells [get_cells -hierarchical -quiet -filter "NAME =~ $pattern"]
    puts "### $pattern : [llength $cells] cell(s)"
    foreach c $cells {
        puts "    $c   (ref=[get_property -quiet REF_NAME $c])"
    }
}

# Trace the data input of every img_hd flop back to its driver.
foreach c [get_cells -hierarchical -quiet -filter {NAME =~ *img_hd*}] {
    foreach pin [get_pins -quiet -of_objects $c -filter {DIRECTION == IN}] {
        set net [get_nets -quiet -of_objects $pin]
        if {[llength $net] == 0} {
            puts "  $pin  <- UNCONNECTED"
        } else {
            set drivers [get_pins -quiet -of_objects $net -filter {DIRECTION == OUT}]
            puts "  $pin  <- net $net  driven by: $drivers"
        }
    }
}

close_project
exit 0
