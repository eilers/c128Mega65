set bit_file [lindex $argv 0]
if {$bit_file eq ""} {
    set bit_file [file normalize [file join [file dirname [info script]] .. \
        CORE-R6-vivado2022.runs impl_1 mega65_r6.bit]]
}
if {![file exists $bit_file]} {
    puts "ERROR: bitstream not found: $bit_file"
    exit 2
}

open_hw_manager
connect_hw_server -allow_non_jtag
open_hw_target
set devices [get_hw_devices -quiet -filter {PART =~ "xc7a200t*"}]
if {[llength $devices] != 1} {
    puts "ERROR: expected one MEGA65 xc7a200t, found [llength $devices]: $devices"
    close_hw_manager
    exit 2
}

set device [lindex $devices 0]
current_hw_device $device
refresh_hw_device -update_hw_probes false $device
set_property PROGRAM.FILE $bit_file $device
puts "Programming $device with $bit_file"
program_hw_devices $device
refresh_hw_device $device
puts "PROGRAM_RESULT: PASS"
close_hw_manager
exit 0
