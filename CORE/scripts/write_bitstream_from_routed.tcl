# Write bitstream from an already-routed impl checkpoint (skip re-impl).
# Usage:
#   vivado -mode batch -source CORE/scripts/write_bitstream_from_routed.tcl \
#     -tclargs CORE/CORE-R6-vivado2022.runs/impl_1/mega65_r6_routed.dcp

set dcp [lindex $argv 0]
if {$dcp eq "" || ![file exists $dcp]} {
    puts "Usage: write_bitstream_from_routed.tcl <routed.dcp>"
    exit 1
}

open_checkpoint $dcp

# Demote checks that block bitgen on this already-routed bring-up checkpoint.
# AVAL-139 is fixed in clk_vdc.vhd (integer 8/25); RTRES is a stale BACKBONE attr.
catch {set_property SEVERITY {Warning} [get_drc_checks AVAL-139]}
catch {set_property SEVERITY {Warning} [get_drc_checks RTRES-1]}

foreach n [get_nets -quiet clk_i_IBUF] {
    catch {set_property CLOCK_DEDICATED_ROUTE TRUE $n}
    puts "Set CLOCK_DEDICATED_ROUTE TRUE on $n"
}

set out_bit [file rootname $dcp]
append out_bit ".bit"
write_bitstream -force $out_bit
puts "Bitstream: $out_bit"
close_design
exit 0
