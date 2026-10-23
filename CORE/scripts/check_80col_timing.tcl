# Re-time a routed design with both video clock muxes on vdc_clk, i.e. 80-column mode.
# CORE.xdc pins the muxes to main_clk, so implementation never times this case itself.
# Usage:
#   vivado -mode batch -source check_80col_timing.tcl -tclargs <routed.dcp>
# or source it with the routed design already open.

if {[llength [get_designs -quiet]] == 0} {
    open_checkpoint [lindex $argv 0]
}

set_case_analysis 1 [get_pins {CORE/i_video_clk_mux/S0 CORE/i_video_clk_mux/S1}]
set_case_analysis 1 [get_pins {CORE/i_vga_clk_mux/S0 CORE/i_vga_clk_mux/S1}]
update_timing -full

set clk [get_clocks vdc_clk]
set wns [get_property SLACK [get_timing_paths -from $clk -to $clk -setup -max_paths 1]]
set whs [get_property SLACK [get_timing_paths -from $clk -to $clk -hold -max_paths 1]]
set fs  [llength [get_timing_paths -from $clk -to $clk -setup -max_paths 10000 -nworst 1 -slack_lesser_than 0]]
set fh  [llength [get_timing_paths -from $clk -to $clk -hold  -max_paths 10000 -nworst 1 -slack_lesser_than 0]]

puts [format "80-column timing (vdc_clk): WNS %.3f ns, WHS %.3f ns, %d setup and %d hold failures" \
    $wns $whs $fs $fh]
if {$fs > 0 || $fh > 0} {
    report_timing -from $clk -to $clk -delay_type min_max -max_paths 5
}
