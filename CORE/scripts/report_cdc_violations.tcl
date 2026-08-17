# Dump every violating path between two clocks from a routed checkpoint.
# Usage:
#   vivado -mode batch -source report_cdc_violations.tcl -tclargs <routed.dcp> <from_clk> <to_clk>

set dcp   [lindex $argv 0]
set from  [lindex $argv 1]
set to    [lindex $argv 2]

open_checkpoint $dcp

foreach {a b} [list $from $to $to $from] {
    set paths [get_timing_paths -from [get_clocks $a] -to [get_clocks $b] \
                                -max_paths 5000 -nworst 1 -slack_lesser_than 0]
    puts "==== $a -> $b : [llength $paths] violating paths ===="
    foreach p $paths {
        puts "PATH [format %.3f [get_property SLACK $p]] \
              [get_property STARTPOINT_PIN $p] -> [get_property ENDPOINT_PIN $p]"
    }
}

exit 0
