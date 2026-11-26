# Report the bus skew constraints of a routed design (CORE.xdc's set_bus_skew), whose result
# is in neither WNS nor WHS. The parsing follows C64MEGA65's signoff_bus_skew (signoff_gates.tcl).
# Usage:
#   vivado -mode batch -source check_bus_skew.tcl -tclargs <routed.dcp>
# or source it with the routed design already open.

if {[llength [get_designs -quiet]] == 0} {
    open_checkpoint [lindex $argv 0]
}

set rpt [report_bus_skew -return_string]
set ids {}
set id ""
foreach line [split $rpt \n] {
    if {[regexp {^Id: +(\d+) *$} $line -> i]} {
        set id $i
        lappend ids $id
        set blk($id) ""
    } elseif {$id ne ""} {
        append blk($id) $line \n
    }
}

set pots 0
set bad  0
foreach id $ids {
    set b $blk($id)
    set req "?"; set ends "?"; set verdict "?"; set slack "?"
    regexp -line {^Requirement: +([-0-9.]+)ns} $b -> req
    regexp -line {^Endpoints: +(\d+)} $b -> ends
    regexp -line {^Slack \((\w+)\) *: +([-0-9.]+)ns} $b -> verdict slack
    if {[string first "i_pots2main" $b] >= 0} {
        incr pots
        set what "paddle crossing"
    } else {
        set what "constraint $id"
    }
    puts "Bus skew, $what: requirement $req ns, $ends endpoints, slack $slack ns ($verdict)"
    if {$verdict ne "MET"} {
        incr bad
    }
}
if {$pots != 1} {
    puts "Bus skew: $pots constraints on the paddle crossing instead of 1 (CORE.xdc set_bus_skew)"
}
if {$bad > 0} {
    puts $rpt
}
