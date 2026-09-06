# Shared launch helper for the testbench runner scripts. Source it and call
# "sim_launch <top>" instead of setting the top and calling launch_simulation
# directly. It guards two Vivado behaviours that otherwise waste a run:
#
# 1. The project keeps its hierarchy up to date automatically, so a top that Vivado
#    cannot validate (a testbench that does not parse, for instance) is silently
#    replaced by the synthesis top mega65_r6. The run then simulates the whole core
#    or aborts with "Cannot find design unit", neither of which points at the real
#    problem. The top is therefore checked after the compile order was refreshed.
#
# 2. xvhdl compiles incrementally and only rebuilds files whose own source changed.
#    Changing a VHDL package invalidates the cached .vdb of every unit that uses it,
#    but units outside the previous run's compile order are never rebuilt, so a later
#    run that does pull such a unit in aborts with "needs to be re-saved since ...
#    changed". Only a full recompile recovers, so a failed launch is retried once
#    against a wiped simulation cache.

proc sim_set_top {tb} {
    set_property top $tb [get_filesets sim_1]
    set_property top_lib xil_defaultlib [get_filesets sim_1]
    update_compile_order -fileset sim_1

    set actual [get_property top [get_filesets sim_1]]
    if {$actual ne $tb} {
        error "simulation top fell back to '$actual' instead of '$tb': Vivado could\
               not validate the testbench, which usually means it does not parse"
    }
}

proc sim_launch {tb} {
    sim_set_top $tb
    if {![catch {launch_simulation -simset sim_1} msg]} {
        return
    }

    puts "launch_simulation failed: $msg"
    puts "Retrying with a clean simulation cache..."

    set sim_root [file join \
        [get_property DIRECTORY [current_project]] \
        "[get_property NAME [current_project]].sim"]
    catch {close_sim -force}
    file delete -force $sim_root

    sim_set_top $tb
    launch_simulation -simset sim_1
}
