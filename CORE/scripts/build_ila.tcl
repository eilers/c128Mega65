# Build a DEBUG bitstream with an inserted Vivado ILA on the mark_debug nets in main.vhd.
# Usage:
#   vivado -mode batch -source build_ila.tcl -tclargs <project.xpr>
#
# Output:
#   <impl_dir>/mega65_r6.bit   (debug bitstream - load via Vivado Hardware Manager / JTAG)
#   <impl_dir>/mega65_r6.ltx   (debug probes file for the Hardware Manager)
#
# NOTE: this is a DEBUG build (ILA consumes BRAM and perturbs placement). Do NOT release it.

set project_file [lindex $argv 0]
if {$project_file eq ""} {
    puts "Usage: vivado -mode batch -source build_ila.tcl -tclargs <project.xpr>"
    exit 1
}

set core_dir [file normalize [file join [file dirname [info script]] ..]]
set repo_dir [file normalize [file join $core_dir ..]]

open_project $project_file

set readonly 0
if {![catch {set readonly [get_property IS_READONLY [current_project]]}]} {
    if {$readonly} {
        set project_dir [file dirname $project_file]
        set project_base [file rootname [file tail $project_file]]
        set compat_project "${project_dir}/${project_base}-vivado2022.xpr"
        puts "Project opened read-only, saving compatibility copy to: $compat_project"
        save_project_as -force $compat_project
        close_project
        open_project $compat_project
    }
}

update_compile_order -fileset sources_1

# Ensure files referenced by instantiated components exist in sources_1.
set required_source_files [list \
    [file join $repo_dir "CORE/C128_MiSTer/rtl/video_vicIIe_jb.sv"] \
    [file join $repo_dir "M2M/vhdl/controllers/MiSTer/video_sync.vhd"] \
]
foreach required_file $required_source_files {
    if {[llength [get_files -quiet $required_file]] == 0} {
        add_files -norecurse -fileset [get_filesets sources_1] $required_file
    }
}

# Force SystemVerilog file type where required.
set sv_files [get_files -all -quiet -filter {NAME =~ "*.sv"}]
foreach sv_file $sv_files {
    set_property file_type {SystemVerilog} $sv_file
}
set sv_compat_v_files [list [file join $repo_dir "CORE/C128_MiSTer/rtl/mos6526_8520.v"]]
foreach v_file $sv_compat_v_files {
    if {[llength [get_files -quiet $v_file]] > 0} {
        set_property file_type {SystemVerilog} [get_files $v_file]
    }
}

# Keep the mark_debug nets through synthesis (do not flatten away debug nets).
if {[llength [get_runs -quiet synth_1]] > 0} {
    reset_run synth_1
}
set_property STEPS.SYNTH_DESIGN.ARGS.FLATTEN_HIERARCHY rebuilt [get_runs synth_1]

puts "Starting synthesis (synth_1)..."
launch_runs synth_1 -jobs 8
wait_on_run synth_1
if {[string match "*ERROR*" [get_property STATUS [get_runs synth_1]]]} {
    puts "ERROR: synthesis failed"
    exit 2
}

# Open the synthesized design in-memory and insert the ILA on the mark_debug nets.
open_run synth_1 -name synth_1

set marked [get_nets -quiet -hierarchical -filter {MARK_DEBUG}]
puts "mark_debug nets found: [llength $marked]"
if {[llength $marked] == 0} {
    puts "ERROR: no mark_debug nets found (check attributes survived synthesis)"
    exit 2
}

# Group bit-nets into buses by stripping a trailing [index].
array unset buses
set order {}
foreach n $marked {
    set base $n
    regexp {^(.*)\[[0-9]+\]$} $n -> base
    if {![info exists buses($base)]} { lappend order $base }
    lappend buses($base) $n
}

# Find the core (main_clk) clock net to sample on.
set clk_net ""
foreach pin {CORE/clk_gen/main_clk_bufg/O i_main/clk_main_i} {
    set c [get_nets -quiet -of_objects [get_pins -quiet $pin]]
    if {[llength $c] > 0} { set clk_net [lindex $c 0]; break }
}
if {$clk_net eq ""} {
    # Fall back: the clock of the first marked sequential net's driver.
    set clk_net [lindex [get_nets -quiet -hier -filter {NAME =~ *main_clk*}] 0]
}
puts "ILA sample clock net: $clk_net"

# Minimal ILA: BRAM-based capture, basic trigger, no storage qualification / advanced
# trigger / input pipeline (those infer large amounts of SRL/LUTRAM and overflow this
# already LUT-heavy design). Single match unit per probe keeps it small.
create_debug_core u_ila ila
set_property C_DATA_DEPTH 4096 [get_debug_cores u_ila]
set_property C_TRIGIN_EN false [get_debug_cores u_ila]
set_property C_TRIGOUT_EN false [get_debug_cores u_ila]
set_property C_ADV_TRIGGER false [get_debug_cores u_ila]
set_property C_INPUT_PIPE_STAGES 0 [get_debug_cores u_ila]
set_property C_EN_STRG_QUAL false [get_debug_cores u_ila]
set_property ALL_PROBE_SAME_MU true [get_debug_cores u_ila]
set_property ALL_PROBE_SAME_MU_CNT 1 [get_debug_cores u_ila]
set_property port_width 1 [get_debug_ports u_ila/clk]
connect_debug_port u_ila/clk [get_nets $clk_net]

set idx 0
foreach base $order {
    set nets [lsort -dictionary $buses($base)]
    set w [llength $nets]
    if {$idx == 0} {
        set port [get_debug_ports u_ila/probe0]
    } else {
        set port [create_debug_port u_ila probe]
    }
    set_property port_width $w $port
    connect_debug_port $port $nets
    puts "  probe$idx <= $base  (width $w)"
    incr idx
}

# Implement the in-memory design (with the ILA) directly.
puts "Implementing (opt/place/route)..."
opt_design
place_design
route_design

set impl_dir [file join [file dirname $project_file] "[file rootname [file tail $project_file]].runs" impl_1]
file mkdir $impl_dir
set bit_file [file join $impl_dir mega65_r6.bit]
set ltx_file [file join $impl_dir mega65_r6.ltx]
write_bitstream -force $bit_file
write_debug_probes -force $ltx_file
puts "ILA bitstream: $bit_file"
puts "ILA probes:    $ltx_file"

set wns [get_property SLACK [get_timing_paths -max_paths 1 -nworst 1 -setup]]
puts "Post-route WNS: $wns"

exit 0
