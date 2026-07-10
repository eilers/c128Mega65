# Run C128 boot-focused behavioural simulation (tb_c128_boot).
# Usage:
#   vivado -mode batch -source CORE/scripts/run_boot_sim.tcl -tclargs CORE/CORE-R6-vivado2022.xpr

set project_file [lindex $argv 0]
set mem_bridge 0
if {[lindex $argv 1] eq "--mem-bridge"} {
    set mem_bridge 1
}
if {$project_file eq ""} {
    set project_file [file normalize [file join [file dirname [info script]] .. CORE-R6-vivado2022.xpr]]
}

set core_dir  [file normalize [file join [file dirname [info script]] ..]]
set repo_dir  [file normalize [file join $core_dir ..]]
set tb_file   [file join $core_dir sim tb_c128_boot.vhd]
set pkg_file  [file join $core_dir sim sim_support_pkg.vhd]
set boot_rom  [file join $repo_dir sdcard c128 boot0.rom]
set log_file  [file join $repo_dir .cursor debug-b576b7.log]

if {![file exists $boot_rom]} {
    puts "ERROR: Missing boot ROM: $boot_rom"
    exit 2
}

if {[file exists $log_file]} {
    file delete -force $log_file
}

open_project $project_file
update_compile_order -fileset sources_1

# Vivado 2022 may import .sv files as plain Verilog; force SystemVerilog so the
# simulator (xvlog) parses them correctly. Mirrors build_bitstream.tcl. Needed after
# the upstream merge added SystemVerilog SID files (e.g. rtl/sid/sid_filter.sv).
set sv_files [get_files -all -quiet -filter {NAME =~ "*.sv"}]
foreach sv_file $sv_files {
    set_property file_type {SystemVerilog} $sv_file
}
set repo_root [file normalize [file join $core_dir ..]]
set sv_compat_v_files [list [file join $repo_root "CORE/C128_MiSTer/rtl/mos6526_8520.v"]]
foreach v_file $sv_compat_v_files {
    if {[llength [get_files -quiet $v_file]] > 0} {
        set_property file_type {SystemVerilog} [get_files $v_file]
    }
}

if {[llength [get_filesets -quiet sim_1]] == 0} {
    create_fileset -simset sim_1
}

if {[llength [get_files -quiet $pkg_file]] == 0} {
    add_files -fileset sources_1 -norecurse $pkg_file
}
if {[llength [get_files -quiet $tb_file]] == 0} {
    add_files -fileset sim_1 -norecurse $tb_file
}

set paths_pkg [file join $core_dir sim boot_paths_pkg.vhd]
set ::sim_mem_bridge $mem_bridge
source [file join $core_dir scripts gen_boot_paths_pkg.tcl]
if {[llength [get_files -quiet $paths_pkg]] == 0} {
    add_files -fileset sources_1 -norecurse $paths_pkg
}
set_property top tb_c128_boot [get_filesets sim_1]
set_property top_lib xil_defaultlib [get_filesets sim_1]
if {$mem_bridge} {
    puts "Mode: C_MEM_BRIDGE=true (H-V23 regression, must fail gate)"
} else {
    puts "Mode: C_MEM_BRIDGE=false (baseline, must pass gate)"
}
puts "ROM path: $boot_rom"
puts "Log path: $log_file"
update_compile_order -fileset sim_1
set_property -name {xsim.simulate.runtime} -value {25ms} -objects [get_filesets sim_1]

puts "Launching boot simulation (top=tb_c128_boot)..."
if {[llength [get_runs -quiet sim_1]] > 0} {
    catch {reset_simulation -simset sim_1 -force}
}
launch_simulation -simset sim_1

set sim_log [file join $core_dir sim boot_sim_console.log]
set sim_out [open $sim_log w]
puts $sim_out [run 25ms]
close $sim_out
puts "Simulator console log: $sim_log"

# Sample internal Z80/VIC bus signals for sim-vs-hardware diagnosis.
set probe_list {
    /tb_c128_boot/u_dut/fpga64_sid_iec_inst/baLoc
    /tb_c128_boot/u_dut/fpga64_sid_iec_inst/reset_t80
    /tb_c128_boot/u_dut/fpga64_sid_iec_inst/cpuBusAk_T80_n
    /tb_c128_boot/u_dut/fpga64_sid_iec_inst/cpuActT80
    /tb_c128_boot/u_dut/fpga64_sid_iec_inst/t80_cyc
    /tb_c128_boot/u_dut/fpga64_sid_iec_inst/reset
    /tb_c128_boot/u_dut/fpga64_sid_iec_inst/sysEnable
    /tb_c128_boot/u_dut/fpga64_sid_iec_inst/cpuAddr_T80
}
set probe_fh [open $log_file a]
foreach p $probe_list {
    if {[catch {set v [get_value $p]} err]} {
        set v "ERR:$err"
    }
    puts $probe_fh "{\"sessionId\":\"b576b7\",\"runId\":\"sim-boot-5\",\"hypothesisId\":\"H6\",\"location\":\"run_boot_sim.tcl:probe\",\"message\":\"xsim get_value\",\"data\":{\"path\":\"$p\",\"value\":\"$v\"},\"timestamp\":0}"
}
close $probe_fh

if {[file exists $log_file]} {
    puts "Debug NDJSON log: $log_file"
    set summary_line ""
    set fh [open $log_file r]
    while {[gets $fh line] >= 0} {
        puts $line
        if {[string match *simulation\ finished* $line]} {
            set summary_line $line
        }
    }
    close $fh

    set gate_ok 1
    if {$summary_line eq ""} {
        puts "ERROR: Boot sim summary line missing in $log_file"
        set gate_ok 0
    } elseif {$mem_bridge} {
        if {[string match *\"pass\":false* $summary_line]} {
            puts "PASS: rejected memory bridge correctly fails boot gate"
        } else {
            puts "ERROR: C_MEM_BRIDGE=true should fail pass gate (H-V23 regression)"
            set gate_ok 0
        }
    } elseif {[string match *\"pass\":false* $summary_line]} {
        puts "ERROR: Boot sim gate failed (pass=false)"
        set gate_ok 0
    } elseif {![string match *\"z80_ram_rd_mismatch\":0* $summary_line]} {
        if {![string match *\"z80_ram_rd_mismatch\"* $summary_line]} {
            puts "WARNING: z80_ram_rd_mismatch not found in summary"
        } else {
            puts "ERROR: Z80 RAM read mismatch detected during boot"
            set gate_ok 0
        }
    }

    if {!$gate_ok} {
        close_sim -force
        close_project
        exit 2
    }
} else {
    puts "WARNING: Debug log not written: $log_file"
}

close_sim -force
close_project
exit 0
