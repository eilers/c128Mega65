# Run the internal floppy's WD1772 front end simulation (tb_c1581_physical).
#
# C64MEGA65's bench for the physical-mode branch of c1581_fdc1772: a mock
# physical_1581_controller and the real FIFO geometry, driven with the 1581
# ROM's own polling loops. It covers reads, Read Address, writes, Write Track,
# write protect, LOST DATA, Force Interrupt and the stale-done tag checks.
#
# The bench brings its own floppy stub, so it is compiled on its own instead of
# through the project (floppy.v would be a second module of that name).
#
# Usage:
#   vivado -mode batch -source CORE/scripts/run_c1581_physical_sim.tcl \
#     -tclargs CORE/CORE-R6-vivado2022.xpr

set project_file [lindex $argv 0]
if {$project_file eq ""} {
    set project_file [file normalize [file join [file dirname [info script]] .. CORE-R6-vivado2022.xpr]]
}

set core_dir [file normalize [file join [file dirname [info script]] ..]]
set drv_dir  [file join $core_dir C128_MiSTer rtl iec_drive]
set work_dir [file join [file dirname [file normalize $project_file]] \
    "[file rootname [file tail $project_file]].sim" tb_c1581_physical]
file mkdir $work_dir
cd $work_dir

set ok 1
# --relax as in project simulations: the fdc uses some names before declaring them
if {[catch {exec xvlog -sv --relax \
        [file join $core_dir sim tb_c1581_physical.sv] \
        [file join $drv_dir c1581_fdc1772.v] \
        [file join $drv_dir iecdrv_misc.sv] > xvlog.log 2>@1}]} {
    puts "ERROR: xvlog failed, see [file join $work_dir xvlog.log]"
    set ok 0
}
if {$ok && [catch {exec xelab --debug off --relax tb_c1581_physical -s tb_c1581_physical_snap \
        > xelab.log 2>@1}]} {
    puts "ERROR: xelab failed, see [file join $work_dir xelab.log]"
    set ok 0
}
if {$ok} {
    catch {exec xsim tb_c1581_physical_snap -R > xsim.log 2>@1}
    set ok 0
    set fh [open xsim.log r]
    while {[gets $fh line] >= 0} {
        if {[string match {*FAIL*} $line] || [string match {*====*} $line]} {
            puts $line
        }
        if {[string match {*==== PASS*} $line]} {
            set ok 1
        }
    }
    close $fh
}

if {!$ok} {
    puts "ERROR: tb_c1581_physical failed"
    exit 2
}
exit 0
