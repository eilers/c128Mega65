# Run the internal floppy track-read simulation (tb_c1581_track).
#
# Our c1581_fdc1772 in physical mode, the real physical_1581_controller and
# C64MEGA65's rotating disk model: a DOS-style read of one track side must come
# in byte-exact and at real 1581 speed (about one revolution for ten sectors).
# It simulates about 0.7 s of drive time.
#
# Usage:
#   vivado -mode batch -source CORE/scripts/run_c1581_track_sim.tcl \
#     -tclargs CORE/CORE-R6-vivado2022.xpr [G_SECTORS]

set project_file [lindex $argv 0]
if {$project_file eq ""} {
    set project_file [file normalize [file join [file dirname [info script]] .. CORE-R6-vivado2022.xpr]]
}
set sectors [lindex $argv 1]
if {$sectors eq ""} {
    set sectors 10
}

set core_dir [file normalize [file join [file dirname [info script]] ..]]
set drv_dir  [file join $core_dir C128_MiSTer rtl iec_drive]
set p_dir    [file join $core_dir vhdl physical_1581]
set s_dir    [file join $core_dir sim physical_1581]
set work_dir [file join [file dirname [file normalize $project_file]] \
    "[file rootname [file tail $project_file]].sim" tb_c1581_track]
file mkdir $work_dir
cd $work_dir

set vhdl_files {}
foreach f {pkg crc inputs mfm_gaps mfm_quantise mfm_gaps_to_bits mfm_bits_to_bytes \
           mfm_decoder mfm_encoder writer wfifo rdfifo controller} {
    lappend vhdl_files [file join $p_dir physical_1581_$f.vhd]
}
foreach f {mfm_flux_gen_pkg golden_1581_pkg mech_model_1581} {
    lappend vhdl_files [file join $s_dir $f.vhd]
}
lappend vhdl_files [file join $core_dir sim tb_c1581_track.vhd]

# The Verilog first: the VHDL bench instantiates c1581_fdc1772.
# --relax as in project simulations: the fdc uses some names before declaring them
set ok 1
if {[catch {exec xvlog -sv --relax \
        [file join $drv_dir c1581_fdc1772.v] \
        [file join $drv_dir floppy.v] \
        [file join $drv_dir iecdrv_misc.sv] > xvlog.log 2>@1}]} {
    puts "ERROR: xvlog failed, see [file join $work_dir xvlog.log]"
    set ok 0
}
if {$ok && [catch {exec xvhdl --2008 {*}$vhdl_files > xvhdl.log 2>@1}]} {
    puts "ERROR: xvhdl failed, see [file join $work_dir xvhdl.log]"
    set ok 0
}
if {$ok && [catch {exec xelab --debug off --relax -generic_top "G_SECTORS=$sectors" \
        tb_c1581_track -s tb_c1581_track_snap > xelab.log 2>@1}]} {
    puts "ERROR: xelab failed, see [file join $work_dir xelab.log]"
    set ok 0
}
if {$ok} {
    catch {exec xsim tb_c1581_track_snap -R > xsim.log 2>@1}
    set ok 0
    set fh [open xsim.log r]
    while {[gets $fh line] >= 0} {
        if {[string match {*Note:*} $line] || [string match {*Error:*} $line] \
            || [string match {*Failure:*} $line] || [string match {*====*} $line]} {
            puts $line
        }
        if {[string match {*==== tb_c1581_track: PASS*} $line]} {
            set ok 1
        }
    }
    close $fh
}

if {!$ok} {
    puts "ERROR: tb_c1581_track failed"
    exit 2
}
exit 0
