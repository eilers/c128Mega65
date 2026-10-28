# Adds the SwiftLink modem's sources to sources_1: CORE/vhdl/modem, the Ethernet MAC
# from C64MEGA65 and NEORV32 (in library neorv32, with our boot ROM image in place of
# its bootloader). Sourced by build_bitstream.tcl and rtl_elab_check.tcl.

proc add_modem_sources {repo_dir} {
    set neo_dir   [file join $repo_dir CORE/vhdl/neorv32]
    set neo_files [list [file join $repo_dir CORE/vhdl/modem/neorv32_bootrom_image.vhd]]
    set fh [open [file join $neo_dir file_list.txt] r]
    foreach name [split [string trim [read $fh]] "\n"] {
        lappend neo_files [file join $neo_dir [string trim $name]]
    }
    close $fh

    set work_files [list \
        [file join $repo_dir M2M/vhdl/axis_fifo_async.vhd] \
        [file join $repo_dir M2M/vhdl/axis_dropper.vhd] \
        [file join $repo_dir M2M/vhdl/controllers/ethernet/eth_rmii.vhd] \
        [file join $repo_dir M2M/vhdl/controllers/ethernet/eth_wrapper.vhd] \
        [file join $repo_dir CORE/vhdl/modem/acia6551.vhd] \
        [file join $repo_dir CORE/vhdl/modem/modem_hr_bridge.vhd] \
        [file join $repo_dir CORE/vhdl/modem/modem.vhd] \
    ]

    foreach f [concat $neo_files $work_files] {
        if {[llength [get_files -quiet $f]] == 0} {
            puts "Adding missing source file: $f"
            add_files -norecurse -fileset [get_filesets sources_1] $f
        }
        set_property file_type {VHDL 2008} [get_files $f]
    }
    foreach f $neo_files {
        set_property library neorv32 [get_files $f]
    }
}
