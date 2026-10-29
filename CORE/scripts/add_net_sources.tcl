# Adds the network device's sources to sources_1: CORE/vhdl/net (the SwiftLink's 6551 and
# the device-bus registers) and the Ethernet MAC from C64MEGA65.
# Sourced by build_bitstream.tcl and rtl_elab_check.tcl.

proc add_net_sources {repo_dir} {
    foreach f [list \
        [file join $repo_dir M2M/vhdl/axis_fifo_async.vhd] \
        [file join $repo_dir M2M/vhdl/axis_dropper.vhd] \
        [file join $repo_dir M2M/vhdl/controllers/ethernet/eth_rmii.vhd] \
        [file join $repo_dir M2M/vhdl/controllers/ethernet/eth_wrapper.vhd] \
        [file join $repo_dir CORE/vhdl/net/acia6551.vhd] \
        [file join $repo_dir CORE/vhdl/net/net.vhd] \
    ] {
        if {[llength [get_files -quiet $f]] == 0} {
            puts "Adding missing source file: $f"
            add_files -norecurse -fileset [get_filesets sources_1] $f
        }
        set_property file_type {VHDL 2008} [get_files $f]
    }
}
