# Adds the RISC-V system CPU (M2M/vhdl/rv) to sources_1; NEORV32 itself comes with the
# modem's sources.
# Sourced by build_bitstream.tcl and rtl_elab_check.tcl.

proc add_rv_sources {repo_dir} {
    foreach f [list \
        [file join $repo_dir M2M/vhdl/rv/rv_hr_bridge.vhd] \
        [file join $repo_dir M2M/vhdl/rv/rv_soc.vhd] \
    ] {
        if {[llength [get_files -quiet $f]] == 0} {
            puts "Adding missing source file: $f"
            add_files -norecurse -fileset [get_filesets sources_1] $f
        }
        set_property file_type {VHDL 2008} [get_files $f]
    }
}
