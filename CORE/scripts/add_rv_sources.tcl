# Adds the RISC-V system CPU to sources_1: NEORV32 (in library neorv32, with a stub in place
# of its own boot ROM's image) and M2M/vhdl/rv.
# Sourced by build_bitstream.tcl and rtl_elab_check.tcl.

proc add_rv_sources {repo_dir} {
    set neo_dir   [file join $repo_dir CORE/vhdl/neorv32]
    set neo_files [list [file join $repo_dir M2M/vhdl/rv/neorv32_bootrom_image.vhd]]
    set fh [open [file join $neo_dir file_list.txt] r]
    foreach name [split [string trim [read $fh]] "\n"] {
        lappend neo_files [file join $neo_dir [string trim $name]]
    }
    close $fh

    set work_files [list \
        [file join $repo_dir M2M/vhdl/rv/rv_hr_bridge.vhd] \
        [file join $repo_dir M2M/vhdl/rv/rv_soc.vhd] \
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
