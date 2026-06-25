# Starter script: mark boot probe nets for Vivado ILA / Hardware Manager.
# Run in Vivado Tcl console AFTER opening the implemented design (or before synth with mark_debug).
#
# Usage (Hardware Manager):
#   open_hw_manager
#   connect_hw_server
#   open_hw_target
#   source CORE/scripts/hw_ila_boot_probe.tcl
#
# Note: ILA requires a debug-enabled bitstream. Do not use for production release builds.

set probe_nets {
   /mega65_r6/CORE/i_main/boot_stage
   /mega65_r6/CORE/i_main/core_z80_n
   /mega65_r6/CORE/i_main/fpga64_sid_iec_inst/reset_t80
   /mega65_r6/CORE/i_main/ram_ce
   /mega65_r6/CORE/i_main/ram_we
   /mega65_r6/CORE/i_main/ram_addr_o
   /mega65_r6/CORE/i_main/core_ram_addr
   /mega65_r6/CORE/i_main/dbg_vec_byte_valid
   /mega65_r6/CORE/i_main/dbg_vec_byte
}

foreach n $probe_nets {
   set cells [get_cells -quiet -hierarchical -filter "NAME =~ *${n}*"]
   if {[llength $cells] == 0} {
      set ports [get_nets -quiet -hierarchical $n]
      if {[llength $ports] > 0} {
         puts "MARK_DEBUG net: $n"
         catch {set_property MARK_DEBUG true $ports}
      } else {
         puts "WARNING: probe not found: $n"
      }
   } else {
      puts "MARK_DEBUG cell: $cells"
      catch {set_property MARK_DEBUG true $cells}
   }
}

puts "Done. Re-run synthesis/implementation with debug, or insert ILA IP on marked nets."
