# Ultimate Command Interface

`uci.vhd` is ours. The other four files are Gideon Zweijtzer's, from
[1541ultimate](https://github.com/GideonZ/1541ultimate) at commit `4b00732`, GPLv3 like
this core. They are unchanged except for line endings (CRLF to LF):

| File | From |
|---|---|
| `command_protocol.vhd` | `fpga/io/command_interface/vhdl_source/` |
| `command_if_pkg.vhd` | `fpga/io/command_interface/vhdl_source/` |
| `io_bus_pkg.vhd` | `fpga/ip/busses/vhdl_source/` |
| `slot_bus_pkg.vhd` | `fpga/ip/busses/vhdl_source/` |

Keep them unchanged, so that a diff against his tree shows only his own changes.
`uci.vhd` replaces his `command_interface.vhd`, which needs his single-clock RAM and I/O bus
splitter.
