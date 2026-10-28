----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Writes what the RISC-V firmware reads from config.vhd and from globals.vhd (through the
-- sysinfo device) to a text file, one "device window index value" line per word, all in
-- hex. The firmware's host test (M2M/firmware/host) runs the Shell against it.
--
-- Run with CORE/scripts/run_config_nvc.sh, which writes config_dump.txt and
-- config_dump_r3.txt.
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

use work.globals.all;

entity tb_config_dump is
   generic (
      G_R3_MEMORY : boolean := false;
      G_OUT       : string  := "config_dump.txt"
   );
end entity tb_config_dump;

architecture sim of tb_config_dump is

   constant C_DEV_CONFIG   : natural := 16#0002#;
   constant C_DEV_SYS_INFO : natural := 16#00FF#;
   constant C_SYS_DRIVES   : natural := 16#0000#;
   constant C_SYS_CRTROMS  : natural := 16#0020#;

   type nat_array is array (natural range <>) of natural;
   -- Strings, read up to their zero, and data per menu item
   constant C_STRINGS  : nat_array := (16#0100#, 16#0101#, 16#0200#, 16#0300#, 16#0308#,
                                       16#030A#, 16#0312#);
   constant C_PER_ITEM : nat_array := (16#0301#, 16#0302#, 16#0303#, 16#0304#, 16#0306#,
                                       16#0307#, 16#0310#, 16#0311#);

   signal clk  : std_logic := '0';
   signal addr : std_logic_vector(27 downto 0) := (others => '0');
   signal data : std_logic_vector(15 downto 0);
   signal done : boolean := false;

begin

   clk <= not clk after 10 ns when not done;

   i_config : entity work.config
      generic map (G_R3_MEMORY => G_R3_MEMORY)
      port map (clk_i => clk, address_i => addr, data_o => data);

   p_dump : process
      file     f : text;
      variable l : line;
      variable v : natural;
      variable n : natural;

      procedure put(dev, sel, idx, val : natural) is
      begin
         write(l, to_hstring(to_unsigned(dev, 16)) & " " & to_hstring(to_unsigned(sel, 16)) & " " &
                  to_hstring(to_unsigned(idx, 12)) & " " & to_hstring(to_unsigned(val, 16)));
         writeline(f, l);
      end procedure put;

      -- config.vhd answers on the falling edge
      procedure rd(sel, idx : natural; val : out natural) is
      begin
         addr <= std_logic_vector(to_unsigned(sel, 16)) & std_logic_vector(to_unsigned(idx, 12));
         wait until falling_edge(clk);
         wait until rising_edge(clk);
         val := to_integer(unsigned(data));
      end procedure rd;

      procedure cfg(sel, idx : natural; val : out natural) is
      begin
         rd(sel, idx, val);
         put(C_DEV_CONFIG, sel, idx, val);
      end procedure cfg;

   begin
      file_open(f, G_OUT, write_mode);

      for i in 0 to 15 loop
         cfg(16#0110#, i, v);                             -- general settings
      end loop;
      cfg(16#0305#, 0, n);                                -- item count
      cfg(16#0309#, 0, v);                                -- menu size
      cfg(16#0309#, 1, v);
      for s in C_STRINGS'range loop
         for i in 0 to 4095 loop
            cfg(C_STRINGS(s), i, v);
            exit when v = 0;
         end loop;
      end loop;
      for s in C_PER_ITEM'range loop
         for i in 0 to n - 1 loop
            cfg(C_PER_ITEM(s), i, v);
         end loop;
      end loop;
      -- Help screens: records 1 to 15, page 0 and its page count
      for r in 1 to 15 loop
         cfg(16#1000# + r * 256, 16#FFF#, v);
         if v > 0 and v /= 16#EEEE# then
            for i in 0 to 4094 loop
               cfg(16#1000# + r * 256, i, v);
               exit when v = 0;
            end loop;
         end if;
      end loop;

      -- Sysinfo: globals.vhd's drives, cartridges and ROMs, as qnice_wrapper.vhd answers
      put(C_DEV_SYS_INFO, C_SYS_DRIVES, 0, C_VDNUM);
      put(C_DEV_SYS_INFO, C_SYS_DRIVES, 1, to_integer(unsigned(C_VD_DEVICE)));
      for i in C_VD_BUFFER'range loop
         put(C_DEV_SYS_INFO, C_SYS_DRIVES, 16#100# + i, to_integer(unsigned(C_VD_BUFFER(i))));
      end loop;
      put(C_DEV_SYS_INFO, C_SYS_CRTROMS, 0, C_CRTROMS_MAN_NUM);
      put(C_DEV_SYS_INFO, C_SYS_CRTROMS, 1, C_CRTROMS_AUTO_NUM);
      for i in C_CRTROMS_MAN'range loop
         put(C_DEV_SYS_INFO, C_SYS_CRTROMS, 16#100# + i, to_integer(unsigned(C_CRTROMS_MAN(i))));
      end loop;
      for i in C_CRTROMS_AUTO'range loop
         put(C_DEV_SYS_INFO, C_SYS_CRTROMS, 16#200# + i, to_integer(unsigned(C_CRTROMS_AUTO(i))));
      end loop;
      for i in C_CRTROMS_AUTO_NAMES'range loop
         put(C_DEV_SYS_INFO, C_SYS_CRTROMS, 16#300# + i - 1, character'pos(C_CRTROMS_AUTO_NAMES(i)));
      end loop;

      file_close(f);
      report "CONFIG DUMP WRITTEN";
      done <= true;
      wait;
   end process p_dump;

end architecture sim;
