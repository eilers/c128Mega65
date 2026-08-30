library ieee;
use ieee.std_logic_1164.all;

library work;
use work.vdrives_pkg.all;

entity tb_vdrive_index is
end entity;

architecture sim of tb_vdrive_index is
   -- SystemVerilog unpacked arrays are [0:DRIVES-1], so the mixed-language side
   -- must be ascending. vdrives itself is descending and is bridged by index.
   signal sv_in  : vd_vec_array(0 to 1)(31 downto 0);
   signal sv_out : vd_vec_array(0 to 1)(31 downto 0);
   signal vd_in  : vd_vec_array(1 downto 0)(31 downto 0);
begin
   sv_in(0) <= x"01234567";
   sv_in(1) <= x"89ABCDEF";

   probe : entity work.vdrive_array_probe
      generic map (DRIVES => 2)
      port map (
         from_vhdl => sv_in,
         to_vhdl   => sv_out
      );

   copy_by_index : for i in 0 to 1 generate
      vd_in(i) <= sv_out(i);
   end generate;

   check : process
   begin
      wait for 1 ns;
      assert sv_out(0) = x"10324567"
         report "FAIL: SystemVerilog drive 0 was positionally reversed" severity failure;
      assert sv_out(1) = x"98BACDEE"
         report "FAIL: SystemVerilog drive 1 was positionally reversed" severity failure;
      assert vd_in(0) = sv_out(0) and vd_in(1) = sv_out(1)
         report "FAIL: descending vdrives bridge changed an index" severity failure;
      report "ALL CHECKS PASSED: mixed-language drive indices remain 0->0 and 1->1";
      wait;
   end process;
end architecture;
