----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Testbench for config.vhd: R3-memory builds (G_R3_MEMORY) leave the 2 MB and 16 MB REU items
-- out of the menu. Reads both menus the way the Shell does and checks that the R3 one is the full
-- one without those items, in every selector, and that it has its own settings file.
--
-- Run with CORE/scripts/run_config_nvc.sh
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.globals.all;

entity tb_config is
end entity tb_config;

architecture sim of tb_config is

   constant SEL_CFG_FILE   : natural := 16#0101#;
   constant SEL_OPTM_ITEMS : natural := 16#0300#;
   constant SEL_ICOUNT     : natural := 16#0305#;
   -- every per-item selector: groups, stdsel, lines, start, mount, singlesel, help, crtrom
   type nat_array is array (natural range <>) of natural;
   constant SEL_PER_ITEM   : nat_array := (16#0301#, 16#0302#, 16#0303#, 16#0304#,
                                           16#0306#, 16#0307#, 16#0310#, 16#0311#);

   type addr_array is array (0 to 1) of std_logic_vector(27 downto 0);
   type data_array is array (0 to 1) of std_logic_vector(15 downto 0);

   signal clk  : std_logic := '0';
   signal addr : addr_array := (others => (others => '0'));
   signal data : data_array;

   constant FULL : natural := 0;
   constant R3   : natural := 1;

begin

   clk <= not clk after 10 ns;

   i_full : entity work.config
      generic map (G_R3_MEMORY => false)
      port map (clk_i => clk, address_i => addr(FULL), data_o => data(FULL));

   i_r3 : entity work.config
      generic map (G_R3_MEMORY => true)
      port map (clk_i => clk, address_i => addr(R3), data_o => data(R3));

   p_test : process
      variable errors : natural := 0;

      -- config.vhd answers on the falling edge
      procedure rd(m : natural; sel : natural; idx : natural; v : out natural) is
      begin
         addr(m) <= std_logic_vector(to_unsigned(sel, 16)) & std_logic_vector(to_unsigned(idx, 12));
         wait until falling_edge(clk);
         wait until rising_edge(clk);
         v := to_integer(unsigned(data(m)));
      end procedure rd;

      -- the zero-terminated string at a selector
      procedure rd_str(m : natural; sel : natural; s : out string; len : out natural) is
         variable c : natural;
      begin
         len := 0;
         for i in 0 to s'length - 1 loop
            rd(m, sel, i, c);
            exit when c = 0;
            s(s'low + i) := character'val(c);
            len := i + 1;
         end loop;
      end procedure rd_str;

      -- item n of a menu: the text of line n of OPTM_ITEMS, without its \n
      procedure item(s : string; len : natural; n : natural; first, last : out natural) is
         variable count : natural := 0;
         variable start : natural := s'low;
      begin
         first := 1; last := 0;
         for i in s'low to s'low + len - 2 loop
            if s(i) = '\' and s(i + 1) = 'n' then
               if count = n then
                  first := start; last := i - 1;
                  return;
               end if;
               count := count + 1;
               start := i + 2;
            end if;
         end loop;
         report "item " & integer'image(n) & " not found" severity failure;
      end procedure item;

      procedure check(ok : boolean; msg : string) is
      begin
         if not ok then
            report "FAIL: " & msg severity error;
            errors := errors + 1;
         end if;
      end procedure check;

      variable items_f, items_r : string(1 to 4096);
      variable len_f, len_r     : natural;
      variable n_f, n_r         : natural;
      variable lines_f, lines_r : natural := 0;
      variable f1, f2, r1, r2   : natural;
      variable vf, vr           : natural;
      variable j                : natural;
      variable cfg_f, cfg_r     : string(1 to 64);
      variable cl_f, cl_r       : natural;

   begin
      wait until rising_edge(clk);

      rd(FULL, SEL_ICOUNT, 0, n_f);
      rd(R3,   SEL_ICOUNT, 0, n_r);
      check(n_r = n_f - C_MENU_R3_DROP, "R3 menu has " & integer'image(n_r) & " items, full " & integer'image(n_f));

      rd_str(FULL, SEL_OPTM_ITEMS, items_f, len_f);
      rd_str(R3,   SEL_OPTM_ITEMS, items_r, len_r);
      for i in 1 to len_f - 1 loop
         if items_f(i to i + 1) = "\n" then lines_f := lines_f + 1; end if;
      end loop;
      for i in 1 to len_r - 1 loop
         if items_r(i to i + 1) = "\n" then lines_r := lines_r + 1; end if;
      end loop;
      check(lines_f = n_f, "full menu has " & integer'image(lines_f) & " lines");
      check(lines_r = n_r, "R3 menu has " & integer'image(lines_r) & " lines");

      item(items_f, len_f, C_MENU_REU_2M, f1, f2);
      check(items_f(f1 to f2) = " 2 MB", "full item C_MENU_REU_2M is """ & items_f(f1 to f2) & """");
      item(items_f, len_f, C_MENU_REU_16M, f1, f2);
      check(items_f(f1 to f2) = " 16 MB", "full item C_MENU_REU_16M is """ & items_f(f1 to f2) & """");
      item(items_f, len_f, C_MENU_CART_CRT, f1, f2);
      check(items_f(f1 to f2) = " CRT file", "full item C_MENU_CART_CRT is """ & items_f(f1 to f2) & """");

      -- R3 item i is full item i, or i + 2 from the 2 MB item on, in text and in every selector
      for i in 0 to n_r - 1 loop
         if i < C_MENU_REU_2M then j := i; else j := i + C_MENU_R3_DROP; end if;
         item(items_f, len_f, j, f1, f2);
         item(items_r, len_r, i, r1, r2);
         check(items_f(f1 to f2) = items_r(r1 to r2),
               "R3 item " & integer'image(i) & " """ & items_r(r1 to r2) & """ is not full item " &
               integer'image(j) & " """ & items_f(f1 to f2) & """");
         for s in SEL_PER_ITEM'range loop
            rd(FULL, SEL_PER_ITEM(s), j, vf);
            rd(R3,   SEL_PER_ITEM(s), i, vr);
            check(vf = vr, "selector " & integer'image(SEL_PER_ITEM(s)) & ", R3 item " & integer'image(i));
         end loop;
      end loop;

      rd_str(FULL, SEL_CFG_FILE, cfg_f, cl_f);
      rd_str(R3,   SEL_CFG_FILE, cfg_r, cl_r);
      report "settings files: " & cfg_f(1 to cl_f) & ", " & cfg_r(1 to cl_r);
      check(cfg_r(1 to cl_r) = cfg_f(1 to cl_f - 4) & "-r3.cfg", "R3 settings file " & cfg_r(1 to cl_r));

      if errors = 0 then
         report "ALL CHECKS PASSED";
      else
         report integer'image(errors) & " CHECKS FAILED" severity error;
      end if;
      std.env.stop;
   end process p_test;

end architecture sim;
