-- tb_input_inject: the REST API's input device (input_inject.vhd). The CPU writes words 0-11,
-- nothing reaches the C128 until the commit, then every field arrives where main.vhd takes it;
-- the words read back; commits back to back never show the C128 a mix of two (the clocks are
-- unrelated, as on the board); the debug register goes both ways; resets clear it all.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.input_inject_pkg.all;

entity tb_input_inject is
end entity;

architecture sim of tb_input_inject is
   signal mclk, qclk   : std_logic := '0';
   signal running      : boolean := true;
   signal mrst, qrst   : std_logic := '0';
   signal inp          : t_api_input;
   signal dbg_we       : std_logic := '0';
   signal dbg_wdata    : std_logic_vector(7 downto 0) := x"00";
   signal dbg          : std_logic_vector(7 downto 0);
   signal addr         : std_logic_vector(11 downto 0) := (others => '0');
   signal wdata        : std_logic_vector(15 downto 0) := (others => '0');
   signal rdata        : std_logic_vector(15 downto 0);
   signal ce, we       : std_logic := '0';
   signal mixed        : natural := 0;    -- C128 clocks that saw words of two commits
   signal watch        : boolean := false;
begin
   mclk <= not mclk after 15.86 ns when running;   -- 31.528 MHz
   qclk <= not qclk after 10 ns when running;      -- 50 MHz

   dut : entity work.input_inject
      port map (
         main_clk_i => mclk, main_rst_i => mrst, main_input_o => inp,
         main_dbg_we_i => dbg_we, main_dbg_data_i => dbg_wdata, main_dbg_o => dbg,
         qnice_clk_i => qclk, qnice_rst_i => qrst, qnice_addr_i => addr, qnice_data_i => wdata,
         qnice_ce_i => ce, qnice_we_i => we, qnice_data_o => rdata);

   -- In the stress phase every word holds the same byte: any two that differ are a mix
   p_watch : process (mclk)
      variable b : std_logic_vector(7 downto 0);
   begin
      if rising_edge(mclk) and watch then
         b := inp.keys(7 downto 0);
         for i in 1 to 10 loop
            if inp.keys(i * 8 + 7 downto i * 8) /= b then
               mixed <= mixed + 1;
            end if;
         end loop;
         if inp.pot2_y /= b or inp.pot1_x /= b then
            mixed <= mixed + 1;
         end if;
      end if;
   end process;

   test : process
      variable fails, checks : natural := 0;
      variable v : std_logic_vector(15 downto 0);

      -- A device bus access as the CPU makes it: two falling edges with ce, then a gap
      procedure wr(a : natural; d : std_logic_vector(15 downto 0)) is
      begin
         wait until rising_edge(qclk);
         addr  <= std_logic_vector(to_unsigned(a, 12));
         wdata <= d;
         ce    <= '1';
         we    <= '1';
         wait until falling_edge(qclk);
         wait until falling_edge(qclk);
         wait until rising_edge(qclk);
         ce <= '0';
         we <= '0';
      end procedure;

      procedure rd(a : natural; d : out std_logic_vector(15 downto 0)) is
      begin
         wait until rising_edge(qclk);
         addr <= std_logic_vector(to_unsigned(a, 12));
         ce   <= '1';
         wait until falling_edge(qclk);
         d := rdata;
         wait until rising_edge(qclk);
         ce <= '0';
      end procedure;

      procedure check(name : string; ok : boolean) is
      begin
         checks := checks + 1;
         if not ok then
            fails := fails + 1;
            report name & ": FAIL" severity warning;
         end if;
      end procedure;

      -- The firmware's commit: wait for the last one, then commit
      procedure commit is
         variable s : std_logic_vector(15 downto 0);
      begin
         loop
            rd(15, s);
            exit when s(0) = '0';
         end loop;
         wr(15, x"0001");
      end procedure;

      procedure settle is
         variable s : std_logic_vector(15 downto 0);
      begin
         loop
            rd(15, s);
            exit when s(0) = '0';
         end loop;
      end procedure;

      type t_vals is array (0 to 11) of std_logic_vector(15 downto 0);
      constant VALS : t_vals := (x"0201", x"0804", x"2010", x"8040", x"A55A", x"00C3",
                                 x"0007", x"0015", x"000A", x"0003", x"7F12", x"80FE");
   begin
      wait for 100 ns;

      -- Written, not committed: the C128 sees nothing
      for i in 0 to 11 loop
         wr(i, VALS(i));
      end loop;
      wait for 500 ns;
      check("nothing before the commit", inp = C_API_INPUT_NONE);
      for i in 0 to 11 loop
         rd(i, v);
         check("word " & integer'image(i) & " reads back", v = VALS(i));
      end loop;

      -- The commit: pending, then every field where main.vhd takes it
      wr(15, x"0001");
      rd(15, v);
      check("commit pending", v(0) = '1');
      settle;
      check("keys row 0 (PA0)", inp.keys(7 downto 0) = x"01");
      check("keys row 1", inp.keys(15 downto 8) = x"02");
      check("keys row 7", inp.keys(63 downto 56) = x"80");
      check("K0", inp.keys(71 downto 64) = x"5A");
      check("K1", inp.keys(79 downto 72) = x"A5");
      check("K2", inp.keys(87 downto 80) = x"C3");
      check("RESTORE, 40/80, Caps Lock", inp.restore = '1' and inp.d4080 = '1' and inp.capslock = '1');
      check("port 1: up, left, fire", inp.joy1 = "10101");
      check("port 2: down, right", inp.joy2 = "01010");
      check("POT enables", inp.pot1_en = '1' and inp.pot2_en = '1');
      check("port 1 POTs", inp.pot1_x = x"12" and inp.pot1_y = x"7F");
      check("port 2 POTs", inp.pot2_x = x"FE" and inp.pot2_y = x"80");

      -- A change waits for its commit too
      wr(6, x"0000");
      wait for 500 ns;
      check("RESTORE held until the commit", inp.restore = '1');
      commit;
      settle;
      check("RESTORE released at the commit", inp.restore = '0' and inp.d4080 = '0');

      -- Commits back to back, every word the same byte: never a mix
      for n in 0 to 60 loop
         v := std_logic_vector(to_unsigned(n * 37 mod 256, 8)) & std_logic_vector(to_unsigned(n * 37 mod 256, 8));
         for i in 0 to 5 loop
            wr(i, v);
         end loop;
         wr(10, v);
         wr(11, v);
         commit;
         if n = 0 then          -- watch from the first uniform commit on
            settle;
            watch <= true;
         end if;
      end loop;
      settle;
      wait for 200 ns;
      watch <= false;
      wait for 50 ns;
      check("60 commits, never a mix (" & integer'image(mixed) & " clocks mixed)", mixed = 0);
      check("the last commit arrived", inp.keys(7 downto 0) = std_logic_vector(to_unsigned(60 * 37 mod 256, 8)));

      -- Debug register: the firmware's write, read back once it is done
      wr(12, x"005A");
      rd(12, v);
      check("debug write pending", v(8) = '1');
      loop
         rd(12, v);
         exit when v(8) = '0';
      end loop;
      check("debug register reads back 5A", v(7 downto 0) = x"5A");
      check("the C128 reads 5A", dbg = x"5A");

      -- The C128's write, as main.vhd strobes it
      wait until rising_edge(mclk);
      dbg_wdata <= x"A5";
      dbg_we    <= '1';
      wait until rising_edge(mclk);
      dbg_we    <= '0';
      wait for 200 ns;
      rd(12, v);
      check("the firmware reads the C128's A5", v = x"00A5");

      -- Resets: the core's clears what the C128 sees, the CPU's the words
      wait until rising_edge(mclk);
      mrst <= '1';
      wait until rising_edge(mclk);
      mrst <= '0';
      wait for 1 ns;
      check("core reset clears the C128's side", inp = C_API_INPUT_NONE and dbg = x"00");
      wait until rising_edge(qclk);
      qrst <= '1';
      wait until falling_edge(qclk);
      wait until rising_edge(qclk);
      qrst <= '0';
      rd(0, v);
      check("CPU reset clears the words", v = x"0000");

      if fails = 0 then
         report "ALL CHECKS PASSED (" & integer'image(checks) & ")";
      else
         report integer'image(fails) & " of " & integer'image(checks) & " checks FAILED" severity error;
      end if;
      running <= false;
      wait;
   end process;
end architecture;
