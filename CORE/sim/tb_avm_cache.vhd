----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- tb_avm_cache: avm_cache.vhd's inval_i, against an Avalon memory that another master
-- changes behind the cache (as the system CPU and reu_sd.vhd change the REU's memory).
--
-- The memory answers reads in order after a few clocks with the data as it was when it
-- took the read. Checks, each first without a drop (the stale word, as UCIREU saw it on
-- the hardware) and then with one: a line read again after a change; a read in the clock
-- of the drop; a drop that comes while the line's fill is under way, before its first word
-- and after it (a hit on the words already in); a drop during the prefetch of the line's
-- second half.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_avm_cache is
end entity tb_avm_cache;

architecture sim of tb_avm_cache is

   constant C_CLK   : time := 10 ns;
   constant C_WORDS : natural := 4096;

   signal clk       : std_logic := '0';
   signal rst       : std_logic := '1';
   signal running   : boolean := true;
   signal inval     : std_logic := '0';

   signal s_wait    : std_logic;
   signal s_write   : std_logic := '0';
   signal s_read    : std_logic := '0';
   signal s_addr    : std_logic_vector(31 downto 0) := (others => '0');
   signal s_wdata   : std_logic_vector(15 downto 0) := (others => '0');
   signal s_rdata   : std_logic_vector(15 downto 0);
   signal s_rvalid  : std_logic;

   signal m_wait    : std_logic := '0';
   signal m_write   : std_logic;
   signal m_read    : std_logic;
   signal m_addr    : std_logic_vector(31 downto 0);
   signal m_wdata   : std_logic_vector(15 downto 0);
   signal m_be      : std_logic_vector(1 downto 0);
   signal m_burst   : std_logic_vector(7 downto 0);
   signal m_rdata   : std_logic_vector(15 downto 0) := (others => '0');
   signal m_rvalid  : std_logic := '0';

   -- the other master: add `gen` to words chg_lo .. chg_lo + 15 when chg_go toggles
   signal chg_go    : boolean := false;
   signal chg_lo    : natural := 0;
   signal gen       : natural := 0;
   signal in_flight : natural := 0;          -- read words taken, not yet answered
   signal latency   : natural := 4;

   function word0(a, g : natural) return std_logic_vector is
   begin
      return std_logic_vector(to_unsigned((a * 3 + g * 1000) mod 65536, 16));
   end function word0;

begin

   clk <= not clk after C_CLK / 2 when running;

   i_dut : entity work.avm_cache
      generic map (
         G_CACHE_SIZE   => 8,
         G_ADDRESS_SIZE => 32,
         G_DATA_SIZE    => 16
      )
      port map (
         clk_i                 => clk,
         rst_i                 => rst,
         inval_i               => inval,
         s_avm_waitrequest_o   => s_wait,
         s_avm_write_i         => s_write,
         s_avm_read_i          => s_read,
         s_avm_address_i       => s_addr,
         s_avm_writedata_i     => s_wdata,
         s_avm_byteenable_i    => "11",
         s_avm_burstcount_i    => x"01",
         s_avm_readdata_o      => s_rdata,
         s_avm_readdatavalid_o => s_rvalid,
         m_avm_waitrequest_i   => m_wait,
         m_avm_write_o         => m_write,
         m_avm_read_o          => m_read,
         m_avm_address_o       => m_addr,
         m_avm_writedata_o     => m_wdata,
         m_avm_byteenable_o    => m_be,
         m_avm_burstcount_o    => m_burst,
         m_avm_readdata_i      => m_rdata,
         m_avm_readdatavalid_i => m_rvalid
      );

   -- memory: word a holds word0(a, generation of its 16-word block); a read takes its data
   -- when the memory takes it and answers `latency` clocks later, a word a clock
   p_mem : process (clk)
      type t_gen is array (0 to C_WORDS / 16 - 1) of natural;
      variable g            : t_gen := (others => 0);
      type t_q is array (0 to 63) of std_logic_vector(15 downto 0);
      variable q_data       : t_q;
      type t_t is array (0 to 63) of natural;
      variable q_due        : t_t;
      variable q_head, q_n  : natural := 0;
      variable t            : natural := 0;
      variable seen         : boolean := false;
   begin
      if rising_edge(clk) then
         t := t + 1;
         if chg_go /= seen then
            seen := chg_go;
            g(chg_lo / 16) := gen;
         end if;
         m_rvalid <= '0';
         if q_n > 0 and q_due(q_head) <= t then
            m_rdata  <= q_data(q_head);
            m_rvalid <= '1';
            q_head   := (q_head + 1) mod 64;
            q_n      := q_n - 1;
         end if;
         if m_read = '1' and m_wait = '0' then
            for k in 0 to to_integer(unsigned(m_burst)) - 1 loop
               q_data((q_head + q_n) mod 64) := word0(to_integer(unsigned(m_addr)) + k,
                                                      g((to_integer(unsigned(m_addr)) + k) / 16));
               q_due((q_head + q_n) mod 64)  := t + latency + k;
               q_n := q_n + 1;
            end loop;
         end if;
         in_flight <= q_n;
      end if;
   end process p_mem;

   p_test : process
      variable errors : natural := 0;
      variable v      : std_logic_vector(15 downto 0);

      procedure read(a : natural; d : out std_logic_vector(15 downto 0)) is
      begin
         s_addr <= std_logic_vector(to_unsigned(a, 32));
         s_read <= '1';
         loop
            wait until rising_edge(clk);
            exit when s_wait = '0';
         end loop;
         s_read <= '0';
         while s_rvalid /= '1' loop
            wait until rising_edge(clk);
         end loop;
         d := s_rdata;
         wait until rising_edge(clk);
      end procedure read;

      -- the other master changes block a / 16
      procedure change(a : natural) is
      begin
         chg_lo <= a;
         gen    <= gen + 1;
         chg_go <= not chg_go;
         wait until rising_edge(clk);
         wait until rising_edge(clk);
      end procedure change;

      procedure drop is
      begin
         inval <= '1';
         wait until rising_edge(clk);
         inval <= '0';
      end procedure drop;

      procedure expect(a : natural; d : std_logic_vector(15 downto 0); g : natural; ok_msg, bad_msg : string) is
      begin
         if d = word0(a, g) then
            report ok_msg;
         else
            report bad_msg & ": word " & integer'image(a) & " is " & integer'image(to_integer(unsigned(d))) &
                   ", expected " & integer'image(to_integer(unsigned(word0(a, g)))) severity error;
            errors := errors + 1;
         end if;
      end procedure expect;

      -- a read that starts a fill, then the change and the drop while it runs
      procedure change_during_fill(a : natural; dropping : boolean) is
      begin
         s_addr <= std_logic_vector(to_unsigned(a, 32));
         s_read <= '1';
         loop
            wait until rising_edge(clk);
            exit when s_wait = '0';
         end loop;
         s_read <= '0';
         wait until rising_edge(clk);
         wait for 1 ns;
         assert in_flight > 0 report "no fill under way" severity failure;
         change(a);                              -- the fill has the old words
         if dropping then
            drop;
         end if;
         while s_rvalid /= '1' loop
            wait until rising_edge(clk);
         end loop;
         wait until rising_edge(clk);
         while in_flight > 0 loop
            wait until rising_edge(clk);
         end loop;
         wait for 5 * C_CLK;
      end procedure change_during_fill;

   begin
      wait for 5 * C_CLK;
      wait until rising_edge(clk);
      rst <= '0';
      wait for 5 * C_CLK;

      -- 1. the line read again after a change
      read(96, v);
      change(96);
      read(97, v);
      expect(97, v, 0, "1 without a drop: the stale word, as on the hardware",
             "1 without a drop: not the stale word (the test misses the bug)");
      drop;
      read(97, v);
      expect(97, v, gen, "1 dropped: the new word", "1 dropped");

      -- 1b. a read in the very clock of the drop
      read(160, v);
      change(160);
      s_addr <= std_logic_vector(to_unsigned(161, 32));
      s_read <= '1';
      inval  <= '1';
      loop
         wait until rising_edge(clk);
         inval <= '0';
         exit when s_wait = '0';
      end loop;
      s_read <= '0';
      while s_rvalid /= '1' loop
         wait until rising_edge(clk);
      end loop;
      v := s_rdata;
      wait until rising_edge(clk);
      expect(161, v, gen, "1b read with the drop: the new word", "1b read with the drop");

      -- 2. a drop while the line's fill is under way: the fill had the old words
      latency <= 12;
      change_during_fill(256, false);
      read(259, v);
      expect(259, v, 0, "2 without a drop: the stale word",
             "2 without a drop: not the stale word (the test misses the bug)");
      change_during_fill(512, true);
      read(515, v);
      expect(515, v, gen, "2 dropped during the fill: the new word", "2 dropped during the fill");

      -- 2b. the drop after the fill's first word: the words already in are old too
      s_addr <= std_logic_vector(to_unsigned(640, 32));
      s_read <= '1';
      loop
         wait until rising_edge(clk);
         exit when s_wait = '0';
      end loop;
      s_read <= '0';
      while s_rvalid /= '1' loop
         wait until rising_edge(clk);
      end loop;
      change(640);
      drop;
      read(641, v);                              -- in the line, the fill not yet over
      expect(641, v, gen, "2b dropped after the fill's first word: the new word",
             "2b dropped after the fill's first word");

      -- 3. a drop while it prefetches the line's second half (a read at offset 3 starts it)
      read(1024, v);
      read(1025, v);
      read(1026, v);
      read(1027, v);                             -- the prefetch of 1032 .. 1035 starts
      assert in_flight > 0 report "no prefetch under way" severity failure;
      change(1024);
      drop;
      wait for 30 * C_CLK;
      read(1033, v);
      expect(1033, v, gen, "3 dropped during the prefetch: the new word", "3 dropped during the prefetch");
      read(1028, v);
      expect(1028, v, gen, "3 the rest of the line: the new word", "3 the rest of the line");

      if errors = 0 then
         report "ALL CHECKS PASSED";
      else
         report "FAILED with " & integer'image(errors) & " errors" severity error;
      end if;
      running <= false;
      wait;
   end process p_test;

end architecture sim;
