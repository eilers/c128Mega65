----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- tb_reu_sd: reu_sd.vhd through its device words, against a model of sdhost's engine
-- port (reads and writes), an Avalon memory with random wait states and read latency, and
-- a main clock of its own for the REU's cache.
--
-- The engine model moves a block's 512 bytes with gaps, takes a write's byte one clock
-- after its address (as sdhost), pulses e_blk after each block, honours e_limit_i (a block
-- starts only below it) and can end a transfer early at a block boundary (a QNICE command)
-- or fail it. Checks: loads land at the right place and stop at the length; saves put the
-- REU's bytes on the card; early ends are picked up again; errors and another card fail
-- the transfer; a load is busy until its writes are in memory and the cache's line was
-- dropped once; the CPU's drop request drops it once; an unanswered memory fails a
-- transfer instead of hanging it.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use work.sdhost_pkg.all;

entity tb_reu_sd is
end entity tb_reu_sd;

architecture sim of tb_reu_sd is

   constant C_CLK      : time := 20 ns;          -- QNICE clock
   constant C_MAIN_CLK : time := 31.7 ns;        -- the C128's
   constant C_BASE     : natural := 16#1000#;    -- word address of REU byte 0
   constant C_ROOM     : natural := 17;          -- 128 KB of REU
   constant C_MEM_W    : natural := C_BASE + 2 ** (C_ROOM - 1);
   constant C_SENTINEL : std_logic_vector(15 downto 0) := x"A5C3";
   constant C_CARD_BLK : natural := 3000;        -- the card's blocks that saves may write
   constant C_CARD_N   : natural := 64;

   signal clk          : std_logic := '0';
   signal main_clk     : std_logic := '0';
   signal rst          : std_logic := '1';
   signal running      : boolean := true;

   signal q_addr       : std_logic_vector(11 downto 0) := (others => '0');
   signal q_wdata      : std_logic_vector(15 downto 0) := (others => '0');
   signal q_ce         : std_logic := '0';
   signal q_we         : std_logic := '0';
   signal q_rdata      : std_logic_vector(15 downto 0);

   signal c2q          : t_sd_c2q;
   signal q2c          : t_sd_q2c := (others => '0');
   signal want         : std_logic;

   signal avm_write    : std_logic;
   signal avm_read     : std_logic;
   signal avm_address  : std_logic_vector(31 downto 0);
   signal avm_wdata    : std_logic_vector(15 downto 0);
   signal avm_be       : std_logic_vector(1 downto 0);
   signal avm_burst    : std_logic_vector(7 downto 0);
   signal avm_rdata    : std_logic_vector(15 downto 0) := (others => '0');
   signal avm_rvalid   : std_logic := '0';
   signal avm_wait     : std_logic := '0';
   signal drop         : std_logic;

   type t_mem is array (0 to C_MEM_W - 1) of std_logic_vector(15 downto 0);
   signal mem          : t_mem := (others => C_SENTINEL);

   type t_card is array (0 to C_CARD_N * 512 - 1) of std_logic_vector(7 downto 0);
   signal card         : t_card := (others => x"EE");

   -- engine model controls
   signal preempt_at   : integer := -1;          -- end the next transfer after this many blocks
   signal fail_at      : integer := -1;          -- fail the next transfer in this block
   signal throttled    : natural := 0;           -- blocks that waited for room or data
   signal starts       : natural := 0;
   signal card_gen     : std_logic_vector(7 downto 0) := x"01";

   -- memory model controls and records
   signal mem_dead     : boolean := false;       -- answers nothing
   signal last_write   : time := 0 ns;           -- when the last write was taken
   signal last_rvalid  : time := 0 ns;           -- when the last read data came
   signal drops        : natural := 0;
   signal last_drop    : time := 0 ns;
   signal fill_go      : boolean := false;      -- toggles: sentinels into fill_n words at fill_lo
   signal fill_lo      : natural := 0;          -- word offset in the REU
   signal fill_n       : natural := 0;

   -- the byte that SD block b holds at index i
   function sd_byte(b : natural; i : natural) return std_logic_vector is
   begin
      return std_logic_vector(to_unsigned((b * 37 + i * 11 + (i / 256) * 5 + 3) mod 256, 8));
   end function sd_byte;

   -- a pattern for REU byte o, put in memory for saves
   function reu_byte(o : natural) return std_logic_vector is
   begin
      return std_logic_vector(to_unsigned((o * 7 + o / 512 * 13 + 1) mod 256, 8));
   end function reu_byte;

begin

   clk      <= not clk after C_CLK / 2 when running;
   main_clk <= not main_clk after C_MAIN_CLK / 2 when running;

   i_dut : entity work.reu_sd
      generic map (
         G_BASE       => std_logic_vector(to_unsigned(C_BASE, 32)),
         G_ROOM_BITS  => C_ROOM,
         G_BUF_BLOCKS => 4
      )
      port map (
         clk_i               => clk,
         rst_i               => rst,
         qnice_addr_i        => q_addr,
         qnice_data_i        => q_wdata,
         qnice_ce_i          => q_ce,
         qnice_we_i          => q_we,
         qnice_data_o        => q_rdata,
         eng_want_o          => want,
         eng_o               => c2q,
         eng_i               => q2c,
         avm_write_o         => avm_write,
         avm_read_o          => avm_read,
         avm_address_o       => avm_address,
         avm_writedata_o     => avm_wdata,
         avm_byteenable_o    => avm_be,
         avm_burstcount_o    => avm_burst,
         avm_readdata_i      => avm_rdata,
         avm_readdatavalid_i => avm_rvalid,
         avm_waitrequest_i   => avm_wait,
         main_clk_i          => main_clk,
         main_drop_o         => drop
      );

   p_drops : process (main_clk)
   begin
      if rising_edge(main_clk) and drop = '1' then
         drops     <= drops + 1;
         last_drop <= now;
      end if;
   end process p_drops;

   -- Avalon memory: random wait states, reads answered in order after a few clocks
   p_mem : process (clk)
      variable seed1, seed2 : positive := 7;
      variable r            : real;
      variable a            : natural;
      type t_q is array (0 to 63) of natural;
      variable q_adr        : t_q;
      variable q_due        : t_q;
      variable q_head, q_n  : natural := 0;
      variable t            : natural := 0;
      variable fill_seen    : boolean := false;
   begin
      if rising_edge(clk) then
         if t = 0 then                           -- REU memory holds a pattern for the saves
            for w in 0 to 2 ** (C_ROOM - 1) - 1 loop
               mem(C_BASE + w) <= reu_byte(2 * w + 1) & reu_byte(2 * w);
            end loop;
         end if;
         if fill_go /= fill_seen then
            fill_seen := fill_go;
            for k in 0 to fill_n - 1 loop
               mem(C_BASE + (fill_lo + k) mod 2 ** (C_ROOM - 1)) <= C_SENTINEL;
            end loop;
         end if;
         t := t + 1;
         uniform(seed1, seed2, r);
         avm_rvalid <= '0';
         if q_n > 0 and q_due(q_head) <= t then
            avm_rdata  <= mem(q_adr(q_head));
            avm_rvalid <= '1';
            last_rvalid <= now;
            q_head := (q_head + 1) mod 64;
            q_n    := q_n - 1;
         end if;
         if avm_wait = '0' and not mem_dead then
            a := to_integer(unsigned(avm_address));
            if avm_write = '1' then
               assert a >= C_BASE and a < C_MEM_W report "write outside the REU" severity failure;
               assert unsigned(avm_burst) = 1 report "a write burst" severity failure;
               if avm_be(0) = '1' then
                  mem(a)(7 downto 0) <= avm_wdata(7 downto 0);
               end if;
               if avm_be(1) = '1' then
                  mem(a)(15 downto 8) <= avm_wdata(15 downto 8);
               end if;
               last_write <= now;
            elsif avm_read = '1' then
               uniform(seed1, seed2, r);
               for k in 0 to to_integer(unsigned(avm_burst)) - 1 loop
                  q_adr((q_head + q_n) mod 64) := (a + k) mod C_MEM_W;
                  q_due((q_head + q_n) mod 64) := t + 3 + integer(r * 4.0) + k;
                  q_n := q_n + 1;
               end loop;
            end if;
         end if;
         uniform(seed1, seed2, r);
         avm_wait <= '1' when r < 0.7 or mem_dead else '0';
      end if;
   end process p_mem;

   -- sdhost's engine port
   p_engine : process
      variable seed1, seed2 : positive := 3;
      variable r            : real;
      variable addr         : natural;
      variable cnt          : natural;
      variable wr           : boolean;
      variable failed       : boolean;
      variable k, c         : natural;
   begin
      q2c <= (others => '0');
      q2c(Q2C_READY) <= '1';
      q2c(Q2C_CARD)  <= card_gen;
      loop
         wait until rising_edge(clk);
         q2c(Q2C_CARD) <= card_gen;
         if c2q(C2Q_START) = '1' and q2c(Q2C_BUSY) = '0' then
            starts <= starts + 1;
            addr   := to_integer(unsigned(c2q(C2Q_ADDR)));
            cnt    := to_integer(unsigned(c2q(C2Q_COUNT)));
            wr     := c2q(C2Q_WRITE) = '1';
            failed := false;
            assert cnt > 0 report "start with count 0" severity failure;
            wait until rising_edge(clk);
            q2c(Q2C_BUSY) <= '1';
            k := 0;
            while k < cnt loop
               if k >= to_integer(unsigned(c2q(C2Q_LIMIT))) then
                  throttled <= throttled + 1;
               end if;
               while k >= to_integer(unsigned(c2q(C2Q_LIMIT))) loop
                  wait until rising_edge(clk);
               end loop;
               for i in 0 to 511 loop
                  uniform(seed1, seed2, r);
                  for g in 0 to integer(r * 3.0) loop
                     wait until rising_edge(clk);
                  end loop;
                  if wr then
                     -- the byte comes one clock after its address, which then runs ahead
                     q2c(Q2C_WRBLK) <= std_logic_vector(to_unsigned(k, 16));
                     q2c(Q2C_WRIDX) <= std_logic_vector(to_unsigned(i, 9));
                     wait until rising_edge(clk);
                     q2c(Q2C_WRIDX) <= std_logic_vector(to_unsigned((i + 1) mod 512, 9));
                     wait for 1 ns;
                     c := (addr + k - C_CARD_BLK) * 512 + i;
                     assert addr + k >= C_CARD_BLK and c < C_CARD_N * 512
                        report "a write outside the test blocks" severity failure;
                     card(c) <= c2q(C2Q_WRDATA);
                  else
                     q2c(Q2C_RDWE)   <= '1';
                     q2c(Q2C_RDBLK)  <= std_logic_vector(to_unsigned(k, 16));
                     q2c(Q2C_RDIDX)  <= std_logic_vector(to_unsigned(i, 9));
                     q2c(Q2C_RDDATA) <= sd_byte(addr + k, i);
                     wait until rising_edge(clk);
                     q2c(Q2C_RDWE)   <= '0';
                  end if;
                  if fail_at = k and i = 300 then
                     failed := true;
                     exit;
                  end if;
               end loop;
               exit when failed;
               wait until rising_edge(clk);
               q2c(Q2C_BLK) <= '1';
               wait until rising_edge(clk);
               q2c(Q2C_BLK) <= '0';
               k := k + 1;
               exit when preempt_at = k or c2q(C2Q_ABORT) = '1';
            end loop;
            wait until rising_edge(clk);
            wait until rising_edge(clk);
            q2c(Q2C_BUSY)  <= '0';
            q2c(Q2C_DONE)  <= '1';
            q2c(Q2C_ERROR) <= '1' when failed or c2q(C2Q_ABORT) = '1' else '0';
            wait until rising_edge(clk);
            q2c(Q2C_DONE)  <= '0';
         end if;
      end loop;
   end process p_engine;

   p_test : process
      variable errors : natural := 0;
      variable st     : std_logic_vector(15 downto 0);
      variable d0     : natural;
      variable t0     : time;

      -- a device write as the bus makes it: ce high for two falling edges, then low
      procedure wr(a : natural; v : natural) is
      begin
         wait until falling_edge(clk);
         q_addr  <= std_logic_vector(to_unsigned(a, 12));
         q_wdata <= std_logic_vector(to_unsigned(v mod 65536, 16));
         q_ce    <= '1';
         q_we    <= '1';
         wait until falling_edge(clk);
         wait until falling_edge(clk);
         wait for 1 ns;
         q_ce    <= '0';
         q_we    <= '0';
         wait until falling_edge(clk);
      end procedure wr;

      procedure rd(a : natural; v : out std_logic_vector(15 downto 0)) is
      begin
         q_addr <= std_logic_vector(to_unsigned(a, 12));
         q_ce   <= '1';
         wait for 5 ns;
         v      := q_rdata;
         q_ce   <= '0';
         wait for 15 ns;
      end procedure rd;

      procedure sentinel(lo, n : natural) is
      begin
         fill_lo <= lo;
         fill_n  <= n;
         fill_go <= not fill_go;
         wait until rising_edge(clk);
         wait until rising_edge(clk);
      end procedure sentinel;

      procedure fail(msg : string) is
      begin
         report msg severity error;
         errors := errors + 1;
      end procedure fail;

      -- set up and start a transfer, then wait while busy; st is the status after it
      procedure xfer(save : boolean; b, n, reu_addr, len : natural) is
      begin
         wr(16#002#, b mod 65536);
         wr(16#003#, b / 65536);
         wr(16#004#, n);
         wr(16#005#, reu_addr mod 65536);
         wr(16#006#, reu_addr / 65536);
         wr(16#007#, len mod 65536);
         wr(16#008#, len / 65536);
         wr(16#00A#, to_integer(unsigned(card_gen)));
         if save then
            wr(16#001#, 1);
         else
            wr(16#001#, 0);
         end if;
         rd(16#001#, st);
         if st(0) /= '1' then
            fail("not busy right after the start");
         end if;
         loop
            rd(16#001#, st);
            exit when st(0) = '0';
            wait for 200 ns;
         end loop;
      end procedure xfer;

      impure function mem_byte(o : natural) return std_logic_vector is
         variable w : std_logic_vector(15 downto 0);
      begin
         w := mem(C_BASE + (o / 2) mod 2 ** (C_ROOM - 1));
         if o mod 2 = 0 then
            return w(7 downto 0);
         end if;
         return w(15 downto 8);
      end function mem_byte;

      -- a load: REU bytes a .. a + n*512 - 1 hold SD blocks b .., cut at len; beyond: sentinel
      procedure check_load(b, n, a, len : natural; name : string) is
         variable bad  : natural := 0;
         variable want : std_logic_vector(7 downto 0);
      begin
         for j in 0 to n * 512 - 1 loop
            if j < len then
               want := sd_byte(b + j / 512, j mod 512);
            elsif (a + j) mod 2 = 0 then
               want := C_SENTINEL(7 downto 0);
            else
               want := C_SENTINEL(15 downto 8);
            end if;
            if mem_byte(a + j) /= want then
               if bad < 5 then
                  report name & ": REU byte " & integer'image(a + j) & " is " &
                         integer'image(to_integer(unsigned(mem_byte(a + j)))) & ", expected " &
                         integer'image(to_integer(unsigned(want))) severity error;
               end if;
               bad := bad + 1;
            end if;
         end loop;
         if bad = 0 then
            report name & ": ok";
         else
            errors := errors + 1;
         end if;
      end procedure check_load;

      -- a save: card blocks b .. b + n - 1 hold REU bytes a ..
      procedure check_save(b, n, a : natural; name : string) is
         variable bad : natural := 0;
      begin
         for j in 0 to n * 512 - 1 loop
            if card((b - C_CARD_BLK) * 512 + j) /= mem_byte(a + j) then
               if bad < 5 then
                  report name & ": card byte " & integer'image(j) & " of block " & integer'image(b) &
                         " is " & integer'image(to_integer(unsigned(card((b - C_CARD_BLK) * 512 + j)))) &
                         ", expected " & integer'image(to_integer(unsigned(mem_byte(a + j)))) severity error;
               end if;
               bad := bad + 1;
            end if;
         end loop;
         if bad = 0 then
            report name & ": ok";
         else
            errors := errors + 1;
         end if;
      end procedure check_save;

      -- a load must be busy until its writes are in memory and the line was dropped once
      procedure check_order(d_before : natural; name : string) is
      begin
         if drops /= d_before + 1 then
            fail(name & ": " & integer'image(drops - d_before) & " drops, not 1");
         elsif last_drop < last_write or last_rvalid < last_write or last_drop < last_rvalid then
            fail(name & ": the drop came before the writes were answered");
         else
            report name & ": dropped once, after its writes: ok";
         end if;
      end procedure check_order;

   begin
      wait for 5 * C_CLK;
      wait until rising_edge(clk);
      rst <= '0';
      wait for 5 * C_CLK;

      rd(16#000#, st);
      if st /= x"5D52" then
         fail("magic");
      end if;

      -- 1. a save of 12 blocks from REU byte 0x200 (the engine waits for the data)
      xfer(true, C_CARD_BLK, 12, 16#200#, 0);
      if st(1) /= '0' then fail("1: error"); end if;
      check_save(C_CARD_BLK, 12, 16#200#, "1 save");
      report "1: " & integer'image(throttled) & " blocks waited";
      if throttled = 0 then fail("1: the data limit never engaged"); end if;

      -- 2. a load of 16 blocks to REU byte 0x10000, cut 100 bytes into the last block
      sentinel(16#10000# / 2, 16 * 256);
      d0 := drops;
      xfer(false, 1000, 16, 16#10000#, 15 * 512 + 100);
      if st(1) /= '0' then fail("2: error"); end if;
      check_load(1000, 16, 16#10000#, 15 * 512 + 100, "2 load, cut at the file's end");
      check_order(d0, "2");

      -- 3. a load that wraps at the REU's top, odd length
      sentinel(2 ** (C_ROOM - 1) - 512, 4 * 256);
      d0 := drops;
      xfer(false, 2000, 4, 2 ** C_ROOM - 1024, 4 * 512 - 1);
      if st(1) /= '0' then fail("3: error"); end if;
      check_load(2000, 4, 2 ** C_ROOM - 1024, 4 * 512 - 1, "3 load around the top");
      check_order(d0, "3");

      -- 4. a load and a save that a QNICE command ends after 3 blocks: the rest again
      preempt_at <= 3;
      d0 := starts;
      xfer(false, 300, 8, 16#4000#, 8 * 512);
      if st(1) /= '0' then fail("4: load error"); end if;
      check_load(300, 8, 16#4000#, 8 * 512, "4 load preempted and resumed");
      xfer(true, C_CARD_BLK + 20, 8, 16#6000#, 0);
      preempt_at <= -1;
      if st(1) /= '0' then fail("4: save error"); end if;
      check_save(C_CARD_BLK + 20, 8, 16#6000#, "4 save preempted and resumed");
      if starts - d0 < 4 then fail("4: no transfer was issued again"); end if;

      -- 5. a read error in the fifth block, then a write error: the error bit, then cleared
      fail_at <= 4;
      d0 := drops;
      xfer(false, 700, 8, 16#8000#, 8 * 512);
      if st(1) /= '1' then fail("5: no error after a failed load"); else report "5 load error: ok"; end if;
      if drops /= d0 + 1 then fail("5: a failed load didn't drop the line"); end if;
      xfer(true, C_CARD_BLK + 30, 8, 16#8000#, 0);
      fail_at <= -1;
      if st(1) /= '1' then fail("5: no error after a failed save"); else report "5 save error: ok"; end if;
      xfer(false, 900, 2, 16#8000#, 2 * 512);
      if st(1) /= '0' then fail("5: error stuck"); end if;
      check_load(900, 2, 16#8000#, 2 * 512, "5 after an error");

      -- 6. another card: the transfer fails at once
      d0 := starts;
      card_gen <= x"02";
      wait for 100 ns;
      card_gen <= x"01";                         -- the registers still hold card 1
      wr(16#00A#, 1);
      card_gen <= x"03";
      wait for 100 ns;
      wr(16#004#, 4);
      wr(16#001#, 1);
      loop
         rd(16#001#, st);
         exit when st(0) = '0';
         wait for 200 ns;
      end loop;
      if st(1) /= '1' then fail("6: no error on another card"); end if;
      if starts /= d0 then fail("6: the engine started on another card"); end if;
      rd(16#00A#, st);
      if st(7 downto 0) /= x"03" then fail("6: card generation read"); end if;
      card_gen <= x"01";
      wait for 100 ns;
      report "6 another card: done";

      -- 7. the CPU asks for a drop: one, and the status bit until it is done
      d0 := drops;
      wr(16#009#, 0);
      rd(16#001#, st);
      if st(2) /= '1' then fail("7: drop not pending right after the request"); end if;
      t0 := now;
      loop
         rd(16#001#, st);
         exit when st(2) = '0';
         if now - t0 > 2 us then fail("7: drop never done"); exit; end if;
      end loop;
      if drops /= d0 + 1 then fail("7: " & integer'image(drops - d0) & " drops, not 1"); end if;
      -- two requests close together: at least one drop after the second
      d0 := drops;
      wr(16#009#, 0);
      wr(16#009#, 0);
      t0 := now;
      loop
         rd(16#001#, st);
         exit when st(2) = '0';
      end loop;
      if drops < d0 + 1 or last_drop < t0 then fail("7: no drop after the second request"); end if;
      report "7 CPU drop: done";

      -- 8. a memory that answers nothing: the load fails instead of hanging (21 ms)
      mem_dead <= true;
      xfer(false, 100, 2, 16#2000#, 2 * 512);
      mem_dead <= false;
      if st(1) /= '1' then fail("8: no error with a dead memory"); else report "8 dead memory: ok"; end if;
      wait for 1 us;
      xfer(false, 900, 2, 16#2000#, 2 * 512);
      if st(1) /= '0' then fail("8: error stuck after a dead memory"); end if;
      check_load(900, 2, 16#2000#, 2 * 512, "8 after a dead memory");

      if errors = 0 then
         report "ALL CHECKS PASSED";
      else
         report "FAILED with " & integer'image(errors) & " errors" severity error;
      end if;
      running <= false;
      wait;
   end process p_test;

end architecture sim;
