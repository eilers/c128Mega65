----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- tb_sd_dma: sd_dma.vhd against a model of sdhost's engine port and an Avalon memory
-- with random wait states.
--
-- The engine model sends a block's 512 bytes with gaps, pulses e_blk after each block,
-- honours e_limit_i like sdhost (a block starts only below the limit) and can end a
-- transfer early at a block boundary (a QNICE command) or fail it. Checks: the bytes
-- land at the right place, nothing is written at or beyond the file length, an early end
-- is picked up again, an error ends the DMA with error_o.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use work.sdhost_pkg.all;

entity tb_sd_dma is
end entity tb_sd_dma;

architecture sim of tb_sd_dma is

   constant C_CLK      : time := 20 ns;          -- QNICE clock
   constant C_MEM_W    : natural := 16384;       -- words of memory (32 kB)
   constant C_SENTINEL : std_logic_vector(15 downto 0) := x"A5C3";

   signal clk          : std_logic := '0';
   signal rst          : std_logic := '1';
   signal running      : boolean := true;

   signal start        : std_logic := '0';
   signal blk          : std_logic_vector(31 downto 0) := (others => '0');
   signal count        : std_logic_vector(15 downto 0) := (others => '0');
   signal dest         : std_logic_vector(31 downto 0) := (others => '0');
   signal length       : std_logic_vector(31 downto 0) := (others => '0');
   signal busy         : std_logic;
   signal err          : std_logic;

   signal c2q          : t_sd_c2q;
   signal q2c          : t_sd_q2c := (others => '0');

   signal avm_write    : std_logic;
   signal avm_address  : std_logic_vector(31 downto 0);
   signal avm_wdata    : std_logic_vector(15 downto 0);
   signal avm_be       : std_logic_vector(1 downto 0);
   signal avm_wait     : std_logic := '0';

   type t_mem is array (0 to C_MEM_W - 1) of std_logic_vector(15 downto 0);
   signal mem          : t_mem := (others => C_SENTINEL);

   -- engine model controls
   signal preempt_at   : integer := -1;          -- end the next transfer after this many blocks
   signal fail_at      : integer := -1;          -- fail the next transfer in this block
   signal throttled    : natural := 0;           -- blocks that waited for room in the buffer

   -- the byte that SD block b holds at index i
   function sd_byte(b : natural; i : natural) return std_logic_vector is
   begin
      return std_logic_vector(to_unsigned((b * 37 + i * 11 + (i / 256) * 5 + 3) mod 256, 8));
   end function sd_byte;

begin

   clk <= not clk after C_CLK / 2 when running;

   i_dut : entity work.sd_dma
      generic map (
         G_BUF_BLOCKS => 4
      )
      port map (
         clk_i             => clk,
         rst_i             => rst,
         start_i           => start,
         blk_i             => blk,
         count_i           => count,
         dest_i            => dest,
         length_i          => length,
         busy_o            => busy,
         error_o           => err,
         eng_o             => c2q,
         eng_i             => q2c,
         avm_write_o       => avm_write,
         avm_address_o     => avm_address,
         avm_writedata_o   => avm_wdata,
         avm_byteenable_o  => avm_be,
         avm_waitrequest_i => avm_wait
      );

   -- Avalon memory: random wait states
   p_mem : process (clk)
      variable seed1, seed2 : positive := 7;
      variable r            : real;
      variable a            : natural;
   begin
      if rising_edge(clk) then
         uniform(seed1, seed2, r);
         avm_wait <= '1' when r < 0.92 else '0';    -- slower than the engine: the limit bites
         if avm_write = '1' and avm_wait = '0' then
            a := to_integer(unsigned(avm_address));
            assert a < C_MEM_W report "write outside the memory" severity failure;
            if avm_be(0) = '1' then
               mem(a)(7 downto 0) <= avm_wdata(7 downto 0);
            end if;
            if avm_be(1) = '1' then
               mem(a)(15 downto 8) <= avm_wdata(15 downto 8);
            end if;
         end if;
      end if;
   end process p_mem;

   -- sdhost's engine port, roughly as fast as SPI at 25 MHz (a byte every few clocks)
   p_engine : process
      variable seed1, seed2 : positive := 3;
      variable r            : real;
      variable addr         : natural;
      variable cnt          : natural;
      variable failed       : boolean;
      variable k            : natural;
   begin
      q2c <= (others => '0');
      q2c(Q2C_READY) <= '1';
      loop
         wait until rising_edge(clk);
         if c2q(C2Q_START) = '1' and q2c(Q2C_BUSY) = '0' then
            assert c2q(C2Q_WRITE) = '0' report "the DMA asked for a write" severity failure;
            addr   := to_integer(unsigned(c2q(C2Q_ADDR)));
            cnt    := to_integer(unsigned(c2q(C2Q_COUNT)));
            failed := false;
            assert cnt > 0 report "start with count 0" severity failure;
            wait until rising_edge(clk);
            q2c(Q2C_BUSY) <= '1';
            k := 0;
            while k < cnt loop
               -- a block only starts below the limit (sdhost's e_room)
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
                  q2c(Q2C_RDWE)   <= '1';
                  q2c(Q2C_RDBLK)  <= std_logic_vector(to_unsigned(k, 16));
                  q2c(Q2C_RDIDX)  <= std_logic_vector(to_unsigned(i, 9));
                  q2c(Q2C_RDDATA) <= sd_byte(addr + k, i);
                  wait until rising_edge(clk);
                  q2c(Q2C_RDWE)   <= '0';
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
               exit when preempt_at = k;
            end loop;
            wait until rising_edge(clk);
            wait until rising_edge(clk);
            q2c(Q2C_BUSY)  <= '0';
            q2c(Q2C_DONE)  <= '1';
            q2c(Q2C_ERROR) <= '1' when failed else '0';
            wait until rising_edge(clk);
            q2c(Q2C_DONE)  <= '0';
         end if;
      end loop;
   end process p_engine;

   p_test : process
      variable errors : natural := 0;

      procedure dma(b, n, d, len : natural) is
      begin
         blk    <= std_logic_vector(to_unsigned(b, 32));
         count  <= std_logic_vector(to_unsigned(n, 16));
         dest   <= std_logic_vector(to_unsigned(d, 32));
         length <= std_logic_vector(to_unsigned(len, 32));
         wait until rising_edge(clk);
         start <= '1';
         wait until rising_edge(clk);
         start <= '0';
         wait until rising_edge(clk);
         wait until rising_edge(clk);
         while busy = '1' loop
            wait until rising_edge(clk);
         end loop;
      end procedure dma;

      -- memory byte at byte offset o
      impure function mem_byte(o : natural) return std_logic_vector is
      begin
         if o mod 2 = 0 then
            return mem(o / 2)(7 downto 0);
         else
            return mem(o / 2)(15 downto 8);
         end if;
      end function mem_byte;

      -- bytes d .. d + n*512 - 1 must hold SD blocks b .., cut at len; beyond: sentinel
      procedure check(b, n, d, len : natural; name : string) is
         variable o, bad : natural := 0;
         variable want   : std_logic_vector(7 downto 0);
      begin
         for j in 0 to n * 512 - 1 loop
            o := d + j;
            if o < len then
               want := sd_byte(b + j / 512, j mod 512);
            elsif o mod 2 = 0 then
               want := C_SENTINEL(7 downto 0);
            else
               want := C_SENTINEL(15 downto 8);
            end if;
            if mem_byte(o) /= want then
               if bad < 5 then
                  report name & ": byte " & integer'image(o) & " is " &
                         integer'image(to_integer(unsigned(mem_byte(o)))) & ", expected " &
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
      end procedure check;

   begin
      wait for 5 * C_CLK;
      wait until rising_edge(clk);
      rst <= '0';
      wait for 5 * C_CLK;

      -- 1. one run of 16 blocks, the file ends 100 bytes into the last block
      dma(1000, 16, 0, 15 * 512 + 100);
      assert err = '0' report "1: error" severity error;
      check(1000, 16, 0, 15 * 512 + 100, "1 whole run, cut at the file's end");
      report "1: " & integer'image(throttled) & " blocks waited for room in the buffer";
      if throttled = 0 then
         report "1: the buffer limit never engaged" severity error;
         errors := errors + 1;
      end if;

      -- 2. a second run behind it (a fragmented file), length with an odd byte
      dma(5000, 6, 16 * 512, 22 * 512 - 1);
      assert err = '0' report "2: error" severity error;
      check(5000, 6, 16 * 512, 22 * 512 - 1, "2 second run, odd length");
      check(1000, 15, 0, 15 * 512 + 100, "2 first run untouched");

      -- 3. a QNICE command ends the transfer after 3 blocks: the rest is issued again
      preempt_at <= 3;
      dma(200, 8, 22 * 512, 30 * 512);
      preempt_at <= -1;
      assert err = '0' report "3: error" severity error;
      check(200, 8, 22 * 512, 30 * 512, "3 preempted and resumed");

      -- 4. a read error in the fifth block: error_o, and the DMA ends
      fail_at <= 4;
      dma(700, 8, 30 * 512, 38 * 512);
      fail_at <= -1;
      if err /= '1' then
         report "4: no error after a failed transfer" severity error;
         errors := errors + 1;
      else
         report "4 read error reported: ok";
      end if;

      -- 5. the next DMA after an error clears it
      dma(900, 2, 30 * 512, 32 * 512);
      assert err = '0' report "5: error stuck" severity error;
      check(900, 2, 30 * 512, 32 * 512, "5 after an error");

      if errors = 0 and err = '0' then
         report "ALL CHECKS PASSED";
      else
         report "FAILED with " & integer'image(errors) & " errors" severity error;
      end if;
      running <= false;
      wait;
   end process p_test;

end architecture sim;
