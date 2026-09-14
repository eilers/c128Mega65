----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65 (c128mega65)
--
-- Testbench for iecdrv_mem / iecdrv_trackmem, the block RAMs that back the drive
-- CPU's work RAM (1541/1571 ram + extram, 1581 ram) and the GCR track buffer.
--
-- Both were altsyncram instances upstream and had to be rewritten for Vivado. The
-- rewrite has one way to go wrong that hardware cannot show you: every caller drives
-- wren from a single-cycle strobe (ena_r or ph2_f for the CPU, buff_we per bit-cell
-- for the head) while address and data are only valid during that strobe. A version
-- that registers the address a cycle ahead of the data stores the byte that follows
-- the write. The drive then still boots -- its DOS runs from ROM, so it answers ATN
-- and does not report "device not present" -- but every RAM-held variable is wrong,
-- so DS$ comes back empty, the directory produces nothing and the bus stalls.
--
-- The bus model below is what the drives actually do: hold address and data for the
-- whole ph2 cycle, pulse wren for exactly one clock, then move both on.
--
-- MEGA65 port done by Stefan Eilers in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity tb_iecdrv_mem is
end entity tb_iecdrv_mem;

architecture sim of tb_iecdrv_mem is

   constant C_CLK_PERIOD : time    := 31 ns;   -- ~32 MHz drive clock domain
   constant C_ADDR_WIDTH : natural := 13;      -- 8 kB, the 1581 and 157x extram shape
   constant C_NUM_BYTES  : natural := 512;     -- enough to cover carries in the address

   -- Clocks per CPU cycle. The drives run a 1 MHz ph2 off a 16 MHz ce, so the write
   -- strobe is one clock out of many and address/data move well after it.
   constant C_CPU_CYCLE  : natural := 16;

   signal clk       : std_logic := '0';
   signal running   : boolean   := true;

   signal address_a : std_logic_vector(C_ADDR_WIDTH - 1 downto 0) := (others => '0');
   signal data_a    : std_logic_vector(7 downto 0) := (others => '0');
   signal wren_a    : std_logic := '0';
   signal q_a       : std_logic_vector(7 downto 0);

   signal address_b : std_logic_vector(C_ADDR_WIDTH - 1 downto 0) := (others => '0');
   signal data_b    : std_logic_vector(7 downto 0) := (others => '0');
   signal wren_b    : std_logic := '0';
   signal q_b       : std_logic_vector(7 downto 0);

   signal t_address_a : std_logic_vector(C_ADDR_WIDTH - 1 downto 0) := (others => '0');
   signal t_data_a    : std_logic_vector(7 downto 0) := (others => '0');
   signal t_wren_a    : std_logic := '0';
   signal t_q_a       : std_logic_vector(7 downto 0);

   signal t_address_b : std_logic_vector(C_ADDR_WIDTH - 1 downto 0) := (others => '0');
   signal t_data_b    : std_logic_vector(7 downto 0) := (others => '0');
   signal t_wren_b    : std_logic := '0';
   signal t_q_b       : std_logic_vector(7 downto 0);

   -- Deliberately not a plain ramp: folding the address into the value makes a
   -- one-byte shift, a stuck address bit and a swapped port all visible.
   function ram_byte (addr : natural) return std_logic_vector is
      variable v : natural;
   begin
      v := (addr * 7 + addr / 256 + 16#5A#) mod 256;
      return std_logic_vector(to_unsigned(v, 8));
   end function ram_byte;

begin

   p_clk : process
   begin
      while running loop
         clk <= '0';
         wait for C_CLK_PERIOD / 2;
         clk <= '1';
         wait for C_CLK_PERIOD / 2;
      end loop;
      wait;
   end process p_clk;

   i_mem : entity work.iecdrv_mem
      generic map (
         DATAWIDTH => 8,
         ADDRWIDTH => C_ADDR_WIDTH
      )
      port map (
         clock_a   => clk,
         address_a => address_a,
         data_a    => data_a,
         wren_a    => wren_a,
         q_a       => q_a,

         clock_b   => clk,
         address_b => address_b,
         data_b    => data_b,
         wren_b    => wren_b,
         q_b       => q_b
      ); -- i_mem

   i_trackmem : entity work.iecdrv_trackmem
      generic map (
         ADDRWIDTH => C_ADDR_WIDTH,
         WORDS     => 2 ** C_ADDR_WIDTH,
         DATAWIDTH => 8
      )
      port map (
         clock_a   => clk,
         address_a => t_address_a,
         data_a    => t_data_a,
         wren_a    => t_wren_a,
         q_a       => t_q_a,

         clock_b   => clk,
         address_b => t_address_b,
         data_b    => t_data_b,
         wren_b    => t_wren_b,
         q_b       => t_q_b
      ); -- i_trackmem

   p_stim : process
      variable l      : line;
      variable errors : natural := 0;

      -- One CPU write cycle: address and data are presented, wren is high for a
      -- single clock, and afterwards the bus immediately carries the next access.
      -- The trailing bus activity is the whole point -- a RAM that samples the data
      -- one cycle late latches this instead of the byte the CPU wrote.
      procedure cpu_write (addr : natural; val : std_logic_vector(7 downto 0)) is
      begin
         address_a   <= std_logic_vector(to_unsigned(addr, C_ADDR_WIDTH));
         data_a      <= val;
         t_address_b <= std_logic_vector(to_unsigned(addr, C_ADDR_WIDTH));
         t_data_b    <= val;
         wait until rising_edge(clk);

         wren_a   <= '1';
         t_wren_b <= '1';
         wait until rising_edge(clk);
         wren_a   <= '0';
         t_wren_b <= '0';

         -- Bus moves on straight away, exactly as the 6502 does after ph2 falls.
         address_a   <= (others => '1');
         data_a      <= x"FF";
         t_address_b <= (others => '1');
         t_data_b    <= x"FF";

         for i in 0 to C_CPU_CYCLE - 3 loop
            wait until rising_edge(clk);
         end loop;
      end procedure cpu_write;

      procedure cpu_read (addr : natural; expected : std_logic_vector(7 downto 0)) is
      begin
         address_a   <= std_logic_vector(to_unsigned(addr, C_ADDR_WIDTH));
         t_address_b <= std_logic_vector(to_unsigned(addr, C_ADDR_WIDTH));
         wait until rising_edge(clk);
         wait until rising_edge(clk);

         if q_a /= expected then
            if errors < 10 then
               write(l, string'("MISMATCH iecdrv_mem addr="));
               write(l, addr);
               write(l, string'(" expected="));
               write(l, to_integer(unsigned(expected)));
               write(l, string'(" got="));
               write(l, to_integer(unsigned(q_a)));
               writeline(output, l);
            end if;
            errors := errors + 1;
         end if;

         if t_q_b /= expected then
            if errors < 10 then
               write(l, string'("MISMATCH iecdrv_trackmem addr="));
               write(l, addr);
               write(l, string'(" expected="));
               write(l, to_integer(unsigned(expected)));
               write(l, string'(" got="));
               write(l, to_integer(unsigned(t_q_b)));
               writeline(output, l);
            end if;
            errors := errors + 1;
         end if;

         for i in 0 to C_CPU_CYCLE - 3 loop
            wait until rising_edge(clk);
         end loop;
      end procedure cpu_read;

   begin
      write(l, string'("{""sessionId"":""drvmem"",""runId"":""sim-drvmem"","
                     & """location"":""tb_iecdrv_mem:stim"",""message"":""simulation start""}"));
      writeline(output, l);

      wait for 10 * C_CLK_PERIOD;

      for addr in 0 to C_NUM_BYTES - 1 loop
         cpu_write(addr, ram_byte(addr));
      end loop;

      for addr in 0 to C_NUM_BYTES - 1 loop
         cpu_read(addr, ram_byte(addr));
      end loop;

      -- Rewrite in reverse so that a RAM which stores the *previous* address cannot
      -- pass by accident: ascending order alone lets an off-by-one look self-consistent.
      for addr in C_NUM_BYTES - 1 downto 0 loop
         cpu_write(addr, ram_byte(addr + 1));
      end loop;

      for addr in 0 to C_NUM_BYTES - 1 loop
         cpu_read(addr, ram_byte(addr + 1));
      end loop;

      write(l, string'("{""sessionId"":""drvmem"",""runId"":""sim-drvmem"","
                     & """location"":""tb_iecdrv_mem:summary"",""message"":""simulation finished"",""data"":{""bytes"":"));
      write(l, 4 * C_NUM_BYTES);
      write(l, string'(",""errors"":"));
      write(l, errors);
      write(l, string'(",""pass"":"));
      if errors = 0 then
         write(l, string'("true}}"));
      else
         write(l, string'("false}}"));
      end if;
      writeline(output, l);

      assert errors = 0
         report "drive RAM does not store what the CPU wrote" severity failure;

      running <= false;
      wait;
   end process p_stim;

end architecture sim;
