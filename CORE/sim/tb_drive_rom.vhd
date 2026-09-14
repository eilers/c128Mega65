----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65 (c128mega65)
--
-- Testbench for the drive DOS ROM handover: drive_rom_server.vhd feeding the real
-- iecdrv_rom.sv out of a block RAM that stands in for the boot1.rom device.
--
-- This exists because a wrong handover is invisible from the outside: iecdrv_rom
-- reports rom_valid after 32768 write strobes no matter which bytes it received, so
-- a shifted or duplicated image still lets the drive leave reset and simply makes it
-- execute garbage. On hardware that shows up only as "device not present".
--
-- Two banks are loaded in sequence. The switch from the first to the second is what
-- happens when the drive model changes in the menu, and it also covers the reload
-- path that a plain single-bank test would miss.
--
-- MEGA65 port done by Stefan Eilers in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity tb_drive_rom is
end entity tb_drive_rom;

architecture sim of tb_drive_rom is

   constant C_CLK_PERIOD  : time    := 20 ns;     -- 50 MHz QNICE clock
   constant C_BANK_SIZE   : natural := 32768;     -- one drive ROM bank
   constant C_NUM_BANKS   : natural := 6;         -- boot1.rom for DRIVES=2

   -- Banks of boot1.rom: 0,1 = 1541   2,3 = 1571   4,5 = 1581.
   -- Bank 2 is the power-on default of drive 8 (the menu defaults to 1571).
   constant C_BANK_FIRST  : natural := 0;
   constant C_BANK_SECOND : natural := 2;

   signal clk             : std_logic := '0';
   signal running         : boolean   := true;

   signal rom_loading     : std_logic := '1';
   signal rom_req         : std_logic;
   signal rom_offset      : std_logic_vector(14 downto 0);
   signal rom_addr        : std_logic_vector(18 downto 0);
   signal rom_wr          : std_logic;
   signal rom_data        : std_logic_vector(7 downto 0) := (others => '0');
   signal rom_ram_addr    : std_logic_vector(17 downto 0);

   signal drv_reset       : std_logic := '1';
   signal rom_valid       : std_logic;
   signal empty8k         : std_logic;
   signal rom_bank        : std_logic_vector(3 downto 0) := std_logic_vector(to_unsigned(C_BANK_FIRST, 4));
   signal mem_a           : std_logic_vector(14 downto 0) := (others => '0');
   signal rom_do          : std_logic_vector(7 downto 0);

   -- Contents of the stand-in boot1.rom, as a function of the absolute address.
   -- Deliberately not a plain ramp: mixing in the high address bytes means a wrong bank,
   -- an off-by-one and a stuck address bit all show up as a mismatch.
   function rom_byte (addr : natural) return std_logic_vector is
   begin
      return std_logic_vector(to_unsigned((addr * 7 + (addr / 256) * 3 + 1) mod 256, 8));
   end function rom_byte;

begin

   clk <= not clk after C_CLK_PERIOD / 2 when running else '0';

   -- iecdrv_rom only drives the offset; the bank comes from iec_drive's arbitration
   rom_addr <= rom_bank & rom_offset;

   ---------------------------------------------------------------------------------
   -- Source ROM: registered read, mirroring dualport_2clk_ram port B in mega65.vhd
   ---------------------------------------------------------------------------------
   p_source_rom : process (clk)
      variable a : integer;
   begin
      if rising_edge(clk) then
         if is_x(rom_ram_addr) then
            rom_data <= (others => 'X');
         else
            a := to_integer(unsigned(rom_ram_addr));
            if a < C_NUM_BANKS * C_BANK_SIZE then
               rom_data <= rom_byte(a);
            else
               -- boot1.rom holds exactly six banks; the server must never look past them
               rom_data <= (others => 'X');
            end if;
         end if;
      end if;
   end process p_source_rom;

   ---------------------------------------------------------------------------------
   -- Device under test
   ---------------------------------------------------------------------------------
   i_server : entity work.drive_rom_server
      generic map (
         G_ROM_ADDR_WIDTH => 18
      )
      port map (
         clk_i          => clk,
         rom_loading_i  => rom_loading,
         rom_req_i      => rom_req,
         rom_addr_i     => rom_addr,
         rom_wr_o       => rom_wr,
         rom_ram_addr_o => rom_ram_addr
      );

   -- The real consumer, unmodified apart from its Vivado block RAM port
   i_iecdrv_rom : entity work.iecdrv_rom
      port map (
         clk_sys     => clk,
         clk         => clk,
         reset       => drv_reset,
         rom_loading => rom_loading,
         empty8k     => empty8k,
         rom_valid   => rom_valid,
         rom_bank    => rom_bank,
         mem_a       => mem_a,
         rom_do      => rom_do,
         rom_req     => rom_req,
         rom_addr    => rom_offset,
         rom_wr      => rom_wr,
         rom_data    => rom_data
      );

   ---------------------------------------------------------------------------------
   -- Stimulus and check
   ---------------------------------------------------------------------------------
   p_stim : process
      variable l      : line;
      variable errors : natural := 0;
      variable cycles : natural;

      -- Wait for the bank currently selected on rom_bank to finish loading, then read it
      -- back out through the drive-side port and compare it against the source image.
      procedure load_and_check (bank : natural; total_errors : inout natural) is
         variable expected : std_logic_vector(7 downto 0);
         variable ln       : line;
         variable n        : natural := 0;
      begin
         cycles := 0;
         while rom_valid = '0' and cycles < 8 * C_BANK_SIZE loop
            wait until rising_edge(clk);
            cycles := cycles + 1;
         end loop;

         assert rom_valid = '1'
            report "rom_valid never asserted for bank " & integer'image(bank)
                 & ": the ROM handshake stalled"
            severity failure;

         for i in 0 to C_BANK_SIZE - 1 loop
            mem_a <= std_logic_vector(to_unsigned(i, 15));
            -- mem_a_d inside iecdrv_rom, then the tdp_ram address register, plus margin
            for w in 0 to 2 loop
               wait until rising_edge(clk);
            end loop;
            wait for 1 ns;

            expected := rom_byte(bank * C_BANK_SIZE + i);
            if rom_do /= expected then
               if n < 8 then
                  write(ln, string'("MISMATCH bank "));
                  write(ln, bank);
                  write(ln, string'(" offset 0x"));
                  hwrite(ln, std_logic_vector(to_unsigned(i, 16)));
                  write(ln, string'(" expected 0x"));
                  hwrite(ln, expected);
                  write(ln, string'(" got 0x"));
                  hwrite(ln, rom_do);
                  writeline(output, ln);
                  n := n + 1;
               end if;
               total_errors := total_errors + 1;
            end if;
         end loop;

         write(ln, string'("bank "));
         write(ln, bank);
         write(ln, string'(" loaded in "));
         write(ln, cycles);
         write(ln, string'(" cycles"));
         writeline(output, ln);
      end procedure load_and_check;

   begin
      write(l, string'("{""sessionId"":""drvrom"",""runId"":""sim-drvrom"","
                     & """location"":""tb_drive_rom:stim"",""message"":""simulation start""}"));
      writeline(output, l);

      wait for 10 * C_CLK_PERIOD;
      rom_loading <= '0';
      drv_reset   <= '0';

      load_and_check(C_BANK_FIRST, errors);

      -- Changing the drive model switches the bank; iecdrv_rom must drop rom_valid and
      -- pull the new bank in, otherwise the drive keeps running the previous model's DOS.
      rom_bank <= std_logic_vector(to_unsigned(C_BANK_SECOND, 4));
      wait until rising_edge(clk);
      wait until rising_edge(clk);
      assert rom_valid = '0'
         report "rom_valid stayed high across a bank switch: the new bank is never loaded"
         severity failure;

      load_and_check(C_BANK_SECOND, errors);

      write(l, string'("{""sessionId"":""drvrom"",""runId"":""sim-drvrom"","
                     & """location"":""tb_drive_rom:summary"",""message"":""simulation finished"",""data"":{""banks"":"));
      write(l, 2);
      write(l, string'(",""bytes"":"));
      write(l, 2 * C_BANK_SIZE);
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
         report "drive ROM handover corrupted the image" severity failure;

      running <= false;
      wait;
   end process p_stim;

end architecture sim;
