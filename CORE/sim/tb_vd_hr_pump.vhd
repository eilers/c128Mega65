----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Testbench for vd_hr_pump: the hardware fill of a virtual-drive sector buffer
-- from HyperRAM.
--
-- A mock Avalon slave returns (byte_address xor 0xA5) in little-endian words.
-- The drive-side protocol is the MiSTer sd_rd / sd_ack / sd_buff_wr handshake.
--
-- MEGA65 port done by Stefan Eilers in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

library work;
use work.vdrives_pkg.all;

entity tb_vd_hr_pump is
end entity tb_vd_hr_pump;

architecture sim of tb_vd_hr_pump is

   constant C_QNICE_PERIOD : time    := 20 ns;  -- 50 MHz
   constant C_HR_PERIOD    : time    := 10 ns;  -- 100 MHz
   constant C_VDNUM        : natural := 2;
   constant C_PAT          : unsigned(7 downto 0) := x"A5";

   -- Word bases: drive 8 at 0, drive 9 at 0x100 (byte 0x200)
   constant C_BASE_D8 : std_logic_vector(21 downto 0) := (others => '0');
   constant C_BASE_D9 : std_logic_vector(21 downto 0) := std_logic_vector(to_unsigned(16#100#, 22));

   signal clk_qnice : std_logic := '0';
   signal rst_qnice : std_logic := '1';
   signal clk_hr    : std_logic := '0';
   signal rst_hr    : std_logic := '1';
   signal running   : boolean   := true;

   signal sd_lba     : vd_vec_array(C_VDNUM - 1 downto 0)(31 downto 0) := (others => (others => '0'));
   signal sd_blk_cnt : vd_vec_array(C_VDNUM - 1 downto 0)(5 downto 0)  := (others => (others => '0'));
   signal sd_rd      : vd_std_array(C_VDNUM - 1 downto 0) := (others => '0');
   signal sd_ack     : vd_std_array(C_VDNUM - 1 downto 0);
   signal sd_addr    : std_logic_vector(13 downto 0);
   signal sd_dout    : std_logic_vector(7 downto 0);
   signal sd_wr      : std_logic;
   signal pump_busy  : std_logic;

   signal hr_write         : std_logic;
   signal hr_read          : std_logic;
   signal hr_address       : std_logic_vector(31 downto 0);
   signal hr_writedata     : std_logic_vector(15 downto 0);
   signal hr_byteenable    : std_logic_vector(1 downto 0);
   signal hr_burstcount    : std_logic_vector(7 downto 0);
   signal hr_readdata      : std_logic_vector(15 downto 0) := (others => '0');
   signal hr_readdatavalid : std_logic := '0';
   signal hr_waitrequest   : std_logic := '0';

   type t_buf is array (0 to 511) of std_logic_vector(7 downto 0);
   signal cap_buf   : t_buf := (others => (others => '0'));
   signal cap_count : natural := 0;
   signal cap_ack_d : std_logic := '0';

   -- Pipeline of accepted HyperRAM reads (word address), 4-cycle latency
   type t_pend is array (0 to 3) of std_logic;
   type t_paddr is array (0 to 3) of unsigned(31 downto 0);
   signal pend  : t_pend  := (others => '0');
   signal paddr : t_paddr := (others => (others => '0'));

   function pat_byte (byte_addr : unsigned) return std_logic_vector is
   begin
      return std_logic_vector(byte_addr(7 downto 0) xor byte_addr(15 downto 8) xor C_PAT);
   end function;

   function expected_byte (base_words : unsigned;
                           lba        : natural;
                           index      : natural) return std_logic_vector is
      variable ba : unsigned(31 downto 0);
   begin
      ba := shift_left(resize(base_words, 32), 1) + to_unsigned(lba * 256 + index, 32);
      return pat_byte(ba);
   end function;

begin

   clk_qnice <= not clk_qnice after C_QNICE_PERIOD / 2 when running else '0';
   clk_hr    <= not clk_hr    after C_HR_PERIOD    / 2 when running else '0';

   i_dut : entity work.vd_hr_pump
      generic map (
         G_VDNUM => C_VDNUM
      )
      port map (
         clk_qnice_i        => clk_qnice,
         rst_qnice_i        => rst_qnice,
         sd_lba_i           => sd_lba,
         sd_blk_cnt_i       => sd_blk_cnt,
         sd_rd_i            => sd_rd,
         sd_ack_o           => sd_ack,
         sd_buff_addr_o     => sd_addr,
         sd_buff_dout_o     => sd_dout,
         sd_buff_wr_o       => sd_wr,
         busy_o             => pump_busy,
         hr_base_d8_i       => C_BASE_D8,
         hr_base_d9_i       => C_BASE_D9,
         hr_clk_i           => clk_hr,
         hr_rst_i           => rst_hr,
         hr_write_o         => hr_write,
         hr_read_o          => hr_read,
         hr_address_o       => hr_address,
         hr_writedata_o     => hr_writedata,
         hr_byteenable_o    => hr_byteenable,
         hr_burstcount_o    => hr_burstcount,
         hr_readdata_i      => hr_readdata,
         hr_readdatavalid_i => hr_readdatavalid,
         hr_waitrequest_i   => hr_waitrequest
      );

   -- Capture bytes while ack is held, the way the drive buffer RAM is written
   p_capture : process (clk_qnice)
      variable any_ack : std_logic;
   begin
      if rising_edge(clk_qnice) then
         any_ack   := sd_ack(0) or sd_ack(1);
         cap_ack_d <= any_ack;
         if any_ack = '1' and sd_wr = '1' then
            cap_buf(to_integer(unsigned(sd_addr))) <= sd_dout;
            if cap_ack_d = '0' then
               cap_count <= 1;
            else
               cap_count <= cap_count + 1;
            end if;
         elsif cap_ack_d = '0' and any_ack = '1' then
            cap_count <= 0;
         end if;
      end if;
   end process p_capture;

   -- Mock HyperRAM: accept immediately, return the word four cycles later
   hr_waitrequest <= '0';

   p_hr : process (clk_hr)
      variable ba : unsigned(31 downto 0);
   begin
      if rising_edge(clk_hr) then
         if rst_hr = '1' then
            pend             <= (others => '0');
            hr_readdatavalid <= '0';
         else
            hr_readdatavalid <= '0';

            for i in 3 downto 1 loop
               pend(i)  <= pend(i - 1);
               paddr(i) <= paddr(i - 1);
            end loop;
            pend(0) <= '0';

            if hr_read = '1' and hr_waitrequest = '0' then
               pend(0)  <= '1';
               paddr(0) <= unsigned(hr_address);
            end if;

            if pend(3) = '1' then
               ba := shift_left(paddr(3), 1);
               hr_readdata(7 downto 0)  <= pat_byte(ba);
               hr_readdata(15 downto 8) <= pat_byte(ba + 1);
               hr_readdatavalid         <= '1';
            end if;
         end if;
      end if;
   end process p_hr;

   p_stim : process
      variable errors : natural := 0;
      variable l      : line;
      variable n      : natural;
      variable lba    : natural;
      variable drv    : natural;

      procedure start_read (drive     : natural;
                            lba_val   : natural;
                            blk_cnt   : natural) is
      begin
         wait until rising_edge(clk_qnice);
         sd_lba(drive)     <= std_logic_vector(to_unsigned(lba_val, 32));
         sd_blk_cnt(drive) <= std_logic_vector(to_unsigned(blk_cnt, 6));
         sd_rd(drive)      <= '1';
      end procedure;

      procedure wait_done (drive : natural; timeout_cycles : natural) is
         variable c : natural := 0;
      begin
         while sd_ack(drive) = '0' loop
            wait until rising_edge(clk_qnice);
            c := c + 1;
            if c > timeout_cycles then
               report "timeout waiting for ack rise" severity failure;
            end if;
         end loop;
         -- c157x_track drops sd_rd in the cycle it sees ack
         sd_rd(drive) <= '0';
         while sd_ack(drive) = '1' loop
            wait until rising_edge(clk_qnice);
            c := c + 1;
            if c > timeout_cycles then
               report "timeout waiting for ack fall" severity failure;
            end if;
         end loop;
         wait until rising_edge(clk_qnice);
      end procedure;

      procedure check_buf (drive : natural; lba_val : natural; nbytes : natural) is
         variable base : unsigned(21 downto 0);
         variable i    : natural;
      begin
         if drive = 0 then
            base := unsigned(C_BASE_D8);
         else
            base := unsigned(C_BASE_D9);
         end if;
         if cap_count /= nbytes then
            report "byte count " & integer'image(cap_count)
               & " expected " & integer'image(nbytes) severity error;
            errors := errors + 1;
         end if;
         for i in 0 to nbytes - 1 loop
            if cap_buf(i) /= expected_byte(unsigned(base), lba_val, i) then
               report "mismatch at " & integer'image(i)
                  & " got " & integer'image(to_integer(unsigned(cap_buf(i))))
                  severity error;
               errors := errors + 1;
               exit;
            end if;
         end loop;
      end procedure;

   begin
      write(l, string'("{""sessionId"":""vdhrpump"",""runId"":""sim-vdhrpump"","
                     & """location"":""tb_vd_hr_pump:stim"",""message"":""simulation start""}"));
      writeline(output, l);

      wait for 30 * C_QNICE_PERIOD;
      rst_qnice <= '0';
      rst_hr    <= '0';
      wait for 20 * C_QNICE_PERIOD;

      -- Drive 8, LBA 2, one 256-byte block
      lba := 2;
      drv := 0;
      n   := 256;
      start_read(drv, lba, 0);
      wait_done(drv, 20000);
      check_buf(drv, lba, n);

      -- Drive 9, LBA 0, two 256-byte blocks (1581-style), non-zero HyperRAM base
      lba := 0;
      drv := 1;
      n   := 512;
      start_read(drv, lba, 1);
      wait_done(drv, 40000);
      check_buf(drv, lba, n);

      if hr_write /= '0' then
         report "pump issued a HyperRAM write" severity error;
         errors := errors + 1;
      end if;

      write(l, string'("{""sessionId"":""vdhrpump"",""runId"":""sim-vdhrpump"","
                     & """location"":""tb_vd_hr_pump:summary"",""message"":""simulation finished"",""data"":{""errors"":"));
      write(l, errors);
      write(l, string'(",""pass"":"));
      if errors = 0 then
         write(l, string'("true}}"));
         writeline(output, l);
         write(l, string'("ALL CHECKS PASSED"));
         writeline(output, l);
      else
         write(l, string'("false}}"));
         writeline(output, l);
      end if;

      assert errors = 0 report "vd_hr_pump filled the buffer incorrectly" severity failure;

      running <= false;
      wait;
   end process p_stim;

end architecture sim;
