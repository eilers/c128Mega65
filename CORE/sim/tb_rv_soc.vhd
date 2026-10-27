----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Testbench for the RISC-V system CPU (M2M/vhdl/rv/rv_soc.vhd). It boots
-- M2M/firmware/test/selftest.rom, which checks the registers, the device bus (the
-- On-Screen-Menu RAM and a RAM with wait states, both modelled here with QNICE's
-- falling-edge timing), the SD card registers and HyperRAM, including code run from
-- it. The firmware writes PASS or FAIL into row 1 of the On-Screen-Menu.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.sdhost_pkg.all;

entity tb_rv_soc is
   generic (
      G_BOOT_ROM : string  := "selftest.rom";
      G_TRACE_HR : boolean := false       -- report every HyperRAM access
   );
end entity tb_rv_soc;

architecture sim of tb_rv_soc is

   constant C_PERIOD     : time    := 20 ns;       -- 50 MHz; the CPU and HyperRAM run at twice that
   constant C_HR_WORDS   : natural := 2 ** 18;
   constant C_HR_LATENCY : natural := 16;          -- 100 MHz clocks before read data
   constant C_DEV_WAIT   : natural := 3;           -- wait clocks of the test device
   constant C_TIMEOUT    : time    := 5 ms;

   signal clk50   : std_logic := '0';
   signal clk100  : std_logic := '1';      -- so that each rising edge of clk50 is one of clk100
   signal reset_n : std_logic := '0';
   signal done    : boolean := false;

   signal ramrom_dev     : std_logic_vector(15 downto 0);
   signal ramrom_addr    : std_logic_vector(27 downto 0);
   signal ramrom_wdata   : std_logic_vector(15 downto 0);
   signal ramrom_rdata   : std_logic_vector(15 downto 0);
   signal ramrom_ce      : std_logic;
   signal ramrom_we      : std_logic;
   signal ramrom_wait    : std_logic;
   signal control_d      : std_logic_vector(255 downto 0);
   signal control_m      : std_logic_vector(255 downto 0);
   signal csr_osm        : std_logic;
   signal csr_reset      : std_logic;

   signal hr_write, hr_read : std_logic;
   signal hr_address        : std_logic_vector(31 downto 0);
   signal hr_writedata      : std_logic_vector(15 downto 0);
   signal hr_byteenable     : std_logic_vector(1 downto 0);
   signal hr_burstcount     : std_logic_vector(7 downto 0);
   signal hr_readdata       : std_logic_vector(15 downto 0) := (others => '0');
   signal hr_readdatavalid  : std_logic := '0';
   signal hr_waitrequest    : std_logic := '1';

   -- On-Screen-Menu RAM and the test device
   type t_words is array (natural range <>) of std_logic_vector(15 downto 0);
   signal vram_data : t_words(0 to 2047) := (others => x"0020");
   signal vram_attr : t_words(0 to 2047) := (others => x"0000");
   signal vram_q    : std_logic_vector(15 downto 0);
   signal dev_ram   : t_words(0 to 255) := (others => x"0000");
   signal dev_q     : std_logic_vector(15 downto 0);
   signal dev_count : natural range 0 to C_DEV_WAIT := 0;
   signal dev_done  : std_logic := '0';

   function str_at(mem : t_words; pos : natural; s : string) return boolean is
   begin
      for i in s'range loop
         if mem(pos + i - s'low)(7 downto 0) /= std_logic_vector(to_unsigned(character'pos(s(i)), 8)) then
            return false;
         end if;
      end loop;
      return true;
   end function str_at;

begin

   clk50   <= not clk50 after C_PERIOD / 2 when not done;
   clk100  <= not clk100 after C_PERIOD / 4 when not done;
   reset_n <= '1' after 200 ns;

   i_dut : entity work.rv_soc
      generic map (
         G_BOOT_ROM => G_BOOT_ROM,
         G_VGA_DX   => 720,
         G_VGA_DY   => 540,
         G_FONT_DX  => 16,
         G_FONT_DY  => 16
      )
      port map (
         clk50_i            => clk50,
         clk100_i           => clk100,
         reset_n_i          => reset_n,
         uart_rxd_i         => '1',
         uart_txd_o         => open,
         sd_clk_o           => open,
         sd_cmd_o           => open,
         sd_cmd_oe_o        => open,
         sd_cmd_i           => '1',
         sd_dat_o           => open,
         sd_dat_oe_o        => open,
         sd_dat_i           => "1111",
         sd_cd_i            => '1',
         sd2_clk_o          => open,
         sd2_cmd_o          => open,
         sd2_cmd_oe_o       => open,
         sd2_cmd_i          => '1',
         sd2_dat_o          => open,
         sd2_dat_oe_o       => open,
         sd2_dat_i          => "1111",
         sd2_cd_i           => '1',
         sd_eng_i           => (others => '0'),
         sd_eng_o           => open,
         csr_reset_o        => csr_reset,
         csr_pause_o        => open,
         csr_osm_o          => csr_osm,
         csr_keyboard_o     => open,
         csr_joy1_o         => open,
         csr_joy2_o         => open,
         osm_xy_o           => open,
         osm_dxdy_o         => open,
         ascal_mode_i       => "00000",
         ascal_mode_o       => open,
         keys_n_i           => not x"0100",    -- F1 held
         control_d_o        => control_d,
         control_m_o        => control_m,
         special_i          => x"0000",
         general_i          => x"0000",
         ramrom_dev_o       => ramrom_dev,
         ramrom_addr_o      => ramrom_addr,
         ramrom_data_o      => ramrom_wdata,
         ramrom_data_i      => ramrom_rdata,
         ramrom_ce_o        => ramrom_ce,
         ramrom_wait_i      => ramrom_wait,
         ramrom_we_o        => ramrom_we,
         hr_write_o         => hr_write,
         hr_read_o          => hr_read,
         hr_address_o       => hr_address,
         hr_writedata_o     => hr_writedata,
         hr_byteenable_o    => hr_byteenable,
         hr_burstcount_o    => hr_burstcount,
         hr_readdata_i      => hr_readdata,
         hr_readdatavalid_i => hr_readdatavalid,
         hr_waitrequest_i   => hr_waitrequest
      ); -- i_dut

   -----------------------------------------------------------------------------
   -- Device bus: devices act on the falling edge, as in the framework
   -----------------------------------------------------------------------------

   ramrom_rdata <= x"00" & vram_q(7 downto 0) when ramrom_dev = x"0000" or ramrom_dev = x"0001" else
                   dev_q                      when ramrom_dev = x"0100" else
                   x"EEEE";
   ramrom_wait  <= '1' when ramrom_dev = x"0100" and ramrom_ce = '1' and dev_done = '0' else '0';

   p_vram : process (clk50)
      variable a : natural;
   begin
      if falling_edge(clk50) then
         a := to_integer(unsigned(ramrom_addr(10 downto 0)));
         if ramrom_ce = '1' and ramrom_we = '1' then
            if ramrom_dev = x"0000" then
               vram_data(a) <= x"00" & ramrom_wdata(7 downto 0);
            elsif ramrom_dev = x"0001" then
               vram_attr(a) <= x"00" & ramrom_wdata(7 downto 0);
            end if;
         end if;
         if ramrom_dev = x"0001" then
            vram_q <= vram_attr(a);
         else
            vram_q <= vram_data(a);
         end if;
      end if;
   end process p_vram;

   -- Each access waits C_DEV_WAIT clocks, then reads or writes once
   p_dev : process (clk50)
      variable a : natural;
   begin
      if falling_edge(clk50) then
         a := to_integer(unsigned(ramrom_addr(7 downto 0)));
         if ramrom_ce = '0' or ramrom_dev /= x"0100" then
            dev_done  <= '0';
            dev_count <= 0;
         elsif dev_done = '0' then
            if dev_count = C_DEV_WAIT - 1 then
               if ramrom_we = '1' then
                  dev_ram(a) <= ramrom_wdata;
               end if;
               dev_q    <= dev_ram(a);
               dev_done <= '1';
            else
               dev_count <= dev_count + 1;
            end if;
         end if;
      end if;
   end process p_dev;

   -----------------------------------------------------------------------------
   -- HyperRAM: one read burst at a time, waitrequest now and then
   -----------------------------------------------------------------------------

   p_hr : process
      type t_mem is array (0 to C_HR_WORDS - 1) of std_logic_vector(15 downto 0);
      variable mem    : t_mem := (others => x"0000");
      variable lfsr   : std_logic_vector(7 downto 0) := x"5A";
      variable a      : natural := 0;
      variable beats  : natural := 0;
      variable count  : natural;
   begin
      wait until rising_edge(clk100);
      hr_readdatavalid <= '0';
      lfsr := lfsr(6 downto 0) & (lfsr(7) xor lfsr(5) xor lfsr(4) xor lfsr(3));

      if hr_waitrequest = '0' and hr_write = '1' then
         if beats = 0 then
            a     := to_integer(unsigned(hr_address)) mod C_HR_WORDS;
            beats := to_integer(unsigned(hr_burstcount));
         end if;
         if hr_byteenable(0) = '1' then
            mem(a)(7 downto 0) := hr_writedata(7 downto 0);
         end if;
         if hr_byteenable(1) = '1' then
            mem(a)(15 downto 8) := hr_writedata(15 downto 8);
         end if;
         if G_TRACE_HR then
            report "HR write " & to_hstring(to_unsigned(a, 20)) & " = " & to_hstring(hr_writedata) &
                   " be " & to_string(hr_byteenable);
         end if;
         a     := (a + 1) mod C_HR_WORDS;
         beats := beats - 1;
      elsif hr_waitrequest = '0' and hr_read = '1' then
         assert beats = 0 report "read inside a write burst" severity failure;
         a     := to_integer(unsigned(hr_address)) mod C_HR_WORDS;
         count := to_integer(unsigned(hr_burstcount));
         assert count >= 1 and count <= 16 report "HyperRAM burst of " & integer'image(count) severity failure;
         if G_TRACE_HR then
            report "HR read " & to_hstring(to_unsigned(a, 20)) & " x " & integer'image(count);
         end if;
         hr_waitrequest <= '1';
         for i in 1 to C_HR_LATENCY loop
            wait until rising_edge(clk100);
         end loop;
         for i in 0 to count - 1 loop
            hr_readdata      <= mem((a + i) mod C_HR_WORDS);
            hr_readdatavalid <= '1';
            wait until rising_edge(clk100);
         end loop;
         hr_readdatavalid <= '0';
      end if;

      hr_waitrequest <= lfsr(0) and lfsr(2);    -- busy a quarter of the time
   end process p_hr;

   -----------------------------------------------------------------------------
   -- Checks
   -----------------------------------------------------------------------------

   p_check : process
   begin
      wait until reset_n = '1';
      while now < C_TIMEOUT loop
         wait for 10 us;
         exit when str_at(vram_data, 45, "PASS") or str_at(vram_data, 45, "FAIL");
      end loop;

      assert str_at(vram_data, 45, "PASS")
         report "selftest did not pass: row 1 reads " &
                character'val(to_integer(unsigned(vram_data(45)(7 downto 0)))) &
                character'val(to_integer(unsigned(vram_data(46)(7 downto 0)))) &
                character'val(to_integer(unsigned(vram_data(47)(7 downto 0)))) &
                character'val(to_integer(unsigned(vram_data(48)(7 downto 0)))) & " " &
                character'val(to_integer(unsigned(vram_data(50)(7 downto 0)))) &
                character'val(to_integer(unsigned(vram_data(51)(7 downto 0))))
         severity error;
      assert control_d(127 downto 96) = x"DEADBEEF" report "control_d" severity error;
      assert control_m(255 downto 224) = x"12345678" report "control_m" severity error;
      assert csr_reset = '1' report "core left reset" severity error;

      if str_at(vram_data, 45, "PASS") and control_d(127 downto 96) = x"DEADBEEF" and
         control_m(255 downto 224) = x"12345678" and csr_reset = '1' then
         report "ALL CHECKS PASSED";
      end if;
      done <= true;
      wait;
   end process p_check;

end architecture sim;
