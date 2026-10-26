----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Testbench for cartridge I/O in FAST mode. At 2 MHz the VIC leaves PHI1 to the CPU, so the
-- core also decodes $DExx/$DFxx there; a device that acts on its select's rising edge must
-- still see each access once.
--
-- main boots as in tb_c128_boot, with the REU (2 MB, on a memory model) and the SwiftLink at
-- $DE00. 8502 code run from the cartridge port's /NMI starts an REU transfer and reads the
-- REU's status at 1 MHz, then again at 2 MHz; reading the status clears it, so a second
-- strobe would hand the CPU a cleared one. Then it reads and writes the 6551 at 2 MHz and reads
-- it at 1 MHz, and the modem's select must rise once for each.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

library work;
use work.globals.all;
use work.sim_support_pkg.all;
use work.boot_paths_pkg.all;

entity tb_fast_io is
end entity tb_fast_io;

architecture sim of tb_fast_io is

   constant C_BOOT0_SIZE   : natural := 73728;
   constant C_MENU_HOLD    : time    := 500 us;
   constant C_MAIN_PERIOD  : time    := 1 sec / CORE_CLK_SPEED;
   constant C_QNICE_PERIOD : time    := 20 ns;

   function osm_bits return std_logic_vector is
      variable v : std_logic_vector(255 downto 0) := (others => '0');
   begin
      v(C_MENU_REU_2M)     := '1';
      v(C_MENU_MODEM_DE00) := '1';
      return v;
   end function osm_bits;

   signal osm              : std_logic_vector(255 downto 0) := osm_bits;
   signal clk_main         : std_logic := '0';
   signal clk_qnice        : std_logic := '0';
   signal reset_hard       : std_logic := '1';

   signal ram_addr         : unsigned(17 downto 0);
   signal ram_data_to      : unsigned(7 downto 0);
   signal ram_we           : std_logic;
   signal ram_data_from    : std_logic_vector(7 downto 0);
   signal ram_a_addr       : std_logic_vector(17 downto 0) := (others => '0');
   signal ram_a_data       : std_logic_vector(7 downto 0) := (others => '0');
   signal ram_a_we         : std_logic := '0';
   signal ram_a_q          : std_logic_vector(7 downto 0);
   signal sys_rom_addr     : std_logic_vector(16 downto 0);
   signal sys_rom_data     : std_logic_vector(7 downto 0);
   signal qnice_sysrom_addr: std_logic_vector(16 downto 0) := (others => '0');
   signal sysrom_data_a    : std_logic_vector(7 downto 0) := (others => '0');
   signal sysrom_we_a      : std_logic := '0';
   signal rom_mem          : byte_rom_t(0 to C_BOOT0_SIZE - 1) := (others => (others => '0'));
   signal rom_ready        : std_logic := '0';
   signal sysrom_ready     : std_logic := '0';
   signal boot_z80_n       : std_logic;

   -- The empty slot: main reads back what it drives, else the pull-ups
   signal cart_ctrl_oe     : std_logic;
   signal cart_rw_o        : std_logic;
   signal cart_addr_oe     : std_logic;
   signal cart_a_o         : unsigned(15 downto 0);
   signal cart_data_oe     : std_logic;
   signal cart_d_o         : unsigned(7 downto 0);
   signal in_rw            : std_logic;
   signal in_a             : unsigned(15 downto 0);
   signal in_d             : unsigned(7 downto 0);
   signal tb_nmi_n         : std_logic := '1';

   -- The REU's memory
   signal reu_write        : std_logic;
   signal reu_read         : std_logic;
   signal reu_address      : std_logic_vector(31 downto 0);
   signal reu_writedata    : std_logic_vector(15 downto 0);
   signal reu_byteenable   : std_logic_vector(1 downto 0);
   signal reu_burstcount   : std_logic_vector(7 downto 0);
   signal reu_readdata     : std_logic_vector(15 downto 0);
   signal reu_readdatavalid: std_logic;
   signal reu_waitrequest  : std_logic;
   signal mem_rst          : std_logic := '1';

   -- The 6551's select, counted while the /NMI code runs
   signal modem_cs         : std_logic;
   signal modem_we         : std_logic;
   signal counting         : boolean := false;
   signal n_cs_rd          : natural := 0;
   signal n_cs_wr          : natural := 0;

begin

   clk_main  <= not clk_main  after C_MAIN_PERIOD / 2;
   clk_qnice <= not clk_qnice after C_QNICE_PERIOD / 2;

   u_main_ram : entity work.dualport_2clk_ram
      generic map (
         ADDR_WIDTH => 18,
         DATA_WIDTH => 8,
         FALLING_A  => true
      )
      port map (
         clock_a         => clk_qnice,
         address_a       => ram_a_addr,
         do_latch_addr_a => '0',
         data_a          => ram_a_data,
         wren_a          => ram_a_we,
         q_a             => ram_a_q,
         clock_b         => clk_main,
         address_b       => std_logic_vector(ram_addr),
         do_latch_addr_b => '0',
         data_b          => std_logic_vector(ram_data_to),
         wren_b          => ram_we,
         q_b             => ram_data_from
      );

   u_system_rom : entity work.dualport_2clk_ram
      generic map (
         ADDR_WIDTH => 17,
         DATA_WIDTH => 8,
         FALLING_A  => true
      )
      port map (
         clock_a         => clk_qnice,
         address_a       => qnice_sysrom_addr,
         do_latch_addr_a => '0',
         data_a          => sysrom_data_a,
         wren_a          => sysrom_we_a,
         q_a             => open,
         clock_b         => clk_main,
         address_b       => sys_rom_addr,
         do_latch_addr_b => '0',
         data_b          => (others => '0'),
         wren_b          => '0',
         q_b             => sys_rom_data
      );

   -- 1 MW covers a 2 MiB REU; the window's base bit (20) falls away
   u_reu_memory : entity work.avm_memory_pause
      generic map (
         G_REQ_PAUSE    => 2,
         G_RESP_PAUSE   => 2,
         G_ADDRESS_SIZE => 20,
         G_DATA_SIZE    => 16
      )
      port map (
         clk_i               => clk_main,
         rst_i               => mem_rst,
         avm_write_i         => reu_write,
         avm_read_i          => reu_read,
         avm_address_i       => reu_address(19 downto 0),
         avm_writedata_i     => reu_writedata,
         avm_byteenable_i    => reu_byteenable,
         avm_burstcount_i    => reu_burstcount,
         avm_readdata_o      => reu_readdata,
         avm_readdatavalid_o => reu_readdatavalid,
         avm_waitrequest_o   => reu_waitrequest
      );

   u_dut : entity work.main
      generic map (
         G_BOARD => "MEGA65_R6",
         G_VDNUM => 0
      )
      port map (
         clk_main_i           => clk_main,
         clk_vdc_i            => clk_main,
         clk_sd_i             => clk_qnice,
         qnice_vd_addr_i      => (others => '0'),
         qnice_vd_data_i      => (others => '0'),
         qnice_vd_data_o      => open,
         qnice_vd_ce_i        => '0',
         qnice_vd_we_i        => '0',
         drv_rom_loading_i    => '0',
         drv_rom_req_o        => open,
         drv_rom_addr_o       => open,
         drv_rom_data_i       => (others => '0'),
         drv_rom_wr_i         => '0',
         reset_soft_i         => '0',
         reset_hard_i         => reset_hard,
         pause_i              => '0',
         clk_main_speed_i     => CORE_CLK_SPEED,
         osm_control_i        => osm,
         f9_latched_o         => open,
         video_ce_o           => open,
         video_red_o          => open,
         video_green_o        => open,
         video_blue_o         => open,
         video_vs_o           => open,
         video_hs_o           => open,
         video_hblank_o       => open,
         video_vblank_o       => open,
         video_select_vdc_o   => open,
         vga_video_ce_o         => open,
         vga_video_ce_ovl_o     => open,
         vga_video_red_o        => open,
         vga_video_green_o      => open,
         vga_video_blue_o       => open,
         vga_video_vs_o         => open,
         vga_video_hs_o         => open,
         vga_video_hblank_o     => open,
         vga_video_vblank_o     => open,
         vga_video_select_vdc_o => open,
         audio_left_o         => open,
         audio_right_o        => open,
         drive_led_o          => open,
         drive_led_col_o      => open,
         boot_z80_n_o         => boot_z80_n,
         ram_addr_o           => ram_addr,
         ram_data_o           => ram_data_to,
         ram_we_o             => ram_we,
         ram_data_i           => unsigned(ram_data_from),
         sys_rom_addr_o       => sys_rom_addr,
         sys_rom_data_i       => sys_rom_data,
         reu_avm_write_o         => reu_write,
         reu_avm_read_o          => reu_read,
         reu_avm_address_o       => reu_address,
         reu_avm_writedata_o     => reu_writedata,
         reu_avm_byteenable_o    => reu_byteenable,
         reu_avm_burstcount_o    => reu_burstcount,
         reu_avm_readdata_i      => reu_readdata,
         reu_avm_readdatavalid_i => reu_readdatavalid,
         reu_avm_waitrequest_i   => reu_waitrequest,
         modem_cs_o           => modem_cs,
         modem_we_o           => modem_we,
         cart_en_o            => open,
         cart_phi2_o          => open,
         cart_dotclock_o      => open,
         cart_dma_i           => '1',
         cart_reset_oe_o      => open,
         cart_reset_i         => '1',
         cart_reset_o         => open,
         cart_game_oe_o       => open,
         cart_game_i          => '1',
         cart_game_o          => open,
         cart_exrom_oe_o      => open,
         cart_exrom_i         => '1',
         cart_exrom_o         => open,
         cart_nmi_oe_o        => open,
         cart_nmi_i           => tb_nmi_n,
         cart_nmi_o           => open,
         cart_irq_oe_o        => open,
         cart_irq_i           => '1',
         cart_irq_o           => open,
         cart_roml_oe_o       => open,
         cart_roml_i          => '1',
         cart_roml_o          => open,
         cart_romh_oe_o       => open,
         cart_romh_i          => '1',
         cart_romh_o          => open,
         cart_ctrl_oe_o       => cart_ctrl_oe,
         cart_ba_i            => '1',
         cart_rw_i            => in_rw,
         cart_io1_i           => '1',
         cart_io2_i           => '1',
         cart_ba_o            => open,
         cart_rw_o            => cart_rw_o,
         cart_io1_o           => open,
         cart_io2_o           => open,
         cart_addr_oe_o       => cart_addr_oe,
         cart_a_i             => in_a,
         cart_a_o             => cart_a_o,
         cart_data_oe_o       => cart_data_oe,
         cart_d_i             => in_d,
         cart_d_o             => cart_d_o,
         iec_hardware_port_en_i => '0',
         iec_reset_n_o        => open,
         iec_atn_n_o          => open,
         iec_clk_en_o         => open,
         iec_clk_n_o          => open,
         iec_clk_n_i          => '0',
         iec_data_en_o        => open,
         iec_data_n_o         => open,
         iec_data_n_i         => '0',
         iec_srq_en_o         => open,
         iec_srq_n_o          => open,
         iec_srq_n_i          => '0',
         kb_key_num_i         => 0,
         kb_key_pressed_n_i   => '1',
         kb_key_enabled_i     => '1',
         joy_1_up_n_i         => '1',
         joy_1_down_n_i       => '1',
         joy_1_left_n_i       => '1',
         joy_1_right_n_i      => '1',
         joy_1_fire_n_i       => '1',
         joy_2_up_n_i         => '1',
         joy_2_down_n_i       => '1',
         joy_2_left_n_i       => '1',
         joy_2_right_n_i      => '1',
         joy_2_fire_n_i       => '1',
         pot1_x_i             => (others => '0'),
         pot1_y_i             => (others => '0'),
         pot2_x_i             => (others => '0'),
         pot2_y_i             => (others => '0')
      );

   in_rw <= cart_rw_o when cart_ctrl_oe = '1' else '1';
   in_a  <= cart_a_o  when cart_addr_oe = '1' else (others => '1');
   in_d  <= cart_d_o  when cart_data_oe = '1' else (others => '1');

   --------------------------------------------------------------------------------
   -- Boot, as in tb_c128_boot
   --------------------------------------------------------------------------------
   rom_load_proc : process
   begin
      rom_mem   <= load_bin_rom(BOOT0_PATH, C_BOOT0_SIZE);
      rom_ready <= '1';
      wait;
   end process;

   sysrom_preload_proc : process (clk_qnice)
      variable idx : natural := 0;
   begin
      if rising_edge(clk_qnice) then
         sysrom_we_a <= '0';
         if rom_ready = '1' and sysrom_ready = '0' then
            if idx < C_BOOT0_SIZE then
               qnice_sysrom_addr <= std_logic_vector(to_unsigned(idx, 17));
               sysrom_data_a     <= rom_mem(idx);
               sysrom_we_a       <= '1';
               idx := idx + 1;
            else
               sysrom_ready <= '1';
            end if;
         end if;
      end if;
   end process;

   p_modem_cs : process (clk_main)
      variable last : std_logic := '0';
   begin
      if rising_edge(clk_main) then
         if counting and modem_cs = '1' and last = '0' then
            if modem_we = '1' then
               n_cs_wr <= n_cs_wr + 1;
            else
               n_cs_rd <= n_cs_rd + 1;
            end if;
         end if;
         last := modem_cs;
      end if;
   end process p_modem_cs;

   --------------------------------------------------------------------------------
   -- Test
   --------------------------------------------------------------------------------
   p_test : process
      variable errors : natural := 0;
      variable d      : std_logic_vector(7 downto 0);
      variable st1    : std_logic_vector(7 downto 0);
      variable st2    : std_logic_vector(7 downto 0);
      variable l      : line;

      procedure check(cond : boolean; msg : string) is
      begin
         if not cond then
            report msg severity error;
            errors := errors + 1;
         end if;
      end procedure check;

      -- RAM bank 0, through the BRAM's other port
      procedure ram_poke(addr : natural; data : std_logic_vector(7 downto 0)) is
      begin
         ram_a_addr <= std_logic_vector(to_unsigned(addr, 18));
         ram_a_data <= data;
         ram_a_we   <= '1';
         wait until rising_edge(clk_qnice);
         wait until rising_edge(clk_qnice);
         ram_a_we   <= '0';
         wait until rising_edge(clk_qnice);
      end procedure ram_poke;

      procedure ram_peek(addr : natural; data : out std_logic_vector(7 downto 0)) is
      begin
         ram_a_addr <= std_logic_vector(to_unsigned(addr, 18));
         wait until rising_edge(clk_qnice);
         wait until rising_edge(clk_qnice);
         wait until rising_edge(clk_qnice);
         data := ram_a_q;
      end procedure ram_peek;

      -- From /NMI through ($0318), in bank 15. $FF33 returns.
      type t_code is array (natural range <>) of std_logic_vector(7 downto 0);
      constant nmi_code : t_code := (
         x"A9", x"00", x"8D", x"30", x"D0",    -- lda #$00 / sta $d030: 1 MHz
         x"A9", x"80", x"8D", x"02", x"DF",    -- lda #$80 / sta $df02: C128 address $0c80
         x"A9", x"0C", x"8D", x"03", x"DF",    -- lda #$0c / sta $df03
         x"A9", x"00", x"8D", x"04", x"DF",    -- lda #$00 / sta $df04: REU address 0
         x"8D", x"05", x"DF",                  -- sta $df05
         x"8D", x"06", x"DF",                  -- sta $df06
         x"8D", x"08", x"DF",                  -- sta $df08: length 1
         x"A9", x"01", x"8D", x"07", x"DF",    -- lda #$01 / sta $df07
         x"A9", x"90", x"8D", x"01", x"DF",    -- lda #$90 / sta $df01: C128 -> REU, now
         x"AD", x"00", x"DF",                  -- lda $df00: status at 1 MHz
         x"8D", x"90", x"0C",                  -- sta $0c90
         x"A9", x"90", x"8D", x"01", x"DF",    -- lda #$90 / sta $df01: again
         x"A9", x"01", x"8D", x"30", x"D0",    -- lda #$01 / sta $d030: 2 MHz
         x"AD", x"00", x"DF",                  -- lda $df00: status at 2 MHz
         x"8D", x"91", x"0C",                  -- sta $0c91
         x"AD", x"01", x"DE",                  -- lda $de01: 6551 status at 2 MHz
         x"8D", x"03", x"DE",                  -- sta $de03: 6551 control at 2 MHz
         x"A9", x"00", x"8D", x"30", x"D0",    -- lda #$00 / sta $d030: 1 MHz
         x"AD", x"01", x"DE",                  -- lda $de01: 6551 status at 1 MHz
         x"EE", x"9F", x"0C",                  -- inc $0c9f
         x"4C", x"33", x"FF");                 -- jmp $ff33
   begin
      wait until sysrom_ready = '1';
      reset_hard <= '1';
      wait for C_MENU_HOLD;
      reset_hard <= '0';
      mem_rst    <= '0';
      if boot_z80_n /= '0' then
         wait until boot_z80_n = '0';
      end if;
      wait until boot_z80_n = '1';
      report "8502 has the bus at " & time'image(now);
      wait for 20 ms;   -- the kernal's init is done with $0318

      for i in nmi_code'range loop
         ram_poke(16#0C00# + i, nmi_code(i));
      end loop;
      ram_poke(16#0C90#, x"EE");
      ram_poke(16#0C91#, x"EE");
      ram_poke(16#0C9F#, x"00");
      ram_poke(16#0318#, x"00");
      ram_poke(16#0319#, x"0C");

      counting <= true;
      tb_nmi_n <= '0';
      wait for 3 us;
      tb_nmi_n <= '1';
      wait for 500 us;
      counting <= false;

      ram_peek(16#0C9F#, d);
      check(d = x"01", "the /NMI code ran " & integer'image(to_integer(unsigned(d))) & " times");
      ram_peek(16#0C90#, st1);
      ram_peek(16#0C91#, st2);
      write(l, string'("REU status read at 1 MHz $") & to_hstring(st1) & ", at 2 MHz $" & to_hstring(st2) &
               "; the 6551's select rose for " & integer'image(n_cs_rd) & " reads and " &
               integer'image(n_cs_wr) & " writes (the code made 2 and 1)");
      writeline(output, l);
      check(st1 = x"50", "REU status at 1 MHz: $" & to_hstring(st1) & ", not $50 (end of block)");
      check(st2 = x"50", "REU status at 2 MHz: $" & to_hstring(st2) & ", not $50 (end of block)");
      check(n_cs_rd = 2, "the 6551's select rose for " & integer'image(n_cs_rd) & " reads, not 2");
      check(n_cs_wr = 1, "the 6551's select rose for " & integer'image(n_cs_wr) & " writes, not 1");

      if errors = 0 then
         write(l, string'("ALL CHECKS PASSED"));
      else
         write(l, integer'image(errors) & string'(" CHECKS FAILED"));
      end if;
      writeline(output, l);
      std.env.stop;
      wait;
   end process p_test;

end architecture sim;
