----------------------------------------------------------------------------------
-- C128 boot simulation.
--
-- Instantiates main + BRAM RAM/ROM preloaded with boot0.rom and checks that the core
-- boots: the Z80 executes the boot ROM and hands control over to the 8502 (boot_z80_n
-- transitions 0 -> 1) while writing to RAM. This is the minimal regression that guards
-- the Z80 first-fetch fix (see cpu_z80.vhd / doc/z80-first-fetch-boot-hang.md).
----------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.globals.all;
use work.sim_support_pkg.all;
use work.boot_paths_pkg.all;

entity tb_c128_boot is
end entity tb_c128_boot;

architecture sim of tb_c128_boot is

   constant C_BOOT0_SIZE   : natural := 73728;
   constant C_SIM_TIME     : time    := 20 ms;
   constant C_SESSION_ID   : string  := "boot";
   constant C_RUN_ID       : string  := "sim-boot";
   -- M2M holds the core in reset until menu exit; use a short stand-in before CSR_UN_RESET.
   constant C_MENU_HOLD    : time    := 500 us;

   constant C_MAIN_HZ      : natural := CORE_CLK_SPEED;
   constant C_MAIN_PERIOD  : time    := 1 sec / C_MAIN_HZ;
   constant C_QNICE_PERIOD : time    := 20 ns; -- 50 MHz, matches M2M qnice_clk

   signal clk_main         : std_logic := '0';
   signal clk_qnice        : std_logic := '0';
   signal qnice_sysrom_addr: std_logic_vector(16 downto 0) := (others => '0');
   signal reset_soft       : std_logic := '0';
   signal reset_hard       : std_logic := '1';
   signal pause            : std_logic := '0';

   signal ram_addr         : unsigned(17 downto 0);
   signal ram_data_to      : unsigned(7 downto 0);
   signal ram_we           : std_logic;
   signal ram_data_from    : std_logic_vector(7 downto 0);
   signal sys_rom_addr     : std_logic_vector(16 downto 0);
   signal sys_rom_data     : std_logic_vector(7 downto 0);
   signal sysrom_data_a    : std_logic_vector(7 downto 0) := (others => '0');
   signal sysrom_we_a      : std_logic := '0';

   signal boot_z80_n       : std_logic;

   signal rom_mem          : byte_rom_t(0 to C_BOOT0_SIZE - 1) := (others => (others => '0'));
   signal rom_ready        : std_logic := '0';
   signal sysrom_ready     : std_logic := '0';

   signal sim_done         : boolean := false;
   signal core_released    : boolean := false;
   signal ram_we_count     : natural := 0;
   signal z80_handoff_seen : boolean := false;

begin

   clk_main  <= not clk_main  after C_MAIN_PERIOD / 2;
   clk_qnice <= not clk_qnice after C_QNICE_PERIOD / 2;

   -- Main RAM (1-cycle read latency, matches the hardware BRAM).
   u_main_ram : entity work.dualport_2clk_ram
      generic map (
         ADDR_WIDTH => 18,
         DATA_WIDTH => 8,
         FALLING_A  => true
      )
      port map (
         clock_a         => clk_qnice,
         address_a       => (others => '0'),
         do_latch_addr_a => '0',
         data_a          => (others => '0'),
         wren_a          => '0',
         q_a             => open,
         clock_b         => clk_main,
         address_b       => std_logic_vector(ram_addr),
         do_latch_addr_b => '0',
         data_b          => std_logic_vector(ram_data_to),
         wren_b          => ram_we,
         q_b             => ram_data_from
      );

   -- System ROM (1-cycle read latency, matches the hardware BRAM), preloaded via QNICE port A.
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

   u_dut : entity work.main
      generic map (
         G_VDNUM => 0
      )
      port map (
         clk_main_i           => clk_main,
         clk_vdc_i            => clk_main,
         reset_soft_i         => reset_soft,
         reset_hard_i         => reset_hard,
         pause_i              => pause,
         clk_main_speed_i     => CORE_CLK_SPEED,
         video_ce_o           => open,
         video_ce_ovl_o       => open,
         video_red_o          => open,
         video_green_o        => open,
         video_blue_o         => open,
         video_vs_o           => open,
         video_hs_o           => open,
         video_hblank_o       => open,
         video_vblank_o       => open,
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
         cart_reset_i         => '1',
         cart_reset_o         => open,
         cart_dma_i           => '0',
         cart_game_i          => '1',
         cart_exrom_i         => '1',
         cart_nmi_i           => '1',
         cart_irq_i           => '1',
         cart_game_o          => open,
         cart_exrom_o         => open,
         cart_nmi_o           => open,
         cart_irq_o           => open,
         cart_roml_o          => open,
         cart_romh_o          => open,
         cart_io1_o           => open,
         cart_io2_o           => open,
         iec_reset_n_o        => open,
         iec_atn_n_o          => open,
         iec_clk_en_o         => open,
         iec_clk_n_o          => open,
         iec_clk_n_i          => '1',
         iec_data_en_o        => open,
         iec_data_n_o         => open,
         iec_data_n_i         => '1',
         iec_srq_en_o         => open,
         iec_srq_n_o          => open,
         iec_srq_n_i          => '1',
         kb_key_num_i         => 0,
         kb_key_pressed_n_i   => '1',
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

   rom_load_proc : process
      variable loaded : byte_rom_t(0 to C_BOOT0_SIZE - 1);
   begin
      loaded := load_bin_rom(BOOT0_PATH, C_BOOT0_SIZE);
      rom_mem   <= loaded;
      rom_ready <= '1';
      wait;
   end process;

   sysrom_preload_proc : process (clk_qnice)
      variable idx : natural := 0;
   begin
      if rising_edge(clk_qnice) then
         sysrom_we_a <= '0';
         qnice_sysrom_addr <= (others => '0');
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

   stim_proc : process
   begin
      wait until sysrom_ready = '1';
      log_boot_json(LOG_PATH, C_SESSION_ID, C_RUN_ID, "SIM", "tb_c128_boot:stim",
         "simulation start", "{}");

      -- M2M RESET_KEEP: core held in hard reset until menu exit (main_reset_m2m_i='1').
      reset_hard <= '1';
      reset_soft <= '0';
      wait for C_MENU_HOLD;
      -- shell.asm START_CONNECT: M2M$CSR_UN_RESET after welcome/menu.
      reset_hard <= '0';
      core_released <= true;
      wait for C_SIM_TIME;
      sim_done <= true;
      wait;
   end process;

   -- Count RAM write pulses and latch the Z80 -> 8502 handoff (boot_z80_n rising 0 -> 1).
   monitor_proc : process (clk_main)
      variable last_we    : std_logic := '0';
      variable last_z80_n : std_logic := '0';
   begin
      if rising_edge(clk_main) then
         if core_released then
            if ram_we = '1' and last_we = '0' then
               ram_we_count <= ram_we_count + 1;
            end if;
            if boot_z80_n = '1' and last_z80_n = '0' then
               z80_handoff_seen <= true;
            end if;
         end if;
         last_we    := ram_we;
         last_z80_n := boot_z80_n;
      end if;
   end process;

   summary_proc : process
      variable pass : boolean;
   begin
      wait until sim_done;
      pass := ram_we_count > 0 and z80_handoff_seen and boot_z80_n = '1';
      log_boot_json(
         LOG_PATH, C_SESSION_ID, C_RUN_ID, "H1",
         "tb_c128_boot:summary",
         "simulation finished",
         "{""ram_we_count"":" & integer'image(ram_we_count) &
         ",""z80_handoff_seen"":" & boolean'image(z80_handoff_seen) &
         ",""final_z80_n"":""" & std_logic'image(boot_z80_n) &
         """,""pass"":" & boolean'image(pass) & "}"
      );
      if pass then
         report "PASS: Z80 handed off to 8502 (ram_we_count=" &
                integer'image(ram_we_count) & ")" severity note;
      else
         report "FAIL: no Z80->8502 handoff within " & time'image(C_SIM_TIME) &
                " (ram_we_count=" & integer'image(ram_we_count) &
                ", handoff_seen=" & boolean'image(z80_handoff_seen) & ")"
            severity error;
      end if;
      wait;
   end process;

end architecture sim;
