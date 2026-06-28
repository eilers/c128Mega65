----------------------------------------------------------------------------------
-- C128 boot simulation: main + BRAM RAM/ROM + boot0.rom, monitor ramWE / boot stage.
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
   constant C_SESSION_ID   : string  := "b576b7";
   constant C_RUN_ID       : string  := "sim-boot-41";
   constant C_SNAPSHOT_US  : natural := 500;
   -- M2M holds core in reset until menu exit; use a short stand-in before CSR_UN_RESET.
   constant C_MENU_HOLD    : time    := 500 us;

   constant C_MAIN_HZ      : natural := CORE_CLK_SPEED;
   constant C_MAIN_PERIOD  : time    := 1 sec / C_MAIN_HZ;
   constant C_VDC_PERIOD   : time    := 31.25 ns;
   constant C_QNICE_PERIOD : time    := 20 ns; -- 50 MHz, matches M2M qnice_clk

   signal clk_main         : std_logic := '0';
   signal clk_qnice        : std_logic := '0';
   signal clk_vdc          : std_logic := '0';
   signal qnice_ram_addr   : std_logic_vector(17 downto 0) := (others => '0');
   signal qnice_sysrom_addr: std_logic_vector(16 downto 0) := (others => '0');
   signal reset_soft       : std_logic := '0';
   signal reset_hard       : std_logic := '1';
   signal pause            : std_logic := '0';

   signal ram_addr         : unsigned(17 downto 0);
   signal ram_data_to      : unsigned(7 downto 0);
   signal ram_we           : std_logic;
   signal ram_data_from    : unsigned(7 downto 0);
   signal sys_rom_addr     : std_logic_vector(16 downto 0);
   signal sys_rom_data     : std_logic_vector(7 downto 0);
   signal sysrom_data_a    : std_logic_vector(7 downto 0) := (others => '0');
   signal sysrom_we_a      : std_logic := '0';

   signal boot_stage       : std_logic_vector(2 downto 0);
   signal boot_z80_n       : std_logic;
   signal boot_ram_we      : std_logic;
   signal boot_dbg_probe   : std_logic_vector(6 downto 0);
   signal boot_dbg_z80_rd  : std_logic;
   signal boot_dbg_z80_we  : std_logic;
   signal boot_dbg_z80_sys : std_logic;
   signal boot_dbg_vec_v   : std_logic;
   signal boot_dbg_vec_b   : std_logic_vector(7 downto 0);
   signal boot_dbg_vic_pixel_ce : std_logic;
   signal boot_dbg_ram_hold_tick: std_logic;
   signal boot_dbg_vic_fetch   : std_logic;
   signal boot_dbg_vic_enable  : std_logic;
   signal boot_dbg_vic_aec     : std_logic;
   signal boot_dbg_vic_pipe    : std_logic_vector(7 downto 0);
   signal boot_dbg_ram_din     : std_logic_vector(7 downto 0);
   signal boot_dbg_core_ram_addr : unsigned(17 downto 0);
   signal boot_dbg_bram_addr   : unsigned(17 downto 0);
   signal video_ce_sig     : std_logic;
   signal drive_led_col    : std_logic_vector(23 downto 0);

   signal rom_mem          : byte_rom_t(0 to C_BOOT0_SIZE - 1) := (others => (others => '0'));
   signal rom_ready        : std_logic := '0';
   signal sysrom_ready     : std_logic := '0';

   signal sim_done         : boolean := false;
   signal core_released    : boolean := false;
   signal ram_we_count     : natural := 0;
   signal max_boot_stage   : unsigned(2 downto 0) := (others => '0');
   signal max_ram_addr     : unsigned(17 downto 0) := (others => '0');
   signal max_sysrom_addr  : unsigned(16 downto 0) := (others => '0');
   signal dbg_sysrom_cs    : std_logic;
   signal dbg_ram_ce       : std_logic;
   signal dbg_core_running : std_logic;
   signal sysrom_cs_hits   : natural := 0;
   signal ram_ce_hits      : natural := 0;
   signal pixel_ce_count   : natural := 0;
   signal ce_mismatch_count: natural := 0;
   signal free_div_ce      : unsigned(1 downto 0) := "00";
   signal free_div_mismatch: natural := 0;
   signal ram_hold_ticks   : natural := 0;
   signal vic_fetch_count  : natural := 0;
   signal vic_consume_count: natural := 0;
   signal vic_timing_mismatch: natural := 0;
   signal vic_pipe_mismatch: natural := 0;
   signal vic_screen_mismatch: natural := 0;
   signal z80_ram_rd_checks : natural := 0;
   signal z80_ram_rd_mismatch: natural := 0;
   signal z80_mmu_stuck_samples : natural := 0;

   type shadow_ram_t is array (0 to 131071) of unsigned(7 downto 0);
   signal shadow_ram       : shadow_ram_t := (others => (others => '0'));
   signal bram_addr_reg    : unsigned(17 downto 0) := (others => '0');
   signal ram_data_from_bram : std_logic_vector(7 downto 0);
   -- H-V23 bridge model (sim only, when C_MEM_BRIDGE=true)
   signal bram_rd_addr_held  : unsigned(17 downto 0) := (others => '0');
   signal bram_port_addr     : unsigned(17 downto 0);
   signal ram_data_from_held : unsigned(7 downto 0) := (others => '0');
   signal dbg_ram_ce_d       : std_logic := '0';
   signal dbg_ram_ce_d2      : std_logic := '0';

begin

   clk_main  <= not clk_main  after C_MAIN_PERIOD / 2;
   clk_qnice <= not clk_qnice after C_QNICE_PERIOD / 2;
   clk_vdc   <= not clk_vdc   after C_VDC_PERIOD / 2;

   ram_data_from <= ram_data_from_held when C_MEM_BRIDGE
                    else unsigned(ram_data_from_bram);

   -- Baseline: wire the BRAM exactly like mega65.vhd (address_b <= main_ram_addr,
   -- q_b -> main_ram_data_from) so the VIC read datapath matches hardware.
   bram_port_addr <= ram_addr when (not C_MEM_BRIDGE) else
                     ram_addr when ram_we = '1' else
                     bram_rd_addr_held;

   mem_bridge_proc : process (clk_main)
   begin
      if C_MEM_BRIDGE then
         if rising_edge(clk_main) then
            if dbg_ram_ce = '1' and ram_we = '0' and dbg_ram_ce_d = '0' then
               bram_rd_addr_held <= boot_dbg_core_ram_addr;
            end if;
            dbg_ram_ce_d2 <= dbg_ram_ce_d;
            dbg_ram_ce_d  <= dbg_ram_ce;
            if dbg_ram_ce_d2 = '1' and ram_we = '0' then
               ram_data_from_held <= unsigned(ram_data_from_bram);
            end if;
         end if;
      end if;
   end process mem_bridge_proc;

   u_main_ram : entity work.dualport_2clk_ram
      generic map (
         ADDR_WIDTH => 18,
         DATA_WIDTH => 8,
         FALLING_A  => true
      )
      port map (
         clock_a         => clk_qnice,
         address_a       => qnice_ram_addr,
         do_latch_addr_a => '0',
         data_a          => (others => '0'),
         wren_a          => '0',
         q_a             => open,
         clock_b         => clk_main,
         address_b       => std_logic_vector(bram_port_addr),
         do_latch_addr_b => '0',
         data_b          => std_logic_vector(ram_data_to),
         wren_b          => ram_we,
         q_b             => ram_data_from_bram
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

   u_dut : entity work.main
      generic map (
         G_VDNUM => 0
      )
      port map (
         clk_main_i           => clk_main,
         clk_vdc_i            => clk_vdc,
         reset_soft_i         => reset_soft,
         reset_hard_i         => reset_hard,
         pause_i              => pause,
         clk_main_speed_i     => CORE_CLK_SPEED,
         video_ce_o           => video_ce_sig,
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
         drive_led_col_o      => drive_led_col,
         boot_stage_o         => boot_stage,
         boot_led_col_o       => open,
         boot_z80_n_o         => boot_z80_n,
         boot_ram_we_o        => boot_ram_we,
         boot_dbg_probe_o     => boot_dbg_probe,
         boot_dbg_z80_rd_o    => boot_dbg_z80_rd,
         boot_dbg_z80_we_o    => boot_dbg_z80_we,
         boot_dbg_z80_sysrom_o=> boot_dbg_z80_sys,
         boot_dbg_vec_valid_o => boot_dbg_vec_v,
         boot_dbg_vec_byte_o  => boot_dbg_vec_b,
         boot_dbg_sysrom_cs_o => dbg_sysrom_cs,
         boot_dbg_ram_ce_o    => dbg_ram_ce,
         boot_dbg_core_run_o  => dbg_core_running,
         boot_dbg_vic_pixel_ce_o => boot_dbg_vic_pixel_ce,
         boot_dbg_ram_hold_tick_o => boot_dbg_ram_hold_tick,
         boot_dbg_vic_fetch_o    => boot_dbg_vic_fetch,
         boot_dbg_vic_enable_o   => boot_dbg_vic_enable,
         boot_dbg_vic_aec_o      => boot_dbg_vic_aec,
         boot_dbg_vic_pipe_o     => boot_dbg_vic_pipe,
         boot_dbg_ram_din_o      => boot_dbg_ram_din,
         boot_dbg_core_ram_addr_o => boot_dbg_core_ram_addr,
         boot_dbg_bram_addr_o    => boot_dbg_bram_addr,
         boot_pwr_hint_o      => open,
         ram_addr_o           => ram_addr,
         ram_data_o           => ram_data_to,
         ram_we_o             => ram_we,
         ram_data_i           => ram_data_from,
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
         iec_atn_n_o          => open,
         iec_clk_n_o          => open,
         iec_clk_n_i          => '1',
         iec_data_n_o         => open,
         iec_data_n_i         => '1',
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

   snapshot_proc : process
      variable t_us : natural := 0;
   begin
      wait until core_released;
      while not sim_done loop
         log_boot_json(
            LOG_PATH, C_SESSION_ID, C_RUN_ID, "H2",
            "tb_c128_boot:snapshot",
            "periodic sample",
            "{""t_us"":" & integer'image(t_us) &
            ",""boot_stage"":" & integer'image(to_integer(unsigned(boot_stage))) &
            ",""z80_n"":""" & std_logic'image(boot_z80_n) &
            ",""dbg_probe"":" & integer'image(to_integer(unsigned(boot_dbg_probe))) &
            """,""z80_rd"":""" & std_logic'image(boot_dbg_z80_rd) &
            """,""z80_we"":""" & std_logic'image(boot_dbg_z80_we) &
            """,""z80_sysrom"":""" & std_logic'image(boot_dbg_z80_sys) &
            """,""vec_valid"":""" & std_logic'image(boot_dbg_vec_v) &
            """,""vec_byte"":" & integer'image(to_integer(unsigned(boot_dbg_vec_b))) &
            ",""ram_addr"":" & integer'image(to_integer(ram_addr)) &
            ",""sys_rom_addr"":" & integer'image(to_integer(unsigned(sys_rom_addr))) &
            ",""ram_we"":""" & std_logic'image(ram_we) &
            """,""led_r"":" & integer'image(to_integer(unsigned(drive_led_col(23 downto 16)))) &
            ",""led_g"":" & integer'image(to_integer(unsigned(drive_led_col(15 downto 8)))) &
            ",""led_b"":" & integer'image(to_integer(unsigned(drive_led_col(7 downto 0)))) & "}"
         );
         t_us := t_us + C_SNAPSHOT_US;
         wait for C_SNAPSHOT_US * 1 us;
      end loop;
      wait;
   end process;

   stage_watch_proc : process (clk_main)
      variable last_stage : std_logic_vector(2 downto 0) := (others => '0');
   begin
      if rising_edge(clk_main) then
         if boot_stage /= last_stage then
            log_boot_json(
               LOG_PATH, C_SESSION_ID, C_RUN_ID, "H3",
               "tb_c128_boot:stage",
               "boot stage change",
               "{""stage"":" & integer'image(to_integer(unsigned(boot_stage))) &
               ",""z80_rd"":""" & std_logic'image(boot_dbg_z80_rd) &
               """,""z80_sysrom"":""" & std_logic'image(boot_dbg_z80_sys) &
               """,""vec_valid"":""" & std_logic'image(boot_dbg_vec_v) & """}"
            );
            last_stage := boot_stage;
         end if;
      end if;
   end process;

   -- Shadow RAM + BRAM address register (1-cycle read latency, matches tdp_ram).
   shadow_bram_proc : process (clk_main)
   begin
      if rising_edge(clk_main) then
         if ram_we = '1' then
            shadow_ram(to_integer(ram_addr)) <= ram_data_to;
         end if;
         bram_addr_reg <= ram_addr;
      end if;
   end process;

   -- H15 gate: Z80 RAM read data must match BRAM q for the registered BRAM address.
   z80_ram_rd_proc : process (clk_main)
   begin
      if rising_edge(clk_main) then
         if boot_z80_n = '0' and dbg_ram_ce = '1' and ram_we = '0' and dbg_sysrom_cs = '0' then
            z80_ram_rd_checks <= z80_ram_rd_checks + 1;
            if ram_data_from /= shadow_ram(to_integer(bram_addr_reg)) then
               z80_ram_rd_mismatch <= z80_ram_rd_mismatch + 1;
            end if;
         end if;
         -- Stage 101 + Z80 active is normal mid-boot; log only (see final_boot_stage in summary).
         if boot_stage = "101" and boot_z80_n = '0' then
            z80_mmu_stuck_samples <= z80_mmu_stuck_samples + 1;
         end if;
      end if;
   end process;

   -- VIC RAM-read correctness gate.
   -- On a VIC RAM-read launch (boot_dbg_vic_fetch) latch the requested address.
   -- On the next VIC data-sample strobe (boot_dbg_vic_enable) the byte the VIC
   -- actually receives (boot_dbg_ram_din) must equal the true RAM content
   -- (shadow_ram) at that address. A mismatch is exactly the "snow" defect:
   -- the VIC sampling a stale byte instead of the screen-matrix byte it fetched.
   vic_rd_check_proc : process (clk_main)
      variable pend   : std_logic := '0';
      variable addr_l : unsigned(17 downto 0) := (others => '0');
      variable idx    : integer;
   begin
      if rising_edge(clk_main) then
         if core_released then
            if boot_dbg_vic_enable = '1' and pend = '1' then
               vic_consume_count <= vic_consume_count + 1;
               idx := to_integer(addr_l) mod 131072;
               -- boot_dbg_vic_pipe carries the dedicated VIC read data (vic_ram_din).
               -- Skip VIC idle accesses (low 14 bits = $3FFF): the VIC reads $3FFF when
               -- it has no real fetch (border/blank); that byte is never displayed text,
               -- so it must not gate the screen-correctness check.
               if not is_x(boot_dbg_vic_pipe) and (addr_l(13 downto 0) /= "11111111111111") then
                  if unsigned(boot_dbg_vic_pipe) /= shadow_ram(idx) then
                     vic_screen_mismatch <= vic_screen_mismatch + 1;
                     if vic_screen_mismatch < 40 then
                        log_boot_json(LOG_PATH, C_SESSION_ID, C_RUN_ID, "VICDBG",
                           "tb_c128_boot:vic_mismatch", "vic read mismatch",
                           "{""addr"":" & integer'image(to_integer(addr_l)) &
                           ",""expected"":" & integer'image(to_integer(shadow_ram(idx))) &
                           ",""got"":" & integer'image(to_integer(unsigned(boot_dbg_vic_pipe))) &
                           ",""aec"":""" & std_logic'image(boot_dbg_vic_aec) & """}");
                     end if;
                  end if;
               end if;
               pend := '0';
            end if;
            if boot_dbg_vic_fetch = '1' then
               addr_l := boot_dbg_core_ram_addr;
               pend   := '1';
               vic_fetch_count <= vic_fetch_count + 1;
            end if;
         end if;
      end if;
   end process;

   monitor_proc : process (clk_main)
      variable last_we : std_logic := '0';
   begin
      if rising_edge(clk_main) then
         free_div_ce <= free_div_ce + 1;
         if boot_dbg_vic_pixel_ce = '1' then
            pixel_ce_count <= pixel_ce_count + 1;
            if video_ce_sig /= boot_dbg_vic_pixel_ce then
               ce_mismatch_count <= ce_mismatch_count + 1;
            end if;
            if free_div_ce /= "00" then
               free_div_mismatch <= free_div_mismatch + 1;
            end if;
         end if;

         if boot_dbg_ram_hold_tick = '1' then
            ram_hold_ticks <= ram_hold_ticks + 1;
         end if;

         if unsigned(boot_stage) > max_boot_stage then
            max_boot_stage <= unsigned(boot_stage);
         end if;
         if ram_addr > max_ram_addr then
            max_ram_addr <= ram_addr;
         end if;
         if unsigned(sys_rom_addr) > max_sysrom_addr then
            max_sysrom_addr <= unsigned(sys_rom_addr);
         end if;
         if dbg_sysrom_cs = '1' then
            sysrom_cs_hits <= sysrom_cs_hits + 1;
         end if;
         if dbg_ram_ce = '1' then
            ram_ce_hits <= ram_ce_hits + 1;
         end if;

         if ram_we = '1' and last_we = '0' then
            ram_we_count <= ram_we_count + 1;
            log_boot_json(
               LOG_PATH, C_SESSION_ID, C_RUN_ID, "H1",
               "tb_c128_boot:monitor",
               "ramWE rising edge",
               "{""addr"":" & integer'image(to_integer(ram_addr)) &
               ",""z80_n"":""" &
               std_logic'image(boot_z80_n) & """}"
            );
         end if;
         last_we := ram_we;
      end if;
   end process;

   summary_proc : process
      variable pass : boolean;
   begin
      wait until sim_done;
      pass := ram_we_count > 0 and boot_z80_n = '1' and unsigned(boot_stage) = 7 and
              z80_ram_rd_mismatch = 0 and
              vic_consume_count > 0 and vic_screen_mismatch = 0;
      if C_MEM_BRIDGE and pass then
         pass := false; -- rejected bridge must not pass boot gate
      end if;
      log_boot_json(
         LOG_PATH, C_SESSION_ID, C_RUN_ID, "H1",
         "tb_c128_boot:summary",
         "simulation finished",
         "{""ram_we_count"":" & integer'image(ram_we_count) &
         ",""max_boot_stage"":" & integer'image(to_integer(max_boot_stage)) &
         ",""final_boot_stage"":" & integer'image(to_integer(unsigned(boot_stage))) &
         ",""final_z80_n"":""" & std_logic'image(boot_z80_n) &
         """,""z80_rd"":""" & std_logic'image(boot_dbg_z80_rd) &
         """,""z80_we"":""" & std_logic'image(boot_dbg_z80_we) &
         """,""z80_sysrom"":""" & std_logic'image(boot_dbg_z80_sys) &
         """,""vec_valid"":""" & std_logic'image(boot_dbg_vec_v) &
         """,""vec_byte"":" & integer'image(to_integer(unsigned(boot_dbg_vec_b))) &
         ",""max_ram_addr"":" & integer'image(to_integer(max_ram_addr)) &
         ",""max_sysrom_addr"":" & integer'image(to_integer(max_sysrom_addr)) &
         ",""sysrom_cs_hits"":" & integer'image(sysrom_cs_hits) &
         ",""ram_ce_hits"":" & integer'image(ram_ce_hits) &
         ",""pixel_ce_count"":" & integer'image(pixel_ce_count) &
         ",""ce_mismatch_count"":" & integer'image(ce_mismatch_count) &
         ",""free_div_mismatch"":" & integer'image(free_div_mismatch) &
         ",""ram_hold_ticks"":" & integer'image(ram_hold_ticks) &
         ",""vic_fetch_count"":" & integer'image(vic_fetch_count) &
         ",""vic_consume_count"":" & integer'image(vic_consume_count) &
         ",""vic_timing_mismatch"":" & integer'image(vic_timing_mismatch) &
         ",""vic_pipe_mismatch"":" & integer'image(vic_pipe_mismatch) &
         ",""vic_screen_mismatch"":" & integer'image(vic_screen_mismatch) &
         ",""z80_ram_rd_checks"":" & integer'image(z80_ram_rd_checks) &
         ",""z80_ram_rd_mismatch"":" & integer'image(z80_ram_rd_mismatch) &
         ",""z80_mmu_stuck_samples"":" & integer'image(z80_mmu_stuck_samples) &
         ",""mem_bridge"":" & boolean'image(C_MEM_BRIDGE) &
         """,""pass"":" & boolean'image(pass) & "}"
      );
      if pass then
         report "PASS: ramWE observed " & integer'image(ram_we_count) & " time(s)"
            severity note;
      else
         report "FAIL: no ramWE within " & time'image(C_SIM_TIME) &
                " (ram_we_count=" & integer'image(ram_we_count) &
                ", max_boot_stage=" & integer'image(to_integer(max_boot_stage)) &
                ", final_stage=" & integer'image(to_integer(unsigned(boot_stage))) & ")"
            severity error;
      end if;
      wait;
   end process;

end architecture sim;
