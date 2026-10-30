----------------------------------------------------------------------------------
-- The internal function ROM (U36), as in tb_c128_boot: main and the system ROM, boot0.rom
-- with a function ROM from 0x18000 (the firmware's place for it). The 8502's reset vector
-- points at a loop in BASIC LO that maps the function ROM at $8000 and $C000 through the
-- MMU ($FF00 = $14) and copies $8007 and $C007 to RAM $0C00 and $0C01, over and over.
-- With 32 KB both are the ROM's, with 16 KB $C000 floats (MiSTer's), with none both do.
----------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.globals.all;
use work.sim_support_pkg.all;
use work.boot_paths_pkg.all;

entity tb_func_rom is
end entity tb_func_rom;

architecture sim of tb_func_rom is

   constant C_BOOT0_SIZE   : natural := 73728;
   constant C_SIM_TIME     : time    := 20 ms;
   constant C_SESSION_ID   : string  := "funcrom";
   constant C_RUN_ID       : string  := "sim-funcrom";
   constant C_FUNC_AT      : natural := 16#18000#;
   constant C_FL_BYTE      : std_logic_vector(7 downto 0) := x"A5";   -- at $8007
   constant C_FH_BYTE      : std_logic_vector(7 downto 0) := x"5A";   -- at $C007
   constant C_SAMPLES      : natural := 8;                            -- copies of each per case
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
   signal iec_hw_enable    : std_logic := '0';
   signal iec_reset_n      : std_logic;
   signal iec_atn_n        : std_logic;
   signal iec_clk_en       : std_logic;
   signal iec_clk_n        : std_logic;
   signal iec_data_en      : std_logic;
   signal iec_data_n       : std_logic;
   signal iec_srq_en       : std_logic;
   signal iec_srq_n        : std_logic;

   signal rom_mem          : byte_rom_t(0 to C_BOOT0_SIZE - 1) := (others => (others => '0'));
   signal rom_ready        : std_logic := '0';
   signal sysrom_ready     : std_logic := '0';

   signal sim_done         : boolean := false;
   signal core_released    : boolean := false;
   signal ram_we_count     : natural := 0;
   signal z80_handoff_seen : boolean := false;
   signal func_size        : std_logic_vector(1 downto 0) := "10";
   signal case_reset       : boolean := false;    -- a new case: the counts start again
   signal fl_count, fh_count : natural := 0;      -- copies of $8007 and $C007 seen
   signal fl_rom, fh_rom   : natural := 0;        -- of them the ROM's bytes
   signal cases_ok         : boolean := true;

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

   -- Built without emulated drives on purpose. This testbench guards the Z80 boot path,
   -- and two bit-level 1541/1571/1581 models would turn a several-minute run into an
   -- hours-long one without touching anything the gate actually checks. An unmounted
   -- drive is held in reset and releases every IEC line, which is what G_VDNUM = 0 does.
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
         reset_soft_i         => reset_soft,
         reset_hard_i         => reset_hard,
         pause_i              => pause,
         clk_main_speed_i     => CORE_CLK_SPEED,
         osm_control_i        => (others => '0'), -- Follow 40/80 (default decode)
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
         func_rom_size_i      => func_size,
         -- Expansion Port: nothing plugged in. All inputs at the released level, which is what
         -- the board's pull-ups give us on real hardware with an empty slot.
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
         cart_nmi_i           => '1',
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
         cart_ctrl_oe_o       => open,
         cart_ba_i            => '1',
         cart_rw_i            => '1',
         cart_io1_i           => '1',
         cart_io2_i           => '1',
         cart_ba_o            => open,
         cart_rw_o            => open,
         cart_io1_o           => open,
         cart_io2_o           => open,
         cart_addr_oe_o       => open,
         cart_a_i             => (others => '1'),
         cart_a_o             => open,
         cart_data_oe_o       => open,
         cart_d_i             => (others => '1'),
         cart_d_o             => open,
         iec_hardware_port_en_i => iec_hw_enable,
         iec_reset_n_o        => iec_reset_n,
         iec_atn_n_o          => iec_atn_n,
         iec_clk_en_o         => iec_clk_en,
         iec_clk_n_o          => iec_clk_n,
         iec_clk_n_i          => '0',
         iec_data_en_o        => iec_data_en,
         iec_data_n_o         => iec_data_n,
         iec_data_n_i         => '0',
         iec_srq_en_o         => iec_srq_en,
         iec_srq_n_o          => iec_srq_n,
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

   -- Match C64MEGA65's "IEC: Use hardware port" contract. Disabled outputs must
   -- be harmless even if an attached device holds every sensed bus line low.
   check_iec_port : process
   begin
      wait for 1 us;
      assert iec_reset_n = '1' and iec_atn_n = '1'
         report "FAIL: disabled hardware IEC drives RESET or ATN" severity failure;
      assert iec_clk_en = '0' and iec_data_en = '0' and iec_srq_en = '0'
         report "FAIL: disabled hardware IEC enables an output driver" severity failure;
      assert iec_clk_n = '1' and iec_data_n = '1' and iec_srq_n = '1'
         report "FAIL: disabled hardware IEC output levels are not inactive" severity failure;

      iec_hw_enable <= '1';
      wait for 1 ns;
      assert iec_reset_n = '0' and iec_clk_n = '0' and
             iec_data_n = '0' and iec_srq_n = '0'
         report "FAIL: enabled hardware IEC is not connected during reset" severity failure;
      iec_hw_enable <= '0';
      wait;
   end process check_iec_port;

   rom_load_proc : process
      variable loaded : byte_rom_t(0 to C_BOOT0_SIZE - 1);
      type code_t is array (natural range <>) of std_logic_vector(7 downto 0);
      -- At $4000 (BASIC LO, boot0.rom 0x8000):
      --    SEI; LDA #$14; STA $FF00 (I/O, BASIC LO, function ROM mid and high)
      --    loop: LDA $8007; STA $0C00; LDA $C007; STA $0C01; JMP loop
      constant C_CODE : code_t := (x"78", x"A9", x"14", x"8D", x"00", x"FF",
                                   x"AD", x"07", x"80", x"8D", x"00", x"0C",
                                   x"AD", x"07", x"C0", x"8D", x"01", x"0C",
                                   x"4C", x"06", x"40");
   begin
      loaded := load_bin_rom(BOOT0_PATH, C_BOOT0_SIZE);
      for i in C_CODE'range loop
         loaded(16#8000# + i) := C_CODE(i);
      end loop;
      loaded(16#7FFC#) := x"00";                     -- the 8502's reset vector: $4000
      loaded(16#7FFD#) := x"40";
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
         -- boot0.rom, then 32 KB of function ROM: its own bytes at $8007 and $C007, the
         -- rest a pattern
         if rom_ready = '1' and sysrom_ready = '0' then
            if idx < C_BOOT0_SIZE then
               qnice_sysrom_addr <= std_logic_vector(to_unsigned(idx, 17));
               sysrom_data_a     <= rom_mem(idx);
               sysrom_we_a       <= '1';
               idx := idx + 1;
            elsif idx < C_BOOT0_SIZE + 32768 then
               qnice_sysrom_addr <= std_logic_vector(to_unsigned(C_FUNC_AT + idx - C_BOOT0_SIZE, 17));
               sysrom_data_a     <= std_logic_vector(to_unsigned((idx * 7 + 3) mod 256, 8));
               if idx - C_BOOT0_SIZE = 7 then
                  sysrom_data_a <= C_FL_BYTE;
               elsif idx - C_BOOT0_SIZE = 16#4007# then
                  sysrom_data_a <= C_FH_BYTE;
               end if;
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
      -- 32 KB, then 16 KB, then none: each case until it has its copies
      for c in 0 to 2 loop
         if c = 0 then
            func_size <= "10";
         elsif c = 1 then
            func_size <= "01";
         else
            func_size <= "00";
         end if;
         case_reset <= true;
         wait until rising_edge(clk_main);
         wait until rising_edge(clk_main);
         case_reset <= false;
         wait until (fl_count >= C_SAMPLES and fh_count >= C_SAMPLES) for C_SIM_TIME;
         if c = 0 then
            assert fl_count >= C_SAMPLES and fl_rom = fl_count and fh_rom = fh_count
               report "FAIL: 32 KB function ROM: $8007/$C007 not the ROM's (" & integer'image(fl_rom) & "/" &
                      integer'image(fl_count) & ", " & integer'image(fh_rom) & "/" & integer'image(fh_count) & ")"
               severity error;
            cases_ok <= cases_ok and fl_count >= C_SAMPLES and fl_rom = fl_count and fh_rom = fh_count;
         elsif c = 1 then
            assert fl_count >= C_SAMPLES and fl_rom = fl_count and fh_rom < fh_count
               report "FAIL: 16 KB function ROM: $8007 not the ROM's or $C007 not floating" severity error;
            cases_ok <= cases_ok and fl_count >= C_SAMPLES and fl_rom = fl_count and fh_rom < fh_count;
         else
            assert fl_count >= C_SAMPLES and fl_rom < fl_count and fh_rom < fh_count
               report "FAIL: no function ROM: $8007/$C007 not floating" severity error;
            cases_ok <= cases_ok and fl_count >= C_SAMPLES and fl_rom < fl_count and fh_rom < fh_count;
         end if;
         report "funcrom case " & integer'image(c) & ": $8007 " & integer'image(fl_rom) & "/" & integer'image(fl_count) &
                " the ROM's, $C007 " & integer'image(fh_rom) & "/" & integer'image(fh_count) severity note;
      end loop;
      sim_done <= true;
      wait;
   end process;

   -- Count RAM write pulses and latch the Z80 -> 8502 handoff (boot_z80_n rising 0 -> 1).
   monitor_proc : process (clk_main)
      variable last_we    : std_logic := '0';
      variable last_z80_n : std_logic := '0';
   begin
      if rising_edge(clk_main) then
         if case_reset then
            fl_count <= 0;
            fh_count <= 0;
            fl_rom   <= 0;
            fh_rom   <= 0;
         elsif core_released and ram_we = '1' and last_we = '0' then
            if ram_addr = to_unsigned(16#0C00#, 18) then
               fl_count <= fl_count + 1;
               if std_logic_vector(ram_data_to) = C_FL_BYTE then
                  fl_rom <= fl_rom + 1;
               end if;
            elsif ram_addr = to_unsigned(16#0C01#, 18) then
               fh_count <= fh_count + 1;
               if std_logic_vector(ram_data_to) = C_FH_BYTE then
                  fh_rom <= fh_rom + 1;
               end if;
            end if;
         end if;
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
      pass := ram_we_count > 0 and z80_handoff_seen and boot_z80_n = '1' and cases_ok;
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
         report "PASS: the function ROM at $8000 and $C000, 32 KB, 16 KB and none" severity note;
      else
         report "FAIL: function ROM cases (see above)" severity error;
      end if;
      wait;
   end process;

end architecture sim;
