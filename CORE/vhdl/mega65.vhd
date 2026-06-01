----------------------------------------------------------------------------------
-- Commodore C128 for MEGA65
--
-- MEGA65 main file that contains the whole machine
--
-- based on C128_MiSTer by the MiSTer development team
-- powered by MiSTer2MEGA65 done by sy2002 and MJoergen in 2023
-- port done by Stefan Eilers in 2024  and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.globals.all;
use work.types_pkg.all;
use work.video_modes_pkg.all;

library xpm;
use xpm.vcomponents.all;

entity MEGA65_Core is
generic (
   G_BOARD : string                                         -- Which platform are we running on.
);
port (
   --------------------------------------------------------------------------------------------------------
   -- QNICE Clock Domain
   --------------------------------------------------------------------------------------------------------

   -- Get QNICE clock from the framework: for the vdrives as well as for RAMs and ROMs
   qnice_clk_i             : in  std_logic;
   qnice_rst_i             : in  std_logic;

   -- Video and audio mode control
   qnice_dvi_o             : out std_logic;              -- 0=HDMI (with sound), 1=DVI (no sound)
   qnice_video_mode_o      : out video_mode_type;        -- Defined in video_modes_pkg.vhd
   qnice_osm_cfg_scaling_o : out std_logic_vector(8 downto 0);
   qnice_scandoubler_o     : out std_logic;              -- 0 = no scandoubler, 1 = scandoubler
   qnice_audio_mute_o      : out std_logic;
   qnice_audio_filter_o    : out std_logic;
   qnice_zoom_crop_o       : out std_logic;
   qnice_ascal_mode_o      : out std_logic_vector(1 downto 0);
   qnice_ascal_polyphase_o : out std_logic;
   qnice_ascal_triplebuf_o : out std_logic;
   qnice_retro15kHz_o      : out std_logic;              -- 0 = normal frequency, 1 = retro 15 kHz frequency
   qnice_csync_o           : out std_logic;              -- 0 = normal HS/VS, 1 = Composite Sync  

   -- Flip joystick ports
   qnice_flip_joyports_o   : out std_logic;

   -- On-Screen-Menu selections
   qnice_osm_control_i     : in  std_logic_vector(255 downto 0);

   -- QNICE general purpose register
   qnice_gp_reg_i          : in  std_logic_vector(255 downto 0);

   -- Core-specific devices
   qnice_dev_id_i          : in  std_logic_vector(15 downto 0);
   qnice_dev_addr_i        : in  std_logic_vector(27 downto 0);
   qnice_dev_data_i        : in  std_logic_vector(15 downto 0);
   qnice_dev_data_o        : out std_logic_vector(15 downto 0);
   qnice_dev_ce_i          : in  std_logic;
   qnice_dev_we_i          : in  std_logic;
   qnice_dev_wait_o        : out std_logic;

   --------------------------------------------------------------------------------------------------------
   -- HyperRAM Clock Domain
   --------------------------------------------------------------------------------------------------------

   hr_clk_i                : in  std_logic;
   hr_rst_i                : in  std_logic;
   hr_core_write_o         : out std_logic;
   hr_core_read_o          : out std_logic;
   hr_core_address_o       : out std_logic_vector(31 downto 0);
   hr_core_writedata_o     : out std_logic_vector(15 downto 0);
   hr_core_byteenable_o    : out std_logic_vector( 1 downto 0);
   hr_core_burstcount_o    : out std_logic_vector( 7 downto 0);
   hr_core_readdata_i      : in  std_logic_vector(15 downto 0);
   hr_core_readdatavalid_i : in  std_logic;
   hr_core_waitrequest_i   : in  std_logic;
   hr_high_i               : in  std_logic;  -- Core is too fast
   hr_low_i                : in  std_logic;  -- Core is too slow

   --------------------------------------------------------------------------------------------------------
   -- Video Clock Domain
   --------------------------------------------------------------------------------------------------------

   video_clk_o             : out std_logic;
   video_rst_o             : out std_logic;
   video_ce_o              : out std_logic;
   video_ce_ovl_o          : out std_logic;
   video_red_o             : out std_logic_vector(7 downto 0);
   video_green_o           : out std_logic_vector(7 downto 0);
   video_blue_o            : out std_logic_vector(7 downto 0);
   video_vs_o              : out std_logic;
   video_hs_o              : out std_logic;
   video_hblank_o          : out std_logic;
   video_vblank_o          : out std_logic;

   --------------------------------------------------------------------------------------------------------
   -- Core Clock Domain
   --------------------------------------------------------------------------------------------------------

   clk_i                   : in  std_logic;              -- 100 MHz clock

   -- Share clock and reset with the framework
   main_clk_o              : out std_logic;              -- CORE's clock
   main_rst_o              : out std_logic;              -- CORE's reset, synchronized

   -- M2M's reset manager provides 2 signals:
   --    m2m:   Reset the whole machine: Core and Framework
   --    core:  Only reset the core
   main_reset_m2m_i        : in  std_logic;
   main_reset_core_i       : in  std_logic;

   main_pause_core_i       : in  std_logic;

   -- On-Screen-Menu selections
   main_osm_control_i      : in  std_logic_vector(255 downto 0);

   -- QNICE general purpose register converted to main clock domain
   main_qnice_gp_reg_i     : in  std_logic_vector(255 downto 0);

   -- Audio output (Signed PCM)
   main_audio_left_o       : out signed(15 downto 0);
   main_audio_right_o      : out signed(15 downto 0);

   -- M2M Keyboard interface (incl. power led and drive led)
   main_kb_key_num_i       : in  integer range 0 to 79;  -- cycles through all MEGA65 keys
   main_kb_key_pressed_n_i : in  std_logic;              -- low active: debounced feedback: is kb_key_num_i pressed right now?
   main_power_led_o        : out std_logic;
   main_power_led_col_o    : out std_logic_vector(23 downto 0);
   main_drive_led_o        : out std_logic;
   main_drive_led_col_o    : out std_logic_vector(23 downto 0);

   -- Joysticks and paddles input
   main_joy_1_up_n_i       : in  std_logic;
   main_joy_1_down_n_i     : in  std_logic;
   main_joy_1_left_n_i     : in  std_logic;
   main_joy_1_right_n_i    : in  std_logic;
   main_joy_1_fire_n_i     : in  std_logic;
   main_joy_1_up_n_o       : out std_logic;
   main_joy_1_down_n_o     : out std_logic;
   main_joy_1_left_n_o     : out std_logic;
   main_joy_1_right_n_o    : out std_logic;
   main_joy_1_fire_n_o     : out std_logic;
   main_joy_2_up_n_i       : in  std_logic;
   main_joy_2_down_n_i     : in  std_logic;
   main_joy_2_left_n_i     : in  std_logic;
   main_joy_2_right_n_i    : in  std_logic;
   main_joy_2_fire_n_i     : in  std_logic;
   main_joy_2_up_n_o       : out std_logic;
   main_joy_2_down_n_o     : out std_logic;
   main_joy_2_left_n_o     : out std_logic;
   main_joy_2_right_n_o    : out std_logic;
   main_joy_2_fire_n_o     : out std_logic;

   main_pot1_x_i           : in  std_logic_vector(7 downto 0);
   main_pot1_y_i           : in  std_logic_vector(7 downto 0);
   main_pot2_x_i           : in  std_logic_vector(7 downto 0);
   main_pot2_y_i           : in  std_logic_vector(7 downto 0);
   main_rtc_i              : in  std_logic_vector(64 downto 0);

   -- CBM-488/IEC serial port
   iec_reset_n_o           : out std_logic;
   iec_atn_n_o             : out std_logic;
   iec_clk_en_o            : out std_logic;
   iec_clk_n_i             : in  std_logic;
   iec_clk_n_o             : out std_logic;
   iec_data_en_o           : out std_logic;
   iec_data_n_i            : in  std_logic;
   iec_data_n_o            : out std_logic;
   iec_srq_en_o            : out std_logic;
   iec_srq_n_i             : in  std_logic;
   iec_srq_n_o             : out std_logic;

   -- C64 Expansion Port (aka Cartridge Port)
   cart_en_o               : out std_logic;  -- Enable port, active high
   cart_phi2_o             : out std_logic;
   cart_dotclock_o         : out std_logic;
   cart_dma_i              : in  std_logic;
   cart_reset_oe_o         : out std_logic;
   cart_reset_i            : in  std_logic;
   cart_reset_o            : out std_logic;
   cart_game_oe_o          : out std_logic;
   cart_game_i             : in  std_logic;
   cart_game_o             : out std_logic;
   cart_exrom_oe_o         : out std_logic;
   cart_exrom_i            : in  std_logic;
   cart_exrom_o            : out std_logic;
   cart_nmi_oe_o           : out std_logic;
   cart_nmi_i              : in  std_logic;
   cart_nmi_o              : out std_logic;
   cart_irq_oe_o           : out std_logic;
   cart_irq_i              : in  std_logic;
   cart_irq_o              : out std_logic;
   cart_roml_oe_o          : out std_logic;
   cart_roml_i             : in  std_logic;
   cart_roml_o             : out std_logic;
   cart_romh_oe_o          : out std_logic;
   cart_romh_i             : in  std_logic;
   cart_romh_o             : out std_logic;
   cart_ctrl_oe_o          : out std_logic; -- 0 : tristate (i.e. input), 1 : output
   cart_ba_i               : in  std_logic;
   cart_rw_i               : in  std_logic;
   cart_io1_i              : in  std_logic;
   cart_io2_i              : in  std_logic;
   cart_ba_o               : out std_logic;
   cart_rw_o               : out std_logic;
   cart_io1_o              : out std_logic;
   cart_io2_o              : out std_logic;
   cart_addr_oe_o          : out std_logic; -- 0 : tristate (i.e. input), 1 : output
   cart_a_i                : in  unsigned(15 downto 0);
   cart_a_o                : out unsigned(15 downto 0);
   cart_data_oe_o          : out std_logic; -- 0 : tristate (i.e. input), 1 : output
   cart_d_i                : in  unsigned( 7 downto 0);
   cart_d_o                : out unsigned( 7 downto 0)
);
end entity MEGA65_Core;

architecture synthesis of MEGA65_Core is

---------------------------------------------------------------------------------------------
-- Clocks and active high reset signals for each clock domain
---------------------------------------------------------------------------------------------
signal vdc_clk_o : std_logic;

signal hr_core_speed : unsigned(1 downto 0); -- see clock.vhd for details
signal dbg_heartbeat : unsigned(25 downto 0) := (others => '0');
signal dbg_hs_prev   : std_logic := '0';
signal dbg_vs_prev   : std_logic := '0';
signal dbg_hblank_prev : std_logic := '0';
signal dbg_vblank_prev : std_logic := '0';
signal dbg_hs_seen   : std_logic := '0';
signal dbg_vs_seen   : std_logic := '0';
signal dbg_rgb_seen  : std_logic := '0';
signal dbg_ce_high_seen : std_logic := '0';
signal dbg_ce_low_seen  : std_logic := '0';
signal dbg_ce_ovl_high_seen : std_logic := '0';
signal dbg_ce_ovl_low_seen  : std_logic := '0';
signal dbg_hblank_toggled : std_logic := '0';
signal dbg_vblank_toggled : std_logic := '0';

signal core_video_ce      : std_logic;
signal core_video_ce_ovl  : std_logic;
signal core_video_red     : std_logic_vector(7 downto 0);
signal core_video_green   : std_logic_vector(7 downto 0);
signal core_video_blue    : std_logic_vector(7 downto 0);
signal core_video_vs      : std_logic;
signal core_video_hs      : std_logic;
signal core_video_hblank  : std_logic;
signal core_video_vblank  : std_logic;
signal core_cart_reset    : std_logic;

---------------------------------------------------------------------------------------------
-- qnice_clk
---------------------------------------------------------------------------------------------
-- QNICE clock domain

signal qnice_c64_ramx_we   : std_logic;
signal qnice_c64_ramx_addr   : std_logic_vector(17 downto 0);
signal qnice_c64_ramx_d_to   : std_logic_vector(7 downto 0);
signal qnice_c64_ramx_d_from : std_logic_vector(7 downto 0);
signal qnice_sysrom_we       : std_logic;
signal qnice_sysrom_addr     : std_logic_vector(16 downto 0);
signal qnice_sysrom_d_to     : std_logic_vector(7 downto 0);
signal qnice_sysrom_d_from   : std_logic_vector(7 downto 0);
signal qnice_drvrom_we       : std_logic;
signal qnice_drvrom_addr     : std_logic_vector(18 downto 0);
signal qnice_drvrom_d_to     : std_logic_vector(7 downto 0);
signal qnice_drvrom_d_from   : std_logic_vector(7 downto 0);
signal main_ram_addr         : unsigned(17 downto 0);
signal main_ram_data_to      : unsigned(7 downto 0);
signal main_ram_we           : std_logic;
signal main_ram_data_from    : unsigned(7 downto 0);
signal main_ram_q            : std_logic_vector(7 downto 0);
signal main_sysrom_addr      : std_logic_vector(16 downto 0);
signal main_sysrom_data      : std_logic_vector(7 downto 0);


begin

   hr_core_write_o      <= '0';
   hr_core_read_o       <= '0';
   hr_core_address_o    <= (others => '0');
   hr_core_writedata_o  <= (others => '0');
   hr_core_byteenable_o <= (others => '0');
   hr_core_burstcount_o <= (others => '0');

   -- Tristate all expansion port drivers that we can directly control
   -- @TODO: As soon as we support modules that can act as busmaster, we need to become more flexible here
   cart_ctrl_oe_o       <= '0';
   cart_addr_oe_o       <= '0';
   cart_data_oe_o       <= '0';
   cart_en_o            <= '1'; -- Enable expansion port

   cart_reset_oe_o      <= '1';
   cart_game_oe_o       <= '0';
   cart_exrom_oe_o      <= '0';
   cart_nmi_oe_o        <= '0';
   cart_irq_oe_o        <= '0';
   cart_roml_oe_o       <= '1';
   cart_romh_oe_o       <= '1';
   iec_reset_n_o        <= '1';
   iec_clk_en_o         <= '1';
   iec_data_en_o        <= '1';
   iec_srq_en_o         <= '1';

   -- Default values for all signals
   cart_phi2_o          <= '0';
   cart_dotclock_o      <= '0';
   cart_ba_o            <= '0';
   cart_rw_o            <= '0';
   cart_a_o             <= (others => '0');
   cart_d_o             <= (others => '0');

   main_joy_1_up_n_o    <= '1';
   main_joy_1_down_n_o  <= '1';
   main_joy_1_left_n_o  <= '1';
   main_joy_1_right_n_o <= '1';
   main_joy_1_fire_n_o  <= '1';
   main_joy_2_up_n_o    <= '1';
   main_joy_2_down_n_o  <= '1';
   main_joy_2_left_n_o  <= '1';
   main_joy_2_right_n_o <= '1';
   main_joy_2_fire_n_o  <= '1';

  ---------------------------------------------------------------------------------------------
  -- main_clk (MiSTer core's clock)
  ---------------------------------------------------------------------------------------------
   hr_core_speed        <= "00"; -- TODO: This is fixed to PAL for now, check whether frequency changes are required!?
   -- MMCME2_ADV clock generators
   --   PAL: 31.528 MHz (main) and 63.056 MHz (video)
   --        HDMI: Flicker-free: 0.25% slower
   clk_gen : entity work.clk
      port map (
         sys_clk_i         => clk_i,           -- expects 100 MHz
         core_speed_i      => hr_core_speed,   -- 0=PAL/original C64, 1=PAL/HDMI flicker-free, 2=NTSC
         main_clk_o        => main_clk_o,        -- CORE's clock
         main_rst_o        => main_rst_o         -- CORE's reset, synchronized
      ); -- clk_gen


   -- VDC clock generator
   -- TODO: Maybe we can integrate this into the clk.vhd?
   clk_vdc_gen: entity work.vdc_clk
      port map (
         refclk   => clk_i,
         outclk_0 => vdc_clk_o,
         locked   => open        -- TODO: Do we need to detect the locked state?
      ); -- clk_vdc_gen

   video_clk_o <= main_clk_o;
   video_rst_o <= main_rst_o;

   -- Bring-up debug indicator: if this increments/blinks then main clock is alive.
   process(main_clk_o)
   begin
      if rising_edge(main_clk_o) then
         if main_rst_o = '1' then
            dbg_heartbeat <= (others => '0');
         else
            dbg_heartbeat <= dbg_heartbeat + 1;
         end if;
      end if;
   end process;

   -- #region agent log: reduced video-state probe
   process(main_clk_o)
   begin
      if rising_edge(main_clk_o) then
         if main_rst_o = '1' or main_reset_m2m_i = '1' or core_cart_reset = '1' then
            dbg_hs_prev  <= core_video_hs;
            dbg_vs_prev  <= core_video_vs;
            dbg_hblank_prev <= core_video_hblank;
            dbg_vblank_prev <= core_video_vblank;
            dbg_hs_seen  <= '0';
            dbg_vs_seen  <= '0';
            dbg_rgb_seen <= '0';
            dbg_ce_high_seen <= '0';
            dbg_ce_low_seen <= '0';
            dbg_ce_ovl_high_seen <= '0';
            dbg_ce_ovl_low_seen <= '0';
            dbg_hblank_toggled <= '0';
            dbg_vblank_toggled <= '0';
         else
            if dbg_hs_prev /= core_video_hs then
               dbg_hs_seen <= '1';
            end if;
            if dbg_vs_prev /= core_video_vs then
               dbg_vs_seen <= '1';
            end if;
            if dbg_hblank_prev /= core_video_hblank then
               dbg_hblank_toggled <= '1';
            end if;
            if dbg_vblank_prev /= core_video_vblank then
               dbg_vblank_toggled <= '1';
            end if;
            if core_video_ce = '1' then
               dbg_ce_high_seen <= '1';
            else
               dbg_ce_low_seen <= '1';
            end if;
            if core_video_ce_ovl = '1' then
               dbg_ce_ovl_high_seen <= '1';
            else
               dbg_ce_ovl_low_seen <= '1';
            end if;
            if core_video_red /= x"00" or core_video_green /= x"00" or core_video_blue /= x"00" then
               dbg_rgb_seen <= '1';
            end if;
            dbg_hs_prev <= core_video_hs;
            dbg_vs_prev <= core_video_vs;
            dbg_hblank_prev <= core_video_hblank;
            dbg_vblank_prev <= core_video_vblank;
         end if;
      end if;
   end process;
   -- #endregion

   -- MEGA65 power LED now shows simplified video-state diagnostics after reset:
   --   red     = no HS/VS activity
   --   yellow  = timing control invalid (CE/blanking not toggling as expected)
   --   magenta = CE_OVL was never observed high
   --   cyan    = CE_OVL dropped low at least once
   --   blue    = timing valid but RGB stayed zero
   --   green   = timing valid and RGB activity detected
   main_power_led_o     <= '1';
   main_power_led_col_o <= x"FF0000" when (dbg_hs_seen = '0' or dbg_vs_seen = '0') else
                           x"FFFF00" when (dbg_ce_high_seen = '0' or dbg_ce_low_seen = '0' or dbg_hblank_toggled = '0' or dbg_vblank_toggled = '0') else
                           x"FF00FF" when dbg_ce_ovl_high_seen = '0' else
                           x"00FFFF" when dbg_ce_ovl_low_seen = '1' else
                           x"0000FF" when dbg_rgb_seen = '0' else
                           x"00FF00";

   -- #region agent log: LED-encoded runtime states
   main_drive_led_o <= '1';
   -- Reduced palette to make runtime state easy to identify:
   --   red    = main clock/reset generator reset asserted
   --   yellow = framework hard reset request active
   --   blue   = core reset active
   --   green  = core reset released
   main_drive_led_col_o <= x"FF0000" when main_rst_o = '1' else
                           x"FFFF00" when main_reset_m2m_i = '1' else
                           x"0000FF" when core_cart_reset = '1' else
                           x"00FF00";
   -- #endregion

   -- main.vhd contains the actual MiSTer core
   i_main : entity work.main
      generic map (
         G_VDNUM              => C_VDNUM
      )
      port map (
         clk_main_i           => main_clk_o,
         clk_vdc_i            => vdc_clk_o,
         reset_soft_i         => main_reset_core_i,
         reset_hard_i         => main_reset_m2m_i,
         pause_i              => main_pause_core_i,

         clk_main_speed_i     => CORE_CLK_SPEED,

         -- Video output
         -- This is PAL 720x576 @ 50 Hz (pixel clock 27 MHz), but synchronized to main_clk (54 MHz).
         video_ce_o           => core_video_ce,
         video_ce_ovl_o       => core_video_ce_ovl,
         video_red_o          => core_video_red,
         video_green_o        => core_video_green,
         video_blue_o         => core_video_blue,
         video_vs_o           => core_video_vs,
         video_hs_o           => core_video_hs,
         video_hblank_o       => core_video_hblank,
         video_vblank_o       => core_video_vblank,

         -- audio output (pcm format, signed values)
         audio_left_o         => main_audio_left_o,
         audio_right_o        => main_audio_right_o,

         -- Drive led
         drive_led_o => open, 
         drive_led_col_o => open,

         -- M2M Keyboard interface
         kb_key_num_i         => main_kb_key_num_i,
         kb_key_pressed_n_i   => main_kb_key_pressed_n_i,

         -- MEGA65 joysticks and paddles/mouse/potentiometers
         joy_1_up_n_i         => main_joy_1_up_n_i ,
         joy_1_down_n_i       => main_joy_1_down_n_i,
         joy_1_left_n_i       => main_joy_1_left_n_i,
         joy_1_right_n_i      => main_joy_1_right_n_i,
         joy_1_fire_n_i       => main_joy_1_fire_n_i,

         joy_2_up_n_i         => main_joy_2_up_n_i,
         joy_2_down_n_i       => main_joy_2_down_n_i,
         joy_2_left_n_i       => main_joy_2_left_n_i,
         joy_2_right_n_i      => main_joy_2_right_n_i,
         joy_2_fire_n_i       => main_joy_2_fire_n_i,

         pot1_x_i             => main_pot1_x_i,
         pot1_y_i             => main_pot1_y_i,
         pot2_x_i             => main_pot2_x_i,
         pot2_y_i             => main_pot2_y_i,

         -- Add RAM interface
         ram_addr_o           => main_ram_addr,
         ram_data_o           => main_ram_data_to,
         ram_we_o             => main_ram_we,
         ram_data_i           => main_ram_data_from,
         sys_rom_addr_o       => main_sysrom_addr,
         sys_rom_data_i       => main_sysrom_data,

         -- C64 Expansion Port (aka Cartridge Port)
         cart_reset_i         => cart_reset_i, 
         cart_reset_o         => core_cart_reset,
         cart_dma_i           => cart_dma_i,
         cart_game_i          => cart_game_i,
         cart_exrom_i         => cart_exrom_i,
         cart_nmi_i           => cart_nmi_i,
         cart_irq_i           => cart_irq_i,
         cart_game_o          => cart_game_o,
         cart_exrom_o         => cart_exrom_o,
         cart_nmi_o           => cart_nmi_o,
         cart_irq_o           => cart_irq_o,
         cart_roml_o          => cart_roml_o,
         cart_romh_o          => cart_romh_o,
         cart_io1_o           => cart_io1_o,
         cart_io2_o           => cart_io2_o,
         iec_atn_n_o          => iec_atn_n_o,
         iec_clk_n_o          => iec_clk_n_o,
         iec_clk_n_i          => iec_clk_n_i,
         iec_data_n_o         => iec_data_n_o,
         iec_data_n_i         => iec_data_n_i,
         iec_srq_n_o          => iec_srq_n_o,
         iec_srq_n_i          => iec_srq_n_i
      ); -- i_main

   -- #region agent log: keep core timing/blanking semantics
   video_ce_o      <= core_video_ce;
   video_ce_ovl_o  <= core_video_ce_ovl;
   video_hblank_o  <= core_video_hblank;
   video_vblank_o  <= core_video_vblank;
   video_hs_o      <= core_video_hs;
   video_vs_o      <= core_video_vs;
   video_red_o     <= core_video_red;
   video_green_o   <= core_video_green;
   video_blue_o    <= core_video_blue;
   -- #endregion

   cart_reset_o    <= core_cart_reset;

   ---------------------------------------------------------------------------------------------
   -- Audio and video settings (QNICE clock domain)
   ---------------------------------------------------------------------------------------------

   -- Due to a discussion on the MEGA65 discord (https://discord.com/channels/719326990221574164/794775503818588200/1039457688020586507)
   -- we decided to choose a naming convention for the PAL modes that might be more intuitive for the end users than it is
   -- for the programmers: "4:3" means "meant to be run on a 4:3 monitor", "5:4 on a 5:4 monitor".
   -- The technical reality is though, that in our "5:4" mode we are actually doing a 4/3 aspect ratio adjustment
   -- while in the 4:3 mode we are outputting a 5:4 image. This is kind of odd, but it seemed that our 4/3 aspect ratio
   -- adjusted image looks best on a 5:4 monitor and the other way round.
   -- Not sure if this will stay forever or if we will come up with a better naming convention.
   -- Debug: force the most compatible C64-like default timing.
   qnice_video_mode_o <= C_VIDEO_HDMI_16_9_50;

   -- Use On-Screen-Menu selections to configure several audio and video settings
   -- Video and audio mode control
   qnice_dvi_o                <= '0';                                         -- 0=HDMI (with sound), 1=DVI (no sound)
   qnice_scandoubler_o        <= '0';                                         -- no scandoubler
   qnice_audio_mute_o         <= '0';                                         -- audio is not muted
   qnice_audio_filter_o       <= '0'; -- qnice_osm_control_i(C_MENU_IMPROVE_AUDIO);   -- 0 = raw audio, 1 = use filters from globals.vhd
   qnice_zoom_crop_o          <= '0'; -- qnice_osm_control_i(C_MENU_HDMI_ZOOM);       -- 0 = no zoom/crop
   
   -- These two signals are often used as a pair (i.e. both '1'), particularly when
   -- you want to run old analog cathode ray tube monitors or TVs (via SCART)
   -- If you want to provide your users a choice, then a good choice is:
   --    "Standard VGA":                     qnice_retro15kHz_o=0 and qnice_csync_o=0
   --    "Retro 15 kHz with HSync and VSync" qnice_retro15kHz_o=1 and qnice_csync_o=0
   --    "Retro 15 kHz with CSync"           qnice_retro15kHz_o=1 and qnice_csync_o=1
   qnice_retro15kHz_o         <= '0';
   qnice_csync_o              <= '0';
   qnice_osm_cfg_scaling_o    <= (others => '1');

   -- ascal filters that are applied while processing the input
   -- 00 : Nearest Neighbour
   -- 01 : Bilinear
   -- 10 : Sharp Bilinear
   -- 11 : Bicubic
   qnice_ascal_mode_o         <= "00";

   -- If polyphase is '1' then the ascal filter mode is ignored and polyphase filters are used instead
   -- @TODO: Right now, the filters are hardcoded in the M2M framework, we need to make them changeable inside m2m-rom.asm
   qnice_ascal_polyphase_o    <= '0'; -- qnice_osm_control_i(C_MENU_CRT_EMULATION);

   -- ascal triple-buffering
   -- @TODO: Right now, the M2M framework only supports OFF, so do not touch until the framework is upgraded
   qnice_ascal_triplebuf_o    <= '0';

   -- Flip joystick ports (i.e. the joystick in port 2 is used as joystick 1 and vice versa)
   qnice_flip_joyports_o      <= '0';

   ---------------------------------------------------------------------------------------------
   -- Core specific device handling (QNICE clock domain)
   ---------------------------------------------------------------------------------------------

   core_specific_devices : process(all)
   begin
      -- make sure that this is x"EEEE" by default and avoid a register here by having this default value
      qnice_dev_data_o     <= x"EEEE";
      qnice_dev_wait_o     <= '0';
      qnice_c64_ramx_addr  <= (others => '0');
      qnice_c64_ramx_d_to  <= (others => '0');
      qnice_c64_ramx_we    <= '0';
      qnice_sysrom_addr    <= (others => '0');
      qnice_sysrom_d_to    <= (others => '0');
      qnice_sysrom_we      <= '0';
      qnice_drvrom_addr    <= (others => '0');
      qnice_drvrom_d_to    <= (others => '0');
      qnice_drvrom_we      <= '0';

      case qnice_dev_id_i is
         -- Device numbers need to be >= 0x0100
        when C_DEV_RAM => 
            qnice_c64_ramx_addr <= qnice_dev_addr_i(17 downto 0);
            qnice_c64_ramx_we   <= qnice_dev_we_i;
            qnice_c64_ramx_d_to <= qnice_dev_data_i(7 downto 0);
            qnice_dev_data_o    <= x"00" & qnice_c64_ramx_d_from;
        when C_DEV_SYSTEM_ROM =>
            qnice_sysrom_addr <= qnice_dev_addr_i(16 downto 0);
            qnice_sysrom_we   <= qnice_dev_we_i;
            qnice_sysrom_d_to <= qnice_dev_data_i(7 downto 0);
            qnice_dev_data_o  <= x"00" & qnice_sysrom_d_from;
        when C_DEV_DRIVE_ROM =>
            qnice_drvrom_addr <= qnice_dev_addr_i(18 downto 0);
            qnice_drvrom_we   <= qnice_dev_we_i;
            qnice_drvrom_d_to <= qnice_dev_data_i(7 downto 0);
            qnice_dev_data_o  <= x"00" & qnice_drvrom_d_from;

         when others => null;
      end case;
   end process core_specific_devices;

   ---------------------------------------------------------------------------------------------
   -- Dual Clocks
   ---------------------------------------------------------------------------------------------

   -- Put your dual-clock devices such as RAMs and ROMs here
   --
   -- Use the M2M framework's official RAM/ROM: dualport_2clk_ram
   -- and make sure that the you configure the port that works with QNICE as a falling edge
   -- by setting G_FALLING_A or G_FALLING_B (depending on which port you use) to true.
   i_main_ram : entity work.dualport_2clk_ram
      generic map (
         ADDR_WIDTH => 18,
         DATA_WIDTH => 8,
         FALLING_A  => true
      )
      port map (
         clock_a         => qnice_clk_i,
         address_a       => qnice_c64_ramx_addr(17 downto 0),
         do_latch_addr_a => '0',
         data_a          => qnice_c64_ramx_d_to,
         wren_a          => qnice_c64_ramx_we,
         q_a             => qnice_c64_ramx_d_from,
         clock_b         => main_clk_o,
         address_b       => std_logic_vector(main_ram_addr),
         do_latch_addr_b => '0',
         data_b          => std_logic_vector(main_ram_data_to),
         wren_b          => main_ram_we,
         q_b             => main_ram_q
      );

   main_ram_data_from <= unsigned(main_ram_q);

   i_system_rom : entity work.dualport_2clk_ram
      generic map (
         ADDR_WIDTH => 17,
         DATA_WIDTH => 8,
         FALLING_A  => true
      )
      port map (
         clock_a         => qnice_clk_i,
         address_a       => qnice_sysrom_addr,
         do_latch_addr_a => '0',
         data_a          => qnice_sysrom_d_to,
         wren_a          => qnice_sysrom_we,
         q_a             => qnice_sysrom_d_from,
         clock_b         => main_clk_o,
         address_b       => main_sysrom_addr,
         do_latch_addr_b => '0',
         data_b          => (others => '0'),
         wren_b          => '0',
         q_b             => main_sysrom_data
      );

   i_drive_rom : entity work.dualport_2clk_ram
      generic map (
         ADDR_WIDTH => 19,
         DATA_WIDTH => 8,
         FALLING_A  => true
      )
      port map (
         clock_a         => qnice_clk_i,
         address_a       => qnice_drvrom_addr,
         do_latch_addr_a => '0',
         data_a          => qnice_drvrom_d_to,
         wren_a          => qnice_drvrom_we,
         q_a             => qnice_drvrom_d_from,
         clock_b         => main_clk_o,
         address_b       => (others => '0'),
         do_latch_addr_b => '0',
         data_b          => (others => '0'),
         wren_b          => '0',
         q_b             => open
      );

end architecture synthesis;
