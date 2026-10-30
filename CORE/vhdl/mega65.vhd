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
use work.sdhost_pkg.all;

library unisim;
use unisim.vcomponents.all;

library xpm;
use xpm.vcomponents.all;

entity MEGA65_Core is
generic (
   G_BOARD           : string;                             -- Which platform we are running on.
   G_SDRAM           : boolean := true;                    -- false: use HyperRAM only, as on R3
   G_STATS           : boolean := false                    -- the menu shows a virtual cartridge's bank waits
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
   qnice_hdmi_view_size_o  : out std_logic_vector(1 downto 0) := (others => '0');
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

   -- Block transfers to and from the active SD card (sdhost.vhd's engine port)
   qnice_sd_eng_o          : out t_sd_c2q;
   qnice_sd_eng_i          : in  t_sd_q2c;

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

   -- SDRAM (R4 and later), also on the HyperRAM clock: the virtual cartridge's file
   sr_core_refresh_ok_o    : out std_logic;
   sr_core_write_o         : out std_logic;
   sr_core_read_o          : out std_logic;
   sr_core_address_o       : out std_logic_vector(31 downto 0);
   sr_core_writedata_o     : out std_logic_vector(15 downto 0);
   sr_core_byteenable_o    : out std_logic_vector( 1 downto 0);
   sr_core_burstcount_o    : out std_logic_vector( 7 downto 0);
   sr_core_readdata_i      : in  std_logic_vector(15 downto 0);
   sr_core_readdatavalid_i : in  std_logic;
   sr_core_waitrequest_i   : in  std_logic;
   -- The system CPU's SDRAM master (rv_soc.vhd), shared out with the core's own
   sr_cpu_write_i          : in  std_logic;
   sr_cpu_read_i           : in  std_logic;
   sr_cpu_address_i        : in  std_logic_vector(31 downto 0);
   sr_cpu_writedata_i      : in  std_logic_vector(15 downto 0);
   sr_cpu_byteenable_i     : in  std_logic_vector( 1 downto 0);
   sr_cpu_burstcount_i     : in  std_logic_vector( 7 downto 0);
   sr_cpu_readdata_o       : out std_logic_vector(15 downto 0);
   sr_cpu_readdatavalid_o  : out std_logic;
   sr_cpu_waitrequest_o    : out std_logic;

   --------------------------------------------------------------------------------------------------------
   -- Video Clock Domain
   --------------------------------------------------------------------------------------------------------

   -- HDMI stream
   video_clk_o             : out std_logic;
   video_rst_o             : out std_logic;
   video_ce_o              : out std_logic;
   video_red_o             : out std_logic_vector(7 downto 0);
   video_green_o           : out std_logic_vector(7 downto 0);
   video_blue_o            : out std_logic_vector(7 downto 0);
   video_vs_o              : out std_logic;
   video_hs_o              : out std_logic;
   video_hblank_o          : out std_logic;
   video_vblank_o          : out std_logic;

   -- VGA stream: its own clock, so VGA can show the other chip
   vga_video_clk_o         : out std_logic;
   vga_video_rst_o         : out std_logic;
   vga_video_ce_o          : out std_logic;
   vga_video_ce_ovl_o      : out std_logic;
   vga_video_red_o         : out std_logic_vector(7 downto 0);
   vga_video_green_o       : out std_logic_vector(7 downto 0);
   vga_video_blue_o        : out std_logic_vector(7 downto 0);
   vga_video_vs_o          : out std_logic;
   vga_video_hs_o          : out std_logic;
   vga_video_hblank_o      : out std_logic;
   vga_video_vblank_o      : out std_logic;

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
   main_kb_key_enabled_i   : in  std_logic;              -- '0' while the on-screen menu owns the keyboard
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

   -- MEGA65 internal floppy drive (drive A of the connector), used as a 1581
   f_rdata_i               : in  std_logic;
   f_index_i               : in  std_logic;
   f_track0_i              : in  std_logic;
   f_writeprotect_i        : in  std_logic;
   f_diskchanged_i         : in  std_logic;
   f_motora_o              : out std_logic;
   f_selecta_o             : out std_logic;
   f_side1_o               : out std_logic;
   f_stepdir_o             : out std_logic;
   f_step_o                : out std_logic;
   f_density_o             : out std_logic;
   f_wgate_o               : out std_logic;
   f_wdata_o               : out std_logic;

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
   cart_d_o                : out unsigned( 7 downto 0);

   -- Network (SwiftLink modem, Ethernet): its 50 MHz clock and the Ethernet PHY
   modem_clk_i             : in    std_logic;
   modem_rst_i             : in    std_logic;
   eth_clock_o             : out   std_logic;
   eth_reset_o             : out   std_logic;
   eth_mdc_o               : out   std_logic;
   eth_mdio_io             : inout std_logic;
   eth_rxd_i               : in    std_logic_vector(1 downto 0);
   eth_rxdv_i              : in    std_logic;
   eth_rxer_i              : in    std_logic;
   eth_txd_o               : out   std_logic_vector(1 downto 0);
   eth_txen_o              : out   std_logic
);
end entity MEGA65_Core;

architecture synthesis of MEGA65_Core is

-- No SDRAM: the cartridge and the REU go in HyperRAM, and the REU is 512 KB at most
-- (doc/hyperram-map.md)
constant C_R3_MEMORY : boolean := G_BOARD = "MEGA65_R3" or not G_SDRAM;

-- R3-memory builds leave the 2 MB and 16 MB REU items out of the menu (config.vhd). A '0' in
-- each place keeps every C_MENU_* index, and the REU can't be bigger than 512 KB there.
function menu_bits(ctl : std_logic_vector(255 downto 0)) return std_logic_vector is
begin
   if C_R3_MEMORY then
      return ctl(255 - C_MENU_R3_DROP downto C_MENU_REU_2M) &
             std_logic_vector'(C_MENU_R3_DROP - 1 downto 0 => '0') & ctl(C_MENU_REU_2M - 1 downto 0);
   end if;
   return ctl;
end function menu_bits;

-- Word address of the REU's memory: HyperRAM, or SDRAM where the board has it
function reu_base return std_logic_vector is
begin
   if C_R3_MEMORY then
      return "0000000" & C_HMAP_REU(12 downto 0) & x"000";
   end if;
   return "0000000" & C_SMAP_REU(12 downto 0) & x"000";
end function reu_base;

-- The REU's memory: 2^n bytes (16 MiB in SDRAM, 512 KiB in HyperRAM)
function reu_room_bits return natural is
begin
   if C_R3_MEMORY then
      return 19;
   end if;
   return 24;
end function reu_room_bits;

-- Where the REU's memory starts, for the firmware (uci.vhd's word 0x011): bit 15 SDRAM, then
-- its 8 KiB window
function reu_mem_word return std_logic_vector is
begin
   if C_R3_MEMORY then
      return "000" & C_HMAP_REU(12 downto 0);
   end if;
   return "100" & C_SMAP_REU(12 downto 0);
end function reu_mem_word;

-- Word address of the .crt file
function crt_base return std_logic_vector is
begin
   if C_R3_MEMORY then
      return C_HMAP_CRT(9 downto 0) & x"000";
   end if;
   return C_SMAP_CRT(9 downto 0) & x"000";
end function crt_base;

signal qnice_osm_control   : std_logic_vector(255 downto 0);
signal main_osm_control    : std_logic_vector(255 downto 0);

---------------------------------------------------------------------------------------------
-- Clocks and active high reset signals for each clock domain
---------------------------------------------------------------------------------------------
signal vdc_clk             : std_logic;
signal vdc_clk_raw         : std_logic;
signal main_clk_raw        : std_logic;
signal vdc_clk_locked      : std_logic;
signal vdc_rst             : std_logic;
signal main_video_sel_vdc  : std_logic;
signal main_vga_sel_vdc    : std_logic;
signal video_clk_sel       : std_logic;

-- Power-on default is the original PAL speed; the process below has no reset, so the
-- initial value also keeps the clock mux select defined in simulation.
signal hr_core_speed : unsigned(1 downto 0) := "00"; -- see clock.vhd for details
signal hr_hdmi_ff          : std_logic;      -- "HDMI: Flicker-free" menu bit, synced to hr_clk
signal hr_video_sel_vdc    : std_logic;      -- HDMI shows the VDC, synced to hr_clk

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
signal qnice_drvrom_addr     : std_logic_vector(17 downto 0);
signal qnice_drvrom_d_to     : std_logic_vector(7 downto 0);
signal qnice_drvrom_d_from   : std_logic_vector(7 downto 0);
signal main_ram_addr         : unsigned(17 downto 0);
signal main_ram_data_to      : unsigned(7 downto 0);
signal main_ram_we           : std_logic;
signal main_ram_data_from    : unsigned(7 downto 0);
signal main_ram_q            : std_logic_vector(7 downto 0);
signal main_sysrom_addr      : std_logic_vector(16 downto 0);
signal main_sysrom_data      : std_logic_vector(7 downto 0);
-- The internal function ROM, in boot0.rom's device from 0x18000: 0 none, 1 16 KB, 2 32 KB
signal qnice_funcrom_size    : std_logic_vector(1 downto 0) := "00";
signal main_funcrom_size     : std_logic_vector(1 downto 0);

-- Virtual drives: vdrives.vhd lives inside main.vhd but is addressed from here
signal qnice_vd_ce           : std_logic;
signal qnice_vd_we           : std_logic;
signal qnice_vd_data         : std_logic_vector(15 downto 0);

-- Mounted disk images, straight from the SD card (vd_sd.vhd inside main.vhd)
signal qnice_vdsd_ce         : std_logic;
signal qnice_vdsd_we         : std_logic;
signal qnice_vdsd_data       : std_logic_vector(15 downto 0);
signal qnice_vdsd_drive      : natural range 0 to 1;

-- The SD controller's engine port, shared by the drives (A) and the .crt loader (B)
signal qnice_drv_eng_c2q     : t_sd_c2q;
signal qnice_drv_eng_q2c     : t_sd_q2c;
signal qnice_drv_eng_want    : std_logic;
signal qnice_crt_eng_c2q     : t_sd_c2q;
signal qnice_crt_eng_q2c     : t_sd_q2c;
signal qnice_crt_eng_want    : std_logic;

-- R3-memory builds: two Avalon masters share hr_core_*, the REU (s0) and the virtual cartridge
-- (s1), each in its region of doc/hyperram-map.md. Otherwise both are in SDRAM (sr_core_*).

-- REU: main_reu_* on main_clk, reu_hr_* on hr_clk
signal main_reu_write         : std_logic;
signal main_reu_read          : std_logic;
signal main_reu_address       : std_logic_vector(31 downto 0);
signal main_reu_writedata     : std_logic_vector(15 downto 0);
signal main_reu_byteenable    : std_logic_vector( 1 downto 0);
signal main_reu_burstcount    : std_logic_vector( 7 downto 0);
signal main_reu_readdata      : std_logic_vector(15 downto 0);
signal main_reu_readdatavalid : std_logic;
signal main_reu_waitrequest   : std_logic;
signal reu_hr_write           : std_logic;
signal reu_hr_read            : std_logic;
signal reu_hr_address         : std_logic_vector(31 downto 0);
signal reu_hr_writedata       : std_logic_vector(15 downto 0);
signal reu_hr_byteenable      : std_logic_vector( 1 downto 0);
signal reu_hr_burstcount      : std_logic_vector( 7 downto 0);
signal reu_hr_readdata        : std_logic_vector(15 downto 0);
signal reu_hr_readdatavalid   : std_logic;
signal reu_hr_waitrequest     : std_logic;
-- REU in HyperRAM (R3-memory builds)
signal reu_hm_write           : std_logic;
signal reu_hm_read            : std_logic;
signal reu_hm_readdata        : std_logic_vector(15 downto 0);
signal reu_hm_readdatavalid   : std_logic;
signal reu_hm_waitrequest     : std_logic;
-- REU in SDRAM: it may start an access while hr_reu_ok is high
signal main_reu_ok            : std_logic;
signal hr_reu_ok              : std_logic;

-- Virtual cartridge: the QNICE device that loads the .crt file, and its link to main.vhd
signal qnice_crt_ce           : std_logic;
signal qnice_crt_we           : std_logic;
signal qnice_crt_data         : std_logic_vector(15 downto 0);

-- internal floppy diagnostics (physical_1581_diag)
signal qnice_p1581_ce         : std_logic;
signal qnice_p1581_we         : std_logic;
signal qnice_p1581_data       : std_logic_vector(15 downto 0);
signal qnice_crt_wait         : std_logic;
signal main_crt_loaded        : std_logic;
signal main_crt_loading       : std_logic;
signal main_crt_id            : std_logic_vector(15 downto 0);
signal main_crt_exrom         : std_logic_vector( 7 downto 0);
signal main_crt_game          : std_logic_vector( 7 downto 0);
signal main_crt_size          : std_logic_vector(22 downto 0);
signal main_crt_c128          : std_logic;
signal main_crt_subtype       : std_logic_vector( 7 downto 0);
signal main_crt_bank_lo       : std_logic_vector( 6 downto 0);
signal main_crt_bank_hi       : std_logic_vector( 6 downto 0);
signal main_crt_wait          : std_logic;
signal main_crt_addr          : std_logic_vector(15 downto 0);
signal main_crt_data          : std_logic_vector( 7 downto 0);
signal main_crt_lo_data       : std_logic_vector( 7 downto 0);
signal main_crt_hi_data       : std_logic_vector( 7 downto 0);
signal main_crt_vic_data      : std_logic_vector( 7 downto 0);
signal main_crt_rd_lo         : std_logic;
signal main_crt_rd_hi         : std_logic;
signal main_crt_vic_rd        : std_logic;
signal main_crt_vic_addr      : std_logic_vector(12 downto 0);
signal main_crt_rfsh_ok       : std_logic;
signal main_crt_reset         : std_logic;
signal main_crt_phi2          : std_logic;
signal main_crt_flash_wr      : std_logic;
signal main_crt_flash_rd      : std_logic;
signal main_crt_ioe_we        : std_logic;
signal main_crt_iof_we        : std_logic;
signal main_crt_ram_we        : std_logic;
signal main_crt_ioe_data      : std_logic_vector( 7 downto 0);
signal main_crt_iof_data      : std_logic_vector( 7 downto 0);
signal main_crt_ram_data      : std_logic_vector( 7 downto 0);
signal crt_hr_write           : std_logic;
signal crt_hr_read            : std_logic;
signal crt_hr_address         : std_logic_vector(31 downto 0);
signal crt_hr_writedata       : std_logic_vector(15 downto 0);
signal crt_hr_byteenable      : std_logic_vector( 1 downto 0);
signal crt_hr_burstcount      : std_logic_vector( 7 downto 0);
signal crt_hr_readdata        : std_logic_vector(15 downto 0);
signal crt_hr_readdatavalid   : std_logic;
signal crt_hr_waitrequest     : std_logic;
signal crt_sr_write           : std_logic;
signal crt_sr_read            : std_logic;
signal crt_sr_address         : std_logic_vector(31 downto 0);
signal crt_sr_writedata       : std_logic_vector(15 downto 0);
signal crt_sr_byteenable      : std_logic_vector( 1 downto 0);
signal crt_sr_burstcount      : std_logic_vector( 7 downto 0);
signal crt_sr_readdata        : std_logic_vector(15 downto 0);
signal crt_sr_readdatavalid   : std_logic;
signal crt_sr_waitrequest     : std_logic;

-- SwiftLink modem: the 6551's bus on main_clk; the network device on the device bus
signal main_modem_en          : std_logic;
signal main_modem_rst         : std_logic;
signal main_modem_cs          : std_logic;
signal main_modem_we          : std_logic;
signal main_modem_addr        : std_logic_vector(2 downto 0);
signal main_modem_wdata       : std_logic_vector(7 downto 0);
signal main_modem_rdata       : std_logic_vector(7 downto 0);
signal main_modem_irq         : std_logic;
signal qnice_net_ce           : std_logic;
signal qnice_net_we           : std_logic;
signal qnice_net_data         : std_logic_vector(15 downto 0);

-- Ultimate Command Interface: the C128's side on main_clk, the firmware's on the device bus
signal main_uci_rst           : std_logic;
signal main_uci_cs            : std_logic;
signal main_uci_we            : std_logic;
signal main_uci_addr          : std_logic_vector(15 downto 0);
signal main_uci_wdata         : std_logic_vector(7 downto 0);
signal main_uci_rdata         : std_logic_vector(7 downto 0);
signal main_uci_irq           : std_logic;
signal main_uci_dma           : std_logic;
signal main_uci_reboot        : std_logic;
signal main_uci_reu_keep      : std_logic;
signal main_reu_menu          : std_logic_vector(1 downto 0);
signal qnice_uci_ce           : std_logic;
signal qnice_uci_we           : std_logic;
signal qnice_uci_data         : std_logic_vector(15 downto 0);
signal qnice_uci_wait         : std_logic;

-- The system CPU's bus DMA (c128_dma.vhd)
signal main_fwdma_req         : std_logic;
signal main_fwdma_cycle       : std_logic;
signal main_fwdma_addr        : unsigned(15 downto 0);
signal main_fwdma_dout        : unsigned(7 downto 0);
signal main_fwdma_we          : std_logic;
signal main_fwdma_din         : unsigned(7 downto 0);
signal main_fwdma_z80         : std_logic;
signal qnice_dma_ce           : std_logic;
signal qnice_dma_we           : std_logic;
signal qnice_dma_data         : std_logic_vector(15 downto 0);

-- The REU's SD card DMA (reu_sd.vhd): device, engine port, memory on the QNICE clock (dma_q_*)
-- and on hr_clk (dma_hr_*: SDRAM, dma_hm_*: HyperRAM on R3 memory)
signal qnice_reusd_ce         : std_logic;
signal qnice_reusd_we         : std_logic;
signal qnice_reusd_data       : std_logic_vector(15 downto 0);
signal qnice_reusd_eng_c2q    : t_sd_c2q;
signal qnice_reusd_eng_q2c    : t_sd_q2c;
signal qnice_reusd_eng_want   : std_logic;
signal qnice_dma_eng_c2q      : t_sd_c2q;      -- the .crt load and the REU, after their arbiter
signal qnice_dma_eng_q2c      : t_sd_q2c;
signal qnice_dma_eng_want     : std_logic;
signal main_reu_drop          : std_logic;
signal dma_q_write            : std_logic;
signal dma_q_read             : std_logic;
signal dma_q_address          : std_logic_vector(31 downto 0);
signal dma_q_writedata        : std_logic_vector(15 downto 0);
signal dma_q_byteenable       : std_logic_vector( 1 downto 0);
signal dma_q_burstcount       : std_logic_vector( 7 downto 0);
signal dma_q_readdata         : std_logic_vector(15 downto 0);
signal dma_q_readdatavalid    : std_logic;
signal dma_q_waitrequest      : std_logic;
signal dma_hr_write           : std_logic;
signal dma_hr_read            : std_logic;
signal dma_hr_address         : std_logic_vector(31 downto 0);
signal dma_hr_writedata       : std_logic_vector(15 downto 0);
signal dma_hr_byteenable      : std_logic_vector( 1 downto 0);
signal dma_hr_burstcount      : std_logic_vector( 7 downto 0);
signal dma_hr_readdata        : std_logic_vector(15 downto 0);
signal dma_hr_readdatavalid   : std_logic;
signal dma_hr_waitrequest     : std_logic;
signal dma_hm_write           : std_logic;
signal dma_hm_read            : std_logic;
signal dma_hm_readdata        : std_logic_vector(15 downto 0);
signal dma_hm_readdatavalid   : std_logic;
signal dma_hm_waitrequest     : std_logic;

-- REU and cartridge, after their arbiter
signal exp_hr_write           : std_logic;
signal exp_hr_read            : std_logic;
signal exp_hr_address         : std_logic_vector(31 downto 0);
signal exp_hr_writedata       : std_logic_vector(15 downto 0);
signal exp_hr_byteenable      : std_logic_vector( 1 downto 0);
signal exp_hr_burstcount      : std_logic_vector( 7 downto 0);
signal exp_hr_readdata        : std_logic_vector(15 downto 0);
signal exp_hr_readdatavalid   : std_logic;
signal exp_hr_waitrequest     : std_logic;

-- Drive ROM server: hands boot1.rom to iec_drive byte by byte (QNICE clock domain)
signal qnice_drvrom_b_addr   : std_logic_vector(17 downto 0) := (others => '0');
signal qnice_drvrom_b_data   : std_logic_vector(7 downto 0);
signal qnice_drv_rom_loading : std_logic;
signal qnice_drv_rom_req     : std_logic;
signal qnice_drv_rom_addr    : std_logic_vector(18 downto 0);
signal qnice_drv_rom_wr      : std_logic := '0';
signal qnice_drv_reload_f    : std_logic_vector(1 downto 0) := "00";   -- CSR write, falling edge
signal qnice_drv_reload      : std_logic_vector(1 downto 0) := "00";   -- a pulse per drive
signal qnice_drv_rom_ready   : std_logic_vector(1 downto 0);
signal qnice_core_rst        : std_logic;
-- F9 latch, synchronized from the core clock into the QNICE clock for the menu title.
signal f9_latched            : std_logic;

begin

   qnice_osm_control <= menu_bits(qnice_osm_control_i);
   main_osm_control  <= menu_bits(main_osm_control_i);

   -- The Expansion Port is a pure pass-through here: every cart_* signal goes straight to
   -- main.vhd, which owns the bus timing and the transceiver directions. The same is true for
   -- the IEC buffer enables (iec_*_en_o), the reset line (iec_reset_n_o) and the line drivers,
   -- which implement the open-collector behaviour needed to talk to real Commodore drives.
   -- See the i_main port map below.

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
   -- MMCME2_ADV clock generators
   --   PAL: 31.528 MHz (main) and 63.056 MHz (video)
   --        HDMI: Flicker-free: 0.25% slower
   clk_gen : entity work.clk
      port map (
         sys_clk_i         => clk_i,           -- expects 100 MHz
         core_speed_i      => hr_core_speed,   -- 0=PAL/original C64, 1=PAL/HDMI flicker-free, 2=NTSC
         main_clk_o        => main_clk_o,        -- CORE's clock
         main_clk_raw_o    => main_clk_raw,      -- speed-selected core clock for the video mux
         main_rst_o        => main_rst_o         -- CORE's reset, synchronized
      ); -- clk_gen


   -- Dedicated 32.000 MHz VDC clock (MiSTer pll_vdc). CORE.xdc explains why the crossing
   -- to main_clk is safe and how it is constrained.
   clk_vdc_gen: entity work.vdc_clk
      port map (
         refclk       => clk_i,
         outclk_0     => vdc_clk,
         outclk_raw_o => vdc_clk_raw,
         locked       => vdc_clk_locked
      );

   i_xpm_cdc_async_rst_vdc: xpm_cdc_async_rst
      generic map (
         RST_ACTIVE_HIGH => 1,
         DEST_SYNC_FF    => 6
      )
      port map (
         src_arst  => not vdc_clk_locked,
         dest_clk  => vdc_clk,
         dest_arst => vdc_rst
      );

   -- HDMI video_clk: mux the clock nets in front of the BUFGs to avoid a BUFG->BUFGMUX
   -- cascade. main_clk_raw follows the flicker-free selection, so the VIC video stream is
   -- always sampled by the same frequency that generated it.
   -- When VDC is shown, video_* are synchronous to vdc_clk / this mux output.
   video_clk_sel <= main_video_sel_vdc;
   i_video_clk_mux: bufgmux_ctrl
      port map (
         i0 => main_clk_raw,
         i1 => vdc_clk_raw,
         s  => video_clk_sel,
         o  => video_clk_o
      );

   video_rst_o <= vdc_rst when main_video_sel_vdc = '1' else main_rst_o;

   -- VGA vga_video_clk: the same mux again, so VGA can show the other chip than HDMI.
   i_vga_clk_mux: bufgmux_ctrl
      port map (
         i0 => main_clk_raw,
         i1 => vdc_clk_raw,
         s  => main_vga_sel_vdc,
         o  => vga_video_clk_o
      );

   vga_video_rst_o <= vdc_rst when main_vga_sel_vdc = '1' else main_rst_o;

   ---------------------------------------------------------------------------------------------
   -- hr_clk (HyperRAM clock): "HDMI: Flicker-free"
   ---------------------------------------------------------------------------------------------

   -- Clock Domain Crossing: CORE -> HyperRAM
   i_cdc_main2hr : entity work.cdc_stable
      generic map (
         G_DATA_SIZE => 2
      )
      port map (
         src_clk_i     => main_clk_o,
         src_data_i(0) => main_osm_control(C_MENU_HDMI_FF),
         src_data_i(1) => main_video_sel_vdc,
         dst_clk_i     => hr_clk_i,
         dst_data_o(0) => hr_hdmi_ff,
         dst_data_o(1) => hr_video_sel_vdc
      ); -- i_cdc_main2hr

   -- The core's PAL frame rate of 50.124 Hz does not match the exactly 50 Hz that every HDMI
   -- mode outputs, so ascal's frame buffer in HyperRAM slowly drifts until read and write
   -- cross over and the picture tears. The framework reports the drift direction through
   -- hr_high_i / hr_low_i, and we answer by selecting one of the two core clocks that
   -- "embrace" 50 Hz (see clk.vhd). On average the core then runs at exactly 50 Hz.
   --
   -- Only the VIC benefits: the VDC has its own 32 MHz MMCM that is not part of the switch,
   -- so while HDMI shows the VDC we keep the core at full speed instead of slowing it down
   -- without gaining anything. VGA has no frame buffer and does not care.
   p_core_speed : process (hr_clk_i)
   begin
      if rising_edge(hr_clk_i) then
         if hr_low_i = '1' then     -- the core is too slow ...
            hr_core_speed <= "00";  -- ... switch to PAL original (50.124 Hz)
         end if;
         if hr_high_i = '1' then    -- the core is too fast ...
            hr_core_speed <= "01";  -- ... switch to PAL slow (49.999 Hz)
         end if;
         if hr_hdmi_ff = '0' or hr_video_sel_vdc = '1' then
            hr_core_speed <= "00";
         end if;
      end if;
   end process p_core_speed;

   -- Power LED stays on. Green means the latched F9 key is released, yellow means it is pressed.
   main_power_led_o     <= '1';
   main_power_led_col_o <= x"FFFF00" when f9_latched = '1' else x"00FF00";

   -- main.vhd contains the actual MiSTer core
   i_main : entity work.main
      generic map (
         G_BOARD              => G_BOARD,   -- Which platform are we running on
         G_VDNUM              => C_VDNUM,
         G_REU_BASE           => reu_base
      )
      port map (
         clk_main_i           => main_clk_o,
         clk_vdc_i            => vdc_clk,
         clk_sd_i             => qnice_clk_i,
         reset_soft_i         => main_reset_core_i,
         reset_hard_i         => main_reset_m2m_i,
         pause_i              => main_pause_core_i,

         clk_main_speed_i     => CORE_CLK_SPEED,
         osm_control_i        => main_osm_control,
         f9_latched_o         => f9_latched,

         -- HDMI video output (domain follows video_select_vdc_o / BUFGMUX above)
         video_ce_o           => video_ce_o,
         video_red_o          => video_red_o,
         video_green_o        => video_green_o,
         video_blue_o         => video_blue_o,
         video_vs_o           => video_vs_o,
         video_hs_o           => video_hs_o,
         video_hblank_o       => video_hblank_o,
         video_vblank_o       => video_vblank_o,
         video_select_vdc_o   => main_video_sel_vdc,

         -- VGA video output (domain follows vga_video_select_vdc_o / i_vga_clk_mux)
         vga_video_ce_o         => vga_video_ce_o,
         vga_video_ce_ovl_o     => vga_video_ce_ovl_o,
         vga_video_red_o        => vga_video_red_o,
         vga_video_green_o      => vga_video_green_o,
         vga_video_blue_o       => vga_video_blue_o,
         vga_video_vs_o         => vga_video_vs_o,
         vga_video_hs_o         => vga_video_hs_o,
         vga_video_hblank_o     => vga_video_hblank_o,
         vga_video_vblank_o     => vga_video_vblank_o,
         vga_video_select_vdc_o => main_vga_sel_vdc,

         -- audio output (pcm format, signed values)
         audio_left_o         => main_audio_left_o,
         audio_right_o        => main_audio_right_o,

         -- Drive led
         drive_led_o => main_drive_led_o,
         drive_led_col_o => main_drive_led_col_o,
         boot_z80_n_o => open,

         -- M2M Keyboard interface
         kb_key_num_i         => main_kb_key_num_i,
         kb_key_pressed_n_i   => main_kb_key_pressed_n_i,
         kb_key_enabled_i     => main_kb_key_enabled_i,

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
         func_rom_size_i      => main_funcrom_size,

         -- Expansion Port (aka Cartridge Port)
         cart_en_o            => cart_en_o,
         cart_phi2_o          => cart_phi2_o,
         cart_dotclock_o      => cart_dotclock_o,
         cart_dma_i           => cart_dma_i,
         cart_reset_oe_o      => cart_reset_oe_o,
         cart_reset_i         => cart_reset_i,
         cart_reset_o         => cart_reset_o,
         cart_game_oe_o       => cart_game_oe_o,
         cart_game_i          => cart_game_i,
         cart_game_o          => cart_game_o,
         cart_exrom_oe_o      => cart_exrom_oe_o,
         cart_exrom_i         => cart_exrom_i,
         cart_exrom_o         => cart_exrom_o,
         cart_nmi_oe_o        => cart_nmi_oe_o,
         cart_nmi_i           => cart_nmi_i,
         cart_nmi_o           => cart_nmi_o,
         cart_irq_oe_o        => cart_irq_oe_o,
         cart_irq_i           => cart_irq_i,
         cart_irq_o           => cart_irq_o,
         cart_roml_oe_o       => cart_roml_oe_o,
         cart_roml_i          => cart_roml_i,
         cart_roml_o          => cart_roml_o,
         cart_romh_oe_o       => cart_romh_oe_o,
         cart_romh_i          => cart_romh_i,
         cart_romh_o          => cart_romh_o,
         cart_ctrl_oe_o       => cart_ctrl_oe_o,
         cart_ba_i            => cart_ba_i,
         cart_rw_i            => cart_rw_i,
         cart_io1_i           => cart_io1_i,
         cart_io2_i           => cart_io2_i,
         cart_ba_o            => cart_ba_o,
         cart_rw_o            => cart_rw_o,
         cart_io1_o           => cart_io1_o,
         cart_io2_o           => cart_io2_o,
         cart_addr_oe_o       => cart_addr_oe_o,
         cart_a_i             => cart_a_i,
         cart_a_o             => cart_a_o,
         cart_data_oe_o       => cart_data_oe_o,
         cart_d_i             => cart_d_i,
         cart_d_o             => cart_d_o,

         iec_hardware_port_en_i => main_osm_control(C_MENU_IEC),
         iec_reset_n_o        => iec_reset_n_o,
         iec_atn_n_o          => iec_atn_n_o,
         iec_clk_en_o         => iec_clk_en_o,
         iec_clk_n_o          => iec_clk_n_o,
         iec_clk_n_i          => iec_clk_n_i,
         iec_data_en_o        => iec_data_en_o,
         iec_data_n_o         => iec_data_n_o,
         iec_data_n_i         => iec_data_n_i,
         iec_srq_en_o         => iec_srq_en_o,
         iec_srq_n_o          => iec_srq_n_o,
         iec_srq_n_i          => iec_srq_n_i,

         -- Virtual drives
         qnice_vd_addr_i      => qnice_dev_addr_i,
         qnice_vd_data_i      => qnice_dev_data_i,
         qnice_vd_data_o      => qnice_vd_data,
         qnice_vd_ce_i        => qnice_vd_ce,
         qnice_vd_we_i        => qnice_vd_we,

         drv_rom_loading_i    => qnice_drv_rom_loading,
         drv_rom_reload_i     => qnice_drv_reload,
         drv_rom_ready_o      => qnice_drv_rom_ready,
         drv_rom_req_o        => qnice_drv_rom_req,
         drv_rom_addr_o       => qnice_drv_rom_addr,
         drv_rom_data_i       => qnice_drvrom_b_data,
         drv_rom_wr_i         => qnice_drv_rom_wr,

         rst_sd_i             => qnice_rst_i,
         qnice_vdsd_drive_i   => qnice_vdsd_drive,
         qnice_vdsd_addr_i    => qnice_dev_addr_i,
         qnice_vdsd_data_i    => qnice_dev_data_i,
         qnice_vdsd_data_o    => qnice_vdsd_data,
         qnice_vdsd_ce_i      => qnice_vdsd_ce,
         qnice_vdsd_we_i      => qnice_vdsd_we,
         qnice_sd_want_o      => qnice_drv_eng_want,
         qnice_sd_eng_o       => qnice_drv_eng_c2q,
         qnice_sd_eng_i       => qnice_drv_eng_q2c,

         f_rdata_i            => f_rdata_i,
         f_index_i            => f_index_i,
         f_track0_i           => f_track0_i,
         f_writeprotect_i     => f_writeprotect_i,
         f_diskchanged_i      => f_diskchanged_i,
         f_motora_o           => f_motora_o,
         f_selecta_o          => f_selecta_o,
         f_side1_o            => f_side1_o,
         f_stepdir_o          => f_stepdir_o,
         f_step_o             => f_step_o,
         f_density_o          => f_density_o,
         f_wgate_o            => f_wgate_o,
         f_wdata_o            => f_wdata_o,

         qnice_p1581_ce_i     => qnice_p1581_ce,
         qnice_p1581_we_i     => qnice_p1581_we,
         qnice_p1581_addr_i   => qnice_dev_addr_i(7 downto 0),
         qnice_p1581_data_i   => qnice_dev_data_i,
         qnice_p1581_data_o   => qnice_p1581_data,

         reu_avm_write_o         => main_reu_write,
         reu_avm_read_o          => main_reu_read,
         reu_avm_address_o       => main_reu_address,
         reu_avm_writedata_o     => main_reu_writedata,
         reu_avm_byteenable_o    => main_reu_byteenable,
         reu_avm_burstcount_o    => main_reu_burstcount,
         reu_avm_readdata_i      => main_reu_readdata,
         reu_avm_readdatavalid_i => main_reu_readdatavalid,
         reu_avm_waitrequest_i   => main_reu_waitrequest,

         modem_en_o           => main_modem_en,
         modem_rst_o          => main_modem_rst,
         modem_cs_o           => main_modem_cs,
         modem_we_o           => main_modem_we,
         modem_addr_o         => main_modem_addr,
         modem_data_o         => main_modem_wdata,
         modem_data_i         => main_modem_rdata,
         modem_irq_i          => main_modem_irq,

         uci_rst_o            => main_uci_rst,
         uci_cs_o             => main_uci_cs,
         uci_we_o             => main_uci_we,
         uci_addr_o           => main_uci_addr,
         uci_data_o           => main_uci_wdata,
         uci_data_i           => main_uci_rdata,
         uci_irq_i            => main_uci_irq,
         uci_dma_i            => main_uci_dma,
         uci_reboot_i         => main_uci_reboot,
         uci_reu_keep_i       => main_uci_reu_keep,
         reu_menu_o           => main_reu_menu,
         reu_drop_i           => main_reu_drop,

         fwdma_req_i          => main_fwdma_req,
         fwdma_cycle_o        => main_fwdma_cycle,
         fwdma_addr_i         => main_fwdma_addr,
         fwdma_dout_i         => main_fwdma_dout,
         fwdma_we_i           => main_fwdma_we,
         fwdma_din_o          => main_fwdma_din,
         fwdma_z80_o          => main_fwdma_z80,

         crt_loaded_i         => main_crt_loaded,
         crt_loading_i        => main_crt_loading,
         crt_id_i             => main_crt_id,
         crt_exrom_i          => main_crt_exrom,
         crt_game_i           => main_crt_game,
         crt_size_i           => main_crt_size,
         crt_c128_i           => main_crt_c128,
         crt_subtype_i        => main_crt_subtype,
         crt_bank_lo_o        => main_crt_bank_lo,
         crt_bank_hi_o        => main_crt_bank_hi,
         crt_wait_i           => main_crt_wait,
         crt_addr_o           => main_crt_addr,
         crt_data_o           => main_crt_data,
         crt_lo_data_i        => main_crt_lo_data,
         crt_hi_data_i        => main_crt_hi_data,
         crt_vic_data_i       => main_crt_vic_data,
         crt_rd_lo_o          => main_crt_rd_lo,
         crt_rd_hi_o          => main_crt_rd_hi,
         crt_vic_rd_o         => main_crt_vic_rd,
         crt_vic_addr_o       => main_crt_vic_addr,
         crt_rfsh_ok_o        => main_crt_rfsh_ok,
         crt_reu_ok_o         => main_reu_ok,
         crt_reset_o          => main_crt_reset,
         crt_phi2_o           => main_crt_phi2,
         crt_flash_wr_o       => main_crt_flash_wr,
         crt_flash_rd_o       => main_crt_flash_rd,
         crt_ioe_we_o         => main_crt_ioe_we,
         crt_iof_we_o         => main_crt_iof_we,
         crt_ram_we_o         => main_crt_ram_we,
         crt_ioe_data_i       => main_crt_ioe_data,
         crt_iof_data_i       => main_crt_iof_data,
         crt_ram_data_i       => main_crt_ram_data
      ); -- i_main

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
   --
   -- The selection is independent of whether the VIC or the VDC is shown: ascal detects the
   -- incoming image geometry on its own (iauto='1'), so both sources are scaled into the very
   -- same output rectangle of the selected HDMI mode.
   qnice_video_mode_o <= C_VIDEO_SVGA_800_60   when qnice_osm_control(C_MENU_HDMI_800_60)      = '1' else
                         C_VIDEO_HDMI_720_5994 when qnice_osm_control(C_MENU_HDMI_720_5994)    = '1' else
                         C_VIDEO_HDMI_640_60   when qnice_osm_control(C_MENU_HDMI_640_60)      = '1' else
                         C_VIDEO_HDMI_5_4_50   when qnice_osm_control(C_MENU_HDMI_576p_50_5_4) = '1' else
                         C_VIDEO_HDMI_4_3_50   when qnice_osm_control(C_MENU_HDMI_576p_50)     = '1' else
                         C_VIDEO_HDMI_16_9_60  when qnice_osm_control(C_MENU_HDMI_720p_60)     = '1' else
                         C_VIDEO_HDMI_16_9_50;                       -- C_MENU_HDMI_720p_50

   -- Use On-Screen-Menu selections to configure several audio and video settings
   -- Video and audio mode control
   -- "HDMI audio" defaults to selected. Clearing it requests a DVI stream:
   -- video only, no audio packets. The 3.5 mm headphone output is unaffected.
   qnice_dvi_o                <= not qnice_osm_control(C_MENU_HDMI_AUDIO);
   -- VGA Standard doubles each line so the VGA connector runs at ~31.3 kHz
   -- instead of the native 40-column and 80-column ~15.6 kHz. Pulse width and
   -- polarity come from VGA_STD_SYNC. The two retro 15 kHz modes send the
   -- native lines unchanged for RGB monitors and SCART. HDMI does not use
   -- this; the ascaler retimes its own output.
   qnice_scandoubler_o        <= (not qnice_osm_control(C_MENU_VGA_15KHZHSVS)) and
                                 (not qnice_osm_control(C_MENU_VGA_15KHZCS));
   qnice_audio_mute_o         <= '0';                                         -- audio is not muted
   -- Audio filtering and HDMI zoom/crop are not offered in the menu yet, see doc/KNOWN_BUGS.md
   qnice_audio_filter_o       <= '0';                                         -- 0 = raw audio, 1 = use filters from globals.vhd
   qnice_zoom_crop_o          <= '0';                                         -- 0 = no zoom/crop
   qnice_hdmi_view_size_o     <= (others => '0');                             -- use the first cropped-view size

   -- Retro 15 kHz bypasses the sync reshaper and tells the on-screen menu that
   -- each line is still the original television line. Composite sync puts
   -- CSYNC on VGA pin 13 and holds pin 14 high, which is what most RGB
   -- monitors (Sony BVM/PVM) and the MiSTer VGA-to-SCART adaptor expect.
   qnice_retro15kHz_o         <= qnice_osm_control(C_MENU_VGA_15KHZHSVS) or
                                 qnice_osm_control(C_MENU_VGA_15KHZCS);
   qnice_csync_o              <= qnice_osm_control(C_MENU_VGA_15KHZCS);
   qnice_osm_cfg_scaling_o    <= (others => '1');

   -- ascal filters that are applied while processing the input
   -- 00 : Nearest Neighbour
   -- 01 : Bilinear
   -- 10 : Sharp Bilinear
   -- 11 : Bicubic
   qnice_ascal_mode_o         <= "00";

   -- If polyphase is '1' then the ascal filter mode is ignored and polyphase filters are used instead
   -- @TODO: Right now, the filters are hardcoded in the M2M framework, we need to make them changeable inside m2m-rom.asm
   qnice_ascal_polyphase_o    <= qnice_osm_control(C_MENU_CRT_EMULATION);

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
      qnice_vd_ce          <= '0';
      qnice_vd_we          <= '0';
      qnice_vdsd_ce        <= '0';
      qnice_vdsd_we        <= '0';
      qnice_crt_ce         <= '0';
      qnice_crt_we         <= '0';
      qnice_p1581_ce       <= '0';
      qnice_p1581_we       <= '0';
      qnice_uci_ce         <= '0';
      qnice_uci_we         <= '0';
      qnice_reusd_ce       <= '0';
      qnice_reusd_we       <= '0';
      qnice_net_ce         <= '0';
      qnice_net_we         <= '0';
      qnice_dma_ce         <= '0';
      qnice_dma_we         <= '0';

      case qnice_dev_id_i is
         -- Device numbers need to be >= 0x0100
        when C_DEV_RAM => 
            qnice_c64_ramx_addr <= qnice_dev_addr_i(17 downto 0);
            qnice_c64_ramx_we   <= qnice_dev_we_i;
            qnice_c64_ramx_d_to <= qnice_dev_data_i(7 downto 0);
            qnice_dev_data_o    <= x"00" & qnice_c64_ramx_d_from;
        -- boot0.rom, and the internal function ROM from 0x18000; its CSR window: register 0
        -- the function ROM's size, 0 none, 1 16 KB, 2 32 KB
        when C_DEV_SYSTEM_ROM =>
            qnice_sysrom_addr <= qnice_dev_addr_i(16 downto 0);
            qnice_sysrom_d_to <= qnice_dev_data_i(7 downto 0);
            if qnice_dev_addr_i(27 downto 12) = x"FFFF" then
               qnice_dev_data_o  <= x"000" & "00" & qnice_funcrom_size;
            else
               qnice_sysrom_we   <= qnice_dev_we_i;
               qnice_dev_data_o  <= x"00" & qnice_sysrom_d_from;
            end if;
        -- boot1.rom; its CSR window: register 0 R: bit i drive i has its ROM, W: bit i
        -- reloads drive i's bank, holding the drive in reset meanwhile (REST's load_rom)
        when C_DEV_DRIVE_ROM =>
            qnice_drvrom_addr <= qnice_dev_addr_i(17 downto 0);
            qnice_drvrom_d_to <= qnice_dev_data_i(7 downto 0);
            if qnice_dev_addr_i(27 downto 12) = x"FFFF" then
               qnice_dev_data_o  <= x"000" & "00" & qnice_drv_rom_ready;
            else
               qnice_drvrom_we   <= qnice_dev_we_i;
               qnice_dev_data_o  <= x"00" & qnice_drvrom_d_from;
            end if;

        -- Virtual drive control and data registers (vdrives.vhd inside main.vhd)
        when C_DEV_VDRIVES =>
            qnice_vd_ce       <= qnice_dev_ce_i;
            qnice_vd_we       <= qnice_dev_we_i;
            qnice_dev_data_o  <= qnice_vd_data;

        -- Mounted disk images: the block tables and controls of vd_sd (inside main.vhd)
        when C_DEV_MOUNT_D8 | C_DEV_MOUNT_D9 =>
            qnice_vdsd_ce     <= qnice_dev_ce_i;
            qnice_vdsd_we     <= qnice_dev_we_i;
            qnice_dev_data_o  <= qnice_vdsd_data;

        -- Virtual cartridge: the .crt file goes to HyperRAM, the CSRs start the parser
        when C_DEV_CRT =>
            qnice_crt_ce      <= qnice_dev_ce_i;
            qnice_crt_we      <= qnice_dev_we_i;
            qnice_dev_data_o  <= qnice_crt_data;
            qnice_dev_wait_o  <= qnice_crt_wait;

        -- Internal floppy diagnostics (physical_1581_diag inside main.vhd), read-only
        -- except for its precompensation override
        when C_DEV_PHYS1581 =>
            qnice_p1581_ce    <= qnice_dev_ce_i;
            qnice_p1581_we    <= qnice_dev_we_i;
            qnice_dev_data_o  <= qnice_p1581_data;

        -- Ultimate Command Interface: its registers wait while they cross to the core's clock
        when C_DEV_UCI =>
            qnice_uci_ce      <= qnice_dev_ce_i;
            qnice_uci_we      <= qnice_dev_we_i;
            qnice_dev_data_o  <= qnice_uci_data;
            qnice_dev_wait_o  <= qnice_uci_wait;

        -- The REU's SD card DMA
        when C_DEV_REU_SD =>
            qnice_reusd_ce    <= qnice_dev_ce_i;
            qnice_reusd_we    <= qnice_dev_we_i;
            qnice_dev_data_o  <= qnice_reusd_data;

        -- Network: the 6551's FIFOs, the Ethernet MAC, the device DNA (net.vhd)
        when C_DEV_NET =>
            qnice_net_ce      <= qnice_dev_ce_i;
            qnice_net_we      <= qnice_dev_we_i;
            qnice_dev_data_o  <= qnice_net_data;

        -- The system CPU's bus DMA into C128 memory (c128_dma.vhd)
        when C_DEV_DMA =>
            qnice_dma_ce      <= qnice_dev_ce_i;
            qnice_dma_we      <= qnice_dev_we_i;
            qnice_dev_data_o  <= qnice_dma_data;

         when others => null;
      end case;
   end process core_specific_devices;

   qnice_vdsd_drive <= 1 when qnice_dev_id_i = C_DEV_MOUNT_D9 else 0;

   -- The drives get the SD controller first; a .crt load or the REU's DMA waits between its
   -- transfers
   i_sd_eng_arb : entity work.sd_eng_arb
      port map (
         clk_i    => qnice_clk_i,
         rst_i    => qnice_rst_i,
         a_want_i => qnice_drv_eng_want,
         a_c2q_i  => qnice_drv_eng_c2q,
         a_q2c_o  => qnice_drv_eng_q2c,
         b_want_i => qnice_dma_eng_want,
         b_c2q_i  => qnice_dma_eng_c2q,
         b_q2c_o  => qnice_dma_eng_q2c,
         c2q_o    => qnice_sd_eng_o,
         q2c_i    => qnice_sd_eng_i,
         want_o   => open
      ); -- i_sd_eng_arb

   i_sd_eng_arb_dma : entity work.sd_eng_arb
      port map (
         clk_i    => qnice_clk_i,
         rst_i    => qnice_rst_i,
         a_want_i => qnice_crt_eng_want,
         a_c2q_i  => qnice_crt_eng_c2q,
         a_q2c_o  => qnice_crt_eng_q2c,
         b_want_i => qnice_reusd_eng_want,
         b_c2q_i  => qnice_reusd_eng_c2q,
         b_q2c_o  => qnice_reusd_eng_q2c,
         c2q_o    => qnice_dma_eng_c2q,
         q2c_i    => qnice_dma_eng_q2c,
         want_o   => qnice_dma_eng_want
      ); -- i_sd_eng_arb_dma

   -- The REU's image files straight between the SD card and its memory; also tells the REU's
   -- cache when the system CPU wrote that memory
   i_reu_sd : entity work.reu_sd
      generic map (
         G_BASE      => reu_base,
         G_ROOM_BITS => reu_room_bits
      )
      port map (
         clk_i               => qnice_clk_i,
         rst_i               => qnice_rst_i,
         qnice_addr_i        => qnice_dev_addr_i(11 downto 0),
         qnice_data_i        => qnice_dev_data_i,
         qnice_ce_i          => qnice_reusd_ce,
         qnice_we_i          => qnice_reusd_we,
         qnice_data_o        => qnice_reusd_data,
         eng_want_o          => qnice_reusd_eng_want,
         eng_o               => qnice_reusd_eng_c2q,
         eng_i               => qnice_reusd_eng_q2c,
         avm_write_o         => dma_q_write,
         avm_read_o          => dma_q_read,
         avm_address_o       => dma_q_address,
         avm_writedata_o     => dma_q_writedata,
         avm_byteenable_o    => dma_q_byteenable,
         avm_burstcount_o    => dma_q_burstcount,
         avm_readdata_i      => dma_q_readdata,
         avm_readdatavalid_i => dma_q_readdatavalid,
         avm_waitrequest_i   => dma_q_waitrequest,
         main_clk_i          => main_clk_o,
         main_drop_o         => main_reu_drop
      ); -- i_reu_sd

   i_reu_sd_avm_fifo : entity work.avm_fifo
      generic map (
         G_WR_DEPTH     => 16,
         G_RD_DEPTH     => 16,
         G_FILL_SIZE    => 1,
         G_ADDRESS_SIZE => 32,
         G_DATA_SIZE    => 16
      )
      port map (
         s_clk_i               => qnice_clk_i,
         s_rst_i               => qnice_rst_i,
         s_avm_waitrequest_o   => dma_q_waitrequest,
         s_avm_write_i         => dma_q_write,
         s_avm_read_i          => dma_q_read,
         s_avm_address_i       => dma_q_address,
         s_avm_writedata_i     => dma_q_writedata,
         s_avm_byteenable_i    => dma_q_byteenable,
         s_avm_burstcount_i    => dma_q_burstcount,
         s_avm_readdata_o      => dma_q_readdata,
         s_avm_readdatavalid_o => dma_q_readdatavalid,
         m_clk_i               => hr_clk_i,
         m_rst_i               => hr_rst_i,
         m_avm_waitrequest_i   => dma_hr_waitrequest,
         m_avm_write_o         => dma_hr_write,
         m_avm_read_o          => dma_hr_read,
         m_avm_address_o       => dma_hr_address,
         m_avm_writedata_o     => dma_hr_writedata,
         m_avm_byteenable_o    => dma_hr_byteenable,
         m_avm_burstcount_o    => dma_hr_burstcount,
         m_avm_readdata_i      => dma_hr_readdata,
         m_avm_readdatavalid_i => dma_hr_readdatavalid
      ); -- i_reu_sd_avm_fifo

   -- REU: cross its memory port from main_clk to hr_clk
   i_reu_avm_fifo : entity work.avm_fifo
      generic map (
         G_WR_DEPTH     => 16,
         G_RD_DEPTH     => 16,
         G_FILL_SIZE    => 1,
         G_ADDRESS_SIZE => 32,
         G_DATA_SIZE    => 16
      )
      port map (
         s_clk_i               => main_clk_o,
         s_rst_i               => main_reset_m2m_i,
         s_avm_waitrequest_o   => main_reu_waitrequest,
         s_avm_write_i         => main_reu_write,
         s_avm_read_i          => main_reu_read,
         s_avm_address_i       => main_reu_address,
         s_avm_writedata_i     => main_reu_writedata,
         s_avm_byteenable_i    => main_reu_byteenable,
         s_avm_burstcount_i    => main_reu_burstcount,
         s_avm_readdata_o      => main_reu_readdata,
         s_avm_readdatavalid_o => main_reu_readdatavalid,
         m_clk_i               => hr_clk_i,
         m_rst_i               => hr_rst_i,
         m_avm_waitrequest_i   => reu_hr_waitrequest,
         m_avm_write_o         => reu_hr_write,
         m_avm_read_o          => reu_hr_read,
         m_avm_address_o       => reu_hr_address,
         m_avm_writedata_o     => reu_hr_writedata,
         m_avm_byteenable_o    => reu_hr_byteenable,
         m_avm_burstcount_o    => reu_hr_burstcount,
         m_avm_readdata_i      => reu_hr_readdata,
         m_avm_readdatavalid_i => reu_hr_readdatavalid
      ); -- i_reu_avm_fifo

   -- Virtual cartridge: the .crt file in SDRAM where the board has it, read for each access.
   -- On R3 it goes in its HyperRAM region, its banks cached in block RAM.
   i_sw_cartridge_wrapper : entity work.sw_cartridge_wrapper
      generic map (
         G_BASE_ADDRESS => crt_base,
         G_SDRAM        => not C_R3_MEMORY,
         G_STATS        => G_STATS
      )
      port map (
         qnice_clk_i        => qnice_clk_i,
         qnice_rst_i        => qnice_rst_i,
         qnice_addr_i       => qnice_dev_addr_i,
         qnice_data_i       => qnice_dev_data_i,
         qnice_ce_i         => qnice_crt_ce,
         qnice_we_i         => qnice_crt_we,
         qnice_data_o       => qnice_crt_data,
         qnice_wait_o       => qnice_crt_wait,
         qnice_sd_want_o    => qnice_crt_eng_want,
         qnice_sd_eng_o     => qnice_crt_eng_c2q,
         qnice_sd_eng_i     => qnice_crt_eng_q2c,
         main_clk_i         => main_clk_o,
         main_loaded_o      => main_crt_loaded,
         main_loading_o     => main_crt_loading,
         main_id_o          => main_crt_id,
         main_exrom_o       => main_crt_exrom,
         main_game_o        => main_crt_game,
         main_size_o        => main_crt_size,
         main_c128_o        => main_crt_c128,
         main_subtype_o     => main_crt_subtype,
         main_bank_lo_i     => main_crt_bank_lo,
         main_bank_hi_i     => main_crt_bank_hi,
         main_wait_o        => main_crt_wait,
         main_addr_i        => main_crt_addr,
         main_data_i        => main_crt_data,
         main_lo_data_o     => main_crt_lo_data,
         main_hi_data_o     => main_crt_hi_data,
         main_vic_data_o    => main_crt_vic_data,
         main_rd_lo_i       => main_crt_rd_lo,
         main_rd_hi_i       => main_crt_rd_hi,
         main_vic_rd_i      => main_crt_vic_rd,
         main_vic_addr_i    => main_crt_vic_addr,
         main_rfsh_ok_i     => main_crt_rfsh_ok,
         main_reu_ok_i      => main_reu_ok,
         main_reset_i       => main_crt_reset,
         main_phi2_i        => main_crt_phi2,
         main_flash_wr_i    => main_crt_flash_wr,
         main_flash_rd_i    => main_crt_flash_rd,
         main_ioe_we_i      => main_crt_ioe_we,
         main_iof_we_i      => main_crt_iof_we,
         main_ram_we_i      => main_crt_ram_we,
         main_ioe_data_o    => main_crt_ioe_data,
         main_iof_data_o    => main_crt_iof_data,
         main_ram_data_o    => main_crt_ram_data,
         hr_clk_i           => hr_clk_i,
         hr_rst_i           => hr_rst_i,
         hr_write_o         => crt_hr_write,
         hr_read_o          => crt_hr_read,
         hr_address_o       => crt_hr_address,
         hr_writedata_o     => crt_hr_writedata,
         hr_byteenable_o    => crt_hr_byteenable,
         hr_burstcount_o    => crt_hr_burstcount,
         hr_readdata_i      => crt_hr_readdata,
         hr_readdatavalid_i => crt_hr_readdatavalid,
         hr_waitrequest_i   => crt_hr_waitrequest,
         sr_refresh_ok_o    => sr_core_refresh_ok_o,
         sr_reu_ok_o        => hr_reu_ok,
         sr_write_o         => crt_sr_write,
         sr_read_o          => crt_sr_read,
         sr_address_o       => crt_sr_address,
         sr_writedata_o     => crt_sr_writedata,
         sr_byteenable_o    => crt_sr_byteenable,
         sr_burstcount_o    => crt_sr_burstcount,
         sr_readdata_i      => crt_sr_readdata,
         sr_readdatavalid_i => crt_sr_readdatavalid,
         sr_waitrequest_i   => crt_sr_waitrequest
      ); -- i_sw_cartridge_wrapper

   -- With SDRAM, the REU shares it with the virtual cartridge, and the system CPU with both. The
   -- cartridge goes first, then the REU, then the CPU; the REU and the CPU start an access only in
   -- the window main.vhd opens, so neither makes a cartridge read late.
   reu_sdram_gen : if not C_R3_MEMORY generate
      i_sdram_arbit : entity work.sdram_arbit
         port map (
            clk_i                  => hr_clk_i,
            rst_i                  => hr_rst_i,
            slot_ok_i              => hr_reu_ok,
            s0_avm_write_i         => crt_sr_write,
            s0_avm_read_i          => crt_sr_read,
            s0_avm_address_i       => crt_sr_address,
            s0_avm_writedata_i     => crt_sr_writedata,
            s0_avm_byteenable_i    => crt_sr_byteenable,
            s0_avm_burstcount_i    => crt_sr_burstcount,
            s0_avm_readdata_o      => crt_sr_readdata,
            s0_avm_readdatavalid_o => crt_sr_readdatavalid,
            s0_avm_waitrequest_o   => crt_sr_waitrequest,
            s1_avm_write_i         => reu_hr_write,
            s1_avm_read_i          => reu_hr_read,
            s1_avm_address_i       => reu_hr_address,
            s1_avm_writedata_i     => reu_hr_writedata,
            s1_avm_byteenable_i    => reu_hr_byteenable,
            s1_avm_burstcount_i    => reu_hr_burstcount,
            s1_avm_readdata_o      => reu_hr_readdata,
            s1_avm_readdatavalid_o => reu_hr_readdatavalid,
            s1_avm_waitrequest_o   => reu_hr_waitrequest,
            s2_avm_write_i         => sr_cpu_write_i,
            s2_avm_read_i          => sr_cpu_read_i,
            s2_avm_address_i       => sr_cpu_address_i,
            s2_avm_writedata_i     => sr_cpu_writedata_i,
            s2_avm_byteenable_i    => sr_cpu_byteenable_i,
            s2_avm_burstcount_i    => sr_cpu_burstcount_i,
            s2_avm_readdata_o      => sr_cpu_readdata_o,
            s2_avm_readdatavalid_o => sr_cpu_readdatavalid_o,
            s2_avm_waitrequest_o   => sr_cpu_waitrequest_o,
            s3_avm_write_i         => dma_hr_write,
            s3_avm_read_i          => dma_hr_read,
            s3_avm_address_i       => dma_hr_address,
            s3_avm_writedata_i     => dma_hr_writedata,
            s3_avm_byteenable_i    => dma_hr_byteenable,
            s3_avm_burstcount_i    => dma_hr_burstcount,
            s3_avm_readdata_o      => dma_hr_readdata,
            s3_avm_readdatavalid_o => dma_hr_readdatavalid,
            s3_avm_waitrequest_o   => dma_hr_waitrequest,
            m_avm_write_o          => sr_core_write_o,
            m_avm_read_o           => sr_core_read_o,
            m_avm_address_o        => sr_core_address_o,
            m_avm_writedata_o      => sr_core_writedata_o,
            m_avm_byteenable_o     => sr_core_byteenable_o,
            m_avm_burstcount_o     => sr_core_burstcount_o,
            m_avm_readdata_i       => sr_core_readdata_i,
            m_avm_readdatavalid_i  => sr_core_readdatavalid_i,
            m_avm_waitrequest_i    => sr_core_waitrequest_i
         ); -- i_sdram_arbit

      reu_hm_write <= '0';
      reu_hm_read  <= '0';
      dma_hm_write <= '0';
      dma_hm_read  <= '0';
   else generate
      reu_hm_write         <= reu_hr_write;
      reu_hm_read          <= reu_hr_read;
      reu_hr_readdata      <= reu_hm_readdata;
      reu_hr_readdatavalid <= reu_hm_readdatavalid;
      reu_hr_waitrequest   <= reu_hm_waitrequest;
      dma_hm_write         <= dma_hr_write;
      dma_hm_read          <= dma_hr_read;
      dma_hr_readdata      <= dma_hm_readdata;
      dma_hr_readdatavalid <= dma_hm_readdatavalid;
      dma_hr_waitrequest   <= dma_hm_waitrequest;

      sr_core_write_o      <= crt_sr_write;
      sr_core_read_o       <= crt_sr_read;
      sr_core_address_o    <= crt_sr_address;
      sr_core_writedata_o  <= crt_sr_writedata;
      sr_core_byteenable_o <= crt_sr_byteenable;
      sr_core_burstcount_o <= crt_sr_burstcount;
      crt_sr_readdata      <= sr_core_readdata_i;
      crt_sr_readdatavalid <= sr_core_readdatavalid_i;
      crt_sr_waitrequest   <= sr_core_waitrequest_i;

      -- No SDRAM: rv_soc doesn't decode its window, so nothing comes
      sr_cpu_readdata_o      <= (others => '0');
      sr_cpu_readdatavalid_o <= '0';
      sr_cpu_waitrequest_o   <= '1';
   end generate reu_sdram_gen;

   -- s0 = REU, s1 = virtual cartridge, taking turns (R3-memory builds; else both are idle)
   i_core_hr_arbit : entity work.avm_arbit
      generic map (
         G_PREFER_SWAP  => true,
         G_FREQ_HZ      => 100_000_000,
         G_ADDRESS_SIZE => 32,
         G_DATA_SIZE    => 16
      )
      port map (
         clk_i                  => hr_clk_i,
         rst_i                  => hr_rst_i,
         s0_avm_write_i         => reu_hm_write,
         s0_avm_read_i          => reu_hm_read,
         s0_avm_address_i       => reu_hr_address,
         s0_avm_writedata_i     => reu_hr_writedata,
         s0_avm_byteenable_i    => reu_hr_byteenable,
         s0_avm_burstcount_i    => reu_hr_burstcount,
         s0_avm_readdata_o      => reu_hm_readdata,
         s0_avm_readdatavalid_o => reu_hm_readdatavalid,
         s0_avm_waitrequest_o   => reu_hm_waitrequest,
         s1_avm_write_i         => crt_hr_write,
         s1_avm_read_i          => crt_hr_read,
         s1_avm_address_i       => crt_hr_address,
         s1_avm_writedata_i     => crt_hr_writedata,
         s1_avm_byteenable_i    => crt_hr_byteenable,
         s1_avm_burstcount_i    => crt_hr_burstcount,
         s1_avm_readdata_o      => crt_hr_readdata,
         s1_avm_readdatavalid_o => crt_hr_readdatavalid,
         s1_avm_waitrequest_o   => crt_hr_waitrequest,
         m_avm_write_o          => exp_hr_write,
         m_avm_read_o           => exp_hr_read,
         m_avm_address_o        => exp_hr_address,
         m_avm_writedata_o      => exp_hr_writedata,
         m_avm_byteenable_o     => exp_hr_byteenable,
         m_avm_burstcount_o     => exp_hr_burstcount,
         m_avm_readdata_i       => exp_hr_readdata,
         m_avm_readdatavalid_i  => exp_hr_readdatavalid,
         m_avm_waitrequest_i    => exp_hr_waitrequest
      ); -- i_core_hr_arbit

   -- s0 = REU and cartridge, s1 = the REU's DMA on R3 memory (else idle)
   i_dma_hr_arbit : entity work.avm_arbit
      generic map (
         G_PREFER_SWAP  => true,
         G_FREQ_HZ      => 100_000_000,
         G_ADDRESS_SIZE => 32,
         G_DATA_SIZE    => 16
      )
      port map (
         clk_i                  => hr_clk_i,
         rst_i                  => hr_rst_i,
         s0_avm_write_i         => exp_hr_write,
         s0_avm_read_i          => exp_hr_read,
         s0_avm_address_i       => exp_hr_address,
         s0_avm_writedata_i     => exp_hr_writedata,
         s0_avm_byteenable_i    => exp_hr_byteenable,
         s0_avm_burstcount_i    => exp_hr_burstcount,
         s0_avm_readdata_o      => exp_hr_readdata,
         s0_avm_readdatavalid_o => exp_hr_readdatavalid,
         s0_avm_waitrequest_o   => exp_hr_waitrequest,
         s1_avm_write_i         => dma_hm_write,
         s1_avm_read_i          => dma_hm_read,
         s1_avm_address_i       => dma_hr_address,
         s1_avm_writedata_i     => dma_hr_writedata,
         s1_avm_byteenable_i    => dma_hr_byteenable,
         s1_avm_burstcount_i    => dma_hr_burstcount,
         s1_avm_readdata_o      => dma_hm_readdata,
         s1_avm_readdatavalid_o => dma_hm_readdatavalid,
         s1_avm_waitrequest_o   => dma_hm_waitrequest,
         m_avm_write_o          => hr_core_write_o,
         m_avm_read_o           => hr_core_read_o,
         m_avm_address_o        => hr_core_address_o,
         m_avm_writedata_o      => hr_core_writedata_o,
         m_avm_byteenable_o     => hr_core_byteenable_o,
         m_avm_burstcount_o     => hr_core_burstcount_o,
         m_avm_readdata_i       => hr_core_readdata_i,
         m_avm_readdatavalid_i  => hr_core_readdatavalid_i,
         m_avm_waitrequest_i    => hr_core_waitrequest_i
      ); -- i_dma_hr_arbit

   ---------------------------------------------------------------------------------------------
   -- Network: the SwiftLink's 6551 on the C128's bus, the Ethernet MAC and the device DNA on
   -- the device bus; lwIP and the modem run in the system firmware
   ---------------------------------------------------------------------------------------------

   i_net : entity work.net
      generic map (
         G_MAIN_CLK_HZ => CORE_CLK_SPEED
      )
      port map (
         clk_i          => modem_clk_i,
         rst_i          => modem_rst_i,
         main_clk_i     => main_clk_o,
         main_rst_i     => main_modem_rst,
         enable_i       => main_modem_en,
         cs_i           => main_modem_cs,
         we_i           => main_modem_we,
         addr_i         => main_modem_addr,
         data_i         => main_modem_wdata,
         data_o         => main_modem_rdata,
         irq_o          => main_modem_irq,
         dev_ce_i       => qnice_net_ce,
         dev_we_i       => qnice_net_we,
         dev_addr_i     => qnice_dev_addr_i(11 downto 0),
         dev_data_i     => qnice_dev_data_i,
         dev_data_o     => qnice_net_data,
         eth_clock_o    => eth_clock_o,
         eth_reset_o    => eth_reset_o,
         eth_mdc_o      => eth_mdc_o,
         eth_mdio_io    => eth_mdio_io,
         eth_rxd_i      => eth_rxd_i,
         eth_rxdv_i     => eth_rxdv_i,
         eth_rxer_i     => eth_rxer_i,
         eth_txd_o      => eth_txd_o,
         eth_txen_o     => eth_txen_o
      ); -- i_net

   ---------------------------------------------------------------------------------------------
   -- Ultimate Command Interface at $DF1C: the protocol on the core's clock, the firmware's side
   -- on the device bus (CORE/vhdl/uci)
   ---------------------------------------------------------------------------------------------

   i_uci : entity work.uci
      generic map (
         G_REU_MEM       => reu_mem_word
      )
      port map (
         main_clk_i      => main_clk_o,
         main_rst_i      => main_rst_o or main_reset_m2m_i,
         main_c128_rst_i => main_uci_rst,
         main_cs_i       => main_uci_cs,
         main_we_i       => main_uci_we,
         main_addr_i     => main_uci_addr,
         main_data_i     => main_uci_wdata,
         main_data_o     => main_uci_rdata,
         main_irq_o      => main_uci_irq,
         main_dma_o      => main_uci_dma,
         main_reboot_o   => main_uci_reboot,
         main_reu_i      => main_reu_menu,
         main_reu_keep_o => main_uci_reu_keep,
         qnice_clk_i     => qnice_clk_i,
         qnice_rst_i     => qnice_rst_i,
         qnice_addr_i    => qnice_dev_addr_i(11 downto 0),
         qnice_data_i    => qnice_dev_data_i,
         qnice_ce_i      => qnice_uci_ce,
         qnice_we_i      => qnice_uci_we,
         qnice_data_o    => qnice_uci_data,
         qnice_wait_o    => qnice_uci_wait
      ); -- i_uci

   ---------------------------------------------------------------------------------------------
   -- The system CPU's bus DMA: REST's readmem, writemem, pause and resume (CORE/vhdl/c128_dma.vhd)
   ---------------------------------------------------------------------------------------------

   i_c128_dma : entity work.c128_dma
      port map (
         main_clk_i      => main_clk_o,
         main_rst_i      => main_rst_o or main_reset_m2m_i,
         main_c128_rst_i => main_uci_rst,
         main_req_o      => main_fwdma_req,
         main_cycle_i    => main_fwdma_cycle,
         main_addr_o     => main_fwdma_addr,
         main_dout_o     => main_fwdma_dout,
         main_we_o       => main_fwdma_we,
         main_din_i      => main_fwdma_din,
         main_z80_i      => main_fwdma_z80,
         qnice_clk_i     => qnice_clk_i,
         qnice_rst_i     => qnice_rst_i,
         qnice_addr_i    => qnice_dev_addr_i(11 downto 0),
         qnice_data_i    => qnice_dev_data_i,
         qnice_ce_i      => qnice_dma_ce,
         qnice_we_i      => qnice_dma_we,
         qnice_data_o    => qnice_dma_data
      ); -- i_c128_dma

   ---------------------------------------------------------------------------------------------
   -- Drive ROM server (QNICE clock domain)
   ---------------------------------------------------------------------------------------------

   -- The handshake itself lives in drive_rom_server.vhd, which CORE/sim/tb_drive_rom.vhd
   -- exercises against the real iecdrv_rom. Four QNICE cycles per byte load a full 32 kB
   -- bank in about 2.6 ms.
   --
   -- rom_loading tracks the core reset because the M2M Shell holds the core in reset while it
   -- streams the mandatory C_CRTROMS_AUTO files in; serving from the ROM before that finishes
   -- would hand the drives garbage and latch it as valid.
   i_cdc_main2qnice_rst : xpm_cdc_array_single
      generic map (
         WIDTH => 1
      )
      port map (
         src_clk     => main_clk_o,
         src_in(0)   => main_reset_m2m_i or main_reset_core_i or main_rst_o,
         dest_clk    => qnice_clk_i,
         dest_out(0) => qnice_core_rst
      ); -- i_cdc_main2qnice_rst

   qnice_drv_rom_loading <= qnice_rst_i or qnice_core_rst;

   -- A drive's bank again: its iecdrv_rom starts over, and the server drops a byte it was
   -- fetching, which would land at the restarted address
   p_drv_reload_f : process (qnice_clk_i)
   begin
      if falling_edge(qnice_clk_i) then
         qnice_drv_reload_f <= "00";
         if qnice_dev_id_i = C_DEV_DRIVE_ROM and qnice_dev_ce_i = '1' and qnice_dev_we_i = '1' and
            qnice_dev_addr_i(27 downto 0) = x"FFFF000" then
            qnice_drv_reload_f <= qnice_dev_data_i(1 downto 0);
         end if;
      end if;
   end process p_drv_reload_f;

   p_drv_reload : process (qnice_clk_i)
   begin
      if rising_edge(qnice_clk_i) then
         qnice_drv_reload <= qnice_drv_reload_f;
      end if;
   end process p_drv_reload;

   i_drive_rom_server : entity work.drive_rom_server
      generic map (
         G_ROM_ADDR_WIDTH => 18
      )
      port map (
         clk_i          => qnice_clk_i,
         rom_loading_i  => qnice_drv_rom_loading or qnice_drv_reload(0) or qnice_drv_reload(1),
         rom_req_i      => qnice_drv_rom_req,
         rom_addr_i     => qnice_drv_rom_addr,
         rom_wr_o       => qnice_drv_rom_wr,
         rom_ram_addr_o => qnice_drvrom_b_addr
      ); -- i_drive_rom_server

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

   p_funcrom_size : process (qnice_clk_i)
   begin
      if falling_edge(qnice_clk_i) then
         if qnice_rst_i = '1' then
            qnice_funcrom_size <= "00";
         elsif qnice_dev_id_i = C_DEV_SYSTEM_ROM and qnice_dev_ce_i = '1' and qnice_dev_we_i = '1' and
               qnice_dev_addr_i(27 downto 0) = x"FFFF000" then
            qnice_funcrom_size <= qnice_dev_data_i(1 downto 0);
         end if;
      end if;
   end process p_funcrom_size;

   -- Written while the C128 is held in reset
   i_cdc_funcrom_size : entity work.cdc_stable
      generic map (
         G_DATA_SIZE => 2
      )
      port map (
         src_clk_i  => qnice_clk_i,
         src_data_i => qnice_funcrom_size,
         dst_clk_i  => main_clk_o,
         dst_data_o => main_funcrom_size
      ); -- i_cdc_funcrom_size

   -- boot1.rom holds the six 32 kB drive DOS banks that iec_drive can ask for with DRIVES = 2
   -- (1541 x2, 1571 x2, 1581 x2), so the highest address it ever generates is 5 * 32768 + 32767
   -- = 196,607. ADDR_WIDTH = 18 covers exactly that; the 19 bits this used to have would have
   -- cost another 64 block RAM tiles that the design cannot spare.
   i_drive_rom : entity work.dualport_2clk_ram
      generic map (
         ADDR_WIDTH => 18,
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
         -- Port B serves the drives. It runs on the QNICE clock as well: the pull protocol of
         -- iec_drive lives in its clk_sys domain, which is the QNICE clock here.
         clock_b         => qnice_clk_i,
         address_b       => qnice_drvrom_b_addr,
         do_latch_addr_b => '0',
         data_b          => (others => '0'),
         wren_b          => '0',
         q_b             => qnice_drvrom_b_data
      );

   -- Drive LED colour comes from main.vhd (VIC/VDC probe during debug)

end architecture synthesis;
