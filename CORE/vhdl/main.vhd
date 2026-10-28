----------------------------------------------------------------------------------
-- Commodore C128 for Mega65
--
-- Wrapper for the MiSTer core that runs exclusively in the core's clock domanin
--
-- MiSTer2MEGA65 done by sy2002 and MJoergen in 2022 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.numeric_std_unsigned.all;


library work;
use work.video_modes_pkg.all;
use work.globals.all;
use work.vdrives_pkg.all;
use work.sdhost_pkg.all;

entity main is
   generic (
      G_BOARD                 : string;                     -- Which MEGA65 revision are we running on
      G_VDNUM                 : natural;                    -- amount of virtual drives
      G_EXP_RESET_LEN         : natural := CORE_CLK_SPEED / 20;  -- reset on a cartridge change: 50 ms
      G_REU_BASE              : std_logic_vector(31 downto 0) :=   -- word address of the REU's memory
                                "0000000" & C_HMAP_REU(12 downto 0) & x"000"
   );
   port (
      clk_main_i              : in  std_logic;  -- Main core clock (~31.53 MHz PAL)
      clk_vdc_i               : in  std_logic;  -- VDC clock (32.000 MHz)
      clk_sd_i                : in  std_logic;  -- QNICE clock: "SD card" side of the virtual drives
      reset_soft_i            : in  std_logic;  -- Soft reset
      reset_hard_i            : in  std_logic;  -- Hard reset
      pause_i                 : in  std_logic;  -- Pause

      -- MiSTer core main clock speed:
      -- Make sure you pass very exact numbers here, because they are used for avoiding clock drift at derived clocks
      clk_main_speed_i        : in  natural;

      -- Help-menu selections (main clock domain; bit index = OPTM_ITEMS line)
      osm_control_i           : in  std_logic_vector(255 downto 0);

      -- Latched F9 key: '1' = pressed. Drives the power LED color and the menu title.
      f9_latched_o            : out std_logic;

      -- HDMI video stream (synchronous to VIC or VDC clock; see video_select_vdc_o)
      video_ce_o              : out std_logic;
      video_red_o             : out std_logic_vector(7 downto 0);
      video_green_o           : out std_logic_vector(7 downto 0);
      video_blue_o            : out std_logic_vector(7 downto 0);
      video_vs_o              : out std_logic;
      video_hs_o              : out std_logic;
      video_hblank_o          : out std_logic;
      video_vblank_o          : out std_logic;
      -- '1' = HDMI shows VDC (video_* on clk_vdc); '0' = VIC (video_* on clk_main)
      video_select_vdc_o      : out std_logic;

      -- VGA video stream, same rules (see vga_video_select_vdc_o)
      vga_video_ce_o          : out std_logic;
      vga_video_ce_ovl_o      : out std_logic;
      vga_video_red_o         : out std_logic_vector(7 downto 0);
      vga_video_green_o       : out std_logic_vector(7 downto 0);
      vga_video_blue_o        : out std_logic_vector(7 downto 0);
      vga_video_vs_o          : out std_logic;
      vga_video_hs_o          : out std_logic;
      vga_video_hblank_o      : out std_logic;
      vga_video_vblank_o      : out std_logic;
      vga_video_select_vdc_o  : out std_logic;

      -- Audio output (Signed PCM)
      audio_left_o            : out signed(15 downto 0);
      audio_right_o           : out signed(15 downto 0);

      -- Drive led (monochrome + RGB colour)
      drive_led_o             : out std_logic;
      drive_led_col_o         : out std_logic_vector(23 downto 0);
      -- Active CPU indicator: '0' = Z80 (C128 boot), '1' = 8502. Used by the boot sim.
      boot_z80_n_o            : out std_logic;

     -- C64 RAM: No address latching necessary and the chip can always be enabled
     ram_addr_o               : out unsigned(17 downto 0);    -- address bus (18 Bit!)
     ram_data_o               : out unsigned(7 downto 0);     -- RAM data out
     ram_we_o                 : out std_logic;                -- RAM write enable
     ram_data_i               : in unsigned(7 downto 0);      -- RAM data in
      sys_rom_addr_o           : out std_logic_vector(16 downto 0);
      sys_rom_data_i           : in  std_logic_vector(7 downto 0);

      -- Expansion Port (aka Cartridge Port). The MEGA65's slot is electrically a C64
      -- expansion port, so it takes both C64 and C128 cartridges.
      -- Every line comes as an *_i / *_o / *_oe_o triple: *_oe_o = '1' switches the board's
      -- level shifter to FPGA->Port, '0' to Port->FPGA. Which of them actually exist depends
      -- on the board revision; the top level ties off what it cannot do (see top_mega65-r*.vhd).
      cart_en_o              : out std_logic;   -- Enable the slot, active high
      cart_phi2_o            : out std_logic;
      cart_dotclock_o        : out std_logic;
      cart_dma_i             : in  std_logic;
      cart_reset_oe_o        : out std_logic;
      cart_reset_i           : in  std_logic;
      cart_reset_o           : out std_logic;
      cart_game_oe_o         : out std_logic;
      cart_game_i            : in  std_logic;
      cart_game_o            : out std_logic;
      cart_exrom_oe_o        : out std_logic;
      cart_exrom_i           : in  std_logic;
      cart_exrom_o           : out std_logic;
      cart_nmi_oe_o          : out std_logic;
      cart_nmi_i             : in  std_logic;
      cart_nmi_o             : out std_logic;
      cart_irq_oe_o          : out std_logic;
      cart_irq_i             : in  std_logic;
      cart_irq_o             : out std_logic;
      cart_roml_oe_o         : out std_logic;
      cart_roml_i            : in  std_logic;
      cart_roml_o            : out std_logic;
      cart_romh_oe_o         : out std_logic;
      cart_romh_i            : in  std_logic;
      cart_romh_o            : out std_logic;
      cart_ctrl_oe_o         : out std_logic;
      cart_ba_i              : in  std_logic;
      cart_rw_i              : in  std_logic;
      cart_io1_i             : in  std_logic;
      cart_io2_i             : in  std_logic;
      cart_ba_o              : out std_logic;
      cart_rw_o              : out std_logic;
      cart_io1_o             : out std_logic;
      cart_io2_o             : out std_logic;
      cart_addr_oe_o         : out std_logic;
      cart_a_i               : in  unsigned(15 downto 0);
      cart_a_o               : out unsigned(15 downto 0);
      cart_data_oe_o         : out std_logic;
      cart_d_i               : in  unsigned( 7 downto 0);
      cart_d_o               : out unsigned( 7 downto 0);

      -- IEC serial bus interface to MEGA65 pins (active low at top level).
      -- CLK/DATA/SRQ are open-collector: *_en_o = '1' pulls the line low, '0' releases it.
      -- ATN is push-pull (driven by the computer only). RESET resets attached real drives.
      iec_hardware_port_en_i : in  std_logic;
      iec_reset_n_o          : out std_logic;
      iec_atn_n_o            : out std_logic;
      iec_clk_en_o           : out std_logic;
      iec_clk_n_i            : in  std_logic;
      iec_clk_n_o            : out std_logic;
      iec_data_en_o          : out std_logic;
      iec_data_n_i           : in  std_logic;
      iec_data_n_o           : out std_logic;
      iec_srq_en_o           : out std_logic;
      iec_srq_n_i            : in  std_logic;
      iec_srq_n_o            : out std_logic;

      -- M2M Keyboard interface
      kb_key_num_i            : in  integer range 0 to 79;    -- cycles through all MEGA65 keys
      kb_key_pressed_n_i      : in  std_logic;                -- low active: debounced feedback: is kb_key_num_i pressed right now?
      kb_key_enabled_i        : in  std_logic;                -- '0' while the on-screen menu owns the keyboard (all keys read released)

      -- MEGA65 joysticks and paddles/mouse/potentiometers
      joy_1_up_n_i            : in  std_logic;
      joy_1_down_n_i          : in  std_logic;
      joy_1_left_n_i          : in  std_logic;
      joy_1_right_n_i         : in  std_logic;
      joy_1_fire_n_i          : in  std_logic;

      joy_2_up_n_i            : in  std_logic;
      joy_2_down_n_i          : in  std_logic;
      joy_2_left_n_i          : in  std_logic;
      joy_2_right_n_i         : in  std_logic;
      joy_2_fire_n_i          : in  std_logic;

      pot1_x_i                : in  std_logic_vector(7 downto 0);
      pot1_y_i                : in  std_logic_vector(7 downto 0);
      pot2_x_i                : in  std_logic_vector(7 downto 0);
      pot2_y_i                : in  std_logic_vector(7 downto 0);

      -- Virtual drives: QNICE device interface of vdrives.vhd (clk_sd_i domain)
      qnice_vd_addr_i         : in  std_logic_vector(27 downto 0);
      qnice_vd_data_i         : in  std_logic_vector(15 downto 0);
      qnice_vd_data_o         : out std_logic_vector(15 downto 0);
      qnice_vd_ce_i           : in  std_logic;
      qnice_vd_we_i           : in  std_logic;

      -- Drive ROM pull interface (clk_sd_i domain). The drives fetch their DOS ROM
      -- byte by byte out of the boot1.rom block RAM that mega65.vhd serves.
      drv_rom_loading_i       : in  std_logic;
      drv_rom_req_o           : out std_logic;
      drv_rom_addr_o          : out std_logic_vector(18 downto 0);
      drv_rom_data_i          : in  std_logic_vector(7 downto 0);
      drv_rom_wr_i            : in  std_logic;

      -- Disk images straight from the SD card (vd_sd.vhd, clk_sd_i domain): the mount
      -- devices of the drives, and the SD controller's engine port. Defaults keep
      -- tb_c128_boot (G_VDNUM => 0) from having to list them.
      rst_sd_i                : in  std_logic := '0';                   -- QNICE reset
      qnice_vdsd_drive_i      : in  natural range 0 to 1 := 0;
      qnice_vdsd_addr_i       : in  std_logic_vector(27 downto 0) := (others => '0');
      qnice_vdsd_data_i       : in  std_logic_vector(15 downto 0) := (others => '0');
      qnice_vdsd_data_o       : out std_logic_vector(15 downto 0);
      qnice_vdsd_ce_i         : in  std_logic := '0';
      qnice_vdsd_we_i         : in  std_logic := '0';
      qnice_sd_want_o         : out std_logic;
      qnice_sd_eng_o          : out t_sd_c2q;
      qnice_sd_eng_i          : in  t_sd_q2c := (others => '0');

      -- The MEGA65's internal 3.5" drive as a 1581 (physical_1581_controller, clk_sd_i
      -- domain). Only drive A of the floppy connector is used. All active low.
      f_rdata_i               : in  std_logic := '1';
      f_index_i               : in  std_logic := '1';
      f_track0_i              : in  std_logic := '1';
      f_writeprotect_i        : in  std_logic := '1';
      f_diskchanged_i         : in  std_logic := '1';
      f_motora_o              : out std_logic;
      f_selecta_o             : out std_logic;
      f_side1_o               : out std_logic;
      f_stepdir_o             : out std_logic;
      f_step_o                : out std_logic;
      f_density_o             : out std_logic;
      f_wgate_o               : out std_logic;
      f_wdata_o               : out std_logic;

      -- Its QNICE diagnostic device (physical_1581_diag, C_DEV_PHYS1581, clk_sd_i domain)
      qnice_p1581_ce_i        : in  std_logic := '0';
      qnice_p1581_we_i        : in  std_logic := '0';
      qnice_p1581_addr_i      : in  std_logic_vector(7 downto 0) := (others => '0');
      qnice_p1581_data_i      : in  std_logic_vector(15 downto 0) := (others => '0');
      qnice_p1581_data_o      : out std_logic_vector(15 downto 0);

      -- REU's memory port, HyperRAM or SDRAM (clk_main_i domain; mega65.vhd crosses it to hr_clk)
      reu_avm_write_o         : out std_logic;
      reu_avm_read_o          : out std_logic;
      reu_avm_address_o       : out std_logic_vector(31 downto 0);
      reu_avm_writedata_o     : out std_logic_vector(15 downto 0);
      reu_avm_byteenable_o    : out std_logic_vector(1 downto 0);
      reu_avm_burstcount_o    : out std_logic_vector(7 downto 0);
      reu_avm_readdata_i      : in  std_logic_vector(15 downto 0) := (others => '0');
      reu_avm_readdatavalid_i : in  std_logic := '0';
      reu_avm_waitrequest_i   : in  std_logic := '0';

      -- SwiftLink modem: the bus of its 6551 (clk_main_i domain); the modem is in mega65.vhd
      modem_en_o              : out std_logic;                    -- switched on in the menu
      modem_rst_o             : out std_logic;                    -- C128 reset
      modem_cs_o              : out std_logic;                    -- select strobe, CPU4-7
      modem_we_o              : out std_logic;
      modem_addr_o            : out std_logic_vector(2 downto 0);
      modem_data_o            : out std_logic_vector(7 downto 0);
      modem_data_i            : in  std_logic_vector(7 downto 0) := x"FF";
      modem_irq_i             : in  std_logic := '0';             -- to NMI, as on a SwiftLink

      -- Ultimate Command Interface at $DF1C (clk_main_i domain); uci.vhd is in mega65.vhd
      uci_rst_o               : out std_logic;                    -- C128 reset
      uci_cs_o                : out std_logic;                    -- select strobe, CPU4-7
      uci_we_o                : out std_logic;                    -- every write, $FF00 included
      uci_addr_o              : out std_logic_vector(15 downto 0);
      uci_data_o              : out std_logic_vector(7 downto 0);
      uci_data_i              : in  std_logic_vector(7 downto 0) := x"FF";
      uci_irq_i               : in  std_logic := '0';
      uci_dma_i               : in  std_logic := '0';             -- hold the CPU, as /DMA does

      -- Virtual cartridge: the .crt file and its bank cache (sw_cartridge_wrapper in mega65.vhd)
      crt_loaded_i            : in  std_logic := '0';
      crt_loading_i           : in  std_logic := '0';
      crt_id_i                : in  std_logic_vector(15 downto 0) := (others => '0');
      crt_exrom_i             : in  std_logic_vector( 7 downto 0) := (others => '1');
      crt_game_i              : in  std_logic_vector( 7 downto 0) := (others => '1');
      crt_size_i              : in  std_logic_vector(22 downto 0) := (others => '0');
      crt_c128_i              : in  std_logic := '0';
      crt_subtype_i           : in  std_logic_vector( 7 downto 0) := (others => '0');
      crt_bank_lo_o           : out std_logic_vector( 6 downto 0);
      crt_bank_hi_o           : out std_logic_vector( 6 downto 0);
      crt_wait_i              : in  std_logic := '0';
      crt_addr_o              : out std_logic_vector(15 downto 0);
      crt_data_o              : out std_logic_vector( 7 downto 0);
      crt_lo_data_i           : in  std_logic_vector( 7 downto 0) := (others => '1');
      crt_hi_data_i           : in  std_logic_vector( 7 downto 0) := (others => '1');
      crt_vic_data_i          : in  std_logic_vector( 7 downto 0) := (others => '1');  -- ROMH for the VIC
      crt_rd_lo_o             : out std_logic;   -- SDRAM: the CPU's address is in ROML, IO1 or IO2
      crt_rd_hi_o             : out std_logic;   -- SDRAM: the CPU's address is in ROMH
      crt_vic_rd_o            : out std_logic;   -- SDRAM: the VIC fetches ROMH (Ultimax)
      crt_vic_addr_o          : out std_logic_vector(12 downto 0);
      crt_rfsh_ok_o           : out std_logic;   -- SDRAM: no cartridge read is due for a while
      crt_reu_ok_o            : out std_logic;   -- SDRAM: the REU may start an access
      crt_reset_o             : out std_logic;   -- the C128 is in reset
      crt_phi2_o              : out std_logic;
      crt_flash_wr_o          : out std_logic;   -- a CPU write selects ROML or ROMH (Ultimax), one clock
      crt_flash_rd_o          : out std_logic;   -- a CPU read selects ROML or ROMH, one clock
      crt_ioe_we_o            : out std_logic;
      crt_iof_we_o            : out std_logic;
      crt_ram_we_o            : out std_logic;
      crt_ioe_data_i          : in  std_logic_vector( 7 downto 0) := (others => '1');
      crt_iof_data_i          : in  std_logic_vector( 7 downto 0) := (others => '1');
      crt_ram_data_i          : in  std_logic_vector( 7 downto 0) := (others => '1')
   );
end entity main;

architecture synthesis of main is

-- signals for RAM
signal ram_ce   : std_logic;
signal ram_we   : std_logic;
signal system_we : std_logic;   -- every CPU/DMA write, I/O and $FF00 included; ram_we is RAM only
signal ram_data : unsigned(7 downto 0);
signal core_ram_data_out : unsigned(7 downto 0);   -- what the CPU writes (the core's ramDout)
signal core_ram_addr     : unsigned(17 downto 0);
signal sysrom_cs         : std_logic;
signal sysrom_bank       : unsigned(4 downto 0);
signal sysrom_data       : unsigned(7 downto 0);
-- MiSTer SDRAM latches addr at ce rise; hold BRAM addr during burst, always drive read data to CPU.
signal sysrom_cs_d       : std_logic := '0';
signal sysrom_data_r     : unsigned(7 downto 0) := (others => '0');
signal rom_addr_held     : std_logic_vector(16 downto 0) := (others => '0');
signal ram_ce_d          : std_logic := '0';
signal ram_addr_held     : unsigned(17 downto 0) := (others => '0');
signal ram_data_r        : unsigned(7 downto 0) := (others => '0');
signal joy_a             : std_logic_vector(6 downto 0);
signal joy_b             : std_logic_vector(6 downto 0);
signal sid_audio_l       : std_logic_vector(17 downto 0);
signal sid_audio_r       : std_logic_vector(17 downto 0);
signal vdc_hs            : std_logic;
signal vdc_vs            : std_logic;
signal vdc_r             : unsigned(7 downto 0);
signal vdc_g             : unsigned(7 downto 0);
signal vdc_b             : unsigned(7 downto 0);
signal vic_r             : unsigned(7 downto 0);
signal vic_g             : unsigned(7 downto 0);
signal vic_b             : unsigned(7 downto 0);
signal vic_pixel_ce      : std_logic;
signal vic_ovl_half      : std_logic := '0';  -- every other clk_main, on-screen menu at 15 kHz

signal vic_r_reg        : unsigned(7 downto 0) := (others => '0');
signal vic_g_reg         : unsigned(7 downto 0) := (others => '0');
signal vic_b_reg         : unsigned(7 downto 0) := (others => '0');
signal core_vic_hs       : std_logic;
signal core_vic_vs       : std_logic;

signal vic_hs_out        : std_logic;
signal vic_vs_out        : std_logic;
signal vic_hblank        : std_logic;
signal vic_vblank        : std_logic;

signal vdc_hs_out        : std_logic;
signal vdc_vs_out        : std_logic;
signal vdc_hblank        : std_logic;
signal vdc_vblank        : std_logic;
signal vdc_dot_ce        : std_logic := '0';  -- every other clk_vdc: one pulse per VDC dot
signal vdc_r_reg        : unsigned(7 downto 0) := (others => '0');
signal vdc_g_reg         : unsigned(7 downto 0) := (others => '0');
signal vdc_b_reg         : unsigned(7 downto 0) := (others => '0');
signal vdc_sync_rst_sl   : std_logic_vector(1 downto 0) := "11";  -- core reset in clk_vdc
attribute ASYNC_REG : string;
attribute ASYNC_REG of vdc_sync_rst_sl : signal is "TRUE";

signal hdmi_sel_vdc      : std_logic := '0';
signal vga_sel_vdc       : std_logic := '0';
signal vga_retro15khz    : std_logic;
signal hdmi_sel_vdc_d    : std_logic := '0';
signal vga_sel_vdc_d     : std_logic := '0';
signal hdmi_switching    : std_logic := '0';
signal vga_switching     : std_logic := '0';
signal hdmi_switch_cnt   : natural range 0 to 65535 := 0;
signal vga_switch_cnt    : natural range 0 to 65535 := 0;
signal vic_jailbars      : std_logic_vector(1 downto 0) := "00";
signal core_z80_n        : std_logic;  -- MMU CPU select: '0'=Z80, '1'=8502 (MiSTer z80_n port)
signal core_c128_n       : std_logic;
constant C_PWRUP_RESET_LEN   : natural := 4095;
signal pwrup_reset_cnt       : natural range 0 to C_PWRUP_RESET_LEN := C_PWRUP_RESET_LEN;

-- Direct CIA1 keyboard matrix (MEGA65 -> keyboard.vhd -> fpga64_sid_iec)
signal cia1_pa_in            : std_logic_vector(7 downto 0);
signal cia1_pa_out           : std_logic_vector(7 downto 0);
signal cia1_pb_in            : std_logic_vector(7 downto 0);
signal cia1_pb_out           : std_logic_vector(7 downto 0);
signal vic_ko_s              : std_logic_vector(2 downto 0);
signal capslock_engaged_n    : std_logic := '1';
-- F9 (matrix index 68) is momentary, so its function is a toggle. '0' = released.
signal f9_prev_n             : std_logic := '1';
signal f9_seen               : std_logic := '0';
signal f9_latch              : std_logic := '0';
signal role_ascii_d          : std_logic := '0';

-- 40/80 column sense and the Caps Lock / ASCII-DIN sense. Both are active-low:
-- '1' = released (40 columns, or Caps Lock released). Which physical key drives
-- which sense is chosen by the Keys menu.
signal d4080_sel_s       : std_logic := '1';
signal cpslk_sense_s     : std_logic := '1';
-- RESTORE (key 75) -> NMI: latched on the RESTORE key edge, cleared by nmi_ack.
signal restore_key_s     : std_logic := '0';
signal restore_key_d     : std_logic := '0';
signal nmi_q             : std_logic := '0';
signal core_nmi_n_s      : std_logic := '1';

-- RESET SEMANTICS
--
-- The C64 core implements core specific semantics: A standard reset of the core is a soft reset and
-- will not interfere with any "reset protections". This also means that a soft reset will start
-- soft- and hardware cartridges. A hard reset on the other hand does circumvent "reset protections"
-- and will therefore also exit games which prevent you from exitting them via reset and you can
-- also exit from simulated cartridges using a hard reset.
--
-- When pulsing reset_soft_i from the outside (mega65.vhd), then you need to ensure that this
-- pulse is at least 32 clock cycles long. The M2M reset manager is its only source; a loaded
-- .crt file resets the core through exp_change_reset_proc instead.
--
-- A reset that is coming from a hardware cartridge via cart_reset_n_q (which is low active) is treated
-- just like reset_soft_i. We can assume that the pulse will be long enough because cartridges are
-- aware of minimum reset durations. (Example: The EF3 pulses the reset for 7xphi2, which is way longer
-- then 32 cycles.)
--
-- CAUTION: NEVER DIRECTLY USE THE INPUT SIGNALS
--       reset_soft_i and
--       reset_hard_i
-- IN MAIN.VHD AS YOU WILL RISK DATA CORRUPTION!
-- Exceptions are the processes "hard_reset" and "handle_cartridge_triggered_resets",
-- which "know what they are doing".
--
-- The go-to signal for all standard reset situations within main.vhd:
--       reset_core_n
-- To prevent data corruption, there is a protected version of reset_soft_i called reset_core_n.
-- Data corruption can for example occur, when a user presses the reset button while a simulated
-- disk drive is still writing to the disk image on the SD card. Therefore reset_core_n is
-- protected by using the signal prevent_reset.
--
-- hard_reset_n IS NOT MEANT TO BE USED IN MAIN.VHD
-- with the exception of the "cpu_data_in" the reset input of "i_cartridge".
signal reset_core_n     : std_logic := '1';
signal reset_core_int_n : std_logic := '1';
signal hard_reset_n     : std_logic := '1';

constant C_HARD_RST_DELAY : natural := 100_000; -- roundabout 1/30 of a second
signal hard_rst_counter : natural   := 0;
signal hard_reset_n_d   : std_logic := '1';
signal cold_start_done  : std_logic := '0';

-- Core's simulated expansion port
signal core_roml            : std_logic;
signal core_romh            : std_logic;
signal core_ioe             : std_logic;
signal core_iof             : std_logic;
signal core_io7             : std_logic;                      -- $D7xx, CPU4-7
signal core_ioe_bus         : std_logic;
signal core_iof_bus         : std_logic;
signal core_nmi_ack         : std_logic;
signal core_umax_romh       : std_logic;
signal core_io_rom          : std_logic;
signal core_io_ext          : std_logic;
signal core_io_data         : unsigned(7 downto 0);
signal core_game_n          : std_logic;
signal core_exrom_n         : std_logic;
signal core_irq_n           : std_logic;
signal core_nmi_n           : std_logic;
signal core_phi2            : std_logic;
signal core_phi2_prev       : std_logic := '0';
signal core_ba              : std_logic;
signal core_aec             : std_logic;   -- the VIC's AEC: '1' while the CPU or DMA may use the bus
signal core_vic_addr        : unsigned(13 downto 0);   -- the VIC's address while core_aec = '0'
signal cart_umax_vic        : std_logic;   -- Ultimax: the VIC is fetching $3000-$3FFF, i.e. ROMH
signal core_dma             : std_logic;   -- bus-master request from the REU or a hardware cartridge
-- The core's DMA port, shared by the REU and a hardware cartridge
signal core_dma_cycle       : std_logic;
signal core_dma_addr        : unsigned(15 downto 0);
signal core_dma_dout        : unsigned( 7 downto 0);
signal core_dma_din         : unsigned( 7 downto 0);
signal core_dma_we          : std_logic;

-- Expansion Port: one cartridge source, plus an optional REU laid over it
type t_cart_source is (CART_HW, CART_CRT);
signal cart_source          : t_cart_source;

-- REU
signal reu_menu             : std_logic_vector(1 downto 0);   -- the menu's choice, as reu.sv's cfg
signal reu_cfg              : std_logic_vector(1 downto 0);   -- reu.sv cfg: "00" off, "01" 512 KB, "10" 2 MB, "11" 16 MB
signal reu_io               : std_logic;                      -- $DF00-$DF1F
signal reu_cs               : std_logic;                      -- reu_io's strobe, once per access
signal io_phi2              : std_logic;                      -- PHI2 of an I/O access: CPU4-6
signal modem_base           : std_logic_vector(1 downto 0);   -- "00" off, "01" $DE00, "10" $DF00, "11" $D700
signal modem_sel            : std_logic;                      -- the 6551's 8 bytes at its base
signal uci_on               : std_logic;                      -- switched on, and the cartridge leaves IO2 free
signal uci_sel              : std_logic;                      -- $DF18-$DF1F
signal reu_dma_req          : std_logic;
signal reu_irq              : std_logic;
signal reu_dout             : std_logic_vector(7 downto 0);
-- reu <-> C128 DMA
signal reu_dma_cycle        : std_logic;
signal reu_dma_addr         : std_logic_vector(15 downto 0);
signal reu_dma_dout         : std_logic_vector( 7 downto 0);
signal reu_dma_din          : unsigned(7 downto 0);
signal reu_dma_we           : std_logic;
-- reu <-> reu_mapper; map_* is reu_mapper <-> avm_cache
signal core_ext_cycle       : std_logic;
signal reu_ram_cycle        : std_logic;
signal reu_ram_addr         : std_logic_vector(24 downto 0);
signal reu_ram_addr_m       : std_logic_vector(24 downto 0);   -- masked to the REU's size
signal reu_ram_dout         : std_logic_vector( 7 downto 0);
signal reu_ram_din          : std_logic_vector( 7 downto 0);
signal reu_ram_we           : std_logic;
signal reu_ram_cs           : std_logic;
signal map_write            : std_logic;
signal map_read             : std_logic;
signal map_address          : std_logic_vector(31 downto 0);
signal map_writedata        : std_logic_vector(15 downto 0);
signal map_byteenable       : std_logic_vector( 1 downto 0);
signal map_burstcount       : std_logic_vector( 7 downto 0);
signal map_readdata         : std_logic_vector(15 downto 0);
signal map_readdatavalid    : std_logic;
signal map_waitrequest      : std_logic;

-- Reset the C128 when the REU or Cartridge setting changes (C64MEGA65 #225, #226), and when a
-- .crt file is loaded
signal exp_cfg_d            : std_logic_vector(3 downto 0) := "0010";   -- REU off, hardware slot, no .crt
signal exp_reset_pend       : std_logic := '0';
signal exp_reset_cnt        : natural range 0 to G_EXP_RESET_LEN := 0;

-- Virtual cartridge (cartridge.vhd)
signal crt_off              : std_logic := '1';   -- a hard reset turned it off, until the next reset
signal crt_active           : std_logic;          -- a .crt file is loaded, selected and on
signal crt_hold             : std_logic;          -- keep the C128 in reset until the cache has the banks
signal crt_io2              : std_logic;          -- it uses $DFxx, so the REU stays off
signal crt_game_n           : std_logic;
signal crt_exrom_n          : std_logic;
signal crt_io_rom           : std_logic;
signal crt_io_ext           : std_logic;
signal crt_io_data          : std_logic_vector(7 downto 0);
signal crt_nmi              : std_logic;
signal crt_roml_we          : std_logic;          -- ROML reads come from the cartridge's RAM
signal crt_ioe_wr_ena       : std_logic;          -- $DExx reads come from its RAM, else they mirror $9Exx
signal crt_iof_wr_ena       : std_logic;
signal crt_bank_lo          : std_logic_vector(6 downto 0);
signal crt_addr_9exx        : std_logic;
signal crt_addr_9fxx        : std_logic;
signal crt_rfsh_ok          : std_logic := '1';
signal crt_reu_ok           : std_logic := '1';
constant C_REU_FROM         : natural := 7;    -- SDRAM: the REU's window, clocks into PHI2 low
constant C_REU_TO           : natural := 15;

-- Hardware Expansion Port
signal exp_port_hw          : std_logic;   -- '1' = the physical slot is in use
signal cart_rom_sel         : std_logic;   -- the CPU's access may select ROML/ROMH (PLA rules)
signal cart_roml_n          : std_logic;
signal cart_romh_n          : std_logic;
signal cart_io1_n           : std_logic := '1';
signal cart_io2_n           : std_logic := '1';
constant C_RD_RELEASE       : natural := 2;
signal cart_rd_now          : std_logic;   -- the cartridge is answering a read
signal cart_rd_hold         : natural range 0 to C_RD_RELEASE := 0;
signal cart_nmi_n           : std_logic;
signal cart_irq_n           : std_logic;
signal cart_exrom_n         : std_logic;
signal cart_game_n          : std_logic;
signal cart_reset_n_q       : std_logic := '1';
signal cart_game_n_q        : std_logic := '1';
signal cart_exrom_n_q       : std_logic := '1';
signal cart_irq_n_q         : std_logic := '1';
signal cart_nmi_n_q         : std_logic := '1';
signal data_from_cart       : unsigned(7 downto 0);
-- What the C128 itself puts on the data bus. Kept separate from ram_data (which may carry
-- data_from_cart) so that driving the cartridge's data bus cannot create a combinational loop.
signal machine_data         : unsigned(7 downto 0);
-- Low active reset request towards the cartridge, from the core's point of view
signal cart_reset_n_int     : std_logic;

-- Hardware Expansion Port: a cartridge as bus master (/DMA), e.g. the Ultimate II+ menu.
-- Counts are clk_main_i clocks into the current PHI2 phase; PHI2 rises at VIC3 and falls at CPU7.
constant C_DMA_ADDR         : natural := 5;   -- PHI2 high: latch address and R/W, ahead of the VIC's read latch (DMA3)
constant C_DMA_DATA         : natural := 8;   -- PHI2 high: latch write data, ahead of the CIA (CPU1)
constant C_BA_HOLD          : natural := 1;   -- PHI2 high: keep driving BA while the cart samples it at the rise
constant C_BA_HOLD_W        : natural := 6;   -- same while BA is low, past the Ultimate's read gate
constant C_DATA_HOLD        : natural := 4;   -- PHI2 low: keep read data on the bus
constant C_CTRL_TURN        : natural := 3;   -- PHI2 low: let the cart release R/W before we drive BA
signal cart_dma_n_q         : std_logic := '1';
signal cart_rw_q            : std_logic := '1';
signal cart_a_q             : unsigned(15 downto 0) := (others => '1');
signal cart_d_q             : unsigned( 7 downto 0) := (others => '1');
signal cart_dma_req         : std_logic;
signal cart_dma             : std_logic := '0';   -- the cartridge, not the REU, owns the core's DMA port
signal cart_dma_addr        : unsigned(15 downto 0) := (others => '1');
signal cart_dma_dout        : unsigned( 7 downto 0) := (others => '0');
signal cart_dma_rd          : std_logic := '1';
signal cart_dma_live        : std_logic := '0';   -- the latched access came from the pins, not the idle default
signal cart_dma_din_q       : unsigned( 7 downto 0) := (others => '0');
signal cart_dma_ctrl_oe     : std_logic := '1';   -- registered, so the level shifters cannot glitch
signal cart_dma_data_oe     : std_logic := '0';
signal phi2_d               : std_logic := '0';
signal phi2_cnt             : natural range 0 to 31 := 31;
signal phi2_ph              : natural range 0 to 31;
signal ba_lo_cnt            : natural range 0 to 3 := 0;   -- PHI2 rises in a row with BA low
signal ba_warn              : std_logic;                   -- BA low, but the cart may still write
signal ba_idle              : std_logic;                   -- BA low, and the cart leaves the bus alone

-- Hardware Expansion Port: Handle specifics of certain cartridges
constant C_EF3_RESET_LEN : natural := 7; -- measured in phi2 cycles
signal cart_reset_counter : natural range 0 to C_EF3_RESET_LEN := 0;
signal cart_res_flckr_ign : natural range 0 to 2 := 0; -- avoid a short cart_reset_o after cart_reset_counter reached zero
signal cart_is_an_EF3     : std_logic;

-- Simulated IEC drives
signal prevent_reset : std_logic;
signal cache_dirty   : std_logic_vector(G_VDNUM - 1 downto 0);

-- Scalar summaries of the per-drive vectors above. They exist so that the rest of the
-- architecture works unchanged when G_VDNUM is 0 (the boot simulation builds main that
-- way to stay fast): a reduction over a null vector is not something numeric_std does
-- usefully, so the drives_gen block below decides these two instead.
signal drives_dirty  : std_logic;
signal drives_busy   : std_logic;

-- 16 MHz chip enable for iec_drive, see iec_drive_ce_proc
signal iec_drive_ce  : std_logic;
signal iec_dce_sum   : integer := 0;   -- 32-bit integer, initialized to 0

signal iec_img_mounted  : std_logic_vector(G_VDNUM - 1 downto 0);
signal iec_disk_ready   : std_logic;   -- every 1541/1571 out of reset has its disk
signal iec_img_readonly : std_logic;
signal iec_img_size     : std_logic_vector(31 downto 0);
signal iec_img_type     : std_logic_vector(1 downto 0);
signal iec_img_type4    : std_logic_vector(3 downto 0);
-- vdrives only strobes one shared img_type. Remember it per drive so a D71 on
-- unit 8 stays a 1571 after unit 9 mounts a D64.
signal iec_img_type_latched : vd_vec_array(0 to G_VDNUM - 1)(1 downto 0) := (others => "00");

signal iec_drives_reset : std_logic_vector(G_VDNUM - 1 downto 0);
signal vdrives_mounted  : std_logic_vector(G_VDNUM - 1 downto 0);
signal iec_drive_led    : std_logic_vector(G_VDNUM - 1 downto 0);

-- How long the drive's own LED has to stay dark before the write-back cache may
-- claim the LED. Comfortably longer than the gap between two error blinks, so the
-- cache warning can never fill one in and turn the blink into a steady light.
constant C_DRIVE_LED_IDLE : natural := CORE_CLK_SPEED / 2;   -- 0.5 s
-- SystemVerilog unpacked ports are declared [NDR], i.e. indices 0..NDR-1.
-- Keep the outer VHDL range ascending so Vivado binds drive index 0 to index 0.
-- Packed vectors (reset, sd_rd, etc.) intentionally retain their downto ranges.
signal iec_out_track    : vd_vec_array(0 to G_VDNUM - 1)(7 downto 0);
signal iec_out_we       : std_logic_vector(G_VDNUM - 1 downto 0);
signal iec_drv_mode     : vd_vec_array(0 to G_VDNUM - 1)(1 downto 0);

signal iec_sd_lba          : vd_vec_array(0 to G_VDNUM - 1)(31 downto 0);
signal iec_sd_blk_cnt      : vd_vec_array(0 to G_VDNUM - 1)( 5 downto 0);
signal iec_sd_rd           : vd_std_array(G_VDNUM - 1 downto 0);
signal iec_sd_wr           : vd_std_array(G_VDNUM - 1 downto 0);
signal iec_sd_ack          : vd_std_array(G_VDNUM - 1 downto 0);
signal iec_sd_buf_addr     : std_logic_vector(13 downto 0);
-- vdrives addresses 16 kB, iec_drive expects 16 bits; see the BLKSZ note at vdrives_inst
signal iec_sd_buf_addr16   : std_logic_vector(15 downto 0);
signal iec_sd_buf_data_in  : std_logic_vector( 7 downto 0);
signal iec_sd_buf_data_out : vd_vec_array(0 to G_VDNUM - 1)(7 downto 0);
signal iec_sd_buf_wr       : std_logic;

-- vdrives declares its array ports as (VDNUM-1 downto 0). Associating the ascending
-- iec_drive signals with them directly would pair the elements up positionally and
-- swap the drives again, so mirror them by index instead.
signal vd_sd_lba           : vd_vec_array(G_VDNUM - 1 downto 0)(31 downto 0);
signal vd_sd_blk_cnt       : vd_vec_array(G_VDNUM - 1 downto 0)( 5 downto 0);
signal vd_sd_buf_data_out  : vd_vec_array(G_VDNUM - 1 downto 0)( 7 downto 0);

-- Core's IEC serial bus line levels (fpga64_sid_iec convention: '1' = line released/high,
-- '0' = line asserted/low; srq is active low: '0' = asserted).
signal core_iec_clk_o   : std_logic;
signal core_iec_data_o  : std_logic;
signal core_iec_atn_o   : std_logic;
signal core_iec_srq_n_o : std_logic;

-- Same convention, driven by the emulated drives. The _raw versions are what
-- iec_drive outputs; drv_iec_*_o is the same a short while later, see
-- drv_iec_delay_proc for why the computer must not see them instantly.
signal drv_iec_clk_raw  : std_logic;
signal drv_iec_data_raw : std_logic;
signal drv_iec_srq_raw  : std_logic;
signal drv_iec_clk_o    : std_logic;
signal drv_iec_data_o   : std_logic;
signal drv_iec_srq_o    : std_logic;
-- 16 clk_main cycles = 0.51 us of propagation delay, see drv_iec_delay_proc.
-- Initialised to "released" so the bus is quiet until the first drive output.
constant C_DRV_IEC_DELAY : natural := 16;
signal drv_iec_clk_dly  : std_logic_vector(C_DRV_IEC_DELAY - 1 downto 0) := (others => '1');
signal drv_iec_data_dly : std_logic_vector(C_DRV_IEC_DELAY - 1 downto 0) := (others => '1');
signal drv_iec_srq_dly  : std_logic_vector(C_DRV_IEC_DELAY - 1 downto 0) := (others => '1');
signal hw_iec_clk_n_i   : std_logic;
signal hw_iec_data_n_i  : std_logic;
signal hw_iec_srq_n_i   : std_logic;

-- TODO: Add rtc support

begin

-- prevent data corruption by not allowing a soft reset to happen while a write-back cache
-- still has to reach the SD card
prevent_reset <= drives_dirty;

-- Active-CPU indicator for the boot simulation ('0' = Z80, '1' = 8502).
boot_z80_n_o <= core_z80_n;

-- Drive LED: follow the emulated drive's own activity LED (1581 CIA PA6, 157x "act").
-- The DOS drives that bit itself, so both the steady light during an access and the
-- error blink that PRINT DS$ clears reach the MEGA65 LED exactly as on real hardware.
-- The write-back cache warning is strictly secondary and only takes the LED once the
-- drive has been dark long enough that this cannot be the gap between two blinks.
drive_led_policy_inst : entity work.drive_led_policy
   generic map (
      G_IDLE_CYCLES => C_DRIVE_LED_IDLE
   )
   port map (
      clk_i      => clk_main_i,
      activity_i => drives_busy,
      dirty_i    => drives_dirty,
      led_o      => drive_led_o,
      colour_o   => drive_led_col_o
   );

--------------------------------------------------------------------------------------------------
-- Video Output: which chip each port shows. Follow (the default) uses the 40/80 sense
-- ('1'=40-col/VIC, '0'=80-col/VDC); Swap puts that chip on HDMI and the other one on VGA.
--------------------------------------------------------------------------------------------------
hdmi_sel_vdc <= '1' when osm_control_i(C_MENU_VIDEO_80) = '1' or osm_control_i(C_MENU_VIDEO_H80_V40) = '1' else
                '0' when osm_control_i(C_MENU_VIDEO_40) = '1' or osm_control_i(C_MENU_VIDEO_H40_V80) = '1' else
                not d4080_sel_s;

vga_sel_vdc  <= '1' when osm_control_i(C_MENU_VIDEO_80) = '1' or osm_control_i(C_MENU_VIDEO_H40_V80) = '1' else
                '0' when osm_control_i(C_MENU_VIDEO_40) = '1' or osm_control_i(C_MENU_VIDEO_H80_V40) = '1' else
                d4080_sel_s when osm_control_i(C_MENU_VIDEO_SWAP) = '1' else
                not d4080_sel_s;

video_select_vdc_o     <= hdmi_sel_vdc;
vga_video_select_vdc_o <= vga_sel_vdc;

vic_jailbars <= "11" when osm_control_i(C_MENU_JAILBARS_HIGH)   = '1' else
                "10" when osm_control_i(C_MENU_JAILBARS_MEDIUM) = '1' else
                "01" when osm_control_i(C_MENU_JAILBARS_LOW)    = '1' else
                "00";

-- VIC path: sample on enablePixel; blanking via M2M/C64 video_sync.
vic_pixel_sample_proc : process (clk_main_i)
begin
  if rising_edge(clk_main_i) then
    vic_ovl_half <= not vic_ovl_half;
    if vic_pixel_ce = '1' then
      vic_r_reg <= vic_r;
      vic_g_reg <= vic_g;
      vic_b_reg <= vic_b;
    end if;
  end if;
end process vic_pixel_sample_proc;

video_sync_vic : entity work.video_sync
  port map (
    clk32     => clk_main_i,
    pause     => '0',
    hsync     => core_vic_hs,
    vsync     => core_vic_vs,
    ntsc      => '0',
    wide      => '0',
    hsync_out => vic_hs_out,
    vsync_out => vic_vs_out,
    hblank    => vic_hblank,
    vblank    => vic_vblank
  );

-- VDC path: C128 MiSTer video_sync for sync and blanking (centered PAL/NTSC shifts).
vdc_sync_rst_proc : process (clk_vdc_i)
begin
  if rising_edge(clk_vdc_i) then
    vdc_sync_rst_sl <= vdc_sync_rst_sl(0) & (reset_soft_i or reset_hard_i);
  end if;
end process vdc_sync_rst_proc;

video_sync_vdc : entity work.video_sync_c128
  port map (
    reset       => vdc_sync_rst_sl(1),
    clk32       => clk_vdc_i,
    pause       => '0',
    hshift_r60  => std_logic_vector(to_unsigned(43, 12)),   -- centered, not wide
    hshift_l60  => std_logic_vector(to_unsigned(236, 12)),
    hshift_r50  => std_logic_vector(to_unsigned(87, 12)),
    hshift_l50  => std_logic_vector(to_unsigned(290, 12)),
    hsync       => vdc_hs,
    vsync       => vdc_vs,
    hsync_out   => vdc_hs_out,
    vsync_out   => vdc_vs_out,
    hblank      => vdc_hblank,
    vblank      => vdc_vblank,
    ilace       => open,
    field       => open,
    valid       => open,
    ce          => open
  );

-- The VDC outputs a dot every other clock. A one-clock strobe on alternate clocks samples each
-- dot once; video_sync_c128's 2-on/2-off ce made the VGA path skip every other dot.
vdc_pixel_sample_proc : process (clk_vdc_i)
begin
  if rising_edge(clk_vdc_i) then
    vdc_dot_ce <= not vdc_dot_ce;
    vdc_r_reg  <= vdc_r;
    vdc_g_reg  <= vdc_g;
    vdc_b_reg  <= vdc_b;
  end if;
end process vdc_pixel_sample_proc;

-- Blank a port briefly when its source changes (its video clock BUFGMUX switches with it).
video_switch_blank_proc : process (clk_main_i)
begin
  if rising_edge(clk_main_i) then
    hdmi_sel_vdc_d <= hdmi_sel_vdc;
    if hdmi_sel_vdc /= hdmi_sel_vdc_d then
      hdmi_switching  <= '1';
      hdmi_switch_cnt <= 65535;
    elsif hdmi_switch_cnt /= 0 then
      hdmi_switch_cnt <= hdmi_switch_cnt - 1;
    else
      hdmi_switching <= '0';
    end if;

    vga_sel_vdc_d <= vga_sel_vdc;
    if vga_sel_vdc /= vga_sel_vdc_d then
      vga_switching  <= '1';
      vga_switch_cnt <= 65535;
    elsif vga_switch_cnt /= 0 then
      vga_switch_cnt <= vga_switch_cnt - 1;
    else
      vga_switching <= '0';
    end if;
  end if;
end process video_switch_blank_proc;

-- Native-domain muxes: VIC on main_clk, VDC on vdc_clk. mega65 BUFGMUXes each port's clock to match.
video_ce_o     <= vdc_dot_ce when hdmi_sel_vdc = '1' else vic_pixel_ce;
video_hs_o     <= vdc_hs_out when hdmi_sel_vdc = '1' else vic_hs_out;
video_vs_o     <= vdc_vs_out when hdmi_sel_vdc = '1' else vic_vs_out;
video_hblank_o <= vdc_hblank when hdmi_sel_vdc = '1' else vic_hblank;
video_vblank_o <= vdc_vblank when hdmi_sel_vdc = '1' else vic_vblank;
video_red_o    <= (others => '0') when hdmi_switching = '1' else
                  std_logic_vector(vdc_r_reg) when hdmi_sel_vdc = '1' else std_logic_vector(vic_r_reg);
video_green_o  <= (others => '0') when hdmi_switching = '1' else
                  std_logic_vector(vdc_g_reg) when hdmi_sel_vdc = '1' else std_logic_vector(vic_g_reg);
video_blue_o   <= (others => '0') when hdmi_switching = '1' else
                  std_logic_vector(vdc_b_reg) when hdmi_sel_vdc = '1' else std_logic_vector(vic_b_reg);

vga_video_ce_o <= vdc_dot_ce when vga_sel_vdc = '1' else vic_pixel_ce;
-- The on-screen menu counts this enable while the data-enable window is open
-- and lays the menu out on the 720x540 canvas in globals.vhd. It also
-- samples the VGA picture and syncs on it. Same rule as the C64 core:
-- * VGA Standard: the scan-doubled window is about 768 video clocks for the
--   40-column picture and the same order for 80 columns, so count every clock.
-- * Retro 15 kHz: the native window is twice as long, so count every other
--   clock. Without the scan doubler the mixer updates the picture at most every
--   other clock, so this still samples every pixel, and the even spacing keeps
--   the menu's font columns the same width.
vga_retro15khz     <= osm_control_i(C_MENU_VGA_15KHZHSVS) or osm_control_i(C_MENU_VGA_15KHZCS);
vga_video_ce_ovl_o <= '1'          when vga_retro15khz = '0' else
                      vdc_dot_ce   when vga_sel_vdc = '1'    else
                      vic_ovl_half;
vga_video_hs_o     <= vdc_hs_out when vga_sel_vdc = '1' else vic_hs_out;
vga_video_vs_o     <= vdc_vs_out when vga_sel_vdc = '1' else vic_vs_out;
vga_video_hblank_o <= vdc_hblank when vga_sel_vdc = '1' else vic_hblank;
vga_video_vblank_o <= vdc_vblank when vga_sel_vdc = '1' else vic_vblank;
vga_video_red_o    <= (others => '0') when vga_switching = '1' else
                      std_logic_vector(vdc_r_reg) when vga_sel_vdc = '1' else std_logic_vector(vic_r_reg);
vga_video_green_o  <= (others => '0') when vga_switching = '1' else
                      std_logic_vector(vdc_g_reg) when vga_sel_vdc = '1' else std_logic_vector(vic_g_reg);
vga_video_blue_o   <= (others => '0') when vga_switching = '1' else
                      std_logic_vector(vdc_b_reg) when vga_sel_vdc = '1' else std_logic_vector(vic_b_reg);
--------------------------------------------------------------------------------------------------
-- Expansion Port (aka Cartridge Port): real cartridges in the MEGA65's slot
--------------------------------------------------------------------------------------------------
-- The MEGA65's slot is electrically a C64 expansion port, so it accepts both C64 cartridges
-- (which pull EXROM and/or GAME low and thereby make the C128 come up in C64 mode) and C128
-- cartridges (which leave both released and are found by the C128 boot ROM as External Function
-- ROM at $8000, i.e. through the same ROML line).
--
-- Address, data and /ROML-/ROMH are not registered: the C128 bus and the pins are in the same
-- clock domain (clk_main_i) and a cartridge expects them to settle within the PHI2 cycle they
-- belong to. /IO1-/IO2 are (cart_io_proc), and so are the inputs (cart_input_proc), except read
-- data, which the CPU latches late.
cart_source <= CART_CRT when osm_control_i(C_MENU_CART_CRT) = '1' else CART_HW;
exp_port_hw <= '1' when cart_source = CART_HW else '0';

reu_menu <= "01" when osm_control_i(C_MENU_REU_512K) = '1' else
            "10" when osm_control_i(C_MENU_REU_2M)   = '1' else
            "11" when osm_control_i(C_MENU_REU_16M)  = '1' else
            "00";
reu_cfg  <= "00" when crt_io2 = '1' or modem_base = "10" else reu_menu;
reu_io  <= '1' when core_iof = '1' and core_ram_addr(7 downto 5) = "000" else '0';

-- In FAST mode the CPU has PHI1 too, and IOE/IOF/IO7 come up there as well as in CPU4-7. The REU
-- and the 6551 act on their select's rising edge, so they take it in PHI2 alone, once per access.
-- Read data still follows the select, to the end of CPU7.
io_phi2 <= core_phi2 and phi2_d;
reu_cs  <= reu_io and io_phi2;

-- SwiftLink modem: the 6551 and the Turbo232 register take 8 bytes at the chosen base. At $DF00
-- it shares IO2 with the REU's registers, so the REU is off then.
modem_base <= "01" when osm_control_i(C_MENU_MODEM_DE00) = '1' else
              "10" when osm_control_i(C_MENU_MODEM_DF00) = '1' else
              "11" when osm_control_i(C_MENU_MODEM_D700) = '1' else
              "00";
modem_sel  <= '1' when core_ram_addr(7 downto 3) = "00000" and
                       ((modem_base = "01" and core_ioe = '1') or
                        (modem_base = "10" and core_iof = '1') or
                        (modem_base = "11" and core_io7 = '1')) else '0';

modem_en_o   <= '0' when modem_base = "00" else '1';
modem_rst_o  <= not reset_core_n;
modem_cs_o   <= modem_sel and io_phi2;
modem_we_o   <= system_we;
modem_addr_o <= std_logic_vector(core_ram_addr(2 downto 0));
modem_data_o <= std_logic_vector(core_ram_data_out);

-- Ultimate Command Interface: $DF18-$DF1F, laid over the REU's unused $DF18-$DF1F (they read
-- $FF there) as on the Ultimate. Off while the virtual cartridge uses IO2, as Gideon's firmware
-- does for a cartridge that rules it out.
uci_on  <= '1' when osm_control_i(C_MENU_UCI) = '1' and crt_io2 = '0' else '0';
uci_sel <= '1' when uci_on = '1' and core_iof = '1' and core_ram_addr(7 downto 3) = "00011" else '0';

uci_rst_o  <= not reset_core_n;
uci_cs_o   <= uci_sel and io_phi2;
uci_we_o   <= system_we;
uci_addr_o <= std_logic_vector(core_ram_addr(15 downto 0));
uci_data_o <= std_logic_vector(core_ram_data_out);

-- Low-active chip selects as the expansion port expects them. The core reports these accesses
-- active high. ROMH covers both the CPU's access to $A000-$BFFF / $E000-$FFFF and the VIC-II's
-- Ultimax character fetch, which a cartridge cannot tell apart.
-- The core decodes the CPU's next address in PHI1 too; a PLA selects ROML/ROMH only with AEC.
-- The VIC's Ultimax ROMH and address last its whole phase, as on a C64, not just the core's
-- VIC0-3 slot: the Ultimate samples the address ~100 ns into PHI1.
cart_umax_vic <= '1' when core_aec = '0' and core_exrom_n = '1' and core_game_n = '0' and
                          core_vic_addr(13 downto 12) = "11" else '0';
-- A PLA selects ROML/ROMH for a write only in Ultimax mode. In 8K/16K mode the write goes to RAM
-- alone, and a plain ROM cartridge, which has no R/W, would drive the bus against it.
cart_rom_sel <= core_aec and ((not system_we) or (core_exrom_n and not core_game_n));
cart_roml_n  <= not (core_roml and cart_rom_sel);
cart_romh_n  <= not (core_romh and cart_rom_sel) and (not cart_umax_vic);

-- /IO1 and /IO2 span the CPU's PHI2 phase as on a C64, not the core's CPU4-7 strobe: the Ultimate
-- latches them ~350 ns into PHI2. Registered against the CPU/VIC address handover; low EXT5..CPU7,
-- one clock past the PHI2 fall, where the 8502 takes read data and a cart latches a write.
-- A cart read leaves the data bus to the cart for C_RD_RELEASE clocks more, while it lets go.
cart_io_proc : process (clk_main_i)
begin
   if rising_edge(clk_main_i) then
      cart_io1_n <= '1';
      cart_io2_n <= '1';
      if core_phi2 = '1' and phi2_d = '1' then
         cart_io1_n <= not core_ioe_bus;
         cart_io2_n <= not core_iof_bus;
      end if;

      if cart_rd_now = '1' then
         cart_rd_hold <= C_RD_RELEASE;
      elsif cart_rd_hold /= 0 then
         cart_rd_hold <= cart_rd_hold - 1;
      end if;
   end if;
end process cart_io_proc;

-- While the VIC has the bus, a select is always a read; system_we may still be the CPU's.
cart_rd_now <= '1' when (system_we = '0' or core_aec = '0') and
                        (cart_roml_n = '0' or cart_romh_n = '0' or cart_io1_n = '0' or cart_io2_n = '0') else '0';

-- What the cartridge tells the C128. Forced to the inactive level while the slot is unused so
-- that an empty slot can never inject an interrupt or map a phantom cartridge into the address
-- space.
cart_game_n  <= cart_game_n_q  when exp_port_hw = '1' else '1';
cart_exrom_n <= cart_exrom_n_q when exp_port_hw = '1' else '1';
cart_irq_n   <= cart_irq_n_q   when exp_port_hw = '1' else '1';
cart_nmi_n   <= cart_nmi_n_q   when exp_port_hw = '1' else '1';

-- The pins are asynchronous to clk_main_i: unregistered, they corrupt BRAM (C64MEGA65 c556cdc),
-- and RESET would let parts of the core leave reset a clock apart.
cart_input_proc : process (clk_main_i)
begin
   if rising_edge(clk_main_i) then
      cart_reset_n_q <= cart_reset_i;
      cart_game_n_q  <= cart_game_i;
      cart_exrom_n_q <= cart_exrom_i;
      cart_irq_n_q   <= cart_irq_i;
      cart_nmi_n_q   <= cart_nmi_i;
      cart_dma_n_q   <= cart_dma_i;
      cart_rw_q      <= cart_rw_i;
      cart_a_q       <= cart_a_i;
      cart_d_q       <= cart_d_i;
   end if;
end process cart_input_proc;

-- Low-active reset request towards the cartridge. This tracks the INTERNAL reset request only,
-- never reset_core_n: a cartridge that pulls RESET wants to reset the C128, not itself, and
-- routing reset_core_n back out closes a purely combinational self-latching loop through the pin
-- (reset_core_n -> cart_reset_o -> pin -> cart_reset_i -> reset_core_n) whose settling at
-- reset release depends on routing delay, i.e. a placement-dependent, STA-invisible boot lottery.
cart_reset_n_int <= '0' when (pwrup_reset_cnt /= 0 or reset_core_int_n = '0') else '1';

-- A cartridge takes the bus with /DMA (C64MEGA65 #199), but its address, R/W and data are only
-- certain while PHI2 is high. So its access is latched once per cycle and held: ramWE is not
-- cycle-gated and FAST mode adds a CPUC slot. The 8502 must own the bus, as the core's Z80 does
-- not stop for DMA (a real C128's does, PRG p. 636).
phi2_ph        <= 0 when core_phi2 /= phi2_d else phi2_cnt;
ba_warn        <= '1' when core_ba = '0' and ba_lo_cnt /= 3 else '0';
ba_idle        <= '1' when core_ba = '0' and ba_lo_cnt = 3 else '0';
cart_dma_req   <= exp_port_hw and not cart_dma_n_q and core_z80_n;

cart_dma_proc : process (clk_main_i)
begin
   if rising_edge(clk_main_i) then
      cart_dma_din_q <= core_dma_din;

      phi2_d <= core_phi2;
      if core_phi2 /= phi2_d then
         phi2_cnt <= 1;
      elsif phi2_cnt /= 31 then
         phi2_cnt <= phi2_cnt + 1;
      end if;

      -- BA as the cart samples it at each PHI2 rise. A VIC-II, like the core's, takes the bus in the
      -- fourth cycle with BA low, but the Ultimate already leaves the third alone and floats R/W
      -- there ("Patch: 2 cycles only" in its slot_timing.vhd), so the third is ba_idle too.
      if core_phi2 = '1' and phi2_d = '0' then
         if core_ba = '1' then
            ba_lo_cnt <= 0;
         elsif ba_lo_cnt /= 3 then
            ba_lo_cnt <= ba_lo_cnt + 1;
         end if;
      end if;

      -- While BA is low we hold the R/W line ourselves, so the access is latched later then.
      -- Any cycle without a cart access reads $FFFF, as the Ultimate does when idle.
      if core_phi2 = '1' and phi2_ph = C_DMA_ADDR then
         cart_dma_addr <= x"FFFF";
         cart_dma_rd   <= '1';
         cart_dma_live <= '0';
         if cart_dma_req = '1' and core_ba = '1' then
            cart_dma_addr <= cart_a_q;
            cart_dma_rd   <= cart_rw_q;
            cart_dma_live <= '1';
         end if;
      end if;
      if core_phi2 = '1' and phi2_ph = C_DMA_DATA then
         cart_dma_dout <= cart_d_q;
         if cart_dma_req = '1' and ba_warn = '1' then
            cart_dma_addr <= cart_a_q;
            cart_dma_rd   <= cart_rw_q;
            cart_dma_live <= '1';
         end if;
      end if;

      -- Hand the DMA port back to the REU at the next PHI2 rise, after the core has dropped DMA
      if cart_dma_req = '1' then
         cart_dma <= '1';
      elsif core_phi2 = '1' and phi2_d = '0' then
         cart_dma <= '0';
      end if;

      -- Level shifter directions while the cart has the bus, for the next clock. BA shares one
      -- with R/W, so we drive BA (and R/W high) only in PHI1, at the PHI2 rise when the cart
      -- samples it, past the Ultimate's read gate while BA is low, and in ba_idle cycles.
      -- While BA is low, that overlaps an Ultimate write's R/W by ~130 ns.
      cart_dma_ctrl_oe <= '0';
      if ba_idle = '1' or
         (core_phi2 = '0' and phi2_ph >= C_CTRL_TURN - 1) or
         (core_phi2 = '1' and phi2_ph < C_BA_HOLD - 1) or
         (core_phi2 = '1' and ba_warn = '1' and phi2_ph < C_BA_HOLD_W - 1) then
         cart_dma_ctrl_oe <= '1';
      end if;

      -- Read data from the latch until just past the PHI2 fall. ROML/ROMH reads are a cart's: by
      -- the core's decode, which outlasts the AEC-gated selects while the cart lets go.
      cart_dma_data_oe <= '0';
      if cart_dma = '1' and cart_dma_live = '1' and cart_dma_rd = '1' and
         core_roml = '0' and core_romh = '0' and core_umax_romh = '0' and
         ((core_phi2 = '1' and phi2_ph > C_DMA_ADDR) or (core_phi2 = '0' and phi2_ph < C_DATA_HOLD - 1)) then
         cart_dma_data_oe <= '1';
      end if;
   end if;
end process cart_dma_proc;

-- The cartridge owns the core's DMA port while it holds /DMA, and the REU waits. When only the
-- virtual cartridge's cache stalls the CPU, the DMA cycles read RAM at $0000.
core_dma_addr <= cart_dma_addr   when cart_dma = '1' else
                 unsigned(reu_dma_addr) when reu_dma_req = '1' else
                 x"0000";
core_dma_dout <= cart_dma_dout   when cart_dma = '1' else unsigned(reu_dma_dout);
core_dma_we   <= not cart_dma_rd when cart_dma = '1' else reu_dma_we;
reu_dma_cycle <= core_dma_cycle and not cart_dma;
reu_dma_din   <= core_dma_din;

handle_hardware_expansion_proc : process (all)
begin
   -- Tri-state everything we can directly control. The C128 owns address, data and the control
   -- lines, unless a cartridge has taken the bus with /DMA.
   cart_ctrl_oe_o  <= '0';
   cart_addr_oe_o  <= '0';
   cart_data_oe_o  <= '0';

   -- Due to a bug in the R5/R6 boards the slot has to be enabled ALWAYS, otherwise joystick
   -- port B does not work correctly.
   cart_en_o       <= '1';

   -- GAME, EXROM, NMI and IRQ stay read-only on every board revision. The C128's MMU can force
   -- GAME/EXROM low internally (mmu8722.vhd applies that to the bus logic), but we do not
   -- transmit it to the cartridge, just like the C64 core does not.
   cart_game_oe_o  <= '0';
   cart_exrom_oe_o <= '0';
   cart_nmi_oe_o   <= '0';
   cart_irq_oe_o   <= '0';

   -- ROML/ROMH become write-only as soon as the slot is in use
   cart_roml_oe_o  <= '0';
   cart_romh_oe_o  <= '0';

   cart_phi2_o     <= '0';
   cart_dotclock_o <= '0';
   cart_game_o     <= '1';
   cart_exrom_o    <= '1';
   cart_nmi_o      <= '1';
   cart_irq_o      <= '1';
   cart_roml_o     <= '1';
   cart_romh_o     <= '1';
   cart_ba_o       <= '0';
   cart_rw_o       <= '1';
   cart_io1_o      <= '1';
   cart_io2_o      <= '1';
   cart_a_o        <= (others => '0');
   cart_d_o        <= (others => '0');

   -- While the slot is unused, keep driving RESET unconditionally, exactly as this core did
   -- before it had expansion port support, so that switching the feature off restores the
   -- known-good boot behaviour bit for bit.
   cart_reset_o    <= cart_reset_n_int;
   cart_reset_oe_o <= '1';

   data_from_cart  <= x"00";

   if exp_port_hw = '1' then
      cart_ctrl_oe_o  <= '1';
      cart_roml_oe_o  <= '1';
      cart_romh_oe_o  <= '1';

      -- Bi-directional RESET: drive the line only while we want to reset the cartridge and read
      -- it otherwise, so that the reset button of a freezer cartridge reaches the C128 (see
      -- combined_reset_proc). On R3/R3A/R4 the pin is output-only and the top level feeds back a
      -- constant '1', so those boards simply never sense a reset and rely on the EF3 heuristics
      -- below instead. cart_reset_counter/cart_res_flckr_ign suppress our driver while such a
      -- faked cartridge-triggered reset is in progress.
      cart_reset_o    <= cart_reset_n_int when cart_reset_counter = 0 and cart_res_flckr_ign = 0 else '1';
      cart_reset_oe_o <= not cart_reset_o;

      cart_roml_o     <= cart_roml_n;
      cart_romh_o     <= cart_romh_n;
      cart_io1_o      <= cart_io1_n;
      cart_io2_o      <= cart_io2_n;
      cart_rw_o       <= not system_we;
      cart_phi2_o     <= core_phi2;
      cart_dotclock_o <= vic_pixel_ce;
      -- The VIC's BA, as C64MEGA65 V6 does
      cart_ba_o       <= core_ba;

      if cart_dma_n_q = '0' then
         -- The cartridge is bus master, see cart_dma_proc
         cart_addr_oe_o <= '0';
         cart_ctrl_oe_o <= cart_dma_ctrl_oe;
         cart_rw_o      <= '1';
         cart_io1_o     <= '1';
         cart_io2_o     <= '1';
         cart_data_oe_o <= cart_dma_data_oe;
         cart_d_o       <= cart_dma_din_q;
      else
         -- Address bus. Only the low 16 bits of the core's address are a CPU/VIC address; bits
         -- 17..16 are the C128's RAM bank select and must not leave the machine.
         cart_addr_oe_o  <= '1';
         if cart_umax_vic = '0' then
            cart_a_o     <= core_ram_addr(15 downto 0);
         else
            -- Ultimax mode with the VIC on the bus: per "The PLA Dissected", A12..A15 are pulled up
            -- by RP4 whenever the VIC has the bus, so a cartridge sees them as %1111.
            cart_a_o     <= "11" & core_vic_addr;
         end if;

         -- Data bus: turn it into an input while the cartridge answers a read, drive it otherwise,
         -- so that the CPU can also write to the cartridge (bank switching).
         if cart_rd_now = '1' or cart_rd_hold /= 0 then
            cart_data_oe_o <= '0';  -- input
            data_from_cart <= cart_d_i;
         else
            cart_data_oe_o <= '1';  -- output
            if system_we = '0' then
               cart_d_o    <= machine_data;
            else
               cart_d_o    <= core_ram_data_out;
            end if;
         end if;
      end if;
   end if;
end process handle_hardware_expansion_proc;

--------------------------------------------------------------------------------------------------
-- REU (MiSTer reu.sv) in HyperRAM, or SDRAM where the board has it
--------------------------------------------------------------------------------------------------
-- cpu_we must see I/O writes ($DF01-$DF0A) and $FF00, so it takes system_we, not ram_we.
reu_inst : entity work.reu
   generic map (
      REU_ADDR => 0
   )
   port map (
      clk       => clk_main_i,
      reset     => not reset_core_n,
      cfg       => reu_cfg,
      wrap      => '0',           -- 2 MB counts straight through, see doc/hyperram-map.md
      dma_req   => reu_dma_req,
      dma_cycle => reu_dma_cycle,
      dma_addr  => reu_dma_addr,
      dma_dout  => reu_dma_dout,
      dma_din   => std_logic_vector(reu_dma_din),
      dma_we    => reu_dma_we,
      ram_cycle => reu_ram_cycle,
      ram_addr  => reu_ram_addr,
      ram_dout  => reu_ram_dout,
      ram_din   => reu_ram_din,
      ram_we    => reu_ram_we,
      ram_cs    => reu_ram_cs,
      cpu_addr  => std_logic_vector(core_ram_addr(15 downto 0)),
      cpu_dout  => std_logic_vector(core_ram_data_out),
      cpu_din   => reu_dout,
      cpu_we    => system_we,
      cpu_cs    => reu_cs,
      irq       => reu_irq
   ); -- reu_inst

-- reu.sv masks addr_ram only when a transfer starts. Mask here too, so that reu_mapper's
-- prefetch of a freshly written $DF04-$DF06 stays inside the REU's window.
reu_ram_addr_m <= reu_ram_addr and ("000000" & "111" & x"FFFF") when reu_cfg = "01" else
                  reu_ram_addr and ("0000" & "11111" & x"FFFF") when reu_cfg = "10" else
                  reu_ram_addr and ('0' & x"FFFFFF");

-- As in C64MEGA65: reu_mapper prefetches and holds the REU's RAM cycle until the data is
-- there, avm_cache keeps sequential reads fast, and avm_fifo in mega65.vhd crosses to hr_clk.
reu_mapper_inst : entity work.reu_mapper
   generic map (
      G_BASE_ADDRESS => G_REU_BASE
   )
   port map (
      clk_i               => clk_main_i,
      rst_i               => not reset_core_n,
      reu_ext_cycle_i     => core_ext_cycle,
      reu_ext_cycle_o     => reu_ram_cycle,
      reu_addr_i          => reu_ram_addr_m,
      reu_dout_i          => reu_ram_dout,
      reu_din_o           => reu_ram_din,
      reu_we_i            => reu_ram_we,
      reu_cs_i            => reu_ram_cs,
      avm_write_o         => map_write,
      avm_read_o          => map_read,
      avm_address_o       => map_address,
      avm_writedata_o     => map_writedata,
      avm_byteenable_o    => map_byteenable,
      avm_burstcount_o    => map_burstcount,
      avm_readdata_i      => map_readdata,
      avm_readdatavalid_i => map_readdatavalid,
      avm_waitrequest_i   => map_waitrequest
   ); -- reu_mapper_inst

reu_avm_cache_inst : entity work.avm_cache
   generic map (
      G_CACHE_SIZE   => 8,
      G_ADDRESS_SIZE => 32,
      G_DATA_SIZE    => 16
   )
   port map (
      clk_i                 => clk_main_i,
      rst_i                 => not reset_core_n,
      s_avm_waitrequest_o   => map_waitrequest,
      s_avm_write_i         => map_write,
      s_avm_read_i          => map_read,
      s_avm_address_i       => map_address,
      s_avm_writedata_i     => map_writedata,
      s_avm_byteenable_i    => map_byteenable,
      s_avm_burstcount_i    => map_burstcount,
      s_avm_readdata_o      => map_readdata,
      s_avm_readdatavalid_o => map_readdatavalid,
      m_avm_waitrequest_i   => reu_avm_waitrequest_i,
      m_avm_write_o         => reu_avm_write_o,
      m_avm_read_o          => reu_avm_read_o,
      m_avm_address_o       => reu_avm_address_o,
      m_avm_writedata_o     => reu_avm_writedata_o,
      m_avm_byteenable_o    => reu_avm_byteenable_o,
      m_avm_burstcount_o    => reu_avm_burstcount_o,
      m_avm_readdata_i      => reu_avm_readdata_i,
      m_avm_readdatavalid_i => reu_avm_readdatavalid_i
   ); -- reu_avm_cache_inst

--------------------------------------------------------------------------------------------------
-- Virtual cartridge: a .crt file in HyperRAM, its active banks cached in block RAM
--------------------------------------------------------------------------------------------------
-- A long reset press turns the cartridge off, so the C128 starts without it; the next reset or
-- a newly loaded file turns it back on.
crt_off_proc : process (clk_main_i)
begin
   if rising_edge(clk_main_i) then
      if crt_loading_i = '1' or (reset_core_n = '0' and hard_reset_n = '1') then
         crt_off <= '0';
      elsif hard_reset_n = '0' and hard_reset_n_d = '1' then
         crt_off <= '1';
      end if;
   end if;
end process crt_off_proc;

crt_active <= '1' when cart_source = CART_CRT and crt_loaded_i = '1' and crt_off = '0' else '0';
crt_hold   <= crt_active and crt_wait_i;

-- These types use $DF00-$DF1F, where the REU's registers are, so the REU is off while one runs, as
-- the Ultimate turns its own off for them (c64_crt.cc). Subtype 1 of FC3 and EasyFlash shares IO2.
crt_io2 <= '1' when crt_active = '1' and crt_c128_i = '0' and
                    (crt_id_i = x"0001" or crt_id_i = x"0008" or crt_id_i = x"001C" or
                     ((crt_id_i = x"0003" or crt_id_i = x"0020") and crt_subtype_i /= x"01")) else '0';

-- Each reset starts the cartridge again from its power-up state
cartridge_inst : entity work.cartridge
   port map (
      clk_i             => clk_main_i,
      rst_i             => crt_off,
      cart_loading_i    => crt_loading_i or not reset_core_n,
      cart_id_i         => crt_id_i,
      cart_exrom_i      => crt_exrom_i,
      cart_game_i       => crt_game_i,
      cart_size_i       => crt_size_i,
      cart_c128_i       => crt_c128_i,
      ioe_i             => core_ioe,
      iof_i             => core_iof,
      wr_en_i           => system_we,
      wr_data_i         => std_logic_vector(core_ram_data_out),
      addr_i            => std_logic_vector(core_ram_addr(15 downto 0)),
      bank_lo_o         => crt_bank_lo,
      bank_hi_o         => crt_bank_hi_o,
      ioe_wr_ena_o      => crt_ioe_wr_ena,
      iof_wr_ena_o      => crt_iof_wr_ena,
      io_rom_o          => crt_io_rom,
      io_ext_o          => crt_io_ext,
      io_data_o         => crt_io_data,
      exrom_o           => crt_exrom_n,
      game_o            => crt_game_n,
      roml_we_o         => crt_roml_we,
      freeze_key_i      => '0',
      suppress_freeze_i => '0',
      nmi_o             => crt_nmi,
      nmi_ack_i         => core_nmi_ack
   ); -- cartridge_inst

crt_bank_lo_o <= crt_bank_lo;
crt_addr_o    <= std_logic_vector(core_ram_addr(15 downto 0));
crt_data_o    <= std_logic_vector(core_ram_data_out);

-- With the file in SDRAM, a read is requested as soon as the address is out: the CPU's from
-- the start of its access (IO1/IO2 by the selects the port uses, which span it too), the VIC's
-- a clock after each phase starts, when its registered address has caught up.
crt_rd_lo_o    <= crt_active and (core_roml or core_ioe_bus or core_iof_bus);
crt_rd_hi_o    <= crt_active and core_romh;
crt_vic_rd_o   <= crt_active and cart_umax_vic when phi2_ph /= 1 else '0';
crt_vic_addr_o <= std_logic_vector(core_vic_addr(12 downto 0));

-- SDRAM refreshes go in the middle of PHI2. Every read is requested at a phase start, the CPU's
-- in FAST mode 8 clocks before it is latched, so a refresh there never makes one late. With no
-- cartridge, PHI2 stopped (pause) or the C128 in reset, any time will do.
crt_rfsh_proc : process (clk_main_i)
begin
   if rising_edge(clk_main_i) then
      crt_rfsh_ok <= '0';
      if crt_active = '0' or phi2_cnt = 31 or reset_core_n = '0' or
         (core_phi2 = '1' and phi2_ph >= 3 and phi2_ph <= 10) then
         crt_rfsh_ok <= '1';
      end if;
   end if;
end process crt_rfsh_proc;

crt_rfsh_ok_o <= crt_rfsh_ok;

-- The REU starts its SDRAM accesses in the second half of PHI2 low, after the FAST CPU's read at
-- its start, and they are done before the refreshes in PHI2 high (tb_reu_sdram). Ending earlier
-- would delay its read cache's refills past SWAP's next write.
crt_reu_proc : process (clk_main_i)
begin
   if rising_edge(clk_main_i) then
      crt_reu_ok <= '0';
      if crt_active = '0' or phi2_cnt = 31 or reset_core_n = '0' or
         (core_phi2 = '0' and phi2_ph >= C_REU_FROM and phi2_ph <= C_REU_TO) then
         crt_reu_ok <= '1';
      end if;
   end if;
end process crt_reu_proc;

crt_reu_ok_o <= crt_reu_ok;

-- EasyFlash's flash chips see each CPU access to ROML/ROMH once, at its RAM strobe. As with
-- the port's /ROML-/ROMH, only an Ultimax write selects a chip.
crt_reset_o    <= not reset_core_n;
crt_phi2_o     <= core_phi2;
crt_flash_wr_o <= crt_active and ram_ce and system_we and (core_roml or core_romh) and
                  core_exrom_n and not core_game_n;
crt_flash_rd_o <= crt_active and ram_ce and not system_we and (core_roml or core_romh);

-- The cartridge's RAMs. While its RAM is at ROML, writes to $9Exx/$9Fxx also reach the
-- $DExx/$DFxx copy (Action Replay, see C64MEGA65's main.vhd). EasyFlash's $DF00 RAM is separate.
crt_addr_9exx <= '1' when core_ram_addr(12 downto 8) = "11110" else '0';
crt_addr_9fxx <= '1' when core_ram_addr(12 downto 8) = "11111" else '0';
crt_ioe_we_o  <= crt_active and system_we and
                 (core_ioe or (core_roml and crt_roml_we and crt_ioe_wr_ena and crt_addr_9exx));
crt_iof_we_o  <= crt_active and system_we and
                 (core_iof or (core_roml and crt_roml_we and crt_iof_wr_ena and crt_addr_9fxx));
crt_ram_we_o  <= crt_active and system_we and core_roml and crt_roml_we;

-- Route the cartridge source back into the C128, then lay the REU over it
handle_cores_expansion_port_signals_proc : process (all)
   variable v_irq_n : std_logic;
   variable v_dma   : std_logic;
begin
   core_game_n  <= '1';
   core_exrom_n <= '1';
   core_io_rom  <= '0';
   core_io_ext  <= '0';
   core_io_data <= x"FF";
   core_nmi_n   <= core_nmi_n_s;    -- RESTORE key, see restore_nmi
   v_irq_n      := '1';
   v_dma        := '0';

   case cart_source is
      when CART_HW =>
         core_game_n  <= cart_game_n;
         core_exrom_n <= cart_exrom_n;
         v_irq_n      := cart_irq_n;
         core_nmi_n   <= cart_nmi_n and core_nmi_n_s;
         -- Let the cartridge win every $DExx/$DFxx read. io_rom stays '0' because that would map
         -- the I/O windows into cartridge ROM held in the core's own memory, which only applies to
         -- emulated cartridges.
         core_io_ext  <= core_ioe or core_iof;
         core_io_data <= data_from_cart;
         v_dma        := cart_dma_req;

      when CART_CRT =>
         -- Its ROM reads come through ramDin (cpu_data_in_proc). While the cache fetches a bank
         -- the CPU waits, as for DMA.
         if crt_active = '1' then
            core_game_n  <= crt_game_n;
            core_exrom_n <= crt_exrom_n;
            core_io_rom  <= crt_io_rom;
            core_io_ext  <= crt_io_ext;
            core_io_data <= unsigned(crt_io_data);
            core_nmi_n   <= core_nmi_n_s and not crt_nmi;
            v_dma        := crt_wait_i;
         end if;
   end case;

   -- The REU only claims $DF00-$DF1F; the rest of IO2 stays with the cartridge (C64MEGA65 #208)
   if reu_cfg /= "00" then
      if reu_io = '1' then
         core_io_rom  <= '0';
         core_io_ext  <= '1';
         core_io_data <= unsigned(reu_dout);
      end if;
      v_irq_n := v_irq_n and not reu_irq;
      v_dma   := v_dma or reu_dma_req;
   end if;

   -- The command interface answers over the REU's $DF18-$DF1F and a cartridge's
   if uci_sel = '1' then
      core_io_rom  <= '0';
      core_io_ext  <= '1';
      core_io_data <= unsigned(uci_data_i);
   end if;
   if uci_on = '1' then
      v_irq_n := v_irq_n and not uci_irq_i;
      v_dma   := v_dma or (uci_dma_i and core_z80_n);
   end if;

   -- The SwiftLink answers its registers ahead of a cartridge, and its 6551 pulls NMI
   if modem_sel = '1' then
      core_io_rom  <= '0';
      core_io_ext  <= '1';
      core_io_data <= unsigned(modem_data_i);
   end if;
   if modem_irq_i = '1' then
      core_nmi_n <= '0';
   end if;

   core_irq_n <= v_irq_n;
   core_dma   <= v_dma;
end process handle_cores_expansion_port_signals_proc;

-- Detect cartridges that need special treatment because R3/R3A/R4 boards cannot sense a
-- cartridge-driven reset. Harmless (and optimised away) on R5 and newer.
cartridge_heuristics_inst : entity work.cartridge_heuristics
   port map (
      clk_main_i     => clk_main_i,
      reset_core_n_i => reset_core_n,
      cart_exrom_n_i => cart_exrom_n,
      cart_game_n_i  => cart_game_n,
      cart_io1_n_i   => cart_io1_n,
      ram_we_i       => system_we,
      ram_addr_i     => std_logic_vector(core_ram_addr(15 downto 0)),
      phi2_i         => core_phi2,
      is_an_EF3_o    => cart_is_an_EF3
   ); -- cartridge_heuristics_inst

-- Workaround for R3/R3A/R4 boards, which cannot let a cartridge pull the reset line low.
-- We watch for the EasyFlash 3 asking for a reset and generate it ourselves.
-- Background: https://github.com/MJoergen/C64MEGA65/issues/60
handle_cartridge_triggered_resets_proc : process (clk_main_i)
begin
   if rising_edge(clk_main_i) then
      if G_BOARD = "MEGA65_R3" or G_BOARD = "MEGA65_R4" then
         core_phi2_prev <= core_phi2;

         -- We cannot use reset_core_n here: it goes low as soon as cart_reset_counter is > 0,
         -- which would clear the counter again prematurely.
         if reset_soft_i = '1' or reset_hard_i = '1' then
            cart_reset_counter <= 0;
            cart_res_flckr_ign <= 0;
         elsif cart_reset_counter > 0 and core_phi2_prev = '1' and core_phi2 = '0' then
            -- The reset duration is measured in phi2 cycles
            cart_reset_counter <= cart_reset_counter - 1;
         end if;

         -- Suppress the trailing reset pulse towards the cartridge after the counter reached
         -- zero but reset_core_n has not been released yet.
         if reset_core_n = '0' and cart_reset_counter = 0 and cart_res_flckr_ign /= 0 then
            cart_res_flckr_ign <= cart_res_flckr_ign - 1;
         end if;

         -- The EF3 signals "reset me and start the selected mode" by writing the mode to $DE0F.
         -- Mode $02 (Kernal) is deliberately not supported: in Kernal mode the EF3 manipulates
         -- A14 itself and would fight the MEGA65's address transceiver, which could damage
         -- either side.
         if cart_is_an_EF3 = '1' and system_we = '1' and cart_io1_n = '0' and core_ram_addr(15 downto 0) = x"DE0F" then
            if core_ram_data_out = x"00" or core_ram_data_out = x"04" or
               core_ram_data_out = x"05" or core_ram_data_out = x"07" then
               cart_reset_counter <= C_EF3_RESET_LEN;
               cart_res_flckr_ign <= 2;
            end if;
         end if;
      else
         cart_reset_counter <= 0;
         cart_res_flckr_ign <= 0;
      end if;
   end if;
end process handle_cartridge_triggered_resets_proc;

--------------------------------------------------------------------------------------------------
-- Hardware IEC serial port (real Commodore drives, e.g. 1541/1571/1581)
--------------------------------------------------------------------------------------------------
-- The MEGA65 drives the physical IEC lines through bidirectional level shifters. CLK, DATA and
-- SRQ are open-collector: every participant either pulls the line low or releases it (never
-- actively drives it high). We emulate this by tri-stating the driver (via *_en_o = '0') whenever
-- the core releases the line, and enabling it to drive a hard '0' whenever the core asserts it.
-- ATN is only ever driven by the computer (the bus controller), so it is a plain push-pull output.
-- The input lines are sensed active-high (1 = line released) which is exactly what
-- fpga64_sid_iec expects, so iec_*_n_i pass straight through into the core (see instantiation).
--
-- The emulated drives share this bus with the physical port, so every open-collector line is
-- merged by a wired-AND, exactly as the wire itself would do on a real C128: the computer sees
-- the pin AND the emulated drives, the emulated drives see the computer AND the pin, and the pin
-- is pulled low as soon as either of them asserts. ATN stays push-pull and computer-driven, since
-- only the bus controller ever drives it.
handle_hardware_iec_proc : process (all)
begin
   -- Disabled means electrically silent and logically disconnected. In particular,
   -- a powered external device holding a line low cannot disturb the virtual drives.
   iec_reset_n_o   <= '1';
   iec_atn_n_o     <= '1';
   iec_clk_n_o     <= '1';
   iec_clk_en_o    <= '0';
   iec_data_n_o    <= '1';
   iec_data_en_o   <= '0';
   iec_srq_n_o     <= '1';
   iec_srq_en_o    <= '0';
   hw_iec_clk_n_i  <= '1';
   hw_iec_data_n_i <= '1';
   hw_iec_srq_n_i  <= '1';

   if iec_hardware_port_en_i = '1' then
      iec_reset_n_o   <= reset_core_n;
      iec_atn_n_o     <= core_iec_atn_o;
      iec_clk_n_o     <= '0';
      iec_clk_en_o    <= not (core_iec_clk_o and drv_iec_clk_o);
      iec_data_n_o    <= '0';
      iec_data_en_o   <= not (core_iec_data_o and drv_iec_data_o);
      iec_srq_n_o     <= '0';
      iec_srq_en_o    <= not (core_iec_srq_n_o and drv_iec_srq_o);
      hw_iec_clk_n_i  <= iec_clk_n_i;
      hw_iec_data_n_i <= iec_data_n_i;
      hw_iec_srq_n_i  <= iec_srq_n_i;
   end if;
end process handle_hardware_iec_proc;


--------------------------------------------------------------------------------------------------
-- Hard reset
--------------------------------------------------------------------------------------------------

-- A changed REU or Cartridge setting, or a .crt file being loaded, resets the C128 and a hardware
-- cartridge, as soon as no write-back cache is dirty. A virtual cartridge's reset lasts until
-- the cache holds the banks it starts in.
exp_change_reset_proc : process (clk_main_i)
   variable v_cfg : std_logic_vector(3 downto 0);
begin
   if rising_edge(clk_main_i) then
      v_cfg     := reu_menu & exp_port_hw & crt_loaded_i;
      exp_cfg_d <= v_cfg;
      if exp_reset_cnt > 1 or (exp_reset_cnt = 1 and crt_hold = '0') then
         exp_reset_cnt <= exp_reset_cnt - 1;
      end if;
      if v_cfg /= exp_cfg_d then
         exp_reset_pend <= '1';
      elsif exp_reset_pend = '1' and prevent_reset = '0' then
         exp_reset_pend <= '0';
         exp_reset_cnt  <= G_EXP_RESET_LEN;
      end if;
   end if;
end process exp_change_reset_proc;

hard_reset_proc: process (clk_main_i)
  begin
    if rising_edge(clk_main_i) then
      if reset_soft_i = '1' or reset_hard_i = '1' or cart_reset_counter /= 0 or exp_reset_cnt /= 0 then
        -- Due to sw_cartridge_wrapper's logic, reset_soft_i stays high longer than reset_hard_i.
        -- We need to make sure that this is not interfering with hard_reset_n
        if reset_hard_i = '1' then
          hard_rst_counter <= C_HARD_RST_DELAY;
          hard_reset_n <= '0';
        end if;

        -- reset_core_n is low-active, so prevent_reset = 0 means execute reset
        -- but a hard reset can override
        reset_core_int_n <= prevent_reset and (not reset_hard_i);
      else
        -- The idea of the hard reset is, that while reset_core_n is back at '1' and therefore the core is
        -- running (not being reset any more), hard_reset_n stays low for C_HARD_RST_DELAY clock cycles.
        -- Reason: We need to give the KERNAL time to execute the routine $FD02 where it checks for the
        -- cartridge signature "CBM80" in $8003 onwards. In case reset_n = '0' during these tests (i.e. hard
        -- reset active) we will return zero instead of "CBM80" and therefore perform a hard reset.
        reset_core_int_n <= '1';
        if hard_rst_counter = 0 then
          hard_reset_n <= '1';
        else
          hard_rst_counter <= hard_rst_counter - 1;
        end if;
      end if;
    end if;
  end process;

-- Combined reset signal to be used throughout main.vhd: reset triggered by the MEGA65's reset button (reset_core_int_n)
-- and reset triggered by an external cartridge.

combined_reset_proc: process (all)
  begin
    reset_core_n <= '1';

    if pwrup_reset_cnt /= 0 then
      reset_core_n <= '0';
    elsif reset_core_int_n = '0' then
      reset_core_n <= '0';
    elsif cart_reset_n_q = '0' and prevent_reset = '0' and exp_port_hw = '1' then
      -- A cartridge (or its reset button) pulls RESET low. Only honoured while the slot is in
      -- use, because otherwise we drive cart_reset_o permanently and would read back our own level.
      reset_core_n <= '0';
    end if;
  end process;

pwrup_reset_proc: process (clk_main_i)
begin
  if rising_edge(clk_main_i) then
    if pwrup_reset_cnt /= 0 then
      pwrup_reset_cnt <= pwrup_reset_cnt - 1;
    end if;
  end if;
end process;

-- To make sure that cartridges in the Expansion Port start properly, we must not do a hard reset and mask the $8000 memory area,
-- when the core is launched for the first time (cold start).

handle_cold_start_proc: process (clk_main_i)
  begin
    if rising_edge(clk_main_i) then
      hard_reset_n_d <= hard_reset_n;
      -- detect the rising edge of hard_reset_n_d
      if hard_reset_n = '1' and hard_reset_n_d = '0' and cold_start_done = '0' then
        cold_start_done <= '1';
      end if;
    end if;
  end process;

--------------------------------------------------------------------------------------------------
-- Access to C64's RAM and hardware/simulated cartridge ROM
--------------------------------------------------------------------------------------------------
mem_hold_proc: process (clk_main_i)
  variable rom_addr_live : std_logic_vector(16 downto 0);
begin
  if rising_edge(clk_main_i) then
    rom_addr_live := std_logic_vector(sysrom_bank) & std_logic_vector(core_ram_addr(11 downto 0));
    sysrom_cs_d <= sysrom_cs;
    ram_ce_d    <= ram_ce;

    if reset_core_n = '0' then
      rom_addr_held <= (others => '0');
      ram_addr_held <= (others => '0');
      ram_data_r    <= (others => '0');
    else
      if sysrom_cs = '1' then
        if sysrom_cs_d = '0' then
          rom_addr_held <= rom_addr_live;
        end if;
        sysrom_data_r <= unsigned(sys_rom_data_i);
      end if;

      -- CPU read path (unchanged): the CPU presents its address before ce, so the
      -- BRAM data is already valid at ce.
      if ram_ce = '1' and ram_we = '0' then
        if ram_ce_d = '0' then
          ram_addr_held <= core_ram_addr;
        end if;
        ram_data_r <= ram_data_i;
      end if;
    end if;
  end if;
end process;

-- Simplified memory read mux (post-upstream-merge re-tune):
-- Present the core's LIVE address to the BRAM/ROM and return the LIVE 1-cycle-latency
-- read data. The merged core holds systemAddr stable across an access (as the MiSTer
-- SDRAM expects, latching addr at ce), so no address/data holding shim is needed here.
-- What the C128 itself puts on the data bus. Deliberately free of any dependency on ram_data so
-- that feeding it to the cartridge's data bus cannot create a combinational loop.
machine_data <= unsigned(sys_rom_data_i) when sysrom_cs = '1' else ram_data_i;

cpu_data_in_proc: process (all)
  begin
    ram_data <= x"00";

    -- We are emulating what is written here: https://www.c64-wiki.com/wiki/Reset_Button
    -- and avoid that the KERNAL ever sees the CBM80 signature during hard reset reset.
    if hard_reset_n = '0' and core_ram_addr(15 downto 12) = x"8" and cold_start_done = '1' then
      ram_data <= x"00";
    -- A cartridge in the slot answers the read instead of RAM/ROM. The C128's bus logic routes
    -- ROML/ROMH reads (both the C64 cartridge windows and the C128's External Function ROM)
    -- through ramDin, which is exactly this signal. The VIC's Ultimax fetch counts only in the
    -- core's VIC slot, so the Z80's PHI1 slots still read RAM.
    elsif exp_port_hw = '1' and (cart_roml_n = '0' or (core_romh = '1' and core_aec = '1') or
                                 core_umax_romh = '1') then
      ram_data <= data_from_cart;
    -- A virtual cartridge's ROM, or its RAM where the cartridge maps it. $DExx/$DFxx only reach
    -- the CPU from here while the cartridge sets io_rom.
    elsif crt_active = '1' and core_roml = '1' then
      ram_data <= unsigned(crt_ram_data_i) when crt_roml_we = '1' else unsigned(crt_lo_data_i);
    elsif crt_active = '1' and core_umax_romh = '1' then
      ram_data <= unsigned(crt_vic_data_i);
    elsif crt_active = '1' and core_romh = '1' then
      ram_data <= unsigned(crt_hi_data_i);
    elsif crt_active = '1' and core_ioe = '1' then
      ram_data <= unsigned(crt_ioe_data_i) when crt_ioe_wr_ena = '1' else unsigned(crt_lo_data_i);
    elsif crt_active = '1' and core_iof = '1' then
      ram_data <= unsigned(crt_iof_data_i) when crt_iof_wr_ena = '1' else unsigned(crt_lo_data_i);
    else
      ram_data <= machine_data;
    end if;
  end process;

-- MiSTer SDRAM ANDs ramCE with ramWE. MEGA65 BRAM uses ramWE alone, so fpga64
-- already gates ramWE with cs_ram (I/O stores must not write RAM). Do not AND
-- ramCE here: Z80 latch writes miss the CE strobe.
ram_we_o <= ram_we;
ram_data_o <= core_ram_data_out;
sys_rom_addr_o <= std_logic_vector(sysrom_bank) & std_logic_vector(core_ram_addr(11 downto 0));
ram_addr_o <= core_ram_addr;
sysrom_data <= unsigned(sys_rom_data_i);
joy_a <= '0' & (not joy_1_fire_n_i) & (not joy_1_right_n_i) & (not joy_1_left_n_i) &
         (not joy_1_down_n_i) & (not joy_1_up_n_i) & '0';
joy_b <= '0' & (not joy_2_fire_n_i) & (not joy_2_right_n_i) & (not joy_2_left_n_i) &
         (not joy_2_down_n_i) & (not joy_2_up_n_i) & '0';
audio_left_o <= signed(sid_audio_l(17 downto 2));
audio_right_o <= signed(sid_audio_r(17 downto 2));

--------------------------------------------------------------------------------------------------
-- Keyboard: direct MEGA65 matrix emulation (same approach as C64 MEGA65 core).
--------------------------------------------------------------------------------------------------
-- Key 72 is the real Caps Lock key and stays down on its own. Key 68 is F9: a
-- press edge toggles f9_latch. Changing which key has which job clears that latch
-- so the function that just moved onto F9 starts released.
--
-- A short reset (reset_core_n) is the C128's reset button: the latched F9 function
-- (Caps Lock, or 40/80) must survive it. Only a long press, which is reset_hard_i
-- (M2M/QNICE reset, also power-on via main_rst), clears that latch.
--
-- While the menu or a reset has the keyboard, every key reads released, so both keys
-- keep their last real state. Otherwise "Follow 40/80" flips to the VIC whenever the
-- menu opens.
capslock_tracker : process(clk_main_i)
   variable v_role : std_logic;
begin
  if rising_edge(clk_main_i) then
    v_role := osm_control_i(C_MENU_KEYS_ASCII);
    if reset_hard_i = '1' then
      capslock_engaged_n <= '1';
      f9_prev_n          <= '1';
      f9_seen            <= '0';
      f9_latch           <= '0';
      role_ascii_d       <= '0';
    elsif reset_core_n = '0' then
      null;
    else
      if kb_key_num_i = 72 and kb_key_enabled_i = '1' then
        capslock_engaged_n <= kb_key_pressed_n_i;
      end if;
      if kb_key_num_i = 68 and kb_key_enabled_i = '1' then
        -- The first sample after reset only records the level, so a key that
        -- is still held does not count as a new press.
        if f9_seen = '1' and kb_key_pressed_n_i = '0' and f9_prev_n = '1'
           and v_role = role_ascii_d then
          f9_latch <= not f9_latch;
        end if;
        f9_prev_n <= kb_key_pressed_n_i;
        f9_seen   <= '1';
      end if;
      if v_role /= role_ascii_d then
        f9_latch <= '0';
      end if;
      role_ascii_d <= v_role;
    end if;
  end if;
end process;

f9_latched_o <= f9_latch;

-- Default (ASCII/DIN menu bit clear): real Caps Lock is 40/80, F9 is Caps Lock.
-- The other choice swaps them. A pressed key reads as '0' on the sense line.
d4080_sel_s   <= capslock_engaged_n when osm_control_i(C_MENU_KEYS_ASCII) = '0' else not f9_latch;
cpslk_sense_s <= not f9_latch       when osm_control_i(C_MENU_KEYS_ASCII) = '0' else capslock_engaged_n;

keyboard_inst : entity work.keyboard
  port map (
    clk_main_i           => clk_main_i,
    reset_i              => not reset_core_n,

    trigger_run_i        => '0',

    key_num_i            => kb_key_num_i,
    key_pressed_n_i      => kb_key_pressed_n_i,

    joy_1_up_n_i         => joy_1_up_n_i,
    joy_1_down_n_i       => joy_1_down_n_i,
    joy_1_left_n_i       => joy_1_left_n_i,
    joy_1_right_n_i      => joy_1_right_n_i,
    joy_1_fire_n_i       => joy_1_fire_n_i,

    joy_1_up_n_o         => open,
    joy_1_down_n_o       => open,
    joy_1_left_n_o       => open,
    joy_1_right_n_o      => open,
    joy_1_fire_n_o       => open,

    joy_2_up_n_i         => joy_2_up_n_i,
    joy_2_down_n_i       => joy_2_down_n_i,
    joy_2_left_n_i       => joy_2_left_n_i,
    joy_2_right_n_i      => joy_2_right_n_i,
    joy_2_fire_n_i       => joy_2_fire_n_i,

    joy_2_up_n_o         => open,
    joy_2_down_n_o       => open,
    joy_2_left_n_o       => open,
    joy_2_right_n_o      => open,
    joy_2_fire_n_o       => open,

    vic_ko_i             => vic_ko_s,

    cia1_pai_o           => cia1_pa_in,
    cia1_pao_i           => cia1_pa_out,
    cia1_pbi_o           => cia1_pb_in,
    cia1_pbo_i           => cia1_pb_out,

    restore_key_o        => restore_key_s
  );

--------------------------------------------------------------------------------------------------
-- RESTORE key -> NMI: the core exposes the RESTORE key as restore_key_s (freeze_key). Turn a
-- key-press edge into an NMI request that is held until the CPU acknowledges it (nmi_ack), just
-- like the MiSTer cartridge module does. RUN/STOP+RESTORE reset is handled by the KERNAL.
--------------------------------------------------------------------------------------------------
restore_nmi : process(clk_main_i)
begin
  if rising_edge(clk_main_i) then
    restore_key_d <= restore_key_s;
    if reset_core_n = '0' then
      nmi_q <= '0';
    else
      if restore_key_s = '1' and restore_key_d = '0' then
        nmi_q <= '1';
      elsif core_nmi_ack = '1' then
        nmi_q <= '0';
      end if;
    end if;
  end if;
end process;

core_nmi_n_s <= not nmi_q;

--------------------------------------------------------------------------------------------------
-- MiSTer Commodore 64 core / main machine
--------------------------------------------------------------------------------------------------
fpga64_sid_iec_inst: entity work.fpga64_sid_iec
    port map (
      clk32         => clk_main_i,
      -- clk32_speed   => clk_main_speed_i, TODO: remove CORE_CLK_SPEED? 
      clk_vdc       => clk_vdc_i,
      reset_n       => reset_core_n,

      -- Direct MEGA65 keyboard matrix on CIA1
      cia1_pa_i     => cia1_pa_in,
      cia1_pa_o     => cia1_pa_out,
      cia1_pb_i     => cia1_pb_in,
      cia1_pb_o     => cia1_pb_out,
      vic_ko_o      => vic_ko_s,

      cpslk_mode    => '0',
      cpslk_sense_i => cpslk_sense_s,
      sftlk_sense   => open,
      cpslk_sense   => open,
      d4080_sense   => open,
      noscr_sense   => open,

      -- Select C128's system ROM banks (boot0.rom)
      sysRom        => sysrom_cs,
      sysRomBank    => sysrom_bank,

      pause         => pause_i,
      pause_out     => open,      -- unused

      -- external memory
      ramAddr       => core_ram_addr,
      ramDin        => ram_data,
      vicRamDin     => ram_data, -- TEMP: live data (snow-fix timing to be reworked after boot)
      ramDout       => core_ram_data_out,
      ramCE         => ram_ce,
      ramWE         => ram_we,
      systemWe_o    => system_we,
      -- Reads answered by a hardware cartridge are substituted into ramDin (see cpu_data_in_proc),
      -- so from the bus logic's point of view the data bus is never floating.
      ramDinFloat   => '0',

      io_cycle      => open, -- 1 when an external I/O accesss is happening
      ext_cycle     => core_ext_cycle, -- the REU's RAM slot in every C128 cycle
      refresh       => open, -- 1 when a refresh cycle is happening (Not relevant for us)

      cia_mode      => '1',  -- 0 - 6526 "old", 1 - 8521 "new"
      turbo_mode    => "000",

      -- VGA/SCART interface
      -- The hsync frequency is 15.64 kHz (period 63.94 us).
      -- The hsync pulse width is 12.69 us.
      ntscMode      => '0',
      vic_variant   => "01",
      vicJailbars   => vic_jailbars,
      vicPalette    => "000",     -- default/standard C64 palette (upstream palette-selection feature)
      vicHsync      => core_vic_hs,
      vicVsync      => core_vic_vs,
      vicR          => vic_r,
      vic_pixel_ce_o => vic_pixel_ce,
      phi2_o        => core_phi2,      -- output. Expansion Port PHI2
      ba_o          => core_ba,        -- output. Expansion Port BA
      aec_o         => core_aec,       -- output. The VIC's AEC
      vic_addr_o    => core_vic_addr,  -- output. The VIC's address
      vicG          => vic_g,
      vicB          => vic_b,

      -- TODO: Add VDC support
      vdcHsync      => vdc_hs,
      vdcVsync      => vdc_vs,
      vdcR          => vdc_r,
      vdcG          => vdc_g,
      vdcB          => vdc_b,
      vdcVersion    => '0',
      vdc64k        => '1',
      vdcInitRam    => '1',       -- MiSTer default: clear VDC RAM on reset
      vdcPalette    => "0000",
      vdcDebug      => '0',

      -- cartridge port. Driven by handle_cores_expansion_port_signals_proc, which forces
      -- everything inactive while the Expansion Port is switched off so that an empty or
      -- floating slot cannot disturb the machine.
      game          => core_game_n,
      game_mmu      => open,           -- output. MMU's view of GAME; only needed by soft carts
      exrom         => core_exrom_n,
      exrom_mmu     => open,           -- output. MMU's view of EXROM; only needed by soft carts
      io_rom        => core_io_rom,    -- input
      io_ext        => core_io_ext,    -- input
      io_data       => core_io_data,   -- input
      irq_n         => core_irq_n,
      nmi_n         => core_nmi_n,     -- RESTORE key and/or cartridge NMI
      nmi_ack       => core_nmi_ack,   -- output
      -- Internal Function ROM selects. These live in a socket inside the C128, not on the
      -- expansion port, so a hardware cartridge never drives them.
      romFL         => open,           -- output
      romFH         => open,           -- output
      romL          => core_roml,      -- output. CPU access to 0x8000-0x9FFF
      romH          => core_romh,      -- output. CPU access to 0xA000-0xBFFF or 0xE000-0xFFFF (ultimax)
      UMAXromH      => core_umax_romh, -- output
      IOE           => core_ioe,       -- output. aka IO1. CPU access to 0xDExx
      IOF           => core_iof,       -- output. aka IO2. CPU access to 0xDFxx
      IO7           => core_io7,       -- output. CPU access to 0xD7xx
      IOE_bus       => core_ioe_bus,   -- output. IO1 for the Expansion Port
      IOF_bus       => core_iof_bus,   -- output. IO2 for the Expansion Port
      freeze_key  => open,
      mod_key     => open,
      tape_play   => open,
      
      -- The REU or a hardware cartridge, see cart_dma_proc
      dma_req       => core_dma,
      dma_cycle     => core_dma_cycle,
      dma_addr      => core_dma_addr,
      dma_dout      => core_dma_dout,
      dma_din       => core_dma_din,
      dma_we        => core_dma_we,
      irq_ext_n     => '1',            -- the REU's IRQ goes through irq_n, see core_irq_n


      -- paddle interface
      pot1          => pot1_x_i,
      pot2          => pot1_y_i,
      pot3          => pot2_x_i,
      pot4          => pot2_y_i,

      -- Joystick ports
      joyA          => joy_a,
      joyB          => joy_b,

      -- SID
      audio_l       => sid_audio_l,
      audio_r       => sid_audio_r,
      sid_filter    => "11",           -- filter enable = true for both SIDs, low bit = left SID
      sid_ver       => "01",           -- SID version, 0=6581, 1=8580, low bit = left SID
      sid_mode      => "000",          -- Right SID Port: 0=same as left, 1=DE00, 2=D420, 3=D500, 4=DF00
      sid_cfg       => "0000",         -- filter type: 0=Default, 1=Custom 1, 2=Custom 2, 3=Custom 3, lower two bits = left SID
      sid_fc_off_l  => (others => '0'),
      sid_fc_off_r  => (others => '0'),
      sid_digifix   => '0',           
      -- mechanism for loading custom SID filters
      sid_ld_clk    => clk_main_i,
      sid_ld_addr   => "000000000000",
      sid_ld_data   => x"0000",
      sid_ld_wr     => '0',

      -- User Port: Unused inputs need to be high
      -- TODO: Add User Port support
      pb_i          => x"FF",
      pb_o          => open,
      pa2_i         => '1',
      pa2_o         => open,
      pc2_n_o       => open,
      flag2_n_i     => '1',
      sp2_i         => '1',
      sp2_o         => open,
      sp1_i         => '1',
      sp1_o         => open,
      cnt2_i        => '1',
      cnt2_o        => open,
      cnt1_i        => '1',
      cnt1_o        => open,

      -- IEC serial bus. The open-collector output emulation lives in the concurrent
      -- assignments above; here we only tap the core's raw line-level signals. Inputs are
      -- sensed active-high (1 = released), matching the MEGA65 IEC buffer, so pass through.
      iec_srq_n_o   => core_iec_srq_n_o,
      iec_srq_n_i   => hw_iec_srq_n_i and drv_iec_srq_o,
      iec_clk_i     => hw_iec_clk_n_i and drv_iec_clk_o,
      iec_clk_o     => core_iec_clk_o,
      iec_atn_o     => core_iec_atn_o,
      iec_data_i    => hw_iec_data_n_i and drv_iec_data_o,
      iec_data_o    => core_iec_data_o,

      -- Cassette drive
      cass_write    => open,     -- output
      cass_motor    => open,     -- output
      cass_sense    => '1',
      cass_read     => '1',

      -- D7xx port
      d7port        => open,
      d7port_trig   => open,

      -- System mode
      sys256k       => '0', -- We have 128k memory
      force64       => '0',
      pure64        => '0',
      d4080_sel     => d4080_sel_s, -- 40/80 sense: real Caps Lock, or latched F9 (see d4080_sel_s)
      c128_n        => core_c128_n,
      z80_n           => core_z80_n,
      z80_we_o        => open,
      dbg_vic_has_bus_o => open,
      dbg_enable_vic_o  => open,
      dbg_aec_o         => open,
      dbg_vicdi_o       => open
    ); -- fpga64_sid_iec_inst

--------------------------------------------------------------------------------------------------
-- Virtual disk drives (device 8 and 9)
--------------------------------------------------------------------------------------------------

-- G_VDNUM = 0 builds the core without any emulated drive. The boot simulation uses that to
-- keep its runtime in minutes: two full 1541/1571/1581 models with bit-level GCR and MFM
-- emulation dominate a behavioural run of the C128 boot path they have nothing to do with.
drives_gen : if G_VDNUM > 0 generate

-- vd_sd serves every read and write of the drives from the SD card; vdrives only
-- signals the mounts (image size and type) and sees no requests.
signal vd_sd_off          : vd_std_array(G_VDNUM - 1 downto 0) := (others => '0');

-- The internal floppy as a 1581 (ported from C64MEGA65 issues #90, #93 and #244).
-- phys_en is the menu's choice. iec_drive gets phys_en_q, which goes through an
-- all-zero gap whenever the choice changes, so the controller sees its enable drop
-- on every switch and the drive select inside c1581_multi never moves while live.
-- A drive whose mode changes is held in reset during the gap, like a remount.
constant C_PHYS_SWITCH_GAP : natural := 255;
signal phys_switch_cnt    : natural range 0 to C_PHYS_SWITCH_GAP := 0;
signal phys_en            : std_logic_vector(G_VDNUM - 1 downto 0);
signal phys_en_d          : std_logic_vector(G_VDNUM - 1 downto 0) := (others => '0');
signal phys_en_q          : std_logic_vector(G_VDNUM - 1 downto 0) := (others => '0');
signal phys_changed       : std_logic_vector(G_VDNUM - 1 downto 0) := (others => '0');

-- Both sides of each floppy FIFO reset from the QNICE reset, never from the core
-- reset: a one-sided reset would desynchronize the pointers. fdc1772 and the
-- controller drain leftover bytes instead.
signal p1581_fiforst_m    : std_logic := '1';
signal p1581_fiforst_s    : std_logic := '1';

-- image drive busy or dirty, for the diag device (clk_main_i, then clk_sd_i)
signal img_drive_busy     : std_logic_vector(1 downto 0);
signal img_busy_sd_m      : std_logic_vector(1 downto 0) := (others => '0');
signal img_busy_sd_s      : std_logic_vector(1 downto 0) := (others => '0');

-- iec_drive <-> controller
signal p1581_active        : std_logic;
signal p1581_cia_motor_on  : std_logic;
signal p1581_cia_side      : std_logic;
signal p1581_step_req_tgl  : std_logic;
signal p1581_step_outward  : std_logic;
signal p1581_rd_req_tgl    : std_logic;
signal p1581_rd_op         : std_logic_vector(2 downto 0);
signal p1581_rd_track      : std_logic_vector(7 downto 0);
signal p1581_rd_side       : std_logic;
signal p1581_rd_sector     : std_logic_vector(7 downto 0);
signal p1581_rd_cancel_tgl : std_logic;
signal p1581_rd_seq        : std_logic_vector(1 downto 0);
signal p1581_step_ack_tgl  : std_logic;
signal p1581_rd_done_tgl   : std_logic;
signal p1581_rd_done_seq   : std_logic_vector(1 downto 0);
signal p1581_rd_result     : std_logic_vector(4 downto 0);
signal p1581_rd_crc_err    : std_logic;
signal p1581_rd_rnf        : std_logic;
signal p1581_rd_deleted    : std_logic;
signal p1581_rd_c          : unsigned(7 downto 0);
signal p1581_rd_h          : unsigned(7 downto 0);
signal p1581_rd_r          : unsigned(7 downto 0);
signal p1581_rd_n          : unsigned(7 downto 0);
signal p1581_media_ready   : std_logic;
signal p1581_index         : std_logic;
signal p1581_track0        : std_logic;
signal p1581_wprot         : std_logic;
signal p1581_change        : std_logic;
signal p1581_motor_on      : std_logic;
signal p1581_head_settled  : std_logic;
signal p1581_head_cyl      : unsigned(7 downto 0);
signal p1581_locked        : std_logic;

-- read FIFO: controller (clk_sd_i) -> drive (clk_main_i)
signal p1581_fifo_wr_data  : unsigned(7 downto 0);
signal p1581_fifo_wr_en    : std_logic;
signal p1581_fifo_wr_full  : std_logic;
signal p1581_fifo_level    : unsigned(9 downto 0);
signal p1581_byte_rd_en    : std_logic;
signal p1581_byte_data     : unsigned(7 downto 0);
signal p1581_byte_empty    : std_logic;

-- write FIFO: drive (clk_main_i) -> controller (clk_sd_i), and the write path levels
signal p1581_wr_deleted      : std_logic;
signal p1581_wr_precomp      : std_logic;
signal p1581_wr_byte_wr      : std_logic;
signal p1581_wr_byte_data    : std_logic_vector(7 downto 0);
signal p1581_wr_fifo_full    : std_logic;
signal p1581_wbyte_data      : std_logic_vector(7 downto 0);
signal p1581_wbyte_empty     : std_logic;
signal p1581_wbyte_rd        : std_logic;
signal p1581_wfifo_rd_level  : unsigned(2 downto 0);
signal p1581_wr_feed         : std_logic;
signal p1581_wr_lost         : std_logic;
signal p1581_wr_busy         : std_logic;
signal p1581_wr_ok           : std_logic;
signal p1581_wr_precomp_mode : std_logic_vector(1 downto 0);
signal p1581_f_wgate         : std_logic;
signal p1581_f_wdata         : std_logic;

-- fdc1772 event toggles (clk_main_i), 2-FF synced into clk_sd_i for the diag device
signal p1581_dbg_tgl       : std_logic_vector(7 downto 0);
signal p1581_dbg_sd_m      : std_logic_vector(7 downto 0) := (others => '0');
signal p1581_dbg_sd_s      : std_logic_vector(7 downto 0) := (others => '0');
signal p1581_dbg_pres_cnt  : std_logic_vector(10 downto 0);

-- controller observation taps for the diag device (all clk_sd_i)
signal p1581_diag_in_bits      : std_logic_vector(15 downto 0);
signal p1581_diag_out_bits     : std_logic_vector(15 downto 0);
signal p1581_diag_idx_period   : unsigned(31 downto 0);
signal p1581_diag_idx_width    : unsigned(31 downto 0);
signal p1581_diag_last_gap     : unsigned(15 downto 0);
signal p1581_diag_calc_crc     : unsigned(15 downto 0);
signal p1581_diag_stored_crc   : unsigned(15 downto 0);
signal p1581_diag_index_edge   : std_logic;
signal p1581_diag_index_qual   : std_logic;
signal p1581_diag_id_valid     : std_logic;
signal p1581_diag_id_crc_ok    : std_logic;
signal p1581_diag_data_end     : std_logic;
signal p1581_diag_data_crc_ok  : std_logic;
signal p1581_diag_gap_error    : std_logic;
signal p1581_diag_runt         : std_logic;
signal p1581_diag_est          : unsigned(11 downto 0);
signal p1581_diag_a1_candidate : std_logic;
signal p1581_diag_a1_reject    : std_logic;
signal p1581_diag_a1_train     : std_logic;
signal p1581_diag_mark_fe      : std_logic;
signal p1581_diag_mark_dam     : std_logic;
signal p1581_diag_dam_unarmed  : std_logic;
signal p1581_diag_match_id     : std_logic;
signal p1581_diag_dam_miss     : std_logic;
signal p1581_diag_rd_phase     : std_logic_vector(3 downto 0);
signal p1581_diag_step_phase   : std_logic_vector(1 downto 0);
signal p1581_diag_head_valid   : std_logic;
signal p1581_diag_head_dir     : std_logic;
signal p1581_diag_rd_req       : std_logic;
signal p1581_diag_rd_req_op    : std_logic_vector(2 downto 0);
signal p1581_diag_rd_req_trk   : unsigned(7 downto 0);
signal p1581_diag_rd_req_sec   : unsigned(7 downto 0);
signal p1581_diag_rd_req_side  : std_logic;
signal p1581_diag_wr_phase        : std_logic_vector(3 downto 0);
signal p1581_diag_wr_session      : std_logic;
signal p1581_diag_wr_feed         : std_logic;
signal p1581_diag_wr_abort        : std_logic;
signal p1581_diag_wr_prec         : std_logic;
signal p1581_diag_wr_done         : std_logic;
signal p1581_diag_wr_last         : std_logic_vector(7 downto 0);
signal p1581_diag_wr_bytes        : unsigned(15 downto 0);
signal p1581_diag_wr_first_margin : unsigned(7 downto 0);
signal p1581_diag_wr_trk_cyc      : unsigned(31 downto 0);
signal p1581_diag_wr_gate         : std_logic;
signal p1581_diag_wr_gate_cyc     : unsigned(31 downto 0);
signal p1581_diag_wr_underrun     : unsigned(15 downto 0);
signal p1581_diag_wr_discard      : unsigned(15 downto 0);
signal p1581_diag_wr_precnt       : unsigned(15 downto 0);
signal p1581_diag_wr_pushfull     : unsigned(15 downto 0);
signal p1581_diag_wr_reason       : std_logic_vector(7 downto 0);
signal p1581_diag_wr_reason_acc   : std_logic_vector(7 downto 0);
signal p1581_diag_wr_wflags       : std_logic_vector(4 downto 0);

attribute async_reg : string;
attribute async_reg of p1581_fiforst_m : signal is "true";
attribute async_reg of p1581_fiforst_s : signal is "true";
attribute async_reg of img_busy_sd_m   : signal is "true";
attribute async_reg of img_busy_sd_s   : signal is "true";
attribute async_reg of p1581_dbg_sd_m  : signal is "true";
attribute async_reg of p1581_dbg_sd_s  : signal is "true";

begin

-- 16 MHz chip enable for the IEC drives, so that ph2_r and ph2_f can be 1 MHz (the 1541's CPU
-- runs at 1 MHz). A counter compensates for the fact that the input clock is not exactly 32 MHz.
--
-- clk_main_speed_i is deliberately the vanilla CORE_CLK_SPEED_PAL and not the speed-adjusted
-- version used by HDMI Flicker-free: otherwise the compensation would cancel out the slow-down
-- and change the C128-to-drive frequency ratio, which breaks fastloaders.
-- See https://github.com/MJoergen/C64MEGA65/issues/2
iec_drive_ce_proc : process (all)
  variable msum, nextsum : integer;
begin
  msum    := clk_main_speed_i;
  nextsum := iec_dce_sum + 16000000;

  if rising_edge(clk_main_i) then
    iec_drive_ce <= '0';
    if reset_core_n = '0' then
      iec_dce_sum <= 0;
    else
      iec_dce_sum <= nextsum;
      if nextsum >= msum then
        iec_dce_sum  <= nextsum - msum;
        iec_drive_ce <= '1';
      end if;
    end if;
  end if;
end process iec_drive_ce_proc;

-- The internal floppy is drive 8 or drive 9 or off (one menu group). Should a bad
-- config file set both, drive 8 wins.
phys_en_gen : for i in 0 to G_VDNUM - 1 generate
  phys_en(i) <= osm_control_i(C_MENU_INT1581_D8) when i = 0 else
                osm_control_i(C_MENU_INT1581_D9) and not osm_control_i(C_MENU_INT1581_D8) when i = 1 else
                '0';
end generate phys_en_gen;

phys_switch_proc : process (clk_main_i)
begin
  if rising_edge(clk_main_i) then
    if phys_en /= phys_en_d then
      phys_changed    <= phys_changed or (phys_en xor phys_en_d);
      phys_en_d       <= phys_en;
      phys_switch_cnt <= C_PHYS_SWITCH_GAP;
      phys_en_q       <= (others => '0');
    elsif phys_switch_cnt /= 0 then
      phys_switch_cnt <= phys_switch_cnt - 1;
      phys_en_q       <= (others => '0');
    else
      phys_changed    <= (others => '0');
      phys_en_q       <= phys_en_d;
    end if;
  end if;
end process phys_switch_proc;

-- Drive enable is "If Mounted": a drive without a disk image stays in reset and is therefore
-- electrically silent on the IEC bus. MiSTer additionally offers "Always" and "Never".
-- The internal floppy runs without an image, and a drive is reset when it switches
-- between image and internal floppy.
iec_drives_reset_gen : for i in 0 to G_VDNUM - 1 generate
  iec_drives_reset(i) <= (not reset_core_n) or phys_changed(i) or
                         (not vdrives_mounted(i) and not phys_en_q(i));

  -- One shared menu group picks the 5.25" model for both drives. A mounted .D81
  -- overrides this inside iec_drive (img_hd). A long reset drops the volume
  -- (vdrives clears drive_mounted), so the remembered type goes with it.
  latch_img_type : process (clk_main_i)
  begin
    if rising_edge(clk_main_i) then
      if reset_hard_i = '1' then
        iec_img_type_latched(i) <= "00";
      elsif iec_img_mounted(i) = '1' then
        iec_img_type_latched(i) <= iec_img_type;
      end if;
    end if;
  end process latch_img_type;

  iec_drv_mode(i) <= "10" when iec_img_type_latched(i) = "01" else
                     "00" when osm_control_i(C_MENU_DRV_1541) = '1' else
                     "10";

  vd_sd_lba(i)          <= iec_sd_lba(i);
  vd_sd_blk_cnt(i)      <= iec_sd_blk_cnt(i);
  vd_sd_buf_data_out(i) <= iec_sd_buf_data_out(i);
end generate iec_drives_reset_gen;

-- vdrives carries a 2-bit image type, iec_drive wants {img_hd, img_mfm, img_gcr, img_ds}.
-- Raw GCR images (G64/G71) are not supported, which is what keeps the framework unmodified.
iec_sd_buf_addr16 <= std_logic_vector(resize(unsigned(iec_sd_buf_addr), 16));

with iec_img_type select iec_img_type4 <=
  "0010" when "00",     -- D64: GCR, single sided
  "0011" when "01",     -- D71: GCR, double sided
  "1000" when "10",     -- D81: HD 3.5"
  "0010" when others;

iec_drive_inst : entity work.iec_drive
  generic map (
    PARPORT => 0,       -- no parallel port (DolphinDOS speeder)
    DRIVES  => G_VDNUM
  )
  port map (
    clk          => clk_main_i,
    ce           => iec_drive_ce,
    reset        => iec_drives_reset,
    pause        => pause_i,

    drv_mode     => iec_drv_mode,

    -- IEC bus, wired-AND merged with the physical port (see the assignments further up)
    iec_atn_i    => core_iec_atn_o,
    iec_clk_i    => core_iec_clk_o   and hw_iec_clk_n_i,
    iec_data_i   => core_iec_data_o  and hw_iec_data_n_i,
    iec_fclk_i   => core_iec_srq_n_o and hw_iec_srq_n_i,
    iec_clk_o    => drv_iec_clk_raw,
    iec_data_o   => drv_iec_data_raw,
    iec_fclk_o   => drv_iec_srq_raw,

    -- disk image status
    img_mounted  => iec_img_mounted,
    img_readonly => iec_img_readonly,
    img_size     => iec_img_size,
    img_type     => iec_img_type4,

    led          => iec_drive_led,
    disk_ready   => iec_disk_ready,   -- the firmware's MOUNT_DISK waits for it (vd_sd's CSR)
    -- The track-number display (MiSTer's drv_overlay.sv) is out of scope, but these two
    -- cannot be left open: xsim refuses a VHDL-to-Verilog binding with an unconnected
    -- vector or array output, even though synthesis accepts it.
    out_track    => iec_out_track,
    out_we       => iec_out_we,

    -- parallel port, unused
    par_data_i   => x"FF",
    par_stb_i    => '1',
    par_data_o   => open,
    par_stb_o    => open,

    -- QNICE SD-card / FAT32 interface. vdrives deliberately does not cross the SD block and
    -- byte signals into the core clock domain, so this whole side runs on the QNICE clock.
    clk_sys      => clk_sd_i,

    sd_lba       => iec_sd_lba,
    sd_blk_cnt   => iec_sd_blk_cnt,
    sd_rd        => iec_sd_rd,
    sd_wr        => iec_sd_wr,
    sd_ack       => iec_sd_ack,
    sd_buff_addr => iec_sd_buf_addr16,
    sd_buff_dout => iec_sd_buf_data_in,      -- data going into the drive's buffer RAM
    sd_buff_din  => iec_sd_buf_data_out,     -- data read back from the drive's buffer RAM
    sd_buff_wr   => iec_sd_buf_wr,

    -- DOS ROM, pulled out of boot1.rom by the server FSM in mega65.vhd
    rom_loading  => drv_rom_loading_i,
    rom_req      => drv_rom_req_o,
    rom_addr     => drv_rom_addr_o,
    rom_data     => drv_rom_data_i,
    rom_wr       => drv_rom_wr_i,

    -- the internal floppy as a 1581, see i_physical_1581_controller below
    phys_mode              => phys_en_q,
    phys_active            => p1581_active,
    phys_cia_motor_on      => p1581_cia_motor_on,
    phys_cia_side          => p1581_cia_side,
    phys_step_req_tgl      => p1581_step_req_tgl,
    phys_step_outward      => p1581_step_outward,
    phys_rd_req_tgl        => p1581_rd_req_tgl,
    phys_rd_op             => p1581_rd_op,
    phys_rd_track          => p1581_rd_track,
    phys_rd_side           => p1581_rd_side,
    phys_rd_sector         => p1581_rd_sector,
    phys_rd_cancel_tgl     => p1581_rd_cancel_tgl,
    phys_rd_seq            => p1581_rd_seq,
    phys_step_ack_tgl      => p1581_step_ack_tgl,
    phys_rd_done_tgl       => p1581_rd_done_tgl,
    phys_rd_done_seq       => p1581_rd_done_seq,
    phys_rd_result         => p1581_rd_result,
    phys_rd_crc_err        => p1581_rd_crc_err,
    phys_rd_rnf            => p1581_rd_rnf,
    phys_rd_deleted        => p1581_rd_deleted,
    phys_rd_c              => std_logic_vector(p1581_rd_c),
    phys_byte_rd_en        => p1581_byte_rd_en,
    phys_byte_data         => std_logic_vector(p1581_byte_data),
    phys_byte_empty        => p1581_byte_empty,
    phys_media_ready       => p1581_media_ready,
    phys_index             => p1581_index,
    phys_track0            => p1581_track0,
    phys_wprot             => p1581_wprot,
    phys_change            => p1581_change,
    phys_motor_on          => p1581_motor_on,
    phys_head_settled      => p1581_head_settled,
    phys_wr_deleted        => p1581_wr_deleted,
    phys_wr_precomp        => p1581_wr_precomp,
    phys_wr_byte_wr        => p1581_wr_byte_wr,
    phys_wr_byte_data      => p1581_wr_byte_data,
    phys_wr_fifo_full      => p1581_wr_fifo_full,
    phys_wr_feed           => p1581_wr_feed,
    phys_wr_lost           => p1581_wr_lost,
    phys_wr_busy           => p1581_wr_busy,
    phys_dbg_lost_tgl      => p1581_dbg_tgl(0),
    phys_dbg_drain_tgl     => p1581_dbg_tgl(1),
    phys_dbg_staledone_tgl => p1581_dbg_tgl(2),
    phys_dbg_busycmd_tgl   => p1581_dbg_tgl(3),
    phys_dbg_fin_tgl       => p1581_dbg_tgl(4),
    phys_dbg_pres_cnt      => p1581_dbg_pres_cnt,
    phys_dbg_wr_fin_tgl    => p1581_dbg_tgl(5),
    phys_dbg_wr_wp_tgl     => p1581_dbg_tgl(6),
    phys_dbg_wr_drop_tgl   => p1581_dbg_tgl(7)
  ); -- iec_drive_inst

--------------------------------------------------------------------------------------------------
-- The MEGA65's internal 3.5" drive as a 1581 (CORE/vhdl/physical_1581, from C64MEGA65)
--------------------------------------------------------------------------------------------------
-- The controller runs on the QNICE clock and owns the floppy pins; it decodes and
-- encodes MFM and runs the mechanism. Read bytes cross to the drive clock through the
-- read FIFO, which holds a whole sector until its CRC is checked; write bytes come in
-- through the write FIFO. WGATE opens only while, among other things, the disk's
-- write-protect tab reads writable.
i_physical_1581_controller : entity work.physical_1581_controller
  generic map (
    G_CAPABLE => true
  )
  port map (
    clk_i            => clk_sd_i,
    rst_i            => rst_sd_i,

    f_rdata_i        => f_rdata_i,
    f_index_i        => f_index_i,
    f_track0_i       => f_track0_i,
    f_writeprotect_i => f_writeprotect_i,
    f_diskchanged_i  => f_diskchanged_i,
    f_motora_o       => f_motora_o,
    f_selecta_o      => f_selecta_o,
    f_side1_o        => f_side1_o,
    f_stepdir_o      => f_stepdir_o,
    f_step_o         => f_step_o,
    f_density_o      => f_density_o,
    f_wgate_o        => p1581_f_wgate,
    f_wdata_o        => p1581_f_wdata,

    phys_active_i    => p1581_active,
    cia_motor_on_i   => p1581_cia_motor_on,
    cia_side_i       => p1581_cia_side,

    step_req_tgl_i   => p1581_step_req_tgl,
    step_outward_i   => p1581_step_outward,
    step_ack_tgl_o   => p1581_step_ack_tgl,

    rd_req_tgl_i     => p1581_rd_req_tgl,
    rd_op_i          => p1581_rd_op,
    rd_track_i       => unsigned(p1581_rd_track),
    rd_side_i        => p1581_rd_side,
    rd_sector_i      => unsigned(p1581_rd_sector),
    rd_seq_i         => p1581_rd_seq,
    rd_cancel_tgl_i  => p1581_rd_cancel_tgl,
    rd_done_tgl_o    => p1581_rd_done_tgl,
    rd_done_seq_o    => p1581_rd_done_seq,
    rd_result_o      => p1581_rd_result,
    rd_crc_err_o     => p1581_rd_crc_err,
    rd_rnf_o         => p1581_rd_rnf,
    rd_deleted_o     => p1581_rd_deleted,
    rd_c_o           => p1581_rd_c,
    rd_h_o           => p1581_rd_h,
    rd_r_o           => p1581_rd_r,
    rd_n_o           => p1581_rd_n,

    byte_data_o      => p1581_fifo_wr_data,
    byte_wr_o        => p1581_fifo_wr_en,
    byte_ovf_i       => p1581_fifo_wr_full,

    wr_deleted_i     => p1581_wr_deleted,
    wr_precomp_i     => p1581_wr_precomp,
    wr_precomp_mode_i=> p1581_wr_precomp_mode,
    wbyte_data_i     => p1581_wbyte_data,
    wbyte_empty_i    => p1581_wbyte_empty,
    wbyte_rd_o       => p1581_wbyte_rd,
    wr_feed_o        => p1581_wr_feed,
    wr_lost_o        => p1581_wr_lost,
    wr_busy_o        => p1581_wr_busy,
    wr_ok_o          => p1581_wr_ok,

    st_media_ready_o => p1581_media_ready,
    st_index_o       => p1581_index,
    st_track0_o      => p1581_track0,
    st_wprot_o       => p1581_wprot,
    st_change_o      => p1581_change,
    st_motor_on_o    => p1581_motor_on,
    st_head_settled_o=> p1581_head_settled,
    st_head_cyl_o    => p1581_head_cyl,
    st_locked_o      => p1581_locked,

    diag_index_period_o => p1581_diag_idx_period,
    diag_index_width_o  => p1581_diag_idx_width,
    diag_index_edge_o   => p1581_diag_index_edge,
    diag_index_qual_o   => p1581_diag_index_qual,
    diag_last_gap_o     => p1581_diag_last_gap,
    diag_calc_crc_o     => p1581_diag_calc_crc,
    diag_stored_crc_o   => p1581_diag_stored_crc,
    diag_id_valid_o     => p1581_diag_id_valid,
    diag_id_crc_ok_o    => p1581_diag_id_crc_ok,
    diag_data_end_o     => p1581_diag_data_end,
    diag_data_crc_ok_o  => p1581_diag_data_crc_ok,
    diag_gap_error_o    => p1581_diag_gap_error,
    diag_runt_o         => p1581_diag_runt,
    diag_a1_candidate_o => p1581_diag_a1_candidate,
    diag_a1_reject_o    => p1581_diag_a1_reject,
    diag_a1_train_o     => p1581_diag_a1_train,
    diag_mark_fe_o      => p1581_diag_mark_fe,
    diag_mark_dam_o     => p1581_diag_mark_dam,
    diag_dam_unarmed_o  => p1581_diag_dam_unarmed,
    diag_match_id_o     => p1581_diag_match_id,
    diag_dam_miss_o     => p1581_diag_dam_miss,
    diag_est_o          => p1581_diag_est,
    diag_rd_phase_o     => p1581_diag_rd_phase,
    diag_step_phase_o   => p1581_diag_step_phase,
    diag_head_valid_o   => p1581_diag_head_valid,
    diag_head_dir_out_o => p1581_diag_head_dir,
    diag_rd_req_o        => p1581_diag_rd_req,
    diag_rd_req_op_o     => p1581_diag_rd_req_op,
    diag_rd_req_track_o  => p1581_diag_rd_req_trk,
    diag_rd_req_sector_o => p1581_diag_rd_req_sec,
    diag_rd_req_side_o   => p1581_diag_rd_req_side,
    diag_in_bits_o      => p1581_diag_in_bits,
    diag_out_bits_o     => p1581_diag_out_bits,

    diag_wr_phase_o        => p1581_diag_wr_phase,
    diag_wr_session_o      => p1581_diag_wr_session,
    diag_wr_feed_o         => p1581_diag_wr_feed,
    diag_wr_abort_o        => p1581_diag_wr_abort,
    diag_wr_prec_o         => p1581_diag_wr_prec,
    diag_wr_done_o         => p1581_diag_wr_done,
    diag_wr_last_o         => p1581_diag_wr_last,
    diag_wr_bytes_o        => p1581_diag_wr_bytes,
    diag_wr_first_margin_o => p1581_diag_wr_first_margin,
    diag_wr_trk_cyc_o      => p1581_diag_wr_trk_cyc,
    diag_wr_gate_o         => p1581_diag_wr_gate,
    diag_wr_gate_cyc_o     => p1581_diag_wr_gate_cyc,
    diag_wr_words_last_o   => open,
    diag_wr_underrun_o     => p1581_diag_wr_underrun,
    diag_wr_discard_o      => p1581_diag_wr_discard,
    diag_wr_gateopen_o     => open,
    diag_wr_wabort_o       => open,
    diag_wr_precnt_o       => p1581_diag_wr_precnt,
    diag_wr_pushfull_o     => p1581_diag_wr_pushfull,
    diag_wr_reason_o       => p1581_diag_wr_reason,
    diag_wr_reason_acc_o   => p1581_diag_wr_reason_acc,
    diag_wr_wflags_o       => p1581_diag_wr_wflags
  ); -- i_physical_1581_controller

f_wgate_o <= p1581_f_wgate;
f_wdata_o <= p1581_f_wdata;

p1581_fiforst_proc : process (clk_main_i)
begin
  if rising_edge(clk_main_i) then
    p1581_fiforst_m <= rst_sd_i;
    p1581_fiforst_s <= p1581_fiforst_m;
  end if;
end process p1581_fiforst_proc;

-- One sector deep: the controller can capture a whole field before fdc1772 may
-- present any of it, and an error result drains it unseen.
i_physical_1581_rdfifo : entity work.physical_1581_rdfifo
  generic map (
    G_AW => 9
  )
  port map (
    wr_clk_i   => clk_sd_i,
    wr_rst_i   => rst_sd_i,
    wr_en_i    => p1581_fifo_wr_en,
    wr_data_i  => p1581_fifo_wr_data,
    wr_full_o  => p1581_fifo_wr_full,
    wr_level_o => p1581_fifo_level,
    rd_clk_i   => clk_main_i,
    rd_rst_i   => p1581_fiforst_s,
    rd_en_i    => p1581_byte_rd_en,
    rd_data_o  => p1581_byte_data,
    rd_empty_o => p1581_byte_empty
  ); -- i_physical_1581_rdfifo

-- Four entries: fdc1772 pushes a byte per data-register write, the controller
-- pops them at disk pace. DRQ is "not full".
i_physical_1581_wfifo : entity work.physical_1581_wfifo
  generic map (
    G_DW => 8,
    G_AW => 2
  )
  port map (
    wr_clk_i   => clk_main_i,
    wr_rst_i   => p1581_fiforst_s,
    wr_en_i    => p1581_wr_byte_wr,
    wr_data_i  => p1581_wr_byte_data,
    wr_full_o  => p1581_wr_fifo_full,
    wr_level_o => open,
    rd_clk_i   => clk_sd_i,
    rd_rst_i   => rst_sd_i,
    rd_en_i    => p1581_wbyte_rd,
    rd_data_o  => p1581_wbyte_data,
    rd_empty_o => p1581_wbyte_empty,
    rd_level_o => p1581_wfifo_rd_level
  ); -- i_physical_1581_wfifo

-- The drive LED of a drive on the internal floppy shows physical activity, which
-- the diag device sees on its own.
img_drive_busy <= std_logic_vector(resize(unsigned((iec_drive_led and not phys_en_q) or cache_dirty), 2));

p1581_sync_proc : process (clk_sd_i)
begin
  if rising_edge(clk_sd_i) then
    img_busy_sd_m  <= img_drive_busy;
    img_busy_sd_s  <= img_busy_sd_m;
    p1581_dbg_sd_m <= p1581_dbg_tgl;
    p1581_dbg_sd_s <= p1581_dbg_sd_m;
  end if;
end process p1581_sync_proc;

-- Observational QNICE register bank (doc/1581_dd_debug_device.md in C64MEGA65).
-- The one writable word, WR_CTRL, overrides write precompensation.
i_physical_1581_diag : entity work.physical_1581_diag
  port map (
    clk_i             => clk_sd_i,
    rst_i             => rst_sd_i,

    st_media_ready_i  => p1581_media_ready,
    st_index_i        => p1581_index,
    st_track0_i       => p1581_track0,
    st_wprot_i        => p1581_wprot,
    st_change_i       => p1581_change,
    st_motor_on_i     => p1581_motor_on,
    st_head_settled_i => p1581_head_settled,
    st_locked_i       => p1581_locked,
    st_head_cyl_i     => p1581_head_cyl,

    rd_done_tgl_i     => p1581_rd_done_tgl,
    rd_result_i       => p1581_rd_result,
    rd_crc_err_i      => p1581_rd_crc_err,
    rd_rnf_i          => p1581_rd_rnf,
    rd_deleted_i      => p1581_rd_deleted,
    rd_c_i            => p1581_rd_c,
    rd_h_i            => p1581_rd_h,
    rd_r_i            => p1581_rd_r,
    rd_n_i            => p1581_rd_n,
    step_ack_tgl_i    => p1581_step_ack_tgl,

    diag_in_bits_i      => p1581_diag_in_bits,
    diag_out_bits_i     => p1581_diag_out_bits,
    diag_index_period_i => p1581_diag_idx_period,
    diag_index_width_i  => p1581_diag_idx_width,
    diag_last_gap_i     => p1581_diag_last_gap,
    diag_calc_crc_i     => p1581_diag_calc_crc,
    diag_stored_crc_i   => p1581_diag_stored_crc,
    diag_index_edge_i   => p1581_diag_index_edge,
    diag_index_qual_i   => p1581_diag_index_qual,
    diag_id_valid_i     => p1581_diag_id_valid,
    diag_id_crc_ok_i    => p1581_diag_id_crc_ok,
    diag_data_end_i     => p1581_diag_data_end,
    diag_data_crc_ok_i  => p1581_diag_data_crc_ok,
    diag_gap_error_i    => p1581_diag_gap_error,
    diag_est_i          => p1581_diag_est,
    diag_rd_phase_i     => p1581_diag_rd_phase,
    diag_step_phase_i   => p1581_diag_step_phase,
    diag_head_valid_i   => p1581_diag_head_valid,
    diag_head_dir_out_i => p1581_diag_head_dir,

    img_drive_busy_i    => img_busy_sd_s,

    rd_req_evt_i        => p1581_diag_rd_req,
    rd_req_op_i         => p1581_diag_rd_req_op,
    rd_req_track_i      => p1581_diag_rd_req_trk,
    rd_req_sector_i     => p1581_diag_rd_req_sec,
    rd_req_side_i       => p1581_diag_rd_req_side,

    dbg_lost_i          => p1581_dbg_sd_s(0),
    dbg_drain_i         => p1581_dbg_sd_s(1),
    dbg_staledone_i     => p1581_dbg_sd_s(2),
    dbg_busycmd_i       => p1581_dbg_sd_s(3),
    dbg_fin_i           => p1581_dbg_sd_s(4),
    dbg_pres_cnt_i      => unsigned(p1581_dbg_pres_cnt),
    fifo_level_i        => p1581_fifo_level,
    runt_i              => p1581_diag_runt,
    a1_candidate_i      => p1581_diag_a1_candidate,
    a1_span_reject_i    => p1581_diag_a1_reject,
    a1_train_i          => p1581_diag_a1_train,
    mark_fe_i           => p1581_diag_mark_fe,
    mark_dam_i          => p1581_diag_mark_dam,
    dam_unarmed_i       => p1581_diag_dam_unarmed,
    match_id_i          => p1581_diag_match_id,
    dam_miss_i          => p1581_diag_dam_miss,

    diag_wr_phase_i        => p1581_diag_wr_phase,
    diag_wr_session_i      => p1581_diag_wr_session,
    diag_wr_feed_i         => p1581_diag_wr_feed,
    diag_wr_abort_i        => p1581_diag_wr_abort,
    diag_wr_prec_i         => p1581_diag_wr_prec,
    diag_wr_done_i         => p1581_diag_wr_done,
    diag_wr_last_i         => p1581_diag_wr_last,
    diag_wr_bytes_i        => p1581_diag_wr_bytes,
    diag_wr_first_margin_i => p1581_diag_wr_first_margin,
    diag_wr_trk_cyc_i      => p1581_diag_wr_trk_cyc,
    diag_wr_gate_i         => p1581_diag_wr_gate,
    diag_wr_gate_cyc_i     => p1581_diag_wr_gate_cyc,
    diag_wr_underrun_i     => p1581_diag_wr_underrun,
    diag_wr_discard_i      => p1581_diag_wr_discard,
    diag_wr_precnt_i       => p1581_diag_wr_precnt,
    diag_wr_pushfull_i     => p1581_diag_wr_pushfull,
    diag_wr_reason_i       => p1581_diag_wr_reason,
    diag_wr_reason_acc_i   => p1581_diag_wr_reason_acc,
    diag_wr_wflags_i       => p1581_diag_wr_wflags,
    wr_busy_i              => p1581_wr_busy,
    wr_ok_i                => p1581_wr_ok,
    f_wgate_i              => p1581_f_wgate,
    f_wdata_i              => p1581_f_wdata,
    wr_pop_i               => p1581_wbyte_rd,
    wr_fifo_level_i        => p1581_wfifo_rd_level,
    dbg_wr_fin_i           => p1581_dbg_sd_s(5),
    dbg_wr_wp_i            => p1581_dbg_sd_s(6),
    dbg_wr_drop_i          => p1581_dbg_sd_s(7),
    wr_precomp_mode_o      => p1581_wr_precomp_mode,

    qnice_ce_i        => qnice_p1581_ce_i,
    qnice_we_i        => qnice_p1581_we_i,
    qnice_addr_i      => qnice_p1581_addr_i,
    qnice_data_i      => qnice_p1581_data_i,
    qnice_data_o      => qnice_p1581_data_o
  ); -- i_physical_1581_diag

vdrives_inst : entity work.vdrives
  generic map (
    VDNUM => G_VDNUM,
    -- 256 byte blocks. This is not the framework default of 512 and it matters: it matches
    -- hps_io #(.VDNUM(2), .BLKSZ(1)) in MiSTer's c128.sv. The largest request is
    -- SD_BLK_CNT_157X = 52, i.e. 53 * 256 = 13,568 bytes, safely below the 16,384 byte ceiling.
    BLKSZ => 1
  )
  port map (
    clk_qnice_i      => clk_sd_i,
    clk_core_i       => clk_main_i,
    reset_core_i     => not reset_core_n,
    reset_hard_i     => reset_hard_i,

    img_mounted_o    => iec_img_mounted,
    img_readonly_o   => iec_img_readonly,
    img_size_o       => iec_img_size,
    img_type_o       => iec_img_type,        -- 00=D64, 01=D71, 10=D81

    -- latched version of the strobed img_mounted_o, used to un-reset a drive
    drive_mounted_o  => vdrives_mounted,

    cache_dirty_o    => open,
    cache_flushing_o => open,

    sd_lba_i         => vd_sd_lba,
    sd_blk_cnt_i     => vd_sd_blk_cnt,
    sd_rd_i          => vd_sd_off,
    sd_wr_i          => vd_sd_off,
    sd_ack_o         => open,

    sd_buff_addr_o   => open,
    sd_buff_dout_o   => open,
    sd_buff_din_i    => vd_sd_buf_data_out,
    sd_buff_wr_o     => open,

    qnice_addr_i     => qnice_vd_addr_i,
    qnice_data_i     => qnice_vd_data_i,
    qnice_data_o     => qnice_vd_data_o,
    qnice_ce_i       => qnice_vd_ce_i,
    qnice_we_i       => qnice_vd_we_i
  ); -- vdrives_inst

-- The disk images straight from the SD card. Its state follows QNICE, not the core:
-- a reset of the C128 must not lose writes that are not on the card yet.
i_vd_sd : entity work.vd_sd
  generic map (
    G_VDNUM => G_VDNUM
  )
  port map (
    clk_i          => clk_sd_i,
    rst_i          => rst_sd_i,

    qnice_drive_i  => qnice_vdsd_drive_i,
    qnice_addr_i   => qnice_vdsd_addr_i,
    qnice_data_i   => qnice_vdsd_data_i,
    qnice_ce_i     => qnice_vdsd_ce_i,
    qnice_we_i     => qnice_vdsd_we_i,
    qnice_data_o   => qnice_vdsd_data_o,

    sd_lba_i       => vd_sd_lba,
    sd_blk_cnt_i   => vd_sd_blk_cnt,
    sd_rd_i        => iec_sd_rd,
    sd_wr_i        => iec_sd_wr,
    sd_ack_o       => iec_sd_ack,
    sd_buff_addr_o => iec_sd_buf_addr,
    sd_buff_dout_o => iec_sd_buf_data_in,
    sd_buff_wr_o   => iec_sd_buf_wr,
    sd_buff_din_i  => vd_sd_buf_data_out,

    dirty_o        => cache_dirty,
    ready_i        => iec_disk_ready,

    eng_want_o     => qnice_sd_want_o,
    eng_o          => qnice_sd_eng_o,
    eng_i          => qnice_sd_eng_i
  ); -- i_vd_sd

drives_dirty <= '1' when unsigned(cache_dirty)    /= 0 else '0';
drives_busy  <= '1' when unsigned(iec_drive_led)  /= 0 else '0';

-- Let the computer see the emulated drive's lines about half a microsecond late,
-- the way it sees a real drive on the cable.
--
-- The 1571 side of iec_drive changes a line one clk after its processor's write
-- cycle, and the 8502 above samples the bus 14 clks before the end of its own
-- cycle; nothing in between adds any delay. A real 1571's 6522 port chip, its
-- 7406 bus driver, the cable and the MEGA65 port's level shifters do, and the
-- two-bit fast loaders in GEOS and Wheels were tuned on that hardware with
-- margins of well under a microsecond.
--
-- Wheels' TURBO41 is the tightest: on a 50 Hz (PAL) machine the computer runs
-- 1.5% slower than the drive, so by the third of the four bit pairs in a byte
-- the drive is already 0.7 us ahead of where the loader counted it, and the next
-- pair can land on the bus up to 0.1 us before the computer samples the current
-- one. On the MEGA65 that showed as single-bit errors (bits 7 and 5 of a byte)
-- roughly once per 2 KB of a Wheels load, which left a damaged kernal that
-- crashed with "System error near $CA8B". A real 1571 on the physical port does
-- not show it because its reply arrives those few hundred nanoseconds later.
--
-- Sixteen clks at 31.5 MHz are 0.51 us. The late-side margins of the same
-- loaders (how long a pair stays on the bus after the computer samples it) are
-- 5 us and more, so the delay costs nothing there, and every handshake that
-- waits for the other side simply waits half a microsecond longer. All three
-- drive lines are delayed by the same amount, so the fast serial clock on the
-- service request line keeps its alignment with the data it clocks.
drv_iec_delay_proc : process (clk_main_i)
begin
  if rising_edge(clk_main_i) then
    drv_iec_clk_dly  <= drv_iec_clk_dly (drv_iec_clk_dly'high  - 1 downto 0) & drv_iec_clk_raw;
    drv_iec_data_dly <= drv_iec_data_dly(drv_iec_data_dly'high - 1 downto 0) & drv_iec_data_raw;
    drv_iec_srq_dly  <= drv_iec_srq_dly (drv_iec_srq_dly'high  - 1 downto 0) & drv_iec_srq_raw;
  end if;
end process drv_iec_delay_proc;

drv_iec_clk_o  <= drv_iec_clk_dly (drv_iec_clk_dly'high);
drv_iec_data_o <= drv_iec_data_dly(drv_iec_data_dly'high);
drv_iec_srq_o  <= drv_iec_srq_dly (drv_iec_srq_dly'high);

else generate
begin

  -- No emulated drives: release every IEC line so the wired-AND merge sees only the
  -- computer and whatever real hardware is on the physical port.
  drv_iec_clk_o  <= '1';
  drv_iec_data_o <= '1';
  drv_iec_srq_o  <= '1';

  drives_dirty   <= '0';
  drives_busy    <= '0';

  qnice_vd_data_o   <= (others => '0');
  qnice_vdsd_data_o <= (others => '0');
  qnice_sd_want_o   <= '0';
  qnice_sd_eng_o    <= (others => '0');
  drv_rom_req_o     <= '0';
  drv_rom_addr_o    <= (others => '0');

  -- internal floppy idle
  f_motora_o         <= '1';
  f_selecta_o        <= '1';
  f_side1_o          <= '1';
  f_stepdir_o        <= '1';
  f_step_o           <= '1';
  f_density_o        <= '1';
  f_wgate_o          <= '1';
  f_wdata_o          <= '1';
  qnice_p1581_data_o <= (others => '0');

end generate drives_gen;

end architecture synthesis;
