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

entity main is
   generic (
      G_BOARD                 : string;                     -- Which MEGA65 revision are we running on
      G_VDNUM                 : natural                     -- amount of virtual drives
   );
   port (
      clk_main_i              : in  std_logic;  -- Main core clock (~31.53 MHz PAL)
      clk_vdc_i               : in  std_logic;  -- VDC clock (32.000 MHz)
      reset_soft_i            : in  std_logic;  -- Soft reset
      reset_hard_i            : in  std_logic;  -- Hard reset
      pause_i                 : in  std_logic;  -- Pause

      -- MiSTer core main clock speed:
      -- Make sure you pass very exact numbers here, because they are used for avoiding clock drift at derived clocks
      clk_main_speed_i        : in  natural;

      -- Help-menu selections (main clock domain; bit index = OPTM_ITEMS line)
      osm_control_i           : in  std_logic_vector(255 downto 0);

      -- Video output (synchronous to VIC or VDC clock; see video_select_vdc_o)
      video_ce_o              : out std_logic;
      video_ce_ovl_o          : out std_logic;
      video_red_o             : out std_logic_vector(7 downto 0);
      video_green_o           : out std_logic_vector(7 downto 0);
      video_blue_o            : out std_logic_vector(7 downto 0);
      video_vs_o              : out std_logic;
      video_hs_o              : out std_logic;
      video_hblank_o          : out std_logic;
      video_vblank_o          : out std_logic;
      -- '1' = HDMI shows VDC (video_* on clk_vdc); '0' = VIC (video_* on clk_main)
      video_select_vdc_o      : out std_logic;

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
      pot2_y_i                : in  std_logic_vector(7 downto 0)
   );
end entity main;

architecture synthesis of main is

-- signals for RAM
signal ram_ce   : std_logic;
signal ram_we   : std_logic;
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
signal vic_pixel_ce_d    : std_logic := '0';

signal vic_r_reg         : unsigned(7 downto 0) := (others => '0');
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
signal vdc_ce            : std_logic;
signal vdc_ce_d          : std_logic := '0';
signal vdc_r_reg         : unsigned(7 downto 0) := (others => '0');
signal vdc_g_reg         : unsigned(7 downto 0) := (others => '0');
signal vdc_b_reg         : unsigned(7 downto 0) := (others => '0');

signal sel_vdc           : std_logic := '0';
signal sel_vdc_d         : std_logic := '0';
signal video_switching   : std_logic := '0';
signal video_switch_cnt  : natural range 0 to 65535 := 0;
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

-- Caps Lock (MEGA65 key 72) drives the C128 40/80-column sense line as a level.
signal d4080_sel_s       : std_logic := '1';
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
-- pulse is at least 32 clock cycles long. Currently (see mega65.vhd) there are two sources that
-- trigger reset_soft_i: The M2M reset manager and sw_cartridge_wrapper. Both are ensuring that
-- the rest pulse is at least 32 clock cycles long.
--
-- A reset that is coming from a hardware cartridge via cart_reset_i (which is low active) is treated
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

-- Hardware Expansion Port
signal exp_port_hw          : std_logic;   -- '1' = the physical slot is in use
signal cart_roml_n          : std_logic;
signal cart_romh_n          : std_logic;
signal cart_io1_n           : std_logic;
signal cart_io2_n           : std_logic;
signal cart_nmi_n           : std_logic;
signal cart_irq_n           : std_logic;
signal cart_exrom_n         : std_logic;
signal cart_game_n          : std_logic;
signal data_from_cart       : unsigned(7 downto 0);
-- What the C128 itself puts on the data bus. Kept separate from ram_data (which may carry
-- data_from_cart) so that driving the cartridge's data bus cannot create a combinational loop.
signal machine_data         : unsigned(7 downto 0);
-- Low active reset request towards the cartridge, from the core's point of view
signal cart_reset_n_int     : std_logic;

-- Hardware Expansion Port: Handle specifics of certain cartridges
constant C_EF3_RESET_LEN : natural := 7; -- measured in phi2 cycles
signal cart_reset_counter : natural range 0 to C_EF3_RESET_LEN := 0;
signal cart_res_flckr_ign : natural range 0 to 2 := 0; -- avoid a short cart_reset_o after cart_reset_counter reached zero
signal cart_is_an_EF3     : std_logic;

-- Simulated IEC drives
-- TODO: I only added the minimum signals that I might need for a first start.
-- signal cache_dirty : std_logic_vector(G_VDNUM - 1 downto 0);
signal prevent_reset : std_logic;
signal cache_dirty   : std_logic; -- TODO: Hack!

-- Core's IEC serial bus line levels (fpga64_sid_iec convention: '1' = line released/high,
-- '0' = line asserted/low; srq is active low: '0' = asserted).
signal core_iec_clk_o   : std_logic;
signal core_iec_data_o  : std_logic;
signal core_iec_atn_o   : std_logic;
signal core_iec_srq_n_o : std_logic;

-- TODO: Add reu and rtc support

begin

-- prevent data corruption by not allowing a soft reset to happen while the cache is still dirty
-- since we can have more than one cache that might be dirty, we convert the std_logic_vector of length G_VDNUM
-- into an unsigned and check for zero
-- TODO: Add cache_dirty support when virtual drives are implemented
cache_dirty <= '0';
prevent_reset <= '0'; -- when unsigned(cache_dirty) = 0 else '1';

-- Active-CPU indicator for the boot simulation ('0' = Z80, '1' = 8502).
boot_z80_n_o <= core_z80_n;

-- Drive LED: virtual disk drives are not implemented yet, so there is no activity to show.
drive_led_o     <= '0';
drive_led_col_o <= x"00FF00";

--------------------------------------------------------------------------------------------------
-- Video Out select (MiSTer status[106:105] / auto_config): Follow 40/80, force VIC, force VDC.
-- Follow uses Caps Lock -> d4080_sel ('1'=40-col/VIC, '0'=80-col/VDC).
--------------------------------------------------------------------------------------------------
sel_vdc <= '1' when osm_control_i(C_MENU_VIDEO_VDC) = '1' else
           '0' when osm_control_i(C_MENU_VIDEO_VIC) = '1' else
           not d4080_sel_s;

video_select_vdc_o <= sel_vdc;

vic_jailbars <= "11" when osm_control_i(C_MENU_JAILBARS_HIGH)   = '1' else
                "10" when osm_control_i(C_MENU_JAILBARS_MEDIUM) = '1' else
                "01" when osm_control_i(C_MENU_JAILBARS_LOW)    = '1' else
                "00";

-- VIC path: sample on enablePixel; blanking via M2M/C64 video_sync.
vic_pixel_sample_proc : process (clk_main_i)
begin
  if rising_edge(clk_main_i) then
    vic_pixel_ce_d <= vic_pixel_ce;
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

-- VDC path: C128 MiSTer video_sync (centered PAL/NTSC shifts) + ~16 MHz CE.
video_sync_vdc : entity work.video_sync_c128
  port map (
    reset       => reset_soft_i or reset_hard_i,
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
    ce          => vdc_ce
  );

vdc_pixel_sample_proc : process (clk_vdc_i)
begin
  if rising_edge(clk_vdc_i) then
    vdc_ce_d <= vdc_ce;
    if vdc_ce = '1' then
      vdc_r_reg <= vdc_r;
      vdc_g_reg <= vdc_g;
      vdc_b_reg <= vdc_b;
    end if;
  end if;
end process vdc_pixel_sample_proc;

-- Blank briefly when HDMI source changes (video_clk BUFGMUX switches with sel_vdc).
video_switch_blank_proc : process (clk_main_i)
begin
  if rising_edge(clk_main_i) then
    sel_vdc_d <= sel_vdc;
    if sel_vdc /= sel_vdc_d then
      video_switching  <= '1';
      video_switch_cnt <= 65535;
    elsif video_switch_cnt /= 0 then
      video_switch_cnt <= video_switch_cnt - 1;
    else
      video_switching <= '0';
    end if;
  end if;
end process video_switch_blank_proc;

-- Native-domain mux: VIC on main_clk, VDC on vdc_clk. mega65 BUFGMUXes video_clk to match.
video_ce_o     <= vdc_ce when sel_vdc = '1' else vic_pixel_ce;
video_ce_ovl_o <= (vdc_ce or vdc_ce_d) when sel_vdc = '1' else (vic_pixel_ce or vic_pixel_ce_d);
video_hs_o     <= vdc_hs_out when sel_vdc = '1' else vic_hs_out;
video_vs_o     <= vdc_vs_out when sel_vdc = '1' else vic_vs_out;
video_hblank_o <= vdc_hblank when sel_vdc = '1' else vic_hblank;
video_vblank_o <= vdc_vblank when sel_vdc = '1' else vic_vblank;
video_red_o    <= (others => '0') when video_switching = '1' else
                  std_logic_vector(vdc_r_reg) when sel_vdc = '1' else std_logic_vector(vic_r_reg);
video_green_o  <= (others => '0') when video_switching = '1' else
                  std_logic_vector(vdc_g_reg) when sel_vdc = '1' else std_logic_vector(vic_g_reg);
video_blue_o   <= (others => '0') when video_switching = '1' else
                  std_logic_vector(vdc_b_reg) when sel_vdc = '1' else std_logic_vector(vic_b_reg);
--------------------------------------------------------------------------------------------------
-- Expansion Port (aka Cartridge Port): real cartridges in the MEGA65's slot
--------------------------------------------------------------------------------------------------
-- The MEGA65's slot is electrically a C64 expansion port, so it accepts both C64 cartridges
-- (which pull EXROM and/or GAME low and thereby make the C128 come up in C64 mode) and C128
-- cartridges (which leave both released and are found by the C128 boot ROM as External Function
-- ROM at $8000, i.e. through the same ROML line).
--
-- Nothing here is registered: the C128 bus and the pins are in the same clock domain
-- (clk_main_i) and a cartridge expects to see address, data and the chip selects settle
-- within the PHI2 cycle they belong to, so an extra pipeline stage is not an option.
exp_port_hw <= osm_control_i(C_MENU_EXP_PORT_HW);

-- Low-active chip selects as the expansion port expects them. The core reports these accesses
-- active high. ROMH covers both the CPU's access to $A000-$BFFF / $E000-$FFFF and the VIC-II's
-- Ultimax character fetch, which a cartridge cannot tell apart.
cart_roml_n <= not core_roml;
cart_romh_n <= (not core_romh) and (not core_umax_romh);
cart_io1_n  <= not core_ioe;
cart_io2_n  <= not core_iof;

-- What the cartridge tells the C128. Forced to the inactive level while the slot is unused so
-- that an empty slot can never inject an interrupt or map a phantom cartridge into the address
-- space.
cart_game_n  <= cart_game_i  when exp_port_hw = '1' else '1';
cart_exrom_n <= cart_exrom_i when exp_port_hw = '1' else '1';
cart_irq_n   <= cart_irq_i   when exp_port_hw = '1' else '1';
cart_nmi_n   <= cart_nmi_i   when exp_port_hw = '1' else '1';

-- Low-active reset request towards the cartridge. This tracks the INTERNAL reset request only,
-- never reset_core_n: a cartridge that pulls RESET wants to reset the C128, not itself, and
-- routing reset_core_n back out closes a purely combinational self-latching loop through the pin
-- (reset_core_n -> cart_reset_o -> pin -> cart_reset_i -> reset_core_n) whose settling at
-- reset release depends on routing delay, i.e. a placement-dependent, STA-invisible boot lottery.
cart_reset_n_int <= '0' when (pwrup_reset_cnt /= 0 or reset_core_int_n = '0') else '1';

handle_hardware_expansion_proc : process (all)
begin
   -- Tri-state everything we can directly control. As long as we do not support cartridges
   -- that can become bus master, the C128 owns address, data and the control lines.
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
      cart_rw_o       <= not ram_we;
      cart_phi2_o     <= core_phi2;
      cart_dotclock_o <= vic_pixel_ce;

      -- BA is held high. Handing the cartridge the core's real bus arbitration state was tried
      -- in the C64 core and reduced compatibility rather than improving it (the Kung Fu Flash
      -- stopped working altogether).
      cart_ba_o       <= '1';

      -- Address bus. Only the low 16 bits of the core's address are a CPU/VIC address; bits
      -- 17..16 are the C128's RAM bank select and must not leave the machine.
      cart_addr_oe_o  <= '1';
      if core_umax_romh = '0' then
         cart_a_o     <= core_ram_addr(15 downto 0);
      else
         -- Ultimax mode with the VIC on the bus: per "The PLA Dissected", A12..A15 are pulled up
         -- by RP4 whenever the VIC has the bus, so a cartridge sees them as %1111.
         cart_a_o     <= "11" & core_ram_addr(13 downto 0);
      end if;

      -- Data bus: turn it into an input while the cartridge answers a read, drive it otherwise,
      -- so that the CPU can also write to the cartridge (bank switching).
      if ram_we = '0' and (cart_roml_n = '0' or cart_romh_n = '0' or cart_io1_n = '0' or cart_io2_n = '0') then
         cart_data_oe_o <= '0';  -- input
         data_from_cart <= cart_d_i;
      else
         cart_data_oe_o <= '1';  -- output
         if ram_we = '0' then
            cart_d_o    <= machine_data;
         else
            cart_d_o    <= core_ram_data_out;
         end if;
      end if;
   end if;
end process handle_hardware_expansion_proc;

-- Route the cartridge back into the C128
handle_cores_expansion_port_signals_proc : process (all)
begin
   core_game_n  <= '1';
   core_exrom_n <= '1';
   core_io_rom  <= '0';
   core_io_ext  <= '0';
   core_io_data <= x"FF";
   core_irq_n   <= '1';
   core_nmi_n   <= core_nmi_n_s;    -- RESTORE key, see restore_nmi

   if exp_port_hw = '1' then
      core_game_n  <= cart_game_n;
      core_exrom_n <= cart_exrom_n;
      core_irq_n   <= cart_irq_n;
      core_nmi_n   <= cart_nmi_n and core_nmi_n_s;
      -- Let the cartridge win every $DExx/$DFxx read. io_rom stays '0' because that would map
      -- the I/O windows into cartridge ROM held in the core's own memory, which only applies to
      -- emulated cartridges.
      core_io_ext  <= core_ioe or core_iof;
      core_io_data <= data_from_cart;
   end if;
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
      ram_we_i       => ram_we,
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
         if cart_is_an_EF3 = '1' and ram_we = '1' and cart_io1_n = '0' and core_ram_addr(15 downto 0) = x"DE0F" then
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
iec_reset_n_o <= reset_core_n;            -- reset attached real drives together with the core
iec_atn_n_o   <= core_iec_atn_o;          -- push-pull: '0' = ATN asserted (low) on the bus
iec_clk_n_o   <= '0';
iec_clk_en_o  <= not core_iec_clk_o;      -- pull CLK low while the core asserts it (core = '0')
iec_data_n_o  <= '0';
iec_data_en_o <= not core_iec_data_o;     -- pull DATA low while the core asserts it (core = '0')
iec_srq_n_o   <= '0';
iec_srq_en_o  <= not core_iec_srq_n_o;    -- srq is active low: assert ('0') -> enable the driver


--------------------------------------------------------------------------------------------------
-- Hard reset
--------------------------------------------------------------------------------------------------

hard_reset_proc: process (clk_main_i)
  begin
    if rising_edge(clk_main_i) then
      if reset_soft_i = '1' or reset_hard_i = '1' or cart_reset_counter /= 0 then
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
    elsif cart_reset_i = '0' and prevent_reset = '0' and exp_port_hw = '1' then
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
    -- through ramDin, which is exactly this signal.
    elsif exp_port_hw = '1' and (cart_roml_n = '0' or cart_romh_n = '0') then
      ram_data <= data_from_cart;
    else
      ram_data <= machine_data;
    end if;
  end process;

-- MiSTer exposes ramWE/ramCE separately; do not AND them (Z80 latch writes miss CE).
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
capslock_tracker : process(clk_main_i)
begin
  if rising_edge(clk_main_i) then
    if reset_core_n = '0' then
      capslock_engaged_n <= '1';
    elsif kb_key_num_i = 72 then
      capslock_engaged_n <= kb_key_pressed_n_i;
    end if;
  end if;
end process;

-- capslock_engaged_n is the raw low-active scan level: released = '1' = 40 col (power-on default).
d4080_sel_s <= capslock_engaged_n;

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
      -- Reads answered by a hardware cartridge are substituted into ramDin (see cpu_data_in_proc),
      -- so from the bus logic's point of view the data bus is never floating.
      ramDinFloat   => '0',

      io_cycle      => open, -- 1 when an external I/O accesss is happening
      ext_cycle     => open, -- 1 when a DMA access is happening (REU).
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
      freeze_key  => open,
      mod_key     => open,
      tape_play   => open,
      
      -- The expansion port's /DMA line (cart_dma_i) is deliberately ignored: dma_req is the
      -- REU-style bus-master interface, and asserting it from a floating pin would freeze the
      -- Z80. Cartridges that need to become bus master are therefore not supported yet.
      dma_req       => '0',
      dma_cycle     => open,
      dma_addr      => open,
      dma_dout      => open,
      dma_din       => open,
      dma_we        => '0',
      irq_ext_n     => '1',


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
      iec_srq_n_i   => iec_srq_n_i,
      iec_clk_i     => iec_clk_n_i,
      iec_clk_o     => core_iec_clk_o,
      iec_atn_o     => core_iec_atn_o,
      iec_data_i    => iec_data_n_i,
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
      d4080_sel     => d4080_sel_s, -- Caps Lock acts as the 40/80-column key (see d4080_sel_s)
      c128_n        => core_c128_n,
      z80_n           => core_z80_n,
      z80_we_o        => open,
      dbg_vic_has_bus_o => open,
      dbg_enable_vic_o  => open,
      dbg_aec_o         => open,
      dbg_vicdi_o       => open
    ); -- fpga64_sid_iec_inst



end architecture synthesis;
