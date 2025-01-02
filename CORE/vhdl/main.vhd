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
      G_VDNUM                 : natural                     -- amount of virtual drives
   );
   port (
      clk_main_i              : in  std_logic;
      clk_vdc_i               : in  std_logic;
      reset_soft_i            : in  std_logic;
      reset_hard_i            : in  std_logic;
      pause_i                 : in  std_logic;

      -- MiSTer core main clock speed:
      -- Make sure you pass very exact numbers here, because they are used for avoiding clock drift at derived clocks
      clk_main_speed_i        : in  natural;

      -- Video output
      video_ce_o              : out std_logic;
      video_ce_ovl_o          : out std_logic;
      video_red_o             : out std_logic_vector(7 downto 0);
      video_green_o           : out std_logic_vector(7 downto 0);
      video_blue_o            : out std_logic_vector(7 downto 0);
      video_vs_o              : out std_logic;
      video_hs_o              : out std_logic;
      video_hblank_o          : out std_logic;
      video_vblank_o          : out std_logic;

      -- Audio output (Signed PCM)
      audio_left_o            : out signed(15 downto 0);
      audio_right_o           : out signed(15 downto 0);

      -- Drive led (color is RGB)
      drive_led_o             : out std_logic;
      drive_led_col_o         : out std_logic_vector(23 downto 0);

      -- C64 Expansion Port (aka Cartridge Port)
      cart_reset_i           : in  std_logic;
      cart_reset_o           : out std_logic;

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
signal core_nmi_n           : std_logic;
signal core_nmi_ack         : std_logic;
signal core_irq_n           : std_logic;
signal core_dma             : std_logic;
signal core_exrom_n         : std_logic;
signal core_game_n          : std_logic;
signal core_umax_romh       : std_logic;
signal core_io_rom          : std_logic;
signal core_io_ext          : std_logic;
signal core_io_data         : unsigned(7 downto 0);
signal core_dotclk          : std_logic;
signal core_phi2            : std_logic;
signal core_phi2_prev       : std_logic;
signal cartridge_bank_raddr : std_logic_vector(24 downto 0);

-- Hardware Expansion Port (aka Cartridge Port)
signal cart_roml_n    : std_logic;
signal cart_romh_n    : std_logic;
signal cart_io1_n     : std_logic;
signal cart_io2_n     : std_logic;
signal cart_nmi_n     : std_logic;
signal cart_irq_n     : std_logic;
signal cart_dma_n     : std_logic;
signal cart_exrom_n   : std_logic;
signal cart_game_n    : std_logic;
signal data_from_cart : unsigned(7 downto 0);

-- Hardware Expansion Port: Handle specifics of certain cartridges
constant C_EF3_RESET_LEN : natural := 7; -- measured in phi2 cycles
signal cart_reset_counter : natural range 0 to C_EF3_RESET_LEN := 0;
signal cart_res_flckr_ign : natural range 0 to 2; -- avoid a short cart_reset_o after cart_reset_counter reached zero
signal cart_is_an_EF3     : std_logic;

-- Simulated IEC drives
-- TODO: I only added the minimum signals that I might need for a first start.
-- signal cache_dirty : std_logic_vector(G_VDNUM - 1 downto 0);
signal prevent_reset : std_logic;
signal cache_dirty   : std_logic; -- TODO: Hack!

-- TODO: Add reu and rtc support

begin

-- prevent data corruption by not allowing a soft reset to happen while the cache is still dirty
-- since we can have more than one cache that might be dirty, we convert the std_logic_vector of length G_VDNUM
-- into an unsigned and check for zero
-- TODO: Add cache_dirty support when virtual drives are implemented
cache_dirty <= '0';
prevent_reset <= '0'; -- when unsigned(cache_dirty) = 0 else '1';

-- the color of the drive led is green normally, but it turns yellow
-- when the cache is dirty and/or currently being flushed
drive_led_col_o <= x"00FF00" when unsigned(cache_dirty) = 0 else
                   x"FFFF00";

-- the drive led is on if either the C128 is writing to the virtual disk (cached in RAM)
-- or if the dirty cache is dirty and/orcurrently being flushed
drive_led_o <= '1';


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

    -- cart_reset_i becomes cart_reset_o as soon as cart_reset_oe_o = '1', and the latter one becomes '1' as soon
    -- as reset_core_int_n = '0' so we need to ignore cart_reset_i in this case
    if reset_core_int_n = '0' then
      reset_core_n <= '0';
    elsif cart_reset_i = '0' and prevent_reset = '0' then
      reset_core_n <= '0';
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
-- MiSTer Commodore 64 core / main machine
--------------------------------------------------------------------------------------------------
fpga64_sid_iec_inst: entity work.fpga64_sid_iec
    port map (
      clk32         => clk_main_i,
      -- clk32_speed   => clk_main_speed_i, TODO: remove CORE_CLK_SPEED? 
      clk_vdc       => clk_vdc_i,
      reset_n       => reset_core_n,

      -- Select C64's ROM: 0=Custom, 1=Standard, 2=GS, 3=Japan
      bios          => c64_rom_i,

      pause         => pause_i,
      pause_out     => c64_pause,      -- unused

      -- keyboard interface: directly connect the CIA1
      cia1_pa_i     => cia1_pa_in,
      cia1_pa_o     => cia1_pa_out,
      cia1_pb_i     => cia1_pb_in,
      cia1_pb_o     => cia1_pb_out,

      -- external memory
      ramAddr       => c64_ram_addr_o,
      ramDin        => c64_ram_data,
      ramDout       => c64_ram_data_o,
      ramCE         => c64_ram_ce,
      ramWE         => c64_ram_we,

      io_cycle      => open,
      ext_cycle     => sim_ext_cycle,
      refresh       => open,

      cia_mode      => c64_cia_ver_i,  -- 0 - 6526 "old", 1 - 8521 "new"
      turbo_mode    => "00",
      turbo_speed   => "00",

      -- VGA/SCART interface
      -- The hsync frequency is 15.64 kHz (period 63.94 us).
      -- The hsync pulse width is 12.69 us.
      ntscMode      => c64_ntsc_i,
      hsync         => vga_hs,
      vsync         => vga_vs,
      r             => vga_red,
      g             => vga_green,
      b             => vga_blue,

      -- cartridge port
      game          => core_game_n,    -- input: low active
      exrom         => core_exrom_n,   -- input: low active
      io_rom        => core_io_rom,    -- input
      io_ext        => core_io_ext,    -- input
      io_data       => core_io_data,   -- input
      irq_n         => core_irq_n,     -- input: low active
      nmi_n         => core_nmi_n,     -- input
      nmi_ack       => core_nmi_ack,   -- output
      romL          => core_roml,      -- output. CPU access to 0x8000-0x9FFF
      romH          => core_romh,      -- output. CPU access to 0xA000-0xBFFF or 0xE000-0xFFFF (ultimax)
      UMAXromH      => core_umax_romh, -- output
      IOE           => core_ioe,       -- output. aka IO1. CPU access to 0xDExx
      IOF           => core_iof,       -- output. aka IO2. CPU access to 0xDFxx
      dotclk        => core_dotclk,    -- output
      phi0          => open,           -- output
      phi2          => core_phi2,      -- output
      --         freeze_key  => open,
      --         mod_key     => open,
      --         tape_play   => open,
      -- dma access
      dma_req       => core_dma,
      dma_cycle     => reu_dma_cycle,
      dma_addr      => unsigned(reu_dma_addr),
      dma_dout      => unsigned(reu_dma_dout),
      dma_din       => reu_dma_din,
      dma_we        => reu_dma_we,
      irq_ext_n     => not reu_irq,

      -- paddle interface
      pot1          => pot1_x_i,
      pot2          => pot1_y_i,
      pot3          => pot2_x_i,
      pot4          => pot2_y_i,

      -- SID
      audio_l       => c64_sid_l,
      audio_r       => c64_sid_r,
      sid_filter    => "11",           -- filter enable = true for both SIDs, low bit = left SID
      sid_ver       => c64_sid_ver_i,  -- SID version, 0=6581, 1=8580, low bit = left SID
      sid_mode      => c64_sid_port_i, -- Right SID Port: 0=same as left, 1=DE00, 2=D420, 3=D500, 4=DF00
      sid_cfg       => "0000",         -- filter type: 0=Default, 1=Custom 1, 2=Custom 2, 3=Custom 3, lower two bits = left SID

      -- mechanism for loading custom SID filters: not supported, yet
      sid_ld_clk    => '0',
      sid_ld_addr   => "000000000000",
      sid_ld_data   => x"0000",
      sid_ld_wr     => '0',

      -- User Port: Unused inputs need to be high
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

      -- IEC
      iec_clk_i     => c64_iec_clk_in and hw_iec_clk_n_in,
      iec_clk_o     => c64_iec_clk_out,
      iec_atn_o     => c64_iec_atn_out,
      iec_data_i    => c64_iec_data_in and hw_iec_data_n_in,
      iec_data_o    => c64_iec_data_out,

      -- Cassette drive
      cass_write    => cass_write,     -- output
      cass_motor    => cass_motor,     -- output
      cass_sense    => cass_rtc,       -- input
      cass_read     => '1',            -- default is '1' according to MiSTer's c1530.vhd

      -- Access custom Kernal: C64's Basic and DOS
      c64rom_clk_i  => c64_clk_sd_i,
      c64rom_we_i   => c64rom_we_i,
      c64rom_addr_i => c64rom_addr_i,
      c64rom_data_i => c64rom_data_i,
      c64rom_data_o => c64rom_data_o
    ); -- fpga64_sid_iec_inst



end architecture synthesis;
