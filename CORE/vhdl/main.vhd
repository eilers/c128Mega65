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
      clk_main_i              : in  std_logic;  -- Main core clock (54 MHz)
      clk_vdc_i               : in  std_logic;  -- VDC clock (32 MHz)
      reset_soft_i            : in  std_logic;  -- Soft reset
      reset_hard_i            : in  std_logic;  -- Hard reset
      pause_i                 : in  std_logic;  -- Pause  

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

     -- C64 RAM: No address latching necessary and the chip can always be enabled
     ram_addr_o               : out unsigned(17 downto 0);    -- address bus (18 Bit!)
     ram_data_o               : out unsigned(7 downto 0);     -- RAM data out
     ram_we_o                 : out std_logic;                -- RAM write enable
     ram_data_i               : in unsigned(7 downto 0);      -- RAM data in
      sys_rom_addr_o           : out std_logic_vector(16 downto 0);
      sys_rom_data_i           : in  std_logic_vector(7 downto 0);

      -- C64 Expansion Port (aka Cartridge Port)
      cart_reset_i           : in  std_logic;
      cart_reset_o           : out std_logic;
      cart_dma_i             : in  std_logic;
      cart_game_i            : in  std_logic;
      cart_exrom_i           : in  std_logic;
      cart_nmi_i             : in  std_logic;
      cart_irq_i             : in  std_logic;
      cart_game_o            : out std_logic;
      cart_exrom_o           : out std_logic;
      cart_nmi_o             : out std_logic;
      cart_irq_o             : out std_logic;
      cart_roml_o            : out std_logic;
      cart_romh_o            : out std_logic;
      cart_io1_o             : out std_logic;
      cart_io2_o             : out std_logic;

      -- IEC serial bus interface to MEGA65 pins (active low at top level)
      iec_atn_n_o            : out std_logic;
      iec_clk_n_o            : out std_logic;
      iec_clk_n_i            : in  std_logic;
      iec_data_n_o           : out std_logic;
      iec_data_n_i           : in  std_logic;
      iec_srq_n_o            : out std_logic;
      iec_srq_n_i            : in  std_logic;

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
signal sysrom_cs         : std_logic;
signal sysrom_bank       : unsigned(4 downto 0);
signal sysrom_data       : unsigned(7 downto 0);
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
signal ps2_key           : std_logic_vector(10 downto 0) := (others => '0');
signal ps2_stb           : std_logic := '0';
signal video_ce_div      : std_logic := '0';

type key_state_t is array (0 to 79) of std_logic;
signal key_pressed : key_state_t := (others => '0');

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
drive_led_col_o <= x"00FF00" when cache_dirty = '0' else
                   x"FFFF00";

-- the drive led is on if either the C128 is writing to the virtual disk (cached in RAM)
-- or if the dirty cache is dirty and/orcurrently being flushed
drive_led_o <= '1';
video_timing_proc: process(clk_main_i)
begin
  if rising_edge(clk_main_i) then
    -- The framework expects a toggling CE for the 27 MHz pixel cadence on a 54 MHz clock.
    video_ce_div <= not video_ce_div;
  end if;
end process;
video_ce_o <= video_ce_div;
-- OVL CE is expected at 2x pixel cadence (effectively every 54 MHz cycle here).
video_ce_ovl_o <= '1';

-- Provide toggling blanking signals derived from sync until native blanking is wired.
video_hblank_o <= not video_hs_o;
video_vblank_o <= not video_vs_o;
video_red_o <= std_logic_vector(vic_r);
video_green_o <= std_logic_vector(vic_g);
video_blue_o <= std_logic_vector(vic_b);
-- cart_reset_o is low-active at the expansion port:
-- drive low while core reset is active, high otherwise.
cart_reset_o <= reset_core_n;
cart_roml_o <= core_roml;
cart_romh_o <= core_romh;
cart_io1_o <= core_ioe;
cart_io2_o <= core_iof;
cart_game_o <= '1';
cart_exrom_o <= '1';
cart_nmi_o <= not core_nmi_ack;
cart_irq_o <= '1';


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
-- Access to C64's RAM and hardware/simulated cartridge ROM
--------------------------------------------------------------------------------------------------
cpu_data_in_proc: process (all)
  begin
    ram_data <= x"00";

    -- We are emulating what is written here: https://www.c64-wiki.com/wiki/Reset_Button
    -- and avoid that the KERNAL ever sees the CBM80 signature during hard reset reset.
    -- But we cannot do it like on real hardware using the exrom signal because the
    -- MiSTer core is not supporting this.
    if hard_reset_n = '0' and ram_addr_o(15 downto 12) = x"8" and cold_start_done = '1' then
      ram_data <= x"00";
    -- C128 system ROM bank access: each bank is 4 KiB.
    elsif sysrom_cs = '1' then
      ram_data <= sysrom_data;
    -- TODO: Add REU support
    -- Standard access to the C64's RAM
    else
      ram_data <= ram_data_i;

    end if;
  end process;

-- RAM write enable also needs to check for chip enable
ram_we_o <= ram_ce and ram_we;
sys_rom_addr_o <= std_logic_vector(sysrom_bank) & std_logic_vector(ram_addr_o(11 downto 0));
sysrom_data <= unsigned(sys_rom_data_i);
joy_a <= '0' & (not joy_1_fire_n_i) & (not joy_1_right_n_i) & (not joy_1_left_n_i) &
         (not joy_1_down_n_i) & (not joy_1_up_n_i) & '0';
joy_b <= '0' & (not joy_2_fire_n_i) & (not joy_2_right_n_i) & (not joy_2_left_n_i) &
         (not joy_2_down_n_i) & (not joy_2_up_n_i) & '0';
audio_left_o <= signed(sid_audio_l(17 downto 2));
audio_right_o <= signed(sid_audio_r(17 downto 2));

--------------------------------------------------------------------------------------------------
-- Keyboard: Convert MEGA65 key scan stream to fpga64_keyboard's ps2_key event format.
--------------------------------------------------------------------------------------------------
keyboard_ps2_bridge : process(clk_main_i)
  variable idx : integer range 0 to 79;
  variable pressed_v : std_logic;
  variable valid_v : std_logic;
  variable ext_v : std_logic;
  variable scancode_v : std_logic_vector(7 downto 0);
  variable next_stb : std_logic;
begin
  if rising_edge(clk_main_i) then
    idx := kb_key_num_i;
    pressed_v := not kb_key_pressed_n_i;
    valid_v := '0';
    ext_v := '0';
    scancode_v := (others => '0');

    if reset_core_n = '0' then
      key_pressed <= (others => '0');
      ps2_stb <= '0';
      ps2_key <= (others => '0');
    elsif key_pressed(idx) /= pressed_v then
      key_pressed(idx) <= pressed_v;

      case idx is
        -- Main alphanumeric keys needed for BASIC interaction.
        when 1  => valid_v := '1'; scancode_v := x"5A"; -- Return
        when 8  => valid_v := '1'; scancode_v := x"26"; -- 3
        when 9  => valid_v := '1'; scancode_v := x"1D"; -- W
        when 10 => valid_v := '1'; scancode_v := x"1C"; -- A
        when 11 => valid_v := '1'; scancode_v := x"25"; -- 4
        when 12 => valid_v := '1'; scancode_v := x"1A"; -- Z
        when 13 => valid_v := '1'; scancode_v := x"1B"; -- S
        when 14 => valid_v := '1'; scancode_v := x"24"; -- E
        when 15 => valid_v := '1'; scancode_v := x"12"; -- Left Shift
        when 16 => valid_v := '1'; scancode_v := x"2E"; -- 5
        when 17 => valid_v := '1'; scancode_v := x"2D"; -- R
        when 18 => valid_v := '1'; scancode_v := x"23"; -- D
        when 19 => valid_v := '1'; scancode_v := x"36"; -- 6
        when 20 => valid_v := '1'; scancode_v := x"21"; -- C
        when 21 => valid_v := '1'; scancode_v := x"2B"; -- F
        when 22 => valid_v := '1'; scancode_v := x"2C"; -- T
        when 23 => valid_v := '1'; scancode_v := x"22"; -- X
        when 24 => valid_v := '1'; scancode_v := x"3D"; -- 7
        when 25 => valid_v := '1'; scancode_v := x"35"; -- Y
        when 26 => valid_v := '1'; scancode_v := x"34"; -- G
        when 27 => valid_v := '1'; scancode_v := x"3E"; -- 8
        when 28 => valid_v := '1'; scancode_v := x"32"; -- B
        when 29 => valid_v := '1'; scancode_v := x"33"; -- H
        when 30 => valid_v := '1'; scancode_v := x"3C"; -- U
        when 31 => valid_v := '1'; scancode_v := x"2A"; -- V
        when 32 => valid_v := '1'; scancode_v := x"46"; -- 9
        when 33 => valid_v := '1'; scancode_v := x"43"; -- I
        when 34 => valid_v := '1'; scancode_v := x"3B"; -- J
        when 35 => valid_v := '1'; scancode_v := x"45"; -- 0
        when 36 => valid_v := '1'; scancode_v := x"3A"; -- M
        when 37 => valid_v := '1'; scancode_v := x"42"; -- K
        when 38 => valid_v := '1'; scancode_v := x"44"; -- O
        when 39 => valid_v := '1'; scancode_v := x"31"; -- N
        when 40 => valid_v := '1'; scancode_v := x"55"; -- +
        when 41 => valid_v := '1'; scancode_v := x"4D"; -- P
        when 42 => valid_v := '1'; scancode_v := x"4B"; -- L
        when 43 => valid_v := '1'; scancode_v := x"4E"; -- -
        when 44 => valid_v := '1'; scancode_v := x"49"; -- .
        when 45 => valid_v := '1'; scancode_v := x"4C"; -- :
        when 47 => valid_v := '1'; scancode_v := x"41"; -- ,
        when 52 => valid_v := '1'; scancode_v := x"59"; -- Right Shift
        when 53 => valid_v := '1'; scancode_v := x"09"; -- =
        when 55 => valid_v := '1'; scancode_v := x"4A"; -- /
        when 56 => valid_v := '1'; scancode_v := x"16"; -- 1
        when 58 => valid_v := '1'; scancode_v := x"14"; -- Ctrl
        when 59 => valid_v := '1'; scancode_v := x"1E"; -- 2
        when 60 => valid_v := '1'; scancode_v := x"29"; -- Space
        when 62 => valid_v := '1'; scancode_v := x"15"; -- Q
        when 63 => valid_v := '1'; ext_v := '1'; scancode_v := x"69"; -- Run/Stop
        when 71 => valid_v := '1'; scancode_v := x"76"; -- Esc
        when 73 => valid_v := '1'; ext_v := '1'; scancode_v := x"75"; -- Cursor Up
        when 74 => valid_v := '1'; ext_v := '1'; scancode_v := x"6B"; -- Cursor Left
        when others => null;
      end case;

      if valid_v = '1' then
        next_stb := not ps2_stb;
        ps2_stb <= next_stb;
        ps2_key <= next_stb & pressed_v & ext_v & scancode_v;
      end if;
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

      -- Translate MEGA65 keyboard scans to the core's ps2_key event format.
      ps2_key       => ps2_key,
      kbd_reset     => not reset_core_n,
      shift_mod     => "00",
      azerty        => '0',
      cpslk_mode    => '0',
      sftlk_sense   => open,
      cpslk_sense   => open,
      d4080_sense   => open,
      -- keyboard interface: directly connect the CIA1
      -- cia1_pa_i     => cia1_pa_in,
      -- cia1_pa_o     => cia1_pa_out,
      -- cia1_pb_i     => cia1_pb_in,
      -- cia1_pb_o     => cia1_pb_out,

      noscr_sense  => open,
      go64         => '0',  -- Add this line: '0' for C128 mode

      -- Select C128's system ROM banks (boot0.rom)
      sysRom        => sysrom_cs,
      sysRomBank    => sysrom_bank,

      pause         => pause_i,
      pause_out     => open,      -- unused

      -- external memory
      ramAddr       => ram_addr_o,
      ramDin        => ram_data,
      ramDout       => ram_data_o,
      ramCE         => ram_ce,
      ramWE         => ram_we,
      ramDinFloat   => '1', -- Signalling that the Cartridge is in high impedance. ???

      io_cycle      => open, -- 1 when an external I/O accesss is happening
      ext_cycle     => open, -- 1 when a DMA access is happening (REU).
      refresh       => open, -- 1 when a refresh cycle is happening (Not relevant for us)

      cia_mode      => '1',  -- 0 - 6526 "old", 1 - 8521 "new"
      turbo_mode    => "000",

      -- VGA/SCART interface
      -- The hsync frequency is 15.64 kHz (period 63.94 us).
      -- The hsync pulse width is 12.69 us.
      ntscMode      => '0',
      vic_variant   => "00",     -- Add this line: "00" for 6569 (PAL-B)
      vicJailbars   => "00",      -- disable jailbars
      vicHsync      => video_hs_o,
      vicVsync      => video_vs_o,
      vicR          => vic_r,
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
      vdcInitRam    => '0',
      vdcPalette    => "0000",
      vdcDebug      => '0',

      -- cartridge port
      -- TODO: Add cartridge support
      game          => cart_game_i,    -- input: low active
      game_mmu      => open,           -- output: 
      exrom         => cart_exrom_i,   -- input: low active
      exrom_mmu     => open,           -- output
      io_rom        => core_io_rom,    -- input
      io_ext        => core_io_ext,    -- input
      io_data       => core_io_data,   -- input
      irq_n         => cart_irq_i,     -- input: low active
      nmi_n         => cart_nmi_i,     -- input
      nmi_ack       => core_nmi_ack,   -- output
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
      
      -- dma access
      dma_req       => cart_dma_i,
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
      -- TODO: See removal of the ps2_key and kbd_reset signals. 
      -- The C64 core added a keyboard controller.
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
      sid_ld_clk    => '0',
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

      -- IEC
      -- TODO: Add IEC support
      iec_srq_n_o   => iec_srq_n_o,
      iec_srq_n_i   => iec_srq_n_i,
      iec_clk_i     => iec_clk_n_i,
      iec_clk_o     => iec_clk_n_o,
      iec_atn_o     => iec_atn_n_o,
      iec_data_i    => iec_data_n_i,
      iec_data_o    => iec_data_n_o,

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
      d4080_sel     => '0', -- TODO: Force to 40 column mode
      c128_n        => open,
      z80_n         => open
    ); -- fpga64_sid_iec_inst



end architecture synthesis;
