-------------------------------------------------------------------------------------------------------------
-- Commodore 64 for MEGA65 (C64MEGA65)
--
-- Clock Generator using the Xilinx specific MMCME2_ADV:
--
--   MiSTer's Commodore 64 expects:
--      PAL:  31,527,778 MHz, this divided by 32 = 0,98525 MHz (C64 clock speed)
--            Additionally (PAL only) we use a 0.25% slower system clock for the HDMI flicker-fix
--      NTSC: @TODO
--
-- Note about Flicker-Free: The method used here is to seamlessly (i.e. without glitch)
-- switch automatically between two very close clock speeds. The switching is done based
-- on the feedback from the HDMI ascal'er and is done in mega65.vhd.
--
-- However, there is an alternative method possible, where only a single clock is used and
-- furthermore removes the dependency on the HDMI ascaler. Instead, it uses the "fine
-- phase shift" capability of the MMCM. This makes it possible to dynamically "bend" the
-- clock frequency, but only by a small amount. The calculations are as follows: Starting
-- from the MMCM "i_clk_c64_slow" the actual frequency is 31.44899285 MHz, whereas the
-- desired frequency is 31.449600 MHz.  Since the actual clock is too slow, we need to
-- "insert" extra clock cycles. We do this by occasionally shortening a clock cycle by a
-- small amount.  The number of clock cycles before we "insert" a complete extra clock
-- cycle is: 31.449600 / (31.449600 - 31.44899285) = 51799.  Given the configuration
-- values of the MMCM there is a total of 56*21.375 = 1197 units of "fine phase shift" in
-- each clock cycle.  Therefore, we need to remove one unit of phase shift every
-- 51799/1197 = 43.27 clock cycles.
--
-- An even better alternative is to cascade two MMCM's: To get from 100 MHz to 31.4496 MHz
-- we need a factor of 0.31449600. Written as a fraction that is 4914/15625 = 2*3^3*7*13 / 5^6.
-- We can factor this fraction into two factors: 2*3*13/5^3 = 0.624 and 3^2*7/5^3 = 0.504.
-- These two factors can we implemented as follows:
-- 0.504 : CLKFBOUT_MULT_F = 47.25, DIVCLK_DIVIDE = 5, CLKOUT0_DIVIDE_F = 18.75
-- 0.624 : CLKFBOUT_MULT_F = 19.50, DIVCLK_DIVIDE = 1, CLKOUT0_DIVIDE_F = 31.25
-- Note: It's necessary that the first MMCM is 0.504, to keep f_VCO of the second MMCM
-- within the range of 600 - 1200 MHz.
-- With the above approach we get the exact clock frequency required, and therefore no
-- longer need any dynamic shifting of phase.
--
-- Powered by MiSTer2MEGA65
-- MEGA65 port done by MJoergen and sy2002 in 2023 and licensed under GPL v3
-------------------------------------------------------------------------------------------------------------

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

library unisim;
  use unisim.vcomponents.all;

library xpm;
  use xpm.vcomponents.all;

entity clk is
  port (
    sys_clk_i    : in  std_logic;            -- expects 100 MHz

    -- switchable clock for the C64 core
    -- 00 = PAL, as close as possible to the C64's original clock:
    --           @TODO exact clock values for main and video here
    --
    -- 01 = PAL  HDMI flicker-fix that makes sure the C64 is synchronous with the 50 Hz PAL frequency
    --           This is 99.75% of the original system speed.
    --           @TODO exact clock values for main and video here
    --
    -- 10 = NTSC @TODO
    core_speed_i :     unsigned(1 downto 0); -- asynchronous

    main_clk_o   : out std_logic;
    main_rst_o   : out std_logic
  );
end entity;

architecture rtl of clk is

  -- MMCM signals
  signal main_fb_mmcm_orig  : std_logic;
  signal main_clk_mmcm_orig : std_logic;
  signal main_locked_orig   : std_logic;

  signal main_fb_mmcm_slow  : std_logic;
  signal main_clk_mmcm_slow : std_logic;
  signal main_locked_slow   : std_logic;

  signal main_clk_mmcm : std_logic;

begin

  ---------------------------------------------------------------------------------------
  -- Generate as-close-as-possible-to-the-original version of the C64 clock
  -- This has a frame rate of 31527777/(312*63*32) = 50.124 Hz
  ---------------------------------------------------------------------------------------
  i_clk_c64_orig: MMCME2_ADV
    generic map (
      BANDWIDTH            => "OPTIMIZED",
      CLKOUT4_CASCADE      => FALSE,
      COMPENSATION         => "ZHOLD",
      STARTUP_WAIT         => FALSE,
      CLKIN1_PERIOD        => 10.0,   -- INPUT @ 100 MHz
      REF_JITTER1          => 0.010,
      DIVCLK_DIVIDE        => 6,
      CLKFBOUT_MULT_F      => 56.750, -- 945.833 MHz
      CLKFBOUT_PHASE       => 0.000,
      CLKFBOUT_USE_FINE_PS => FALSE,
      CLKOUT0_DIVIDE_F     => 30.000, -- 31.5277777778 MHz
      CLKOUT0_PHASE        => 0.000,
      CLKOUT0_DUTY_CYCLE   => 0.500,
      CLKOUT0_USE_FINE_PS  => FALSE
    )
    port map (
      -- Output clocks
      CLKFBOUT     => main_fb_mmcm_orig,
      CLKOUT0      => main_clk_mmcm_orig,
      -- Input clock control
      CLKFBIN      => main_fb_mmcm_orig,
      CLKIN1       => sys_clk_i,
      CLKIN2       => '0',
      -- Tied to always select the primary input clock
      CLKINSEL     => '1',
      -- Ports for dynamic reconfiguration
      DADDR        => (others => '0'),
      DCLK         => '0',
      DEN          => '0',
      DI           => (others => '0'),
      DO           => open,
      DRDY         => open,
      DWE          => '0',
      -- Ports for dynamic phase shift
      PSCLK        => '0',
      PSEN         => '0',
      PSINCDEC     => '0',
      PSDONE       => open,
      -- Other control and status signals
      LOCKED       => main_locked_orig,
      CLKINSTOPPED => open,
      CLKFBSTOPPED => open,
      PWRDWN       => '0',
      RST          => '0'
    ); -- i_clk_c64_orig

  ---------------------------------------------------------------------------------------
  -- Generate a slightly slower version of the C64 clock
  -- This has a frame rate of 31448993/(312*63*32) = 49.999 Hz
  -- It's important that this rate is slightly *slower* than 50 Hz.
  ---------------------------------------------------------------------------------------
  i_clk_c64_slow: MMCME2_ADV
    generic map (
      BANDWIDTH            => "OPTIMIZED",
      CLKOUT4_CASCADE      => FALSE,
      COMPENSATION         => "ZHOLD",
      STARTUP_WAIT         => FALSE,
      CLKIN1_PERIOD        => 10.0,   -- INPUT @ 100 MHz
      REF_JITTER1          => 0.010,
      DIVCLK_DIVIDE        => 9,
      CLKFBOUT_MULT_F      => 60.500, -- 672.222 MHz
      CLKFBOUT_PHASE       => 0.000,
      CLKFBOUT_USE_FINE_PS => FALSE,
      CLKOUT0_DIVIDE_F     => 21.375, -- 31.448993 MHz
      CLKOUT0_PHASE        => 0.000,
      CLKOUT0_DUTY_CYCLE   => 0.500,
      CLKOUT0_USE_FINE_PS  => FALSE
    )
    port map (
      -- Output clocks
      CLKFBOUT     => main_fb_mmcm_slow,
      CLKOUT0      => main_clk_mmcm_slow,
      -- Input clock control
      CLKFBIN      => main_fb_mmcm_slow,
      CLKIN1       => sys_clk_i,
      CLKIN2       => '0',
      -- Tied to always select the primary input clock
      CLKINSEL     => '1',
      -- Ports for dynamic reconfiguration
      DADDR        => (others => '0'),
      DCLK         => '0',
      DEN          => '0',
      DI           => (others => '0'),
      DO           => open,
      DRDY         => open,
      DWE          => '0',
      -- Ports for dynamic phase shift
      PSCLK        => '0',
      PSEN         => '0',
      PSINCDEC     => '0',
      PSDONE       => open,
      -- Other control and status signals
      LOCKED       => main_locked_slow,
      CLKINSTOPPED => open,
      CLKFBSTOPPED => open,
      PWRDWN       => '0',
      RST          => '0'
    ); -- i_clk_c64_slow

  -- This is a glitch-free mux switching between the fast and the slow clock.
  -- The select signal is treated asynchronously to the input clocks.
  bufgmux_ctrl_inst: bufgmux_ctrl
    port map (
      i0 => main_clk_mmcm_orig, -- 1-bit input: clock input (s=0)
      i1 => main_clk_mmcm_slow, -- 1-bit input: clock input (s=1)
      s  => core_speed_i(0),    -- 1-bit input: clock select
      o  => main_clk_mmcm -- 1-bit output: clock output
    );

  -------------------------------------------------------------------------------------
  -- Output buffering
  -------------------------------------------------------------------------------------
  main_clk_bufg: BUFG
    port map (
      I => main_clk_mmcm,
      O => main_clk_o
    );

  -------------------------------------
  -- Reset generation
  -------------------------------------
  i_xpm_cdc_async_rst_main: xpm_cdc_async_rst
    generic map (
      RST_ACTIVE_HIGH => 1,
      DEST_SYNC_FF    => 6
    )
    port map (
      src_arst  => not(main_locked_orig and main_locked_slow), -- 1-bit input: Source reset signal.
      dest_clk  => main_clk_o,                                 -- 1-bit input: Destination clock.
      dest_arst => main_rst_o -- 1-bit output: src_arst synchronized to the destination clock domain.
        -- This output is registered.
    );

end architecture;
