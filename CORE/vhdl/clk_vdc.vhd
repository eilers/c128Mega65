-- VDC clock generator: exact 32.000 MHz from 100 MHz (matches MiSTer pll_vdc).
-- Integer path: VCO = 100*8 = 800 MHz; CLKOUT0 = 800/25 = 32.000 MHz
--
-- outclk_0     : BUFG-buffered clock for the VDC chip / core logic
-- outclk_raw_o : MMCM CLKOUT before BUFG — for video BUFGMUX only

library IEEE;
  use IEEE.STD_LOGIC_1164.all;
library UNISIM;
  use UNISIM.VCOMPONENTS.all;

entity vdc_clk is
  port (
    refclk       : in  std_logic;
    outclk_0     : out std_logic;
    outclk_raw_o : out std_logic;
    locked       : out std_logic
  );
end entity;

architecture rtl of vdc_clk is
  signal clkfbout : std_logic;
  signal clk_mmcm : std_logic;
  signal locked_i : std_logic;
begin

  mmcm_adv_inst: MMCME2_ADV
    generic map (
      BANDWIDTH          => "OPTIMIZED",
      CLKOUT4_CASCADE    => FALSE,
      COMPENSATION       => "ZHOLD",
      STARTUP_WAIT       => FALSE,
      DIVCLK_DIVIDE      => 1,
      CLKFBOUT_MULT_F    => 8.0,   -- 100MHz * 8 = 800MHz VCO
      CLKFBOUT_PHASE     => 0.000,
      CLKOUT0_DIVIDE_F   => 25.0,  -- 800MHz / 25 = 32.000MHz
      CLKOUT0_PHASE      => 0.000,
      CLKOUT0_DUTY_CYCLE => 0.500,
      CLKIN1_PERIOD      => 10.000 -- 100MHz input clock (10ns period)
    )
    port map (
      CLKFBOUT     => clkfbout,
      CLKFBOUTB    => open,
      CLKOUT0      => clk_mmcm,
      CLKOUT0B     => open,
      CLKOUT1      => open,
      CLKOUT1B     => open,
      CLKOUT2      => open,
      CLKOUT2B     => open,
      CLKOUT3      => open,
      CLKOUT3B     => open,
      CLKOUT4      => open,
      CLKOUT5      => open,
      CLKOUT6      => open,
      CLKFBIN      => clkfbout,
      CLKIN1       => refclk,
      CLKIN2       => '0',
      CLKINSEL     => '1',
      DADDR        => (others => '0'),
      DCLK         => '0',
      DEN          => '0',
      DI           => (others => '0'),
      DO           => open,
      DRDY         => open,
      DWE          => '0',
      PSCLK        => '0',
      PSEN         => '0',
      PSINCDEC     => '0',
      PSDONE       => open,
      CLKINSTOPPED => open,
      CLKFBSTOPPED => open,
      LOCKED       => locked_i,
      PWRDWN       => '0',
      RST          => '0'
    );

  outclk_raw_o <= clk_mmcm;

  bufg_vdc: BUFG
    port map (
      I => clk_mmcm,
      O => outclk_0
    );

  locked <= locked_i;

end architecture;
