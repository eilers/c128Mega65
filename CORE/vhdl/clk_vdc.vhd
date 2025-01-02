-- VDC clock generator
-- Calculated the appropriate multiplication and division factors to 
-- get 32MHz output from 100MHz input:
-- Input frequency: 100 MHz 
-- CLKFBOUT_MULT_F = 11.52 (creates 1152 MHz VCO) 
-- CLKOUT0_DIVIDE_F = 36.0 (divides to get 32 MHz)

library IEEE;
  use IEEE.STD_LOGIC_1164.all;
library UNISIM;
  use UNISIM.VCOMPONENTS.all;

entity vdc_clk is
  port (
    refclk   : in  std_logic;
    rst      : in  std_logic;
    outclk_0 : out std_logic;
    locked   : out std_logic
  );
end entity;

architecture rtl of vdc_clk is
  signal clkfbout     : std_logic;
  signal clkfbout_buf : std_logic;
begin

  -- MMCME2_ADV: Advanced Mixed Mode Clock Manager
  -- Configured for 32MHz output from 100MHz input
  mmcm_adv_inst: MMCME2_ADV
    generic map (
      BANDWIDTH          => "OPTIMIZED",
      CLKOUT4_CASCADE    => FALSE,
      COMPENSATION       => "ZHOLD",
      STARTUP_WAIT       => FALSE,
      DIVCLK_DIVIDE      => 1,
      CLKFBOUT_MULT_F    => 11.52, -- 100MHz * 11.52 = 1152MHz VCO
      CLKFBOUT_PHASE     => 0.000,
      CLKOUT0_DIVIDE_F   => 36.0,  -- 1152MHz / 36 = 32MHz
      CLKOUT0_PHASE      => 0.000,
      CLKOUT0_DUTY_CYCLE => 0.500,
      CLKIN1_PERIOD      => 10.000 -- 100MHz input clock (10ns period)
    )
    port map (
      -- Output clocks
      CLKFBOUT  => clkfbout,
      CLKFBOUTB => open,
      CLKOUT0   => outclk_0,
      CLKOUT0B  => open,
      CLKOUT1   => open,
      CLKOUT1B  => open,
      CLKOUT2   => open,
      CLKOUT2B  => open,
      CLKOUT3   => open,
      CLKOUT3B  => open,
      CLKOUT4   => open,
      CLKOUT5   => open,
      CLKOUT6   => open,
      -- Input clock control
      CLKFBIN   => clkfbout,
      CLKIN1    => refclk,
      CLKIN2    => '0',
      -- Tied to always select the primary input clock
      CLKINSEL  => '1',
      -- Ports for dynamic reconfiguration
      DADDR     => (others => '0'),
      DCLK      => '0',
      DEN       => '0',
      DI        => (others => '0'),
      DO        => open,
      DRDY      => open,
      DWE       => '0',
      -- Other control and status signals
      LOCKED    => locked,
      PWRDWN    => '0',
      RST       => rst
    );

end architecture;
