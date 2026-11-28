----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- What main.vhd receives from the REST API's input device (input_inject.vhd).
--
-- This package is its own file so a simulation of main compiles it. Vivado builds
-- that compile list from the testbench hierarchy and drops a file whose entity is
-- outside it. input_inject is instantiated only from mega65.vhd, so tb_c128_boot,
-- tb_func_rom, tb_cart_dma, tb_fast_io and tb_crt never compiled the package while
-- it shared input_inject.vhd with that entity.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

package input_inject_pkg is
   type t_api_input is record
      keys     : std_logic_vector(87 downto 0);   -- key index: row * 8 + PB bit, K0-K2 rows 8-10
      restore  : std_logic;
      d4080    : std_logic;                       -- 40/80 down
      capslock : std_logic;                       -- Caps Lock down
      joy1     : std_logic_vector(4 downto 0);    -- fire, right, left, down, up
      joy2     : std_logic_vector(4 downto 0);
      pot1_en  : std_logic;
      pot2_en  : std_logic;
      pot1_x   : std_logic_vector(7 downto 0);
      pot1_y   : std_logic_vector(7 downto 0);
      pot2_x   : std_logic_vector(7 downto 0);
      pot2_y   : std_logic_vector(7 downto 0);
   end record;

   constant C_API_INPUT_NONE : t_api_input := (
      keys => (others => '0'), restore => '0', d4080 => '0', capslock => '0',
      joy1 => (others => '0'), joy2 => (others => '0'), pot1_en => '0', pot2_en => '0',
      pot1_x => (others => '0'), pot1_y => (others => '0'),
      pot2_x => (others => '0'), pot2_y => (others => '0'));
end package input_inject_pkg;
