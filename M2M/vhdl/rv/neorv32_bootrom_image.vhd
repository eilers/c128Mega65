-- NEORV32's own boot ROM is unused: rv_soc starts in its own (BOOT_MODE_SELECT 1).
-- neorv32_bootrom.vhd still needs this package to compile.
library ieee;
use ieee.std_logic_1164.all;

package neorv32_bootrom_image is

type rom_t is array (0 to 1) of std_ulogic_vector(31 downto 0);
constant image_size_c : natural := 8;
constant image_data_c : rom_t := (others => (others => '0'));

end package;
