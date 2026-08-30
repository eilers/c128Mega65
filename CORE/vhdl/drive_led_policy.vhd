library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity drive_led_policy is
   generic (
      G_IDLE_CYCLES : positive := 1
   );
   port (
      clk_i      : in  std_logic;
      activity_i : in  std_logic;
      dirty_i    : in  std_logic;
      led_o      : out std_logic;
      colour_o   : out std_logic_vector(23 downto 0)
   );
end entity;

architecture rtl of drive_led_policy is
   signal idle_count : natural range 0 to G_IDLE_CYCLES := 0;
   signal idle       : std_logic;
begin
   process (clk_i)
   begin
      if rising_edge(clk_i) then
         if activity_i = '1' then
            idle_count <= 0;
         elsif idle_count < G_IDLE_CYCLES then
            idle_count <= idle_count + 1;
         end if;
      end if;
   end process;

   idle     <= '1' when idle_count = G_IDLE_CYCLES else '0';
   led_o    <= activity_i or (dirty_i and idle);
   colour_o <= x"00FF00" when activity_i = '1' else x"FFFF00";
end architecture;
