library ieee;
use ieee.std_logic_1164.all;

entity tb_drive_led_policy is
end entity;

architecture sim of tb_drive_led_policy is
   signal clk      : std_logic := '0';
   signal activity : std_logic := '0';
   signal dirty    : std_logic := '0';
   signal led      : std_logic;
   signal colour   : std_logic_vector(23 downto 0);
   signal done     : boolean := false;
begin
   clk <= not clk after 5 ns when not done else '0';

   dut : entity work.drive_led_policy
      generic map (G_IDLE_CYCLES => 8)
      port map (clk, activity, dirty, led, colour);

   process
   begin
      wait for 1 ns;
      assert led = '0' report "FAIL: clean idle LED is on" severity failure;

      activity <= '1';
      wait for 10 ns;
      assert led = '1' and colour = x"00FF00"
         report "FAIL: access is not green" severity failure;

      -- A dirty cache must not fill the dark gap in a DOS error blink.
      dirty <= '1';
      activity <= '0';
      wait for 40 ns;
      assert led = '0' report "FAIL: dirty warning filled an error-blink gap" severity failure;
      activity <= '1';
      wait for 10 ns;
      assert led = '1' and colour = x"00FF00"
         report "FAIL: error blink pulse is not green" severity failure;

      -- PRINT DS$ stops the DOS blink. Dirty cache takes over only after the
      -- configured quiet interval.
      activity <= '0';
      wait for 70 ns;
      assert led = '0' report "FAIL: dirty warning appeared before idle delay" severity failure;
      wait for 20 ns;
      assert led = '1' and colour = x"FFFF00"
         report "FAIL: idle dirty cache is not yellow" severity failure;
      dirty <= '0';
      wait for 1 ns;
      assert led = '0' report "FAIL: clean drive stayed lit" severity failure;

      report "ALL CHECKS PASSED: authentic drive LED policy";
      done <= true;
      wait;
   end process;
end architecture;
