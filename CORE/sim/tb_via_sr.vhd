----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- tb_via_sr: iecdrv_via6522's shift register sends bit 7 first, each bit on CB2 by the
-- time CB1 rises, as a 6522 does. The CMD HD's fast serial is the VIA's SR under T2
-- (ACR $14); the C128's CIA takes each bit at a rising edge of CB1.
--
-- Done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_via_sr is
end entity tb_via_sr;

architecture sim of tb_via_sr is
   signal clk      : std_logic := '0';
   signal cnt      : unsigned(3 downto 0) := (others => '0');
   signal ph2_r    : std_logic;
   signal ph2_f    : std_logic;
   signal rst      : std_logic := '1';
   signal addr     : std_logic_vector(3 downto 0) := (others => '0');
   signal wen      : std_logic := '0';
   signal din      : std_logic_vector(7 downto 0) := (others => '0');
   signal cb1_o, cb1_t, cb2_o, cb2_t : std_logic;
   signal cb1      : std_logic;
   signal got      : std_logic_vector(7 downto 0);
   signal bits     : natural := 0;
   signal done     : boolean := false;
begin
   clk   <= not clk after 15.625 ns when not done;     -- 32 MHz, a 2 MHz phi2 as cmdhd_drive
   ph2_r <= '1' when cnt = 0 else '0';
   ph2_f <= '1' when cnt = 8 else '0';

   p_cnt : process (clk)
   begin
      if rising_edge(clk) then
         cnt <= cnt + 1;
      end if;
   end process p_cnt;

   i_via : entity work.iecdrv_via6522
      port map (
         clock    => clk,
         rising   => ph2_f,
         falling  => ph2_r,
         reset    => rst,
         addr     => addr,
         wen      => wen,
         ren      => '0',
         data_in  => din,
         data_out => open,
         phi2_ref => open,
         port_a_o => open,
         port_a_t => open,
         port_a_i => x"FF",
         port_b_o => open,
         port_b_t => open,
         port_b_i => x"FF",
         ca1_i    => '1',
         ca2_o    => open,
         ca2_i    => '1',
         ca2_t    => open,
         cb1_o    => cb1_o,
         cb1_i    => cb1,
         cb1_t    => cb1_t,
         cb2_o    => cb2_o,
         cb2_i    => '1',
         cb2_t    => cb2_t,
         irq      => open
      );

   cb1 <= cb1_o or not cb1_t;

   -- the receiver: a bit at each rising edge of CB1, MSB first
   p_rx : process (cb1)
   begin
      if rising_edge(cb1) and rst = '0' and cb2_t = '1' then
         got  <= got(6 downto 0) & cb2_o;
         bits <= bits + 1;
      end if;
   end process p_rx;

   p_main : process
      variable errors : natural := 0;

      procedure wr(r : natural; d : natural) is
      begin
         addr <= std_logic_vector(to_unsigned(r, 4));
         din  <= std_logic_vector(to_unsigned(d, 8));
         wen  <= '1';
         wait until rising_edge(clk) and ph2_r = '1';
         wen  <= '0';
         wait until rising_edge(clk);
      end procedure;

      procedure send(acr : natural; b : natural) is
         variable n : natural;
      begin
         wr(16#B#, acr);
         n := bits;
         wr(16#A#, b);
         wait until bits = n + 8 for 200 us;
         wait for 20 us;                               -- no ninth edge
         if bits /= n + 8 or got /= std_logic_vector(to_unsigned(b, 8)) then
            report "ACR $" & to_hstring(to_unsigned(acr, 8)) & ", SR $" & to_hstring(to_unsigned(b, 8)) &
                   ": got $" & to_hstring(got) & " in " & integer'image(bits - n) & " edges" severity error;
            errors := errors + 1;
         end if;
      end procedure;

      constant BYTES : integer_vector := (16#4F#, 16#30#, 16#80#, 16#01#, 16#A5#, 16#FF#, 16#00#);
   begin
      wait for 1 us;
      wait until rising_edge(clk);
      rst <= '0';
      wr(16#8#, 4);                                    -- T2 low latch: the shift rate
      for i in BYTES'range loop
         send(16#14#, BYTES(i));                       -- out under T2, as the CMD HD
         send(16#18#, BYTES(i));                       -- out under phi2
      end loop;
      if errors = 0 then
         report "VIA SR: ALL CHECKS PASSED";
      else
         report "VIA SR: " & integer'image(errors) & " FAILED" severity failure;
      end if;
      done <= true;
      wait;
   end process p_main;
end architecture sim;
