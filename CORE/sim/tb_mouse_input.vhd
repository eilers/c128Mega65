-- tb_mouse_input: Amiga mouse detection in M2M's mouse_input.vhdl. A joystick passes through;
-- an Amiga mouse (pots at the edge, an impossible direction pair) is masked and counted into
-- 1351 POT values; idle timeout; paddles end it; the right button arms only after pin 9 reads high.
-- Timeouts are shortened by generics; POT lines are modelled as charge times after the drain.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_mouse_input is
end entity;

architecture sim of tb_mouse_input is
   constant C_MODE_TO : natural := 50_000;      -- 1 ms
   constant C_RMB_TO  : natural := 250_000;     -- 5 ms
   constant C_SAMPLE  : time    := 1100 us;     -- two POT sample periods (514 us each)
   constant T_FLOAT   : natural := 999;         -- line never charges: reads 255
   constant T_HIGH    : natural := 0;           -- line pulled high: reads 0

   signal clk      : std_logic := '0';
   signal running  : boolean := true;
   signal drain    : std_logic;
   signal pot_line : std_logic_vector(3 downto 0) := (others => '0');  -- ax, ay, bx, by
   type t_targets is array (0 to 3) of natural;
   signal target   : t_targets := (others => T_FLOAT);

   -- Port lines, low active: up, down, left, right. Port B powers up with a passive mouse
   -- resting with left and right low (recognized before the first POT sample)
   signal a_in         : std_logic_vector(3 downto 0) := "1111";
   signal b_in         : std_logic_vector(3 downto 0) := "0011";
   signal a_out, b_out : std_logic_vector(3 downto 0);
   signal pa_x, pa_y, pb_x, pb_y : unsigned(7 downto 0);

   signal errors : natural := 0;
begin
   clk <= not clk after 10 ns when running;   -- 50 MHz

   dut : entity work.mouse_input
      generic map (G_MODE_TIMEOUT => C_MODE_TO, G_RMB_TIMEOUT => C_RMB_TO)
      port map (
         clk => clk,
         amiga_mouse_enable_a => '1', amiga_mouse_enable_b => '1',
         amiga_mouse_assume_a => '0', amiga_mouse_assume_b => '0',
         pot_drain => drain,
         fa_potx => pot_line(0), fa_poty => pot_line(1),
         fb_potx => pot_line(2), fb_poty => pot_line(3),
         fa_fire => '1', fa_up => a_in(0), fa_down => a_in(1), fa_left => a_in(2), fa_right => a_in(3),
         fb_fire => '1', fb_up => b_in(0), fb_down => b_in(1), fb_left => b_in(2), fb_right => b_in(3),
         fa_up_out => a_out(0), fa_down_out => a_out(1), fa_left_out => a_out(2), fa_right_out => a_out(3),
         fb_up_out => b_out(0), fb_down_out => b_out(1), fb_left_out => b_out(2), fb_right_out => b_out(3),
         mouse_debug => open,
         pota_x => pa_x, pota_y => pa_y, potb_x => pb_x, potb_y => pb_y);

   -- A POT line stays low (charging) for target us after the drain ends
   g_pots : for i in 0 to 3 generate
      p_pot : process (clk)
         variable cycles : natural := 0;
      begin
         if rising_edge(clk) then
            if drain = '1' then
               cycles := 0;
               pot_line(i) <= '0';
            else
               cycles := cycles + 1;
               if target(i) /= T_FLOAT and cycles >= target(i) * 50 then
                  pot_line(i) <= '1';
               end if;
            end if;
         end if;
      end process;
   end generate;

   p_test : process
      -- Quadrature state per port: x = (right, down), y = (left, up); "11" at rest
      variable ax, ay, bx, by : natural := 0;
      type t_quad is array (0 to 3) of std_logic_vector(1 downto 0);
      constant XSEQ : t_quad := ("11", "10", "00", "01");   -- counts up (mouse_input's table)
      constant YSEQ : t_quad := ("11", "01", "00", "10");

      procedure check(ok : boolean; msg : string) is
      begin
         if not ok then
            report msg & " at " & time'image(now) severity error;
            errors <= errors + 1;
         end if;
      end procedure;

      procedure tick(n : natural := 1) is
      begin
         for i in 1 to n loop
            wait until rising_edge(clk);
         end loop;
         wait for 1 ns;
      end procedure;

      -- One quadrature step on port A or B, +1 or -1, x or y axis
      procedure step(port_b : boolean; y_axis : boolean; up : boolean) is
         variable s : natural;
         variable d : natural := 3;          -- +1, or -1 mod 4
      begin
         if up then d := 1; end if;
         if not port_b and not y_axis then
            ax := (ax + d) mod 4;  s := ax;
            a_in(3) <= XSEQ(s)(1);  a_in(1) <= XSEQ(s)(0);
         elsif not port_b then
            ay := (ay + d) mod 4;  s := ay;
            a_in(2) <= YSEQ(s)(1);  a_in(0) <= YSEQ(s)(0);
         elsif not y_axis then
            bx := (bx + d) mod 4;  s := bx;
            b_in(3) <= XSEQ(s)(1);  b_in(1) <= XSEQ(s)(0);
         else
            by := (by + d) mod 4;  s := by;
            b_in(2) <= YSEQ(s)(1);  b_in(0) <= YSEQ(s)(0);
         end if;
         tick(100);   -- 2 us per step, far faster than a hand moves
      end procedure;

      variable x0, y0 : unsigned(7 downto 0);
   begin
      tick(10);
      wait for C_SAMPLE;

      -- 0. Port B's mouse from power-up: masked, and its floating pin 9 is no right button
      check(b_out = "1111", "a mouse present at power-up should be masked, right button up, got " &
            to_string(b_out));
      b_in <= "1111";                    -- unplugged: back to joystick after the idle timeout

      -- 1. Joystick on port A (pots open): every direction passes, POTs stay real (255)
      check(pa_x = 255 and pa_y = 255, "open POTs should read 255");
      for i in 0 to 3 loop
         a_in(i) <= '0';  tick(5);
         check(a_out(i) = '0', "joystick direction " & integer'image(i) & " should pass");
         a_in(i) <= '1';  tick(5);
         check(a_out = "1111", "joystick release should pass");
      end loop;

      -- 2. Amiga mouse on port A: pure x motion with y at rest high is not yet recognized
      step(false, false, true);
      check(a_out(1) = '0', "before detection a mouse line passes as a direction");
      step(false, true, true);           -- y to (left 0, up 1)
      step(false, false, true);          -- x to (right 0, down 0): left and right low together
      tick(2);
      check(a_out = "1111", "Amiga mouse should mask the directions");
      x0 := pa_x;  y0 := pa_y;
      check(x0 = x"7F" and y0 = x"7F", "mouse POTs should start from the open reading (7F)");
      for i in 1 to 10 loop
         step(false, false, true);
         check(a_out = "1111", "directions masked while moving");
      end loop;
      for i in 1 to 3 loop
         step(false, true, false);
      end loop;
      check(pa_x = x0 + 10, "x should count 10 steps, got " & integer'image(to_integer(pa_x)));
      check(pa_y = y0 - 3, "y should count -3 steps, got " & integer'image(to_integer(pa_y)));
      for i in 1 to 80 loop               -- through 0xBF -> 0x40 (7 bits, as a 1351)
         step(false, false, true);
      end loop;
      check(pa_x = x"40" + ((x0 - x"40" + 90) mod 128), "x should wrap in 7 bits");
      check(b_out = "1111", "port B should not notice port A");

      -- 3. Resting with a line low keeps the mouse; all lines high times out to joystick
      tick(C_MODE_TO * 2);
      check(a_out = "1111", "a resting mouse with a line low should stay a mouse");
      while ax /= 0 loop step(false, false, true); end loop;
      while ay /= 0 loop step(false, true, true); end loop;
      x0 := pa_x;
      tick(C_MODE_TO + 10);
      a_in(0) <= '0';  tick(5);
      check(a_out(0) = '0', "after the idle timeout a direction should pass again");
      check(pa_x = x0, "after the timeout the mouse position should hold");
      a_in(0) <= '1';  tick(5);

      -- 4. Paddles (POTs off the edge) end the mouse: real readings come back
      target(0) <= 130;  target(1) <= 70;
      wait for C_SAMPLE;
      check(pa_x > 120 and pa_x < 132, "paddle x should read about 128, got " & integer'image(to_integer(pa_x)));
      check(pa_y > 60 and pa_y < 72, "paddle y should read about 68, got " & integer'image(to_integer(pa_y)));
      a_in(2) <= '0';  a_in(3) <= '0';  tick(5);     -- both paddle buttons
      check(a_out(2) = '0' and a_out(3) = '0', "paddle buttons should pass");
      a_in(2) <= '1';  a_in(3) <= '1';  tick(5);

      -- 5. Passive mouse (pin 9 open) on port A: the right button never shows
      target(0) <= T_FLOAT;  target(1) <= T_FLOAT;
      wait for C_SAMPLE;
      step(false, true, true);  step(false, false, true);  step(false, false, true);
      tick(2);
      check(a_out = "1111", "port A should be a mouse again");
      wait for C_SAMPLE;
      check(a_out(0) = '1', "a passive mouse's floating pin 9 must not press the right button");

      -- 6. Mouse with a pull-up on port B: the right button reaches up, as on a 1351
      target(2) <= 12;
      wait for C_SAMPLE;
      step(true, true, true);  step(true, false, true);  step(true, false, true);
      tick(2);
      wait for C_SAMPLE;
      check(b_out = "1111", "port B mouse, right button released");
      target(2) <= T_FLOAT;              -- pressed: pin 9 grounded
      wait for C_SAMPLE;
      check(b_out(0) = '0', "right button pressed should pull up low");
      check(b_out(3 downto 1) = "111", "other directions stay masked");
      target(2) <= 12;
      wait for C_SAMPLE;
      check(b_out(0) = '1', "right button released");
      target(2) <= T_FLOAT;              -- unplugged: floats "pressed" until the watchdog
      wait for C_SAMPLE;
      check(b_out(0) = '0', "held again");
      tick(C_RMB_TO + 100);
      check(b_out(0) = '1', "after the watchdog a floating pin 9 should read released");

      running <= false;
      if errors = 0 then
         report "ALL CHECKS PASSED";
      else
         report "FAILED with " & integer'image(errors) & " errors" severity error;
      end if;
      wait;
   end process;
end architecture;
