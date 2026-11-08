----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Checks the window his VIC stream takes (stream_tx: colorX 0..383, colorY 16..287) against
-- the core's VIC itself. The VIC runs on fpga64_sid_iec's 32-state cycle, PAL, set to 25
-- rows and 40 columns with a white border around a blue background, and every pixel it
-- puts out is taken with its colorX/colorY as stream_tx takes them. The display must open
-- at x 32 on raster 51 and close after x 351 and raster 250, as on a C64 (32 pixels and 35
-- lines of border in his 384x272), and no blanked pixel may fall inside the window.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_vic_window is
end entity tb_vic_window;

architecture sim of tb_vic_window is

   constant C_PERIOD : time := 31.718 ns;

   signal clk      : std_logic := '0';
   signal done     : boolean := false;
   signal reset    : std_logic := '1';
   signal cyc      : natural range 0 to 31 := 0;      -- fpga64_sid_iec's sysCycle, EXT0 = 0
   signal ena_pix  : std_logic := '0';
   signal ena_data : std_logic := '0';
   signal phi      : std_logic;

   signal cs       : std_logic := '0';
   signal we       : std_logic := '0';
   signal areg     : unsigned(5 downto 0) := (others => '0');
   signal dreg     : unsigned(7 downto 0) := (others => '0');

   signal index    : unsigned(3 downto 0);
   signal cx       : unsigned(9 downto 0);
   signal cy       : unsigned(8 downto 0);

   signal frames   : natural := 0;
   signal errors   : natural := 0;

   constant C_EC  : natural := 1;     -- border: white
   constant C_B0C : natural := 6;     -- background: blue

begin

   clk <= not clk after C_PERIOD / 2 when not done;

   -- As fpga64_sid_iec: enablePixel registered at VIC2, EXT2, DMA2, EXT6, CPU2, CPU6, CPUA,
   -- CPUE; enableVic at VIC2 and CPU6; phi0_cpu over EXT4..CPU7
   cycle_proc : process (clk)
   begin
      if rising_edge(clk) then
         cyc      <= (cyc + 1) mod 32;
         ena_pix  <= '1' when cyc mod 4 = 2 else '0';
         ena_data <= '1' when cyc = 6 or cyc = 22 else '0';
      end if;
   end process cycle_proc;
   phi <= '1' when cyc >= 8 and cyc <= 23 else '0';

   i_vic : entity work.video_vicii_656x
      generic map (
         registeredAddress => true,
         emulateRefresh    => true,
         emulateLightpen   => true,
         emulateGraphics   => true
      )
      port map (
         clk         => clk,
         phi         => phi,
         enaData     => ena_data,
         enaPixel    => ena_pix,
         baSync      => '0',
         ba          => open,
         ba_dma      => open,
         mode6569    => '1',
         mode6567old => '0',
         mode6567R8  => '0',
         mode6572    => '0',
         turbo_en    => '0',
         turbo_state => open,
         vic2e       => '1',
         variant     => "00",
         reset       => reset,
         cs          => cs,
         we          => we,
         lp_n        => '1',
         aRegisters  => areg,
         diRegisters => dreg,
         di          => (others => '0'),
         diColor     => (others => '0'),
         do          => open,
         vicAddr     => open,
         irq_n       => open,
         hSync       => open,
         vSync       => open,
         colorIndex  => index,
         colorX      => cx,
         colorY      => cy,
         invertV     => open,
         ko          => open,
         debugX      => open,
         debugY      => open,
         vicRefresh  => open,
         addrValid   => open
      ); -- i_vic

   -- Register writes, a whole CPU half-cycle each
   cpu_proc : process
      procedure poke(a, d : natural) is
      begin
         wait until rising_edge(clk) and cyc = 7;
         areg <= to_unsigned(a, 6);
         dreg <= to_unsigned(d, 8);
         cs   <= '1';
         we   <= '1';
         wait until rising_edge(clk) and cyc = 24;
         cs   <= '0';
         we   <= '0';
      end procedure poke;
   begin
      wait for 1 us;
      reset <= '0';
      wait for 1 us;
      poke(16#11#, 16#1B#);    -- DEN, 25 rows, yscroll 3
      poke(16#16#, 16#08#);    -- 40 columns
      poke(16#20#, C_EC);
      poke(16#21#, C_B0C);
      wait;
   end process cpu_proc;

   -- Each pixel as stream_tx takes it: on the ce, the index with the x and y registered with it
   check_proc : process (clk)
      variable first_x, last_x : integer := -1;
      variable first_y, last_y : integer := -1;
      variable blanks          : natural := 0;
      variable checking        : boolean := false;
      variable x, y            : natural;
   begin
      if rising_edge(clk) and ena_pix = '1' then
         x := to_integer(cx);
         y := to_integer(cy);
         if x = 0 and y = 16 then
            -- A frame starts: judge the last one once the registers were in for all of it
            if checking then
               if first_x /= 32 or last_x /= 351 or first_y /= 51 or last_y /= 250 then
                  report "display x " & integer'image(first_x) & ".." & integer'image(last_x) &
                         ", y " & integer'image(first_y) & ".." & integer'image(last_y) &
                         "; want 32..351, 51..250" severity error;
                  errors <= errors + 1;
               end if;
               if blanks /= 0 then
                  report integer'image(blanks) & " blanked pixels in the window" severity error;
                  errors <= errors + 1;
               end if;
               frames <= frames + 1;
            end if;
            checking := now > 100 us;
            first_x := -1; last_x := -1; first_y := -1; last_y := -1; blanks := 0;
         end if;
         if x < 384 and y >= 16 and y < 288 then
            if to_integer(index) = C_B0C then
               if first_x < 0 or x < first_x then first_x := x; end if;
               if x > last_x then last_x := x; end if;
               if first_y < 0 then first_y := y; end if;
               last_y := y;
            elsif to_integer(index) /= C_EC then
               blanks := blanks + 1;
            end if;
         end if;
      end if;
   end process check_proc;

   watchdog_proc : process
   begin
      wait for 100 ms;
      assert done report "TIMEOUT: no frame starts at x 0, y 16" severity failure;
      wait;
   end process watchdog_proc;

   end_proc : process
   begin
      wait until frames = 2;
      if errors = 0 then
         report "ALL CHECKS PASSED";
      else
         report "FAILED" severity error;
      end if;
      done <= true;
      wait;
   end process end_proc;

end architecture sim;
