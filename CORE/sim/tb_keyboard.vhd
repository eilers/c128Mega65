-- tb_keyboard: the MEGA65 arrows in the C128 matrix, scanned as the KERNAL and CP/M do (a PA
-- line or a K line low, PB read) and the other way round (a PB line low, PA read). With the
-- 8502 they are the C64 CRSR keys (Right Shift for up and left); with the Z80 (alt_crsr_i) the
-- top-row cursor keys. A key is its KERNAL index: row (PA bit, or 8-10 for K0-K2) * 8 + PB bit.

library ieee;
use ieee.std_logic_1164.all;

entity tb_keyboard is
end entity;

architecture sim of tb_keyboard is
   signal clk      : std_logic := '0';
   signal running  : boolean := true;
   signal key_num  : integer range 0 to 79 := 0;
   signal key_n    : std_logic := '1';
   signal alt      : std_logic := '0';
   signal ko       : std_logic_vector(2 downto 0) := "111";
   signal pao, pbo : std_logic_vector(7 downto 0) := (others => '1');
   signal pai, pbi : std_logic_vector(7 downto 0);

   -- MEGA65 key numbers (keyboard.vhd)
   constant M_HORZ : natural := 2;   constant M_VERT  : natural := 7;
   constant M_UP   : natural := 73;  constant M_LEFT  : natural := 74;
   constant M_W    : natural := 9;   constant M_RSHFT : natural := 52;
   constant M_NOSC : natural := 64;  constant M_TAB   : natural := 65;
   constant M_HELP : natural := 67;
   -- C128 matrix (KERNAL index)
   constant C_LR   : natural := 2;   constant C_UD    : natural := 7;
   constant C_W    : natural := 9;   constant C_RSHFT : natural := 52;
   constant C_HELP : natural := 64;  constant C_TAB   : natural := 67;
   constant C_UP   : natural := 83;  constant C_DOWN  : natural := 84;
   constant C_LEFT : natural := 85;  constant C_RIGHT : natural := 86;
   constant C_NOSC : natural := 87;
   constant NONE   : natural := 88;

   type t_keys is array (natural range <>) of natural;
begin
   clk <= not clk after 5 ns when running;

   dut : entity work.keyboard
      port map (
         clk_main_i => clk, reset_i => '0', trigger_run_i => '0', alt_crsr_i => alt,
         key_num_i => key_num, key_pressed_n_i => key_n,
         joy_1_up_n_i => '1', joy_1_down_n_i => '1', joy_1_left_n_i => '1',
         joy_1_right_n_i => '1', joy_1_fire_n_i => '1',
         joy_1_up_n_o => open, joy_1_down_n_o => open, joy_1_left_n_o => open,
         joy_1_right_n_o => open, joy_1_fire_n_o => open,
         joy_2_up_n_i => '1', joy_2_down_n_i => '1', joy_2_left_n_i => '1',
         joy_2_right_n_i => '1', joy_2_fire_n_i => '1',
         joy_2_up_n_o => open, joy_2_down_n_o => open, joy_2_left_n_o => open,
         joy_2_right_n_o => open, joy_2_fire_n_o => open,
         vic_ko_i => ko,
         cia1_pai_o => pai, cia1_pao_i => pao, cia1_pbi_o => pbi, cia1_pbo_i => pbo,
         restore_key_o => open);

   test : process
      variable fails, checks : natural := 0;

      procedure key(k : natural; down : boolean) is
      begin
         key_num <= k;
         key_n   <= '1';
         if down then
            key_n <= '0';
         end if;
         wait until rising_edge(clk);
         wait for 1 ns;
      end procedure;

      -- Every key the matrix shows, both scan directions (K lines only on PB)
      procedure scan(pic : out std_logic_vector(87 downto 0); tr : out std_logic_vector(63 downto 0)) is
      begin
         for r in 0 to 10 loop
            pao <= (others => '1');
            ko  <= "111";
            if r < 8 then
               pao(r) <= '0';
            else
               ko(r - 8) <= '0';
            end if;
            wait for 1 ns;
            for c in 0 to 7 loop
               pic(r * 8 + c) := not pbi(c);
            end loop;
         end loop;
         pao <= (others => '1');
         ko  <= "111";
         for c in 0 to 7 loop
            pbo    <= (others => '1');
            pbo(c) <= '0';
            wait for 1 ns;
            for r in 0 to 7 loop
               tr(r * 8 + c) := not pai(r);
            end loop;
         end loop;
         pbo <= (others => '1');
         wait for 1 ns;
      end procedure;

      function list(v : std_logic_vector) return string is
      begin
         for i in v'low to v'high loop
            if v(i) = '1' then
               return integer'image(i) & " " & list(v(v'high downto i + 1));
            end if;
         end loop;
         return "";
      end function;

      procedure expect(name : string; keys : t_keys) is
         variable pic  : std_logic_vector(87 downto 0);
         variable tr   : std_logic_vector(63 downto 0);
         variable want : std_logic_vector(87 downto 0) := (others => '0');
      begin
         for i in keys'range loop
            if keys(i) /= NONE then
               want(keys(i)) := '1';
            end if;
         end loop;
         scan(pic, tr);
         checks := checks + 1;
         if pic /= want or tr /= want(63 downto 0) then
            fails := fails + 1;
            report name & ": FAIL, PB scan [ " & list(pic) & "] PA scan [ " & list(tr) &
                   "] want [ " & list(want) & "]" severity warning;
         else
            report name & ": ok [ " & list(pic) & "]";
         end if;
      end procedure;

      procedure one(name : string; m65 : natural; keys : t_keys) is
      begin
         key(m65, true);
         expect(name, keys);
         key(m65, false);
      end procedure;
   begin
      wait for 20 ns;
      for z80 in 0 to 1 loop
         if z80 = 1 then
            alt <= '1';
            wait for 1 ns;
         end if;
         expect("nothing pressed", (0 => NONE));
         one("W",           M_W,     (0 => C_W));
         one("Right Shift", M_RSHFT, (0 => C_RSHFT));
         one("TAB",         M_TAB,   (0 => C_TAB));
         one("HELP",        M_HELP,  (0 => C_HELP));
         one("NO SCROLL",   M_NOSC,  (0 => C_NOSC));
         if z80 = 0 then
            one("8502: up",    M_UP,   (C_UD, C_RSHFT));
            one("8502: down",  M_VERT, (0 => C_UD));
            one("8502: left",  M_LEFT, (C_LR, C_RSHFT));
            one("8502: right", M_HORZ, (0 => C_LR));
         else
            one("Z80: up",    M_UP,   (0 => C_UP));
            one("Z80: down",  M_VERT, (0 => C_DOWN));
            one("Z80: left",  M_LEFT, (0 => C_LEFT));
            one("Z80: right", M_HORZ, (0 => C_RIGHT));
            key(M_RSHFT, true);
            one("Z80: Right Shift + up", M_UP, (C_RSHFT, C_UP));
            key(M_RSHFT, false);
         end if;
      end loop;

      -- Held while the CPU changes: the matrix follows at once
      key(M_VERT, true);
      expect("down held, Z80", (0 => C_DOWN));
      alt <= '0';
      wait for 1 ns;
      expect("down held, back to the 8502", (0 => C_UD));
      key(M_VERT, false);
      expect("released", (0 => NONE));

      if fails = 0 then
         report "ALL CHECKS PASSED (" & integer'image(checks) & ")";
      else
         report integer'image(fails) & " of " & integer'image(checks) & " checks FAILED" severity error;
      end if;
      running <= false;
      wait;
   end process;
end architecture;
