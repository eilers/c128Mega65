----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- The REST API's machine:input: keys, RESTORE, the 40/80 and Caps Lock keys, both joysticks
-- and the POT lines, held by the firmware as Gideon's Ultimate 64 holds them. And his
-- machine:debugreg: a byte at $D7FF that the C128 and the firmware both read and write.
--
-- Device words (C_DEV_INPUT). Words 0-11 reach the C128 together, at a write to word 15.
-- A '1' holds the key, line or direction.
--   0-3  C64 matrix keys: row 2w in word w's low byte, row 2w+1 in its high byte; bit c of
--        row r is the key between PA r and PB c (the KERNAL's key index r * 8 + c)
--   4    C128 keys: K0 in the low byte, K1 in the high byte, bit c on PB c
--   5    K2 in the low byte
--   6    bit 0 RESTORE, bit 1 40/80 down, bit 2 Caps Lock down
--   7    control port 1: bit 0 up, 1 down, 2 left, 3 right, 4 fire
--   8    control port 2: the same
--   9    bit 0: port 1's POT lines come from word 10, bit 1: port 2's from word 11
--   10   port 1: POTX in the low byte, POTY in the high byte
--   11   port 2: the same
--   12   the debug register: W sets it; R its value in bits 7-0, and bit 8 while a write has
--        not reached the C128 yet (the value read is the old one then)
--   15   W: commit words 0-11; R: bit 0 the last commit has not reached the C128 yet,
--        wait before the next
-- Words 0-11 read back what was written. Ports are the C128's, after Swap joystick ports.
--
-- Crossing: a commit copies words 0-11 and flips a toggle; the C128 side takes the copy once
-- the toggle arrives, and answers with it. The copy stays put until then (word 15, bit 0).
-- A debug register write crosses the same way. Its value comes back as a level, taken once
-- two samples agree.
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

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.input_inject_pkg.all;

entity input_inject is
   port (
      -- C128 side (main clock)
      main_clk_i      : in  std_logic;
      main_rst_i      : in  std_logic;
      main_input_o    : out t_api_input;
      main_dbg_we_i   : in  std_logic;                      -- the C128 writes $D7FF
      main_dbg_data_i : in  std_logic_vector(7 downto 0);
      main_dbg_o      : out std_logic_vector(7 downto 0);   -- what $D7FF reads

      -- Device bus (QNICE clock)
      qnice_clk_i     : in  std_logic;
      qnice_rst_i     : in  std_logic;
      qnice_addr_i    : in  std_logic_vector(11 downto 0);
      qnice_data_i    : in  std_logic_vector(15 downto 0);
      qnice_ce_i      : in  std_logic;
      qnice_we_i      : in  std_logic;
      qnice_data_o    : out std_logic_vector(15 downto 0)
   );
end entity input_inject;

architecture synthesis of input_inject is

   type t_words is array (0 to 11) of std_logic_vector(15 downto 0);

   -- CPU side
   signal q_words     : t_words := (others => (others => '0'));
   signal q_copy      : t_words := (others => (others => '0'));
   signal q_commit    : std_logic := '0';             -- toggles: a copy to take
   signal q_commit_we : std_logic;
   signal q_commit_d  : std_logic := '0';
   signal q_ack_m     : std_logic := '0';
   signal q_ack_s     : std_logic := '0';
   signal q_dbg       : std_logic_vector(7 downto 0) := (others => '0');
   signal q_dbg_new   : std_logic := '0';             -- toggles: q_dbg_w to take
   signal q_dbg_we    : std_logic;
   signal q_dbg_we_d  : std_logic := '0';
   signal q_dbg_ack_m : std_logic := '0';
   signal q_dbg_ack_s : std_logic := '0';
   signal q_dbg_ack_d : std_logic_vector(1 downto 0) := "00";
   signal q_dbg_w     : std_logic_vector(7 downto 0) := (others => '0');
   signal q_dbg_m     : std_logic_vector(7 downto 0) := (others => '0');
   signal q_dbg_s     : std_logic_vector(7 downto 0) := (others => '0');

   -- C128 side
   signal m_commit_m  : std_logic := '0';
   signal m_commit_s  : std_logic := '0';
   signal m_ack       : std_logic := '0';             -- = the commit toggle of the copy taken
   signal m_words     : t_words := (others => (others => '0'));
   signal m_dbg_new_m : std_logic := '0';
   signal m_dbg_new_s : std_logic := '0';
   signal m_dbg_ack   : std_logic := '0';             -- = the toggle of the write taken
   signal m_dbg       : std_logic_vector(7 downto 0) := (others => '0');

   attribute ASYNC_REG : string;
   attribute ASYNC_REG of m_commit_m, m_commit_s, q_ack_m, q_ack_s, m_dbg_new_m, m_dbg_new_s,
                          q_dbg_m, q_dbg_s, q_dbg_ack_m, q_dbg_ack_s : signal is "TRUE";

begin

   ---------------------------------------------------------------------------------------------
   -- C128 side (main clock)
   ---------------------------------------------------------------------------------------------

   main_input_o.keys     <= m_words(5)(7 downto 0) & m_words(4) & m_words(3) & m_words(2) &
                            m_words(1) & m_words(0);
   main_input_o.restore  <= m_words(6)(0);
   main_input_o.d4080    <= m_words(6)(1);
   main_input_o.capslock <= m_words(6)(2);
   main_input_o.joy1     <= m_words(7)(4 downto 0);
   main_input_o.joy2     <= m_words(8)(4 downto 0);
   main_input_o.pot1_en  <= m_words(9)(0);
   main_input_o.pot2_en  <= m_words(9)(1);
   main_input_o.pot1_x   <= m_words(10)(7 downto 0);
   main_input_o.pot1_y   <= m_words(10)(15 downto 8);
   main_input_o.pot2_x   <= m_words(11)(7 downto 0);
   main_input_o.pot2_y   <= m_words(11)(15 downto 8);
   main_dbg_o            <= m_dbg;

   p_main : process (main_clk_i)
   begin
      if rising_edge(main_clk_i) then
         m_commit_m  <= q_commit;
         m_commit_s  <= m_commit_m;
         m_dbg_new_m <= q_dbg_new;
         m_dbg_new_s <= m_dbg_new_m;

         -- The copy was stable before its toggle flipped
         if m_commit_s /= m_ack then
            m_words <= q_copy;
            m_ack   <= m_commit_s;
         end if;

         if m_dbg_new_s /= m_dbg_ack then
            m_dbg     <= q_dbg_w;
            m_dbg_ack <= m_dbg_new_s;
         elsif main_dbg_we_i = '1' then
            m_dbg <= main_dbg_data_i;
         end if;

         if main_rst_i = '1' then
            m_words <= (others => (others => '0'));
            m_dbg   <= (others => '0');
         end if;
      end if;
   end process p_main;

   ---------------------------------------------------------------------------------------------
   -- CPU side (QNICE clock): devices act on the falling edge
   ---------------------------------------------------------------------------------------------

   q_commit_we  <= '1' when qnice_ce_i = '1' and qnice_we_i = '1' and qnice_addr_i = x"00F" else '0';
   q_dbg_we     <= '1' when qnice_ce_i = '1' and qnice_we_i = '1' and qnice_addr_i = x"00C" else '0';
   qnice_data_o <= q_words(to_integer(unsigned(qnice_addr_i(3 downto 0))))
                                         when unsigned(qnice_addr_i) < 12 else
                   "0000000" & (q_dbg_new xor q_dbg_ack_d(1)) & q_dbg when qnice_addr_i = x"00C" else
                   x"000" & "000" & (q_commit xor q_ack_s) when qnice_addr_i = x"00F" else
                   x"0000";

   p_qnice : process (qnice_clk_i)
   begin
      if falling_edge(qnice_clk_i) then
         q_ack_m <= m_ack;
         q_ack_s <= q_ack_m;
         q_dbg_m <= m_dbg;
         q_dbg_s <= q_dbg_m;
         if q_dbg_m = q_dbg_s then
            q_dbg <= q_dbg_s;
         end if;
         -- Two stages more than the value: a write is done once q_dbg holds it, even when its
         -- value's synchronizer resolves a clock after the toggle's
         q_dbg_ack_m <= m_dbg_ack;
         q_dbg_ack_s <= q_dbg_ack_m;
         q_dbg_ack_d <= q_dbg_ack_d(0) & q_dbg_ack_s;

         if qnice_ce_i = '1' and qnice_we_i = '1' and unsigned(qnice_addr_i) < 12 then
            q_words(to_integer(unsigned(qnice_addr_i(3 downto 0)))) <= qnice_data_i;
         end if;

         -- ce can be high for more than one edge, and is low between two accesses
         q_commit_d <= q_commit_we;
         if q_commit_we = '1' and q_commit_d = '0' then
            q_copy   <= q_words;
            q_commit <= not q_commit;
         end if;
         q_dbg_we_d <= q_dbg_we;
         if q_dbg_we = '1' and q_dbg_we_d = '0' then
            q_dbg_w   <= qnice_data_i(7 downto 0);
            q_dbg_new <= not q_dbg_new;
         end if;

         if qnice_rst_i = '1' then
            q_words <= (others => (others => '0'));
         end if;
      end if;
   end process p_qnice;

end architecture synthesis;
