----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- sd_eng_arb: shares the SD controller's engine port (sdhost.vhd) between two clients.
--
-- One client owns the port. The other sees it busy and no strobes, so it never starts a
-- transfer. A client raises want_i before it starts anything and holds it for as long as
-- it has transfers to do; the port changes hands only between transfers, when its owner
-- does not want it and the other client does. Client A (the virtual drives) gets the
-- port first when both ask at once.
--
-- QNICE clock. Done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.sdhost_pkg.all;

entity sd_eng_arb is
port (
   clk_i      : in  std_logic;
   rst_i      : in  std_logic;

   a_want_i   : in  std_logic;
   a_c2q_i    : in  t_sd_c2q;
   a_q2c_o    : out t_sd_q2c;

   b_want_i   : in  std_logic;
   b_c2q_i    : in  t_sd_c2q;
   b_q2c_o    : out t_sd_q2c;

   c2q_o      : out t_sd_c2q;
   q2c_i      : in  t_sd_q2c
);
end entity sd_eng_arb;

architecture synthesis of sd_eng_arb is

   signal owner : std_logic := '0';          -- 0 = A, 1 = B
   signal xfer  : std_logic := '0';          -- a transfer runs (start .. done)

   -- what a client that does not own the port sees
   function locked_out(q : t_sd_q2c) return t_sd_q2c is
      variable r : t_sd_q2c := (others => '0');
   begin
      r(Q2C_READY) := q(Q2C_READY);
      r(Q2C_BUSY)  := '1';
      r(Q2C_CARD)  := q(Q2C_CARD);
      return r;
   end function locked_out;

begin

   c2q_o   <= b_c2q_i when owner = '1' else a_c2q_i;
   a_q2c_o <= q2c_i when owner = '0' else locked_out(q2c_i);
   b_q2c_o <= q2c_i when owner = '1' else locked_out(q2c_i);

   p_owner : process (clk_i)
      variable start, want, other : std_logic;
   begin
      if rising_edge(clk_i) then
         if owner = '0' then
            start := a_c2q_i(C2Q_START);
            want  := a_want_i;
            other := b_want_i;
         else
            start := b_c2q_i(C2Q_START);
            want  := b_want_i;
            other := a_want_i;
         end if;

         if start = '1' then
            xfer <= '1';
         elsif q2c_i(Q2C_DONE) = '1' then
            xfer <= '0';
         end if;

         if xfer = '0' and start = '0' and want = '0' and other = '1' then
            owner <= not owner;
         end if;

         if rst_i = '1' then
            owner <= '0';
            xfer  <= '0';
         end if;
      end if;
   end process p_owner;

end architecture synthesis;
