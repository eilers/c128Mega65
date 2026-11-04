----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- The system CPU's reads of the VDC, device C_DEV_VDC: its RAM and registers without a
-- CPU access's side effects (no update address moved, no light pen flag cleared). REST's
-- machine:vdcmem and vdcregs and the serial monitor's vdcram: and vdcreg: spaces read through it.
--
-- Device words, one byte each in the low half, read only (writes are ignored):
--   0x0000-0xFFFF        VDC RAM at the address a program sets in R18/R19, mapped as R28
--                        bit 4 has it (16K or 64K)
--   0xFFFF000-0xFFFF027  the CSR window: R0-R37 as the CPU reads them, then the status
--                        byte ($D600) and the selected register
-- A read crosses to clk_vdc and back, so the CPU waits about 250 ns. The VDC reads on its
-- own clock, so a read meeting the C128's write gets the old byte and never harms the write.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity vdc_peek is
   port (
      -- VDC side (clk_vdc): vdc_top's dbg_* ports
      vdc_clk_i    : in  std_logic;
      vdc_rd_o     : out std_logic;
      vdc_addr_o   : out unsigned(15 downto 0);
      vdc_ram_i    : in  unsigned(7 downto 0);
      vdc_reg_i    : in  unsigned(7 downto 0);

      -- Device bus (QNICE clock)
      qnice_clk_i  : in  std_logic;
      qnice_rst_i  : in  std_logic;
      qnice_addr_i : in  std_logic_vector(27 downto 0);
      qnice_ce_i   : in  std_logic;
      qnice_we_i   : in  std_logic;
      qnice_data_o : out std_logic_vector(15 downto 0);
      qnice_wait_o : out std_logic
   );
end entity vdc_peek;

architecture synthesis of vdc_peek is

   -- Device bus side
   signal q_req   : std_logic := '0';
   signal q_busy  : std_logic := '0';
   signal q_done  : std_logic := '0';
   signal q_csr   : std_logic := '0';
   signal q_addr  : std_logic_vector(15 downto 0) := (others => '0');
   signal q_rdata : std_logic_vector(7 downto 0) := (others => '0');
   signal q_ack_m : std_logic := '0';
   signal q_ack_s : std_logic := '0';
   signal q_ack_d : std_logic := '0';

   -- VDC side
   signal v_req_m : std_logic := '0';
   signal v_req_s : std_logic := '0';
   signal v_req_d : std_logic := '0';
   signal v_step  : natural range 0 to 2 := 0;
   signal v_csr   : std_logic := '0';
   signal v_rd    : std_logic := '0';
   signal v_addr  : unsigned(15 downto 0) := (others => '0');
   signal v_rdata : std_logic_vector(7 downto 0) := (others => '0');
   signal v_ack   : std_logic := '0';

   attribute ASYNC_REG : string;
   attribute ASYNC_REG of q_ack_m, q_ack_s, v_req_m, v_req_s : signal is "TRUE";

begin

   ---------------------------------------------------------------------------------------------
   -- CPU side (QNICE clock): devices act on the falling edge, as the device bus wants
   ---------------------------------------------------------------------------------------------

   qnice_wait_o <= qnice_ce_i and not qnice_we_i and not q_done;
   qnice_data_o <= x"00" & q_rdata;

   p_qnice : process (qnice_clk_i)
   begin
      if falling_edge(qnice_clk_i) then
         q_ack_m <= v_ack;
         q_ack_s <= q_ack_m;

         -- ce is low for at least one falling edge between two accesses
         if qnice_ce_i = '0' then
            q_done <= '0';
         end if;
         if q_busy = '0' then
            q_ack_d <= q_ack_s;
         end if;
         if qnice_ce_i = '1' and qnice_we_i = '0' and q_busy = '0' and q_done = '0' then
            q_addr <= qnice_addr_i(15 downto 0);
            if qnice_addr_i(27 downto 12) = x"FFFF" then
               q_csr <= '1';
            else
               q_csr <= '0';
            end if;
            q_req  <= not q_req;
            q_busy <= '1';
         end if;
         if q_busy = '1' and q_ack_s /= q_ack_d then
            q_ack_d <= q_ack_s;
            q_rdata <= v_rdata;
            q_busy  <= '0';
            q_done  <= '1';
         end if;

         if qnice_rst_i = '1' then
            q_busy <= '0';
            q_done <= '0';
         end if;
      end if;
   end process p_qnice;

   ---------------------------------------------------------------------------------------------
   -- VDC side: the address goes out, vdc_top reads on the next edge, the byte is there after it
   ---------------------------------------------------------------------------------------------

   p_vdc : process (vdc_clk_i)
   begin
      if rising_edge(vdc_clk_i) then
         v_req_m <= q_req;
         v_req_s <= v_req_m;
         v_req_d <= v_req_s;
         v_rd    <= '0';
         case v_step is
            when 0 =>
               -- q_addr and q_csr were stable before the toggle flipped
               if v_req_s /= v_req_d then
                  v_addr <= unsigned(q_addr);
                  v_csr  <= q_csr;
                  v_rd   <= not q_csr;
                  v_step <= 1;
               end if;
            when 1 =>
               v_step <= 2;
            when 2 =>
               if v_csr = '1' then
                  v_rdata <= std_logic_vector(vdc_reg_i);
               else
                  v_rdata <= std_logic_vector(vdc_ram_i);
               end if;
               v_ack  <= not v_ack;
               v_step <= 0;
         end case;
      end if;
   end process p_vdc;

   vdc_rd_o   <= v_rd;
   vdc_addr_o <= v_addr;

end architecture synthesis;
