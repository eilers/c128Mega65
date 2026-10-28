----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Ultimate Command Interface (UCI): the registers at $DF1C-$DF1F through which a C128
-- program sends commands to the system CPU's firmware, as on Gideon's Ultimate.
--
-- command_protocol.vhd is Gideon's, unchanged (README.md), on the C128's clock. This file
-- takes the place of his command_interface.vhd: the 2 KB buffer is a dual-clock RAM, and
-- the CPU reaches it and the protocol's registers as device C_DEV_UCI on the QNICE clock.
--
-- Device words, one byte each in the low half:
--   0x000-0x00F  the protocol's registers (his CMD_IF_BASE + n). Each access crosses to
--                the C128's clock and back, so the CPU waits about 200 ns.
--   0x010        bit 0: the C128 was reset since this bit was cleared (write 1 to clear)
--   0x800-0xFFF  the buffer: command at 0x800, response at 0xB80, status at 0xF00
--
-- C128 side: cs_i is the select in PHI2 (main.vhd decodes $DF18-$DF1F). An access takes
-- effect on its rising edge, as the REU's and the 6551's do: a read latches its byte
-- there, before the queue moves on to the next one.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.io_bus_pkg.all;
use work.slot_bus_pkg.all;

entity uci is
   port (
      -- C128 side (main clock)
      main_clk_i      : in  std_logic;
      main_rst_i      : in  std_logic;                      -- the whole machine's reset
      main_c128_rst_i : in  std_logic;                      -- the C128's own reset
      main_cs_i       : in  std_logic;
      main_we_i       : in  std_logic;                      -- every CPU write, $FF00 included
      main_addr_i     : in  std_logic_vector(15 downto 0);
      main_data_i     : in  std_logic_vector(7 downto 0);
      main_data_o     : out std_logic_vector(7 downto 0) := x"FF";
      main_irq_o      : out std_logic;
      main_dma_o      : out std_logic;                      -- hold the CPU (the DMA bit)

      -- Device bus (QNICE clock)
      qnice_clk_i     : in  std_logic;
      qnice_rst_i     : in  std_logic;
      qnice_addr_i    : in  std_logic_vector(11 downto 0);
      qnice_data_i    : in  std_logic_vector(15 downto 0);
      qnice_ce_i      : in  std_logic;
      qnice_we_i      : in  std_logic;
      qnice_data_o    : out std_logic_vector(15 downto 0);
      qnice_wait_o    : out std_logic
   );
end entity uci;

architecture synthesis of uci is

   -- C128 side
   signal slot_req    : t_slot_req;
   signal slot_resp   : t_slot_resp;
   signal io_req      : t_io_req;
   signal io_resp     : t_io_resp;
   signal b_address   : unsigned(10 downto 0);
   signal b_rdata     : std_logic_vector(7 downto 0);
   signal b_wdata     : std_logic_vector(7 downto 0);
   signal b_en        : std_logic;
   signal b_we        : std_logic;
   signal cs_d        : std_logic := '0';
   signal we_d        : std_logic := '0';
   signal access_now  : std_logic;
   signal write_ff00  : std_logic;
   signal m_c128_rst  : std_logic := '0';
   signal m_req_m     : std_logic := '0';
   signal m_req_s     : std_logic := '0';
   signal m_req_d     : std_logic := '0';
   signal m_ack       : std_logic := '0';
   signal m_rdata     : std_logic_vector(7 downto 0) := x"00";

   -- CPU side
   signal q_is_reg    : std_logic;
   signal q_ram_we    : std_logic;
   signal q_ram_q     : std_logic_vector(7 downto 0);
   signal q_req       : std_logic := '0';
   signal q_reg_adr   : std_logic_vector(3 downto 0) := x"0";
   signal q_reg_we    : std_logic := '0';
   signal q_reg_wdata : std_logic_vector(7 downto 0) := x"00";
   signal q_busy      : std_logic := '0';
   signal q_done      : std_logic := '0';
   signal q_ack_m     : std_logic := '0';
   signal q_ack_s     : std_logic := '0';
   signal q_ack_d     : std_logic := '0';
   signal q_rdata     : std_logic_vector(7 downto 0) := x"00";
   signal q_rst_m     : std_logic := '0';
   signal q_rst_s     : std_logic := '0';
   signal q_was_reset : std_logic := '0';

   attribute ASYNC_REG : string;
   attribute ASYNC_REG of m_req_m, m_req_s, q_ack_m, q_ack_s, q_rst_m, q_rst_s : signal is "TRUE";

begin

   ---------------------------------------------------------------------------------------------
   -- C128 side (main clock)
   ---------------------------------------------------------------------------------------------

   access_now <= main_cs_i and not cs_d;
   -- reu.sv's $FF00 test: the write's first clock
   write_ff00 <= '1' when main_we_i = '1' and we_d = '0' and main_addr_i = x"FF00" else '0';

   slot_req.bus_address   <= unsigned(main_addr_i);
   slot_req.bus_rwn       <= not main_we_i;
   slot_req.bus_write     <= '0';
   slot_req.io_address    <= unsigned(main_addr_i);
   slot_req.io_read       <= access_now and not main_we_i;
   slot_req.io_read_early <= '0';
   slot_req.io_write      <= access_now and main_we_i;
   slot_req.late_write    <= '0';
   slot_req.data          <= main_data_i;
   slot_req.sample_io     <= '0';

   i_protocol : entity work.command_protocol
      port map (
         clock      => main_clk_i,
         reset      => main_rst_i,
         io_req     => io_req,
         io_resp    => io_resp,
         io_irq     => open,                -- the firmware polls
         slot_req   => slot_req,
         slot_resp  => slot_resp,
         freeze     => main_dma_o,
         write_ff00 => write_ff00,
         address    => b_address,
         rdata      => b_rdata,
         wdata      => b_wdata,
         en         => b_en,
         we         => b_we
      ); -- i_protocol

   main_irq_o <= slot_resp.irq;

   -- A register access from the CPU arrives as a toggle; its address and data were stable
   -- before it flipped. The answer goes back the same way.
   p_main : process (main_clk_i)
   begin
      if rising_edge(main_clk_i) then
         cs_d       <= main_cs_i;
         we_d       <= main_we_i;
         m_c128_rst <= main_c128_rst_i;
         if access_now = '1' and main_we_i = '0' then
            main_data_o <= slot_resp.data;
         end if;

         m_req_m <= q_req;
         m_req_s <= m_req_m;
         m_req_d <= m_req_s;
         io_req  <= c_io_req_init;
         if m_req_s /= m_req_d then
            io_req.address <= x"00000" & unsigned(q_reg_adr);
            io_req.data    <= q_reg_wdata;
            io_req.read    <= not q_reg_we;
            io_req.write   <= q_reg_we;
         end if;
         if io_resp.ack = '1' then
            m_rdata <= io_resp.data;
            m_ack   <= not m_ack;
         end if;
      end if;
   end process p_main;

   ---------------------------------------------------------------------------------------------
   -- The buffer: port A for the CPU (QNICE clock, falling edge as the device bus wants),
   -- port B for the protocol
   ---------------------------------------------------------------------------------------------

   q_ram_we <= qnice_ce_i and qnice_we_i and qnice_addr_i(11);

   i_buffer : entity work.dualport_2clk_ram
      generic map (
         ADDR_WIDTH => 11,
         DATA_WIDTH => 8,
         FALLING_A  => true
      )
      port map (
         clock_a   => qnice_clk_i,
         address_a => qnice_addr_i(10 downto 0),
         data_a    => qnice_data_i(7 downto 0),
         wren_a    => q_ram_we,
         q_a       => q_ram_q,
         clock_b   => main_clk_i,
         address_b => std_logic_vector(b_address),
         data_b    => b_wdata,
         wren_b    => b_we and b_en,
         q_b       => b_rdata
      ); -- i_buffer

   ---------------------------------------------------------------------------------------------
   -- CPU side (QNICE clock): devices act on the falling edge
   ---------------------------------------------------------------------------------------------

   q_is_reg     <= '1' when qnice_addr_i(11) = '0' and qnice_addr_i(4) = '0' else '0';
   qnice_wait_o <= qnice_ce_i and q_is_reg and not q_done;
   qnice_data_o <= x"00" & q_ram_q when qnice_addr_i(11) = '1' else
                   x"00" & q_rdata when q_is_reg = '1' else
                   x"000" & "000" & q_was_reset;

   p_qnice : process (qnice_clk_i)
   begin
      if falling_edge(qnice_clk_i) then
         q_ack_m <= m_ack;
         q_ack_s <= q_ack_m;
         q_rst_m <= m_c128_rst;
         q_rst_s <= q_rst_m;

         -- ce is low for at least one falling edge between two accesses
         if qnice_ce_i = '0' then
            q_done <= '0';
         end if;
         if q_busy = '0' then
            q_ack_d <= q_ack_s;
         end if;
         if qnice_ce_i = '1' and q_is_reg = '1' and q_busy = '0' and q_done = '0' then
            q_reg_adr   <= qnice_addr_i(3 downto 0);
            q_reg_we    <= qnice_we_i;
            q_reg_wdata <= qnice_data_i(7 downto 0);
            q_req       <= not q_req;
            q_busy      <= '1';
         end if;
         if q_busy = '1' and q_ack_s /= q_ack_d then
            q_ack_d <= q_ack_s;
            q_rdata <= m_rdata;
            q_busy  <= '0';
            q_done  <= '1';
         end if;

         if qnice_ce_i = '1' and qnice_we_i = '1' and qnice_addr_i(11) = '0' and qnice_addr_i(4) = '1' and
            qnice_data_i(0) = '1' then
            q_was_reset <= '0';
         end if;
         if q_rst_s = '1' then
            q_was_reset <= '1';
         end if;

         if qnice_rst_i = '1' then
            q_busy <= '0';
            q_done <= '0';
         end if;
      end if;
   end process p_qnice;

end architecture synthesis;
