----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- The system CPU's bus DMA: it takes the C128's bus on the core's DMA port, as the REU
-- does, to read or write C128 memory as the CPU sees it (the MMU's banks, ROMs, I/O). The
-- REST API's readmem, writemem, pause and resume use it, as an Ultimate's do over the
-- cartridge port.
--
-- Device words (C_DEV_DMA), one byte each in the low half:
--   0x000  bit 0 hold: the bus stays the firmware's between jobs (machine:pause); a C128
--          reset clears it, as it resumes an Ultimate's C64
--   0x001  the C128 address of the next job
--   0x002  its length in bytes, 1-2048
--   0x003  W: start a job: bit 0 write (buffer -> C128), else read; bit 1 stop the job
--          R: bit 0 a job runs, bit 1 the CPU is stopped (the DMA has had the bus since its
--          request went up), bit 2 the Z80 runs: the core takes no DMA then
--   0x800-0xFFF  the buffer: the job's byte n at 0x800 + n
--
-- C128 side: like the REU (reu.sv), a byte's address goes out while dma_cycle is low and
-- stays for the whole 16-clock DMA window; a read takes dma_din at its end. main.vhd hands
-- this the window only when neither a cartridge nor the REU has the bus.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity c128_dma is
   port (
      -- C128 side (main clock)
      main_clk_i      : in  std_logic;
      main_rst_i      : in  std_logic;
      main_c128_rst_i : in  std_logic;                      -- the C128's reset: it ends a hold
      main_req_o      : out std_logic;                      -- hold the CPU
      main_cycle_i    : in  std_logic;                      -- the DMA window, ours
      main_addr_o     : out unsigned(15 downto 0) := (others => '0');
      main_dout_o     : out unsigned(7 downto 0) := (others => '0');
      main_we_o       : out std_logic;
      main_din_i      : in  unsigned(7 downto 0);
      main_z80_i      : in  std_logic;                      -- the Z80 has the bus

      -- Device bus (QNICE clock)
      qnice_clk_i     : in  std_logic;
      qnice_rst_i     : in  std_logic;
      qnice_addr_i    : in  std_logic_vector(11 downto 0);
      qnice_data_i    : in  std_logic_vector(15 downto 0);
      qnice_ce_i      : in  std_logic;
      qnice_we_i      : in  std_logic;
      qnice_data_o    : out std_logic_vector(15 downto 0)
   );
end entity c128_dma;

architecture synthesis of c128_dma is

   -- CPU side
   signal q_hold      : std_logic := '0';
   signal q_addr      : std_logic_vector(15 downto 0) := (others => '0');
   signal q_len       : std_logic_vector(11 downto 0) := x"001";
   signal q_write     : std_logic := '0';
   signal q_go        : std_logic := '0';               -- toggles: a job
   signal q_stop      : std_logic := '0';               -- toggles: stop it
   signal q_go_we     : std_logic;
   signal q_go_d      : std_logic := '0';
   signal q_done_m    : std_logic := '0';
   signal q_done_s    : std_logic := '0';
   signal q_stat_m    : std_logic_vector(1 downto 0) := "00";
   signal q_stat_s    : std_logic_vector(1 downto 0) := "00";
   signal q_rst_m     : std_logic := '0';
   signal q_rst_s     : std_logic := '0';
   signal q_ram_we    : std_logic;
   signal q_ram_q     : std_logic_vector(7 downto 0);

   -- C128 side
   type t_state is (S_IDLE, S_FETCH, S_SETUP, S_ACCESS, S_NEXT);
   signal state       : t_state := S_IDLE;
   signal m_hold_m    : std_logic := '0';
   signal m_hold_s    : std_logic := '0';
   signal m_go_m      : std_logic := '0';
   signal m_go_s      : std_logic := '0';
   signal m_go_d      : std_logic := '0';
   signal m_stop_m    : std_logic := '0';
   signal m_stop_s    : std_logic := '0';
   signal m_stop_d    : std_logic := '0';
   signal m_done      : std_logic := '0';               -- = the go toggle of the last job done
   signal m_job       : std_logic := '0';
   signal m_granted   : std_logic := '0';
   signal m_write     : std_logic := '0';
   signal m_base      : unsigned(15 downto 0) := (others => '0');
   signal m_len       : unsigned(11 downto 0) := (others => '0');
   signal m_idx       : unsigned(11 downto 0) := (others => '0');
   signal m_cnt       : unsigned(3 downto 0) := (others => '0');
   signal m_we_r      : std_logic := '0';
   signal m_c128_rst  : std_logic := '0';
   signal m_req       : std_logic;
   signal b_wdata     : std_logic_vector(7 downto 0) := x"00";
   signal b_we        : std_logic := '0';
   signal b_q         : std_logic_vector(7 downto 0);
   signal b_addr      : std_logic_vector(10 downto 0);

   attribute ASYNC_REG : string;
   attribute ASYNC_REG of m_hold_m, m_hold_s, m_go_m, m_go_s, m_stop_m, m_stop_s,
                          q_done_m, q_done_s, q_stat_m, q_stat_s, q_rst_m, q_rst_s : signal is "TRUE";

begin

   ---------------------------------------------------------------------------------------------
   -- C128 side (main clock)
   ---------------------------------------------------------------------------------------------

   m_req       <= m_hold_s or m_job;
   main_req_o  <= m_req;
   main_we_o   <= m_we_r and main_cycle_i;
   b_addr      <= std_logic_vector(m_idx(10 downto 0));

   p_main : process (main_clk_i)
   begin
      if rising_edge(main_clk_i) then
         m_hold_m <= q_hold;
         m_hold_s <= m_hold_m;
         m_go_m   <= q_go;
         m_go_s   <= m_go_m;
         m_go_d   <= m_go_s;
         m_stop_m <= q_stop;
         m_stop_s <= m_stop_m;
         m_stop_d <= m_stop_s;
         m_c128_rst <= main_c128_rst_i;
         b_we     <= '0';

         if m_req = '0' then
            m_granted <= '0';
         elsif main_cycle_i = '1' then
            m_granted <= '1';
         end if;

         case state is
            when S_IDLE =>
               -- The job's address, length and direction were stable before its toggle
               if m_go_s /= m_go_d then
                  m_base  <= unsigned(q_addr);
                  m_len   <= unsigned(q_len);
                  m_write <= q_write;
                  m_idx   <= (others => '0');
                  m_job   <= '1';
                  state   <= S_FETCH;
               end if;

            -- The buffer's byte for a write is there a clock after its address
            when S_FETCH =>
               state <= S_SETUP;

            when S_SETUP =>
               if main_cycle_i = '0' then
                  main_addr_o <= m_base + m_idx;
                  main_dout_o <= unsigned(b_q);
                  m_we_r      <= m_write;
                  m_cnt       <= (others => '0');
                  state       <= S_ACCESS;
               end if;

            when S_ACCESS =>
               if main_cycle_i = '1' then
                  m_cnt <= m_cnt + 1;
                  if m_cnt = 15 then
                     main_addr_o <= (others => '0');   -- no device's data read while idle
                     m_we_r      <= '0';
                     b_wdata     <= std_logic_vector(main_din_i);
                     b_we        <= not m_write;       -- at this byte's index, next clock
                     state       <= S_NEXT;
                  end if;
               end if;

            when S_NEXT =>
               m_idx <= m_idx + 1;
               if m_idx + 1 = m_len then
                  m_job  <= '0';
                  m_done <= m_go_d;
                  state  <= S_IDLE;
               else
                  state  <= S_FETCH;
               end if;
         end case;

         if m_stop_s /= m_stop_d or main_rst_i = '1' then
            main_addr_o <= (others => '0');
            m_we_r      <= '0';
            m_job       <= '0';
            m_done      <= m_go_d;
            state       <= S_IDLE;
         end if;
      end if;
   end process p_main;

   ---------------------------------------------------------------------------------------------
   -- The buffer: port A for the CPU (QNICE clock, falling edge as the device bus wants),
   -- port B for the DMA
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
         address_b => b_addr,
         data_b    => b_wdata,
         wren_b    => b_we,
         q_b       => b_q
      ); -- i_buffer

   ---------------------------------------------------------------------------------------------
   -- CPU side (QNICE clock): devices act on the falling edge
   ---------------------------------------------------------------------------------------------

   q_go_we      <= '1' when qnice_ce_i = '1' and qnice_we_i = '1' and qnice_addr_i = x"003" else '0';
   qnice_data_o <= x"00" & q_ram_q             when qnice_addr_i(11) = '1' else
                   x"000" & "000" & q_hold    when qnice_addr_i = x"000" else
                   q_addr                      when qnice_addr_i = x"001" else
                   x"0" & q_len                when qnice_addr_i = x"002" else
                   x"000" & "0" & q_stat_s(1) & q_stat_s(0) & (q_go xor q_done_s);

   p_qnice : process (qnice_clk_i)
   begin
      if falling_edge(qnice_clk_i) then
         q_done_m <= m_done;
         q_done_s <= q_done_m;
         q_stat_m <= main_z80_i & m_granted;
         q_stat_s <= q_stat_m;
         q_rst_m  <= m_c128_rst;
         q_rst_s  <= q_rst_m;

         -- ce can be high for more than one edge, and is low between two accesses
         q_go_d <= q_go_we;
         if qnice_ce_i = '1' and qnice_we_i = '1' then
            case qnice_addr_i is
               when x"000" => q_hold <= qnice_data_i(0);
               when x"001" => q_addr <= qnice_data_i;
               when x"002" => q_len  <= qnice_data_i(11 downto 0);
               when others => null;
            end case;
         end if;
         if q_go_we = '1' and q_go_d = '0' then
            if qnice_data_i(1) = '1' then
               q_stop <= not q_stop;
            else
               q_write <= qnice_data_i(0);
               q_go    <= not q_go;
            end if;
         end if;

         if qnice_rst_i = '1' or q_rst_s = '1' then
            q_hold <= '0';
         end if;
      end if;
   end process p_qnice;

end architecture synthesis;
