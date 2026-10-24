----------------------------------------------------------------------------------
-- MEGA65 port of the C128 core
--
-- Lets the modem's NEORV32 read its firmware from HyperRAM. The Shell loads
-- /c128/modem.bin with one byte in the low half of each 16-bit word, so CPU
-- byte address A is HyperRAM word G_BASE + A.
--
-- CPU side: NEORV32 XBUS (pipelined Wishbone) on the modem clock. A cache line
-- fill (cti "010") becomes one HyperRAM burst; the cache's remaining requests
-- are answered from it in order. Single reads fetch 4 words. Writes are bus
-- errors: the firmware never writes here.
-- HyperRAM side: an Avalon master on hr_clk. Requests and data cross in
-- LUTRAM FIFOs.
--
-- GPL-3.0-or-later
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity modem_hr_bridge is
   generic (
      G_BASE       : natural;              -- HyperRAM word address of CPU address 0
      G_LINE_BYTES : natural               -- NEORV32 cache line size
   );
   port (
      clk_i          : in  std_logic;
      rst_i          : in  std_logic;
      stb_i          : in  std_logic;
      adr_i          : in  std_logic_vector(19 downto 0);   -- byte offset in the window
      we_i           : in  std_logic;
      cti_i          : in  std_logic_vector(2 downto 0);
      dat_o          : out std_logic_vector(31 downto 0);
      ack_o          : out std_logic;
      err_o          : out std_logic;

      hr_clk_i           : in  std_logic;
      hr_rst_i           : in  std_logic;
      hr_write_o         : out std_logic;
      hr_read_o          : out std_logic;
      hr_address_o       : out std_logic_vector(31 downto 0);
      hr_writedata_o     : out std_logic_vector(15 downto 0);
      hr_byteenable_o    : out std_logic_vector(1 downto 0);
      hr_burstcount_o    : out std_logic_vector(7 downto 0);
      hr_readdata_i      : in  std_logic_vector(15 downto 0);
      hr_readdatavalid_i : in  std_logic;
      hr_waitrequest_i   : in  std_logic
   );
end entity modem_hr_bridge;

architecture synthesis of modem_hr_bridge is

   constant C_LINE_WORDS : natural := G_LINE_BYTES / 4;

   -- CPU side
   signal busy        : std_logic := '0';
   signal expect      : natural range 0 to C_LINE_WORDS := 0;   -- CPU words still to answer
   signal requested   : natural range 0 to C_LINE_WORDS := 0;   -- of those, already asked for
   signal req_valid   : std_logic := '0';
   signal req_ready   : std_logic;
   signal req_data    : std_logic_vector(20 downto 0);         -- burst flag & byte offset
   signal rsp_valid   : std_logic;
   signal rsp_ready   : std_logic;
   signal rsp_data    : std_logic_vector(31 downto 0);

   -- HyperRAM side
   signal hr_req_valid : std_logic;
   signal hr_req_ready : std_logic := '0';
   signal hr_req_data  : std_logic_vector(20 downto 0);
   signal hr_busy      : std_logic := '0';
   signal hr_left      : natural range 0 to G_LINE_BYTES := 0;  -- words still to come
   signal hr_byte      : natural range 0 to 3 := 0;
   signal hr_word      : std_logic_vector(31 downto 0) := (others => '0');
   signal hr_rsp_valid : std_logic := '0';
   signal hr_rsp_data  : std_logic_vector(31 downto 0);
   signal hr_rsp_ready : std_logic;
   signal hr_cpu_rst_m : std_logic := '1';
   signal hr_cpu_rst   : std_logic := '1';   -- the CPU side's reset, on hr_clk
   signal hr_keep      : std_logic := '0';   -- deliver this burst (not one from before a CPU reset)

   attribute async_reg : string;
   attribute async_reg of hr_cpu_rst_m : signal is "true";
   attribute async_reg of hr_cpu_rst   : signal is "true";

begin

   -----------------------------------------------------------------------------
   -- CPU side
   -----------------------------------------------------------------------------

   rsp_ready <= '1' when busy = '1' and requested > 0 else '0';

   cpu_proc : process (clk_i)
      variable v_requested : natural range 0 to C_LINE_WORDS;
   begin
      if rising_edge(clk_i) then
         ack_o <= '0';
         err_o <= '0';
         v_requested := requested;

         if req_ready = '1' then
            req_valid <= '0';
         end if;

         if stb_i = '1' then
            if we_i = '1' then
               ack_o <= '1';
               err_o <= '1';
            elsif busy = '0' then
               busy      <= '1';
               req_valid <= '1';
               if cti_i = "010" then
                  expect   <= C_LINE_WORDS;
                  req_data <= '1' & adr_i;
               else
                  expect   <= 1;
                  req_data <= '0' & adr_i;
               end if;
               v_requested := 1;
            else
               v_requested := v_requested + 1;   -- the rest of a line fill
            end if;
         end if;

         if rsp_valid = '1' and rsp_ready = '1' then
            ack_o       <= '1';
            dat_o       <= rsp_data;
            v_requested := v_requested - 1;
            if expect = 1 then
               busy <= '0';
            end if;
            expect <= expect - 1;
         end if;
         requested <= v_requested;

         if rst_i = '1' then
            busy      <= '0';
            req_valid <= '0';
            expect    <= 0;
            requested <= 0;
         end if;
      end if;
   end process cpu_proc;

   i_req_fifo : entity work.axis_fifo_async
      generic map (
         G_ADDR_BITS => 2,
         G_DATA_BITS => 21,
         G_RAM_STYLE => "distributed"
      )
      port map (
         async_rst_i => rst_i,
         s_clk_i     => clk_i,
         s_ready_o   => req_ready,
         s_valid_i   => req_valid,
         s_data_i    => req_data,
         s_fill_o    => open,
         m_clk_i     => hr_clk_i,
         m_ready_i   => hr_req_ready,
         m_valid_o   => hr_req_valid,
         m_data_o    => hr_req_data,
         m_fill_o    => open
      ); -- i_req_fifo

   -- Room for two lines, so HyperRAM data (which can't wait) always fits
   i_rsp_fifo : entity work.axis_fifo_async
      generic map (
         G_ADDR_BITS => 5,
         G_DATA_BITS => 32,
         G_RAM_STYLE => "distributed"
      )
      port map (
         async_rst_i => rst_i,
         s_clk_i     => hr_clk_i,
         s_ready_o   => hr_rsp_ready,
         s_valid_i   => hr_rsp_valid,
         s_data_i    => hr_rsp_data,
         s_fill_o    => open,
         m_clk_i     => clk_i,
         m_ready_i   => rsp_ready,
         m_valid_o   => rsp_valid,
         m_data_o    => rsp_data,
         m_fill_o    => open
      ); -- i_rsp_fifo

   -----------------------------------------------------------------------------
   -- HyperRAM side
   -----------------------------------------------------------------------------

   hr_write_o      <= '0';
   hr_writedata_o  <= (others => '0');
   hr_byteenable_o <= "11";

   hr_proc : process (hr_clk_i)
      variable v_count : natural range 0 to G_LINE_BYTES;
   begin
      if rising_edge(hr_clk_i) then
         hr_req_ready <= '0';
         hr_rsp_valid <= '0';
         hr_cpu_rst_m <= rst_i;
         hr_cpu_rst   <= hr_cpu_rst_m;
         if hr_cpu_rst = '1' then
            hr_keep <= '0';
         end if;

         if hr_read_o = '1' and hr_waitrequest_i = '0' then
            hr_read_o <= '0';
         end if;

         if hr_busy = '0' and hr_req_valid = '1' and hr_req_ready = '0' and hr_cpu_rst = '0' then
            if hr_req_data(20) = '1' then
               v_count := G_LINE_BYTES;
            else
               v_count := 4;
            end if;
            hr_busy         <= '1';
            hr_keep         <= '1';
            hr_req_ready    <= '1';   -- pop the request
            hr_left         <= v_count;
            hr_byte         <= 0;
            hr_read_o       <= '1';
            hr_address_o    <= std_logic_vector(to_unsigned(G_BASE, 32) +
                                                unsigned(hr_req_data(19 downto 2) & "00"));
            hr_burstcount_o <= std_logic_vector(to_unsigned(v_count, 8));
         end if;

         -- Each word carries one byte, little endian within the CPU word
         if hr_busy = '1' and hr_readdatavalid_i = '1' then
            hr_word(8 * hr_byte + 7 downto 8 * hr_byte) <= hr_readdata_i(7 downto 0);
            if hr_byte = 3 then
               hr_rsp_valid <= hr_keep and not hr_cpu_rst;
               hr_rsp_data  <= hr_readdata_i(7 downto 0) & hr_word(23 downto 0);
               hr_byte      <= 0;
            else
               hr_byte <= hr_byte + 1;
            end if;
            if hr_left = 1 then
               hr_busy <= '0';
            end if;
            hr_left <= hr_left - 1;
         end if;

         if hr_rst_i = '1' then
            hr_busy   <= '0';
            hr_read_o <= '0';
         end if;
      end if;
   end process hr_proc;

end architecture synthesis;
