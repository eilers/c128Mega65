----------------------------------------------------------------------------------
-- MEGA65 port of the C128 core
--
-- Lets the modem's NEORV32 read its firmware from HyperRAM. The Shell loads
-- /c128/modem.bin with one byte in the low half of each 16-bit word, so CPU
-- byte address A is HyperRAM word G_BASE + A.
--
-- CPU side: NEORV32 XBUS (pipelined Wishbone) on the modem clock. A cache line
-- fill (cti "010" on its first request) is read in 16-word HyperRAM bursts; the cache's
-- next requests, each the following word, are answered in order. Single reads fetch
-- 4 words. HDMI's scaler takes turns with us on HyperRAM, and a 64-word burst made it
-- miss its deadline on the 80-column screen: pixels from further along the line.
-- Writes are bus errors: the firmware never writes here.
-- The CPU gives up on an access when its bus times out, without telling us. Any
-- request that isn't the next word of the line being filled therefore starts
-- afresh. Requests carry a generation and answers to an older one are dropped,
-- so a late answer never reaches the wrong access.
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
   constant C_HR_TIMEOUT : natural := 2 ** 18;   -- hr_clk cycles (2.6 ms, beyond the CPU's own timeout)
   constant C_HR_BURST   : natural := 16;        -- HyperRAM words per burst of a line fill

   -- CPU side
   signal busy        : std_logic := '0';
   signal gen         : unsigned(1 downto 0) := "00";
   signal expect      : natural range 0 to C_LINE_WORDS := 0;   -- answers still to come
   signal requested   : natural range 0 to C_LINE_WORDS := 0;   -- of those, already asked for
   signal beats_left  : natural range 0 to C_LINE_WORDS := 0;   -- line requests still to come
   signal next_adr    : unsigned(19 downto 0) := (others => '0');
   signal req_valid   : std_logic := '0';
   signal req_ready   : std_logic;
   signal req_data    : std_logic_vector(22 downto 0);         -- generation & burst flag & byte offset
   signal rsp_valid   : std_logic;
   signal rsp_ready   : std_logic;
   signal rsp_data    : std_logic_vector(33 downto 0);         -- generation & word
   signal rsp_current : std_logic;

   -- HyperRAM side
   signal hr_req_valid : std_logic;
   signal hr_req_ready : std_logic := '0';
   signal hr_req_data  : std_logic_vector(22 downto 0);
   signal hr_busy      : std_logic := '0';
   signal hr_gen       : std_logic_vector(1 downto 0) := "00";
   signal hr_left      : natural range 0 to G_LINE_BYTES := 0;  -- words still to come
   signal hr_burst     : natural range 0 to C_HR_BURST := 0;    -- of those, in this burst
   signal hr_byte      : natural range 0 to 3 := 0;
   signal hr_word      : std_logic_vector(31 downto 0) := (others => '0');
   signal hr_rsp_valid : std_logic := '0';
   signal hr_rsp_data  : std_logic_vector(33 downto 0);
   signal hr_rsp_ready : std_logic;
   signal hr_wait      : natural range 0 to C_HR_TIMEOUT - 1 := 0;
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

   -- Answers to an older generation are dropped as they come
   rsp_current <= '1' when unsigned(rsp_data(33 downto 32)) = gen else '0';
   rsp_ready   <= '1' when rsp_current = '0' or (busy = '1' and requested > 0) else '0';

   cpu_proc : process (clk_i)
      variable v_busy      : std_logic;
      variable v_gen       : unsigned(1 downto 0);
      variable v_expect    : natural range 0 to C_LINE_WORDS;
      variable v_requested : natural range 0 to C_LINE_WORDS;
   begin
      if rising_edge(clk_i) then
         ack_o       <= '0';
         err_o       <= '0';
         v_busy      := busy;
         v_gen       := gen;
         v_expect    := expect;
         v_requested := requested;

         if req_ready = '1' then
            req_valid <= '0';
         end if;

         -- An answer for the current request
         if rsp_valid = '1' and rsp_ready = '1' and rsp_current = '1' then
            ack_o       <= '1';
            dat_o       <= rsp_data(31 downto 0);
            v_requested := v_requested - 1;
            v_expect    := v_expect - 1;
            if v_expect = 0 then
               v_busy := '0';
            end if;
         end if;

         if stb_i = '1' then
            if we_i = '1' then
               ack_o <= '1';
               err_o <= '1';
            elsif v_busy = '1' and beats_left > 0 and unsigned(adr_i) = next_adr then
               -- The next word of the line being filled. Its cti is "000" for the last word.
               -- The cache always asks for the whole line, even after a bus error.
               v_requested := v_requested + 1;
               beats_left  <= beats_left - 1;
               next_adr    <= next_adr + 4;
            else
               if v_busy = '1' then              -- the CPU gave up on the last request
                  v_gen := v_gen + 1;
               end if;
               v_busy      := '1';
               v_requested := 1;
               req_valid   <= '1';
               next_adr    <= unsigned(adr_i) + 4;
               if cti_i = "010" then
                  v_expect   := C_LINE_WORDS;
                  beats_left <= C_LINE_WORDS - 1;
                  req_data   <= std_logic_vector(v_gen) & '1' & adr_i;
               else
                  v_expect   := 1;
                  beats_left <= 0;
                  req_data   <= std_logic_vector(v_gen) & '0' & adr_i;
               end if;
            end if;
         end if;

         busy      <= v_busy;
         gen       <= v_gen;
         expect    <= v_expect;
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
         G_DATA_BITS => 23,
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
         G_DATA_BITS => 34,
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
      variable v_burst : natural range 0 to C_HR_BURST;
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
               v_burst := C_HR_BURST;
            else
               v_count := 4;
               v_burst := 4;
            end if;
            hr_busy         <= '1';
            hr_keep         <= '1';
            hr_gen          <= hr_req_data(22 downto 21);
            hr_req_ready    <= '1';   -- pop the request
            hr_left         <= v_count;
            hr_burst        <= v_burst;
            hr_byte         <= 0;
            hr_wait         <= 0;
            hr_read_o       <= '1';
            hr_address_o    <= std_logic_vector(to_unsigned(G_BASE, 32) +
                                                unsigned(hr_req_data(19 downto 2) & "00"));
            hr_burstcount_o <= std_logic_vector(to_unsigned(v_burst, 8));
         end if;

         if hr_busy = '1' then
            if hr_readdatavalid_i = '1' then
               -- Each word carries one byte, little endian within the CPU word
               hr_word(8 * hr_byte + 7 downto 8 * hr_byte) <= hr_readdata_i(7 downto 0);
               if hr_byte = 3 then
                  hr_rsp_valid <= hr_keep and not hr_cpu_rst;
                  hr_rsp_data  <= hr_gen & hr_readdata_i(7 downto 0) & hr_word(23 downto 0);
                  hr_byte      <= 0;
               else
                  hr_byte <= hr_byte + 1;
               end if;
               if hr_left = 1 then
                  hr_busy <= '0';
               elsif hr_burst = 1 then
                  -- The line's next burst: the arbiter can give the scaler a turn first
                  hr_read_o    <= '1';
                  hr_address_o <= std_logic_vector(unsigned(hr_address_o) + C_HR_BURST);
               end if;
               if hr_burst = 1 then
                  hr_burst <= C_HR_BURST;
               else
                  hr_burst <= hr_burst - 1;
               end if;
               hr_left <= hr_left - 1;
               hr_wait <= 0;
            elsif hr_wait = C_HR_TIMEOUT - 1 then
               -- HyperRAM never finished this burst: give up rather than wait forever
               hr_busy   <= '0';
               hr_read_o <= '0';
            else
               hr_wait <= hr_wait + 1;
            end if;
         end if;

         if hr_rst_i = '1' then
            hr_busy   <= '0';
            hr_read_o <= '0';
         end if;
      end if;
   end process hr_proc;

end architecture synthesis;
