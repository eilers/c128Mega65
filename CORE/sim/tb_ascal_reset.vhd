----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Testbench for HDMI after a reset. A short press of the reset button also resets
-- HyperRAM and the scaler (ascal). When that lands while ascal is writing to its frame
-- buffer, ascal must not leave a burst half done, or HyperRAM stays locked up and HDMI
-- freezes until power-off.
--
-- ascal and avm_decrease as in digital_pipeline, a small picture in and out, and a model
-- of the HyperRAM controller: a write burst only ends after all its words, and a read
-- that comes in the middle of one is lost. Other masters take turns on the bus, so ascal
-- often waits with a write. Each reset is pressed while ascal is writing; afterwards
-- ascal's reads must come back again, with no read lost and no burst left open.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_ascal_reset is
end entity tb_ascal_reset;

architecture sim of tb_ascal_reset is

   constant C_RESETS    : natural := 4;
   constant C_INIT      : natural := 15_000;   -- hr_clk cycles: the controller's 150 us after reset
   constant C_STUCK     : natural := 10_000;   -- hr_clk cycles a write burst may wait for its next word
   constant C_MIN_READS : natural := 50;       -- read bursts expected in the window after a reset

   signal hr_clk  : std_logic := '0';
   signal i_clk   : std_logic := '0';
   signal o_clk   : std_logic := '0';
   signal hr_rst  : std_logic := '1';
   signal reset_na : std_logic;
   signal done    : boolean := false;

   -- Input picture: 128 x 32 of 160 x 40, one pixel every other i_clk
   signal i_ce    : std_logic := '0';
   signal i_hcnt  : natural range 0 to 159 := 0;
   signal i_vcnt  : natural range 0 to 39 := 0;
   signal i_hs    : std_logic := '0';
   signal i_vs    : std_logic := '0';
   signal i_de    : std_logic := '0';
   signal i_pix   : unsigned(7 downto 0) := (others => '0');

   -- ascal's Avalon port and avm_decrease's
   signal w_write, w_read, w_wait, w_rdv : std_logic;
   signal w_address    : std_logic_vector(18 downto 0);
   signal w_writedata  : std_logic_vector(127 downto 0);
   signal w_readdata   : std_logic_vector(127 downto 0);
   signal w_byteenable : std_logic_vector(15 downto 0);
   signal w_burstcount : std_logic_vector(7 downto 0);
   signal m_write, m_read : std_logic;
   signal m_wait       : std_logic := '1';
   signal m_rdv        : std_logic := '0';
   signal m_address    : std_logic_vector(21 downto 0);
   signal m_writedata  : std_logic_vector(15 downto 0);
   signal m_readdata   : std_logic_vector(15 downto 0) := (others => '0');
   signal m_byteenable : std_logic_vector(1 downto 0);
   signal m_burstcount : std_logic_vector(7 downto 0);

   -- HyperRAM controller model
   type t_state is (INIT_ST, IDLE_ST, BUSY_ST, WLAT_ST, WBURST_ST, RLAT_ST, RDATA_ST, RECOVERY_ST);
   signal state      : t_state := INIT_ST;
   signal cnt        : natural := 0;
   signal left       : natural := 0;
   signal quiet      : natural := 0;
   signal lfsr       : unsigned(15 downto 0) := x"ACE1";
   signal reads_done : natural := 0;
   signal lost_reads : natural := 0;
   signal stuck      : natural := 0;

begin

   hr_clk <= not hr_clk after 5 ns     when not done;   -- 100 MHz
   i_clk  <= not i_clk  after 15.625 ns when not done;  -- 32 MHz, the VDC's clock
   o_clk  <= not o_clk  after 6.734 ns when not done;   -- 74.25 MHz

   reset_na <= not hr_rst;

   video_proc : process (i_clk)
   begin
      if rising_edge(i_clk) then
         i_ce <= not i_ce;
         if i_ce = '1' then
            if i_hcnt = 159 then
               i_hcnt <= 0;
               i_vcnt <= 0 when i_vcnt = 39 else i_vcnt + 1;
            else
               i_hcnt <= i_hcnt + 1;
            end if;
            i_de  <= '1' when i_hcnt < 128 and i_vcnt < 32 else '0';
            i_hs  <= '1' when i_hcnt >= 136 and i_hcnt < 144 else '0';
            i_vs  <= '1' when i_vcnt >= 34 and i_vcnt < 36 else '0';
            i_pix <= i_pix + 1;
         end if;
      end if;
   end process video_proc;

   i_ascal : entity work.ascal
      generic map (
         MASK         => x"ff",
         RAMBASE      => (others => '0'),
         RAMSIZE      => to_unsigned(2**16, 32),
         INTER        => false,
         HEADER       => false,
         DOWNSCALE    => true,
         DOWNSCALE_NN => false,
         BYTESWAP     => true,
         ADAPTIVE     => true,
         PALETTE      => false,
         PALETTE2     => false,
         FRAC         => 6,
         OHRES        => 2048,
         IHRES        => 1024,
         N_DW         => 128,
         N_AW         => 19,
         N_BURST      => 128
      )
      port map (
         i_r => i_pix, i_g => not i_pix, i_b => i_pix,
         i_hs => i_hs, i_vs => i_vs, i_fl => '0', i_de => i_de, i_ce => i_ce, i_clk => i_clk,
         o_r => open, o_g => open, o_b => open, o_hs => open, o_vs => open, o_de => open,
         o_vbl => open, o_ce => '1', o_clk => o_clk, o_border => x"000000",
         o_fb_ena => '0', o_fb_hsize => 0, o_fb_vsize => 0, o_fb_format => "000101",
         o_fb_base => x"0000_0000", o_fb_stride => (others => '0'),
         pal1_clk => '0', pal1_dw => x"000000000000", pal1_dr => open, pal1_a => "0000000",
         pal1_wr => '0', pal_n => '0',
         pal2_clk => '0', pal2_dw => x"000000", pal2_dr => open, pal2_a => "00000000", pal2_wr => '0',
         o_lltune => open,
         iauto => '1', himin => 0, himax => 0, vimin => 0, vimax => 0,
         i_hdmax => open, i_vdmax => open,
         run => '1', freeze => '0', mode => "00000",
         htotal => 200, hsstart => 170, hsend => 180, hdisp => 160,
         vtotal => 60, vsstart => 50, vsend => 52, vdisp => 48,
         hmin => 0, hmax => 159, vmin => 0, vmax => 47,
         format => "01",
         poly_clk => '0', poly_dw => (others => '0'), poly_a => (others => '0'), poly_wr => '0',
         avl_clk           => hr_clk,
         avl_waitrequest   => w_wait,
         avl_readdata      => w_readdata,
         avl_readdatavalid => w_rdv,
         avl_burstcount    => w_burstcount,
         avl_writedata     => w_writedata,
         avl_address       => w_address,
         avl_write         => w_write,
         avl_read          => w_read,
         avl_byteenable    => w_byteenable,
         reset_na          => reset_na
      ); -- i_ascal

   i_avm_decrease : entity work.avm_decrease
      generic map (
         G_SLAVE_ADDRESS_SIZE  => 19,
         G_SLAVE_DATA_SIZE     => 128,
         G_MASTER_ADDRESS_SIZE => 22,
         G_MASTER_DATA_SIZE    => 16
      )
      port map (
         clk_i                 => hr_clk,
         rst_i                 => hr_rst,
         s_avm_write_i         => w_write,
         s_avm_read_i          => w_read,
         s_avm_address_i       => w_address,
         s_avm_writedata_i     => w_writedata,
         s_avm_byteenable_i    => w_byteenable,
         s_avm_burstcount_i    => w_burstcount,
         s_avm_readdata_o      => w_readdata,
         s_avm_readdatavalid_o => w_rdv,
         s_avm_waitrequest_o   => w_wait,
         m_avm_write_o         => m_write,
         m_avm_read_o          => m_read,
         m_avm_address_o       => m_address,
         m_avm_writedata_o     => m_writedata,
         m_avm_byteenable_o    => m_byteenable,
         m_avm_burstcount_o    => m_burstcount,
         m_avm_readdata_i      => m_readdata,
         m_avm_readdatavalid_i => m_rdv,
         m_avm_waitrequest_i   => m_wait
      ); -- i_avm_decrease

   -- The HyperRAM controller as hyperram_ctrl behaves: a command is taken while waitrequest
   -- is low, a write burst takes words only while write is high, and a read in the middle
   -- of a write burst is taken and never answered. Between commands another master
   -- sometimes has the bus for a while.
   hyperram_proc : process (hr_clk)
      procedure next_turn is
      begin
         if lfsr(0) = '1' then
            m_wait <= '1';
            cnt    <= 20 + to_integer(lfsr(6 downto 1));
            state  <= BUSY_ST;
         else
            m_wait <= '0';
            state  <= IDLE_ST;
         end if;
      end procedure next_turn;
   begin
      if rising_edge(hr_clk) then
         lfsr  <= lfsr(14 downto 0) & (lfsr(15) xor lfsr(13) xor lfsr(12) xor lfsr(10));
         m_rdv <= '0';

         case state is
            when INIT_ST =>
               if cnt = 0 then
                  next_turn;
               else
                  cnt <= cnt - 1;
               end if;

            when IDLE_ST =>
               if m_write = '1' then
                  left   <= to_integer(unsigned(m_burstcount)) - 1;
                  quiet  <= 0;
                  m_wait <= '1';
                  cnt    <= 6;
                  state  <= WLAT_ST;
               elsif m_read = '1' then
                  left   <= to_integer(unsigned(m_burstcount));
                  m_wait <= '1';
                  cnt    <= 8;
                  state  <= RLAT_ST;
               end if;

            when BUSY_ST =>
               if cnt = 0 then
                  m_wait <= '0';
                  state  <= IDLE_ST;
               else
                  cnt <= cnt - 1;
               end if;

            when WLAT_ST =>
               if cnt = 0 then
                  m_wait <= '0';
                  state  <= WBURST_ST;
               else
                  cnt <= cnt - 1;
               end if;

            when WBURST_ST =>
               if m_read = '1' then
                  report "a read came in the middle of a write burst: it is lost" severity error;
                  lost_reads <= lost_reads + 1;
               end if;
               if m_write = '1' then
                  quiet <= 0;
                  left  <= left - 1;
                  if left = 1 then
                     m_wait <= '1';
                     cnt    <= 2;
                     state  <= RECOVERY_ST;
                  end if;
               else
                  quiet <= quiet + 1;
                  if quiet = C_STUCK then
                     report "a write burst waits for " & integer'image(left) & " more words" severity error;
                     stuck <= stuck + 1;
                  end if;
               end if;

            when RLAT_ST =>
               if cnt = 0 then
                  state <= RDATA_ST;
               else
                  cnt <= cnt - 1;
               end if;

            when RDATA_ST =>
               m_rdv      <= '1';
               m_readdata <= std_logic_vector(lfsr);
               left       <= left - 1;
               if left = 1 then
                  reads_done <= reads_done + 1;
                  cnt        <= 1;
                  state      <= RECOVERY_ST;
               end if;

            when RECOVERY_ST =>
               if cnt = 0 then
                  next_turn;
               else
                  cnt <= cnt - 1;
               end if;
         end case;

         if hr_rst = '1' then
            m_wait <= '1';
            cnt    <= C_INIT;
            state  <= INIT_ST;
         end if;
      end if;
   end process hyperram_proc;

   -- Like xpm_cdc_async_rst: the reset starts at any moment and ends on an hr_clk edge
   test_proc : process
      variable v_reads  : natural;
      variable v_errors : natural := 0;
   begin
      wait for 1 us;
      wait until rising_edge(hr_clk);
      hr_rst <= '0';
      wait for 1500 us;

      for i in 1 to C_RESETS loop
         wait until rising_edge(hr_clk) and w_write = '1';
         wait for 3 ns;
         hr_rst <= '1';
         wait for 20 us;
         wait until rising_edge(hr_clk);
         hr_rst <= '0';
         v_reads := reads_done;
         wait for 750 us;
         report "reset " & integer'image(i) & ": " & integer'image(reads_done - v_reads) &
                " read bursts since, " & integer'image(lost_reads) & " lost, " &
                integer'image(stuck) & " bursts left open";
         if reads_done - v_reads < C_MIN_READS or lost_reads /= 0 or stuck /= 0 then
            v_errors := v_errors + 1;
         end if;
      end loop;

      if v_errors = 0 then
         report "ALL CHECKS PASSED";
      else
         report "FAILED after " & integer'image(v_errors) & " of " & integer'image(C_RESETS) & " resets" severity error;
      end if;
      done <= true;
      wait;
   end process test_proc;

end architecture sim;
