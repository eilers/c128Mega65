----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- This module handles the QNICE "Control and Status" interface of the
-- sw_cartridge_wrapper modulde.
--
-- done by MJoergen in 2023 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.qnice_csr_pkg.all;
use work.sdhost_pkg.all;

entity sw_cartridge_csr is
generic (
   G_BASE_ADDRESS : std_logic_vector(21 downto 0);
   G_MAX_HI       : std_logic_vector(15 downto 0) := x"0020";  -- largest file, in 64 kB (CSR $020)
   G_STATS        : boolean := false                           -- the Shell shows CSR $021-$022
);
port (
   qnice_clk_i               : in  std_logic;
   qnice_rst_i               : in  std_logic;
   qnice_addr_i              : in  std_logic_vector(27 downto 0);
   qnice_data_i              : in  std_logic_vector(15 downto 0);
   qnice_ce_i                : in  std_logic;
   qnice_we_i                : in  std_logic;
   qnice_data_o              : out std_logic_vector(15 downto 0);
   qnice_wait_o              : out std_logic;

   qnice_req_length_o        : out std_logic_vector(22 downto 0);
   qnice_req_valid_o         : out std_logic;
   qnice_resp_status_i       : in  std_logic_vector( 3 downto 0);
   qnice_resp_error_i        : in  std_logic_vector( 3 downto 0);
   qnice_resp_address_i      : in  std_logic_vector(22 downto 0);

   qnice_avm_write_o         : out std_logic;
   qnice_avm_read_o          : out std_logic;
   qnice_avm_address_o       : out std_logic_vector(31 downto 0);
   qnice_avm_writedata_o     : out std_logic_vector(15 downto 0);
   qnice_avm_byteenable_o    : out std_logic_vector( 1 downto 0);
   qnice_avm_burstcount_o    : out std_logic_vector( 7 downto 0);
   qnice_avm_readdata_i      : in  std_logic_vector(15 downto 0);
   qnice_avm_readdatavalid_i : in  std_logic;
   qnice_avm_waitrequest_i   : in  std_logic;

   -- SD controller's engine port: the Shell loads the file by DMA (sd_dma.vhd)
   qnice_sd_want_o           : out std_logic;
   qnice_sd_eng_o            : out t_sd_c2q;
   qnice_sd_eng_i            : in  t_sd_q2c := (others => '0');

   -- How often the C128 waited for a bank (low word) and its longest wait (high word)
   qnice_stats_i             : in  std_logic_vector(31 downto 0) := (others => '0');
   qnice_stats_clr_o         : out std_logic;                        -- toggles to clear them

   -- UCI's EASYFLASH erase: the sector (bit 6 ROMH's chip, bits 5-0 a bank of it), a toggle
   -- for each start, and the toggle back once the core has done it
   qnice_erase_o             : out std_logic_vector(6 downto 0);
   qnice_erase_req_o         : out std_logic;
   qnice_erase_ack_i         : in  std_logic := '0'
);
end entity sw_cartridge_csr;

architecture synthesis of sw_cartridge_csr is

   signal qnice_csr               : std_logic;
   signal qnice_csr_wait          : std_logic;
   signal qnice_csr_data          : std_logic_vector(15 downto 0);
   signal qnice_req_status        : std_logic_vector( 3 downto 0);

   -- Read-only: the largest file the memory behind this device takes, so the Shell can check
   constant C_CSR_MAX_HI          : std_logic_vector(11 downto 0) := X"020";

   -- Bank waits since the last clear (sw_cartridge_wrapper), which the Shell's menu shows if G_STATS
   constant C_CSR_STALLS          : std_logic_vector(11 downto 0) := X"021";  -- R: count, W: clear
   constant C_CSR_STALL_MAX       : std_logic_vector(11 downto 0) := X"022";  -- R: longest, C64 cycles
   constant C_CSR_STATS           : std_logic_vector(11 downto 0) := X"023";  -- R: 1 = show them

   signal stats_clr               : std_logic := '0';

   -- UCI's EASYFLASH erase (sw_cartridge_wrapper, crt_flash): the sector, then a start; busy
   -- until the core has erased it, or seen that there is no EasyFlash to erase
   constant C_CSR_ERASE           : std_logic_vector(11 downto 0) := X"024";  -- bit 6 ROMH, 5-0 a bank
   constant C_CSR_ERASE_GO        : std_logic_vector(11 downto 0) := X"025";  -- W: start, R: bit 0 busy

   signal erase_sel               : std_logic_vector(6 downto 0) := (others => '0');
   signal erase_req               : std_logic := '0';

   signal qnice_hr_ce             : std_logic;
   signal qnice_hr_addr           : std_logic_vector(31 downto 0);
   signal qnice_hr_wait           : std_logic;
   signal qnice_hr_data           : std_logic_vector(15 downto 0);
   signal qnice_hr_byteenable     : std_logic_vector( 1 downto 0);

   -- DMA from the SD card (CSR $030..$038): the Shell checks the magic, then sets up and
   -- starts one run of consecutive blocks after the other
   constant C_CSR_DMA_MAGIC       : std_logic_vector(11 downto 0) := X"030";  -- R: 0x5D4A
   constant C_CSR_DMA_CTRL        : std_logic_vector(11 downto 0) := X"031";  -- W: start, R: bit 0 busy, bit 1 error
   constant C_CSR_DMA_BLK_LO      : std_logic_vector(11 downto 0) := X"032";  -- first SD block
   constant C_CSR_DMA_BLK_HI      : std_logic_vector(11 downto 0) := X"033";
   constant C_CSR_DMA_COUNT       : std_logic_vector(11 downto 0) := X"034";  -- blocks
   constant C_CSR_DMA_DEST_LO     : std_logic_vector(11 downto 0) := X"035";  -- byte offset in the file
   constant C_CSR_DMA_DEST_HI     : std_logic_vector(11 downto 0) := X"036";
   constant C_CSR_DMA_LEN_LO      : std_logic_vector(11 downto 0) := X"037";  -- file length: nothing is
   constant C_CSR_DMA_LEN_HI      : std_logic_vector(11 downto 0) := X"038";  -- written beyond it

   signal dma_blk                 : std_logic_vector(31 downto 0) := (others => '0');
   signal dma_count               : std_logic_vector(15 downto 0) := (others => '0');
   signal dma_dest                : std_logic_vector(31 downto 0) := (others => '0');
   signal dma_len                 : std_logic_vector(31 downto 0) := (others => '0');
   signal dma_req                 : std_logic := '0';                 -- toggled by a start (falling edge)
   signal dma_seen                : std_logic := '0';
   signal dma_start               : std_logic := '0';
   signal dma_busy                : std_logic;
   signal dma_error               : std_logic;
   signal dma_avm_write           : std_logic;
   signal dma_avm_address         : std_logic_vector(31 downto 0);
   signal dma_avm_writedata       : std_logic_vector(15 downto 0);
   signal dma_avm_byteenable      : std_logic_vector( 1 downto 0);

   -- qnice2hyperram's side of the memory port, which the DMA takes over while it runs
   signal q2h_write               : std_logic;
   signal q2h_read                : std_logic;
   signal q2h_address             : std_logic_vector(31 downto 0);
   signal q2h_writedata           : std_logic_vector(15 downto 0);
   signal q2h_byteenable          : std_logic_vector( 1 downto 0);
   signal q2h_burstcount          : std_logic_vector( 7 downto 0);
   signal q2h_waitrequest         : std_logic;

   constant C_ERROR_STRINGS : string_vector(0 to 15) := (
     "OK                 \n",
     "Missing CRT header \n",
     "Missing CHIP header\n",
     "Wrong CRT header   \n",
     "Wrong CHIP header  \n",
     "Truncated CHIP     \n",
     "Unsupported type   \n",
     "OK                 \n",
     "OK                 \n",
     "OK                 \n",
     "OK                 \n",
     "OK                 \n",
     "OK                 \n",
     "OK                 \n",
     "OK                 \n",
     "OK                 \n");

begin

   -- Handle the generic framework CSR registers
   i_qnice_csr : entity work.qnice_csr
      generic map (
         G_ERROR_STRINGS => C_ERROR_STRINGS
      )
      port map (
         qnice_clk_i          => qnice_clk_i,
         qnice_rst_i          => qnice_rst_i,
         qnice_addr_i         => qnice_addr_i,
         qnice_data_i         => qnice_data_i,
         qnice_ce_i           => qnice_ce_i,
         qnice_we_i           => qnice_we_i,
         qnice_data_o         => qnice_csr_data,
         qnice_wait_o         => qnice_csr_wait,
         qnice_csr_o          => qnice_csr,
         qnice_req_status_o   => qnice_req_status,
         qnice_req_length_o   => qnice_req_length_o,
         qnice_resp_status_i  => qnice_resp_status_i,
         qnice_resp_error_i   => qnice_resp_error_i,
         qnice_resp_address_i => qnice_resp_address_i
      ); -- i_qnice_csr

   qnice_req_valid_o <= '1' when qnice_req_status = C_CSR_REQ_OK else '0';

   p_read : process (all)
   begin
      qnice_data_o <= x"0000"; -- By default read back zeros.
      qnice_wait_o <= '0';

      if qnice_ce_i = '1' then
         case qnice_csr is
            when '0' =>
               qnice_wait_o <= qnice_hr_wait;
               if qnice_addr_i(0) = '1' then
                  qnice_data_o <= X"00" & qnice_hr_data(15 downto 8);
               else
                  qnice_data_o <= X"00" & qnice_hr_data(7 downto 0);
               end if;

            when '1' =>
               qnice_wait_o <= qnice_csr_wait;
               qnice_data_o <= qnice_csr_data;
               if qnice_addr_i(11 downto 0) = C_CSR_MAX_HI then
                  qnice_data_o <= G_MAX_HI;
               elsif qnice_addr_i(11 downto 0) = C_CSR_STALLS then
                  qnice_data_o <= qnice_stats_i(15 downto 0);
               elsif qnice_addr_i(11 downto 0) = C_CSR_STALL_MAX then
                  qnice_data_o <= qnice_stats_i(31 downto 16);
               elsif qnice_addr_i(11 downto 0) = C_CSR_STATS then
                  qnice_data_o <= x"0000";
                  if G_STATS then
                     qnice_data_o <= x"0001";
                  end if;
               elsif qnice_addr_i(11 downto 0) = C_CSR_ERASE then
                  qnice_data_o <= X"00" & '0' & erase_sel;
               elsif qnice_addr_i(11 downto 0) = C_CSR_ERASE_GO then
                  qnice_data_o <= X"000" & "000" & (erase_req xor qnice_erase_ack_i);
               elsif qnice_addr_i(11 downto 0) = C_CSR_DMA_MAGIC then
                  qnice_data_o <= X"5D4A";
               elsif qnice_addr_i(11 downto 0) = C_CSR_DMA_CTRL then
                  -- busy from the start write on, before the DMA has seen it
                  qnice_data_o <= X"000" & "00" & dma_error &
                                  (dma_busy or dma_start or (dma_req xor dma_seen));
               end if;

            when others =>
               null;
         end case;
      end if;
   end process p_read;

   qnice_hr_ce <= qnice_ce_i and not qnice_csr;
   qnice_hr_addr <= std_logic_vector(("00000" & unsigned(qnice_addr_i(27 downto 1))) +
                                     ("0000000000" & unsigned(G_BASE_ADDRESS)));
   qnice_hr_byteenable <= "10" when qnice_addr_i(0) = '1'
                     else "01";

   i_qnice2hyperram : entity work.qnice2hyperram
      port map (
         clk_i                 => qnice_clk_i,
         rst_i                 => qnice_rst_i,
         s_qnice_wait_o        => qnice_hr_wait,
         s_qnice_address_i     => qnice_hr_addr,
         s_qnice_cs_i          => qnice_hr_ce,
         s_qnice_write_i       => qnice_we_i,
         s_qnice_writedata_i   => qnice_data_i(7 downto 0) & qnice_data_i(7 downto 0),
         s_qnice_byteenable_i  => qnice_hr_byteenable,
         s_qnice_readdata_o    => qnice_hr_data,
         m_avm_write_o         => q2h_write,
         m_avm_read_o          => q2h_read,
         m_avm_address_o       => q2h_address,
         m_avm_writedata_o     => q2h_writedata,
         m_avm_byteenable_o    => q2h_byteenable,
         m_avm_burstcount_o    => q2h_burstcount,
         m_avm_readdata_i      => qnice_avm_readdata_i,
         m_avm_readdatavalid_i => qnice_avm_readdatavalid_i,
         m_avm_waitrequest_i   => q2h_waitrequest
      ); -- i_qnice2hyperram

   ---------------------------------------------------
   -- DMA from the SD card
   ---------------------------------------------------

   p_dma_regs : process (qnice_clk_i)
   begin
      if falling_edge(qnice_clk_i) then
         if qnice_csr = '1' and qnice_we_i = '1' then
            case qnice_addr_i(11 downto 0) is
               when C_CSR_DMA_CTRL    => dma_req                 <= not dma_req;
               when C_CSR_DMA_BLK_LO  => dma_blk(15 downto 0)    <= qnice_data_i;
               when C_CSR_DMA_BLK_HI  => dma_blk(31 downto 16)   <= qnice_data_i;
               when C_CSR_DMA_COUNT   => dma_count               <= qnice_data_i;
               when C_CSR_DMA_DEST_LO => dma_dest(15 downto 0)   <= qnice_data_i;
               when C_CSR_DMA_DEST_HI => dma_dest(31 downto 16)  <= qnice_data_i;
               when C_CSR_DMA_LEN_LO  => dma_len(15 downto 0)    <= qnice_data_i;
               when C_CSR_DMA_LEN_HI  => dma_len(31 downto 16)   <= qnice_data_i;
               when C_CSR_STALLS      => stats_clr               <= not stats_clr;
               when C_CSR_ERASE       => erase_sel               <= qnice_data_i(6 downto 0);
               when C_CSR_ERASE_GO    => erase_req               <= not erase_req;
               when others => null;
            end case;
         end if;
      end if;
   end process p_dma_regs;

   p_dma_start : process (qnice_clk_i)
   begin
      if rising_edge(qnice_clk_i) then
         dma_start <= dma_req xor dma_seen;
         dma_seen  <= dma_req;
      end if;
   end process p_dma_start;

   i_sd_dma : entity work.sd_dma
      port map (
         clk_i             => qnice_clk_i,
         rst_i             => qnice_rst_i,
         start_i           => dma_start,
         blk_i             => dma_blk,
         count_i           => dma_count,
         dest_i            => dma_dest,
         length_i          => dma_len,
         busy_o            => dma_busy,
         error_o           => dma_error,
         eng_o             => qnice_sd_eng_o,
         eng_i             => qnice_sd_eng_i,
         avm_write_o       => dma_avm_write,
         avm_address_o     => dma_avm_address,
         avm_writedata_o   => dma_avm_writedata,
         avm_byteenable_o  => dma_avm_byteenable,
         avm_waitrequest_i => qnice_avm_waitrequest_i
      ); -- i_sd_dma

   qnice_sd_want_o        <= dma_busy;
   qnice_stats_clr_o      <= stats_clr;
   qnice_erase_o          <= erase_sel;
   qnice_erase_req_o      <= erase_req;

   -- The DMA owns the memory port while it runs; QNICE does not touch the file meanwhile
   qnice_avm_write_o      <= dma_avm_write when dma_busy = '1' else q2h_write;
   qnice_avm_read_o       <= '0' when dma_busy = '1' else q2h_read;
   qnice_avm_address_o    <= std_logic_vector(unsigned(dma_avm_address) + unsigned(G_BASE_ADDRESS))
                             when dma_busy = '1' else q2h_address;
   qnice_avm_writedata_o  <= dma_avm_writedata when dma_busy = '1' else q2h_writedata;
   qnice_avm_byteenable_o <= dma_avm_byteenable when dma_busy = '1' else q2h_byteenable;
   qnice_avm_burstcount_o <= X"01" when dma_busy = '1' else q2h_burstcount;
   q2h_waitrequest        <= '1' when dma_busy = '1' else qnice_avm_waitrequest_i;

end architecture synthesis;

