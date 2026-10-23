----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Virtual cartridge: the QNICE device that loads a .crt file into HyperRAM, the parser and
-- bank cache behind it, and the cartridge's RAMs. main.vhd runs the cartridge's logic
-- (cartridge.vhd) and reads ROM through the main_* ports.
--
-- After MJoergen's sw_cartridge_wrapper for C64MEGA65 (2023, GPL v3).
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sw_cartridge_wrapper is
generic (
   G_BASE_ADDRESS : std_logic_vector(21 downto 0)   -- word address of the CRT file in HyperRAM
);
port (
   qnice_clk_i         : in  std_logic;
   qnice_rst_i         : in  std_logic;
   qnice_addr_i        : in  std_logic_vector(27 downto 0);
   qnice_data_i        : in  std_logic_vector(15 downto 0);
   qnice_ce_i          : in  std_logic;
   qnice_we_i          : in  std_logic;
   qnice_data_o        : out std_logic_vector(15 downto 0);
   qnice_wait_o        : out std_logic;

   main_clk_i          : in  std_logic;
   main_loaded_o       : out std_logic;                      -- a .crt file is parsed and in use
   main_loading_o      : out std_logic;                      -- the parser is busy
   main_id_o           : out std_logic_vector(15 downto 0);
   main_exrom_o        : out std_logic_vector( 7 downto 0);
   main_game_o         : out std_logic_vector( 7 downto 0);
   main_size_o         : out std_logic_vector(22 downto 0);
   main_c128_o         : out std_logic;
   main_subtype_o      : out std_logic_vector( 7 downto 0);  -- the CRT header's hardware subtype
   main_bank_lo_i      : in  std_logic_vector( 6 downto 0);
   main_bank_hi_i      : in  std_logic_vector( 6 downto 0);
   main_wait_o         : out std_logic;                      -- the selected banks are not cached yet
   main_addr_i         : in  std_logic_vector(15 downto 0);  -- CPU or VIC address
   main_data_i         : in  std_logic_vector( 7 downto 0);  -- CPU write data
   main_lo_data_o      : out std_logic_vector( 7 downto 0);  -- ROML
   main_hi_data_o      : out std_logic_vector( 7 downto 0);  -- ROMH
   main_ioe_we_i       : in  std_logic;
   main_iof_we_i       : in  std_logic;
   main_ram_we_i       : in  std_logic;
   main_ioe_data_o     : out std_logic_vector( 7 downto 0);  -- RAM at $DExx
   main_iof_data_o     : out std_logic_vector( 7 downto 0);  -- RAM at $DFxx
   main_ram_data_o     : out std_logic_vector( 7 downto 0);  -- 32 kB cartridge RAM

   hr_clk_i            : in  std_logic;
   hr_rst_i            : in  std_logic;
   hr_write_o          : out std_logic;
   hr_read_o           : out std_logic;
   hr_address_o        : out std_logic_vector(31 downto 0);
   hr_writedata_o      : out std_logic_vector(15 downto 0);
   hr_byteenable_o     : out std_logic_vector( 1 downto 0);
   hr_burstcount_o     : out std_logic_vector( 7 downto 0);
   hr_readdata_i       : in  std_logic_vector(15 downto 0);
   hr_readdatavalid_i  : in  std_logic;
   hr_waitrequest_i    : in  std_logic
);
end entity sw_cartridge_wrapper;

architecture synthesis of sw_cartridge_wrapper is

   -- 8 slots of 8 kB on each side, i.e. 64 kB of block RAM each for ROML and ROMH
   constant C_CACHE_SIZE  : natural := 3;
   constant C_STAT_READY  : std_logic_vector(3 downto 0) := "0010";
   -- Bits of the HyperRAM -> core crossing
   constant C_HR2MAIN     : natural := 1 + 1 + 16 + 8 + 8 + 23 + 1 + 7 + 7 + 1 + 2*C_CACHE_SIZE + 8;

   signal qnice_req_length        : std_logic_vector(22 downto 0);
   signal qnice_req_valid         : std_logic;
   signal qnice_resp_status       : std_logic_vector( 3 downto 0);
   signal qnice_resp_error        : std_logic_vector( 3 downto 0);
   signal qnice_resp_address      : std_logic_vector(22 downto 0);
   signal qnice_avm_write         : std_logic;
   signal qnice_avm_read          : std_logic;
   signal qnice_avm_address       : std_logic_vector(31 downto 0);
   signal qnice_avm_writedata     : std_logic_vector(15 downto 0);
   signal qnice_avm_byteenable    : std_logic_vector( 1 downto 0);
   signal qnice_avm_burstcount    : std_logic_vector( 7 downto 0);
   signal qnice_avm_readdata      : std_logic_vector(15 downto 0);
   signal qnice_avm_readdatavalid : std_logic;
   signal qnice_avm_waitrequest   : std_logic;

   signal hr_req_length           : std_logic_vector(22 downto 0);
   signal hr_req_valid            : std_logic;
   signal hr_resp_status          : std_logic_vector( 3 downto 0);
   signal hr_resp_error           : std_logic_vector( 3 downto 0);
   signal hr_resp_address         : std_logic_vector(22 downto 0);
   signal hr_qnice_write          : std_logic;
   signal hr_qnice_read           : std_logic;
   signal hr_qnice_address        : std_logic_vector(31 downto 0);
   signal hr_qnice_writedata      : std_logic_vector(15 downto 0);
   signal hr_qnice_byteenable     : std_logic_vector( 1 downto 0);
   signal hr_qnice_burstcount     : std_logic_vector( 7 downto 0);
   signal hr_qnice_readdata       : std_logic_vector(15 downto 0);
   signal hr_qnice_readdatavalid  : std_logic;
   signal hr_qnice_waitrequest    : std_logic;
   signal hr_crt_write            : std_logic;
   signal hr_crt_read             : std_logic;
   signal hr_crt_address          : std_logic_vector(31 downto 0) := (others => '0');
   signal hr_crt_writedata        : std_logic_vector(15 downto 0);
   signal hr_crt_byteenable       : std_logic_vector( 1 downto 0);
   signal hr_crt_burstcount       : std_logic_vector( 7 downto 0);
   signal hr_crt_readdata         : std_logic_vector(15 downto 0);
   signal hr_crt_readdatavalid    : std_logic;
   signal hr_crt_waitrequest      : std_logic;

   signal hr_bram_address         : std_logic_vector(C_CACHE_SIZE+11 downto 0);
   signal hr_bram_data            : std_logic_vector(15 downto 0);
   signal hr_bram_lo_wren         : std_logic;
   signal hr_bram_hi_wren         : std_logic;
   signal hr_bank_lo              : std_logic_vector( 6 downto 0);
   signal hr_bank_hi              : std_logic_vector( 6 downto 0);
   signal hr_ack_lo               : std_logic_vector( 6 downto 0);
   signal hr_ack_hi               : std_logic_vector( 6 downto 0);
   signal hr_ready                : std_logic;
   signal hr_cache_addr_lo        : std_logic_vector(C_CACHE_SIZE-1 downto 0);
   signal hr_cache_addr_hi        : std_logic_vector(C_CACHE_SIZE-1 downto 0);
   signal hr_id                   : std_logic_vector(15 downto 0);
   signal hr_exrom                : std_logic_vector( 7 downto 0);
   signal hr_game                 : std_logic_vector( 7 downto 0);
   signal hr_size                 : std_logic_vector(22 downto 0);
   signal hr_c128                 : std_logic;
   signal hr_subtype              : std_logic_vector( 7 downto 0);
   signal hr_loaded               : std_logic;
   signal hr_loading              : std_logic;

   signal main_ack_lo             : std_logic_vector( 6 downto 0);
   signal main_ack_hi             : std_logic_vector( 6 downto 0);
   signal main_ready              : std_logic;
   signal main_loaded             : std_logic;
   signal main_c128               : std_logic;
   signal main_cache_addr_lo      : std_logic_vector(C_CACHE_SIZE-1 downto 0);
   signal main_cache_addr_hi      : std_logic_vector(C_CACHE_SIZE-1 downto 0);
   signal main_slot_lo            : std_logic_vector(C_CACHE_SIZE-1 downto 0);
   signal main_slot_hi            : std_logic_vector(C_CACHE_SIZE-1 downto 0);
   signal main_lo_word            : std_logic_vector(15 downto 0);
   signal main_hi_word            : std_logic_vector(15 downto 0);
   signal main_a0_d               : std_logic := '0';

begin

   ---------------------------------------------------
   -- QNICE: control and status registers, and the file's path into HyperRAM
   ---------------------------------------------------

   i_sw_cartridge_csr : entity work.sw_cartridge_csr
      generic map (
         G_BASE_ADDRESS => G_BASE_ADDRESS
      )
      port map (
         qnice_clk_i               => qnice_clk_i,
         qnice_rst_i               => qnice_rst_i,
         qnice_addr_i              => qnice_addr_i,
         qnice_data_i              => qnice_data_i,
         qnice_ce_i                => qnice_ce_i,
         qnice_we_i                => qnice_we_i,
         qnice_data_o              => qnice_data_o,
         qnice_wait_o              => qnice_wait_o,
         qnice_req_length_o        => qnice_req_length,
         qnice_req_valid_o         => qnice_req_valid,
         qnice_resp_status_i       => qnice_resp_status,
         qnice_resp_error_i        => qnice_resp_error,
         qnice_resp_address_i      => qnice_resp_address,
         qnice_avm_write_o         => qnice_avm_write,
         qnice_avm_read_o          => qnice_avm_read,
         qnice_avm_address_o       => qnice_avm_address,
         qnice_avm_writedata_o     => qnice_avm_writedata,
         qnice_avm_byteenable_o    => qnice_avm_byteenable,
         qnice_avm_burstcount_o    => qnice_avm_burstcount,
         qnice_avm_readdata_i      => qnice_avm_readdata,
         qnice_avm_readdatavalid_i => qnice_avm_readdatavalid,
         qnice_avm_waitrequest_i   => qnice_avm_waitrequest
      ); -- i_sw_cartridge_csr

   i_cdc_qnice2hr : entity work.cdc_stable
      generic map (
         G_REGISTER_SRC => true,
         G_DATA_SIZE    => 24
      )
      port map (
         src_clk_i                => qnice_clk_i,
         src_data_i(22 downto  0) => qnice_req_length,
         src_data_i(23)           => qnice_req_valid,
         dst_clk_i                => hr_clk_i,
         dst_data_o(22 downto  0) => hr_req_length,
         dst_data_o(23)           => hr_req_valid
      ); -- i_cdc_qnice2hr

   i_avm_fifo_qnice : entity work.avm_fifo
      generic map (
         G_WR_DEPTH     => 16,
         G_RD_DEPTH     => 16,
         G_FILL_SIZE    => 1,
         G_ADDRESS_SIZE => 32,
         G_DATA_SIZE    => 16
      )
      port map (
         s_clk_i               => qnice_clk_i,
         s_rst_i               => qnice_rst_i,
         s_avm_waitrequest_o   => qnice_avm_waitrequest,
         s_avm_write_i         => qnice_avm_write,
         s_avm_read_i          => qnice_avm_read,
         s_avm_address_i       => qnice_avm_address,
         s_avm_writedata_i     => qnice_avm_writedata,
         s_avm_byteenable_i    => qnice_avm_byteenable,
         s_avm_burstcount_i    => qnice_avm_burstcount,
         s_avm_readdata_o      => qnice_avm_readdata,
         s_avm_readdatavalid_o => qnice_avm_readdatavalid,
         m_clk_i               => hr_clk_i,
         m_rst_i               => hr_rst_i,
         m_avm_waitrequest_i   => hr_qnice_waitrequest,
         m_avm_write_o         => hr_qnice_write,
         m_avm_read_o          => hr_qnice_read,
         m_avm_address_o       => hr_qnice_address,
         m_avm_writedata_o     => hr_qnice_writedata,
         m_avm_byteenable_o    => hr_qnice_byteenable,
         m_avm_burstcount_o    => hr_qnice_burstcount,
         m_avm_readdata_i      => hr_qnice_readdata,
         m_avm_readdatavalid_i => hr_qnice_readdatavalid
      ); -- i_avm_fifo_qnice

   i_cdc_hr2qnice : entity work.cdc_stable
      generic map (
         G_DATA_SIZE => 31
      )
      port map (
         src_clk_i               => hr_clk_i,
         src_data_i( 3 downto 0) => hr_resp_status,
         src_data_i( 7 downto 4) => hr_resp_error,
         src_data_i(30 downto 8) => hr_resp_address,
         dst_clk_i               => qnice_clk_i,
         dst_data_o( 3 downto 0) => qnice_resp_status,
         dst_data_o( 7 downto 4) => qnice_resp_error,
         dst_data_o(30 downto 8) => qnice_resp_address
      ); -- i_cdc_hr2qnice

   ---------------------------------------------------
   -- HyperRAM: parser and bank cache, sharing the port with QNICE
   ---------------------------------------------------

   i_crt_loader : entity work.crt_loader
      generic map (
         G_CACHE_SIZE => C_CACHE_SIZE
      )
      port map (
         clk_i               => hr_clk_i,
         rst_i               => hr_rst_i,
         req_start_i         => hr_req_valid,
         req_address_i       => G_BASE_ADDRESS,
         req_length_i        => hr_req_length,
         resp_status_o       => hr_resp_status,
         resp_error_o        => hr_resp_error,
         resp_address_o      => hr_resp_address,
         bank_lo_i           => hr_bank_lo,
         bank_hi_i           => hr_bank_hi,
         ack_lo_o            => hr_ack_lo,
         ack_hi_o            => hr_ack_hi,
         ready_o             => hr_ready,
         cache_addr_lo_o     => hr_cache_addr_lo,
         cache_addr_hi_o     => hr_cache_addr_hi,
         avm_write_o         => hr_crt_write,
         avm_read_o          => hr_crt_read,
         avm_address_o       => hr_crt_address(21 downto 0),
         avm_writedata_o     => hr_crt_writedata,
         avm_byteenable_o    => hr_crt_byteenable,
         avm_burstcount_o    => hr_crt_burstcount,
         avm_readdata_i      => hr_crt_readdata,
         avm_readdatavalid_i => hr_crt_readdatavalid,
         avm_waitrequest_i   => hr_crt_waitrequest,
         cart_bank_laddr_o   => open,
         cart_bank_size_o    => open,
         cart_bank_num_o     => open,
         cart_bank_raddr_o   => open,
         cart_bank_wr_o      => open,
         cart_loading_o      => hr_loading,
         cart_id_o           => hr_id,
         cart_exrom_o        => hr_exrom,
         cart_game_o         => hr_game,
         cart_size_o         => hr_size,
         cart_c128_o         => hr_c128,
         cart_subtype_o      => hr_subtype,
         bram_address_o      => hr_bram_address,
         bram_data_o         => hr_bram_data,
         bram_lo_wren_o      => hr_bram_lo_wren,
         bram_hi_wren_o      => hr_bram_hi_wren
      ); -- i_crt_loader

   i_avm_arbit : entity work.avm_arbit
      generic map (
         G_PREFER_SWAP  => false,
         G_ADDRESS_SIZE => 32,
         G_DATA_SIZE    => 16
      )
      port map (
         clk_i                  => hr_clk_i,
         rst_i                  => hr_rst_i,
         s0_avm_write_i         => hr_qnice_write,
         s0_avm_read_i          => hr_qnice_read,
         s0_avm_address_i       => hr_qnice_address,
         s0_avm_writedata_i     => hr_qnice_writedata,
         s0_avm_byteenable_i    => hr_qnice_byteenable,
         s0_avm_burstcount_i    => hr_qnice_burstcount,
         s0_avm_readdata_o      => hr_qnice_readdata,
         s0_avm_readdatavalid_o => hr_qnice_readdatavalid,
         s0_avm_waitrequest_o   => hr_qnice_waitrequest,
         s1_avm_write_i         => hr_crt_write,
         s1_avm_read_i          => hr_crt_read,
         s1_avm_address_i       => hr_crt_address,
         s1_avm_writedata_i     => hr_crt_writedata,
         s1_avm_byteenable_i    => hr_crt_byteenable,
         s1_avm_burstcount_i    => hr_crt_burstcount,
         s1_avm_readdata_o      => hr_crt_readdata,
         s1_avm_readdatavalid_o => hr_crt_readdatavalid,
         s1_avm_waitrequest_o   => hr_crt_waitrequest,
         m_avm_write_o          => hr_write_o,
         m_avm_read_o           => hr_read_o,
         m_avm_address_o        => hr_address_o,
         m_avm_writedata_o      => hr_writedata_o,
         m_avm_byteenable_o     => hr_byteenable_o,
         m_avm_burstcount_o     => hr_burstcount_o,
         m_avm_readdata_i       => hr_readdata_i,
         m_avm_readdatavalid_i  => hr_readdatavalid_i,
         m_avm_waitrequest_i    => hr_waitrequest_i
      ); -- i_avm_arbit

   ---------------------------------------------------
   -- Crossings between the core and HyperRAM. Each group moves as a whole (cdc_stable), so
   -- a slot always arrives together with the bank it holds and whether it is loaded.
   ---------------------------------------------------

   i_cdc_main2hr : entity work.cdc_stable
      generic map (
         G_REGISTER_SRC => true,
         G_DATA_SIZE    => 14
      )
      port map (
         src_clk_i                => main_clk_i,
         src_data_i( 6 downto  0) => main_bank_lo_i,
         src_data_i(13 downto  7) => main_bank_hi_i,
         dst_clk_i                => hr_clk_i,
         dst_data_o( 6 downto  0) => hr_bank_lo,
         dst_data_o(13 downto  7) => hr_bank_hi
      ); -- i_cdc_main2hr

   hr_loaded <= '1' when hr_resp_status = C_STAT_READY else '0';

   i_cdc_hr2main : entity work.cdc_stable
      generic map (
         G_REGISTER_SRC => true,
         G_DATA_SIZE    => C_HR2MAIN
      )
      port map (
         src_clk_i                 => hr_clk_i,
         src_data_i(0)             => hr_loaded,
         src_data_i(1)             => hr_loading,
         src_data_i(17 downto  2)  => hr_id,
         src_data_i(25 downto 18)  => hr_exrom,
         src_data_i(33 downto 26)  => hr_game,
         src_data_i(56 downto 34)  => hr_size,
         src_data_i(57)            => hr_c128,
         src_data_i(64 downto 58)  => hr_ack_lo,
         src_data_i(71 downto 65)  => hr_ack_hi,
         src_data_i(72)            => hr_ready,
         src_data_i(72+C_CACHE_SIZE downto 73)                => hr_cache_addr_lo,
         src_data_i(72+2*C_CACHE_SIZE downto 73+C_CACHE_SIZE) => hr_cache_addr_hi,
         src_data_i(C_HR2MAIN-1 downto C_HR2MAIN-8)           => hr_subtype,
         dst_clk_i                 => main_clk_i,
         dst_data_o(0)             => main_loaded,
         dst_data_o(1)             => main_loading_o,
         dst_data_o(17 downto  2)  => main_id_o,
         dst_data_o(25 downto 18)  => main_exrom_o,
         dst_data_o(33 downto 26)  => main_game_o,
         dst_data_o(56 downto 34)  => main_size_o,
         dst_data_o(57)            => main_c128,
         dst_data_o(64 downto 58)  => main_ack_lo,
         dst_data_o(71 downto 65)  => main_ack_hi,
         dst_data_o(72)            => main_ready,
         dst_data_o(72+C_CACHE_SIZE downto 73)                => main_cache_addr_lo,
         dst_data_o(72+2*C_CACHE_SIZE downto 73+C_CACHE_SIZE) => main_cache_addr_hi,
         dst_data_o(C_HR2MAIN-1 downto C_HR2MAIN-8)           => main_subtype_o
      ); -- i_cdc_hr2main

   main_loaded_o <= main_loaded;
   main_c128_o   <= main_c128;

   -- Wait from the moment the core asks for another bank until the cache reports it loaded
   main_wait_o   <= '1' when main_loaded = '1' and
                             (main_bank_lo_i /= main_ack_lo or main_bank_hi_i /= main_ack_hi or main_ready = '0')
                        else '0';

   ---------------------------------------------------
   -- Bank cache: 16-bit words, a C128 cartridge's 16 kB windows in slot pairs
   ---------------------------------------------------

   main_slot_lo <= main_cache_addr_lo(C_CACHE_SIZE-1 downto 1) & main_addr_i(13) when main_c128 = '1' else
                   main_cache_addr_lo;
   main_slot_hi <= main_cache_addr_hi(C_CACHE_SIZE-1 downto 1) & main_addr_i(13) when main_c128 = '1' else
                   main_cache_addr_hi;

   p_a0 : process (main_clk_i)
   begin
      if rising_edge(main_clk_i) then
         main_a0_d <= main_addr_i(0);
      end if;
   end process p_a0;

   main_lo_data_o <= main_lo_word(15 downto 8) when main_a0_d = '1' else main_lo_word(7 downto 0);
   main_hi_data_o <= main_hi_word(15 downto 8) when main_a0_d = '1' else main_hi_word(7 downto 0);

   i_crt_lo_ram : entity work.tdp_ram
      generic map (
         ADDR_WIDTH => 12 + C_CACHE_SIZE,
         DATA_WIDTH => 16
      )
      port map (
         clock_a    => main_clk_i,
         address_a  => main_slot_lo & main_addr_i(12 downto 1),
         data_a     => (others => '0'),
         wren_a     => '0',
         q_a        => main_lo_word,
         clock_b    => hr_clk_i,
         address_b  => hr_bram_address,
         data_b     => hr_bram_data,
         wren_b     => hr_bram_lo_wren,
         q_b        => open
      ); -- i_crt_lo_ram

   i_crt_hi_ram : entity work.tdp_ram
      generic map (
         ADDR_WIDTH => 12 + C_CACHE_SIZE,
         DATA_WIDTH => 16
      )
      port map (
         clock_a    => main_clk_i,
         address_a  => main_slot_hi & main_addr_i(12 downto 1),
         data_a     => (others => '0'),
         wren_a     => '0',
         q_a        => main_hi_word,
         clock_b    => hr_clk_i,
         address_b  => hr_bram_address,
         data_b     => hr_bram_data,
         wren_b     => hr_bram_hi_wren,
         q_b        => open
      ); -- i_crt_hi_ram

   ---------------------------------------------------
   -- Cartridge RAMs: 256 bytes each at $DExx and $DFxx, and 32 kB in 8 kB banks at ROML
   ---------------------------------------------------

   i_ioe_ram : entity work.tdp_ram
      generic map (
         ADDR_WIDTH => 8,
         DATA_WIDTH => 8
      )
      port map (
         clock_a    => main_clk_i,
         address_a  => main_addr_i(7 downto 0),
         data_a     => main_data_i,
         wren_a     => main_ioe_we_i,
         q_a        => main_ioe_data_o,
         clock_b    => '0',
         address_b  => (others => '0'),
         data_b     => (others => '0'),
         wren_b     => '0',
         q_b        => open
      ); -- i_ioe_ram

   i_iof_ram : entity work.tdp_ram
      generic map (
         ADDR_WIDTH => 8,
         DATA_WIDTH => 8
      )
      port map (
         clock_a    => main_clk_i,
         address_a  => main_addr_i(7 downto 0),
         data_a     => main_data_i,
         wren_a     => main_iof_we_i,
         q_a        => main_iof_data_o,
         clock_b    => '0',
         address_b  => (others => '0'),
         data_b     => (others => '0'),
         wren_b     => '0',
         q_b        => open
      ); -- i_iof_ram

   i_cart_ram : entity work.tdp_ram
      generic map (
         ADDR_WIDTH => 15,
         DATA_WIDTH => 8
      )
      port map (
         clock_a    => main_clk_i,
         address_a  => main_bank_lo_i(1 downto 0) & main_addr_i(12 downto 0),
         data_a     => main_data_i,
         wren_a     => main_ram_we_i,
         q_a        => main_ram_data_o,
         clock_b    => '0',
         address_b  => (others => '0'),
         data_b     => (others => '0'),
         wren_b     => '0',
         q_b        => open
      ); -- i_cart_ram

end architecture synthesis;
