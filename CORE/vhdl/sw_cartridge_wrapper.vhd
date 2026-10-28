----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Virtual cartridge: the QNICE device that loads a .crt file into memory, the parser, and the
-- cartridge's RAMs. main.vhd runs the cartridge's logic (cartridge.vhd) and reads ROM through
-- the main_* ports.
--
-- Without SDRAM (R3) the file sits in HyperRAM and a bank cache in block RAM serves the reads;
-- the C128 waits while a bank loads. With SDRAM (G_SDRAM, R4 and later) the file sits there
-- and crt_reader fetches each byte the C128 or VIC asks for, in time for the bus cycle.
--
-- An EasyFlash's flash chips (crt_flash) take programs and erases, which crt_reader (SDRAM) or
-- crt_cacher (R3) writes into the file.
--
-- After MJoergen's sw_cartridge_wrapper for C64MEGA65 (2023, GPL v3).
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.sdhost_pkg.all;

entity sw_cartridge_wrapper is
generic (
   G_BASE_ADDRESS : std_logic_vector(21 downto 0);  -- word address of the CRT file
   G_SDRAM        : boolean := false;               -- the file is in SDRAM, not HyperRAM
   G_STATS        : boolean := false                -- the Shell shows the bank waits (CSR $023)
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
   qnice_sd_want_o     : out std_logic;                      -- the Shell loads the file by DMA
   qnice_sd_eng_o      : out t_sd_c2q;
   qnice_sd_eng_i      : in  t_sd_q2c := (others => '0');

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
   main_vic_data_o     : out std_logic_vector( 7 downto 0);  -- ROMH for the VIC's Ultimax fetch
   main_rd_lo_i        : in  std_logic := '0';               -- SDRAM: the CPU's address is in ROML (or IO1/IO2)
   main_rd_hi_i        : in  std_logic := '0';               -- SDRAM: the CPU's address is in ROMH
   main_vic_rd_i       : in  std_logic := '0';               -- SDRAM: the VIC fetches ROMH (Ultimax)
   main_vic_addr_i     : in  std_logic_vector(12 downto 0) := (others => '0');
   main_rfsh_ok_i      : in  std_logic := '1';               -- SDRAM: no read is due soon
   main_reu_ok_i       : in  std_logic := '1';               -- SDRAM: the REU may start an access
   main_reset_i        : in  std_logic := '0';               -- the C128 is in reset
   main_phi2_i         : in  std_logic := '0';
   main_flash_wr_i     : in  std_logic := '0';               -- a write selects ROML or ROMH (Ultimax)
   main_flash_rd_i     : in  std_logic := '0';               -- a read selects ROML or ROMH
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
   hr_waitrequest_i    : in  std_logic;

   -- SDRAM (G_SDRAM), on the HyperRAM clock
   sr_refresh_ok_o     : out std_logic;
   sr_reu_ok_o         : out std_logic;                      -- main_reu_ok_i, crossed
   sr_write_o          : out std_logic;
   sr_read_o           : out std_logic;
   sr_address_o        : out std_logic_vector(31 downto 0);
   sr_writedata_o      : out std_logic_vector(15 downto 0);
   sr_byteenable_o     : out std_logic_vector( 1 downto 0);
   sr_burstcount_o     : out std_logic_vector( 7 downto 0);
   sr_readdata_i       : in  std_logic_vector(15 downto 0) := (others => '0');
   sr_readdatavalid_i  : in  std_logic := '0';
   sr_waitrequest_i    : in  std_logic := '0'
);
end entity sw_cartridge_wrapper;


architecture synthesis of sw_cartridge_wrapper is

   -- 8 slots of 8 kB on each side, i.e. 64 kB of block RAM each for ROML and ROMH
   constant C_CACHE_SIZE  : natural := 3;
   constant C_STAT_READY  : std_logic_vector(3 downto 0) := "0010";
   -- Largest .crt file, in 64 kB: the cartridge's 2 MiB region in HyperRAM, or 4 MiB in SDRAM,
   -- which a full-size Magic Desk 16K (2 MiB of ROM plus headers) needs
   function max_hi(sdram : boolean) return std_logic_vector is
   begin
      if sdram then
         return x"0040";
      end if;
      return x"0020";
   end function max_hi;
   constant C_MAX_HI      : std_logic_vector(15 downto 0) := max_hi(G_SDRAM);
   -- Bits of the HyperRAM -> core crossing, and where in it the loaded bursts of each side are
   constant C_HR2MAIN     : natural := 1 + 1 + 16 + 8 + 8 + 23 + 1 + 7 + 7 + 1 + 2*C_CACHE_SIZE + 128 + 8;
   constant C_VIEW        : natural := 73 + 2*C_CACHE_SIZE;

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
   signal hr_mem_write            : std_logic;
   signal hr_mem_read             : std_logic;
   signal hr_mem_address          : std_logic_vector(31 downto 0);
   signal hr_mem_writedata        : std_logic_vector(15 downto 0);
   signal hr_mem_byteenable       : std_logic_vector( 1 downto 0);
   signal hr_mem_burstcount       : std_logic_vector( 7 downto 0);
   signal hr_mem_readdata         : std_logic_vector(15 downto 0);
   signal hr_mem_readdatavalid    : std_logic;
   signal hr_mem_waitrequest      : std_logic;

   signal hr_ack_lo               : std_logic_vector( 6 downto 0);
   signal hr_ack_hi               : std_logic_vector( 6 downto 0);
   signal hr_ready                : std_logic;
   signal hr_cache_addr_lo        : std_logic_vector(C_CACHE_SIZE-1 downto 0);
   signal hr_cache_addr_hi        : std_logic_vector(C_CACHE_SIZE-1 downto 0);
   signal hr_view_lo              : std_logic_vector(63 downto 0);   -- loaded bursts, see crt_cacher
   signal hr_view_hi              : std_logic_vector(63 downto 0);
   signal hr_id                   : std_logic_vector(15 downto 0);
   signal hr_exrom                : std_logic_vector( 7 downto 0);
   signal hr_game                 : std_logic_vector( 7 downto 0);
   signal hr_size                 : std_logic_vector(22 downto 0);
   signal hr_c128                 : std_logic;
   signal hr_subtype              : std_logic_vector( 7 downto 0);
   signal hr_ef                   : std_logic;
   signal hr_loaded               : std_logic;
   signal hr_loading              : std_logic;

   signal main_ack_lo             : std_logic_vector( 6 downto 0);
   signal main_ack_hi             : std_logic_vector( 6 downto 0);
   signal main_ready              : std_logic;
   signal main_loaded             : std_logic;
   signal main_c128               : std_logic;
   signal main_cache_addr_lo      : std_logic_vector(C_CACHE_SIZE-1 downto 0);
   signal main_cache_addr_hi      : std_logic_vector(C_CACHE_SIZE-1 downto 0);
   signal main_view_lo            : std_logic_vector(63 downto 0);
   signal main_view_hi            : std_logic_vector(63 downto 0);
   signal main_lo_mem             : std_logic_vector( 7 downto 0);  -- ROML and ROMH from memory
   signal main_hi_mem             : std_logic_vector( 7 downto 0);

   -- Bank waits for the Shell's menu: count (low word) and longest (high word), CSR $021-$022
   signal qnice_stats             : std_logic_vector(31 downto 0);
   signal qnice_stats_clr         : std_logic;

   -- EasyFlash's flash chips (index 0 ROML's, 1 ROMH's) and their jobs
   signal main_flash_rst          : std_logic;
   signal main_flash_oe           : std_logic_vector( 1 downto 0);
   signal main_flash_lo_data      : std_logic_vector( 7 downto 0);
   signal main_flash_hi_data      : std_logic_vector( 7 downto 0);
   signal main_fl_req             : std_logic_vector( 1 downto 0);
   signal main_fl_lo_job          : std_logic_vector(28 downto 0);
   signal main_fl_hi_job          : std_logic_vector(28 downto 0);
   signal main_fl_done            : std_logic_vector( 1 downto 0);
   signal main_fl_fail            : std_logic_vector( 1 downto 0);
   signal main_fl_done_d          : std_logic_vector( 1 downto 0) := "00";
   signal main_fl_done_s          : std_logic_vector( 1 downto 0) := "00";
   signal hr_fl_req_d             : std_logic_vector( 1 downto 0) := "00";
   signal hr_fl_req               : std_logic_vector( 1 downto 0) := "00";
   signal hr_fl_done              : std_logic_vector( 1 downto 0);
   signal hr_fl_fail              : std_logic_vector( 1 downto 0);
   signal hr_prefill              : std_logic;                        -- filling EasyFlash spares
   signal main_pre_d              : std_logic := '0';
   signal main_pre                : std_logic := '0';

   attribute async_reg : string;
   attribute async_reg of main_fl_done_d : signal is "true";
   attribute async_reg of main_fl_done_s : signal is "true";
   attribute async_reg of hr_fl_req_d    : signal is "true";
   attribute async_reg of hr_fl_req      : signal is "true";
   attribute async_reg of main_pre_d     : signal is "true";
   attribute async_reg of main_pre       : signal is "true";

begin

   ---------------------------------------------------
   -- QNICE: control and status registers, and the file's path into memory
   ---------------------------------------------------

   i_sw_cartridge_csr : entity work.sw_cartridge_csr
      generic map (
         G_BASE_ADDRESS => G_BASE_ADDRESS,
         G_MAX_HI       => C_MAX_HI,
         G_STATS        => G_STATS and not G_SDRAM
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
         qnice_avm_waitrequest_i   => qnice_avm_waitrequest,
         qnice_sd_want_o           => qnice_sd_want_o,
         qnice_sd_eng_o            => qnice_sd_eng_o,
         qnice_sd_eng_i            => qnice_sd_eng_i,
         qnice_stats_i             => qnice_stats,
         qnice_stats_clr_o         => qnice_stats_clr
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

   -- The file's memory, shared by QNICE and the parser (and the cache or reader behind them)
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
         m_avm_write_o          => hr_mem_write,
         m_avm_read_o           => hr_mem_read,
         m_avm_address_o        => hr_mem_address,
         m_avm_writedata_o      => hr_mem_writedata,
         m_avm_byteenable_o     => hr_mem_byteenable,
         m_avm_burstcount_o     => hr_mem_burstcount,
         m_avm_readdata_i       => hr_mem_readdata,
         m_avm_readdatavalid_i  => hr_mem_readdatavalid,
         m_avm_waitrequest_i    => hr_mem_waitrequest
      ); -- i_avm_arbit

   ---------------------------------------------------
   -- Status towards the core. Each group moves as a whole (cdc_stable), so a cache slot always
   -- arrives together with the bank it holds and whether it is loaded.
   ---------------------------------------------------

   hr_loaded <= '1' when hr_resp_status = C_STAT_READY else '0';
   hr_ef     <= '1' when hr_id = x"0020" and hr_c128 = '0' else '0';

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
         src_data_i(C_VIEW+63 downto C_VIEW)                  => hr_view_lo,
         src_data_i(C_VIEW+127 downto C_VIEW+64)              => hr_view_hi,
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
         dst_data_o(C_VIEW+63 downto C_VIEW)                  => main_view_lo,
         dst_data_o(C_VIEW+127 downto C_VIEW+64)              => main_view_hi,
         dst_data_o(C_HR2MAIN-1 downto C_HR2MAIN-8)           => main_subtype_o
      ); -- i_cdc_hr2main

   main_loaded_o <= main_loaded;
   main_c128_o   <= main_c128;

   ---------------------------------------------------
   -- EasyFlash's flash chips. While one is busy or shows its ID, it answers reads itself.
   ---------------------------------------------------

   main_flash_rst <= '1' when main_reset_i = '1' or main_loaded = '0' or main_c128 = '1' or
                              main_id_o /= x"0020" else '0';

   i_crt_flash : entity work.crt_flash
      port map (
         clk_i     => main_clk_i,
         rst_i     => main_flash_rst,
         wr_i      => main_flash_wr_i and not main_flash_rst,
         rd_i      => main_flash_rd_i,
         hi_i      => main_rd_hi_i,
         bank_lo_i => main_bank_lo_i(5 downto 0),
         bank_hi_i => main_bank_hi_i(5 downto 0),
         addr_i    => main_addr_i(12 downto 0),
         data_i    => main_data_i,
         phi2_i    => main_phi2_i,
         oe_o      => main_flash_oe,
         lo_data_o => main_flash_lo_data,
         hi_data_o => main_flash_hi_data,
         req_o     => main_fl_req,
         lo_job_o  => main_fl_lo_job,
         hi_job_o  => main_fl_hi_job,
         done_i    => main_fl_done,
         fail_i    => main_fl_fail
      ); -- i_crt_flash

   main_lo_data_o <= main_flash_lo_data when main_flash_oe(0) = '1' else main_lo_mem;
   main_hi_data_o <= main_flash_hi_data when main_flash_oe(1) = '1' else main_hi_mem;

   -- A job is held until it is done, so only the toggles need synchronizers. fail is steady by
   -- the time done has crossed.
   p_fl_hr : process (hr_clk_i)
   begin
      if rising_edge(hr_clk_i) then
         hr_fl_req_d <= main_fl_req;
         hr_fl_req   <= hr_fl_req_d;
      end if;
   end process p_fl_hr;

   p_fl_main : process (main_clk_i)
   begin
      if rising_edge(main_clk_i) then
         main_fl_done_d <= hr_fl_done;
         main_fl_done_s <= main_fl_done_d;
         main_pre_d     <= hr_prefill;
         main_pre       <= main_pre_d;
      end if;
   end process p_fl_main;

   main_fl_done <= main_fl_done_s;
   main_fl_fail <= hr_fl_fail;

   cache_gen : if not G_SDRAM generate

      signal hr_bram_address      : std_logic_vector(C_CACHE_SIZE+11 downto 0);
      signal hr_bram_data         : std_logic_vector(15 downto 0);
      signal hr_bram_lo_wren      : std_logic;
      signal hr_bram_hi_wren      : std_logic;
      signal hr_bank_lo           : std_logic_vector( 6 downto 0);
      signal hr_bank_hi           : std_logic_vector( 6 downto 0);
      signal hr_want_lo           : std_logic_vector(13 downto 0);
      signal hr_want_hi           : std_logic_vector(13 downto 0);
      signal main_burst           : unsigned( 5 downto 0);           -- of the address, in its slot (pair)
      signal main_miss_lo         : std_logic;
      signal main_miss_hi         : std_logic;
      signal main_want_lo         : std_logic_vector(13 downto 0) := (others => '0');  -- valid, bank, burst
      signal main_want_hi         : std_logic_vector(13 downto 0) := (others => '0');
      signal main_slot_lo         : std_logic_vector(C_CACHE_SIZE-1 downto 0);
      signal main_slot_hi         : std_logic_vector(C_CACHE_SIZE-1 downto 0);
      signal main_lo_word         : std_logic_vector(15 downto 0);
      signal main_hi_word         : std_logic_vector(15 downto 0);
      signal main_a0_d            : std_logic := '0';
      signal main_wait            : std_logic;
      signal main_stall           : std_logic;
      signal main_stall_clk       : unsigned( 4 downto 0) := (others => '0');  -- 32 clocks a C64 cycle
      signal main_stall_len       : unsigned(15 downto 0) := (others => '0');
      signal main_stall_cnt       : unsigned(15 downto 0) := (others => '0');
      signal main_stall_max       : unsigned(15 downto 0) := (others => '0');
      signal main_stats_clr       : std_logic;
      signal main_stats_clr_d     : std_logic := '0';

   begin

      ---------------------------------------------------
      -- HyperRAM: parser and bank cache
      ---------------------------------------------------

      i_crt_loader : entity work.crt_loader
         generic map (
            G_CACHE_SIZE    => C_CACHE_SIZE,
            G_SPARE_ADDRESS => std_logic_vector(unsigned(G_BASE_ADDRESS) + 16#80000#)  -- +1 MiB
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
            view_lo_o           => hr_view_lo,
            view_hi_o           => hr_view_hi,
            want_lo_i           => hr_want_lo,
            want_hi_i           => hr_want_hi,
            fl_req_i            => hr_fl_req,
            fl_lo_job_i         => main_fl_lo_job,
            fl_hi_job_i         => main_fl_hi_job,
            fl_done_o           => hr_fl_done,
            fl_fail_o           => hr_fl_fail,
            prefill_o           => hr_prefill,
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

      hr_write_o         <= hr_mem_write;
      hr_read_o          <= hr_mem_read;
      hr_address_o       <= hr_mem_address;
      hr_writedata_o     <= hr_mem_writedata;
      hr_byteenable_o    <= hr_mem_byteenable;
      hr_burstcount_o    <= hr_mem_burstcount;
      hr_mem_readdata    <= hr_readdata_i;
      hr_mem_readdatavalid <= hr_readdatavalid_i;
      hr_mem_waitrequest <= hr_waitrequest_i;

      sr_refresh_ok_o    <= '1';
      sr_reu_ok_o        <= '1';
      sr_write_o         <= '0';
      sr_read_o          <= '0';
      sr_address_o       <= (others => '0');
      sr_writedata_o     <= (others => '0');
      sr_byteenable_o    <= (others => '0');
      sr_burstcount_o    <= (others => '0');

      i_cdc_main2hr : entity work.cdc_stable
         generic map (
            G_REGISTER_SRC => true,
            G_DATA_SIZE    => 42
         )
         port map (
            src_clk_i                => main_clk_i,
            src_data_i( 6 downto  0) => main_bank_lo_i,
            src_data_i(13 downto  7) => main_bank_hi_i,
            src_data_i(27 downto 14) => main_want_lo,
            src_data_i(41 downto 28) => main_want_hi,
            dst_clk_i                => hr_clk_i,
            dst_data_o( 6 downto  0) => hr_bank_lo,
            dst_data_o(13 downto  7) => hr_bank_hi,
            dst_data_o(27 downto 14) => hr_want_lo,
            dst_data_o(41 downto 28) => hr_want_hi
         ); -- i_cdc_main2hr

      -- The C128 waits only when the CPU reads a burst of the selected bank that is not in yet:
      -- ROML or ROMH, or IO1/IO2, whose ROM reads come from ROML's slot. The cache loads that
      -- burst next. The CPU's address is out from the start of its access, so the wait reaches
      -- the core's DMA request before the access is latched, and is held, as the address the
      -- core shows changes while the DMA runs, until the burst is in or the bank is changed.
      -- In reset it waits until both slots are complete, and while EasyFlash spares are filled.
      main_burst   <= (main_addr_i(13) and main_c128) & unsigned(main_addr_i(12 downto 8));
      main_miss_lo <= '1' when main_rd_lo_i = '1' and
                               (main_ack_lo /= main_bank_lo_i or main_view_lo(to_integer(main_burst)) = '0')
                          else '0';
      main_miss_hi <= '1' when main_rd_hi_i = '1' and
                               (main_ack_hi /= main_bank_hi_i or main_view_hi(to_integer(main_burst)) = '0')
                          else '0';

      p_want : process (main_clk_i)
      begin
         if rising_edge(main_clk_i) then
            if main_want_lo(13) = '0' and main_want_hi(13) = '0' then
               if main_miss_lo = '1' then
                  main_want_lo <= '1' & main_bank_lo_i & std_logic_vector(main_burst);
               elsif main_miss_hi = '1' then
                  main_want_hi <= '1' & main_bank_hi_i & std_logic_vector(main_burst);
               end if;
            end if;
            if main_want_lo(13) = '1' and
               (main_want_lo(12 downto 6) /= main_bank_lo_i or
                (main_ack_lo = main_want_lo(12 downto 6) and
                 main_view_lo(to_integer(unsigned(main_want_lo(5 downto 0)))) = '1')) then
               main_want_lo(13) <= '0';
            end if;
            if main_want_hi(13) = '1' and
               (main_want_hi(12 downto 6) /= main_bank_hi_i or
                (main_ack_hi = main_want_hi(12 downto 6) and
                 main_view_hi(to_integer(unsigned(main_want_hi(5 downto 0)))) = '1')) then
               main_want_hi(13) <= '0';
            end if;
            if main_loaded = '0' or main_reset_i = '1' then
               main_want_lo(13) <= '0';
               main_want_hi(13) <= '0';
            end if;
         end if;
      end process p_want;

      main_wait <= '1' when main_loaded = '1' and
                            (main_pre = '1' or
                             (main_reset_i = '1' and (main_bank_lo_i /= main_ack_lo or
                                                      main_bank_hi_i /= main_ack_hi or main_ready = '0')) or
                             main_miss_lo = '1' or main_miss_hi = '1' or
                             main_want_lo(13) = '1' or main_want_hi(13) = '1')
                       else '0';
      main_wait_o <= main_wait;

      ---------------------------------------------------
      -- Bank waits while the C128 runs, for the Shell's menu: how many lasted a C64 cycle or
      -- more, which surely stopped the CPU (a hit only waits for its ack, a few hundred ns), and
      -- the longest in C64 cycles, since the Shell last cleared them or a file was loaded
      ---------------------------------------------------

      main_stall <= main_wait and not main_reset_i;

      p_stats : process (main_clk_i)
      begin
         if rising_edge(main_clk_i) then
            main_stats_clr_d <= main_stats_clr;

            if main_stall = '1' then
               main_stall_clk <= main_stall_clk + 1;
               if main_stall_clk = 0 and main_stall_len /= x"FFFF" then
                  main_stall_len <= main_stall_len + 1;
               end if;
               if main_stall_clk = 31 and main_stall_len = 1 and main_stall_cnt /= x"FFFF" then
                  main_stall_cnt <= main_stall_cnt + 1;
               end if;
            else
               main_stall_clk <= (others => '0');
               main_stall_len <= (others => '0');
            end if;

            if main_stall_len > main_stall_max then
               main_stall_max <= main_stall_len;
            end if;

            if main_stats_clr /= main_stats_clr_d or main_loaded = '0' then
               main_stall_cnt <= (others => '0');
               main_stall_max <= (others => '0');
            end if;
         end if;
      end process p_stats;

      i_cdc_stats : entity work.cdc_stable
         generic map (
            G_REGISTER_SRC => true,
            G_DATA_SIZE    => 32
         )
         port map (
            src_clk_i  => main_clk_i,
            src_data_i => std_logic_vector(main_stall_max) & std_logic_vector(main_stall_cnt),
            dst_clk_i  => qnice_clk_i,
            dst_data_o => qnice_stats
         ); -- i_cdc_stats

      i_cdc_stats_clr : entity work.cdc_stable
         generic map (
            G_REGISTER_SRC => false,
            G_DATA_SIZE    => 1
         )
         port map (
            src_clk_i     => qnice_clk_i,
            src_data_i(0) => qnice_stats_clr,
            dst_clk_i     => main_clk_i,
            dst_data_o(0) => main_stats_clr
         ); -- i_cdc_stats_clr

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

      main_lo_mem     <= main_lo_word(15 downto 8) when main_a0_d = '1' else main_lo_word(7 downto 0);
      main_hi_mem     <= main_hi_word(15 downto 8) when main_a0_d = '1' else main_hi_word(7 downto 0);

      main_vic_data_o <= main_hi_word(15 downto 8) when main_a0_d = '1' else main_hi_word(7 downto 0);

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

   end generate cache_gen;

   sdram_gen : if G_SDRAM generate

      signal hr_bank_laddr        : std_logic_vector(15 downto 0);
      signal hr_bank_size         : std_logic_vector(15 downto 0);
      signal hr_bank_num          : std_logic_vector(15 downto 0);
      signal hr_bank_raddr        : std_logic_vector(24 downto 0);
      signal hr_bank_wr           : std_logic;
      signal hr_cpu_req_d         : std_logic_vector( 2 downto 0) := (others => '0');
      signal hr_cpu_req           : std_logic_vector( 2 downto 0) := (others => '0');
      signal hr_vic_req_d         : std_logic_vector( 2 downto 0) := (others => '0');
      signal hr_vic_req           : std_logic_vector( 2 downto 0) := (others => '0');
      signal hr_rfsh_ok_d         : std_logic := '1';
      signal hr_rfsh_ok           : std_logic := '1';
      signal hr_reu_ok_d          : std_logic := '1';
      signal hr_reu_ok            : std_logic := '1';
      signal hr_cpu_data          : std_logic_vector( 7 downto 0);
      signal hr_vic_data          : std_logic_vector( 7 downto 0);
      signal hr_cpu_done          : std_logic_vector( 2 downto 0);
      signal hr_vic_done          : std_logic_vector( 2 downto 0);
      signal main_fl_done_q       : std_logic_vector( 1 downto 0) := "00";

      -- Requests: a key per port, and a Gray counter that counts its changes. The reader takes
      -- the key once the count has crossed, so the key needs no synchronizer.
      signal main_cpu_bank        : std_logic_vector( 6 downto 0);
      signal main_cpu_key         : std_logic_vector(21 downto 0);
      signal main_vic_key         : std_logic_vector(21 downto 0);
      signal main_cpu_key_r       : std_logic_vector(21 downto 0) := (others => '1');
      signal main_vic_key_r       : std_logic_vector(21 downto 0) := (others => '1');
      signal main_cpu_cnt         : unsigned(2 downto 0) := (others => '0');
      signal main_vic_cnt         : unsigned(2 downto 0) := (others => '0');
      signal main_cpu_req_r       : std_logic_vector(2 downto 0) := (others => '0');
      signal main_vic_req_r       : std_logic_vector(2 downto 0) := (others => '0');
      signal main_cpu_ok          : std_logic := '0';   -- main_cpu_key_r is this cartridge's
      signal main_vic_ok          : std_logic := '0';
      signal main_rfsh_ok_r       : std_logic := '1';
      signal main_reu_ok_r        : std_logic := '1';

      attribute async_reg of hr_cpu_req_d : signal is "true";
      attribute async_reg of hr_cpu_req   : signal is "true";
      attribute async_reg of hr_vic_req_d : signal is "true";
      attribute async_reg of hr_vic_req   : signal is "true";
      attribute async_reg of hr_rfsh_ok_d : signal is "true";
      attribute async_reg of hr_rfsh_ok   : signal is "true";
      attribute async_reg of hr_reu_ok_d  : signal is "true";
      attribute async_reg of hr_reu_ok    : signal is "true";

   begin

      ---------------------------------------------------
      -- SDRAM: parser, and the reader that serves the C128's reads
      ---------------------------------------------------

      i_crt_parser : entity work.crt_parser
         port map (
            clk_i               => hr_clk_i,
            rst_i               => hr_rst_i,
            req_start_i         => hr_req_valid,
            req_address_i       => G_BASE_ADDRESS,
            req_length_i        => hr_req_length,
            resp_status_o       => hr_resp_status,
            resp_error_o        => hr_resp_error,
            resp_address_o      => hr_resp_address,
            avm_write_o         => hr_crt_write,
            avm_read_o          => hr_crt_read,
            avm_address_o       => hr_crt_address(21 downto 0),
            avm_writedata_o     => hr_crt_writedata,
            avm_byteenable_o    => hr_crt_byteenable,
            avm_burstcount_o    => hr_crt_burstcount,
            avm_readdata_i      => hr_crt_readdata,
            avm_readdatavalid_i => hr_crt_readdatavalid,
            avm_waitrequest_i   => hr_crt_waitrequest,
            cart_bank_laddr_o   => hr_bank_laddr,
            cart_bank_size_o    => hr_bank_size,
            cart_bank_num_o     => hr_bank_num,
            cart_bank_raddr_o   => hr_bank_raddr,
            cart_bank_wr_o      => hr_bank_wr,
            cart_loading_o      => hr_loading,
            cart_id_o           => hr_id,
            cart_exrom_o        => hr_exrom,
            cart_game_o         => hr_game,
            cart_size_o         => hr_size,
            cart_c128_o         => hr_c128,
            cart_subtype_o      => hr_subtype
         ); -- i_crt_parser

      hr_ack_lo        <= (others => '0');
      hr_ack_hi        <= (others => '0');
      hr_ready         <= '1';
      hr_cache_addr_lo <= (others => '0');
      hr_cache_addr_hi <= (others => '0');
      hr_view_lo       <= (others => '1');
      hr_view_hi       <= (others => '1');

      i_crt_reader : entity work.crt_reader
         generic map (
            G_SPARE_ADDRESS       => std_logic_vector(unsigned(G_BASE_ADDRESS) + 16#80000#)  -- +1 MiB
         )
         port map (
            clk_i                 => hr_clk_i,
            rst_i                 => hr_rst_i,
            cart_ready_i          => hr_loaded,
            cart_size_i           => hr_size,
            cart_c128_i           => hr_c128,
            cart_ef_i             => hr_ef,
            cart_loading_i        => hr_loading,
            cart_bank_laddr_i     => hr_bank_laddr,
            cart_bank_size_i      => hr_bank_size,
            cart_bank_num_i       => hr_bank_num,
            cart_bank_raddr_i     => hr_bank_raddr,
            cart_bank_wr_i        => hr_bank_wr,
            cpu_req_i             => hr_cpu_req,
            cpu_key_i             => main_cpu_key_r,
            cpu_data_o            => hr_cpu_data,
            cpu_done_o            => hr_cpu_done,
            vic_req_i             => hr_vic_req,
            vic_key_i             => main_vic_key_r,
            vic_data_o            => hr_vic_data,
            vic_done_o            => hr_vic_done,
            slot_ok_i             => hr_rfsh_ok,
            fl_req_i              => hr_fl_req,
            fl_lo_job_i           => main_fl_lo_job,
            fl_hi_job_i           => main_fl_hi_job,
            fl_done_o             => hr_fl_done,
            fl_fail_o             => hr_fl_fail,
            prefill_o             => hr_prefill,
            s_avm_write_i         => hr_mem_write,
            s_avm_read_i          => hr_mem_read,
            s_avm_address_i       => hr_mem_address,
            s_avm_writedata_i     => hr_mem_writedata,
            s_avm_byteenable_i    => hr_mem_byteenable,
            s_avm_burstcount_i    => hr_mem_burstcount,
            s_avm_readdata_o      => hr_mem_readdata,
            s_avm_readdatavalid_o => hr_mem_readdatavalid,
            s_avm_waitrequest_o   => hr_mem_waitrequest,
            m_avm_write_o         => sr_write_o,
            m_avm_read_o          => sr_read_o,
            m_avm_address_o       => sr_address_o,
            m_avm_writedata_o     => sr_writedata_o,
            m_avm_byteenable_o    => sr_byteenable_o,
            m_avm_burstcount_o    => sr_burstcount_o,
            m_avm_readdata_i      => sr_readdata_i,
            m_avm_readdatavalid_i => sr_readdatavalid_i,
            m_avm_waitrequest_i   => sr_waitrequest_i
         ); -- i_crt_reader

      hr_write_o      <= '0';
      hr_read_o       <= '0';
      hr_address_o    <= (others => '0');
      hr_writedata_o  <= (others => '0');
      hr_byteenable_o <= (others => '0');
      hr_burstcount_o <= (others => '0');

      ---------------------------------------------------
      -- Core side: ask for the byte as soon as the CPU or VIC puts its address out. It comes
      -- back well before the C128 latches it (see CORE.xdc), so the C128 never waits.
      ---------------------------------------------------

      -- Only while an EasyFlash's spare banks are filled, which keeps the C128 in reset
      main_wait_o  <= main_loaded and main_pre;
      qnice_stats  <= (others => '0');

      main_cpu_bank <= main_bank_hi_i when main_rd_hi_i = '1' else main_bank_lo_i;
      main_cpu_key  <= main_rd_hi_i & main_cpu_bank & (main_addr_i(13) and main_c128) & main_addr_i(12 downto 0);
      main_vic_key  <= '1' & main_bank_hi_i & '0' & main_vic_addr_i;

      p_request : process (main_clk_i)
         variable v_cnt : unsigned(2 downto 0);
      begin
         if rising_edge(main_clk_i) then
            if (main_rd_lo_i = '1' or main_rd_hi_i = '1') and (main_cpu_key /= main_cpu_key_r or main_cpu_ok = '0') then
               v_cnt          := main_cpu_cnt + 1;
               main_cpu_cnt   <= v_cnt;
               main_cpu_req_r <= std_logic_vector(v_cnt xor ('0' & v_cnt(2 downto 1)));
               main_cpu_key_r <= main_cpu_key;
               main_cpu_ok    <= '1';
            end if;
            if main_vic_rd_i = '1' and (main_vic_key /= main_vic_key_r or main_vic_ok = '0') then
               v_cnt          := main_vic_cnt + 1;
               main_vic_cnt   <= v_cnt;
               main_vic_req_r <= std_logic_vector(v_cnt xor ('0' & v_cnt(2 downto 1)));
               main_vic_key_r <= main_vic_key;
               main_vic_ok    <= '1';
            end if;
            -- A new cartridge, or a flash job changed the file: what the reader holds is stale
            if main_loaded = '0' or main_fl_done_s /= main_fl_done_q then
               main_cpu_ok <= '0';
               main_vic_ok <= '0';
            end if;
            main_rfsh_ok_r <= main_rfsh_ok_i;
            main_reu_ok_r  <= main_reu_ok_i;
            main_fl_done_q <= main_fl_done_s;
         end if;
      end process p_request;

      p_sync : process (hr_clk_i)
      begin
         if rising_edge(hr_clk_i) then
            hr_cpu_req_d <= main_cpu_req_r;
            hr_cpu_req   <= hr_cpu_req_d;
            hr_vic_req_d <= main_vic_req_r;
            hr_vic_req   <= hr_vic_req_d;
            hr_rfsh_ok_d <= main_rfsh_ok_r;
            hr_rfsh_ok   <= hr_rfsh_ok_d;
            hr_reu_ok_d  <= main_reu_ok_r;
            hr_reu_ok    <= hr_reu_ok_d;
         end if;
      end process p_sync;

      sr_refresh_ok_o <= hr_rfsh_ok;
      sr_reu_ok_o     <= hr_reu_ok;

      main_lo_mem     <= hr_cpu_data;
      main_hi_mem     <= hr_cpu_data;
      main_vic_data_o <= hr_vic_data;

      -- pragma translate_off
      -- Simulation: the most core clocks a request has waited for its byte. In FAST mode the CPU
      -- latches its byte 7 clocks after asking, so the reader must answer within 5.
      p_latency : process (main_clk_i)
         variable v_cpu     : natural := 0;
         variable v_vic     : natural := 0;
         variable v_cpu_max : natural := 0;
         variable v_vic_max : natural := 0;
      begin
         if rising_edge(main_clk_i) then
            if hr_cpu_done = main_cpu_req_r then
               v_cpu := 0;
            else
               v_cpu := v_cpu + 1;
            end if;
            if hr_vic_done = main_vic_req_r then
               v_vic := 0;
            else
               v_vic := v_vic + 1;
            end if;
            if v_cpu > v_cpu_max then
               v_cpu_max := v_cpu;
               report "sw_cartridge_wrapper: a CPU read waited " & integer'image(v_cpu) & " core clocks";
            end if;
            if v_vic > v_vic_max then
               v_vic_max := v_vic;
               report "sw_cartridge_wrapper: a VIC read waited " & integer'image(v_vic) & " core clocks";
            end if;
         end if;
      end process p_latency;
      -- pragma translate_on

   end generate sdram_gen;

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
