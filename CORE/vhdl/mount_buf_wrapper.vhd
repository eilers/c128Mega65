----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65 (C128MEGA65)
--
-- HyperRAM-backed disk-image mount buffer (D64 / D71 / D81)
--
-- This replaces the on-chip "mount_buf_ram" BRAM that the M2M template uses for
-- disk images. An 819,200-byte D81 would need roughly 200 RAMB36 tiles, and with
-- two drives twice that; the R6 build already uses 301 of the 365 tiles available.
--
-- Relocating the staging area is invisible to the core-side hardware, because a
-- mounted disk image is QNICE-only staging RAM: the C128 core never reads it, the
-- Shell software-pumps sectors out of it through the vdrives registers. The
-- window-0-based Shell arithmetic stays unchanged as well -- this bridge just adds
-- the HyperRAM base offset of the addressed drive.
--
-- Ported from the C64 core, where the same file is a clone of the .crt loader's
-- byte-window QNICE<->HyperRAM bridge (sw_cartridge_csr.vhd) with the CSR/parser
-- parts removed, plus the avm_fifo clock-domain crossing from sw_cartridge_wrapper.
-- So the QNICE side speaks the RAMROM 4k-window byte protocol (with wait states)
-- and the HyperRAM side is an Avalon master feeding the framework's HyperRAM
-- arbiter. qnice2hyperram performs the byte<->word conversion; the avm_fifo bridges
-- the QNICE clock to the HyperRAM clock, which is what keeps staging safe while
-- ascal is rendering.
--
-- Unlike the C64 original the base address is a port rather than a generic, so a
-- single instance serves both drives: QNICE is single-threaded, hence the two mount
-- devices are never accessed concurrently.
--
-- Ported from the C64 for MEGA65 core, originally done by MJoergen and sy2002 in 2023
-- MEGA65 C128 port done by Stefan Eilers in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity mount_buf_wrapper is
   port (
      -- QNICE clock domain: the C_DEV_MOUNT_D8 / C_DEV_MOUNT_D9 devices
      -- (4k-window byte protocol)
      qnice_clk_i        : in  std_logic;
      qnice_rst_i        : in  std_logic;
      qnice_addr_i       : in  std_logic_vector(27 downto 0);
      qnice_data_i       : in  std_logic_vector(15 downto 0);
      qnice_ce_i         : in  std_logic;
      qnice_we_i         : in  std_logic;
      qnice_data_o       : out std_logic_vector(15 downto 0);
      qnice_wait_o       : out std_logic;

      -- HyperRAM word base address of the currently addressed drive,
      -- i.e. C_HMAP_VD0(9 downto 0) & X"000" or C_HMAP_VD1(9 downto 0) & X"000"
      hr_base_addr_i     : in  std_logic_vector(21 downto 0);

      -- HyperRAM clock domain: Avalon master -> framework HyperRAM arbiter
      hr_clk_i           : in  std_logic;
      hr_rst_i           : in  std_logic;
      hr_write_o         : out std_logic;
      hr_read_o          : out std_logic;
      hr_address_o       : out std_logic_vector(31 downto 0);
      hr_writedata_o     : out std_logic_vector(15 downto 0);
      hr_byteenable_o    : out std_logic_vector( 1 downto 0);
      hr_burstcount_o    : out std_logic_vector( 7 downto 0);
      hr_readdata_i      : in  std_logic_vector(15 downto 0);
      hr_readdatavalid_i : in  std_logic;
      hr_waitrequest_i   : in  std_logic
   );
end entity mount_buf_wrapper;

architecture synthesis of mount_buf_wrapper is

   -- QNICE-domain byte<->word bridge internals
   signal qnice_hr_ce         : std_logic;
   signal qnice_hr_addr       : std_logic_vector(31 downto 0);
   signal qnice_hr_wait       : std_logic;
   signal qnice_hr_data       : std_logic_vector(15 downto 0);
   signal qnice_hr_byteenable : std_logic_vector( 1 downto 0);

   -- QNICE-side Avalon master (-> avm_fifo source side)
   signal qnice_avm_write         : std_logic;
   signal qnice_avm_read          : std_logic;
   signal qnice_avm_address       : std_logic_vector(31 downto 0);
   signal qnice_avm_writedata     : std_logic_vector(15 downto 0);
   signal qnice_avm_byteenable    : std_logic_vector( 1 downto 0);
   signal qnice_avm_burstcount    : std_logic_vector( 7 downto 0);
   signal qnice_avm_readdata      : std_logic_vector(15 downto 0);
   signal qnice_avm_readdatavalid : std_logic;
   signal qnice_avm_waitrequest   : std_logic;

begin

   ------------------------------------------------------------------------------
   -- QNICE byte-window -> HyperRAM word bridge (in the QNICE clock domain)
   ------------------------------------------------------------------------------

   qnice_hr_ce <= qnice_ce_i;

   -- Byte address qnice_addr_i(27..1) (bit 0 dropped) added to the 22-bit WORD base.
   -- bit 0 is the byte-lane select only -- it never enters the word address.
   qnice_hr_addr <= std_logic_vector(("00000" & unsigned(qnice_addr_i(27 downto 1))) +
                                     ("0000000000" & unsigned(hr_base_addr_i)));

   qnice_hr_byteenable <= "10" when qnice_addr_i(0) = '1' else
                          "01";

   -- Read mux + wait pass-through. Combinational, exactly like the proven .crt
   -- bridge: qnice2hyperram holds qnice_wait_o high until the HyperRAM read data
   -- is valid, so the addressed byte on qnice_hr_data is stable when wait drops.
   p_read : process (all)
   begin
      qnice_data_o <= x"0000";
      qnice_wait_o <= '0';
      if qnice_ce_i = '1' then
         qnice_wait_o <= qnice_hr_wait;
         if qnice_addr_i(0) = '1' then
            qnice_data_o <= x"00" & qnice_hr_data(15 downto 8);
         else
            qnice_data_o <= x"00" & qnice_hr_data(7 downto 0);
         end if;
      end if;
   end process p_read;

   i_qnice2hyperram : entity work.qnice2hyperram
      port map (
         clk_i                 => qnice_clk_i,
         rst_i                 => qnice_rst_i,
         s_qnice_wait_o        => qnice_hr_wait,
         s_qnice_address_i     => qnice_hr_addr,
         s_qnice_cs_i          => qnice_hr_ce,
         s_qnice_write_i       => qnice_we_i,
         -- write-data lane duplication: low byte on both lanes; byteenable commits one
         s_qnice_writedata_i   => qnice_data_i(7 downto 0) & qnice_data_i(7 downto 0),
         s_qnice_byteenable_i  => qnice_hr_byteenable,
         s_qnice_readdata_o    => qnice_hr_data,
         m_avm_write_o         => qnice_avm_write,
         m_avm_read_o          => qnice_avm_read,
         m_avm_address_o       => qnice_avm_address,
         m_avm_writedata_o     => qnice_avm_writedata,
         m_avm_byteenable_o    => qnice_avm_byteenable,
         m_avm_burstcount_o    => qnice_avm_burstcount,
         m_avm_readdata_i      => qnice_avm_readdata,
         m_avm_readdatavalid_i => qnice_avm_readdatavalid,
         m_avm_waitrequest_i   => qnice_avm_waitrequest
      ); -- i_qnice2hyperram

   ------------------------------------------------------------------------------
   -- Clock domain crossing QNICE <-> HyperRAM
   ------------------------------------------------------------------------------

   i_avm_fifo : entity work.avm_fifo
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
         m_avm_waitrequest_i   => hr_waitrequest_i,
         m_avm_write_o         => hr_write_o,
         m_avm_read_o          => hr_read_o,
         m_avm_address_o       => hr_address_o,
         m_avm_writedata_o     => hr_writedata_o,
         m_avm_byteenable_o    => hr_byteenable_o,
         m_avm_burstcount_o    => hr_burstcount_o,
         m_avm_readdata_i      => hr_readdata_i,
         m_avm_readdatavalid_i => hr_readdatavalid_i
      ); -- i_avm_fifo

end architecture synthesis;
