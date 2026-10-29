----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- sdram_arbit: shares the SDRAM between the cartridge (s0, crt_reader), the REU (s1), the
-- system CPU (s2, rv_soc.vhd: LOAD_REU, SAVE_REU) and the REU's SD card DMA (s3, reu_sd.vhd).
-- The cartridge goes first and passes straight through, so its reads cost no extra clock. The
-- REU, then the CPU, then the DMA, may only start an access while slot_ok_i is high: a window
-- the core opens where the REU's longest access (8 words) is done before the next cartridge
-- read can be due. The CPU's accesses are 2 words, the DMA's 1 or a read of 8.
-- sdram.vhd takes one access at a time, so a read's data belongs to whoever started it; a write
-- burst keeps its master until its last word.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sdram_arbit is
   port (
      clk_i                 : in  std_logic;
      rst_i                 : in  std_logic;
      slot_ok_i             : in  std_logic;   -- the REU may start an access now

      -- Cartridge
      s0_avm_write_i        : in  std_logic;
      s0_avm_read_i         : in  std_logic;
      s0_avm_address_i      : in  std_logic_vector(31 downto 0);
      s0_avm_writedata_i    : in  std_logic_vector(15 downto 0);
      s0_avm_byteenable_i   : in  std_logic_vector( 1 downto 0);
      s0_avm_burstcount_i   : in  std_logic_vector( 7 downto 0);
      s0_avm_readdata_o     : out std_logic_vector(15 downto 0);
      s0_avm_readdatavalid_o : out std_logic;
      s0_avm_waitrequest_o  : out std_logic;

      -- REU
      s1_avm_write_i        : in  std_logic;
      s1_avm_read_i         : in  std_logic;
      s1_avm_address_i      : in  std_logic_vector(31 downto 0);
      s1_avm_writedata_i    : in  std_logic_vector(15 downto 0);
      s1_avm_byteenable_i   : in  std_logic_vector( 1 downto 0);
      s1_avm_burstcount_i   : in  std_logic_vector( 7 downto 0);
      s1_avm_readdata_o     : out std_logic_vector(15 downto 0);
      s1_avm_readdatavalid_o : out std_logic;
      s1_avm_waitrequest_o  : out std_logic;

      -- System CPU
      s2_avm_write_i        : in  std_logic;
      s2_avm_read_i         : in  std_logic;
      s2_avm_address_i      : in  std_logic_vector(31 downto 0);
      s2_avm_writedata_i    : in  std_logic_vector(15 downto 0);
      s2_avm_byteenable_i   : in  std_logic_vector( 1 downto 0);
      s2_avm_burstcount_i   : in  std_logic_vector( 7 downto 0);
      s2_avm_readdata_o     : out std_logic_vector(15 downto 0);
      s2_avm_readdatavalid_o : out std_logic;
      s2_avm_waitrequest_o  : out std_logic;

      -- REU's SD card DMA
      s3_avm_write_i        : in  std_logic := '0';
      s3_avm_read_i         : in  std_logic := '0';
      s3_avm_address_i      : in  std_logic_vector(31 downto 0) := (others => '0');
      s3_avm_writedata_i    : in  std_logic_vector(15 downto 0) := (others => '0');
      s3_avm_byteenable_i   : in  std_logic_vector( 1 downto 0) := (others => '0');
      s3_avm_burstcount_i   : in  std_logic_vector( 7 downto 0) := (others => '0');
      s3_avm_readdata_o     : out std_logic_vector(15 downto 0);
      s3_avm_readdatavalid_o : out std_logic;
      s3_avm_waitrequest_o  : out std_logic;

      -- SDRAM
      m_avm_write_o         : out std_logic;
      m_avm_read_o          : out std_logic;
      m_avm_address_o       : out std_logic_vector(31 downto 0);
      m_avm_writedata_o     : out std_logic_vector(15 downto 0);
      m_avm_byteenable_o    : out std_logic_vector( 1 downto 0);
      m_avm_burstcount_o    : out std_logic_vector( 7 downto 0);
      m_avm_readdata_i      : in  std_logic_vector(15 downto 0);
      m_avm_readdatavalid_i : in  std_logic;
      m_avm_waitrequest_i   : in  std_logic
   );
end entity sdram_arbit;

architecture synthesis of sdram_arbit is

   type t_who is (NONE_W, CART_W, REU_W, CPU_W, DMA_W);

   signal sel      : t_who;
   signal m_write  : std_logic;
   signal m_read   : std_logic;
   signal m_burst  : std_logic_vector(7 downto 0);
   signal wr_owner : t_who := NONE_W;                 -- a write burst still has words to go
   signal wr_left  : unsigned(7 downto 0) := (others => '0');
   signal rd_owner : t_who := NONE_W;                 -- whose read the data is for

begin

   sel <= wr_owner when wr_owner /= NONE_W else
          CART_W   when s0_avm_read_i = '1' or s0_avm_write_i = '1' else
          REU_W    when (s1_avm_read_i = '1' or s1_avm_write_i = '1') and slot_ok_i = '1' else
          CPU_W    when (s2_avm_read_i = '1' or s2_avm_write_i = '1') and slot_ok_i = '1' else
          DMA_W    when (s3_avm_read_i = '1' or s3_avm_write_i = '1') and slot_ok_i = '1' else
          NONE_W;

   m_write <= s0_avm_write_i when sel = CART_W else
              s1_avm_write_i when sel = REU_W else
              s2_avm_write_i when sel = CPU_W else
              s3_avm_write_i when sel = DMA_W else '0';
   m_read  <= s0_avm_read_i  when sel = CART_W else
              s1_avm_read_i  when sel = REU_W else
              s2_avm_read_i  when sel = CPU_W else
              s3_avm_read_i  when sel = DMA_W else '0';
   m_burst <= s1_avm_burstcount_i when sel = REU_W else
              s2_avm_burstcount_i when sel = CPU_W else
              s3_avm_burstcount_i when sel = DMA_W else s0_avm_burstcount_i;

   m_avm_write_o      <= m_write;
   m_avm_read_o       <= m_read;
   m_avm_address_o    <= s1_avm_address_i    when sel = REU_W else
                         s2_avm_address_i    when sel = CPU_W else
                         s3_avm_address_i    when sel = DMA_W else s0_avm_address_i;
   m_avm_writedata_o  <= s1_avm_writedata_i  when sel = REU_W else
                         s2_avm_writedata_i  when sel = CPU_W else
                         s3_avm_writedata_i  when sel = DMA_W else s0_avm_writedata_i;
   m_avm_byteenable_o <= s1_avm_byteenable_i when sel = REU_W else
                         s2_avm_byteenable_i when sel = CPU_W else
                         s3_avm_byteenable_i when sel = DMA_W else s0_avm_byteenable_i;
   m_avm_burstcount_o <= m_burst;

   s0_avm_waitrequest_o   <= m_avm_waitrequest_i when sel = CART_W else '1';
   s1_avm_waitrequest_o   <= m_avm_waitrequest_i when sel = REU_W  else '1';
   s2_avm_waitrequest_o   <= m_avm_waitrequest_i when sel = CPU_W  else '1';
   s3_avm_waitrequest_o   <= m_avm_waitrequest_i when sel = DMA_W  else '1';
   s0_avm_readdata_o      <= m_avm_readdata_i;
   s1_avm_readdata_o      <= m_avm_readdata_i;
   s2_avm_readdata_o      <= m_avm_readdata_i;
   s3_avm_readdata_o      <= m_avm_readdata_i;
   s0_avm_readdatavalid_o <= m_avm_readdatavalid_i when rd_owner = CART_W else '0';
   s1_avm_readdatavalid_o <= m_avm_readdatavalid_i when rd_owner = REU_W  else '0';
   s2_avm_readdatavalid_o <= m_avm_readdatavalid_i when rd_owner = CPU_W  else '0';
   s3_avm_readdatavalid_o <= m_avm_readdatavalid_i when rd_owner = DMA_W  else '0';

   p_owner : process (clk_i)
   begin
      if rising_edge(clk_i) then
         if m_read = '1' and m_avm_waitrequest_i = '0' then
            rd_owner <= sel;
         end if;
         if m_write = '1' and m_avm_waitrequest_i = '0' then
            if wr_owner = NONE_W then
               if unsigned(m_burst) > 1 then
                  wr_owner <= sel;
                  wr_left  <= unsigned(m_burst) - 1;
               end if;
            else
               wr_left <= wr_left - 1;
               if wr_left = 1 then
                  wr_owner <= NONE_W;
               end if;
            end if;
         end if;
         if rst_i = '1' then
            wr_owner <= NONE_W;
            rd_owner <= NONE_W;
         end if;
      end if;
   end process p_owner;

end architecture synthesis;
