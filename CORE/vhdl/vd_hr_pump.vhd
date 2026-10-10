----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Hardware fill of a virtual drive's sector buffer from the HyperRAM image.
--
-- This file, the mux in main.vhd that hands reads to it instead of the Shell
-- byte pump, and the HyperRAM arbiter in mega65.vhd are the whole path.
--
-- Why it exists: 1541/1571 DOS still needs the Group Coded Recording bitstream
-- (c1541_gcr). The encoder's track buffer used to be filled by QNICE HANDLE_IO,
-- one byte per Shell loop, through HyperRAM wait states. A cylinder is 17 to 21
-- sectors and that fill can last as long as a real revolution, which is how DOS
-- $F556 times out. This engine does the same copy the Shell did, from the same
-- image bytes, without the CPU.
--
-- Writes still go through vdrives and the Shell. They are rare and already
-- cached; the hang we care about is the read seek.
--
-- Protocol (same as vdrives.vhd / MiSTer hps_io):
--   1. The drive raises sd_rd and holds LBA / block count.
--   2. Latch those, raise sd_ack (the drive then drops sd_rd).
--   3. For each byte: present sd_buff_addr / sd_buff_dout and strobe sd_buff_wr
--      while ack stays high. The drive uses (sd_ack and sd_buff_wr) as WE.
--   4. Drop ack. The drive finishes on the falling edge.
--
-- Addressing: BLKSZ=1 means 256-byte blocks, so byte offset = LBA * 256.
-- The HyperRAM word base is C_HMAP_VDx(9 downto 0) & x"000", identical to
-- mount_buf_wrapper.
--
-- MEGA65 port done by Stefan Eilers in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.vdrives_pkg.all;

entity vd_hr_pump is
   generic (
      G_VDNUM : natural := 2
   );
   port (
      clk_qnice_i : in  std_logic;
      rst_qnice_i : in  std_logic;

      -- Drive "SD" interface, QNICE clock (clk_sys of iec_drive)
      sd_lba_i       : in  vd_vec_array(G_VDNUM - 1 downto 0)(31 downto 0);
      sd_blk_cnt_i   : in  vd_vec_array(G_VDNUM - 1 downto 0)(5 downto 0);
      sd_rd_i        : in  vd_std_array(G_VDNUM - 1 downto 0);
      sd_ack_o       : out vd_std_array(G_VDNUM - 1 downto 0);
      sd_buff_addr_o : out std_logic_vector(13 downto 0);
      sd_buff_dout_o : out std_logic_vector(7 downto 0);
      sd_buff_wr_o   : out std_logic;
      busy_o         : out std_logic;

      -- Word base of each drive's image in HyperRAM
      hr_base_d8_i : in std_logic_vector(21 downto 0);
      hr_base_d9_i : in std_logic_vector(21 downto 0);

      -- Avalon master toward HyperRAM (hr_clk domain, via avm_fifo)
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
end entity vd_hr_pump;

architecture synthesis of vd_hr_pump is

   type state_t is (ST_IDLE, ST_ISSUE, ST_WAIT, ST_BYTE0, ST_BYTE1, ST_DONE);

   signal state       : state_t := ST_IDLE;
   signal drive       : natural range 0 to G_VDNUM - 1 := 0;
   signal byte_index  : unsigned(13 downto 0) := (others => '0');
   signal byte_total  : unsigned(13 downto 0) := (others => '0');
   signal lba_u       : unsigned(31 downto 0) := (others => '0');
   signal word_data   : std_logic_vector(15 downto 0) := (others => '0');
   signal ack         : vd_std_array(G_VDNUM - 1 downto 0) := (others => '0');
   signal buff_addr   : std_logic_vector(13 downto 0) := (others => '0');
   signal buff_dout   : std_logic_vector(7 downto 0) := (others => '0');
   signal buff_wr     : std_logic := '0';

   signal avm_read        : std_logic := '0';
   signal avm_address     : std_logic_vector(31 downto 0) := (others => '0');
   signal avm_waitrequest : std_logic;
   signal avm_readdata    : std_logic_vector(15 downto 0);
   signal avm_readdatavalid : std_logic;

   function hr_base_of (idx : natural;
                        b8  : std_logic_vector(21 downto 0);
                        b9  : std_logic_vector(21 downto 0)) return unsigned is
   begin
      if idx = 0 then
         return unsigned(b8);
      else
         return unsigned(b9);
      end if;
   end function;

begin

   sd_ack_o       <= ack;
   sd_buff_addr_o <= buff_addr;
   sd_buff_dout_o <= buff_dout;
   sd_buff_wr_o   <= buff_wr;
   busy_o         <= '0' when state = ST_IDLE else '1';

   i_avm_fifo : entity work.avm_fifo
      generic map (
         G_WR_DEPTH     => 16,
         G_RD_DEPTH     => 16,
         G_FILL_SIZE    => 1,
         G_ADDRESS_SIZE => 32,
         G_DATA_SIZE    => 16
      )
      port map (
         s_clk_i               => clk_qnice_i,
         s_rst_i               => rst_qnice_i,
         s_avm_waitrequest_o   => avm_waitrequest,
         s_avm_write_i         => '0',
         s_avm_read_i          => avm_read,
         s_avm_address_i       => avm_address,
         s_avm_writedata_i     => (others => '0'),
         s_avm_byteenable_i    => "11",
         s_avm_burstcount_i    => x"01",
         s_avm_readdata_o      => avm_readdata,
         s_avm_readdatavalid_o => avm_readdatavalid,
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
      );

   p_fsm : process (clk_qnice_i)
      variable v_word  : unsigned(31 downto 0);
      variable v_found : boolean;
   begin
      if rising_edge(clk_qnice_i) then
         buff_wr  <= '0';
         avm_read <= '0';

         if rst_qnice_i = '1' then
            state      <= ST_IDLE;
            ack        <= (others => '0');
            byte_index <= (others => '0');
            byte_total <= (others => '0');
            avm_read   <= '0';
         else
            case state is
               when ST_IDLE =>
                  ack     <= (others => '0');
                  v_found := false;
                  for i in 0 to G_VDNUM - 1 loop
                     if (not v_found) and sd_rd_i(i) = '1' then
                        v_found := true;
                        drive   <= i;
                        lba_u   <= unsigned(sd_lba_i(i));
                        -- (blk_cnt + 1) * 256 bytes; blk_cnt is already "blocks minus one"
                        byte_total <= shift_left(resize(unsigned(sd_blk_cnt_i(i)), 14) + 1, 8);
                        byte_index <= (others => '0');
                        ack        <= (others => '0');
                        ack(i)     <= '1';
                        state      <= ST_ISSUE;
                     end if;
                  end loop;

               when ST_ISSUE =>
                  -- Word address = image base + (LBA*256 + byte_index) / 2
                  v_word := resize(hr_base_of(drive, hr_base_d8_i, hr_base_d9_i), 32)
                            + resize(shift_left(lba_u, 7), 32)   -- LBA * 128 words
                            + resize(shift_right(byte_index, 1), 32);
                  avm_address <= std_logic_vector(v_word);
                  avm_read    <= '1';
                  -- Accepted when read is already high and the slave is ready.
                  if avm_read = '1' and avm_waitrequest = '0' then
                     avm_read <= '0';
                     state    <= ST_WAIT;
                  end if;

               when ST_WAIT =>
                  if avm_readdatavalid = '1' then
                     word_data <= avm_readdata;
                     state     <= ST_BYTE0;
                  end if;

               when ST_BYTE0 =>
                  buff_addr <= std_logic_vector(byte_index);
                  buff_dout <= word_data(7 downto 0);
                  buff_wr   <= '1';
                  byte_index <= byte_index + 1;
                  state      <= ST_BYTE1;

               when ST_BYTE1 =>
                  buff_addr <= std_logic_vector(byte_index);
                  buff_dout <= word_data(15 downto 8);
                  buff_wr   <= '1';
                  byte_index <= byte_index + 1;
                  if (byte_index + 1) = byte_total then
                     state <= ST_DONE;
                  else
                     state <= ST_ISSUE;
                  end if;

               when ST_DONE =>
                  ack   <= (others => '0');
                  state <= ST_IDLE;
            end case;
         end if;
      end if;
   end process p_fsm;

end architecture synthesis;
