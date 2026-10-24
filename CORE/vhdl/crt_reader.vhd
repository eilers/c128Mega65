----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- crt_reader: reads the cartridge's ROML and ROMH straight from the CRT file in SDRAM, one
-- byte for each access the C128 makes, so no bank is ever copied and the C128 never waits.
-- R4 and later boards use it; R3 has no SDRAM and keeps crt_cacher.
--
-- Two request ports, the CPU's and the VIC's (Ultimax fetches). A request is a key: ROMH,
-- bank and offset in the bank's window. It is new when its Gray-coded count moves, and its
-- byte comes back on *_data_o. The VIC goes first when both wait. The parser and QNICE (s_avm_*) use
-- the SDRAM in between and are held off while a request waits. As in crt_cacher, a chip
-- smaller than its window is mirrored through it, and a bank without a chip reads $FF.
--
-- Everything runs on the HyperRAM clock; the caller does the clock domain crossings.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity crt_reader is
   port (
      clk_i               : in  std_logic;
      rst_i               : in  std_logic;

      -- From the parser
      cart_c128_i         : in  std_logic;
      cart_loading_i      : in  std_logic;                         -- parsing a new file
      cart_bank_laddr_i   : in  std_logic_vector(15 downto 0);
      cart_bank_size_i    : in  std_logic_vector(15 downto 0);
      cart_bank_num_i     : in  std_logic_vector(15 downto 0);
      cart_bank_raddr_i   : in  std_logic_vector(24 downto 0);     -- byte address in SDRAM
      cart_bank_wr_i      : in  std_logic;

      -- Requests: key = ROMH & bank (7) & offset (14)
      cpu_req_i           : in  std_logic_vector( 2 downto 0);
      cpu_key_i           : in  std_logic_vector(21 downto 0);
      cpu_data_o          : out std_logic_vector( 7 downto 0);
      cpu_done_o          : out std_logic_vector( 2 downto 0);     -- the request cpu_data_o answers
      vic_req_i           : in  std_logic_vector( 2 downto 0);
      vic_key_i           : in  std_logic_vector(21 downto 0);
      vic_data_o          : out std_logic_vector( 7 downto 0);
      vic_done_o          : out std_logic_vector( 2 downto 0);

      -- The parser and QNICE
      s_avm_write_i       : in  std_logic;
      s_avm_read_i        : in  std_logic;
      s_avm_address_i     : in  std_logic_vector(31 downto 0);
      s_avm_writedata_i   : in  std_logic_vector(15 downto 0);
      s_avm_byteenable_i  : in  std_logic_vector( 1 downto 0);
      s_avm_burstcount_i  : in  std_logic_vector( 7 downto 0);
      s_avm_readdata_o    : out std_logic_vector(15 downto 0);
      s_avm_readdatavalid_o : out std_logic;
      s_avm_waitrequest_o : out std_logic;

      -- SDRAM
      m_avm_write_o       : out std_logic;
      m_avm_read_o        : out std_logic;
      m_avm_address_o     : out std_logic_vector(31 downto 0);
      m_avm_writedata_o   : out std_logic_vector(15 downto 0);
      m_avm_byteenable_o  : out std_logic_vector( 1 downto 0);
      m_avm_burstcount_o  : out std_logic_vector( 7 downto 0);
      m_avm_readdata_i    : in  std_logic_vector(15 downto 0);
      m_avm_readdatavalid_i : in  std_logic;
      m_avm_waitrequest_i : in  std_logic
   );
end entity crt_reader;

architecture synthesis of crt_reader is

   subtype t_off is unsigned(13 downto 0);

   -- Where each bank's image starts (word address) and the offsets it mirrors through
   type t_addr_tbl is array (0 to 127) of std_logic_vector(21 downto 0);
   type t_mask_tbl is array (0 to 127) of t_off;
   signal lo_addr      : t_addr_tbl := (others => (others => '0'));
   signal hi_addr      : t_addr_tbl := (others => (others => '0'));
   signal lo_mask      : t_mask_tbl := (others => (others => '1'));
   signal hi_mask      : t_mask_tbl := (others => (others => '1'));
   signal lo_present   : std_logic_vector(0 to 127) := (others => '0');   -- the file has this chip
   signal hi_present   : std_logic_vector(0 to 127) := (others => '0');
   signal loading_d    : std_logic := '0';

   type t_state is (IDLE_ST, ADDR_ST, READ_ST);
   signal state        : t_state := IDLE_ST;
   signal cpu_seen     : std_logic_vector(2 downto 0) := (others => '0');
   signal vic_seen     : std_logic_vector(2 downto 0) := (others => '0');
   signal key          : std_logic_vector(21 downto 0) := (others => '0');
   signal key_cnt      : std_logic_vector(2 downto 0) := (others => '0');
   signal for_vic      : std_logic := '0';
   signal a0           : std_logic := '0';
   signal pending      : std_logic;
   signal cart_read    : std_logic := '0';
   signal cart_address : std_logic_vector(31 downto 0) := (others => '0');

   -- Bulk transfers in flight: words still to read, words still to write
   signal bulk_rd      : unsigned(8 downto 0) := (others => '0');
   signal bulk_wr      : unsigned(7 downto 0) := (others => '0');
   signal bulk_hold    : std_logic;
   signal cart_owns    : std_logic;

   -- Offsets a chip of this size mirrors through: one less than the next power of two
   function mirror_mask(size : std_logic_vector(15 downto 0); max_bytes : natural) return t_off is
      variable v : unsigned(15 downto 0) := unsigned(size) - 1;
      variable m : t_off := (others => '0');
   begin
      if unsigned(size) = 0 or unsigned(size) >= max_bytes then
         return to_unsigned(max_bytes - 1, 14);
      end if;
      for i in 13 downto 0 loop
         if v(15 downto i) /= 0 then
            m(i) := '1';
         end if;
      end loop;
      return m;
   end function mirror_mask;

begin

   pending   <= '1' when cpu_req_i /= cpu_seen or vic_req_i /= vic_seen else '0';
   cart_owns <= '0' when state = IDLE_ST else '1';

   -- A waiting request holds new bulk transfers off, but never splits a write burst
   bulk_hold <= '1' when state /= IDLE_ST or (pending = '1' and bulk_wr = 0) else '0';

   m_avm_write_o       <= s_avm_write_i and not bulk_hold;
   m_avm_read_o        <= cart_read when cart_owns = '1' else s_avm_read_i and not bulk_hold;
   m_avm_address_o     <= cart_address when cart_owns = '1' else s_avm_address_i;
   m_avm_burstcount_o  <= X"01" when cart_owns = '1' else s_avm_burstcount_i;
   m_avm_writedata_o   <= s_avm_writedata_i;
   m_avm_byteenable_o  <= s_avm_byteenable_i;
   s_avm_waitrequest_o <= m_avm_waitrequest_i or bulk_hold;
   s_avm_readdata_o    <= m_avm_readdata_i;
   s_avm_readdatavalid_o <= m_avm_readdatavalid_i and not cart_owns;

   p_tables : process (clk_i)
      variable v_bank : natural range 0 to 127;
      variable v_size : natural;
   begin
      if rising_edge(clk_i) then
         loading_d <= cart_loading_i;
         if cart_loading_i = '1' and loading_d = '0' then
            lo_present <= (others => '0');
            hi_present <= (others => '0');
         end if;
         if cart_bank_wr_i = '1' then
            v_bank := to_integer(unsigned(cart_bank_num_i(6 downto 0)));
            v_size := to_integer(unsigned(cart_bank_size_i));
            if cart_c128_i = '1' then
               -- ROML is $8000-$BFFF, ROMH $C000-$FFFF
               if unsigned(cart_bank_laddr_i) < X"C000" then
                  lo_addr(v_bank)    <= cart_bank_raddr_i(22 downto 1);
                  lo_mask(v_bank)    <= mirror_mask(cart_bank_size_i, 16#4000#);
                  lo_present(v_bank) <= '1';
               else
                  hi_addr(v_bank)    <= cart_bank_raddr_i(22 downto 1);
                  hi_mask(v_bank)    <= mirror_mask(cart_bank_size_i, 16#4000#);
                  hi_present(v_bank) <= '1';
               end if;
            elsif unsigned(cart_bank_laddr_i) <= X"8000" then
               -- ROML, and ROMH too: the second half of a 16 kB chip, else the same image
               lo_addr(v_bank)    <= cart_bank_raddr_i(22 downto 1);
               lo_mask(v_bank)    <= mirror_mask(cart_bank_size_i, 16#2000#);
               lo_present(v_bank) <= '1';
               hi_present(v_bank) <= '1';
               if v_size > 16#2000# then
                  hi_addr(v_bank) <= std_logic_vector(unsigned(cart_bank_raddr_i(22 downto 1)) + 16#1000#);
                  hi_mask(v_bank) <= mirror_mask(std_logic_vector(to_unsigned(v_size - 16#2000#, 16)), 16#2000#);
               else
                  hi_addr(v_bank) <= cart_bank_raddr_i(22 downto 1);
                  hi_mask(v_bank) <= mirror_mask(cart_bank_size_i, 16#2000#);
               end if;
            else
               hi_addr(v_bank)    <= cart_bank_raddr_i(22 downto 1);
               hi_mask(v_bank)    <= mirror_mask(cart_bank_size_i, 16#2000#);
               hi_present(v_bank) <= '1';
            end if;
         end if;
      end if;
   end process p_tables;

   p_reader : process (clk_i)
      variable v_bank : natural range 0 to 127;
      variable v_off  : t_off;
      variable v_rd   : unsigned(8 downto 0);
   begin
      if rising_edge(clk_i) then
         if m_avm_waitrequest_i = '0' then
            cart_read <= '0';
         end if;

         case state is
            when IDLE_ST =>
               if pending = '1' and bulk_rd = 0 and bulk_wr = 0 then
                  if vic_req_i /= vic_seen then
                     vic_seen <= vic_req_i;
                     key      <= vic_key_i;
                     key_cnt  <= vic_req_i;
                     for_vic  <= '1';
                  else
                     cpu_seen <= cpu_req_i;
                     key      <= cpu_key_i;
                     key_cnt  <= cpu_req_i;
                     for_vic  <= '0';
                  end if;
                  state <= ADDR_ST;
               end if;

            when ADDR_ST =>
               v_bank := to_integer(unsigned(key(20 downto 14)));
               v_off  := unsigned(key(13 downto 0));
               if key(21) = '1' then
                  v_off := v_off and hi_mask(v_bank);
                  cart_address <= std_logic_vector(resize(unsigned(hi_addr(v_bank)) + v_off(13 downto 1), 32));
               else
                  v_off := v_off and lo_mask(v_bank);
                  cart_address <= std_logic_vector(resize(unsigned(lo_addr(v_bank)) + v_off(13 downto 1), 32));
               end if;
               a0 <= v_off(0);
               if (key(21) = '1' and hi_present(v_bank) = '1') or (key(21) = '0' and lo_present(v_bank) = '1') then
                  cart_read <= '1';
                  state     <= READ_ST;
               else
                  -- No chip here: the bus floats high
                  if for_vic = '1' then
                     vic_data_o <= X"FF";
                     vic_done_o <= key_cnt;
                  else
                     cpu_data_o <= X"FF";
                     cpu_done_o <= key_cnt;
                  end if;
                  state <= IDLE_ST;
               end if;

            when READ_ST =>
               if m_avm_readdatavalid_i = '1' then
                  if for_vic = '1' then
                     vic_data_o <= m_avm_readdata_i(7 downto 0);
                     if a0 = '1' then
                        vic_data_o <= m_avm_readdata_i(15 downto 8);
                     end if;
                     vic_done_o <= key_cnt;
                  else
                     cpu_data_o <= m_avm_readdata_i(7 downto 0);
                     if a0 = '1' then
                        cpu_data_o <= m_avm_readdata_i(15 downto 8);
                     end if;
                     cpu_done_o <= key_cnt;
                  end if;
                  state <= IDLE_ST;
               end if;
         end case;

         -- Bulk transfers in flight
         v_rd := bulk_rd;
         if m_avm_readdatavalid_i = '1' and cart_owns = '0' and v_rd /= 0 then
            v_rd := v_rd - 1;
         end if;
         if s_avm_read_i = '1' and bulk_hold = '0' and m_avm_waitrequest_i = '0' then
            v_rd := v_rd + unsigned(s_avm_burstcount_i);
         end if;
         bulk_rd <= v_rd;
         if s_avm_write_i = '1' and bulk_hold = '0' and m_avm_waitrequest_i = '0' then
            if bulk_wr = 0 then
               bulk_wr <= unsigned(s_avm_burstcount_i) - 1;
            else
               bulk_wr <= bulk_wr - 1;
            end if;
         end if;

         if rst_i = '1' then
            cart_read    <= '0';
            state        <= IDLE_ST;
            cpu_seen     <= cpu_req_i;
            vic_seen     <= vic_req_i;
            bulk_rd      <= (others => '0');
            bulk_wr      <= (others => '0');
            cpu_data_o   <= X"FF";
            vic_data_o   <= X"FF";
            cpu_done_o   <= cpu_req_i;
            vic_done_o   <= vic_req_i;
         end if;
      end if;
   end process p_reader;

end architecture synthesis;
