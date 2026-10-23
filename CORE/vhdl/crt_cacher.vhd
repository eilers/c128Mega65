----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- crt_cacher: keeps the cartridge's active ROML and ROMH banks in two block RAMs, read from
-- the CRT file in HyperRAM. Each side holds 2**G_CACHE_SIZE slots of 8 kB; a C128 cartridge's
-- 16 kB windows use pairs of slots. A chip smaller than its slot is mirrored through it, and a
-- bank or window the file has no chip for reads $FF.
--
-- The core asks for banks by number (bank_lo_i, bank_hi_i). The slot each one maps to is on
-- cache_addr_*_o, and ack_*_o and ready_o say which banks those slots hold and whether they are
-- loaded. A slot is only marked valid once a load into it has finished, so an aborted load is
-- never read as data.
--
-- Everything runs on the HyperRAM clock; the caller does the clock domain crossings.
--
-- After MJoergen's crt_cacher for C64MEGA65 (2023, GPL v3). C128 version by Jeff Flitton, 2026.
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity crt_cacher is
   generic (
      G_CACHE_SIZE : natural
   );
   port (
      clk_i               : in  std_logic;
      rst_i               : in  std_logic;

      -- From the parser
      cart_valid_i        : in  std_logic;
      cart_c128_i         : in  std_logic;
      cart_loading_i      : in  std_logic;                         -- parsing a new file
      cart_bank_laddr_i   : in  std_logic_vector(15 downto 0);
      cart_bank_size_i    : in  std_logic_vector(15 downto 0);
      cart_bank_num_i     : in  std_logic_vector(15 downto 0);
      cart_bank_raddr_i   : in  std_logic_vector(24 downto 0);     -- byte address in HyperRAM
      cart_bank_wr_i      : in  std_logic;

      -- From and to the core
      bank_lo_i           : in  std_logic_vector( 6 downto 0);
      bank_hi_i           : in  std_logic_vector( 6 downto 0);
      ack_lo_o            : out std_logic_vector( 6 downto 0);     -- the bank cache_addr_lo_o holds
      ack_hi_o            : out std_logic_vector( 6 downto 0);
      ready_o             : out std_logic;                         -- both slots are loaded
      cache_addr_lo_o     : out std_logic_vector(G_CACHE_SIZE-1 downto 0);
      cache_addr_hi_o     : out std_logic_vector(G_CACHE_SIZE-1 downto 0);

      -- HyperRAM, read only
      avm_write_o         : out std_logic;
      avm_read_o          : out std_logic;
      avm_address_o       : out std_logic_vector(21 downto 0);     -- word address
      avm_writedata_o     : out std_logic_vector(15 downto 0);
      avm_byteenable_o    : out std_logic_vector( 1 downto 0);
      avm_burstcount_o    : out std_logic_vector( 7 downto 0);
      avm_readdata_i      : in  std_logic_vector(15 downto 0);
      avm_readdatavalid_i : in  std_logic;
      avm_waitrequest_i   : in  std_logic;

      -- Write port of the LO and HI block RAMs: slot & word
      bram_address_o      : out std_logic_vector(G_CACHE_SIZE+11 downto 0);
      bram_data_o         : out std_logic_vector(15 downto 0);
      bram_lo_wren_o      : out std_logic;
      bram_hi_wren_o      : out std_logic
   );
end entity crt_cacher;

architecture synthesis of crt_cacher is

   constant C_SLOTS     : natural := 2**G_CACHE_SIZE;
   constant C_BURST     : natural := 128;    -- words per HyperRAM read burst (256 bytes)

   subtype t_bank   is std_logic_vector(6 downto 0);
   subtype t_slot   is unsigned(G_CACHE_SIZE-1 downto 0);
   subtype t_bursts is unsigned(6 downto 0);  -- 1 to 64 bursts, i.e. up to 16 kB

   -- Where each bank's image starts (word address) and how many bursts long it is
   type t_addr_tbl is array (0 to 127) of std_logic_vector(21 downto 0);
   type t_len_tbl  is array (0 to 127) of t_bursts;
   signal lo_addr      : t_addr_tbl := (others => (others => '0'));
   signal hi_addr      : t_addr_tbl := (others => (others => '0'));
   signal lo_len       : t_len_tbl  := (others => to_unsigned(32, 7));
   signal hi_len       : t_len_tbl  := (others => to_unsigned(32, 7));
   signal lo_present   : std_logic_vector(0 to 127) := (others => '0');   -- the file has this chip
   signal hi_present   : std_logic_vector(0 to 127) := (others => '0');
   signal loading_d    : std_logic := '0';

   type t_tags is array (0 to C_SLOTS-1) of t_bank;
   signal lo_tag       : t_tags := (others => (others => '0'));
   signal hi_tag       : t_tags := (others => (others => '0'));
   signal lo_valid     : std_logic_vector(C_SLOTS-1 downto 0) := (others => '0');
   signal hi_valid     : std_logic_vector(C_SLOTS-1 downto 0) := (others => '0');
   signal lo_next      : t_slot := (others => '0');   -- round-robin victim
   signal hi_next      : t_slot := (others => '0');
   signal lo_req       : t_bank := (others => '0');   -- the request lo_slot belongs to
   signal hi_req       : t_bank := (others => '0');
   signal lo_slot      : t_slot := (others => '0');
   signal hi_slot      : t_slot := (others => '0');
   signal cart_valid_d : std_logic := '0';

   type t_state is (IDLE_ST, READ_ST);
   signal state        : t_state := IDLE_ST;
   signal ld_hi        : std_logic := '0';            -- the load is for the HI side
   signal ld_fill      : std_logic := '0';            -- no chip: fill the slot with $FF
   signal ld_slot      : t_slot := (others => '0');
   signal ld_bank      : t_bank := (others => '0');
   signal ld_base      : unsigned(21 downto 0) := (others => '0');
   signal ld_len       : t_bursts := (others => '0');
   signal ld_src       : t_bursts := (others => '0'); -- burst of the image being read
   signal ld_word      : unsigned(12 downto 0) := (others => '0');  -- word in the slot
   signal ld_last      : unsigned(12 downto 0) := (others => '0');  -- last word of the slot
   signal ld_stale     : std_logic := '0';            -- a new cartridge arrived during the load

   -- Bursts of an image of the given byte size, capped at max_bytes
   function bursts(size : std_logic_vector(15 downto 0); max_bytes : natural) return t_bursts is
      variable v : natural := to_integer(unsigned(size));
   begin
      if v = 0 or v > max_bytes then
         v := max_bytes;
      end if;
      return to_unsigned((v + 255) / 256, 7);
   end function bursts;

   -- Slot holding a bank, or C_SLOTS if none. A C128 cartridge only uses even slots.
   function lookup(tags : t_tags; bank : t_bank; c128 : std_logic) return natural is
   begin
      for i in 0 to C_SLOTS-1 loop
         if (c128 = '0' or i mod 2 = 0) and tags(i) = bank then
            return i;
         end if;
      end loop;
      return C_SLOTS;
   end function lookup;

   signal ld_abort     : std_logic;

begin

   avm_write_o      <= '0';
   avm_writedata_o  <= (others => '0');
   avm_byteenable_o <= (others => '1');
   avm_burstcount_o <= std_logic_vector(to_unsigned(C_BURST, 8));

   ack_lo_o         <= lo_req;
   ack_hi_o         <= hi_req;
   cache_addr_lo_o  <= std_logic_vector(lo_slot);
   cache_addr_hi_o  <= std_logic_vector(hi_slot);
   ready_o          <= cart_valid_i and lo_valid(to_integer(lo_slot)) and hi_valid(to_integer(hi_slot));

   -- Stop at the next burst boundary if the side moved on to another slot or bank
   ld_abort <= '1' when cart_valid_i = '0' or ld_stale = '1' or
                        (ld_hi = '0' and (lo_slot /= ld_slot or lo_tag(to_integer(ld_slot)) /= ld_bank)) or
                        (ld_hi = '1' and (hi_slot /= ld_slot or hi_tag(to_integer(ld_slot)) /= ld_bank)) else '0';

   p_cacher : process (clk_i)
      variable v_bank  : natural range 0 to 127;
      variable v_hit   : natural range 0 to C_SLOTS;
      variable v_slot  : t_slot;
      variable v_step  : t_slot;
      variable v_init  : boolean;
      variable v_size  : natural;
      variable v_fill  : std_logic;
   begin
      if rising_edge(clk_i) then
         bram_lo_wren_o <= '0';
         bram_hi_wren_o <= '0';
         if avm_waitrequest_i = '0' then
            avm_read_o <= '0';
         end if;

         -- Bank tables, filled by the parser one CHIP packet at a time
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
                  lo_len(v_bank)     <= bursts(cart_bank_size_i, 16#4000#);
                  lo_present(v_bank) <= '1';
               else
                  hi_addr(v_bank)    <= cart_bank_raddr_i(22 downto 1);
                  hi_len(v_bank)     <= bursts(cart_bank_size_i, 16#4000#);
                  hi_present(v_bank) <= '1';
               end if;
            elsif unsigned(cart_bank_laddr_i) <= X"8000" then
               -- ROML, and ROMH too: the second half of a 16 kB chip, else the same image
               lo_addr(v_bank)    <= cart_bank_raddr_i(22 downto 1);
               lo_len(v_bank)     <= bursts(cart_bank_size_i, 16#2000#);
               lo_present(v_bank) <= '1';
               hi_present(v_bank) <= '1';
               if v_size > 16#2000# then
                  hi_addr(v_bank) <= std_logic_vector(unsigned(cart_bank_raddr_i(22 downto 1)) + 16#1000#);
                  hi_len(v_bank)  <= bursts(std_logic_vector(to_unsigned(v_size - 16#2000#, 16)), 16#2000#);
               else
                  hi_addr(v_bank) <= cart_bank_raddr_i(22 downto 1);
                  hi_len(v_bank)  <= bursts(cart_bank_size_i, 16#2000#);
               end if;
            else
               hi_addr(v_bank)    <= cart_bank_raddr_i(22 downto 1);
               hi_len(v_bank)     <= bursts(cart_bank_size_i, 16#2000#);
               hi_present(v_bank) <= '1';
            end if;
         end if;

         -- Loader: one slot at a time, HI first (an Ultimax cartridge's reset vector is there).
         -- It comes before the mapping below, so a slot that is re-tagged on the clock its load
         -- ends stays invalid.
         case state is
            when IDLE_ST =>
               if cart_valid_i = '1' and cart_valid_d = '1' and
                  (hi_valid(to_integer(hi_slot)) = '0' or lo_valid(to_integer(lo_slot)) = '0') then
                  if hi_valid(to_integer(hi_slot)) = '0' then
                     v_fill  := not hi_present(to_integer(unsigned(hi_req)));
                     ld_hi   <= '1';
                     ld_slot <= hi_slot;
                     ld_bank <= hi_req;
                     ld_base <= unsigned(hi_addr(to_integer(unsigned(hi_req))));
                     ld_len  <= hi_len(to_integer(unsigned(hi_req)));
                     avm_address_o <= hi_addr(to_integer(unsigned(hi_req)));
                  else
                     v_fill  := not lo_present(to_integer(unsigned(lo_req)));
                     ld_hi   <= '0';
                     ld_slot <= lo_slot;
                     ld_bank <= lo_req;
                     ld_base <= unsigned(lo_addr(to_integer(unsigned(lo_req))));
                     ld_len  <= lo_len(to_integer(unsigned(lo_req)));
                     avm_address_o <= lo_addr(to_integer(unsigned(lo_req)));
                  end if;
                  ld_fill    <= v_fill;
                  ld_src     <= (others => '0');
                  ld_word    <= (others => '0');
                  ld_stale   <= '0';
                  ld_last    <= to_unsigned(4095, 13);
                  if cart_c128_i = '1' then
                     ld_last <= to_unsigned(8191, 13);
                  end if;
                  avm_read_o <= not v_fill;
                  state      <= READ_ST;
               end if;

            when READ_ST =>
               -- A word from HyperRAM, or one $FFFF a clock while filling
               if avm_readdatavalid_i = '1' or ld_fill = '1' then
                  bram_address_o <= std_logic_vector(resize(ld_slot, G_CACHE_SIZE+12) sll 12) or
                                    std_logic_vector(resize(ld_word, G_CACHE_SIZE+12));
                  bram_data_o    <= avm_readdata_i;
                  if ld_fill = '1' then
                     bram_data_o <= (others => '1');
                  end if;
                  bram_lo_wren_o <= not ld_hi;
                  bram_hi_wren_o <= ld_hi;
                  ld_word        <= ld_word + 1;

                  if ld_word(6 downto 0) = C_BURST - 1 then
                     if ld_abort = '1' then
                        state <= IDLE_ST;
                     elsif ld_word = ld_last then
                        -- The slot is full
                        if ld_hi = '1' then
                           hi_valid(to_integer(ld_slot)) <= '1';
                        else
                           lo_valid(to_integer(ld_slot)) <= '1';
                        end if;
                        state <= IDLE_ST;
                     else
                        -- Next burst, wrapping round the image if it is smaller than the slot
                        if ld_src + 1 = ld_len then
                           ld_src        <= (others => '0');
                           avm_address_o <= std_logic_vector(ld_base);
                        else
                           ld_src        <= ld_src + 1;
                           avm_address_o <= std_logic_vector(ld_base + shift_left(resize(ld_src + 1, 22), 7));
                        end if;
                        avm_read_o <= not ld_fill;
                     end if;
                  end if;
               end if;
         end case;

         -- A new cartridge empties the cache and maps both requests afresh
         v_init := cart_valid_i = '1' and cart_valid_d = '0';
         cart_valid_d <= cart_valid_i;
         if v_init then
            ld_stale <= '1';
            lo_valid <= (others => '0');
            hi_valid <= (others => '0');
            lo_tag   <= (others => (others => '1'));
            hi_tag   <= (others => (others => '1'));
         end if;
         if cart_c128_i = '1' then
            v_step := to_unsigned(2, G_CACHE_SIZE);
         else
            v_step := to_unsigned(1, G_CACHE_SIZE);
         end if;

         -- Map each side's request to a slot: a hit, or the round-robin victim, which needs a load
         if cart_valid_i = '1' and (bank_lo_i /= lo_req or v_init) then
            lo_req <= bank_lo_i;
            v_hit  := lookup(lo_tag, bank_lo_i, cart_c128_i);
            if v_hit /= C_SLOTS and not v_init then
               lo_slot <= to_unsigned(v_hit, G_CACHE_SIZE);
            else
               v_slot := lo_next;
               if v_init then
                  v_slot := (others => '0');
               end if;
               lo_slot                      <= v_slot;
               lo_tag(to_integer(v_slot))   <= bank_lo_i;
               lo_valid(to_integer(v_slot)) <= '0';
               lo_next                      <= v_slot + v_step;
            end if;
         end if;

         if cart_valid_i = '1' and (bank_hi_i /= hi_req or v_init) then
            hi_req <= bank_hi_i;
            v_hit  := lookup(hi_tag, bank_hi_i, cart_c128_i);
            if v_hit /= C_SLOTS and not v_init then
               hi_slot <= to_unsigned(v_hit, G_CACHE_SIZE);
            else
               v_slot := hi_next;
               if v_init then
                  v_slot := (others => '0');
               end if;
               hi_slot                      <= v_slot;
               hi_tag(to_integer(v_slot))   <= bank_hi_i;
               hi_valid(to_integer(v_slot)) <= '0';
               hi_next                      <= v_slot + v_step;
            end if;
         end if;

         if rst_i = '1' then
            avm_read_o   <= '0';
            state        <= IDLE_ST;
            lo_valid     <= (others => '0');
            hi_valid     <= (others => '0');
            cart_valid_d <= '0';
         end if;
      end if;
   end process p_cacher;

end architecture synthesis;
