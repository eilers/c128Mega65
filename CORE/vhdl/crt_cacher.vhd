----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- crt_cacher: keeps the cartridge's active ROML and ROMH banks in two block RAMs, read from
-- the CRT file in HyperRAM. Each side holds 2**G_CACHE_SIZE slots of 8 kB; a C128 cartridge's
-- 16 kB windows use pairs of slots. A chip smaller than its slot is mirrored through it, and a
-- bank or window the file has no chip for reads $FF.
--
-- The core asks for banks by number (bank_lo_i, bank_hi_i). The slot each one maps to is on
-- cache_addr_*_o, ack_*_o says which bank that slot holds, and view_*_o which of its 256-byte
-- bursts are loaded (bit n: burst n; a C128 window's pair of slots has 64). Slots fill a burst at
-- a time, and a burst the core waits for (want_*_i) comes next, so the C128 waits for one burst
-- rather than the whole bank. A burst is only marked once all of it is written, so an aborted
-- load is never read as data. ready_o says both slots are complete.
--
-- It also carries out EasyFlash's flash jobs (crt_flash), between loads: a program reads the
-- byte's word from HyperRAM, writes it back ANDed with the new byte and patches the cached copy;
-- an erase fills the banks with $FF and drops their cached copies. Each bank the file lacks has
-- a spare 8 kB bank in the 1 MiB at G_SPARE_ADDRESS, filled with $FF once the file is parsed,
-- while the C128 waits in reset (prefill_o); a first program into one only has to map it.
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
      G_CACHE_SIZE    : natural;
      G_SPARE_ADDRESS : std_logic_vector(21 downto 0) := (others => '0')  -- word address
   );
   port (
      clk_i               : in  std_logic;
      rst_i               : in  std_logic;

      -- From the parser
      cart_valid_i        : in  std_logic;
      cart_size_i         : in  std_logic_vector(22 downto 0);     -- the file's length in bytes
      cart_c128_i         : in  std_logic;
      cart_ef_i           : in  std_logic := '0';                  -- an EasyFlash
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
      view_lo_o           : out std_logic_vector(63 downto 0);     -- loaded bursts of that slot
      view_hi_o           : out std_logic_vector(63 downto 0);
      want_lo_i           : in  std_logic_vector(13 downto 0) := (others => '0');  -- valid, bank, burst
      want_hi_i           : in  std_logic_vector(13 downto 0) := (others => '0');

      -- EasyFlash flash jobs from crt_flash, index 0 for ROML's chip and 1 for ROMH's. A job is
      -- new when its req toggles; done follows req once it is finished.
      fl_req_i            : in  std_logic_vector( 1 downto 0);
      fl_lo_job_i         : in  std_logic_vector(28 downto 0);
      fl_hi_job_i         : in  std_logic_vector(28 downto 0);
      fl_done_o           : out std_logic_vector( 1 downto 0);
      fl_fail_o           : out std_logic_vector( 1 downto 0);
      prefill_o           : out std_logic;                         -- filling the spare banks

      -- HyperRAM
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
   type t_bits is array (0 to C_SLOTS-1) of std_logic_vector(31 downto 0);
   signal lo_bits      : t_bits := (others => (others => '0'));   -- loaded bursts of each slot
   signal hi_bits      : t_bits := (others => (others => '0'));
   signal lo_next      : t_slot := (others => '0');   -- round-robin victim
   signal hi_next      : t_slot := (others => '0');
   signal lo_req       : t_bank := (others => '0');   -- the request lo_slot belongs to
   signal hi_req       : t_bank := (others => '0');
   signal lo_slot      : t_slot := (others => '0');
   signal hi_slot      : t_slot := (others => '0');
   signal cart_valid_d : std_logic := '0';

   -- Each side's request, slot and loaded bursts, all taken on the same clock: what the core
   -- sees, and what the loader decides by
   signal lo_ack       : t_bank := (others => '0');
   signal hi_ack       : t_bank := (others => '0');
   signal lo_cur       : t_slot := (others => '0');
   signal hi_cur       : t_slot := (others => '0');
   signal lo_view      : std_logic_vector(63 downto 0) := (others => '0');
   signal hi_view      : std_logic_vector(63 downto 0) := (others => '0');
   signal lo_full      : std_logic := '0';
   signal hi_full      : std_logic := '0';
   signal lo_seq       : unsigned(5 downto 0) := (others => '0');   -- next burst in order
   signal hi_seq       : unsigned(5 downto 0) := (others => '0');
   signal want_lo      : std_logic_vector(13 downto 0) := (others => '0');
   signal want_hi      : std_logic_vector(13 downto 0) := (others => '0');

   type t_state is (IDLE_ST, SRC_ST, READ_ST, DONE_ST, FL_READ_ST, FL_WRITE_ST);
   signal state        : t_state := IDLE_ST;
   signal ld_hi        : std_logic := '0';            -- the load is for the HI side
   signal ld_fill      : std_logic := '0';            -- no chip: fill the burst with $FF
   signal ld_slot      : t_slot := (others => '0');
   signal ld_bank      : t_bank := (others => '0');
   signal ld_base      : unsigned(21 downto 0) := (others => '0');
   signal ld_len       : t_bursts := (others => '0');
   signal ld_src       : t_bursts := (others => '0'); -- burst of the image being read
   signal ld_word      : unsigned(12 downto 0) := (others => '0');  -- word in the slot (pair)
   signal ld_stale     : std_logic := '0';            -- a new cartridge arrived during the load

   -- Flash jobs: the step the current one is at, and the HyperRAM accesses it makes
   type t_job is (J_IDLE, J_LOOKUP, J_PICK, J_FILL, J_SET, J_INVAL, J_NEXT, J_READ, J_WRITE,
                  J_PATCH, J_DONE);
   signal fl_step      : t_job := J_IDLE;
   signal fl_seen      : std_logic_vector(1 downto 0) := "00";
   signal fl_ch        : std_logic := '0';                         -- 1: ROMH's chip
   signal fl_op        : std_logic_vector(1 downto 0) := "00";
   signal fl_bank      : unsigned(5 downto 0) := (others => '0');
   signal fl_last      : unsigned(5 downto 0) := (others => '0');  -- an erase's last bank
   signal fl_off       : unsigned(12 downto 0) := (others => '0');
   signal fl_data      : std_logic_vector(7 downto 0) := (others => '0');
   signal fl_base      : unsigned(21 downto 0) := (others => '0');
   signal fl_len       : t_bursts := (others => '0');
   signal fl_pres      : std_logic := '0';
   signal fl_addr      : unsigned(21 downto 0) := (others => '0');  -- the next word
   signal fl_left      : unsigned(13 downto 0) := (others => '0');  -- words still to fill
   signal fl_set       : std_logic := '0';                          -- map fl_bank's spare
   signal fl_pre       : std_logic := '0';                          -- filling the spares
   signal pre_req      : std_logic := '0';
   signal fl_fail      : std_logic := '0';
   signal fl_write     : std_logic := '0';
   signal fl_old       : std_logic_vector(15 downto 0) := (others => '1');
   signal fl_word      : std_logic_vector(15 downto 0) := (others => '1');
   signal fl_be        : std_logic_vector(1 downto 0) := "11";
   signal fl_wants     : std_logic;
   signal spare_ok     : std_logic;

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

   -- A missing bank's spare: 8 kB each, ROMH's 64 after ROML's
   function spare_addr(hi : std_logic; bank : unsigned(5 downto 0)) return unsigned is
      variable v : unsigned(6 downto 0);
   begin
      v := hi & bank;
      return unsigned(G_SPARE_ADDRESS) + shift_left(resize(v, 22), 12);
   end function spare_addr;

   signal ld_abort     : std_logic;
   signal ready        : std_logic := '0';

   -- The burst after b, round a slot (32 bursts) or a C128 window (64)
   function next_burst(b : unsigned(5 downto 0); c128 : std_logic) return unsigned is
   begin
      if c128 = '0' then
         return '0' & (b(4 downto 0) + 1);
      end if;
      return b + 1;
   end function next_burst;

   -- The second slot of a C128 window's pair, which starts at an even slot
   function pair(s : t_slot) return natural is
   begin
      return to_integer(s(s'high downto 1) & '1');
   end function pair;

begin

   avm_write_o      <= fl_write;
   avm_writedata_o  <= fl_word;
   avm_byteenable_o <= fl_be;
   avm_burstcount_o <= X"01" when state = FL_READ_ST or state = FL_WRITE_ST else
                       std_logic_vector(to_unsigned(C_BURST, 8));

   fl_wants <= '1' when (fl_step = J_FILL and fl_left /= 0) or fl_step = J_READ or fl_step = J_WRITE else '0';
   -- Spare banks go in the second MiB of the cartridge's region, so the file must fit the first
   spare_ok  <= '1' when unsigned(cart_size_i) <= 16#100000# else '0';
   prefill_o <= fl_pre or pre_req;

   ack_lo_o         <= lo_ack;
   ack_hi_o         <= hi_ack;
   cache_addr_lo_o  <= std_logic_vector(lo_cur);
   cache_addr_hi_o  <= std_logic_vector(hi_cur);
   view_lo_o        <= lo_view;
   view_hi_o        <= hi_view;
   ready_o          <= ready;

   -- Drop a burst whose slot was given to another bank while it loaded
   ld_abort <= '1' when cart_valid_i = '0' or ld_stale = '1' or
                        (ld_hi = '0' and lo_tag(to_integer(ld_slot)) /= ld_bank) or
                        (ld_hi = '1' and hi_tag(to_integer(ld_slot)) /= ld_bank) else '0';

   p_cacher : process (clk_i)
      variable v_bank  : natural range 0 to 127;
      variable v_hit   : natural range 0 to C_SLOTS;
      variable v_slot  : t_slot;
      variable v_step  : t_slot;
      variable v_init  : boolean;
      variable v_size  : natural;
      variable v_fill  : std_logic;
      variable v_job   : std_logic_vector(28 downto 0);
      variable v_old   : std_logic_vector(7 downto 0);
      variable v_new   : std_logic_vector(7 downto 0);
      variable v_valid : std_logic;
      variable v_lo    : std_logic_vector(63 downto 0);
      variable v_hi    : std_logic_vector(63 downto 0);
      variable v_lof   : std_logic;
      variable v_hif   : std_logic;
      variable v_go    : boolean;
      variable v_side  : std_logic;
      variable v_burst : unsigned(5 downto 0);
   begin
      if rising_edge(clk_i) then
         -- Each side as the core sees it, all from this clock's state: the bank, its slot and the
         -- slot's loaded bursts. A C128 window spans a pair of slots; a C64 slot's bursts repeat.
         v_lo := lo_bits(to_integer(lo_slot)) & lo_bits(to_integer(lo_slot));
         v_hi := hi_bits(to_integer(hi_slot)) & hi_bits(to_integer(hi_slot));
         if cart_c128_i = '1' then
            v_lo := lo_bits(pair(lo_slot)) & lo_bits(to_integer(lo_slot));
            v_hi := hi_bits(pair(hi_slot)) & hi_bits(to_integer(hi_slot));
         end if;
         v_lof := '0';
         if v_lo = (v_lo'range => '1') then
            v_lof := '1';
         end if;
         v_hif := '0';
         if v_hi = (v_hi'range => '1') then
            v_hif := '1';
         end if;
         lo_view <= v_lo;
         hi_view <= v_hi;
         lo_full <= v_lof;
         hi_full <= v_hif;
         lo_ack  <= lo_req;
         hi_ack  <= hi_req;
         lo_cur  <= lo_slot;
         hi_cur  <= hi_slot;
         ready   <= cart_valid_i and v_lof and v_hif;
         want_lo <= want_lo_i;
         want_hi <= want_hi_i;

         bram_lo_wren_o <= '0';
         bram_hi_wren_o <= '0';
         if avm_waitrequest_i = '0' then
            avm_read_o <= '0';
         end if;
         fl_set <= '0';
         if cart_valid_i = '1' and cart_valid_d = '0' and cart_ef_i = '1' and spare_ok = '1' then
            pre_req <= '1';
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
               -- ROML, and ROMH too: the second half of a 16 kB chip, else the same image. An
               -- EasyFlash's ROMH chip is separate, so there an 8 kB chip leaves ROMH alone.
               lo_addr(v_bank)    <= cart_bank_raddr_i(22 downto 1);
               lo_len(v_bank)     <= bursts(cart_bank_size_i, 16#2000#);
               lo_present(v_bank) <= '1';
               if v_size > 16#2000# then
                  hi_addr(v_bank)    <= std_logic_vector(unsigned(cart_bank_raddr_i(22 downto 1)) + 16#1000#);
                  hi_len(v_bank)     <= bursts(std_logic_vector(to_unsigned(v_size - 16#2000#, 16)), 16#2000#);
                  hi_present(v_bank) <= '1';
               elsif cart_ef_i = '0' then
                  hi_addr(v_bank)    <= cart_bank_raddr_i(22 downto 1);
                  hi_len(v_bank)     <= bursts(cart_bank_size_i, 16#2000#);
                  hi_present(v_bank) <= '1';
               end if;
            else
               hi_addr(v_bank)    <= cart_bank_raddr_i(22 downto 1);
               hi_len(v_bank)     <= bursts(cart_bank_size_i, 16#2000#);
               hi_present(v_bank) <= '1';
            end if;
         elsif fl_set = '1' and cart_loading_i = '0' then
            -- A flash program into a bank the file lacks: map its spare bank, all $FF
            v_bank := to_integer(fl_bank);
            if fl_ch = '0' then
               lo_addr(v_bank)    <= std_logic_vector(spare_addr('0', fl_bank));
               lo_len(v_bank)     <= to_unsigned(32, 7);
               lo_present(v_bank) <= '1';
            else
               hi_addr(v_bank)    <= std_logic_vector(spare_addr('1', fl_bank));
               hi_len(v_bank)     <= to_unsigned(32, 7);
               hi_present(v_bank) <= '1';
            end if;
         end if;

         -- Loader: a burst at a time. One the core waits for comes first, then the rest of each
         -- slot in order, HI before LO (an Ultimax cartridge's reset vector is there). It comes
         -- before the mapping below, so a slot that is re-tagged on the clock a burst ends keeps
         -- that burst unmarked.
         case state is
            when IDLE_ST =>
               if cart_valid_i = '1' and cart_valid_d = '1' and (hi_full = '0' or lo_full = '0') then
                  v_go := false;
                  if want_hi(13) = '1' and want_hi(12 downto 6) = hi_ack and
                     hi_view(to_integer(unsigned(want_hi(5 downto 0)))) = '0' then
                     v_go    := true;
                     v_side  := '1';
                     v_burst := unsigned(want_hi(5 downto 0));
                  elsif want_lo(13) = '1' and want_lo(12 downto 6) = lo_ack and
                        lo_view(to_integer(unsigned(want_lo(5 downto 0)))) = '0' then
                     v_go    := true;
                     v_side  := '0';
                     v_burst := unsigned(want_lo(5 downto 0));
                  elsif hi_full = '0' then
                     -- Skip a burst that is in already
                     v_go    := hi_view(to_integer(hi_seq)) = '0';
                     v_side  := '1';
                     v_burst := hi_seq;
                     hi_seq  <= next_burst(hi_seq, cart_c128_i);
                  else
                     v_go    := lo_view(to_integer(lo_seq)) = '0';
                     v_side  := '0';
                     v_burst := lo_seq;
                     lo_seq  <= next_burst(lo_seq, cart_c128_i);
                  end if;

                  if v_go then
                     if v_side = '1' then
                        v_bank  := to_integer(unsigned(hi_ack));
                        v_fill  := not hi_present(v_bank);
                        ld_slot <= hi_cur;
                        ld_bank <= hi_ack;
                        ld_base <= unsigned(hi_addr(v_bank));
                        ld_len  <= hi_len(v_bank);
                     else
                        v_bank  := to_integer(unsigned(lo_ack));
                        v_fill  := not lo_present(v_bank);
                        ld_slot <= lo_cur;
                        ld_bank <= lo_ack;
                        ld_base <= unsigned(lo_addr(v_bank));
                        ld_len  <= lo_len(v_bank);
                     end if;
                     ld_hi    <= v_side;
                     ld_fill  <= v_fill;
                     ld_src   <= resize(v_burst, 7);
                     ld_word  <= v_burst & "0000000";
                     ld_stale <= '0';
                     state    <= SRC_ST;
                  end if;
               elsif fl_wants = '1' then
                  -- A flash access, one word, between loads
                  avm_address_o <= std_logic_vector(fl_addr);
                  if fl_step = J_READ then
                     avm_read_o <= '1';
                     state      <= FL_READ_ST;
                  else
                     fl_write <= '1';
                     state    <= FL_WRITE_ST;
                  end if;
               end if;

            when FL_READ_ST =>
               -- A program: the old byte ANDed with the new one, which failed if bits had to rise
               if avm_readdatavalid_i = '1' then
                  v_old := avm_readdata_i(7 downto 0);
                  if fl_off(0) = '1' then
                     v_old := avm_readdata_i(15 downto 8);
                  end if;
                  v_new   := v_old and fl_data;
                  fl_old  <= avm_readdata_i;
                  fl_word <= v_new & v_new;
                  fl_be   <= fl_off(0) & not fl_off(0);
                  if v_new /= fl_data then
                     fl_fail <= '1';
                  end if;
                  fl_step <= J_WRITE;
                  state   <= IDLE_ST;
               end if;

            when FL_WRITE_ST =>
               if avm_waitrequest_i = '0' then
                  fl_write <= '0';
                  fl_addr  <= fl_addr + 1;
                  if fl_step = J_FILL then
                     fl_left <= fl_left - 1;
                  else
                     fl_step <= J_PATCH;
                  end if;
                  state <= IDLE_ST;
               end if;

            when SRC_ST =>
               -- The burst of the image to read: an image smaller than the slot is mirrored
               if ld_src >= ld_len then
                  ld_src <= ld_src - ld_len;
               else
                  avm_address_o <= std_logic_vector(ld_base + shift_left(resize(ld_src, 22), 7));
                  avm_read_o    <= not ld_fill;
                  state         <= READ_ST;
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
                     -- The burst is in. Its bit is in the slot that holds it: the second of a
                     -- C128 window's pair for the window's upper half.
                     if ld_abort = '0' then
                        v_slot := ld_slot + resize(ld_word(12 downto 12), G_CACHE_SIZE);
                        if ld_hi = '1' then
                           hi_bits(to_integer(v_slot))(to_integer(ld_word(11 downto 7))) <= '1';
                        else
                           lo_bits(to_integer(v_slot))(to_integer(ld_word(11 downto 7))) <= '1';
                        end if;
                     end if;
                     state <= DONE_ST;
                  end if;
               end if;

            when DONE_ST =>
               -- One clock for the views to show the new burst, so it is not loaded twice
               state <= IDLE_ST;
         end case;

         -- Flash jobs, a step at a time; FL_READ_ST and FL_WRITE_ST move the HyperRAM steps on
         case fl_step is
            when J_IDLE =>
               if pre_req = '1' then
                  -- A new EasyFlash: fill the spares of the banks it lacks, ROML's then ROMH's
                  pre_req <= '0';
                  fl_pre  <= '1';
                  fl_op   <= "00";
                  fl_ch   <= '0';
                  fl_bank <= (others => '0');
                  fl_last <= (others => '1');
                  fl_step <= J_LOOKUP;
               elsif fl_req_i /= fl_seen then
                  if fl_req_i(0) /= fl_seen(0) then
                     v_job      := fl_lo_job_i;
                     fl_ch      <= '0';
                     fl_seen(0) <= fl_req_i(0);
                  else
                     v_job      := fl_hi_job_i;
                     fl_ch      <= '1';
                     fl_seen(1) <= fl_req_i(1);
                  end if;
                  fl_op   <= v_job(28 downto 27);
                  fl_bank <= unsigned(v_job(26 downto 21));
                  fl_last <= unsigned(v_job(26 downto 21));
                  fl_off  <= unsigned(v_job(20 downto 8));
                  fl_data <= v_job(7 downto 0);
                  fl_fail <= '0';
                  if v_job(28 downto 27) = "11" then
                     -- Chip erase: all 64 banks
                     fl_bank <= (others => '0');
                     fl_last <= (others => '1');
                  elsif v_job(28 downto 27) = "10" then
                     -- Sector erase: the 8 banks of the bank's 64 kB sector
                     fl_bank <= unsigned(v_job(26 downto 24)) & "000";
                     fl_last <= unsigned(v_job(26 downto 24)) & "111";
                  end if;
                  fl_step <= J_LOOKUP;
               end if;

            when J_LOOKUP =>
               v_bank := to_integer(fl_bank);
               if fl_ch = '0' then
                  fl_base <= unsigned(lo_addr(v_bank));
                  fl_len  <= lo_len(v_bank);
                  fl_pres <= lo_present(v_bank);
               else
                  fl_base <= unsigned(hi_addr(v_bank));
                  fl_len  <= hi_len(v_bank);
                  fl_pres <= hi_present(v_bank);
               end if;
               fl_step <= J_PICK;

            when J_PICK =>
               fl_word <= X"FFFF";
               fl_be   <= "11";
               if cart_valid_i = '0' then
                  -- Another file is on its way: drop the job
                  fl_step <= J_DONE;
               elsif fl_op = "00" then
                  if fl_pres = '0' then
                     fl_addr <= spare_addr(fl_ch, fl_bank);
                     fl_left <= to_unsigned(4096, 14);
                     fl_step <= J_FILL;
                  else
                     fl_step <= J_NEXT;
                  end if;
               elsif fl_op = "01" then
                  if fl_pres = '1' and resize(fl_off(12 downto 8), 7) < fl_len then
                     fl_addr <= fl_base + fl_off(12 downto 1);
                     fl_step <= J_READ;
                  elsif fl_pres = '0' and spare_ok = '1' then
                     -- Map the bank's spare, then program it
                     fl_set  <= '1';
                     fl_step <= J_SET;
                  else
                     if fl_data /= X"FF" then
                        fl_fail <= '1';
                     end if;
                     fl_step <= J_DONE;
                  end if;
               elsif fl_pres = '1' then
                  fl_addr <= fl_base;
                  fl_left <= shift_left(resize(fl_len, 14), 7);
                  fl_step <= J_FILL;
               else
                  -- An erase skips a bank the file lacks: it reads $FF anyway
                  fl_step <= J_NEXT;
               end if;

            when J_FILL =>
               if fl_left = 0 and state = IDLE_ST then
                  if fl_op = "00" then
                     fl_step <= J_NEXT;
                  else
                     fl_step <= J_INVAL;
                  end if;
               end if;

            when J_SET =>
               fl_step <= J_LOOKUP;

            when J_INVAL =>
               -- The erased bank's cached copy is stale; a selected one is loaded again
               if fl_ch = '0' then
                  v_hit := lookup(lo_tag, '0' & std_logic_vector(fl_bank), cart_c128_i);
                  if v_hit /= C_SLOTS then
                     lo_bits(v_hit) <= (others => '0');
                     if cart_c128_i = '1' then
                        lo_bits(pair(to_unsigned(v_hit, G_CACHE_SIZE))) <= (others => '0');
                     end if;
                  end if;
               else
                  v_hit := lookup(hi_tag, '0' & std_logic_vector(fl_bank), cart_c128_i);
                  if v_hit /= C_SLOTS then
                     hi_bits(v_hit) <= (others => '0');
                     if cart_c128_i = '1' then
                        hi_bits(pair(to_unsigned(v_hit, G_CACHE_SIZE))) <= (others => '0');
                     end if;
                  end if;
               end if;
               fl_step <= J_NEXT;

            when J_NEXT =>
               if fl_bank = fl_last and fl_op = "00" and fl_ch = '0' and cart_valid_i = '1' then
                  fl_ch   <= '1';
                  fl_bank <= (others => '0');
                  fl_step <= J_LOOKUP;
               elsif fl_bank = fl_last or cart_valid_i = '0' then
                  fl_step <= J_DONE;
               else
                  fl_bank <= fl_bank + 1;
                  fl_step <= J_LOOKUP;
               end if;

            when J_READ | J_WRITE =>
               null;

            when J_PATCH =>
               -- Put the programmed byte in the bank's cached copy, if it has one
               if fl_ch = '0' then
                  v_hit := lookup(lo_tag, '0' & std_logic_vector(fl_bank), cart_c128_i);
               else
                  v_hit := lookup(hi_tag, '0' & std_logic_vector(fl_bank), cart_c128_i);
               end if;
               if v_hit /= C_SLOTS then
                  bram_address_o <= std_logic_vector(to_unsigned(v_hit, G_CACHE_SIZE)) & std_logic_vector(fl_off(12 downto 1));
                  bram_data_o    <= fl_old(15 downto 8) & fl_word(7 downto 0);
                  if fl_off(0) = '1' then
                     bram_data_o <= fl_word(15 downto 8) & fl_old(7 downto 0);
                  end if;
                  bram_lo_wren_o <= not fl_ch;
                  bram_hi_wren_o <= fl_ch;
               end if;
               fl_step <= J_DONE;

            when J_DONE =>
               if fl_op = "00" then
                  fl_pre <= '0';
               elsif fl_ch = '0' then
                  fl_done_o(0) <= fl_seen(0);
                  fl_fail_o(0) <= fl_fail;
               else
                  fl_done_o(1) <= fl_seen(1);
                  fl_fail_o(1) <= fl_fail;
               end if;
               fl_step <= J_IDLE;
         end case;

         -- A new cartridge empties the cache and maps both requests afresh
         v_init := cart_valid_i = '1' and cart_valid_d = '0';
         cart_valid_d <= cart_valid_i;
         if v_init then
            ld_stale <= '1';
            lo_bits  <= (others => (others => '0'));
            hi_bits  <= (others => (others => '0'));
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
               lo_slot                     <= v_slot;
               lo_tag(to_integer(v_slot))  <= bank_lo_i;
               lo_bits(to_integer(v_slot)) <= (others => '0');
               if cart_c128_i = '1' then
                  lo_bits(pair(v_slot)) <= (others => '0');
               end if;
               lo_next                     <= v_slot + v_step;
            end if;
            lo_seq <= (others => '0');
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
               hi_slot                     <= v_slot;
               hi_tag(to_integer(v_slot))  <= bank_hi_i;
               hi_bits(to_integer(v_slot)) <= (others => '0');
               if cart_c128_i = '1' then
                  hi_bits(pair(v_slot)) <= (others => '0');
               end if;
               hi_next                     <= v_slot + v_step;
            end if;
            hi_seq <= (others => '0');
         end if;

         if rst_i = '1' then
            avm_read_o   <= '0';
            fl_write     <= '0';
            fl_set       <= '0';
            fl_pre       <= '0';
            pre_req      <= '0';
            fl_step      <= J_IDLE;
            fl_seen      <= fl_req_i;
            fl_done_o    <= fl_req_i;
            fl_fail_o    <= "00";
            state        <= IDLE_ST;
            lo_bits      <= (others => (others => '0'));
            hi_bits      <= (others => (others => '0'));
            cart_valid_d <= '0';
         end if;
      end if;
   end process p_cacher;

end architecture synthesis;
