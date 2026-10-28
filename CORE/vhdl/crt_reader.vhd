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
-- It also carries out EasyFlash's flash jobs (crt_flash): a program reads the byte's word and
-- writes it back ANDed with the new byte, an erase fills the banks with $FF. Each bank the file
-- lacks has a spare 8 kB bank in the 1 MiB at G_SPARE_ADDRESS. They are filled with $FF once the
-- file is parsed, while the C128 waits in reset (prefill_o), so a first program into one only
-- has to map it: EAPI waits less than 0.5 ms for a byte. Each access starts early in a refresh
-- window (slot_ok_i), one per window, so it is done before the next read is due.
--
-- Everything runs on the HyperRAM clock; the caller does the clock domain crossings.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity crt_reader is
   generic (
      G_SPARE_ADDRESS     : std_logic_vector(21 downto 0) := (others => '0')  -- word address
   );
   port (
      clk_i               : in  std_logic;
      rst_i               : in  std_logic;

      -- From the parser
      cart_ready_i        : in  std_logic;                         -- the file is parsed and in use
      cart_size_i         : in  std_logic_vector(22 downto 0);     -- its length in bytes
      cart_c128_i         : in  std_logic;
      cart_ef_i           : in  std_logic := '0';                  -- an EasyFlash
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

      -- EasyFlash flash jobs from crt_flash, index 0 for ROML's chip and 1 for ROMH's. A job is
      -- new when its req toggles; done follows req once it is finished.
      slot_ok_i           : in  std_logic;                         -- no read is due for a while
      fl_req_i            : in  std_logic_vector( 1 downto 0);
      fl_lo_job_i         : in  std_logic_vector(28 downto 0);
      fl_hi_job_i         : in  std_logic_vector(28 downto 0);
      fl_done_o           : out std_logic_vector( 1 downto 0);
      fl_fail_o           : out std_logic_vector( 1 downto 0);
      prefill_o           : out std_logic;                         -- filling the spare banks

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

   type t_state is (IDLE_ST, ADDR_ST, READ_ST, FL_READ_ST, FL_WRITE_ST);
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

   -- Flash jobs: the step the current one is at, and the SDRAM accesses it makes
   type t_job is (J_IDLE, J_LOOKUP, J_PICK, J_FILL, J_SET, J_NEXT, J_READ, J_WRITE, J_DONE);
   constant C_SLOT_START : natural := 4;     -- clocks into a refresh window an access may start
   constant C_SLOT_OPEN  : natural := 63;    -- a window this long is no window: the C128 is idle
   signal fl_step      : t_job := J_IDLE;
   signal fl_seen      : std_logic_vector(1 downto 0) := "00";
   signal fl_ch        : std_logic := '0';                         -- 1: ROMH's chip
   signal fl_op        : std_logic_vector(1 downto 0) := "00";
   signal fl_bank      : unsigned(5 downto 0) := (others => '0');
   signal fl_last      : unsigned(5 downto 0) := (others => '0');  -- an erase's last bank
   signal fl_off       : unsigned(12 downto 0) := (others => '0');
   signal fl_data      : std_logic_vector(7 downto 0) := (others => '0');
   signal fl_base      : unsigned(21 downto 0) := (others => '0');
   signal fl_mask      : t_off := (others => '0');
   signal fl_pres      : std_logic := '0';
   signal fl_addr      : unsigned(21 downto 0) := (others => '0');  -- the next word
   signal fl_left      : unsigned(12 downto 0) := (others => '0');  -- words still to fill
   signal fl_set       : std_logic := '0';                          -- map fl_bank's spare
   signal fl_pre       : std_logic := '0';                          -- filling the spares
   signal pre_req      : std_logic := '0';
   signal ready_d      : std_logic := '0';
   signal fl_a0        : std_logic := '0';
   signal fl_fail      : std_logic := '0';
   signal fl_write     : std_logic := '0';
   signal fl_burst     : unsigned(3 downto 0) := (others => '0');
   signal fl_cnt       : unsigned(3 downto 0) := (others => '0');
   signal fl_word      : std_logic_vector(15 downto 0) := (others => '1');
   signal fl_be        : std_logic_vector(1 downto 0) := "11";
   signal fl_wants     : std_logic;
   signal fl_slot      : std_logic;
   signal spare_ok     : std_logic;
   signal win_age      : unsigned(5 downto 0) := (others => '0');
   signal win_used     : std_logic := '0';

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

   -- A missing bank's spare: 8 kB each, ROMH's 64 after ROML's
   function spare_addr(hi : std_logic; bank : unsigned(5 downto 0)) return unsigned is
      variable v : unsigned(6 downto 0);
   begin
      v := hi & bank;
      return unsigned(G_SPARE_ADDRESS) + shift_left(resize(v, 22), 12);
   end function spare_addr;

begin

   pending   <= '1' when cpu_req_i /= cpu_seen or vic_req_i /= vic_seen else '0';
   cart_owns <= '0' when state = IDLE_ST else '1';

   -- A waiting request holds new bulk transfers off, but never splits a write burst
   bulk_hold <= '1' when state /= IDLE_ST or (pending = '1' and bulk_wr = 0) else '0';

   m_avm_write_o       <= fl_write when cart_owns = '1' else s_avm_write_i and not bulk_hold;
   m_avm_read_o        <= cart_read when cart_owns = '1' else s_avm_read_i and not bulk_hold;
   m_avm_address_o     <= cart_address when cart_owns = '1' else s_avm_address_i;
   m_avm_burstcount_o  <= "0000" & std_logic_vector(fl_burst) when state = FL_WRITE_ST else
                          X"01" when cart_owns = '1' else s_avm_burstcount_i;
   m_avm_writedata_o   <= fl_word when cart_owns = '1' else s_avm_writedata_i;
   m_avm_byteenable_o  <= fl_be when cart_owns = '1' else s_avm_byteenable_i;
   s_avm_waitrequest_o <= m_avm_waitrequest_i or bulk_hold;
   s_avm_readdata_o    <= m_avm_readdata_i;
   s_avm_readdatavalid_o <= m_avm_readdatavalid_i and not cart_owns;

   -- A flash access may start early in a refresh window, once per window, or at any time while
   -- the window stays open (the C128 paused, or no cartridge running)
   fl_wants <= '1' when (fl_step = J_FILL and fl_left /= 0) or fl_step = J_READ or fl_step = J_WRITE else '0';
   fl_slot  <= '1' when slot_ok_i = '1' and ((win_age < C_SLOT_START and win_used = '0') or
                                             win_age = C_SLOT_OPEN) else '0';
   -- Spare banks go in the second MiB of the cartridge's region, so the file must fit the first
   spare_ok  <= '1' when unsigned(cart_size_i) <= 16#100000# else '0';
   prefill_o <= fl_pre or pre_req;

   p_tables : process (clk_i)
      variable v_bank  : natural range 0 to 127;
      variable v_size  : natural;
      variable v_spare : std_logic_vector(21 downto 0);
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
               -- ROML, and ROMH too: the second half of a 16 kB chip, else the same image. An
               -- EasyFlash's ROMH chip is separate, so there an 8 kB chip leaves ROMH alone.
               lo_addr(v_bank)    <= cart_bank_raddr_i(22 downto 1);
               lo_mask(v_bank)    <= mirror_mask(cart_bank_size_i, 16#2000#);
               lo_present(v_bank) <= '1';
               if v_size > 16#2000# then
                  hi_addr(v_bank)    <= std_logic_vector(unsigned(cart_bank_raddr_i(22 downto 1)) + 16#1000#);
                  hi_mask(v_bank)    <= mirror_mask(std_logic_vector(to_unsigned(v_size - 16#2000#, 16)), 16#2000#);
                  hi_present(v_bank) <= '1';
               elsif cart_ef_i = '0' then
                  hi_addr(v_bank)    <= cart_bank_raddr_i(22 downto 1);
                  hi_mask(v_bank)    <= mirror_mask(cart_bank_size_i, 16#2000#);
                  hi_present(v_bank) <= '1';
               end if;
            else
               hi_addr(v_bank)    <= cart_bank_raddr_i(22 downto 1);
               hi_mask(v_bank)    <= mirror_mask(cart_bank_size_i, 16#2000#);
               hi_present(v_bank) <= '1';
            end if;
         elsif fl_set = '1' and cart_loading_i = '0' then
            -- A flash program into a bank the file lacks: map its spare bank, all $FF
            v_bank  := to_integer(fl_bank);
            v_spare := std_logic_vector(spare_addr(fl_ch, fl_bank));
            if fl_ch = '0' then
               lo_addr(v_bank)    <= v_spare;
               lo_mask(v_bank)    <= to_unsigned(16#1FFF#, 14);
               lo_present(v_bank) <= '1';
            else
               hi_addr(v_bank)    <= v_spare;
               hi_mask(v_bank)    <= to_unsigned(16#1FFF#, 14);
               hi_present(v_bank) <= '1';
            end if;
         end if;
      end if;
   end process p_tables;

   p_reader : process (clk_i)
      variable v_bank : natural range 0 to 127;
      variable v_off  : t_off;
      variable v_rd   : unsigned(8 downto 0);
      variable v_room : unsigned(10 downto 0);
      variable v_n    : unsigned(3 downto 0);
      variable v_job  : std_logic_vector(28 downto 0);
      variable v_old  : std_logic_vector(7 downto 0);
      variable v_new  : std_logic_vector(7 downto 0);
   begin
      if rising_edge(clk_i) then
         if m_avm_waitrequest_i = '0' then
            cart_read <= '0';
         end if;
         fl_set  <= '0';
         ready_d <= cart_ready_i;
         if cart_ready_i = '1' and ready_d = '0' and cart_ef_i = '1' and spare_ok = '1' then
            pre_req <= '1';
         end if;

         if slot_ok_i = '0' then
            win_age  <= (others => '0');
            win_used <= '0';
         elsif win_age /= C_SLOT_OPEN then
            win_age <= win_age + 1;
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
               elsif fl_wants = '1' and fl_slot = '1' and bulk_rd = 0 and bulk_wr = 0 and
                     s_avm_read_i = '0' and s_avm_write_i = '0' then
                  -- A flash access: a word to read, or a burst of up to 8 within the SDRAM row
                  win_used     <= '1';
                  cart_address <= std_logic_vector(resize(fl_addr, 32));
                  if fl_step = J_READ then
                     cart_read <= '1';
                     state     <= FL_READ_ST;
                  else
                     v_room := to_unsigned(1024, 11) - resize(fl_addr(9 downto 0), 11);
                     v_n    := to_unsigned(8, 4);
                     if fl_step = J_WRITE then
                        v_n := to_unsigned(1, 4);
                     elsif fl_left < 8 then
                        v_n := fl_left(3 downto 0);
                     end if;
                     if v_room < v_n then
                        v_n := v_room(3 downto 0);
                     end if;
                     fl_burst <= v_n;
                     fl_cnt   <= v_n;
                     fl_write <= '1';
                     state    <= FL_WRITE_ST;
                  end if;
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

            when FL_READ_ST =>
               -- A program: the old byte ANDed with the new one, which failed if bits had to rise
               if m_avm_readdatavalid_i = '1' then
                  v_old := m_avm_readdata_i(7 downto 0);
                  if fl_a0 = '1' then
                     v_old := m_avm_readdata_i(15 downto 8);
                  end if;
                  v_new   := v_old and fl_data;
                  fl_word <= v_new & v_new;
                  fl_be   <= fl_a0 & not fl_a0;
                  if v_new /= fl_data then
                     fl_fail <= '1';
                  end if;
                  fl_step <= J_WRITE;
                  state   <= IDLE_ST;
               end if;

            when FL_WRITE_ST =>
               if m_avm_waitrequest_i = '0' then
                  fl_addr <= fl_addr + 1;
                  if fl_step = J_FILL then
                     fl_left <= fl_left - 1;
                  end if;
                  fl_cnt <= fl_cnt - 1;
                  if fl_cnt = 1 then
                     fl_write <= '0';
                     state    <= IDLE_ST;
                     if fl_step = J_WRITE then
                        fl_step <= J_DONE;
                     end if;
                  end if;
               end if;
         end case;

         -- Flash jobs, a step at a time; FL_READ_ST and FL_WRITE_ST move the SDRAM steps on
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
                  fl_mask <= lo_mask(v_bank);
                  fl_pres <= lo_present(v_bank);
               else
                  fl_base <= unsigned(hi_addr(v_bank));
                  fl_mask <= hi_mask(v_bank);
                  fl_pres <= hi_present(v_bank);
               end if;
               fl_step <= J_PICK;

            when J_PICK =>
               fl_word <= X"FFFF";
               fl_be   <= "11";
               if cart_ready_i = '0' then
                  -- Another file is on its way: drop the job
                  fl_step <= J_DONE;
               elsif fl_op = "00" then
                  if fl_pres = '0' then
                     fl_addr <= spare_addr(fl_ch, fl_bank);
                     fl_left <= to_unsigned(4096, 13);
                     fl_step <= J_FILL;
                  else
                     fl_step <= J_NEXT;
                  end if;
               elsif fl_op = "01" then
                  if fl_pres = '1' then
                     v_off   := resize(fl_off, 14) and fl_mask;
                     fl_addr <= fl_base + v_off(13 downto 1);
                     fl_a0   <= v_off(0);
                     fl_step <= J_READ;
                  elsif spare_ok = '1' then
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
                  fl_left <= resize(fl_mask(13 downto 1), 13) + 1;
                  fl_step <= J_FILL;
               else
                  -- An erase skips a bank the file lacks: it reads $FF anyway
                  fl_step <= J_NEXT;
               end if;

            when J_FILL =>
               if fl_left = 0 and state = IDLE_ST then
                  fl_step <= J_NEXT;
               end if;

            when J_SET =>
               fl_step <= J_LOOKUP;

            when J_NEXT =>
               if fl_bank = fl_last and fl_op = "00" and fl_ch = '0' and cart_ready_i = '1' then
                  fl_ch   <= '1';
                  fl_bank <= (others => '0');
                  fl_step <= J_LOOKUP;
               elsif fl_bank = fl_last or cart_ready_i = '0' then
                  fl_step <= J_DONE;
               else
                  fl_bank <= fl_bank + 1;
                  fl_step <= J_LOOKUP;
               end if;

            when J_READ | J_WRITE =>
               null;

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
            fl_step      <= J_IDLE;
            fl_seen      <= fl_req_i;
            fl_done_o    <= fl_req_i;
            fl_fail_o    <= "00";
            fl_write     <= '0';
            fl_set       <= '0';
            fl_pre       <= '0';
            pre_req      <= '0';
         end if;
      end if;
   end process p_reader;

end architecture synthesis;
