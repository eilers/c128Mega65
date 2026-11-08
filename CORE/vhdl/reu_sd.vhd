----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- reu_sd: moves blocks between the SD card and the REU's memory through the SD
-- controller's engine port (sdhost.vhd), for the REU's image files (UCI's LOAD_REU and
-- SAVE_REU, the menu's REU File), and tells the REU's cache (avm_cache in main.vhd) when
-- its memory changed behind it.
--
-- Device words, on the QNICE clock:
--   0x000  R: magic 0x5D52
--   0x001  W: start a transfer; bit 0 = 1 from the REU to the card (a save), else from
--             the card to the REU (a load)
--          R: bit 0 busy, bit 1 the last transfer failed, bit 2 the REU's cached line is
--             still to be dropped
--   0x002  W: first SD block, low word        0x003  high word
--   0x004  W: blocks (0 does nothing)
--   0x005  W: REU byte address of the first block (even), low word     0x006  high word
--   0x007  W: a load writes no REU byte this many bytes or more past the start (the end of
--   0x008     the file in its last block), low word, then high word
--   0x009  W: drop the REU's cached line (after the CPU wrote the REU's memory)
--   0x00A  W: the card the blocks were taken from (Q2C_CARD); a transfer on another card fails
--          R: the card now
--
-- A load is busy until its last write is in memory (a read behind the writes answered)
-- and the REU's cached line was dropped, so that the C128 sees the new bytes once busy is
-- low. The blocks pass through a ring buffer of G_BUF_BLOCKS blocks; the engine's limit
-- keeps it from running ahead of the memory (a load) or of the data (a save). A QNICE SD
-- command ends an engine transfer early; the rest is issued again. A memory access that
-- isn't answered in 21 ms (the memory's reset) fails the transfer, so it can't hang.
--
-- The memory side is an Avalon master with 16-bit words, little endian (REU byte 2n in bits
-- 7..0, as reu_mapper.vhd lays them out), on the QNICE clock; mega65.vhd crosses it to the
-- memory's clock. Its addresses stay inside the REU's 2^G_ROOM_BITS bytes from G_BASE.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.sdhost_pkg.all;

entity reu_sd is
generic (
   G_BASE       : std_logic_vector(31 downto 0);           -- word address of REU byte 0
   G_ROOM_BITS  : natural;                                 -- the REU's memory: 2^n bytes
   G_BUF_BLOCKS : natural := 4                             -- power of two
);
port (
   clk_i               : in  std_logic;                    -- QNICE clock
   rst_i               : in  std_logic;

   qnice_addr_i        : in  std_logic_vector(11 downto 0);
   qnice_data_i        : in  std_logic_vector(15 downto 0);
   qnice_ce_i          : in  std_logic;
   qnice_we_i          : in  std_logic;
   qnice_data_o        : out std_logic_vector(15 downto 0);

   eng_want_o          : out std_logic;
   eng_o               : out t_sd_c2q;
   eng_i               : in  t_sd_q2c;

   avm_write_o         : out std_logic;
   avm_read_o          : out std_logic;
   avm_address_o       : out std_logic_vector(31 downto 0);
   avm_writedata_o     : out std_logic_vector(15 downto 0);
   avm_byteenable_o    : out std_logic_vector( 1 downto 0);
   avm_burstcount_o    : out std_logic_vector( 7 downto 0);
   avm_readdata_i      : in  std_logic_vector(15 downto 0);
   avm_readdatavalid_i : in  std_logic;
   avm_waitrequest_i   : in  std_logic;

   main_clk_i          : in  std_logic;
   main_drop_o         : out std_logic := '0'              -- one clock: drop the cached line
);
end entity reu_sd;

architecture synthesis of reu_sd is

   function ceil_log2(n : natural) return natural is
      variable r : natural := 0;
   begin
      while 2 ** r < n loop
         r := r + 1;
      end loop;
      return r;
   end function ceil_log2;

   constant C_SLOT_BITS : natural := ceil_log2(G_BUF_BLOCKS);
   constant C_OFS_BITS  : natural := G_ROOM_BITS - 1;      -- a word's offset in the REU
   constant C_BURST     : natural := 8;                    -- words per read, as the REU's cache
   constant C_WATCHDOG  : natural := 20;                   -- 2^20 clocks: 21 ms

   type t_buf is array (0 to G_BUF_BLOCKS * 256 - 1) of std_logic_vector(15 downto 0);
   signal buf          : t_buf;
   signal buf_we       : std_logic;
   signal buf_waddr    : unsigned(C_SLOT_BITS + 7 downto 0);
   signal buf_wdata    : std_logic_vector(15 downto 0);
   signal buf_raddr    : unsigned(C_SLOT_BITS + 7 downto 0);
   signal buf_rdata    : std_logic_vector(15 downto 0);
   signal hi_sel       : std_logic := '0';                 -- the engine wants the high byte

   -- the registers, written on the falling edge
   signal r_blk        : std_logic_vector(31 downto 0) := (others => '0');
   signal r_count      : std_logic_vector(15 downto 0) := (others => '0');
   signal r_addr       : std_logic_vector(31 downto 0) := (others => '0');
   signal r_len        : std_logic_vector(31 downto 0) := (others => '0');
   signal r_card       : std_logic_vector( 7 downto 0) := (others => '0');
   signal r_save       : std_logic := '0';
   signal q_we_d       : std_logic := '0';
   signal q_start_req  : std_logic := '0';                 -- toggles
   signal q_drop_req   : std_logic := '0';                 -- toggles

   -- the transfer
   signal start_seen   : std_logic := '0';
   signal go           : std_logic := '0';                 -- the clock after the start
   signal busy         : std_logic := '0';
   signal err          : std_logic := '0';
   signal fin          : std_logic := '0';                 -- the memory side is done
   signal save         : std_logic := '0';
   signal blk          : unsigned(31 downto 0) := (others => '0');
   signal count        : unsigned(15 downto 0) := (others => '0');
   signal base_w       : unsigned(C_OFS_BITS - 1 downto 0) := (others => '0');
   signal len          : unsigned(31 downto 0) := (others => '0');
   signal card         : std_logic_vector(7 downto 0) := (others => '0');

   -- engine side
   signal eng_act      : std_logic := '0';                 -- a transfer runs
   signal eng_start    : std_logic := '0';
   signal xfer_base    : unsigned(15 downto 0) := (others => '0');   -- its first block
   signal moved        : unsigned(15 downto 0) := (others => '0');   -- blocks the engine did
   signal wr_blk       : unsigned(15 downto 0);
   signal lo_byte      : std_logic_vector(7 downto 0);
   signal e_buf_we     : std_logic := '0';
   signal e_buf_waddr  : unsigned(C_SLOT_BITS + 7 downto 0);
   signal e_buf_wdata  : std_logic_vector(15 downto 0);

   -- memory side
   type t_mstate is (M_IDLE, M_READ, M_WRITE, M_FETCH, M_FILL, M_CHECK, M_CHECK_WAIT,
                     M_DROP_REQ, M_DROP_WAIT);
   signal mstate       : t_mstate := M_IDLE;
   signal blocks       : unsigned(15 downto 0) := (others => '0');   -- a load's written, a save's read
   signal word         : unsigned(7 downto 0) := (others => '0');    -- in the block
   signal fill_word    : unsigned(7 downto 0) := (others => '0');
   signal mem_write    : std_logic := '0';
   signal mem_read     : std_logic := '0';
   signal mem_ofs      : unsigned(C_OFS_BITS - 1 downto 0) := (others => '0');
   signal mem_be       : std_logic_vector(1 downto 0) := "11";
   signal mem_burst    : unsigned(7 downto 0) := to_unsigned(1, 8);
   signal mem_err      : std_logic := '0';
   signal failed       : std_logic;                        -- err, or mem_err before it
   signal wait_cnt     : unsigned(C_WATCHDOG - 1 downto 0) := (others => '0');

   -- the REU's cache: a toggle to the main clock and back
   signal drop_seen    : std_logic := '0';
   signal drop_want    : std_logic := '0';
   signal drop_req     : std_logic := '0';
   signal drop_ack_m   : std_logic := '0';
   signal drop_ack_s   : std_logic := '0';
   signal drop_busy    : std_logic;
   signal m_req_m      : std_logic := '0';
   signal m_req_s      : std_logic := '0';
   signal m_req_d      : std_logic := '0';
   signal m_ack        : std_logic := '0';

   attribute ASYNC_REG : string;
   attribute ASYNC_REG of m_req_m, m_req_s, drop_ack_m, drop_ack_s : signal is "TRUE";

begin

   ---------------------------------------------------------------------------------------------
   -- Device bus
   ---------------------------------------------------------------------------------------------

   -- ce can be high for more than one edge, and is low between two accesses
   p_regs : process (clk_i)
   begin
      if falling_edge(clk_i) then
         q_we_d <= qnice_ce_i and qnice_we_i;
         if qnice_ce_i = '1' and qnice_we_i = '1' then
            case qnice_addr_i is
               when x"001" =>
                  if q_we_d = '0' then
                     r_save      <= qnice_data_i(0);
                     q_start_req <= not q_start_req;
                  end if;
               when x"002" => r_blk(15 downto 0)   <= qnice_data_i;
               when x"003" => r_blk(31 downto 16)  <= qnice_data_i;
               when x"004" => r_count              <= qnice_data_i;
               when x"005" => r_addr(15 downto 0)  <= qnice_data_i;
               when x"006" => r_addr(31 downto 16) <= qnice_data_i;
               when x"007" => r_len(15 downto 0)   <= qnice_data_i;
               when x"008" => r_len(31 downto 16)  <= qnice_data_i;
               when x"009" =>
                  if q_we_d = '0' then
                     q_drop_req <= not q_drop_req;
                  end if;
               when x"00A" => r_card <= qnice_data_i(7 downto 0);
               when others => null;
            end case;
         end if;
      end if;
   end process p_regs;

   -- busy and dropping from the write on, before the rising edge has seen it
   p_read : process (all)
   begin
      qnice_data_o <= (others => '0');
      case qnice_addr_i is
         when x"000" => qnice_data_o <= x"5D52";
         when x"001" =>
            qnice_data_o(0) <= busy or (q_start_req xor start_seen);
            qnice_data_o(1) <= err;
            qnice_data_o(2) <= drop_busy or (q_drop_req xor drop_seen);
         when x"00A" => qnice_data_o(7 downto 0) <= eng_i(Q2C_CARD);
         when others => null;
      end case;
   end process p_read;

   ---------------------------------------------------------------------------------------------
   -- The ring buffer: a load's engine writes it and its memory side reads it, a save's
   -- memory side writes it and its engine reads it
   ---------------------------------------------------------------------------------------------

   wr_blk    <= xfer_base + unsigned(eng_i(Q2C_WRBLK));
   buf_we    <= avm_readdatavalid_i when save = '1' and mstate = M_FILL else
                '0'                 when save = '1' else e_buf_we;
   buf_waddr <= blocks(C_SLOT_BITS - 1 downto 0) & fill_word when save = '1' else e_buf_waddr;
   buf_wdata <= avm_readdata_i when save = '1' else e_buf_wdata;
   buf_raddr <= wr_blk(C_SLOT_BITS - 1 downto 0) & unsigned(eng_i(Q2C_WRIDX)(Q2C_WRIDX'high downto Q2C_WRIDX'low + 1))
                when save = '1' else
                blocks(C_SLOT_BITS - 1 downto 0) & word;

   -- sdhost wants a save's byte one clock after its address
   p_buf : process (clk_i)
   begin
      if rising_edge(clk_i) then
         if buf_we = '1' then
            buf(to_integer(buf_waddr)) <= buf_wdata;
         end if;
         buf_rdata <= buf(to_integer(buf_raddr));
         hi_sel    <= eng_i(Q2C_WRIDX)(Q2C_WRIDX'low);
      end if;
   end process p_buf;

   ---------------------------------------------------------------------------------------------
   -- Engine side
   ---------------------------------------------------------------------------------------------

   eng_want_o <= busy;

   p_eng_o : process (all)
   begin
      eng_o             <= (others => '0');
      eng_o(C2Q_START)  <= eng_start;
      eng_o(C2Q_WRITE)  <= save;
      eng_o(C2Q_ABORT)  <= err and eng_act;                -- the card changed: stop
      eng_o(C2Q_ADDR)   <= std_logic_vector(blk + resize(xfer_base, 32));
      eng_o(C2Q_COUNT)  <= std_logic_vector(count - xfer_base);
      -- the blocks of this transfer that may be moved now
      if save = '1' then
         eng_o(C2Q_LIMIT) <= std_logic_vector(blocks - xfer_base);
      else
         eng_o(C2Q_LIMIT) <= std_logic_vector(blocks + G_BUF_BLOCKS - xfer_base);
      end if;
      if hi_sel = '1' then
         eng_o(C2Q_WRDATA) <= buf_rdata(15 downto 8);
      else
         eng_o(C2Q_WRDATA) <= buf_rdata(7 downto 0);
      end if;
   end process p_eng_o;

   p_engine : process (clk_i)
      variable b : unsigned(15 downto 0);
   begin
      if rising_edge(clk_i) then
         eng_start <= '0';
         e_buf_we  <= '0';
         go        <= '0';

         if q_start_req /= start_seen and busy = '0' then
            start_seen <= q_start_req;
            if unsigned(r_count) /= 0 then
               busy      <= '1';
               go        <= '1';
               err       <= '0';
               if eng_i(Q2C_CARD) /= r_card then
                  err    <= '1';                           -- not the card the blocks are on
               end if;
               save      <= r_save;
               blk       <= unsigned(r_blk);
               count     <= unsigned(r_count);
               base_w    <= unsigned(r_addr(C_OFS_BITS downto 1));
               len       <= unsigned(r_len);
               card      <= r_card;
               moved     <= (others => '0');
               xfer_base <= (others => '0');
            end if;
         end if;

         if busy = '1' and (eng_i(Q2C_CARD) /= card or mem_err = '1') then
            err <= '1';
         end if;

         -- (re)issue the blocks still to go, once the memory side has started
         if busy = '1' and go = '0' and err = '0' and eng_act = '0' and eng_start = '0' and
            moved /= count and eng_i(Q2C_BUSY) = '0' then
            eng_start <= '1';
            eng_act   <= '1';
            xfer_base <= moved;
         end if;

         if eng_act = '1' then
            if save = '0' and eng_i(Q2C_RDWE) = '1' then
               if eng_i(Q2C_RDIDX)(Q2C_RDIDX'low) = '0' then
                  lo_byte <= eng_i(Q2C_RDDATA);
               else
                  b := xfer_base + unsigned(eng_i(Q2C_RDBLK));
                  e_buf_we    <= '1';
                  e_buf_waddr <= b(C_SLOT_BITS - 1 downto 0) &
                                 unsigned(eng_i(Q2C_RDIDX)(Q2C_RDIDX'high downto Q2C_RDIDX'low + 1));
                  e_buf_wdata <= eng_i(Q2C_RDDATA) & lo_byte;
               end if;
            end if;
            if eng_i(Q2C_BLK) = '1' then
               moved <= moved + 1;
            end if;
            if eng_i(Q2C_DONE) = '1' then
               eng_act <= '0';
               if eng_i(Q2C_ERROR) = '1' then
                  err <= '1';
               end if;
            end if;
         end if;

         if busy = '1' and fin = '1' then
            busy <= '0';
         end if;

         if rst_i = '1' then
            start_seen <= q_start_req;
            busy       <= '0';
            err        <= '0';
            eng_act    <= '0';
         end if;
      end if;
   end process p_engine;

   ---------------------------------------------------------------------------------------------
   -- Memory side
   ---------------------------------------------------------------------------------------------

   failed <= err or mem_err;

   avm_write_o      <= mem_write;
   avm_read_o       <= mem_read;
   avm_address_o    <= std_logic_vector(unsigned(G_BASE) + resize(mem_ofs, 32));
   avm_writedata_o  <= buf_rdata;
   avm_byteenable_o <= mem_be;
   avm_burstcount_o <= std_logic_vector(mem_burst);

   p_memory : process (clk_i)
      variable ofs : unsigned(31 downto 0);
   begin
      if rising_edge(clk_i) then
         if busy = '0' then
            fin     <= '0';
            mem_err <= '0';
         end if;

         if mstate = M_IDLE or mstate = M_READ or mstate = M_DROP_REQ or mstate = M_DROP_WAIT then
            wait_cnt <= (others => '0');
         else
            wait_cnt <= wait_cnt + 1;
         end if;

         case mstate is
            when M_IDLE =>
               if go = '1' then
                  blocks <= (others => '0');
                  word   <= (others => '0');
               elsif busy = '1' and fin = '0' then
                  if save = '0' and failed = '0' and blocks /= moved then
                     mstate <= M_READ;                     -- buf_rdata holds the word next clock
                  elsif save = '0' and eng_act = '0' and eng_start = '0' and
                        (blocks = count or failed = '1') then
                     if mem_err = '1' then
                        mstate <= M_DROP_REQ;              -- no answer from the memory: no read
                     else
                        -- a read behind the writes: they are all in memory once it answers
                        mem_read  <= '1';
                        mem_burst <= to_unsigned(1, 8);
                        mem_ofs   <= base_w;
                        mstate    <= M_CHECK;
                     end if;
                  elsif save = '1' and failed = '0' and blocks /= count and
                        blocks - moved < G_BUF_BLOCKS then
                     mem_read  <= '1';
                     mem_burst <= to_unsigned(C_BURST, 8);
                     mem_ofs   <= base_w + resize(blocks & word, C_OFS_BITS);
                     fill_word <= word;
                     mstate    <= M_FETCH;
                  elsif save = '1' and eng_act = '0' and eng_start = '0' and
                        (moved = count or failed = '1') then
                     fin <= '1';
                  end if;
               end if;

            -- a load: the word, as far as the file goes
            when M_READ =>
               ofs     := resize(blocks & word & '0', 32);
               mem_ofs <= base_w + resize(blocks & word, C_OFS_BITS);
               mem_burst <= to_unsigned(1, 8);
               if ofs + 1 < len then
                  mem_be    <= "11";
                  mem_write <= '1';
               elsif ofs < len then
                  mem_be    <= "01";
                  mem_write <= '1';
               end if;
               mstate <= M_WRITE;

            when M_WRITE =>
               if mem_write = '0' or avm_waitrequest_i = '0' then
                  mem_write <= '0';
                  mem_be    <= "11";
                  word      <= word + 1;
                  if word = 255 then
                     blocks <= blocks + 1;
                  end if;
                  mstate <= M_IDLE;
               end if;

            -- a save: C_BURST words into the buffer
            when M_FETCH =>
               if avm_waitrequest_i = '0' then
                  mem_read <= '0';
                  mstate   <= M_FILL;
               end if;

            when M_FILL =>
               if avm_readdatavalid_i = '1' then
                  fill_word <= fill_word + 1;
                  if fill_word(2 downto 0) = C_BURST - 1 then
                     word <= fill_word + 1;
                     if fill_word = 255 then
                        blocks <= blocks + 1;
                     end if;
                     mstate <= M_IDLE;
                  end if;
               end if;

            when M_CHECK =>
               if avm_waitrequest_i = '0' then
                  mem_read <= '0';
                  mstate   <= M_CHECK_WAIT;
               end if;

            when M_CHECK_WAIT =>
               if avm_readdatavalid_i = '1' then
                  mstate <= M_DROP_REQ;
               end if;

            when M_DROP_REQ =>                             -- p_drop sees it this clock
               mstate <= M_DROP_WAIT;

            when M_DROP_WAIT =>
               if drop_busy = '0' then
                  fin    <= '1';
                  mstate <= M_IDLE;
               end if;
         end case;

         -- no answer from the memory: the transfer fails (a load still drops the line)
         if wait_cnt = 2 ** C_WATCHDOG - 1 then
            mem_err   <= '1';
            mem_write <= '0';
            mem_read  <= '0';
            mstate    <= M_IDLE;
         end if;

         if rst_i = '1' then
            mstate    <= M_IDLE;
            mem_write <= '0';
            mem_read  <= '0';
            fin       <= '0';
            mem_err   <= '0';
         end if;
      end if;
   end process p_memory;

   ---------------------------------------------------------------------------------------------
   -- The REU's cache: drop its line on the main clock, then hear back
   ---------------------------------------------------------------------------------------------

   drop_busy <= drop_want or (drop_req xor drop_ack_s);

   -- A request that comes while one is under way is done again after it, so a drop always
   -- comes after every request before it
   p_drop : process (clk_i)
   begin
      if rising_edge(clk_i) then
         drop_ack_m <= m_ack;
         drop_ack_s <= drop_ack_m;
         if drop_want = '1' and drop_req = drop_ack_s then
            drop_req  <= not drop_req;
            drop_want <= '0';
         end if;
         if q_drop_req /= drop_seen then
            drop_seen <= q_drop_req;
            drop_want <= '1';
         end if;
         if mstate = M_DROP_REQ then
            drop_want <= '1';
         end if;
      end if;
   end process p_drop;

   -- m_ack follows on the clock the cache drops its line
   p_main : process (main_clk_i)
   begin
      if rising_edge(main_clk_i) then
         m_req_m     <= drop_req;
         m_req_s     <= m_req_m;
         m_req_d     <= m_req_s;
         main_drop_o <= m_req_s xor m_req_d;
         m_ack       <= m_req_d;
      end if;
   end process p_main;

end architecture synthesis;
