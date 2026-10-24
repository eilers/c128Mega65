----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- vd_sd: the virtual drives' disk images straight from the SD card, without a copy
-- in memory.
--
-- The drives (iec_drive, MiSTer's "SD" interface) ask for 256-byte blocks of their
-- image. At mount the firmware writes a table with the first SD block of every 4 KiB
-- piece of the image file, which lies in one place on the card (CSR window: see below),
-- so a request turns into engine transfers of the SD controller (sdhost.vhd) without any
-- file system work.
--
--   * Reads go straight into the drive's buffer. Consecutive blocks of the file that are
--     consecutive on the card move in one transfer.
--   * Writes go into a write-back cache of C_LINES blocks and are acknowledged at once.
--     A 1541/1571 cannot go on before its track is saved and loads the next track right
--     away, and an SD card can be busy for 100 ms and more after a write: so the writes
--     reach the card later, G_FLUSH_DELAY clocks after the last one, when the cache runs
--     full, or when the firmware asks. A block that a write covers only in part is read
--     from the card first. Reads take a block from the cache when it is there.
--   * The engine port tells which card is in use (Q2C_CARD). A drive whose card changed
--     since the mount is switched off: it never reads or writes another card.
--
-- QNICE, one device per drive (qnice_drive_i):
--   window 0x0000  W: block table, word 2n / 2n+1 = SD block of image block 8n (lo/hi)
--   window 0xFFFF  CSR:
--     0x030  R: 0x5D4D (the Shell maps disk images instead of loading them)
--     0x031  W: 1 = on (mount, after the table), 0 = off (writes back, then forgets)
--            R: bit 0 on, 1 dirty, 2 busy (off or write-back pending), 3 fault (a
--               write failed and was dropped), 4 lost (the card changed)
--     0x032  W: size of the image in 512-byte blocks
--     0x033  W: write back now
--
-- QNICE clock. Done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.vdrives_pkg.all;
use work.sdhost_pkg.all;

entity vd_sd is
generic (
   G_VDNUM       : natural := 2;                    -- 1 or 2 drives
   G_FLUSH_DELAY : natural := 1_000_000             -- clocks after the last write (20 ms)
);
port (
   clk_i          : in  std_logic;                  -- QNICE clock
   rst_i          : in  std_logic;

   qnice_drive_i  : in  natural range 0 to G_VDNUM - 1;
   qnice_addr_i   : in  std_logic_vector(27 downto 0);
   qnice_data_i   : in  std_logic_vector(15 downto 0);
   qnice_ce_i     : in  std_logic;
   qnice_we_i     : in  std_logic;
   qnice_data_o   : out std_logic_vector(15 downto 0);

   -- the drives: 256-byte blocks, MiSTer's protocol (see vdrives.vhd)
   sd_lba_i       : in  vd_vec_array(G_VDNUM - 1 downto 0)(31 downto 0);
   sd_blk_cnt_i   : in  vd_vec_array(G_VDNUM - 1 downto 0)(5 downto 0);
   sd_rd_i        : in  vd_std_array(G_VDNUM - 1 downto 0);
   sd_wr_i        : in  vd_std_array(G_VDNUM - 1 downto 0);
   sd_ack_o       : out vd_std_array(G_VDNUM - 1 downto 0);
   sd_buff_addr_o : out std_logic_vector(13 downto 0);
   sd_buff_dout_o : out std_logic_vector(7 downto 0);
   sd_buff_wr_o   : out std_logic;
   sd_buff_din_i  : in  vd_vec_array(G_VDNUM - 1 downto 0)(7 downto 0);  -- one clock after the address

   dirty_o        : out std_logic_vector(G_VDNUM - 1 downto 0);         -- writes not on the card yet

   eng_want_o     : out std_logic;
   eng_o          : out t_sd_c2q;
   eng_i          : in  t_sd_q2c
);
end entity vd_sd;

architecture synthesis of vd_sd is

   constant C_LINE_W  : natural := 4;               -- write-back cache: 16 blocks
   constant C_LINES   : natural := 2 ** C_LINE_W;   -- (a track save is up to 12)
   constant C_RUN_MAX : natural := 16;              -- blocks per write-back transfer
   constant C_TBL     : natural := 256;             -- 4 KiB pieces per image (a D81 has 200)
   constant C_RETRIES : natural := 3;               -- failed write-backs before a drop

   ------------------------------------------------------------------------------------
   -- memories
   ------------------------------------------------------------------------------------

   -- the block table of both drives; QNICE writes the low word first
   type t_tbl is array (0 to 2 * C_TBL - 1) of std_logic_vector(31 downto 0);
   signal tbl            : t_tbl;
   signal tbl_wlo        : std_logic_vector(15 downto 0) := (others => '0');
   signal tbl_raddr      : unsigned(8 downto 0) := (others => '0');
   signal tbl_rdata      : unsigned(31 downto 0);

   -- the cache: one write port, one read port, both used by one phase at a time
   type t_cache is array (0 to C_LINES * 512 - 1) of std_logic_vector(7 downto 0);
   signal cache          : t_cache;
   signal c_we           : std_logic := '0';
   signal c_waddr        : unsigned(C_LINE_W + 8 downto 0) := (others => '0');
   signal c_wdata        : std_logic_vector(7 downto 0) := (others => '0');
   signal c_raddr        : unsigned(C_LINE_W + 8 downto 0);
   signal c_rdata        : std_logic_vector(7 downto 0);

   ------------------------------------------------------------------------------------
   -- cache lines
   ------------------------------------------------------------------------------------

   type t_blk_arr is array (0 to C_LINES - 1) of unsigned(10 downto 0);
   type t_lba_arr is array (0 to C_LINES - 1) of unsigned(31 downto 0);
   type t_drv_arr is array (0 to C_LINES - 1) of natural range 0 to G_VDNUM - 1;
   signal l_valid        : std_logic_vector(C_LINES - 1 downto 0) := (others => '0');
   signal l_dirty        : std_logic_vector(C_LINES - 1 downto 0) := (others => '0');
   signal l_drv          : t_drv_arr := (others => 0);
   signal l_blk          : t_blk_arr := (others => (others => '0'));
   signal l_lba          : t_lba_arr := (others => (others => '0'));

   ------------------------------------------------------------------------------------
   -- drives
   ------------------------------------------------------------------------------------

   type t_card_arr is array (0 to G_VDNUM - 1) of std_logic_vector(7 downto 0);
   type t_nblk_arr is array (0 to G_VDNUM - 1) of unsigned(11 downto 0);
   signal d_on           : std_logic_vector(G_VDNUM - 1 downto 0) := (others => '0');
   signal d_card         : t_card_arr := (others => (others => '0'));
   signal d_nblk         : t_nblk_arr := (others => (others => '0'));
   signal d_fault        : std_logic_vector(G_VDNUM - 1 downto 0) := (others => '0');
   signal d_lost         : std_logic_vector(G_VDNUM - 1 downto 0) := (others => '0');
   signal d_dirty        : std_logic_vector(G_VDNUM - 1 downto 0);

   -- commands from QNICE: toggles written on the falling edge, seen on the rising edge
   signal q_on_req, q_off_req, q_fl_req     : std_logic_vector(G_VDNUM - 1 downto 0) := (others => '0');
   signal on_seen, off_seen, fl_seen        : std_logic_vector(G_VDNUM - 1 downto 0) := (others => '0');
   signal off_pend, fl_pend                 : std_logic_vector(G_VDNUM - 1 downto 0) := (others => '0');

   ------------------------------------------------------------------------------------
   -- the state machine
   ------------------------------------------------------------------------------------

   type t_state is (
      S_IDLE,
      S_BLK,                                        -- next block of a request
      S_TBL, S_TBL_RD,                              -- its SD block from the table
      S_RUN, S_RUN_RD,                              -- longer read run: next block's SD block
      S_RD_ENG,                                     -- read run: card -> drive buffer
      S_RD_COPY,                                    -- cache -> drive buffer
      S_ZERO,                                       -- beyond the image: zeros
      S_WR_ALLOC,                                   -- write: find a line
      S_WR_FILL,                                    -- partly written block: card -> line
      S_WR_COPY,                                    -- drive buffer -> line
      S_DONE,
      S_FL_PICK, S_FL_BACK, S_FL_FWD, S_FL_ENG,      -- write-back of one run of lines
      S_FL_DONE
   );
   signal state          : t_state := S_IDLE;
   signal fl_ret         : t_state := S_IDLE;       -- where a write-back returns to
   signal fl_only        : std_logic := '0';        -- write back only drive fl_drv
   signal fl_drv         : natural range 0 to G_VDNUM - 1 := 0;
   signal rr             : natural range 0 to G_VDNUM - 1 := 0;    -- round robin

   -- the request
   signal r_drv          : natural range 0 to G_VDNUM - 1 := 0;
   signal r_wr           : std_logic := '0';
   signal r_f0           : unsigned(19 downto 0) := (others => '0');  -- first byte in the image
   signal r_end          : unsigned(20 downto 0) := (others => '0');  -- one byte past the last
   signal r_cur          : unsigned(11 downto 0) := (others => '0');  -- block being worked on
   signal r_last         : unsigned(11 downto 0) := (others => '0');
   signal ack            : std_logic_vector(G_VDNUM - 1 downto 0) := (others => '0');

   -- the part of block r_cur that the request covers: bytes lo .. hi - 1
   signal b_lo, b_hi     : unsigned(9 downto 0) := (others => '0');
   signal b_line         : natural range 0 to C_LINES - 1 := 0;
   signal i              : unsigned(9 downto 0) := (others => '0');

   -- drive buffer port (registered) and the pipelines towards it and from it
   signal buf_addr       : unsigned(13 downto 0) := (others => '0');
   signal buf_dout       : std_logic_vector(7 downto 0) := (others => '0');
   signal buf_wr         : std_logic := '0';
   signal p1_valid       : std_logic := '0';
   signal p1_ofs         : unsigned(13 downto 0) := (others => '0');
   signal p1_i           : unsigned(8 downto 0) := (others => '0');
   signal p2_valid       : std_logic := '0';
   signal p2_i           : unsigned(8 downto 0) := (others => '0');

   -- engine
   signal eng_start      : std_logic := '0';
   signal eng_act        : std_logic := '0';
   signal eng_wr         : std_logic := '0';
   signal run_blk        : unsigned(11 downto 0) := (others => '0');  -- first image block
   signal run_lba        : unsigned(31 downto 0) := (others => '0');  -- its SD block
   signal w_lba          : unsigned(31 downto 0) := (others => '0');  -- SD block of a written block
   signal run_n          : unsigned(4 downto 0) := (others => '0');   -- blocks in the run
   signal run_done       : unsigned(4 downto 0) := (others => '0');   -- blocks moved so far
   signal xbase          : unsigned(4 downto 0) := (others => '0');   -- first block of a transfer
   signal eng_err        : std_logic := '0';
   signal fill_line      : std_logic := '0';                          -- a read into b_line
   type t_wr_lines is array (0 to C_RUN_MAX - 1) of natural range 0 to C_LINES - 1;
   signal wr_lines       : t_wr_lines := (others => 0);
   signal retries        : natural range 0 to C_RETRIES := 0;
   signal card           : std_logic_vector(7 downto 0);

   signal quiet          : natural range 0 to G_FLUSH_DELAY := 0;

   -- a valid line that holds image block blk of drive drv
   procedure find(signal valid : in std_logic_vector; signal drvs : in t_drv_arr;
                  signal blks : in t_blk_arr; drv : natural; blk : unsigned(11 downto 0);
                  found : out boolean; idx : out natural) is
   begin
      found := false;
      idx   := 0;
      for n in C_LINES - 1 downto 0 loop
         if valid(n) = '1' and drvs(n) = drv and blks(n) = blk(10 downto 0) and blk(11) = '0' then
            found := true;
            idx   := n;
         end if;
      end loop;
   end procedure find;

   -- the table entry of the piece that holds image block blk of drive drv
   function piece(drv : natural; blk : unsigned(11 downto 0)) return unsigned is
   begin
      return to_unsigned(drv, 1) & blk(10 downto 3);
   end function piece;

begin

   card    <= eng_i(Q2C_CARD);
   dirty_o <= d_dirty;

   sd_ack_o       <= vd_std_array(ack);
   sd_buff_addr_o <= std_logic_vector(buf_addr);
   sd_buff_dout_o <= buf_dout;
   sd_buff_wr_o   <= buf_wr;

   g_dirty : for d in 0 to G_VDNUM - 1 generate
      p_dirty : process (all)
         variable v : std_logic;
      begin
         v := '0';
         for n in 0 to C_LINES - 1 loop
            if l_dirty(n) = '1' and l_drv(n) = d then
               v := '1';
            end if;
         end loop;
         d_dirty(d) <= v;
      end process p_dirty;
   end generate g_dirty;

   ------------------------------------------------------------------------------------
   -- memories
   ------------------------------------------------------------------------------------

   p_tbl : process (clk_i)
      variable a : natural;
   begin
      if falling_edge(clk_i) then
         if qnice_ce_i = '1' and qnice_we_i = '1' and qnice_addr_i(27 downto 12) = x"0000" then
            a := qnice_drive_i * C_TBL + to_integer(unsigned(qnice_addr_i(8 downto 1)));
            if qnice_addr_i(0) = '0' then
               tbl_wlo <= qnice_data_i;
            else
               tbl(a)  <= qnice_data_i & tbl_wlo;
            end if;
         end if;
      end if;
   end process p_tbl;

   p_tbl_rd : process (clk_i)
   begin
      if rising_edge(clk_i) then
         tbl_rdata <= unsigned(tbl(to_integer(tbl_raddr)));
      end if;
   end process p_tbl_rd;

   -- write-back reads the line that the engine's block maps to (one clock, as sdhost wants)
   c_raddr <= to_unsigned(wr_lines(to_integer(xbase(3 downto 0) + resize(unsigned(eng_i(Q2C_WRBLK)), 4))), C_LINE_W) &
              unsigned(eng_i(Q2C_WRIDX))
              when state = S_FL_ENG else
              to_unsigned(b_line, C_LINE_W) & i(8 downto 0);

   p_cache : process (clk_i)
   begin
      if rising_edge(clk_i) then
         if c_we = '1' then
            cache(to_integer(c_waddr)) <= c_wdata;
         end if;
         c_rdata <= cache(to_integer(c_raddr));
      end if;
   end process p_cache;

   ------------------------------------------------------------------------------------
   -- QNICE
   ------------------------------------------------------------------------------------

   p_qnice_wr : process (clk_i)
   begin
      if falling_edge(clk_i) then
         if qnice_ce_i = '1' and qnice_we_i = '1' and qnice_addr_i(27 downto 12) = x"FFFF" then
            case qnice_addr_i(11 downto 0) is
               when x"031" =>
                  if qnice_data_i(0) = '1' then
                     q_on_req(qnice_drive_i) <= not q_on_req(qnice_drive_i);
                  else
                     q_off_req(qnice_drive_i) <= not q_off_req(qnice_drive_i);
                  end if;
               when x"032" =>
                  d_nblk(qnice_drive_i) <= unsigned(qnice_data_i(11 downto 0));
               when x"033" =>
                  q_fl_req(qnice_drive_i) <= not q_fl_req(qnice_drive_i);
               when others => null;
            end case;
         end if;
      end if;
   end process p_qnice_wr;

   p_qnice_rd : process (all)
      variable d : natural range 0 to G_VDNUM - 1;
   begin
      d := qnice_drive_i;
      qnice_data_o <= (others => '0');
      if qnice_ce_i = '1' and qnice_we_i = '0' and qnice_addr_i(27 downto 12) = x"FFFF" then
         case qnice_addr_i(11 downto 0) is
            when x"030" => qnice_data_o <= x"5D4D";
            when x"031" =>
               qnice_data_o(0) <= d_on(d);
               qnice_data_o(1) <= d_dirty(d);
               -- busy from the command on, before the state machine has seen it
               qnice_data_o(2) <= off_pend(d) or fl_pend(d) or
                                  (q_on_req(d) xor on_seen(d)) or
                                  (q_off_req(d) xor off_seen(d)) or
                                  (q_fl_req(d) xor fl_seen(d));
               qnice_data_o(3) <= d_fault(d);
               qnice_data_o(4) <= d_lost(d);
            when others => null;
         end case;
      end if;
   end process p_qnice_rd;

   ------------------------------------------------------------------------------------
   -- engine port
   ------------------------------------------------------------------------------------

   eng_want_o <= '0' when state = S_IDLE else '1';

   p_eng_o : process (all)
   begin
      eng_o             <= (others => '0');
      eng_o(C2Q_START)  <= eng_start;
      eng_o(C2Q_WRITE)  <= eng_wr;
      eng_o(C2Q_ADDR)   <= std_logic_vector(run_lba + resize(run_done, 32));
      eng_o(C2Q_COUNT)  <= std_logic_vector(resize(run_n - run_done, 16));
      eng_o(C2Q_LIMIT)  <= x"FFFF";                 -- the destinations never fill up
      eng_o(C2Q_WRDATA) <= c_rdata;
   end process p_eng_o;

   ------------------------------------------------------------------------------------
   -- the state machine
   ------------------------------------------------------------------------------------

   p_fsm : process (clk_i)
      variable found  : boolean;
      variable idx    : natural range 0 to C_LINES - 1;
      variable base   : unsigned(20 downto 0);
      variable lo, hi : unsigned(20 downto 0);
      variable ofs    : unsigned(20 downto 0);
      variable k      : unsigned(4 downto 0);
      variable d      : natural range 0 to G_VDNUM - 1;
      variable sel    : integer;
      variable blk    : unsigned(11 downto 0);
   begin
      if rising_edge(clk_i) then
         eng_start <= '0';
         buf_wr    <= '0';
         c_we      <= '0';
         p1_valid  <= '0';
         p2_valid  <= p1_valid;
         p2_i      <= p1_i;

         if quiet /= G_FLUSH_DELAY then
            quiet <= quiet + 1;
         end if;

         -- QNICE commands
         for n in 0 to G_VDNUM - 1 loop
            if q_off_req(n) /= off_seen(n) then
               off_seen(n) <= q_off_req(n);
               off_pend(n) <= '1';
            end if;
            if q_fl_req(n) /= fl_seen(n) then
               fl_seen(n) <= q_fl_req(n);
               fl_pend(n)  <= '1';
            end if;
         end loop;

         -- engine: strobes of the running transfer
         if eng_act = '1' then
            if eng_i(Q2C_BLK) = '1' then
               run_done <= run_done + 1;
            end if;
            if eng_i(Q2C_DONE) = '1' then
               eng_act <= '0';
               if eng_i(Q2C_ERROR) = '1' then
                  eng_err <= '1';
               end if;
            end if;
            -- read data: into the drive buffer or into a cache line
            if eng_i(Q2C_RDWE) = '1' then
               if fill_line = '1' then
                  c_we    <= '1';
                  c_waddr <= to_unsigned(b_line, C_LINE_W) & unsigned(eng_i(Q2C_RDIDX));
                  c_wdata <= eng_i(Q2C_RDDATA);
               else
                  k   := xbase + resize(unsigned(eng_i(Q2C_RDBLK)), 5);
                  ofs := resize((run_blk + k) & unsigned(eng_i(Q2C_RDIDX)), 21);
                  if ofs >= r_f0 and ofs < r_end then
                     buf_wr   <= '1';
                     buf_addr <= resize(ofs - r_f0, 14);
                     buf_dout <= eng_i(Q2C_RDDATA);
                  end if;
               end if;
            end if;
         end if;

         case state is

            ------------------------------------------------------------------------
            -- idle: card check, requests, commands, write-back
            ------------------------------------------------------------------------
            when S_IDLE =>
               sel := -1;
               for n in 0 to G_VDNUM - 1 loop
                  -- a drive whose card changed forgets everything: never another card
                  if d_on(n) = '1' and d_card(n) /= card then
                     d_on(n)   <= '0';
                     d_lost(n) <= '1';
                     for m in 0 to C_LINES - 1 loop
                        if l_drv(m) = n then
                           l_valid(m) <= '0';
                           l_dirty(m) <= '0';
                        end if;
                     end loop;
                  end if;
                  if q_on_req(n) /= on_seen(n) then
                     on_seen(n) <= q_on_req(n);
                     d_on(n)    <= '1';
                     d_card(n)  <= card;
                     d_fault(n) <= '0';
                     d_lost(n)  <= '0';
                  end if;
               end loop;

               -- the next drive request, round robin
               for n in 0 to G_VDNUM - 1 loop
                  d := (rr + n) mod G_VDNUM;
                  if sel < 0 and (sd_rd_i(d) = '1' or sd_wr_i(d) = '1') then
                     sel := d;
                  end if;
               end loop;

               if sel >= 0 then
                  d          := sel;
                  rr         <= (sel + 1) mod G_VDNUM;
                  r_drv      <= d;
                  r_wr       <= sd_wr_i(d) and not sd_rd_i(d);
                  ack(d)     <= '1';
                  base       := resize(unsigned(sd_lba_i(d)(11 downto 0)) & x"00", 21);
                  r_f0       <= base(19 downto 0);
                  ofs        := base + resize((resize(unsigned(sd_blk_cnt_i(d)), 7) + 1) & x"00", 21);
                  r_end      <= ofs;
                  r_cur      <= base(20 downto 9);
                  r_last     <= resize(shift_right(ofs - 1, 9), 12);
                  state      <= S_BLK;
               else
                  for n in G_VDNUM - 1 downto 0 loop
                     if off_pend(n) = '1' or fl_pend(n) = '1' then
                        sel := n;
                     end if;
                  end loop;
                  if sel >= 0 then
                     -- write back one drive (for off: then forget it)
                     fl_only <= '1';
                     fl_drv  <= sel;
                     fl_ret  <= S_IDLE;
                     state   <= S_FL_PICK;
                  elsif quiet = G_FLUSH_DELAY and unsigned(l_dirty) /= 0 then
                     fl_only <= '0';
                     fl_ret  <= S_IDLE;
                     state   <= S_FL_PICK;
                  end if;
               end if;

            ------------------------------------------------------------------------
            -- a request, block by block
            ------------------------------------------------------------------------
            when S_BLK =>
               if r_cur > r_last then
                  state <= S_DONE;
               else
                  base := resize(r_cur & "000000000", 21);
                  lo   := base;
                  if r_f0 > base then
                     lo := resize(r_f0, 21);
                  end if;
                  hi   := base + 512;
                  if r_end < hi then
                     hi := r_end;
                  end if;
                  b_lo <= resize(lo - base, 10);
                  b_hi <= resize(hi - base, 10);
                  i    <= resize(lo - base, 10);
                  find(l_valid, l_drv, l_blk, r_drv, r_cur, found, idx);
                  b_line <= idx;

                  if d_on(r_drv) = '0' or r_cur >= d_nblk(r_drv) then
                     if r_wr = '1' then
                        r_cur <= r_cur + 1;          -- nowhere to write: drop
                     else
                        state <= S_ZERO;
                     end if;
                  elsif found then
                     if r_wr = '1' then
                        state <= S_WR_COPY;
                     else
                        state <= S_RD_COPY;
                     end if;
                  else
                     tbl_raddr <= piece(r_drv, r_cur);
                     state     <= S_TBL;
                  end if;
               end if;

            when S_TBL =>                            -- table address in, data next clock
               state <= S_TBL_RD;

            when S_TBL_RD =>
               run_blk  <= r_cur;
               run_lba  <= tbl_rdata + r_cur(2 downto 0);
               w_lba    <= tbl_rdata + r_cur(2 downto 0);
               run_n    <= to_unsigned(1, 5);
               run_done <= (others => '0');
               if r_wr = '1' then
                  state <= S_WR_ALLOC;
               else
                  -- try to make the read run longer
                  tbl_raddr <= piece(r_drv, r_cur + 1);
                  state     <= S_RUN;
               end if;

            -- the next block joins the run if the request wants it, it is not in the
            -- cache and it follows on the card (always within a piece)
            when S_RUN =>
               state <= S_RUN_RD;

            when S_RUN_RD =>
               blk := run_blk + run_n;
               find(l_valid, l_drv, l_blk, r_drv, blk, found, idx);
               if blk <= r_last and blk < d_nblk(r_drv) and not found and run_n /= C_RUN_MAX and
                  (blk(2 downto 0) /= "000" or tbl_rdata = run_lba + run_n) then
                  run_n     <= run_n + 1;
                  tbl_raddr <= piece(r_drv, blk + 1);
                  state     <= S_RUN;
               else
                  xbase     <= (others => '0');
                  fill_line <= '0';
                  eng_err   <= '0';
                  state     <= S_RD_ENG;
               end if;

            when S_RD_ENG =>
               if eng_act = '0' and eng_start = '0' then
                  if run_done = run_n or eng_err = '1' then
                     -- a failed read leaves the drive with what came (DOS sees an error)
                     r_cur <= run_blk + run_n;
                     state <= S_BLK;
                  elsif d_card(r_drv) /= card then
                     eng_err <= '1';                  -- another card: never touch it
                  elsif eng_i(Q2C_BUSY) = '0' then
                     eng_start <= '1';                -- (again, after a QNICE command)
                     eng_act   <= '1';
                     eng_wr    <= '0';
                     xbase     <= run_done;
                  end if;
               end if;

            -- cache -> drive buffer: address now, data next clock
            when S_RD_COPY =>
               if i < b_hi then
                  p1_valid <= '1';
                  p1_ofs   <= resize(resize(r_cur & i(8 downto 0), 21) - r_f0, 14);
                  i        <= i + 1;
               end if;
               if p1_valid = '1' then
                  buf_wr   <= '1';
                  buf_addr <= p1_ofs;
                  buf_dout <= c_rdata;
               end if;
               if i >= b_hi and p1_valid = '0' then
                  r_cur <= r_cur + 1;
                  state <= S_BLK;
               end if;

            when S_ZERO =>
               if i < b_hi then
                  buf_wr   <= '1';
                  buf_addr <= resize(resize(r_cur & i(8 downto 0), 21) - r_f0, 14);
                  buf_dout <= (others => '0');
                  i        <= i + 1;
               else
                  r_cur <= r_cur + 1;
                  state <= S_BLK;
               end if;

            ------------------------------------------------------------------------
            -- write: a line for the block, its old content if needed, the new bytes
            ------------------------------------------------------------------------
            when S_WR_ALLOC =>
               sel := -1;
               for n in C_LINES - 1 downto 0 loop     -- a free line ...
                  if l_valid(n) = '0' then
                     sel := n;
                  end if;
               end loop;
               if sel < 0 then
                  for n in C_LINES - 1 downto 0 loop  -- ... or one without changes
                     if l_dirty(n) = '0' then
                        sel := n;
                     end if;
                  end loop;
               end if;
               if sel < 0 then
                  -- all lines hold changes: write some back first
                  fl_only <= '0';
                  fl_ret  <= S_WR_ALLOC;
                  state   <= S_FL_PICK;
               else
                  b_line       <= sel;
                  l_valid(sel) <= '1';
                  l_dirty(sel) <= '0';
                  l_drv(sel)   <= r_drv;
                  l_blk(sel)   <= r_cur(10 downto 0);
                  l_lba(sel)   <= w_lba;
                  if b_lo /= 0 or b_hi /= 512 then
                     run_lba   <= w_lba;
                     run_n     <= to_unsigned(1, 5);
                     run_done  <= (others => '0');
                     fill_line <= '1';
                     eng_err   <= '0';
                     xbase     <= (others => '0');
                     state     <= S_WR_FILL;
                  else
                     state <= S_WR_COPY;
                  end if;
               end if;

            when S_WR_FILL =>
               if eng_act = '0' and eng_start = '0' then
                  if run_done = run_n or eng_err = '1' then
                     fill_line <= '0';
                     state     <= S_WR_COPY;
                  elsif d_card(r_drv) /= card then
                     eng_err <= '1';
                  elsif eng_i(Q2C_BUSY) = '0' then
                     eng_start <= '1';
                     eng_act   <= '1';
                     eng_wr    <= '0';
                  end if;
               end if;

            -- drive buffer -> line: address now, the drive's data one clock later
            when S_WR_COPY =>
               if i < b_hi then
                  buf_addr <= resize(resize(r_cur & i(8 downto 0), 21) - r_f0, 14);
                  p1_valid <= '1';
                  p1_i     <= i(8 downto 0);
                  i        <= i + 1;
               end if;
               if p2_valid = '1' then
                  c_we    <= '1';
                  c_waddr <= to_unsigned(b_line, C_LINE_W) & p2_i;
                  c_wdata <= sd_buff_din_i(r_drv);
               end if;
               if i >= b_hi and p1_valid = '0' and p2_valid = '0' then
                  l_dirty(b_line) <= '1';
                  quiet           <= 0;
                  r_cur           <= r_cur + 1;
                  state           <= S_BLK;
               end if;

            when S_DONE =>
               ack(r_drv) <= '0';
               state      <= S_IDLE;

            ------------------------------------------------------------------------
            -- write-back: the run of consecutive blocks around a changed line
            ------------------------------------------------------------------------
            when S_FL_PICK =>
               sel := -1;
               for n in C_LINES - 1 downto 0 loop
                  if l_dirty(n) = '1' and (fl_only = '0' or l_drv(n) = fl_drv) then
                     sel := n;
                  end if;
               end loop;
               if sel < 0 then
                  -- nothing (more) to write back
                  if fl_only = '1' then
                     fl_pend(fl_drv) <= '0';
                     if off_pend(fl_drv) = '1' then
                        off_pend(fl_drv) <= '0';
                        d_on(fl_drv)     <= '0';
                        for m in 0 to C_LINES - 1 loop
                           if l_drv(m) = fl_drv then
                              l_valid(m) <= '0';
                           end if;
                        end loop;
                     end if;
                  end if;
                  state <= fl_ret;
               else
                  wr_lines(0) <= sel;
                  fl_drv      <= l_drv(sel);
                  run_blk     <= '0' & l_blk(sel);
                  run_lba     <= l_lba(sel);
                  run_n       <= to_unsigned(1, 5);
                  state       <= S_FL_BACK;
               end if;

            -- move the start back while the line before is changed and follows on the card
            when S_FL_BACK =>
               find(l_dirty, l_drv, l_blk, fl_drv, run_blk - 1, found, idx);
               if run_blk /= 0 and found and l_lba(idx) = run_lba - 1 and run_n /= C_RUN_MAX then
                  wr_lines(0) <= idx;
                  run_blk     <= run_blk - 1;
                  run_lba     <= run_lba - 1;
                  run_n       <= run_n + 1;           -- counts the steps back
               else
                  run_n <= to_unsigned(1, 5);
                  state <= S_FL_FWD;
               end if;

            -- collect the lines from there on
            when S_FL_FWD =>
               find(l_dirty, l_drv, l_blk, fl_drv, run_blk + run_n, found, idx);
               if found and l_lba(idx) = run_lba + run_n and run_n /= C_RUN_MAX then
                  wr_lines(to_integer(run_n(3 downto 0))) <= idx;
                  run_n <= run_n + 1;
               else
                  run_done <= (others => '0');
                  eng_err  <= '0';
                  retries  <= 0;
                  state    <= S_FL_ENG;
               end if;

            when S_FL_ENG =>
               if eng_act = '0' and eng_start = '0' then
                  if run_done = run_n or d_card(fl_drv) /= card then
                     state <= S_FL_DONE;              -- (another card: the lines go at idle)
                  elsif eng_err = '1' then
                     -- try again a few times, then drop the run (and say so)
                     eng_err <= '0';
                     if retries = C_RETRIES then
                        d_fault(fl_drv) <= '1';
                        state           <= S_FL_DONE;
                     else
                        retries <= retries + 1;
                     end if;
                  elsif eng_i(Q2C_BUSY) = '0' then
                     eng_start <= '1';
                     eng_act   <= '1';
                     eng_wr    <= '1';
                     xbase     <= run_done;
                  end if;
               end if;

            when S_FL_DONE =>
               for n in 0 to C_RUN_MAX - 1 loop
                  if n < run_n then
                     l_dirty(wr_lines(n)) <= '0';
                  end if;
               end loop;
               eng_wr <= '0';
               if fl_only = '1' then
                  state <= S_FL_PICK;
               else
                  state <= fl_ret;                    -- requests go first between runs
               end if;

         end case;

         if rst_i = '1' then
            state     <= S_IDLE;
            ack       <= (others => '0');
            eng_act   <= '0';
            d_on      <= (others => '0');
            l_valid   <= (others => '0');
            l_dirty   <= (others => '0');
            off_pend  <= (others => '0');
            fl_pend   <= (others => '0');
            on_seen   <= q_on_req;
            off_seen  <= q_off_req;
            fl_seen   <= q_fl_req;
         end if;
      end if;
   end process p_fsm;

end architecture synthesis;
