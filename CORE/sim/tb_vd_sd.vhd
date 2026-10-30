----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- tb_vd_sd: vd_sd.vhd with a model of sdhost's engine port in front of a small SD card,
-- the drives' buffers (written while ack is high, read one clock after the address) and
-- QNICE writes for the block table and the commands.
--
-- The image (64 blocks) lies on the card in three parts, so runs end where the file
-- jumps; the table has one entry per 4 KiB (8 blocks). Checks: track reads that start in the middle of a block, reads across a jump,
-- 1581-style 512-byte reads, a track write (the blocks at its ends are read first and
-- merged), reading it back from the cache, the write-back after the quiet time, a full
-- cache that has to write back to take more, a drive switched off (its changes reach the
-- card first, then it reads zeros), a changed card (nothing reaches it), and the drives'
-- disk_ready in every drive's CSR.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

library work;
use work.vdrives_pkg.all;
use work.sdhost_pkg.all;

entity tb_vd_sd is
end entity tb_vd_sd;

architecture sim of tb_vd_sd is

   constant C_CLK      : time := 20 ns;
   constant C_CARD_BLK : natural := 512;           -- blocks on the model card
   constant C_IMG_BLK  : natural := 64;            -- blocks of the image
   constant C_FLUSH    : natural := 2000;          -- write-back delay in clocks (short)

   signal clk          : std_logic := '0';
   signal rst          : std_logic := '1';
   signal running      : boolean := true;

   -- QNICE
   signal q_drive      : natural range 0 to 1 := 0;
   signal q_addr       : std_logic_vector(27 downto 0) := (others => '0');
   signal q_wdata      : std_logic_vector(15 downto 0) := (others => '0');
   signal q_ce, q_we   : std_logic := '0';
   signal q_rdata      : std_logic_vector(15 downto 0);

   -- drives
   signal sd_lba       : vd_vec_array(1 downto 0)(31 downto 0) := (others => (others => '0'));
   signal sd_blk_cnt   : vd_vec_array(1 downto 0)(5 downto 0) := (others => (others => '0'));
   signal sd_rd, sd_wr : vd_std_array(1 downto 0) := (others => '0');
   signal sd_ack       : vd_std_array(1 downto 0);
   signal buff_addr    : std_logic_vector(13 downto 0);
   signal buff_dout    : std_logic_vector(7 downto 0);
   signal buff_wr      : std_logic;
   signal buff_din     : vd_vec_array(1 downto 0)(7 downto 0);
   signal dirty        : std_logic_vector(1 downto 0);
   signal ready        : std_logic := '1';
   signal power        : vd_vec_array(1 downto 0)(1 downto 0);
   signal run          : std_logic_vector(1 downto 0) := "00";

   type t_dbuf is array (0 to 16383) of std_logic_vector(7 downto 0);
   type t_dbufs is array (0 to 1) of t_dbuf;
   signal dbuf         : t_dbufs := (others => (others => x"EE"));
   -- the test's own port into the drive buffers
   signal tb_we        : std_logic := '0';
   signal tb_clr       : std_logic := '0';
   signal tb_d         : natural range 0 to 1 := 0;
   signal tb_addr      : natural range 0 to 16383 := 0;
   signal tb_data      : std_logic_vector(7 downto 0) := (others => '0');

   -- engine
   signal want         : std_logic;
   signal c2q          : t_sd_c2q;
   signal q2c          : t_sd_q2c := (others => '0');
   signal card_gen     : std_logic_vector(7 downto 0) := x"01";

   type t_card is array (0 to C_CARD_BLK * 512 - 1) of std_logic_vector(7 downto 0);
   signal card_mem     : t_card;
   signal card_writes  : natural := 0;             -- blocks written to the card so far

   -- where image block b lies on the card (the jumps are 4 KiB apart)
   function img2card(b : natural) return natural is
   begin
      if b < 16 then
         return 200 + b;
      elsif b < 40 then
         return 300 + b - 16;
      else
         return 248 + b - 40;
      end if;
   end function img2card;

   function init_byte(cb : natural; i : natural) return std_logic_vector is
   begin
      return std_logic_vector(to_unsigned((cb * 29 + i * 7 + i / 64) mod 256, 8));
   end function init_byte;

begin

   clk <= not clk after C_CLK / 2 when running;

   i_dut : entity work.vd_sd
      generic map (
         G_VDNUM       => 2,
         G_FLUSH_DELAY => C_FLUSH
      )
      port map (
         clk_i          => clk,
         rst_i          => rst,
         qnice_drive_i  => q_drive,
         qnice_addr_i   => q_addr,
         qnice_data_i   => q_wdata,
         qnice_ce_i     => q_ce,
         qnice_we_i     => q_we,
         qnice_data_o   => q_rdata,
         sd_lba_i       => sd_lba,
         sd_blk_cnt_i   => sd_blk_cnt,
         sd_rd_i        => sd_rd,
         sd_wr_i        => sd_wr,
         sd_ack_o       => sd_ack,
         sd_buff_addr_o => buff_addr,
         sd_buff_dout_o => buff_dout,
         sd_buff_wr_o   => buff_wr,
         sd_buff_din_i  => buff_din,
         dirty_o        => dirty,
         ready_i        => ready,
         power_o        => power,
         running_i      => run,
         eng_want_o     => want,
         eng_o          => c2q,
         eng_i          => q2c
      );

   -- the drives' buffers: written while their ack is high, read one clock later
   p_dbuf : process (clk)
   begin
      if rising_edge(clk) then
         for d in 0 to 1 loop
            if buff_wr = '1' and sd_ack(d) = '1' then
               dbuf(d)(to_integer(unsigned(buff_addr))) <= buff_dout;
            end if;
            buff_din(d) <= dbuf(d)(to_integer(unsigned(buff_addr)));
         end loop;
         if tb_we = '1' then
            dbuf(tb_d)(tb_addr) <= tb_data;
         end if;
         if tb_clr = '1' then
            dbuf(tb_d) <= (others => x"EE");
         end if;
      end if;
   end process p_dbuf;

   -- sdhost's engine port in front of the model card
   p_engine : process
      variable seed1, seed2 : positive := 5;
      variable r            : real;
      variable addr, cnt    : natural;
      variable wr           : boolean;
   begin
      for b in 0 to C_CARD_BLK - 1 loop
         for i in 0 to 511 loop
            card_mem(b * 512 + i) <= init_byte(b, i);
         end loop;
      end loop;
      q2c <= (others => '0');
      q2c(Q2C_READY) <= '1';
      q2c(Q2C_CARD)  <= card_gen;
      loop
         wait until rising_edge(clk);
         q2c(Q2C_CARD) <= card_gen;
         if c2q(C2Q_START) = '1' and q2c(Q2C_BUSY) = '0' then
            addr := to_integer(unsigned(c2q(C2Q_ADDR)));
            cnt  := to_integer(unsigned(c2q(C2Q_COUNT)));
            wr   := c2q(C2Q_WRITE) = '1';
            assert cnt > 0 report "start with count 0" severity failure;
            assert addr + cnt <= C_CARD_BLK report "transfer beyond the card" severity failure;
            wait until rising_edge(clk);
            q2c(Q2C_BUSY) <= '1';
            for k in 0 to cnt - 1 loop
               if wr then
                  -- like sdhost: address out, the byte one clock later
                  for i in 0 to 511 loop
                     q2c(Q2C_WRBLK) <= std_logic_vector(to_unsigned(k, 16));
                     q2c(Q2C_WRIDX) <= std_logic_vector(to_unsigned(i, 9));
                     wait until rising_edge(clk);
                     wait until rising_edge(clk);
                     card_mem((addr + k) * 512 + i) <= c2q(C2Q_WRDATA);
                  end loop;
                  card_writes <= card_writes + 1;
               else
                  for i in 0 to 511 loop
                     uniform(seed1, seed2, r);
                     if r < 0.3 then
                        wait until rising_edge(clk);
                     end if;
                     q2c(Q2C_RDWE)   <= '1';
                     q2c(Q2C_RDBLK)  <= std_logic_vector(to_unsigned(k, 16));
                     q2c(Q2C_RDIDX)  <= std_logic_vector(to_unsigned(i, 9));
                     q2c(Q2C_RDDATA) <= card_mem((addr + k) * 512 + i);
                     wait until rising_edge(clk);
                     q2c(Q2C_RDWE)   <= '0';
                  end loop;
               end if;
               wait until rising_edge(clk);
               q2c(Q2C_BLK) <= '1';
               wait until rising_edge(clk);
               q2c(Q2C_BLK) <= '0';
            end loop;
            wait until rising_edge(clk);
            q2c(Q2C_BUSY) <= '0';
            q2c(Q2C_DONE) <= '1';
            wait until rising_edge(clk);
            q2c(Q2C_DONE) <= '0';
         end if;
      end loop;
   end process p_engine;

   p_test : process
      variable errors : natural := 0;
      variable shadow : t_card;                    -- what the card must hold

      procedure qwrite(d : natural; win : std_logic_vector(15 downto 0); ofs : natural; v : natural) is
      begin
         q_drive <= d;
         q_addr  <= win & std_logic_vector(to_unsigned(ofs, 12));
         q_wdata <= std_logic_vector(to_unsigned(v, 16));
         q_ce    <= '1';
         q_we    <= '1';
         wait until rising_edge(clk);
         q_ce    <= '0';
         q_we    <= '0';
         wait until rising_edge(clk);
      end procedure qwrite;

      procedure status(d : natural; st : out std_logic_vector(15 downto 0)) is
      begin
         q_drive <= d;
         q_addr  <= x"FFFF" & x"031";
         q_ce    <= '1';
         q_we    <= '0';
         wait until falling_edge(clk);
         st      := q_rdata;
         wait until rising_edge(clk);
         q_ce    <= '0';
         wait until rising_edge(clk);
      end procedure status;

      procedure csr(d : natural; ofs : natural; st : out std_logic_vector(15 downto 0)) is
      begin
         q_drive <= d;
         q_addr  <= x"FFFF" & std_logic_vector(to_unsigned(ofs, 12));
         q_ce    <= '1';
         q_we    <= '0';
         wait until falling_edge(clk);
         st      := q_rdata;
         wait until rising_edge(clk);
         q_ce    <= '0';
         wait until rising_edge(clk);
      end procedure csr;

      procedure wait_idle(d : natural) is
         variable st : std_logic_vector(15 downto 0);
      begin
         loop
            status(d, st);
            exit when st(2) = '0';
         end loop;
      end procedure wait_idle;

      procedure mount(d : natural) is
      begin
         qwrite(d, x"FFFF", 16#031#, 0);
         wait_idle(d);
         for p in 0 to C_IMG_BLK / 8 - 1 loop
            qwrite(d, x"0000", 2 * p, img2card(8 * p));
            qwrite(d, x"0000", 2 * p + 1, 0);
         end loop;
         qwrite(d, x"FFFF", 16#032#, C_IMG_BLK);
         qwrite(d, x"FFFF", 16#031#, 1);
         wait_idle(d);
      end procedure mount;

      -- the drive asks for n 256-byte blocks from lba on
      procedure request(d : natural; wr : boolean; lba, n : natural) is
      begin
         sd_lba(d)     <= std_logic_vector(to_unsigned(lba, 32));
         sd_blk_cnt(d) <= std_logic_vector(to_unsigned(n - 1, 6));
         if wr then
            sd_wr(d) <= '1';
         else
            sd_rd(d) <= '1';
         end if;
         wait until rising_edge(clk) and sd_ack(d) = '1';
         sd_rd(d) <= '0';
         sd_wr(d) <= '0';
         wait until rising_edge(clk) and sd_ack(d) = '0';
         wait until rising_edge(clk);
      end procedure request;

      -- image byte position -> card byte position
      function img_pos(o : natural) return natural is
      begin
         return img2card(o / 512) * 512 + o mod 512;
      end function img_pos;

      procedure check_read(d : natural; lba, n : natural; name : string) is
         variable bad : natural := 0;
      begin
         for j in 0 to n * 256 - 1 loop
            if dbuf(d)(j) /= shadow(img_pos(lba * 256 + j)) then
               if bad < 4 then
                  report name & ": buffer byte " & integer'image(j) & " is " &
                         integer'image(to_integer(unsigned(dbuf(d)(j)))) & ", expected " &
                         integer'image(to_integer(unsigned(shadow(img_pos(lba * 256 + j))))) severity error;
               end if;
               bad := bad + 1;
            end if;
         end loop;
         if bad = 0 then
            report name & ": ok";
         else
            errors := errors + 1;
         end if;
      end procedure check_read;

      procedure check_card(name : string) is
         variable bad : natural := 0;
      begin
         for j in 0 to C_CARD_BLK * 512 - 1 loop
            if card_mem(j) /= shadow(j) then
               if bad < 4 then
                  report name & ": card byte " & integer'image(j) & " is " &
                         integer'image(to_integer(unsigned(card_mem(j)))) & ", expected " &
                         integer'image(to_integer(unsigned(shadow(j)))) severity error;
               end if;
               bad := bad + 1;
            end if;
         end loop;
         if bad = 0 then
            report name & ": ok";
         else
            errors := errors + 1;
         end if;
      end procedure check_card;

      -- fill the drive's buffer with new data for a write of n blocks at lba
      procedure prepare_write(d : natural; lba, n, seed : natural) is
         variable v : std_logic_vector(7 downto 0);
      begin
         tb_d <= d;
         for j in 0 to n * 256 - 1 loop
            v := std_logic_vector(to_unsigned((j * 13 + seed * 31 + 7) mod 256, 8));
            tb_we   <= '1';
            tb_addr <= j;
            tb_data <= v;
            shadow(img_pos(lba * 256 + j)) := v;
            wait until rising_edge(clk);
         end loop;
         tb_we <= '0';
         wait until rising_edge(clk);
      end procedure prepare_write;

      variable st : std_logic_vector(15 downto 0);
      variable w0 : natural;

   begin
      for b in 0 to C_CARD_BLK - 1 loop
         for i in 0 to 511 loop
            shadow(b * 512 + i) := init_byte(b, i);
         end loop;
      end loop;
      wait for 5 * C_CLK;
      wait until rising_edge(clk);
      rst <= '0';
      wait for 5 * C_CLK;

      mount(0);
      mount(1);

      -- 1. reads
      request(0, false, 0, 21);                    -- track 1 of a D64: blocks 0..10.5
      check_read(0, 0, 21, "1a track read from the start");
      request(0, false, 21, 19);                   -- starts in the middle of block 10, jump at 16
      check_read(0, 21, 19, "1b track read from the middle of a block, across a jump");
      request(1, false, 58, 2);                    -- 1581: one 512-byte sector
      check_read(1, 58, 2, "1c 1581 sector on drive 9");
      request(0, false, 76, 9);                    -- across the jump at block 40
      check_read(0, 76, 9, "1d across the second jump");

      -- 2. a track write: 19 sectors from lba 21 (ends of the run are partial blocks)
      w0 := card_writes;
      prepare_write(0, 21, 19, 1);
      request(0, true, 21, 19);
      if card_writes /= w0 then
         report "2a: the write reached the card before the quiet time" severity error;
         errors := errors + 1;
      end if;
      tb_d   <= 0;
      tb_clr <= '1';
      wait until rising_edge(clk);
      tb_clr <= '0';
      wait until rising_edge(clk);
      request(0, false, 20, 22);                   -- read back, partly from the cache
      check_read(0, 20, 22, "2b read back before the write-back");
      wait for (C_FLUSH + 20000) * C_CLK;
      if dirty(0) = '1' then
         report "2c: still dirty after the quiet time" severity error;
         errors := errors + 1;
      end if;
      check_card("2c write-back after the quiet time");

      -- 3. more changed blocks than the cache holds
      prepare_write(1, 0, 63, 2);                  -- 31.5 blocks of drive 9 ...
      request(1, true, 0, 63);
      prepare_write(0, 64, 64, 3);                 -- ... and 32 of drive 8
      request(0, true, 64, 64);
      wait for (C_FLUSH + 40000) * C_CLK;
      check_card("3 full cache, written back");

      -- 4. off: the changes reach the card first, then the drive reads zeros
      prepare_write(0, 2, 4, 4);
      request(0, true, 2, 4);
      qwrite(0, x"FFFF", 16#031#, 0);
      wait_idle(0);
      check_card("4a off writes back");
      request(0, false, 0, 4);
      if dbuf(0)(0) /= x"00" or dbuf(0)(1023) /= x"00" then
         report "4b: a drive that is off did not read zeros" severity error;
         errors := errors + 1;
      else
         report "4b off reads zeros: ok";
      end if;

      -- 5. another card: drive 9 is switched off and nothing reaches the card
      mount(0);
      prepare_write(1, 40, 2, 5);
      request(1, true, 40, 2);
      for j in 0 to 2 * 256 - 1 loop               -- this write must never arrive
         shadow(img_pos(40 * 256 + j)) := card_mem(img_pos(40 * 256 + j));
      end loop;
      card_gen <= x"02";
      wait for (C_FLUSH + 20000) * C_CLK;
      status(1, st);
      if st(0) /= '0' or st(4) /= '1' then
         report "5: drive 9 not lost after a card change" severity error;
         errors := errors + 1;
      else
         report "5a card change: drive lost: ok";
      end if;
      check_card("5b nothing written to the other card");

      -- 6. iec_drive's disk_ready, from the core's clock, in bit 5 of both drives' CSR
      for want in std_logic range '0' to '1' loop
         ready <= want;
         wait for 5 * C_CLK;
         for d in 0 to 1 loop
            status(d, st);
            if st(5) /= want then
               report "6: drive " & integer'image(8 + d) & " shows ready " & std_logic'image(st(5)) &
                      ", not " & std_logic'image(want) severity error;
               errors := errors + 1;
            end if;
         end loop;
      end loop;
      report "6 disk_ready in the CSR: done";

      -- 7. Each drive's power (CSR 0x034): out as written, the other drive's untouched; the
      -- running state read back from the core's clock; automatic again after a reset
      for d in 0 to 1 loop
         for m in 1 to 3 loop
            qwrite(d, x"FFFF", 16#034#, m mod 3);
            csr(d, 16#034#, st);
            if to_integer(unsigned(power(d))) /= m mod 3 or to_integer(unsigned(st(1 downto 0))) /= m mod 3 or
               power(1 - d) /= "00" then
               report "7: drive " & integer'image(8 + d) & " power " & integer'image(m mod 3) & " reads " &
                      integer'image(to_integer(unsigned(st(1 downto 0)))) severity error;
               errors := errors + 1;
            end if;
         end loop;
         run    <= (others => '0');
         run(d) <= '1';
         wait for 5 * C_CLK;
         for e in 0 to 1 loop
            csr(e, 16#034#, st);
            if st(2) /= run(e) then
               report "7: drive " & integer'image(8 + e) & " running reads " & std_logic'image(st(2)) severity error;
               errors := errors + 1;
            end if;
         end loop;
      end loop;
      qwrite(1, x"FFFF", 16#034#, 2);
      rst <= '1';
      wait for 3 * C_CLK;
      rst <= '0';
      wait for 3 * C_CLK;
      if power(0) /= "00" or power(1) /= "00" then
         report "7: power not automatic after a reset" severity error;
         errors := errors + 1;
      end if;
      report "7 drive power: done";

      -- 8. Hold (CSR 0x035): a held drive's request waits, the other drive's doesn't; it is
      -- served once released; a reset releases it
      mount(0);
      mount(1);
      qwrite(0, x"FFFF", 16#035#, 1);
      csr(0, 16#035#, st);
      sd_lba(0)     <= std_logic_vector(to_unsigned(8, 32));
      sd_blk_cnt(0) <= std_logic_vector(to_unsigned(1, 6));
      sd_rd(0)      <= '1';
      request(1, false, 0, 2);                     -- drive 9 meanwhile
      wait for 2000 * C_CLK;
      if st(0) /= '1' or sd_ack(0) = '1' then
         report "8a: a held drive was served (hold reads " & std_logic'image(st(0)) & ")" severity error;
         errors := errors + 1;
      end if;
      check_read(1, 0, 2, "8a the other drive while one is held");
      qwrite(0, x"FFFF", 16#035#, 0);
      wait until rising_edge(clk) and sd_ack(0) = '1';
      sd_rd(0) <= '0';
      wait until rising_edge(clk) and sd_ack(0) = '0';
      wait until rising_edge(clk);
      check_read(0, 8, 2, "8b served once released");
      qwrite(1, x"FFFF", 16#035#, 1);
      rst <= '1';
      wait for 3 * C_CLK;
      rst <= '0';
      wait for 3 * C_CLK;
      csr(1, 16#035#, st);
      if st(0) /= '0' then
         report "8c: hold not released by a reset" severity error;
         errors := errors + 1;
      end if;
      report "8 hold: done";

      if errors = 0 then
         report "ALL CHECKS PASSED";
      else
         report "FAILED with " & integer'image(errors) & " errors" severity error;
      end if;
      running <= false;
      wait;
   end process p_test;

end architecture sim;
