----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- tb_cmdhd: the CMD HD boots its DOS from a disk image.
--
-- cmdhd runs the real boot ROM (G_ROM, 16 KB or the 32 KB mirrored dump). A process plays
-- the firmware: it answers the disk's SCSI commands as VICE's scsi.c does, with image block n
-- at SD block n, and moves the blocks through cmdhd's engine client. A model of the SD
-- controller's engine port serves the image (G_IMG, its first G_IMG_BLOCKS blocks held in
-- memory; past them the disk reads zeros, up to G_CAPACITY blocks).
--
-- Pass: the drive CPU settles in HD DOS's idle loop ($F870-$F8BF): 70% of a window of 1000
-- samples of its program counter, with no SCSI command in the window.
--
-- Done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.sdhost_pkg.all;

entity tb_cmdhd is
generic (
   G_ROM        : string  := "boot.rom";
   G_IMG        : string  := "disk.dhd";
   G_IMG_BLOCKS : natural := 2048;          -- blocks of the image kept (1 MB)
   G_CAPACITY   : natural := 131072;        -- what READ CAPACITY says (64 MB)
   G_TIMEOUT_MS : natural := 15000          -- drive time
);
end entity tb_cmdhd;

architecture sim of tb_cmdhd is

   constant C_MAIN_HZ : natural := 16_000_000;   -- ce every clock: a 2 MHz drive CPU
   constant C_MAIN_T  : time := 62.5 ns;
   constant C_QN_T    : time := 40 ns;           -- 25 MHz is plenty for the firmware model

   signal clk_main, clk_qn : std_logic := '0';
   signal rst_main         : std_logic := '1';
   signal rst_qn           : std_logic := '1';
   signal done             : boolean := false;

   signal qn_addr   : std_logic_vector(27 downto 0) := (others => '0');
   signal qn_wdata  : std_logic_vector(15 downto 0) := (others => '0');
   signal qn_rdata  : std_logic_vector(15 downto 0);
   signal qn_ce     : std_logic := '0';
   signal qn_we     : std_logic := '0';
   signal eng_want  : std_logic;
   signal eng_c2q   : t_sd_c2q;
   signal eng_q2c   : t_sd_q2c := (others => '0');

   signal iec_clk_o, iec_data_o, iec_fclk_o : std_logic;
   signal running, led_act, led_err : std_logic;
   signal fw_pc            : natural := 0;

   type t_bytes is array (natural range <>) of natural range 0 to 255;
   shared variable img : t_bytes(0 to G_IMG_BLOCKS * 512 - 1);

   -- engine model's view: the image
   impure function img_get(i : natural) return natural is
   begin
      if i < G_IMG_BLOCKS * 512 then
         return img(i);
      end if;
      return 0;
   end function;

begin

   clk_main <= not clk_main after C_MAIN_T / 2 when not done;
   clk_qn   <= not clk_qn   after C_QN_T / 2 when not done;

   i_dut : entity work.cmdhd
      generic map (
         G_CLK_HZ => C_MAIN_HZ
      )
      port map (
         clk_main_i   => clk_main,
         rst_main_i   => rst_main,
         ce_i         => '1',
         pause_i      => '0',
         iec_atn_i    => '1',
         iec_clk_i    => iec_clk_o,
         iec_data_i   => iec_data_o,
         iec_fclk_i   => iec_fclk_o,
         iec_clk_o    => iec_clk_o,
         iec_data_o   => iec_data_o,
         iec_fclk_o   => iec_fclk_o,
         rtc_i        => (others => '0'),
         running_o    => running,
         led_act_o    => led_act,
         led_err_o    => led_err,
         clk_qnice_i  => clk_qn,
         rst_qnice_i  => rst_qn,
         qnice_addr_i => qn_addr,
         qnice_data_i => qn_wdata,
         qnice_ce_i   => qn_ce,
         qnice_we_i   => qn_we,
         qnice_data_o => qn_rdata,
         eng_want_o   => eng_want,
         eng_o        => eng_c2q,
         eng_i        => eng_q2c
      );

   ---------------------------------------------------------------------------------------
   -- the SD controller's engine port: one transfer at a time, card 0
   ---------------------------------------------------------------------------------------
   p_engine : process
      variable blk, n : natural;
   begin
      eng_q2c <= (others => '0');
      eng_q2c(Q2C_READY) <= '1';
      loop
         wait until rising_edge(clk_qn);
         if eng_c2q(C2Q_START) = '1' then
            blk := to_integer(unsigned(eng_c2q(C2Q_ADDR)));
            n   := to_integer(unsigned(eng_c2q(C2Q_COUNT)));
            eng_q2c(Q2C_BUSY) <= '1';
            for k in 1 to 20 loop                         -- the card's latency, a little
               wait until rising_edge(clk_qn);
            end loop;
            for b in 0 to n - 1 loop
               for i in 0 to 511 loop
                  if eng_c2q(C2Q_WRITE) = '0' then
                     eng_q2c(Q2C_RDWE)   <= '1';
                     eng_q2c(Q2C_RDBLK)  <= std_logic_vector(to_unsigned(b, 16));
                     eng_q2c(Q2C_RDIDX)  <= std_logic_vector(to_unsigned(i, 9));
                     eng_q2c(Q2C_RDDATA) <= std_logic_vector(to_unsigned(img_get((blk + b) * 512 + i), 8));
                     wait until rising_edge(clk_qn);
                     eng_q2c(Q2C_RDWE)   <= '0';
                  else
                     eng_q2c(Q2C_WRBLK) <= std_logic_vector(to_unsigned(b, 16));
                     eng_q2c(Q2C_WRIDX) <= std_logic_vector(to_unsigned(i, 9));
                     wait until rising_edge(clk_qn);   -- address out
                     wait until rising_edge(clk_qn);   -- data back
                     if (blk + b) * 512 + i < G_IMG_BLOCKS * 512 then
                        img((blk + b) * 512 + i) := to_integer(unsigned(eng_c2q(C2Q_WRDATA)));
                     end if;
                  end if;
               end loop;
               eng_q2c(Q2C_BLK) <= '1';
               wait until rising_edge(clk_qn);
               eng_q2c(Q2C_BLK) <= '0';
            end loop;
            eng_q2c(Q2C_DONE) <= '1';
            eng_q2c(Q2C_BUSY) <= '0';
            wait until rising_edge(clk_qn);
            eng_q2c(Q2C_DONE) <= '0';
         end if;
      end loop;
   end process p_engine;

   ---------------------------------------------------------------------------------------
   -- the firmware
   ---------------------------------------------------------------------------------------
   p_fw : process
      type t_char_file is file of character;
      file f            : t_char_file;
      variable c        : character;
      variable st       : std_logic_vector(15 downto 0);
      variable cdb      : t_bytes(0 to 15);
      variable len      : natural;
      variable lba, nb  : natural;
      variable sense    : natural := 0;
      variable pc       : natural;
      variable idle_n   : natural := 0;    -- samples in the idle loop, this window
      variable win_n    : natural := 0;    -- samples in this window
      variable win_cmds : natural := 0;    -- commands when it started
      variable cmds     : natural := 0;
      variable reads    : natural := 0;
      variable writes   : natural := 0;
      variable t0       : time;

      procedure wr(a : natural; d : natural) is
      begin
         qn_addr  <= std_logic_vector(to_unsigned(a, 28));
         qn_wdata <= std_logic_vector(to_unsigned(d, 16));
         qn_ce    <= '1';
         qn_we    <= '1';
         wait until rising_edge(clk_qn);
         qn_ce    <= '0';
         qn_we    <= '0';
      end procedure;

      procedure rd(a : natural; d : out std_logic_vector(15 downto 0)) is
      begin
         qn_addr <= std_logic_vector(to_unsigned(a, 28));
         qn_ce   <= '1';
         wait until rising_edge(clk_qn);
         wait until rising_edge(clk_qn);
         d       := qn_rdata;
         qn_ce   <= '0';
      end procedure;

      constant CSR : natural := 16#FFFF000#;

      procedure csr_wr(r : natural; d : natural) is
      begin
         wr(CSR + r, d);
      end procedure;

      procedure wait_event(bitn : natural) is
         variable s : std_logic_vector(15 downto 0);
      begin
         loop
            rd(CSR + 1, s);
            exit when s(bitn) = '1';
         end loop;
         csr_wr(1, 2 ** bitn);
      end procedure;

      procedure go(phase : natural; count : natural; base : natural; status : natural) is
      begin
         csr_wr(4, count);
         csr_wr(5, base);
         csr_wr(6, status);
         csr_wr(3, phase);
      end procedure;

      procedure sd(blk : natural; write : natural) is
         variable s : std_logic_vector(15 downto 0);
      begin
         csr_wr(16#10#, blk mod 65536);
         csr_wr(16#11#, blk / 65536);
         csr_wr(16#12#, 0);
         csr_wr(16#13#, write);
         loop
            rd(CSR + 16#13#, s);
            exit when s(0) = '0';
         end loop;
         assert s(1) = '0' report "SD transfer failed" severity failure;
      end procedure;

      procedure buf_fill(bytes : t_bytes) is
      begin
         for i in bytes'range loop
            wr(16#4000# + i - bytes'low, bytes(i));
         end loop;
      end procedure;

      procedure status(s : natural) is
      begin
         go(2, 1, 0, s);
         wait_event(2);
      end procedure;

   begin
      -- the image
      file_open(f, G_IMG, read_mode);
      for i in 0 to G_IMG_BLOCKS * 512 - 1 loop
         exit when endfile(f);
         read(f, c);
         img(i) := character'pos(c);
      end loop;
      file_close(f);

      wait for 200 ns;
      rst_qn <= '0';
      wait until rising_edge(clk_qn);

      -- the boot ROM
      file_open(f, G_ROM, read_mode);
      for i in 0 to 16383 loop
         read(f, c);
         wr(i, character'pos(c));
      end loop;
      file_close(f);
      report "boot ROM loaded";

      -- power up: clear the RAM while held in reset, then run
      rst_main <= '0';
      csr_wr(16#14#, 0);
      csr_wr(8, 16#10#);
      wait_event(3);
      csr_wr(8, 16#01#);
      t0 := now;
      report "drive runs";

      loop
         rd(CSR + 1, st);
         if st(0) = '1' then                     -- a command
            csr_wr(1, 1);
            rd(CSR + 2, st);
            len := to_integer(unsigned(st(11 downto 8)));
            for i in 0 to len - 1 loop
               rd(16#4400# + i, st);
               cdb(i) := to_integer(unsigned(st(7 downto 0)));
            end loop;
            cmds := cmds + 1;
            if cmds <= 400 then
               report "SCSI " & to_hstring(to_unsigned(cdb(0), 8)) & " " &
                      to_hstring(to_unsigned(cdb(1), 8)) & " " & to_hstring(to_unsigned(cdb(2), 8)) & " " &
                      to_hstring(to_unsigned(cdb(3), 8)) & " " & to_hstring(to_unsigned(cdb(4), 8)) & " " &
                      to_hstring(to_unsigned(cdb(5), 8)) & " len " & integer'image(len) &
                      " at " & time'image(now - t0);
            end if;

            if cdb(1) / 32 /= 0 and cdb(0) /= 16#12# and cdb(0) /= 16#03# then
               sense := 5;                       -- no such LUN
               status(2);
            else
               case cdb(0) is
                  when 16#00# | 16#01# | 16#1B# | 16#1D# | 16#2F# | 16#04# =>
                     status(0);

                  when 16#03# =>                 -- REQUEST SENSE
                     len := cdb(4);
                     if len = 0 then len := 4; end if;
                     if len > 18 then len := 18; end if;
                     buf_fill((16#F0#, 0, sense, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0));
                     sense := 0;
                     go(0, len, 0, 0);
                     wait_event(1);
                     status(0);

                  when 16#12# =>                 -- INQUIRY
                     len := cdb(4);
                     if len > 36 then len := 36; end if;
                     if cdb(1) / 32 /= 0 then
                        buf_fill((0 => 16#60#, 1 => 0, 2 => 1, 3 => 2, 4 => 31));
                     else
                        buf_fill((0, 0, 1, 2, 31, 0, 0, 0,
                                  77, 69, 71, 65, 54, 53, 32, 32,
                                  67, 77, 68, 32, 72, 68, 32, 73, 109, 97, 103, 101, 32, 32, 32, 32,
                                  49, 46, 48, 32));
                     end if;
                     go(0, len, 0, 0);
                     wait_event(1);
                     status(0);

                  when 16#25# =>                 -- READ CAPACITY
                     buf_fill(((G_CAPACITY - 1) / 16777216, ((G_CAPACITY - 1) / 65536) mod 256,
                               ((G_CAPACITY - 1) / 256) mod 256, (G_CAPACITY - 1) mod 256,
                               0, 0, 2, 0));
                     go(0, 8, 0, 0);
                     wait_event(1);
                     status(0);

                  when 16#1A# =>                 -- MODE SENSE: page 0 or 0x20, as VICE
                     if cdb(2) mod 64 = 0 or cdb(2) mod 64 = 16#20# then
                        len := cdb(4);
                        if len > 12 then len := 12; end if;
                        buf_fill((0, 0, 0, 1, 0, 0, 4, 77, 69, 71, 65, 0));
                        go(0, len, 0, 0);
                        wait_event(1);
                        status(0);
                     else
                        sense := 5;
                        status(2);
                     end if;

                  when 16#08# | 16#28# =>        -- READ
                     if cdb(0) = 16#08# then
                        lba := (cdb(1) mod 32) * 65536 + cdb(2) * 256 + cdb(3);
                        nb  := cdb(4);
                        if nb = 0 then nb := 256; end if;
                     else
                        lba := cdb(2) * 16777216 + cdb(3) * 65536 + cdb(4) * 256 + cdb(5);
                        nb  := cdb(7) * 256 + cdb(8);
                     end if;
                     for b in 0 to nb - 1 loop
                        if lba + b >= G_CAPACITY then
                           sense := 5;
                           exit;
                        end if;
                        sd(lba + b, 0);
                        go(0, 512, 0, 0);
                        wait_event(1);
                        reads := reads + 1;
                     end loop;
                     if sense /= 0 then status(2); else status(0); end if;

                  when 16#0A# | 16#2A# =>        -- WRITE
                     if cdb(0) = 16#0A# then
                        lba := (cdb(1) mod 32) * 65536 + cdb(2) * 256 + cdb(3);
                        nb  := cdb(4);
                        if nb = 0 then nb := 256; end if;
                     else
                        lba := cdb(2) * 16777216 + cdb(3) * 65536 + cdb(4) * 256 + cdb(5);
                        nb  := cdb(7) * 256 + cdb(8);
                     end if;
                     for b in 0 to nb - 1 loop
                        go(1, 512, 0, 0);
                        wait_event(1);
                        sd(lba + b, 1);
                        writes := writes + 1;
                     end loop;
                     status(0);

                  when others =>
                     report "unknown SCSI command " & integer'image(cdb(0)) severity warning;
                     sense := 5;
                     status(2);
               end case;
            end if;
         end if;

         if st(2) = '1' then
            csr_wr(1, 4);
         end if;

         -- where the drive is
         rd(CSR + 9, st);
         pc := to_integer(unsigned(st));
         fw_pc <= pc;
         rd(CSR + 8, st);
         -- idle: most of a window of samples in the loop (it calls out of it now and then), and
         -- no command in it
         if pc >= 16#F870# and pc < 16#F8C0# then
            idle_n := idle_n + 1;
         end if;
         win_n := win_n + 1;
         if win_n = 1000 and (idle_n < 700 or cmds /= win_cmds) then
            win_n    := 0;
            idle_n   := 0;
            win_cmds := cmds;
         end if;
         if win_n = 1000 then
            report "HD DOS idle after " & time'image(now - t0) & ": " & integer'image(cmds) &
                   " commands, " & integer'image(reads) & " blocks read, " &
                   integer'image(writes) & " written";
            report "ALL CHECKS PASSED";
            done <= true;
            wait;
         end if;
         if now - t0 > G_TIMEOUT_MS * 1 ms then
            report "timeout: PC $" & to_hstring(to_unsigned(pc, 16)) & ", " &
                   integer'image(cmds) & " commands" severity failure;
         end if;
      end loop;
   end process p_fw;

   -- debug: entries to the routines of interest, and instruction rate
   p_trace : process
      alias regs is <<signal .tb_cmdhd.i_dut.i_drive.cpu_regs : std_logic_vector(63 downto 0)>>;
      alias drst is <<signal .tb_cmdhd.i_dut.drv_rst : std_logic>>;
      variable pc, last : natural := 0;
      variable n : natural := 0;
   begin
      loop
         wait until rising_edge(clk_main);
         pc := to_integer(unsigned(regs(63 downto 48)));
         if pc /= last then
            n := n + 1;
            if pc = 16#DE2E# or pc = 16#DF4D# or pc = 16#DF67# or pc = 16#E087# then
               report "PC $" & to_hstring(to_unsigned(pc, 16)) & " at " & time'image(now) &
                      " changes " & integer'image(n) & " rst " & std_logic'image(drst);
            end if;
            last := pc;
         end if;
         if now > 5 ms and now < 7 ms and (n mod 50000) = 0 and pc /= last then
            null;
         end if;
      end loop;
   end process p_trace;

   p_rate : process
      alias regs is <<signal .tb_cmdhd.i_dut.i_drive.cpu_regs : std_logic_vector(63 downto 0)>>;
      alias ph2r is <<signal .tb_cmdhd.i_dut.i_drive.ph2_r : std_logic>>;
      variable cyc : natural := 0;
   begin
      wait for 6 ms;
      for k in 1 to 6 loop
         cyc := 0;
         for i in 1 to 160000 loop          -- 10 ms of 16 MHz
            wait until rising_edge(clk_main);
            if ph2r = '1' then cyc := cyc + 1; end if;
         end loop;
         report "10 ms: " & integer'image(cyc) & " CPU cycles, PC $" &
                to_hstring(regs(63 downto 48)) & " X $" & to_hstring(regs(15 downto 8)) &
                " Y $" & to_hstring(regs(23 downto 16));
      end loop;
      wait;
   end process p_rate;

   -- progress
   p_watch : process
      variable pc_last : natural := 0;
   begin
      loop
         wait for 100 ms;
         report "t=" & time'image(now) & " PC $" & to_hstring(to_unsigned(fw_pc, 16)) &
                " running=" & std_logic'image(running) &
                " act=" & std_logic'image(led_act) & " err=" & std_logic'image(led_err);
      end loop;
   end process p_watch;

end architecture sim;
