----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Testbench for his video and audio streams (CORE/vhdl/net/stream_tx.vhd) in the network
-- device. A VIC model sends a known pattern at the real pixel rate, the audio is the main
-- clock's count, and a firmware model writes the headers, starts the streams and sends its
-- own frames meanwhile. The RMII output is decoded and checked: FCS, the 42 header bytes,
-- his stream header, every pixel, sequence and frame counting, the sample rate, a restart
-- from 0, nothing after a stop, every firmware frame whole, the counters. A second
-- stream_tx whose output stalls checks that dropped groups show as sequence gaps.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_stream_tx is
end entity tb_stream_tx;

architecture sim of tb_stream_tx is

   constant C_MAIN_PERIOD : time := 31.718 ns;   -- 31.528 MHz
   constant C_NET_PERIOD  : time := 20 ns;

   constant R_ETH_TX      : natural := 16#09#;
   constant R_ETH_STATUS  : natural := 16#0A#;
   constant R_ETH_CTRL    : natural := 16#0C#;
   constant R_STREAM_CTRL : natural := 16#20#;
   constant R_VSENT       : natural := 16#21#;
   constant R_ASENT       : natural := 16#22#;
   constant R_VDROP       : natural := 16#23#;
   constant R_ADROP       : natural := 16#24#;

   constant C_FW_FRAMES   : natural := 12;
   constant C_FW_LEN      : natural := 1000;

   type bytes_t is array (natural range <>) of std_logic_vector(7 downto 0);

   -- The headers the firmware model writes: any bytes go out as written
   function hdr(base : natural) return bytes_t is
      variable h : bytes_t(0 to 41);
   begin
      for i in h'range loop
         h(i) := std_logic_vector(to_unsigned((base + i * 7) mod 256, 8));
      end loop;
      return h;
   end function hdr;
   constant C_VHDR : bytes_t(0 to 41) := hdr(16#10#);
   constant C_AHDR : bytes_t(0 to 41) := hdr(16#83#);

   -- The VIC model's pixel
   function pixel(x, y, f : natural) return natural is
   begin
      return (x + 3 * y + 5 * f) mod 16;
   end function pixel;

   signal main_clk  : std_logic := '0';
   signal clk       : std_logic := '0';
   signal rst       : std_logic := '1';
   signal done      : boolean := false;

   -- VIC and audio models
   signal vic_ce    : std_logic := '0';
   signal vic_x     : unsigned(9 downto 0) := (others => '0');
   signal vic_y     : unsigned(8 downto 0) := to_unsigned(200, 9);
   signal vic_index : unsigned(3 downto 0) := (others => '0');
   signal vic_frame : natural := 0;          -- the model's frame, counted at raster 16
   signal main_cnt  : unsigned(15 downto 0) := (others => '0');
   signal audio_l   : signed(15 downto 0);
   signal audio_r   : signed(15 downto 0);

   -- Device bus
   signal dev_ce    : std_logic := '0';
   signal dev_we    : std_logic := '0';
   signal dev_addr  : std_logic_vector(11 downto 0) := (others => '0');
   signal dev_wdata : std_logic_vector(15 downto 0) := (others => '0');
   signal dev_rdata : std_logic_vector(15 downto 0);

   signal eth_txd   : std_logic_vector(1 downto 0);
   signal eth_txen  : std_logic;
   signal eth_mdio  : std_logic;

   -- What the checker saw
   signal rx_video     : natural := 0;
   signal rx_audio     : natural := 0;
   signal rx_fw        : natural := 0;
   signal rx_vframe1   : boolean := false;     -- video frame 1 has started
   signal rx_audio_seq : integer := -1;        -- the last audio sequence number
   signal ck_errors    : natural := 0;
   signal fw_errors    : natural := 0;
   signal video_on     : boolean := false;     -- the firmware model has video switched on
   signal video_off_at : time := 0 ns;

   -- The stalled second streamer
   signal s2_ready     : std_logic := '1';
   signal s2_valid     : std_logic;
   signal s2_data      : std_logic_vector(7 downto 0);
   signal s2_last      : std_logic;
   signal s2_vdrop     : unsigned(15 downto 0);
   signal s2_gaps      : natural := 0;
   signal s2_packets   : natural := 0;
   signal s2_errors    : natural := 0;

begin

   main_clk <= not main_clk after C_MAIN_PERIOD / 2 when not done;
   clk      <= not clk      after C_NET_PERIOD / 2  when not done;
   rst      <= '0' after 200 ns;

   watchdog_proc : process
   begin
      wait for 60 ms;
      assert done report "TIMEOUT" severity failure;
      wait;
   end process watchdog_proc;

   ---------------------------------------------------------------------------------------------
   -- VIC model: PAL, 504 pixels x 312 lines, a pixel every 4 main clocks. As the core's VIC,
   -- index, x and y change on the clock edge of the ce, so the device takes the ones before.
   ---------------------------------------------------------------------------------------------

   vic_proc : process (main_clk)
      variable div : natural range 0 to 3 := 0;
   begin
      if rising_edge(main_clk) then
         main_cnt <= main_cnt + 1;
         div      := (div + 1) mod 4;
         vic_ce   <= '1' when div = 0 else '0';
         if vic_ce = '1' then
            if vic_x = 503 then
               vic_x <= (others => '0');
               if vic_y = 311 then
                  vic_y <= (others => '0');
               else
                  vic_y <= vic_y + 1;
               end if;
               if vic_y = 15 then
                  vic_frame <= vic_frame + 1;
               end if;
            else
               vic_x <= vic_x + 1;
            end if;
         end if;
      end if;
   end process vic_proc;

   -- The pixel at the model's current position, in its current frame
   vic_index <= to_unsigned(pixel(to_integer(vic_x), to_integer(vic_y), vic_frame), 4);
   audio_l   <= signed(main_cnt);
   audio_r   <= signed(not main_cnt);

   i_dut : entity work.net
      generic map (
         G_MAIN_CLK_HZ => 31_527_778,
         G_SIM         => true
      )
      port map (
         clk_i       => clk,
         rst_i       => rst,
         main_clk_i  => main_clk,
         main_rst_i  => rst,
         enable_i    => '0',
         cs_i        => '0',
         we_i        => '0',
         addr_i      => "000",
         data_i      => x"00",
         data_o      => open,
         irq_o       => open,
         vic_ce_i    => vic_ce,
         vic_index_i => vic_index,
         vic_x_i     => vic_x,
         vic_y_i     => vic_y,
         audio_l_i   => audio_l,
         audio_r_i   => audio_r,
         dev_ce_i    => dev_ce,
         dev_we_i    => dev_we,
         dev_addr_i  => dev_addr,
         dev_data_i  => dev_wdata,
         dev_data_o  => dev_rdata,
         eth_clock_o => open,
         eth_reset_o => open,
         eth_mdc_o   => open,
         eth_mdio_io => eth_mdio,
         eth_rxd_i   => "00",
         eth_rxdv_i  => '0',
         eth_rxer_i  => '0',
         eth_txd_o   => eth_txd,
         eth_txen_o  => eth_txen
      ); -- i_dut

   ---------------------------------------------------------------------------------------------
   -- The firmware, on the device bus
   ---------------------------------------------------------------------------------------------

   fw_proc : process
      variable r : std_logic_vector(15 downto 0);

      -- As rv_soc: ce from one rising edge to the next, low for a clock between accesses
      procedure dev(a : natural; w : boolean; d : natural) is
      begin
         wait until rising_edge(clk);
         dev_addr  <= std_logic_vector(to_unsigned(a, 12));
         dev_we    <= '1' when w else '0';
         dev_wdata <= std_logic_vector(to_unsigned(d, 16));
         dev_ce    <= '1';
         wait until rising_edge(clk);
         r      := dev_rdata;
         dev_ce <= '0';
         dev_we <= '0';
      end procedure dev;

      procedure write_hdr(base : natural; h : bytes_t) is
      begin
         for w in 0 to 20 loop
            dev(base + w, true, to_integer(unsigned(std_logic_vector'(h(2 * w + 1) & h(2 * w)))));
         end loop;
      end procedure write_hdr;

      procedure send_fw_frame(n : natural) is
      begin
         loop
            dev(R_ETH_STATUS, false, 0);
            exit when r(0) = '1';
         end loop;
         for i in 0 to C_FW_LEN - 1 loop
            if i = C_FW_LEN - 1 then
               dev(R_ETH_TX, true, 256 + (n + i) mod 256);
            elsif i < 6 then
               dev(R_ETH_TX, true, 16#F0#);          -- a destination no stream has
            else
               dev(R_ETH_TX, true, (n + i) mod 256);
            end if;
         end loop;
      end procedure send_fw_frame;

      procedure expect(what : string; got, want : natural) is
      begin
         if got /= want then
            report what & ": " & integer'image(got) & ", want " & integer'image(want) severity error;
            fw_errors <= fw_errors + 1;
         end if;
      end procedure expect;

      variable t0 : time;
   begin
      wait until rst = '0';
      dev(R_ETH_CTRL, true, 1);
      wait for 5 us;                                  -- G_SIM: the PHY's reset is 1 us
      write_hdr(16#40#, C_VHDR);
      write_hdr(16#60#, C_AHDR);
      dev(R_STREAM_CTRL, true, 3);
      video_on <= true;
      dev(R_STREAM_CTRL, false, 0);
      expect("STREAM_CTRL on", to_integer(unsigned(r(1 downto 0))), 3);

      -- Firmware frames in between, while both streams run
      for n in 0 to C_FW_FRAMES - 1 loop
         wait for 1 ms;
         send_fw_frame(n);
      end loop;

      -- Audio off and on again: its sequence starts over
      wait until rx_audio >= 4;
      dev(R_STREAM_CTRL, true, 1);
      wait for 6 ms;
      dev(R_STREAM_CTRL, true, 3);

      -- Video into its second frame, then everything off
      wait until rx_vframe1 and rx_audio_seq >= 1;
      wait for 3 ms;
      dev(R_STREAM_CTRL, true, 0);
      video_on     <= false;
      video_off_at <= now;
      t0 := now;
      loop                                           -- a packet going out finishes
         dev(R_STREAM_CTRL, false, 0);
         exit when r(9 downto 8) = "00";
      end loop;
      assert now - t0 < 100 us report "busy too long after the stop" severity error;
      wait for 2 ms;

      dev(R_VSENT, false, 0);
      expect("VSENT", to_integer(unsigned(r)), rx_video);
      dev(R_ASENT, false, 0);
      expect("ASENT", to_integer(unsigned(r)), rx_audio);
      dev(R_VDROP, false, 0);
      expect("VDROP", to_integer(unsigned(r)), 0);
      dev(R_ADROP, false, 0);
      expect("ADROP", to_integer(unsigned(r)), 0);
      expect("firmware frames", rx_fw, C_FW_FRAMES);

      wait for 100 us;
      report "video packets " & integer'image(rx_video) & ", audio " & integer'image(rx_audio) &
             ", firmware frames " & integer'image(rx_fw) & ", stalled stream: " &
             integer'image(s2_packets) & " packets, " & integer'image(to_integer(s2_vdrop)) & " dropped";
      if fw_errors + ck_errors + s2_errors = 0 and rx_video > 70 and rx_audio > 4 and
         s2_vdrop > 0 and s2_gaps = to_integer(s2_vdrop) then
         report "ALL CHECKS PASSED";
      else
         report "FAILED: " & integer'image(fw_errors) & " firmware, " & integer'image(ck_errors) &
                " wire, " & integer'image(s2_errors) & " stall errors, gaps " &
                integer'image(s2_gaps) severity error;
      end if;
      done <= true;
      wait;
   end process fw_proc;

   ---------------------------------------------------------------------------------------------
   -- The wire: RMII dibits into bytes, a frame per TX_EN
   ---------------------------------------------------------------------------------------------

   wire_proc : process
      variable buf     : bytes_t(0 to 2047);
      variable n       : natural;
      variable sh      : std_logic_vector(7 downto 0);
      variable di      : natural range 0 to 3;
      variable crc     : std_logic_vector(31 downto 0);
      variable fcs     : std_logic_vector(31 downto 0);
      variable seq     : natural;
      variable vseq    : integer := -1;
      variable vframe  : integer := -1;
      variable vline   : integer := -1;
      variable vbase   : natural := 0;          -- the model's frame for DUT frame 0
      variable aseq    : integer := -1;
      variable lastl   : integer := -1;          -- the last audio sample's left value
      variable line    : natural;
      variable lastf   : std_logic;
      variable frm     : natural;
      variable b       : natural;
      variable want    : natural;
      variable d       : integer;
      variable fwn     : natural := 0;

      procedure fail(msg : string) is
      begin
         report msg severity error;
         ck_errors <= ck_errors + 1;
      end procedure fail;

      function u16(lo, hi : std_logic_vector(7 downto 0)) return natural is
      begin
         return to_integer(unsigned(std_logic_vector'(hi & lo)));
      end function u16;

      function is_hdr(bb : bytes_t; h : bytes_t) return boolean is
      begin
         for i in h'range loop
            if bb(bb'low + i) /= h(i) then
               return false;
            end if;
         end loop;
         return true;
      end function is_hdr;
   begin
      -- Collect one frame
      wait until rising_edge(clk) and eth_txen = '1';
      n  := 0;
      di := 0;
      sh := x"00";
      while eth_txen = '1' loop
         sh(2 * di + 1 downto 2 * di) := eth_txd;
         if di = 3 then
            buf(n) := sh;
            n      := n + 1;
            di     := 0;
         else
            di := di + 1;
         end if;
         wait until rising_edge(clk);
      end loop;

      -- Preamble, SFD, FCS
      if n < 8 + 64 then
         fail("short frame: " & integer'image(n) & " bytes");
      else
         for i in 0 to 6 loop
            if buf(i) /= x"55" then
               fail("preamble byte " & integer'image(i));
            end if;
         end loop;
         if buf(7) /= x"D5" then
            fail("SFD");
         end if;
         crc := x"FFFFFFFF";
         for i in 8 to n - 5 loop
            crc(7 downto 0) := crc(7 downto 0) xor buf(i);
            for k in 0 to 7 loop
               if crc(0) = '1' then
                  crc := ('0' & crc(31 downto 1)) xor x"EDB88320";
               else
                  crc := '0' & crc(31 downto 1);
               end if;
            end loop;
         end loop;
         crc := not crc;
         fcs := std_logic_vector'(buf(n - 1) & buf(n - 2) & buf(n - 3) & buf(n - 4));
         if fcs /= crc then
            fail("FCS");
         end if;

         -- The frame without preamble and FCS: 8 .. n-5
         if is_hdr(buf(8 to 49), C_VHDR) then
            -- Video: his 12 bytes, then 4 lines of 384 pixels
            if n - 12 /= 42 + 12 + 768 then
               fail("video frame length " & integer'image(n - 12));
            end if;
            if not video_on and now - video_off_at > 100 us then
               fail("video packet after the stop");
            end if;
            seq   := u16(buf(50), buf(51));
            frm   := u16(buf(52), buf(53));
            line  := u16(buf(54), buf(55)) mod 32768;
            lastf := buf(55)(7);
            if u16(buf(56), buf(57)) /= 384 or buf(58) /= x"04" or buf(59) /= x"04" or
               u16(buf(60), buf(61)) /= 0 then
               fail("video format fields");
            end if;
            if vseq < 0 then
               if seq /= 0 or frm /= 0 or line /= 0 then
                  fail("first video packet: seq " & integer'image(seq) & " frame " &
                       integer'image(frm) & " line " & integer'image(line));
               end if;
            else
               if seq /= (vseq + 1) mod 65536 then
                  fail("video seq " & integer'image(seq) & " after " & integer'image(vseq));
               end if;
               if line = 0 then
                  if frm /= vframe + 1 or vline /= 268 then
                     fail("video frame " & integer'image(frm) & " after " & integer'image(vframe));
                  end if;
               elsif frm /= vframe or line /= vline + 4 then
                  fail("video line " & integer'image(line) & " after " & integer'image(vline));
               end if;
            end if;
            if (lastf = '1') /= (line = 268) then
               fail("last flag at line " & integer'image(line));
            end if;
            if line = 0 and frm = 0 then
               vbase := vic_frame;                  -- the model's frame this one started in
            end if;
            if frm = 1 then
               rx_vframe1 <= true;
            end if;
            -- Every pixel: line l of the packet is raster 16 + line + l
            for l in 0 to 3 loop
               for x in 0 to 191 loop
                  b    := to_integer(unsigned(buf(62 + l * 192 + x)));
                  want := pixel(2 * x, 16 + line + l, vbase + frm) +
                          16 * pixel(2 * x + 1, 16 + line + l, vbase + frm);
                  if b /= want then
                     fail("pixel x " & integer'image(2 * x) & " line " & integer'image(line + l) &
                          ": " & integer'image(b) & ", want " & integer'image(want));
                     exit;
                  end if;
               end loop;
            end loop;
            vseq   := seq;
            vframe := frm;
            vline  := line;
            rx_video <= rx_video + 1;

         elsif is_hdr(buf(8 to 49), C_AHDR) then
            -- Audio: a sequence number, 192 frames of L, R
            if n - 12 /= 42 + 2 + 768 then
               fail("audio frame length " & integer'image(n - 12));
            end if;
            seq := u16(buf(50), buf(51));
            if aseq >= 0 and seq /= 0 and seq /= aseq + 1 then
               fail("audio seq " & integer'image(seq) & " after " & integer'image(aseq));
            end if;
            if seq = 0 then
               lastl := -1;                          -- a restart: no sample before
            end if;
            for s in 0 to 191 loop
               d := u16(buf(52 + 4 * s), buf(53 + 4 * s));
               if u16(buf(54 + 4 * s), buf(55 + 4 * s)) /= 65535 - d then
                  fail("audio right sample " & integer'image(s));
               end if;
               -- A sample every 9856/15 = 657.07 main clocks
               if lastl >= 0 and (d - lastl) mod 65536 /= 657 and (d - lastl) mod 65536 /= 658 then
                  fail("audio sample spacing " & integer'image((d - lastl) mod 65536));
               end if;
               lastl := d;
            end loop;
            aseq := seq;
            rx_audio_seq <= seq;
            rx_audio     <= rx_audio + 1;

         else
            -- The firmware's frame, whole
            for i in 0 to C_FW_LEN - 1 loop
               if i < 6 then
                  want := 16#F0#;
               else
                  want := (fwn + i) mod 256;
               end if;
               if to_integer(unsigned(buf(8 + i))) /= want then
                  fail("firmware frame " & integer'image(fwn) & " byte " & integer'image(i));
                  exit;
               end if;
            end loop;
            if n - 12 /= C_FW_LEN then
               fail("firmware frame length " & integer'image(n - 12));
            end if;
            fwn    := fwn + 1;
            rx_fw  <= fwn;
         end if;
      end if;
   end process wire_proc;

   ---------------------------------------------------------------------------------------------
   -- A second streamer whose output stalls for 3 ms: groups that find no free slot are
   -- dropped and counted, and their sequence numbers skipped
   ---------------------------------------------------------------------------------------------

   i_stall : entity work.stream_tx
      port map (
         main_clk_i  => main_clk,
         vic_ce_i    => vic_ce,
         vic_index_i => vic_index,
         vic_x_i     => vic_x,
         vic_y_i     => vic_y,
         audio_l_i   => audio_l,
         audio_r_i   => audio_r,
         clk_i       => clk,
         rst_i       => rst,
         enable_i    => "01",
         busy_o      => open,
         hdr_we_i    => '0',
         hdr_addr_i  => (others => '0'),
         hdr_data_i  => (others => '0'),
         vsent_o     => open,
         asent_o     => open,
         vdrop_o     => s2_vdrop,
         adrop_o     => open,
         m_valid_o   => s2_valid,
         m_ready_i   => s2_ready,
         m_data_o    => s2_data,
         m_last_o    => s2_last
      ); -- i_stall

   stall_ready_proc : process
   begin
      wait for 12 ms;
      s2_ready <= '0';
      wait for 3 ms;
      s2_ready <= '1';
      wait;
   end process stall_ready_proc;

   stall_proc : process
      variable k    : natural := 0;
      variable lo   : std_logic_vector(7 downto 0);
      variable seq  : natural;
      variable prev : integer := -1;
   begin
      wait until rising_edge(clk);
      if s2_valid = '1' and s2_ready = '1' then
         if k = 42 then
            lo := s2_data;
         elsif k = 43 then
            seq := to_integer(unsigned(std_logic_vector'(s2_data & lo)));
            if prev >= 0 then
               if seq <= prev then
                  report "stalled stream: seq " & integer'image(seq) & " after " &
                         integer'image(prev) severity error;
                  s2_errors <= s2_errors + 1;
               else
                  s2_gaps <= s2_gaps + seq - prev - 1;
               end if;
            end if;
            prev := seq;
         end if;
         if s2_last = '1' then
            if k /= 42 + 12 + 768 - 1 then
               report "stalled stream: packet of " & integer'image(k + 1) severity error;
               s2_errors <= s2_errors + 1;
            end if;
            k := 0;
            s2_packets <= s2_packets + 1;
         else
            k := k + 1;
         end if;
      end if;
   end process stall_proc;

end architecture sim;
