----------------------------------------------------------------------------------
-- C128 for MEGA65
--
-- Testbench for the disk-image mount path: vdrives -> iec_drive.
--
-- The QNICE side writes the image size and the 2-bit image type into vdrives and
-- then strobes "image mounted", exactly like VD_STROBE_IM in M2M/rom/vdrives.asm.
-- vdrives crosses those signals into the core clock domain, main.vhd expands the
-- 2-bit type into iec_drive's {img_hd, img_mfm, img_gcr, img_ds} and iec_drive
-- latches the result to pick a drive model.
--
-- The ROM bank that iec_drive requests is the observable proof of which model it
-- picked: with DRIVES=1 bank 0 is the 1541, bank 1 the 1571 and bank 2 the 1581.
-- Holding rom_wr low keeps rom_valid low, so rom_req stays asserted and rom_addr
-- shows the selected bank continuously.
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

library work;
use work.vdrives_pkg.all;

entity tb_vdrive_mount is
end entity tb_vdrive_mount;

architecture sim of tb_vdrive_mount is

   constant C_VDNUM     : integer := 1;

   -- Leave iec_drive out and check the vdrives side on its own. Setting this to true
   -- currently stalls xsim at time 0: c157x_multi and c1581_multi cross-couple their IEC
   -- lines through the combinational iecdrv_reset_filter, and the undefined start-up
   -- values there never resolve. The full core testbenches drive those lines from real
   -- traffic and do not hit it.
   constant C_WITH_DRIVE : boolean := false;

   -- The real core: QNICE runs at 50 MHz, the C128 core clock at 31.5 MHz.
   constant C_QNICE_PER : time := 20.000 ns;
   constant C_CORE_PER  : time := 31.718 ns;

   -- Image types as produced by PREP_LOAD_IMAGE in CORE/m2m-rom/m2m-rom.asm
   constant C_IMGTYPE_D64 : integer := 0;
   constant C_IMGTYPE_D71 : integer := 1;
   constant C_IMGTYPE_D81 : integer := 2;

   -- ROM banks for DRIVES=1
   constant C_BANK_1541 : integer := 0;
   constant C_BANK_1571 : integer := 1;
   constant C_BANK_1581 : integer := 2;

   signal clk_qnice    : std_logic := '0';
   signal clk_core     : std_logic := '0';
   signal reset_core_n : std_logic := '0';
   signal sim_done     : boolean   := false;

   -- vdrives -> core
   signal img_mounted  : std_logic_vector(C_VDNUM - 1 downto 0);
   signal img_readonly : std_logic;
   signal img_size     : std_logic_vector(31 downto 0);
   signal img_type     : std_logic_vector(1 downto 0);
   signal img_type4    : std_logic_vector(3 downto 0);
   signal drive_mounted: std_logic_vector(C_VDNUM - 1 downto 0);
   signal cache_dirty  : std_logic_vector(C_VDNUM - 1 downto 0);

   -- SD card side
   signal sd_lba          : vd_vec_array(C_VDNUM - 1 downto 0)(31 downto 0);
   signal sd_blk_cnt      : vd_vec_array(C_VDNUM - 1 downto 0)( 5 downto 0);
   signal sd_rd           : vd_std_array(C_VDNUM - 1 downto 0);
   signal sd_wr           : vd_std_array(C_VDNUM - 1 downto 0);
   signal sd_ack          : vd_std_array(C_VDNUM - 1 downto 0);
   signal sd_buf_addr     : std_logic_vector(13 downto 0);
   signal sd_buf_addr16   : std_logic_vector(15 downto 0);
   signal sd_buf_data_in  : std_logic_vector( 7 downto 0);
   signal sd_buf_data_out : vd_vec_array(C_VDNUM - 1 downto 0)(7 downto 0);
   signal sd_buf_wr       : std_logic;

   -- drive control / status
   signal drives_reset : std_logic_vector(C_VDNUM - 1 downto 0);
   signal drv_mode     : vd_vec_array(C_VDNUM - 1 downto 0)(1 downto 0);
   signal drive_led    : std_logic_vector(C_VDNUM - 1 downto 0);
   signal out_track    : vd_vec_array(C_VDNUM - 1 downto 0)(7 downto 0);
   signal out_we       : std_logic_vector(C_VDNUM - 1 downto 0);

   -- DOS ROM handover
   signal rom_req      : std_logic;
   signal rom_addr     : std_logic_vector(18 downto 0);

   -- QNICE MMIO
   signal qnice_addr : std_logic_vector(27 downto 0) := (others => '0');
   signal qnice_data : std_logic_vector(15 downto 0) := (others => '0');
   signal qnice_dout : std_logic_vector(15 downto 0);
   signal qnice_ce   : std_logic := '0';
   signal qnice_we   : std_logic := '0';

   -- Glue check: iec_drive presents FDC LBA<<1 to vdrives (BLKSZ=1). Stock ST
   -- T40 S0 is FDC LBA 780 → 1560. HANDLE_DRV_RD must see window 97, offset 2048.
   signal tb_sd_lba : vd_vec_array(C_VDNUM - 1 downto 0)(31 downto 0) :=
      (others => (others => '0'));

   signal errors : integer := 0;

   -- Core clock domain monitor: how wide is the mount pulse that vdrives produces,
   -- and what does img_type look like while it is asserted?
   signal core_pulse_cur  : integer := 0;
   signal core_pulse_last : integer := 0;
   signal core_type_at    : std_logic_vector(1 downto 0) := "00";

   -- QNICE clock domain monitor: this is exactly what iec_drive.sv does, i.e.
   --    always @(posedge clk_sys) if (img_mounted[i] && img_size) {img_hd, ...} = img_type;
   -- on signals that vdrives produces in the *core* clock domain.
   signal qclk_samples : integer := 0;
   signal qclk_latched : std_logic_vector(3 downto 0) := "0000";
   signal samples_before : integer := 0;

   -- rom_addr is undriven while the drive is in reset, so the bank has to be read
   -- defensively: -1 means "not a clean binary value".
   function bank_of(addr : std_logic_vector) return integer is
      variable slice : std_logic_vector(3 downto 0) := addr(18 downto 15);
   begin
      for i in slice'range loop
         if slice(i) /= '0' and slice(i) /= '1' then
            return -1;
         end if;
      end loop;
      return to_integer(unsigned(slice));
   end function;

   -- iec_drive tests "img_size" for non-zero. Before the first mount the register is
   -- still uninitialised, which must not count as a valid size.
   function is_clean_nonzero(v : std_logic_vector) return boolean is
      variable any_one : boolean := false;
   begin
      for i in v'range loop
         if v(i) = '1' then
            any_one := true;
         elsif v(i) /= '0' then
            return false;
         end if;
      end loop;
      return any_one;
   end function;

   function bank_name(b : integer) return string is
   begin
      case b is
         when C_BANK_1541 => return "1541";
         when C_BANK_1571 => return "1571";
         when C_BANK_1581 => return "1581";
         when others      => return "unknown";
      end case;
   end function;

begin

   clk_qnice <= not clk_qnice after C_QNICE_PER / 2 when not sim_done else '0';
   clk_core  <= not clk_core  after C_CORE_PER  / 2 when not sim_done else '0';

   ---------------------------------------------------------------------------
   -- Device under test: the same wiring as CORE/vhdl/main.vhd
   ---------------------------------------------------------------------------

   -- A mounted drive is released from reset, an unmounted one is held in reset.
   -- Written as an explicit compare rather than main.vhd's "or not", so that the
   -- uninitialised drive_mounted at time 0 yields a clean '1' instead of 'U'.
   drives_reset(0) <= '0' when reset_core_n = '1' and drive_mounted(0) = '1' else '1';

   -- The menu selects the 5.25" model; "10" is the 1571 default from config.vhd.
   drv_mode(0) <= "10";

   sd_buf_addr16 <= std_logic_vector(resize(unsigned(sd_buf_addr), 16));

   with img_type select img_type4 <=
      "0010" when "00",     -- D64: GCR, single sided
      "0011" when "01",     -- D71: GCR, double sided
      "1000" when "10",     -- D81: HD 3.5"
      "0010" when others;

   vdrives_inst : entity work.vdrives
      generic map (
         VDNUM => C_VDNUM,
         BLKSZ => 1
      )
      port map (
         clk_qnice_i      => clk_qnice,
         clk_core_i       => clk_core,
         reset_core_i     => not reset_core_n,

         img_mounted_o    => img_mounted,
         img_readonly_o   => img_readonly,
         img_size_o       => img_size,
         img_type_o       => img_type,

         drive_mounted_o  => drive_mounted,

         cache_dirty_o    => cache_dirty,
         cache_flushing_o => open,

         sd_lba_i         => sd_lba,
         sd_blk_cnt_i     => sd_blk_cnt,
         sd_rd_i          => sd_rd,
         sd_wr_i          => sd_wr,
         sd_ack_o         => sd_ack,

         sd_buff_addr_o   => sd_buf_addr,
         sd_buff_dout_o   => sd_buf_data_in,
         sd_buff_din_i    => sd_buf_data_out,
         sd_buff_wr_o     => sd_buf_wr,

         qnice_addr_i     => qnice_addr,
         qnice_data_i     => qnice_data,
         qnice_data_o     => qnice_dout,
         qnice_ce_i       => qnice_ce,
         qnice_we_i       => qnice_we
      );

   no_drive_gen : if not C_WITH_DRIVE generate
      rom_req  <= '0';
      rom_addr <= (others => '0');
      sd_lba          <= tb_sd_lba;
      sd_blk_cnt      <= (others => (others => '0'));
      sd_rd           <= (others => '0');
      sd_wr           <= (others => '0');
      sd_buf_data_out <= (others => (others => '0'));
      drive_led       <= (others => '0');
      out_track       <= (others => (others => '0'));
      out_we          <= (others => '0');
   end generate no_drive_gen;

   drive_gen : if C_WITH_DRIVE generate
   iec_drive_inst : entity work.iec_drive
      generic map (
         PARPORT => 0,
         DRIVES  => C_VDNUM
      )
      port map (
         clk          => clk_core,
         ce           => '0',          -- the drive CPU is not needed for this test
         reset        => drives_reset,
         pause        => '0',

         drv_mode     => drv_mode,

         iec_atn_i    => '1',
         iec_clk_i    => '1',
         iec_data_i   => '1',
         iec_fclk_i   => '1',
         iec_clk_o    => open,
         iec_data_o   => open,
         iec_fclk_o   => open,

         img_mounted  => img_mounted,
         img_readonly => img_readonly,
         img_size     => img_size,
         img_type     => img_type4,

         led          => drive_led,
         disk_ready   => open,
         out_track    => out_track,
         out_we       => out_we,

         par_data_i   => x"FF",
         par_stb_i    => '1',
         par_data_o   => open,
         par_stb_o    => open,

         clk_sys      => clk_qnice,

         sd_lba       => sd_lba,
         sd_blk_cnt   => sd_blk_cnt,
         sd_rd        => sd_rd,
         sd_wr        => sd_wr,
         sd_ack       => sd_ack,
         sd_buff_addr => sd_buf_addr16,
         sd_buff_dout => sd_buf_data_in,
         sd_buff_din  => sd_buf_data_out,
         sd_buff_wr   => sd_buf_wr,

         rom_loading  => '0',
         rom_req      => rom_req,
         rom_addr     => rom_addr,
         rom_data     => x"00",
         rom_wr       => '0'           -- never completes, so rom_req stays asserted
      );
   end generate drive_gen;

   ---------------------------------------------------------------------------
   -- Stimulus: replay VD_STROBE_IM from M2M/rom/vdrives.asm
   ---------------------------------------------------------------------------

   core_monitor : process(clk_core)
   begin
      if rising_edge(clk_core) then
         if img_mounted(0) = '1' then
            if core_pulse_cur = 0 then
               core_type_at <= img_type;
            end if;
            core_pulse_cur <= core_pulse_cur + 1;
         elsif core_pulse_cur /= 0 then
            core_pulse_last <= core_pulse_cur;
            core_pulse_cur  <= 0;
         end if;
      end if;
   end process core_monitor;

   qnice_monitor : process(clk_qnice)
   begin
      if rising_edge(clk_qnice) then
         if img_mounted(0) = '1' and is_clean_nonzero(img_size) then
            qclk_latched <= img_type4;
            qclk_samples <= qclk_samples + 1;
         end if;
      end if;
   end process qnice_monitor;

   -- Heartbeat, so that a hung simulation can be told apart from a slow one.
   heartbeat : process
   begin
      while not sim_done loop
         wait for 500 ns;
         report "heartbeat at " & time'image(now) &
                ": rom_req=" & std_logic'image(rom_req) &
                " bank=" & integer'image(bank_of(rom_addr))
            severity note;
      end loop;
      wait;
   end process heartbeat;

   stimulus : process

      procedure qnice_write(reg : integer; val : integer) is
      begin
         wait until rising_edge(clk_qnice);
         qnice_addr <= std_logic_vector(to_unsigned(reg, 28));
         qnice_data <= std_logic_vector(to_unsigned(val, 16));
         qnice_ce   <= '1';
         qnice_we   <= '1';
         wait until rising_edge(clk_qnice);
         qnice_ce   <= '0';
         qnice_we   <= '0';
         -- VD_CAD_WRITE is a subroutine call, so consecutive register writes are
         -- many QNICE cycles apart.
         for i in 0 to 9 loop
            wait until rising_edge(clk_qnice);
         end loop;
      end procedure;

      -- VD_STROBE_IM: set size / read-only / type, then pulse "image mounted"
      procedure mount(size : integer; img : integer) is
      begin
         -- the latch fires while the strobe is still being issued, so the baseline
         -- for "did it fire?" has to be taken before the sequence starts
         samples_before <= qclk_samples;
         qnice_write(2, size mod 65536);          -- img_size low word
         qnice_write(3, size / 65536);            -- img_size high word
         qnice_write(1, 0);                       -- read/write
         qnice_write(4, img);                     -- image type
         qnice_write(0, 1);                       -- img_mounted: set for drive 0
         qnice_write(0, 0);                       -- img_mounted: clear
      end procedure;

      procedure check_mount(expected_type4 : std_logic_vector(3 downto 0);
                            expected_bank  : integer;
                            what           : string) is
         variable got  : integer;
         variable seen : integer;
      begin
         seen := samples_before;
         -- give the mount time to cross into the core domain and settle
         for i in 0 to 199 loop
            wait until rising_edge(clk_qnice);
         end loop;

         report "  vdrives: mount pulse was " & integer'image(core_pulse_last) &
                " core clocks wide, img_type in the core domain = """ &
                to_string(core_type_at) & """" severity note;
         report "  iec_drive's clk_sys latch fired " & integer'image(qclk_samples - seen) &
                " time(s), latched {img_hd,img_mfm,img_gcr,img_ds} = """ &
                to_string(qclk_latched) & """" severity note;

         if qclk_samples = seen then
            errors <= errors + 1;
            report "FAIL (" & what & "): iec_drive never sampled the mount pulse, " &
                   "so the drive model was not updated at all"
               severity error;
         elsif qclk_latched /= expected_type4 then
            errors <= errors + 1;
            report "FAIL (" & what & "): latched """ & to_string(qclk_latched) &
                   """, expected """ & to_string(expected_type4) & """"
               severity error;
         else
            report "PASS (" & what & "): latched """ & to_string(qclk_latched) & """"
               severity note;
         end if;

         if C_WITH_DRIVE then
            got := bank_of(rom_addr);
            if got /= expected_bank then
               errors <= errors + 1;
               report "FAIL (" & what & "): drive requested ROM bank " & integer'image(got) &
                      " (" & bank_name(got) & "), expected bank " & integer'image(expected_bank) &
                      " (" & bank_name(expected_bank) & ")"
                  severity error;
            else
               report "PASS (" & what & "): drive requested ROM bank " & integer'image(got) &
                      " (" & bank_name(got) & ")"
                  severity note;
            end if;
         end if;
      end procedure;

      procedure qnice_read(addr : integer; variable val : out integer) is
      begin
         wait until rising_edge(clk_qnice);
         qnice_addr <= std_logic_vector(to_unsigned(addr, 28));
         qnice_ce   <= '1';
         qnice_we   <= '0';
         wait until rising_edge(clk_qnice);
         val := to_integer(unsigned(qnice_dout));
         qnice_ce   <= '0';
      end procedure;

   begin
      reset_core_n <= '0';
      for i in 0 to 99 loop
         wait until rising_edge(clk_qnice);
      end loop;
      reset_core_n <= '1';
      for i in 0 to 99 loop
         wait until rising_edge(clk_qnice);
      end loop;

      -- 819200 bytes = a .D81, which must turn the drive into a 1581
      report "--- mounting a .D81 (819200 bytes, image type 2) ---" severity note;
      mount(819200, C_IMGTYPE_D81);
      check_mount("1000", C_BANK_1581, "D81 -> 1581");

      -- Glue: vdrives window math for the T40 header (H33 address).
      -- FDC LBA 780, iec_drive <<1 → 1560, BLKSZ=1 → byte 399360 = win 97 + off 2048.
      report "--- vdrives 4k window for iec LBA 1560 (T40 S0) ---" severity note;
      tb_sd_lba(0) <= std_logic_vector(to_unsigned(1560, 32));
      for i in 0 to 9 loop
         wait until rising_edge(clk_qnice);
      end loop;
      declare
         variable win : integer;
         variable offs : integer;
         -- #region agent log
         file dbg : text open append_mode is "/home/bazzite/Dokumente/Developer/c128Mega65/.cursor/debug-36b09c.log";
         variable l : line;
         -- #endregion
      begin
         qnice_read(16#001006#, win);
         qnice_read(16#001007#, offs);
         report "  vdrives: LBA 1560 -> 4k win=" & integer'image(win) &
                " offs=" & integer'image(offs) severity note;
         -- #region agent log
         write(l, string'("{""sessionId"":""36b09c"",""runId"":""glue"",""hypothesisId"":""G1"",""location"":""tb_vdrive_mount.vhd:lba_window"",""message"":""vdrives window for iec LBA 1560"",""data"":{""win"":"));
         write(l, win);
         write(l, string'(",""offs"":"));
         write(l, offs);
         write(l, string'(",""expect_win"":97,""expect_offs"":2048},""timestamp"":0}"));
         writeline(dbg, l);
         -- #endregion
         if win /= 97 or offs /= 2048 then
            errors <= errors + 1;
            report "FAIL: HANDLE_DRV_RD window/offset wrong for T40 header"
               severity error;
         else
            report "PASS: vdrives maps LBA 1560 to window 97 offset 2048"
               severity note;
         end if;
      end;

      -- 174848 bytes = a 35 track .D64, which must stay on the menu's 5.25" model
      report "--- mounting a .D64 (174848 bytes, image type 0) ---" severity note;
      mount(174848, C_IMGTYPE_D64);
      check_mount("0010", C_BANK_1571, "D64 -> 1571");

      -- and back to the 1581, to prove the switch works in both directions
      report "--- mounting a .D81 again ---" severity note;
      mount(819200, C_IMGTYPE_D81);
      check_mount("1000", C_BANK_1581, "D81 -> 1581 (second time)");

      -- Soft reset must keep the drive mounted. Clearing drive_mounted_reg on
      -- reset_core left the emulated drive permanently in reset (DEVICE NOT PRESENT).
      report "--- soft reset must keep drive mounted ---" severity note;
      -- #region agent log
      declare
         file dbg : text open append_mode is "/home/bazzite/Dokumente/Developer/c128Mega65/.cursor/debug-36b09c.log";
         variable l : line;
      begin
         write(l, string'("{""sessionId"":""36b09c"",""runId"":""post-fix"",""hypothesisId"":""H7"",""location"":""tb_vdrive_mount.vhd:before_soft_reset"",""message"":""mounted before soft reset"",""data"":{""drive_mounted"":"));
         write(l, drive_mounted(0) = '1');
         write(l, string'("},""timestamp"":0}"));
         writeline(dbg, l);
      end;
      -- #endregion
      reset_core_n <= '0';
      for i in 0 to 99 loop
         wait until rising_edge(clk_core);
      end loop;
      reset_core_n <= '1';
      for i in 0 to 99 loop
         wait until rising_edge(clk_core);
      end loop;
      if drive_mounted(0) /= '1' then
         report "FAIL: soft reset cleared drive_mounted (would cause DEVICE NOT PRESENT)"
            severity error;
         errors := errors + 1;
      else
         report "PASS: drive_mounted survived soft reset" severity note;
      end if;
      -- #region agent log
      declare
         file dbg : text open append_mode is "/home/bazzite/Dokumente/Developer/c128Mega65/.cursor/debug-36b09c.log";
         variable l : line;
      begin
         write(l, string'("{""sessionId"":""36b09c"",""runId"":""post-fix"",""hypothesisId"":""H7"",""location"":""tb_vdrive_mount.vhd:after_soft_reset"",""message"":""mounted after soft reset"",""data"":{""drive_mounted"":"));
         write(l, drive_mounted(0) = '1');
         write(l, string'("},""timestamp"":0}"));
         writeline(dbg, l);
      end;
      -- #endregion

      if errors = 0 then
         report "=== tb_vdrive_mount: ALL CHECKS PASSED ===" severity note;
      else
         report "=== tb_vdrive_mount: " & integer'image(errors) & " CHECK(S) FAILED ==="
            severity failure;
      end if;

      sim_done <= true;
      wait;
   end process stimulus;

end architecture sim;
