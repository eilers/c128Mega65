-------------------------------------------------------------------------------
-- tb_c1581_track.vhd
--
-- The internal floppy at disk speed. Our c1581_fdc1772 in physical mode, the
-- real physical_1581_controller with its two FIFOs, and C64MEGA65's rotating
-- disk model (CORE/sim/physical_1581: 300 RPM, a formatted DD disk with known
-- contents). A drive CPU model reads one track side the way the 1581 DOS fills
-- its track cache: Read Address, then a Read Sector for each sector in disk
-- order from the one after it, with the ROM's busy-first loop and G_GAP_US
-- between sectors (the DOS needs about 66 us).
--
-- Checks: every byte against the disk model, clean status, no LOST DATA, and
-- the track has to come in at real 1581 speed: one record (19.3 ms) per sector
-- plus slack. A sector caught one revolution late adds 190 ms and fails.
-------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.golden_1581_pkg.all;

entity tb_c1581_track is
  generic (
    G_SECTORS : natural := 10;
    G_GAP_US  : natural := 70
  );
end entity tb_c1581_track;

architecture sim of tb_c1581_track is

  signal clk          : std_logic := '0';   -- 50 MHz, aligned with the disk model's timebase
  signal clkcpu       : std_logic := '0';   -- 32 MHz drive clock
  signal ce_cnt       : unsigned(1 downto 0) := "00";
  signal clk8m_en     : std_logic := '0';
  signal rst          : std_logic := '1';   -- QNICE-side reset
  signal floppy_reset : std_logic := '0';   -- drive reset, active low

  -- drive CPU bus
  signal cpu_sel      : std_logic := '0';
  signal cpu_rw       : std_logic := '1';
  signal cpu_addr     : std_logic_vector(1 downto 0) := "00";
  signal cpu_din      : std_logic_vector(7 downto 0) := x"00";
  signal cpu_dout     : std_logic_vector(7 downto 0);
  signal floppy_ready : std_logic;
  signal floppy_step  : std_logic;
  signal irq, drq     : std_logic;
  signal disk_present : std_logic;

  -- unused image side
  signal sd_lba       : std_logic_vector(31 downto 0);
  signal sd_rd, sd_wr : std_logic_vector(0 downto 0);
  signal sd_din       : std_logic_vector(7 downto 0);
  signal out_track    : std_logic_vector(7 downto 0);
  signal out_we       : std_logic;

  -- fdc1772 <-> controller
  signal p_active, p_motor, p_side            : std_logic;
  signal p_step_req, p_step_out, p_step_ack   : std_logic;
  signal p_rd_req, p_rd_side, p_rd_cancel     : std_logic;
  signal p_rd_op                              : std_logic_vector(2 downto 0);
  signal p_rd_track, p_rd_sector              : std_logic_vector(7 downto 0);
  signal p_rd_seq, p_rd_done_seq              : std_logic_vector(1 downto 0);
  signal p_rd_done, p_rd_crc, p_rd_rnf, p_rd_del : std_logic;
  signal p_rd_result                          : std_logic_vector(4 downto 0);
  signal p_rd_c, p_rd_h, p_rd_r, p_rd_n       : unsigned(7 downto 0);
  signal p_media_ready, p_index, p_track0     : std_logic;
  signal p_wprot, p_change, p_motor_on, p_settled : std_logic;
  signal p_head_cyl                           : unsigned(7 downto 0);
  signal p_locked                             : std_logic;

  -- read FIFO and write FIFO
  signal rf_wr, rf_full, rf_rd, rf_empty      : std_logic;
  signal rf_wdata, rf_rdata                   : unsigned(7 downto 0);
  signal wf_wr, wf_full, wf_rd, wf_empty      : std_logic;
  signal wf_wdata, wf_rdata                   : std_logic_vector(7 downto 0);
  signal w_deleted, w_precomp                 : std_logic;
  signal w_feed, w_lost, w_busy, w_ok         : std_logic;

  -- fdc1772 diagnostics
  signal d_lost, d_drain, d_stale, d_busycmd, d_fin : std_logic;
  signal d_wr_fin, d_wr_wp, d_wr_drop               : std_logic;
  signal d_pres_cnt                                 : std_logic_vector(10 downto 0);

  -- floppy connector
  signal f_rdata, f_index, f_track0, f_wprot, f_change : std_logic;
  signal f_motora, f_selecta, f_side1, f_stepdir, f_step, f_density : std_logic;
  signal f_wgate, f_wdata                           : std_logic;

  signal lost_events : natural := 0;
  signal done_sim    : boolean := false;

begin

  clk    <= not clk after 10 ns;
  clkcpu <= not clkcpu after 15.625 ns;

  -- 8 MHz WD enable: one clkcpu in four, as the 1581 divider makes it
  ce_proc : process (clkcpu)
  begin
    if rising_edge(clkcpu) then
      ce_cnt   <= ce_cnt + 1;
      clk8m_en <= '1' when ce_cnt = "11" else '0';
    end if;
  end process ce_proc;

  i_fdc : entity work.c1581_fdc1772
    generic map (
      CLK_EN    => 8000,
      FD_NUM    => 1,
      MODEL     => 2,
      EXT_MOTOR => 1,
      IMG_TYPE  => 1
    )
    port map (
      clkcpu         => clkcpu,
      clk_sys        => clk,
      clk8m_en       => clk8m_en,
      floppy_drive   => "0",
      floppy_side    => '1',            -- ~PA0: logical side 0
      floppy_reset   => floppy_reset,
      floppy_step    => floppy_step,
      floppy_motor   => '1',            -- PA2: motor on
      floppy_ready   => floppy_ready,
      irq            => irq,
      drq            => drq,
      cpu_addr       => cpu_addr,
      cpu_sel        => cpu_sel,
      cpu_rw         => cpu_rw,
      cpu_din        => cpu_din,
      cpu_dout       => cpu_dout,
      img_mounted    => "0",
      img_wp         => "0",
      img_ds         => '0',
      img_size       => (others => '0'),
      sd_lba         => sd_lba,
      sd_rd          => sd_rd,
      sd_wr          => sd_wr,
      sd_ack         => '0',
      sd_buff_addr   => (others => '0'),
      sd_dout        => (others => '0'),
      sd_din         => sd_din,
      sd_dout_strobe => '0',
      out_track      => out_track,
      out_we         => out_we,
      disk_present   => disk_present,

      phys_mode              => '1',
      phys_active            => p_active,
      phys_cia_motor_on      => p_motor,
      phys_cia_side          => p_side,
      phys_step_req_tgl      => p_step_req,
      phys_step_outward      => p_step_out,
      phys_rd_req_tgl        => p_rd_req,
      phys_rd_op             => p_rd_op,
      phys_rd_track          => p_rd_track,
      phys_rd_side           => p_rd_side,
      phys_rd_sector         => p_rd_sector,
      phys_rd_cancel_tgl     => p_rd_cancel,
      phys_rd_seq            => p_rd_seq,
      phys_step_ack_tgl      => p_step_ack,
      phys_rd_done_tgl       => p_rd_done,
      phys_rd_done_seq       => p_rd_done_seq,
      phys_rd_result         => p_rd_result,
      phys_rd_crc_err        => p_rd_crc,
      phys_rd_rnf            => p_rd_rnf,
      phys_rd_deleted        => p_rd_del,
      phys_rd_c              => std_logic_vector(p_rd_c),
      phys_byte_rd_en        => rf_rd,
      phys_byte_data         => std_logic_vector(rf_rdata),
      phys_byte_empty        => rf_empty,
      phys_media_ready       => p_media_ready,
      phys_index             => p_index,
      phys_track0            => p_track0,
      phys_wprot             => p_wprot,
      phys_change            => p_change,
      phys_motor_on          => p_motor_on,
      phys_head_settled      => p_settled,
      phys_wr_deleted        => w_deleted,
      phys_wr_precomp        => w_precomp,
      phys_wr_byte_wr        => wf_wr,
      phys_wr_byte_data      => wf_wdata,
      phys_wr_fifo_full      => wf_full,
      phys_wr_feed           => w_feed,
      phys_wr_lost           => w_lost,
      phys_wr_busy           => w_busy,
      phys_dbg_lost_tgl      => d_lost,
      phys_dbg_drain_tgl     => d_drain,
      phys_dbg_staledone_tgl => d_stale,
      phys_dbg_busycmd_tgl   => d_busycmd,
      phys_dbg_fin_tgl       => d_fin,
      phys_dbg_pres_cnt      => d_pres_cnt,
      phys_dbg_wr_fin_tgl    => d_wr_fin,
      phys_dbg_wr_wp_tgl     => d_wr_wp,
      phys_dbg_wr_drop_tgl   => d_wr_drop
    ); -- i_fdc

  i_ctrl : entity work.physical_1581_controller
    port map (
      clk_i             => clk,
      rst_i             => rst,
      f_rdata_i         => f_rdata,
      f_index_i         => f_index,
      f_track0_i        => f_track0,
      f_writeprotect_i  => f_wprot,
      f_diskchanged_i   => f_change,
      f_motora_o        => f_motora,
      f_selecta_o       => f_selecta,
      f_side1_o         => f_side1,
      f_stepdir_o       => f_stepdir,
      f_step_o          => f_step,
      f_density_o       => f_density,
      f_wgate_o         => f_wgate,
      f_wdata_o         => f_wdata,
      phys_active_i     => p_active,
      cia_motor_on_i    => p_motor,
      cia_side_i        => p_side,
      step_req_tgl_i    => p_step_req,
      step_outward_i    => p_step_out,
      step_ack_tgl_o    => p_step_ack,
      rd_req_tgl_i      => p_rd_req,
      rd_op_i           => p_rd_op,
      rd_track_i        => unsigned(p_rd_track),
      rd_side_i         => p_rd_side,
      rd_sector_i       => unsigned(p_rd_sector),
      rd_seq_i          => p_rd_seq,
      rd_cancel_tgl_i   => p_rd_cancel,
      rd_done_tgl_o     => p_rd_done,
      rd_done_seq_o     => p_rd_done_seq,
      rd_result_o       => p_rd_result,
      rd_crc_err_o      => p_rd_crc,
      rd_rnf_o          => p_rd_rnf,
      rd_deleted_o      => p_rd_del,
      rd_c_o            => p_rd_c,
      rd_h_o            => p_rd_h,
      rd_r_o            => p_rd_r,
      rd_n_o            => p_rd_n,
      byte_data_o       => rf_wdata,
      byte_wr_o         => rf_wr,
      byte_ovf_i        => rf_full,
      wr_deleted_i      => w_deleted,
      wr_precomp_i      => w_precomp,
      wbyte_data_i      => wf_rdata,
      wbyte_empty_i     => wf_empty,
      wbyte_rd_o        => wf_rd,
      wr_feed_o         => w_feed,
      wr_lost_o         => w_lost,
      wr_busy_o         => w_busy,
      wr_ok_o           => w_ok,
      st_media_ready_o  => p_media_ready,
      st_index_o        => p_index,
      st_track0_o       => p_track0,
      st_wprot_o        => p_wprot,
      st_change_o       => p_change,
      st_motor_on_o     => p_motor_on,
      st_head_settled_o => p_settled,
      st_head_cyl_o     => p_head_cyl,
      st_locked_o       => p_locked
    ); -- i_ctrl

  i_rdfifo : entity work.physical_1581_rdfifo
    generic map (G_AW => 9)
    port map (
      wr_clk_i   => clk,    wr_rst_i => rst, wr_en_i => rf_wr, wr_data_i => rf_wdata,
      wr_full_o  => rf_full, wr_level_o => open,
      rd_clk_i   => clkcpu, rd_rst_i => rst, rd_en_i => rf_rd, rd_data_o => rf_rdata,
      rd_empty_o => rf_empty
    );

  i_wfifo : entity work.physical_1581_wfifo
    generic map (G_DW => 8, G_AW => 2)
    port map (
      wr_clk_i   => clkcpu, wr_rst_i => rst, wr_en_i => wf_wr, wr_data_i => wf_wdata,
      wr_full_o  => wf_full, wr_level_o => open,
      rd_clk_i   => clk,    rd_rst_i => rst, rd_en_i => wf_rd, rd_data_o => wf_rdata,
      rd_empty_o => wf_empty, rd_level_o => open
    );

  i_disk : entity work.mech_model_1581
    -- the read-only generator: the same records as a real track (19.3 ms each,
    -- a 30-byte gap where a real 1581 format has 35). The writable flux store
    -- trips an xsim array-bound error, and reads do not need it.
    generic map (G_INDEX_PERIOD => 200 ms, G_INDEX_LOW => 2 ms, G_NUM_CYL => 80,
                 G_WRITABLE => false)
    port map (
      f_motora_i       => f_motora,
      f_selecta_i      => f_selecta,
      f_side1_i        => f_side1,
      f_stepdir_i      => f_stepdir,
      f_step_i         => f_step,
      f_density_i      => f_density,
      cfg_present_i    => '1',
      cfg_wprot_i      => '0',
      cfg_change_i     => '0',
      f_index_o        => f_index,
      f_track0_o       => f_track0,
      f_writeprotect_o => f_wprot,
      f_diskchanged_o  => f_change,
      f_rdata_o        => f_rdata,
      f_wgate_i        => f_wgate,
      f_wdata_i        => f_wdata
    );

  -- an unconsumed byte overwritten by the next one
  lost_proc : process (clkcpu)
    variable prev : std_logic := '0';
  begin
    if rising_edge(clkcpu) then
      if d_lost /= prev then
        lost_events <= lost_events + 1;
      end if;
      prev := d_lost;
    end if;
  end process lost_proc;

  watchdog : process
  begin
    wait for 3 sec;
    assert done_sim report "tb_c1581_track: TIMEOUT" severity failure;
    wait;
  end process watchdog;

  stim : process
    type t_buf is array (0 to 1023) of std_logic_vector(7 downto 0);
    variable buf    : t_buf;
    variable cnt    : natural;
    variable st, d  : std_logic_vector(7 downto 0);
    variable errors : natural := 0;
    variable r0, s  : natural;
    variable t0, ts : time;
    variable limit  : time;

    procedure cycles(n : natural) is
    begin
      for i in 1 to n loop
        wait until rising_edge(clkcpu);
      end loop;
    end procedure;

    -- one 2 MHz T65 bus cycle is 16 clkcpu: the select stays up for all of it,
    -- a write strobes R/W low in its last clkcpu (ph2_f), a read latches at the end
    procedure bus_write(a : std_logic_vector(1 downto 0); v : std_logic_vector(7 downto 0)) is
    begin
      wait until rising_edge(clkcpu);
      cpu_sel <= '1'; cpu_rw <= '1'; cpu_addr <= a; cpu_din <= v;
      cycles(15);
      cpu_rw <= '0';
      cycles(1);
      cpu_sel <= '0'; cpu_rw <= '1';
    end procedure;

    procedure bus_read(a : std_logic_vector(1 downto 0); v : out std_logic_vector(7 downto 0)) is
    begin
      wait until rising_edge(clkcpu);
      cpu_sel <= '1'; cpu_rw <= '1'; cpu_addr <= a;
      cycles(16);
      v := cpu_dout;
      cpu_sel <= '0';
    end procedure;

    -- CPU cycles of instructions around the bus accesses
    procedure cpu_cycles(n : natural) is
    begin
      cycles(16 * n);
    end procedure;

    -- after a command write, the ROM waits for BUSY to rise ($CBF4)
    procedure wait_busy_set is
    begin
      for i in 1 to 200 loop
        bus_read("00", st);
        exit when st(0) = '1';
        cpu_cycles(6);
      end loop;
      assert st(0) = '1' report "BUSY never rose after a command" severity failure;
    end procedure;

    -- the ROM transfer loop ($CD17): BUSY first, DRQ second, a byte per DRQ,
    -- about 13 CPU cycles per poll and 15 more per byte at 2 MHz
    procedure rom_loop(n : out natural) is
      variable k     : natural := 0;
      variable polls : natural := 0;
    begin
      loop
        cpu_cycles(3);  bus_read("00", st);  cpu_cycles(9);
        exit when st(0) = '0';
        if st(1) = '1' then
          cpu_cycles(3);  bus_read("11", d);  cpu_cycles(11);
          if k < 1024 then buf(k) := d; end if;
          k := k + 1;
        end if;
        polls := polls + 1;
        assert polls < 500_000 report "ROM loop runaway" severity failure;
      end loop;
      n := k;
    end procedure;

  begin
    rst <= '1'; floppy_reset <= '0';
    wait for 2 us;
    rst <= '0';
    wait for 2 us;
    floppy_reset <= '1';

    -- motor on (PA2) from the start; the DOS waits for RDY (PA1)
    ts := now;
    wait until floppy_ready = '1' for 1500 ms;
    assert floppy_ready = '1' report "RDY never came" severity failure;
    report "RDY " & time'image(now - ts) & " after motor on";

    -- Restore: the head is already on track 0
    bus_write("00", x"00");
    wait_busy_set;
    loop
      cpu_cycles(10); bus_read("00", st);
      exit when st(0) = '0';
    end loop;
    bus_read("01", d);
    assert d = x"00" report "track register not 0 after Restore" severity failure;

    -- Read Address: which sector is passing under the head
    cpu_cycles(130);
    bus_write("00", x"C0");
    wait_busy_set;
    rom_loop(cnt);
    assert cnt = 6 report "Read Address returned " & integer'image(cnt) & " bytes" severity failure;
    assert buf(0) = x"00" and buf(1) = x"00" and buf(3) = x"02"
      report "unexpected Read Address reply" severity failure;
    r0 := to_integer(unsigned(buf(2)));
    report "Read Address: sector " & integer'image(r0);

    -- the track: each sector in disk order from the next one
    t0 := now;
    for i in 1 to G_SECTORS loop
      s  := ((r0 + i - 1) mod 10) + 1;
      ts := now;
      wait for G_GAP_US * 1 us;
      bus_write("10", std_logic_vector(to_unsigned(s, 8)));
      bus_write("00", x"80");
      wait_busy_set;
      rom_loop(cnt);
      bus_read("00", st);                  -- the ROM's status epilogue ($CD3F)
      if cnt /= 512 then
        errors := errors + 1;
        report "sector " & integer'image(s) & ": " & integer'image(cnt) & " bytes" severity error;
      end if;
      for k in 0 to 511 loop
        if k < cnt and unsigned(buf(k)) /= golden_payload(0, 0, s, k) then
          errors := errors + 1;
          report "sector " & integer'image(s) & " byte " & integer'image(k) & " wrong" severity error;
          exit;
        end if;
      end loop;
      if st(4 downto 2) /= "000" then
        errors := errors + 1;
        report "sector " & integer'image(s) & ": status " & to_hstring(st) severity error;
      end if;
      report "sector " & integer'image(s) & " in " & time'image(now - ts);
    end loop;

    -- one record is 604 bytes of 32 us; a missed sector costs a revolution
    limit := 60 ms + G_SECTORS * 20 ms;
    if now - t0 > limit then
      errors := errors + 1;
      report "track took " & time'image(now - t0) & ", limit " & time'image(limit) severity error;
    end if;
    if lost_events /= 0 then
      errors := errors + 1;
      report integer'image(lost_events) & " LOST DATA events" severity error;
    end if;

    done_sim <= true;
    if errors = 0 then
      report "==== tb_c1581_track: PASS, " & integer'image(G_SECTORS) & " sectors in " &
             time'image(now - t0) & " ====";
    else
      report "==== tb_c1581_track: FAIL, " & integer'image(errors) & " error(s), " &
             time'image(now - t0) & " ====" severity failure;
    end if;
    std.env.finish;
  end process stim;

end architecture sim;
