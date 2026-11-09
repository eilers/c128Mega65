----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- cmdhd_drive: the CMD HD's controller board, core clock. Hardware as VICE models it
-- (src/drive/iec/cmdhd.c), except where the boot ROM shows the real part (the 8255).
--
--   CPU  6502 at 2 MHz (T65; the boot ROM and HD DOS 1.92 use no 65C02 instruction)
--   $0000-$3FFF  RAM
--   $4000-$7FFF  RAM: the $4000 page, or the $C000 page when 8255 PC1 is 0
--   $8000-$81FF  VIA U10: IEC on port B, fast serial on the shift register, ATN on CA1
--   $8400-$85FF  VIA U9: SCSI data on port A (CA2 pulses ACK), SCSI status on port B
--   $8800-$89FF  8255 U11: A parallel cable data, B buttons + cable, C SCSI BSY/RST/ATN,
--                RAM window (PC1), boot ROM (PC0), cable handshake
--   $8C00-$8DFF  RTC-72421
--   $8F00-$8FFF  RAM; writes also go to latch U20: LEDs (bits 0, 1 low lit), SCSI parity
--                (bit 4), RAM writes above $8000 (bit 5)
--   other $8xxx, $9000-$BFFF  RAM, written only while U20 bit 5 is set ($8Exx always)
--   $C000-$FFFF  boot ROM while PC0 is set, else RAM (written only with U20 bit 5)
--
-- No parallel cable is attached: the 8255 reads the lines as VICE's cmdbus does with
-- nothing on it (PATN, PCLK, PREADY inactive, data $FF). The port is left out here so that a
-- RAMLink can join later.
--
-- Done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.T65_Pack.all;

entity cmdhd_drive is
generic (
   G_CLK_HZ : natural := 31_528_000
);
port (
   clk_i         : in  std_logic;
   rst_i         : in  std_logic;                    -- held in reset
   ce_i          : in  std_logic;                    -- 16 MHz
   pause_i       : in  std_logic;

   -- IEC, line levels (1 = released)
   iec_atn_i     : in  std_logic;
   iec_clk_i     : in  std_logic;
   iec_data_i    : in  std_logic;
   iec_fclk_i    : in  std_logic;
   iec_clk_o     : out std_logic;
   iec_data_o    : out std_logic;
   iec_fclk_o    : out std_logic;

   -- front panel (1 = pressed / lit)
   btn_wp_i      : in  std_logic;
   btn_swap8_i   : in  std_logic;
   btn_swap9_i   : in  std_logic;
   led_act_o     : out std_logic;
   led_err_o     : out std_logic;

   rtc_i         : in  std_logic_vector(64 downto 0);

   -- the boot ROM (dual-clock RAM, read data one clock after the address)
   rom_addr_o    : out std_logic_vector(13 downto 0);
   rom_data_i    : in  std_logic_vector(7 downto 0);

   -- power-up: clears the RAM while the drive is held in reset
   clear_i       : in  std_logic;                    -- a rising edge starts it
   clear_busy_o  : out std_logic;
   clear_tgl_o   : out std_logic;                    -- flips when it is done

   -- the SCSI buffer and the firmware's side of the bus (see cmdhd_scsi)
   buf_addr_o    : out std_logic_vector(10 downto 0);
   buf_we_o      : out std_logic;
   buf_wdata_o   : out std_logic_vector(7 downto 0);
   buf_rdata_i   : in  std_logic_vector(7 downto 0);
   cmd_tgl_o     : out std_logic;
   cdb_len_o     : out std_logic_vector(3 downto 0);
   done_tgl_o    : out std_logic;
   free_tgl_o    : out std_logic;
   scsi_busy_o   : out std_logic;
   go_tgl_i      : in  std_logic;
   go_phase_i    : in  std_logic_vector(1 downto 0);
   go_count_i    : in  std_logic_vector(10 downto 0);
   go_base_i     : in  std_logic_vector(9 downto 0);
   go_status_i   : in  std_logic_vector(7 downto 0);

   pc_o          : out std_logic_vector(15 downto 0)  -- the CPU's program counter (monitor)
);
end entity cmdhd_drive;

architecture synthesis of cmdhd_drive is

   -- 2 MHz from the 16 MHz enable, as c1581_multi does
   signal div          : unsigned(2 downto 0) := (others => '0');
   signal ena, ena1    : std_logic := '0';
   signal ph2_r, ph2_f : std_logic := '0';

   -- CPU
   signal cpu_a        : std_logic_vector(23 downto 0);
   signal cpu_do       : std_logic_vector(7 downto 0);
   signal cpu_di       : std_logic_vector(7 downto 0);
   signal cpu_rw       : std_logic;
   signal cpu_irq_n    : std_logic;
   signal cpu_regs     : std_logic_vector(63 downto 0);
   signal a            : unsigned(15 downto 0);

   -- decode
   signal via10_cs, via9_cs, ppi_cs, rtc_cs, rom_cs : std_logic;
   signal ram_addr     : unsigned(15 downto 0);
   signal ram_we_cpu   : std_logic;

   -- RAM
   type t_ram is array (0 to 65535) of std_logic_vector(7 downto 0);
   signal ram          : t_ram := (others => (others => '0'));
   signal ram_a        : unsigned(15 downto 0);
   signal ram_we       : std_logic;
   signal ram_wd       : std_logic_vector(7 downto 0);
   signal ram_q        : std_logic_vector(7 downto 0);
   signal clr_addr     : unsigned(16 downto 0) := (others => '1');   -- bit 16 set: done
   signal clr_d        : std_logic := '0';
   signal clr_tgl      : std_logic := '0';

   -- latch U20
   signal u20          : std_logic_vector(7 downto 0) := x"00";

   -- VIA U10: IEC
   signal v10_do, v10_pa_o, v10_pa_t, v10_pb_o, v10_pb_t, v10_pb_i : std_logic_vector(7 downto 0);
   signal v10_cb1_o, v10_cb1_t, v10_cb2_o, v10_cb2_t, v10_irq : std_logic;
   signal v10_cb1, v10_cb2, fast_dir : std_logic;

   -- VIA U9: SCSI
   signal v9_do, v9_pa_o, v9_pa_t, v9_pa_i, v9_pb_o, v9_pb_t, v9_pb_i : std_logic_vector(7 downto 0);
   signal v9_ca2_o, v9_ca2_t, v9_cb2_o, v9_cb2_t, v9_irq : std_logic;
   signal ca2_d        : std_logic := '1';
   signal ack          : std_logic;

   -- 8255
   signal ppi_do, ppi_pa_o, ppi_pb_o, ppi_pc_o, ppi_pb_i : std_logic_vector(7 downto 0);
   signal ppi_we       : std_logic;

   -- RTC
   signal rtc_do       : std_logic_vector(3 downto 0);
   signal rtc_we       : std_logic;

   -- SCSI
   signal s_sel, s_bsy, s_req, s_io : std_logic;
   signal s_phase      : std_logic_vector(2 downto 0);
   signal s_dbus_i, s_dbus_o : std_logic_vector(7 downto 0);
   signal pb5          : std_logic;

begin

   ---------------------------------------------------------------------------------------
   -- clocks
   ---------------------------------------------------------------------------------------

   p_div : process (clk_i)
   begin
      if rising_edge(clk_i) then
         ena1  <= not pause_i;
         if div(1 downto 0) /= "00" then
            ena <= ena1;
         end if;
         ph2_r <= '0';
         ph2_f <= '0';
         if ce_i = '1' then
            div   <= div + 1;
            if div(1 downto 0) = "00" then
               ph2_r <= ena and not div(2);
               ph2_f <= ena and div(2);
            end if;
         end if;
      end if;
   end process p_div;

   ---------------------------------------------------------------------------------------
   -- CPU and decode
   ---------------------------------------------------------------------------------------

   i_cpu : entity work.T65
      port map (
         Mode    => "00",
         Res_n   => not rst_i,
         Enable  => ph2_r,
         Clk     => clk_i,
         IRQ_n   => cpu_irq_n,
         R_W_n   => cpu_rw,
         A       => cpu_a,
         DI      => cpu_di,
         DO      => cpu_do,
         Regs    => cpu_regs,
         Sync    => open,
         EF      => open,
         MF      => open,
         XF      => open,
         ML_n    => open,
         VP_n    => open,
         VDA     => open,
         VPA     => open,
         DEBUG   => open,
         NMI_ack => open
      );

   cpu_irq_n <= not (v10_irq or v9_irq);
   a         <= unsigned(cpu_a(15 downto 0));
   pc_o      <= cpu_regs(63 downto 48);

   via10_cs <= '1' when a(15 downto 9) = "1000000" else '0';      -- $8000-$81FF
   via9_cs  <= '1' when a(15 downto 9) = "1000010" else '0';      -- $8400-$85FF
   ppi_cs   <= '1' when a(15 downto 9) = "1000100" else '0';      -- $8800-$89FF
   rtc_cs   <= '1' when a(15 downto 9) = "1000110" else '0';      -- $8C00-$8DFF
   rom_cs   <= '1' when a(15 downto 14) = "11" and ppi_pc_o(0) = '1' else '0';

   -- the RAM address: $4000-$7FFF is a window onto the $4000 or the $C000 page
   ram_addr <= ("01" & a(13 downto 0)) when a(15 downto 14) = "01" and ppi_pc_o(1) = '1' else
               ("11" & a(13 downto 0)) when a(15 downto 14) = "01" else
               a;

   -- RAM writes, on the CPU's write cycle (ph2_f, as the 1581)
   p_ram_we : process (all)
   begin
      ram_we_cpu <= '0';
      if ph2_f = '1' and cpu_rw = '0' then
         if a(15 downto 14) = "00" or a(15 downto 14) = "01" then
            ram_we_cpu <= '1';
         elsif a(15 downto 12) = x"8" then
            if a(11 downto 8) = x"E" or a(11 downto 8) = x"F" then
               ram_we_cpu <= '1';
            elsif via10_cs = '0' and via9_cs = '0' and ppi_cs = '0' and rtc_cs = '0' then
               ram_we_cpu <= u20(5);
            end if;
         else
            ram_we_cpu <= u20(5);
         end if;
      end if;
   end process p_ram_we;

   -- RAM: the CPU, or the power-up clear
   ram_a  <= clr_addr(15 downto 0) when clr_addr(16) = '0' else ram_addr;
   ram_we <= '1' when clr_addr(16) = '0' else ram_we_cpu;
   ram_wd <= x"00" when clr_addr(16) = '0' else cpu_do;
   clear_busy_o <= not clr_addr(16);

   p_ram : process (clk_i)
   begin
      if rising_edge(clk_i) then
         if ram_we = '1' then
            ram(to_integer(ram_a)) <= ram_wd;
         end if;
         ram_q <= ram(to_integer(ram_a));
      end if;
   end process p_ram;

   p_clear : process (clk_i)
   begin
      if rising_edge(clk_i) then
         clr_d <= clear_i;
         if clr_addr(16) = '0' then
            clr_addr <= clr_addr + 1;
            if clr_addr(15 downto 0) = x"FFFF" then
               clr_tgl <= not clr_tgl;
            end if;
         elsif clear_i = '1' and clr_d = '0' and rst_i = '1' then
            clr_addr <= (others => '0');
         end if;
      end if;
   end process p_clear;
   clear_tgl_o <= clr_tgl;

   rom_addr_o <= cpu_a(13 downto 0);

   cpu_di <= cpu_do                              when cpu_rw = '0' else
             v10_do                              when via10_cs = '1' else
             v9_do                               when via9_cs = '1' else
             ppi_do                              when ppi_cs = '1' else
             "1111" & rtc_do                     when rtc_cs = '1' else
             rom_data_i                          when rom_cs = '1' else
             ram_q;

   -- latch U20, on writes to $8Fxx
   p_u20 : process (clk_i)
   begin
      if rising_edge(clk_i) then
         if ph2_f = '1' and cpu_rw = '0' and a(15 downto 8) = x"8F" then
            u20 <= cpu_do;
         end if;
         if rst_i = '1' then
            u20 <= x"00";                        -- (VICE's power-up value)
         end if;
      end if;
   end process p_u20;

   led_act_o <= not u20(0);
   led_err_o <= not u20(1);

   ---------------------------------------------------------------------------------------
   -- VIA U10: IEC and fast serial (as VICE: the 1581's IEC wiring on a VIA's port B)
   ---------------------------------------------------------------------------------------

   fast_dir <= v10_pb_o(5) or not v10_pb_t(5);
   v10_cb1  <= v10_cb1_o or not v10_cb1_t;
   v10_cb2  <= v10_cb2_o or not v10_cb2_t;

   iec_clk_o  <= not (v10_pb_o(3) and v10_pb_t(3));
   iec_data_o <= not (v10_pb_o(1) and v10_pb_t(1)) and
                 not (v10_pb_o(4) and v10_pb_t(4) and not iec_atn_i) and
                 (not fast_dir or v10_cb2);
   iec_fclk_o <= not fast_dir or v10_cb1;

   -- PB0 data, PB2 clock, PB7 ATN (high = asserted); PB5 and PB6 read low (VICE)
   v10_pb_i <= (not iec_atn_i) & "00" & '1' & '1' & (not iec_clk_i) & '1' & (not iec_data_i);

   i_via10 : entity work.iecdrv_via6522
      port map (
         clock    => clk_i,
         rising   => ph2_f,
         falling  => ph2_r,
         reset    => rst_i,
         addr     => cpu_a(3 downto 0),
         wen      => via10_cs and not cpu_rw,
         ren      => via10_cs and cpu_rw,
         data_in  => cpu_do,
         data_out => v10_do,
         phi2_ref => open,
         port_a_o => v10_pa_o,
         port_a_t => v10_pa_t,
         port_a_i => v10_pa_o or not v10_pa_t,
         port_b_o => v10_pb_o,
         port_b_t => v10_pb_t,
         port_b_i => v10_pb_i and (v10_pb_o or not v10_pb_t),
         ca1_i    => iec_atn_i,                   -- the ATN line itself: it falls as ATN comes (VICE)
         ca2_o    => open,
         ca2_i    => '1',
         ca2_t    => open,
         cb1_o    => v10_cb1_o,
         cb1_i    => iec_fclk_i and v10_cb1,
         cb1_t    => v10_cb1_t,
         cb2_o    => v10_cb2_o,
         cb2_i    => iec_data_i and v10_cb2,
         cb2_t    => v10_cb2_t,
         irq      => v10_irq
      );

   ---------------------------------------------------------------------------------------
   -- VIA U9 and the SCSI disk
   ---------------------------------------------------------------------------------------

   -- the data lines are low active: the disk's byte inverted while it drives them, else the
   -- controller's own outputs (pulled up where port A is an input)
   v9_pa_i  <= not s_dbus_o when s_io = '1' else (v9_pa_o or not v9_pa_t);
   s_dbus_i <= not (v9_pa_o or not v9_pa_t);
   s_sel    <= v9_pb_o(4) and v9_pb_t(4);

   -- U13: PB5 is BSY and SEL while CB2 is high, BSY and not SEL while it is low
   pb5 <= (s_bsy and s_sel) when (v9_cb2_o or not v9_cb2_t) = '1' else (s_bsy and not s_sel);
   v9_pb_i <= s_req & '0' & pb5 & s_sel & (v9_pb_o(3) and v9_pb_t(3)) & s_phase;

   -- ACK: the start of each CA2 pulse
   p_ack : process (clk_i)
   begin
      if rising_edge(clk_i) then
         ca2_d <= v9_ca2_o or not v9_ca2_t;
      end if;
   end process p_ack;
   ack <= ca2_d and not (v9_ca2_o or not v9_ca2_t);

   i_via9 : entity work.iecdrv_via6522
      port map (
         clock    => clk_i,
         rising   => ph2_f,
         falling  => ph2_r,
         reset    => rst_i,
         addr     => cpu_a(3 downto 0),
         wen      => via9_cs and not cpu_rw,
         ren      => via9_cs and cpu_rw,
         data_in  => cpu_do,
         data_out => v9_do,
         phi2_ref => open,
         port_a_o => v9_pa_o,
         port_a_t => v9_pa_t,
         port_a_i => v9_pa_i,
         port_b_o => v9_pb_o,
         port_b_t => v9_pb_t,
         port_b_i => v9_pb_i,
         ca1_i    => '1',
         ca2_o    => v9_ca2_o,
         ca2_i    => '1',
         ca2_t    => v9_ca2_t,
         cb1_o    => open,
         cb1_i    => '1',
         cb1_t    => open,
         cb2_o    => v9_cb2_o,
         cb2_i    => '1',
         cb2_t    => v9_cb2_t,
         irq      => v9_irq
      );

   i_scsi : entity work.cmdhd_scsi
      port map (
         clk_i       => clk_i,
         rst_i       => rst_i,
         sel_i       => s_sel,
         rst_bus_i   => ppi_pc_o(3),
         ack_i       => ack,
         dbus_i      => s_dbus_i,
         bsy_o       => s_bsy,
         req_o       => s_req,
         io_o        => s_io,
         phase_o     => s_phase,
         dbus_o      => s_dbus_o,
         buf_addr_o  => buf_addr_o,
         buf_we_o    => buf_we_o,
         buf_wdata_o => buf_wdata_o,
         buf_rdata_i => buf_rdata_i,
         cmd_tgl_o   => cmd_tgl_o,
         cdb_len_o   => cdb_len_o,
         done_tgl_o  => done_tgl_o,
         free_tgl_o  => free_tgl_o,
         busy_o      => scsi_busy_o,
         go_tgl_i    => go_tgl_i,
         go_phase_i  => go_phase_i,
         go_count_i  => go_count_i,
         go_base_i   => go_base_i,
         go_status_i => go_status_i
      );

   ---------------------------------------------------------------------------------------
   -- 8255 U11 and the RTC
   ---------------------------------------------------------------------------------------

   ppi_we <= ppi_cs and ph2_f and not cpu_rw;

   -- port B: PATN (7) and PCLK (6) and PREADY (0) as the cable reads with nothing on it
   -- (PCLK and PREADY are this drive's own outputs, PC6 and PC7, read back inverted);
   -- WP (3), SWAP 9 (2), SWAP 8 (1) low while pressed
   ppi_pb_i <= '0' & (not ppi_pc_o(6)) & "11" & (not btn_wp_i) & (not btn_swap9_i) &
               (not btn_swap8_i) & (not ppi_pc_o(7));

   i_ppi : entity work.cmdhd_i8255
      port map (
         clk_i    => clk_i,
         rst_i    => rst_i,
         we_i     => ppi_we,
         addr_i   => cpu_a(1 downto 0),
         data_i   => cpu_do,
         data_o   => ppi_do,
         pa_i     => x"FF",
         pb_i     => ppi_pb_i,
         pc_i     => ppi_pc_o,
         pa_o     => ppi_pa_o,
         pb_o     => ppi_pb_o,
         pc_o     => ppi_pc_o,
         pa_out_o => open
      );

   rtc_we <= rtc_cs and ph2_f and not cpu_rw;

   i_rtc : entity work.cmdhd_rtc
      generic map (
         G_CLK_HZ => G_CLK_HZ
      )
      port map (
         clk_i  => clk_i,
         rtc_i  => rtc_i,
         we_i   => rtc_we,
         addr_i => cpu_a(3 downto 0),
         data_i => cpu_do(3 downto 0),
         data_o => rtc_do
      );

end architecture synthesis;
