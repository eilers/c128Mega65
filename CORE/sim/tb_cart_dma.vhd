----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Testbench for a hardware cartridge as bus master (/DMA), such as the Ultimate II+
-- whose menu freezes the machine and draws itself through DMA.
--
-- main boots as in tb_c128_boot. The cartridge model runs on its own 50 MHz clock and
-- follows the Ultimate's slot_master_v4/slot_timing: it pulls /DMA at a PHI2 fall,
-- drives address and R/W while PHI2 is high, write data from 100 ns in, reads the data
-- just before PHI2 falls, waits for BA high before a read, and leaves the third cycle
-- in a row with BA low to the VIC. The connector lines are resolved with pull-ups, so
-- both sides driving a line shows up as 'X'. Before the boot it holds RESET until it
-- has seen PHI2 run, as a Kung Fu Flash 2 does. It also answers the Ultimate's command
-- interface at $DF1C-$DF1F, which the 8502 reads and writes from /NMI code. Then the 8502
-- reads external function ROM, and /ROML-/ROMH must stay out of PHI1. Last, with GAME low the
-- VIC fetches $3000-$3FFF from ROMH, which a slave timed as the Ultimate's must serve.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

library work;
use work.globals.all;
use work.sim_support_pkg.all;
use work.boot_paths_pkg.all;

entity tb_cart_dma is
end entity tb_cart_dma;

architecture sim of tb_cart_dma is

   constant C_BOOT0_SIZE   : natural := 73728;
   constant C_MENU_HOLD    : time    := 500 us;
   constant C_MAIN_PERIOD  : time    := 1 sec / CORE_CLK_SPEED;
   constant C_QNICE_PERIOD : time    := 20 ns;
   constant C_CART_PERIOD  : time    := 20 ns;   -- the U2+L's 50 MHz
   constant C_ADDR_VALID   : natural := 4;       -- the Ultimate's "CPU Addr valid after PHI2/PHI1" default
   constant C_MEM_LAT      : natural := 8;       -- Ultimax slave: address sample to data
   constant C_BURST        : natural := 2048;
   constant C_BURST_BASE   : natural := 16#2000#;

   signal clk_main         : std_logic := '0';
   signal clk_qnice        : std_logic := '0';
   signal cart_clk         : std_logic := '0';
   signal reset_hard       : std_logic := '1';

   signal ram_addr         : unsigned(17 downto 0);
   signal ram_data_to      : unsigned(7 downto 0);
   signal ram_we           : std_logic;
   signal ram_data_from    : std_logic_vector(7 downto 0);
   signal ram_a_addr       : std_logic_vector(17 downto 0) := (others => '0');
   signal ram_a_q          : std_logic_vector(7 downto 0);
   signal sys_rom_addr     : std_logic_vector(16 downto 0);
   signal sys_rom_data     : std_logic_vector(7 downto 0);
   signal qnice_sysrom_addr: std_logic_vector(16 downto 0) := (others => '0');
   signal sysrom_data_a    : std_logic_vector(7 downto 0) := (others => '0');
   signal sysrom_we_a      : std_logic := '0';
   signal rom_mem          : byte_rom_t(0 to C_BOOT0_SIZE - 1) := (others => (others => '0'));
   signal rom_ready        : std_logic := '0';
   signal sysrom_ready     : std_logic := '0';
   signal boot_z80_n       : std_logic;

   -- What main drives towards the slot
   signal cart_phi2        : std_logic;
   signal cart_ctrl_oe     : std_logic;
   signal cart_ba_o        : std_logic;
   signal cart_rw_o        : std_logic;
   signal cart_addr_oe     : std_logic;
   signal cart_a_o         : unsigned(15 downto 0);
   signal cart_data_oe     : std_logic;
   signal cart_d_o         : unsigned(7 downto 0);
   -- What main reads: its own output where the level shifter points outwards (R6), else the connector
   signal in_rw            : std_logic;
   signal in_a             : unsigned(15 downto 0);
   signal in_d             : unsigned(7 downto 0);
   -- The connector: main's drivers, the cartridge's, and pull-ups
   signal x_dma            : std_logic;
   signal x_ba             : std_logic;
   signal x_rw             : std_logic;
   signal x_a              : std_logic_vector(15 downto 0);
   signal x_d              : std_logic_vector(7 downto 0);
   signal x_io2            : std_logic;
   signal cart_io2_o       : std_logic;
   signal cart_roml        : std_logic;
   signal cart_romh        : std_logic;
   signal tb_nmi_n         : std_logic := '1';
   signal tb_game_n        : std_logic := '1';

   -- Cartridge model
   type t_cart is (C_IDLE, C_STOPPED, C_CHECK, C_DO);
   type t_hist is array (0 to 2) of std_logic_vector(7 downto 0);
   signal c_state          : t_cart := C_IDLE;
   signal c_dma_n          : std_logic := '1';
   signal c_dma            : std_logic;
   signal c_rw             : std_logic;
   signal c_a              : std_logic_vector(15 downto 0);
   signal c_d              : std_logic_vector(7 downto 0);
   signal phi2_s           : std_logic_vector(1 downto 0) := "00";
   signal phi2_d           : std_logic := '0';
   signal phi2_rec         : std_logic := '0';
   signal ba_s             : std_logic_vector(1 downto 0) := "11";
   signal ba_hist          : std_logic_vector(2 downto 0) := "111";
   signal d_hist           : t_hist := (others => (others => '1'));
   signal phase_h          : natural range 0 to 63 := 63;
   signal phase_l          : natural range 0 to 63 := 63;
   signal d_out            : std_logic := '0';
   signal d_out_q          : std_logic := '0';
   signal vic_cyc          : boolean;
   signal drive_al         : boolean;
   signal cur_addr         : std_logic_vector(15 downto 0) := x"FFFF";
   signal cur_rw           : std_logic := '1';
   signal cur_data         : std_logic_vector(7 downto 0) := x"00";
   signal n_access         : natural := 0;
   signal n_warn_wr        : natural := 0;   -- writes done with BA low (the Ultimate allows two)
   signal n_ba_wait        : natural := 0;   -- reads held back by BA
   signal n_ba_lo          : natural := 0;   -- PHI2 rises at which the cart saw BA low

   -- I/O slave: the command interface at $DF1C-$DF1F, timed as the Ultimate's slot_slave.vhd
   constant C_SAMPLE_IO    : natural := 17;  -- its do_sample_io (c_write at 50 MHz), when writes are latched
   signal s_a              : t_hist := (others => (others => '1'));   -- (0 to 1): 2-stage sync of A15..A8
   signal s_al             : t_hist := (others => (others => '1'));   -- A7..A0
   signal s_rw             : std_logic_vector(1 downto 0) := "11";
   signal s_io2            : std_logic_vector(1 downto 0) := "11";
   signal s_d              : t_hist := (others => (others => '1'));
   signal s_addr           : std_logic_vector(15 downto 0);
   signal io_rd            : std_logic_vector(7 downto 0);
   signal io_wr_cond       : boolean := false;
   signal io_wr_addr       : std_logic_vector(15 downto 0) := x"0000";
   signal io_wr_data       : std_logic_vector(7 downto 0) := x"00";
   signal c_d_io           : std_logic_vector(7 downto 0);
   signal uci_ctl          : std_logic_vector(7 downto 0) := x"00";
   signal n_uci_wr         : natural := 0;

   -- Ultimax ROMH slave, timed as the Ultimate's slot_slave.vhd
   signal u_addr           : std_logic_vector(15 downto 0) := x"FFFF";
   signal u_wait           : natural range 0 to 63 := 0;
   signal u_dav            : boolean := false;
   signal c_d_umax         : std_logic_vector(7 downto 0);
   signal n_umax           : natural := 0;    -- VIC phases that fetched from ROMH
   signal n_umax_bad       : natural := 0;    -- ... and latched a byte that isn't the slave's for that address

   -- Test process <-> cartridge model (four-phase handshake on cart_clk)
   signal want_dma         : boolean := false;
   signal stopped          : boolean := false;
   signal req              : boolean := false;
   signal done             : boolean := false;
   signal req_rw           : std_logic := '1';
   signal req_addr         : std_logic_vector(15 downto 0) := x"FFFF";
   signal req_data         : std_logic_vector(7 downto 0) := x"00";
   signal rdata            : std_logic_vector(7 downto 0) := x"00";

   -- Monitors
   signal in_dma           : std_logic;
   signal core_released    : boolean := false;
   signal cart_rst_n       : std_logic := '1';
   signal cart_rst_free    : time := 0 ns;     -- when the cart let go of RESET
   signal ram_we_count     : natural := 0;
   signal rw_fight         : boolean;
   signal bus_fight        : boolean;
   signal n_rw_x           : natural := 0;
   signal rw_x_max         : time := 0 ns;
   signal n_bus_x          : natural := 0;
   signal n_tail           : natural := 0;    -- write tails: the I/O slave drives once PHI2 fell, /IO2 still low
   signal tail_max         : time := 0 ns;
   signal phi2_rise_t      : time := 0 ns;
   signal rd_settle_min    : time := 1 us;    -- how long DMA read data was on the connector as PHI2 fell
   signal rd_settle_ram    : time := 1 us;    -- the same for RAM, not $Dxxx
   signal n_rom_sel        : natural := 0;    -- /ROML or /ROMH accesses
   signal n_rom_phi1       : natural := 0;    -- main clocks with /ROML or /ROMH low well into PHI1

   -- What the Ultimax slave serves for an address
   function umax_byte(a : std_logic_vector(15 downto 0)) return std_logic_vector is
   begin
      return a(7 downto 0) xor a(15 downto 8) xor x"5A";
   end function;

begin

   clk_main  <= not clk_main  after C_MAIN_PERIOD / 2;
   clk_qnice <= not clk_qnice after C_QNICE_PERIOD / 2;
   cart_clk  <= not cart_clk  after C_CART_PERIOD / 2;

   u_main_ram : entity work.dualport_2clk_ram
      generic map (
         ADDR_WIDTH => 18,
         DATA_WIDTH => 8,
         FALLING_A  => true
      )
      port map (
         clock_a         => clk_qnice,
         address_a       => ram_a_addr,
         do_latch_addr_a => '0',
         data_a          => (others => '0'),
         wren_a          => '0',
         q_a             => ram_a_q,
         clock_b         => clk_main,
         address_b       => std_logic_vector(ram_addr),
         do_latch_addr_b => '0',
         data_b          => std_logic_vector(ram_data_to),
         wren_b          => ram_we,
         q_b             => ram_data_from
      );

   u_system_rom : entity work.dualport_2clk_ram
      generic map (
         ADDR_WIDTH => 17,
         DATA_WIDTH => 8,
         FALLING_A  => true
      )
      port map (
         clock_a         => clk_qnice,
         address_a       => qnice_sysrom_addr,
         do_latch_addr_a => '0',
         data_a          => sysrom_data_a,
         wren_a          => sysrom_we_a,
         q_a             => open,
         clock_b         => clk_main,
         address_b       => sys_rom_addr,
         do_latch_addr_b => '0',
         data_b          => (others => '0'),
         wren_b          => '0',
         q_b             => sys_rom_data
      );

   u_dut : entity work.main
      generic map (
         G_BOARD => "MEGA65_R6",
         G_VDNUM => 0
      )
      port map (
         clk_main_i           => clk_main,
         clk_vdc_i            => clk_main,
         clk_sd_i             => clk_qnice,
         qnice_vd_addr_i      => (others => '0'),
         qnice_vd_data_i      => (others => '0'),
         qnice_vd_data_o      => open,
         qnice_vd_ce_i        => '0',
         qnice_vd_we_i        => '0',
         drv_rom_loading_i    => '0',
         drv_rom_req_o        => open,
         drv_rom_addr_o       => open,
         drv_rom_data_i       => (others => '0'),
         drv_rom_wr_i         => '0',
         reset_soft_i         => '0',
         reset_hard_i         => reset_hard,
         pause_i              => '0',
         clk_main_speed_i     => CORE_CLK_SPEED,
         osm_control_i        => (others => '0'),   -- REU off, hardware slot
         f9_latched_o         => open,
         video_ce_o           => open,
         video_red_o          => open,
         video_green_o        => open,
         video_blue_o         => open,
         video_vs_o           => open,
         video_hs_o           => open,
         video_hblank_o       => open,
         video_vblank_o       => open,
         video_select_vdc_o   => open,
         vga_video_ce_o         => open,
         vga_video_ce_ovl_o     => open,
         vga_video_red_o        => open,
         vga_video_green_o      => open,
         vga_video_blue_o       => open,
         vga_video_vs_o         => open,
         vga_video_hs_o         => open,
         vga_video_hblank_o     => open,
         vga_video_vblank_o     => open,
         vga_video_select_vdc_o => open,
         audio_left_o         => open,
         audio_right_o        => open,
         drive_led_o          => open,
         drive_led_col_o      => open,
         boot_z80_n_o         => boot_z80_n,
         ram_addr_o           => ram_addr,
         ram_data_o           => ram_data_to,
         ram_we_o             => ram_we,
         ram_data_i           => unsigned(ram_data_from),
         sys_rom_addr_o       => sys_rom_addr,
         sys_rom_data_i       => sys_rom_data,
         cart_en_o            => open,
         cart_phi2_o          => cart_phi2,
         cart_dotclock_o      => open,
         cart_dma_i           => in_dma,
         cart_reset_oe_o      => open,
         cart_reset_i         => cart_rst_n,
         cart_reset_o         => open,
         cart_game_oe_o       => open,
         cart_game_i          => tb_game_n,
         cart_game_o          => open,
         cart_exrom_oe_o      => open,
         cart_exrom_i         => '1',
         cart_exrom_o         => open,
         cart_nmi_oe_o        => open,
         cart_nmi_i           => tb_nmi_n,
         cart_nmi_o           => open,
         cart_irq_oe_o        => open,
         cart_irq_i           => '1',
         cart_irq_o           => open,
         cart_roml_oe_o       => open,
         cart_roml_i          => '1',
         cart_roml_o          => cart_roml,
         cart_romh_oe_o       => open,
         cart_romh_i          => '1',
         cart_romh_o          => cart_romh,
         cart_ctrl_oe_o       => cart_ctrl_oe,
         cart_ba_i            => '1',
         cart_rw_i            => in_rw,
         cart_io1_i           => '1',
         cart_io2_i           => '1',
         cart_ba_o            => cart_ba_o,
         cart_rw_o            => cart_rw_o,
         cart_io1_o           => open,
         cart_io2_o           => cart_io2_o,
         cart_addr_oe_o       => cart_addr_oe,
         cart_a_i             => in_a,
         cart_a_o             => cart_a_o,
         cart_data_oe_o       => cart_data_oe,
         cart_d_i             => in_d,
         cart_d_o             => cart_d_o,
         iec_hardware_port_en_i => '0',
         iec_reset_n_o        => open,
         iec_atn_n_o          => open,
         iec_clk_en_o         => open,
         iec_clk_n_o          => open,
         iec_clk_n_i          => '0',
         iec_data_en_o        => open,
         iec_data_n_o         => open,
         iec_data_n_i         => '0',
         iec_srq_en_o         => open,
         iec_srq_n_o          => open,
         iec_srq_n_i          => '0',
         kb_key_num_i         => 0,
         kb_key_pressed_n_i   => '1',
         kb_key_enabled_i     => '1',
         joy_1_up_n_i         => '1',
         joy_1_down_n_i       => '1',
         joy_1_left_n_i       => '1',
         joy_1_right_n_i      => '1',
         joy_1_fire_n_i       => '1',
         joy_2_up_n_i         => '1',
         joy_2_down_n_i       => '1',
         joy_2_left_n_i       => '1',
         joy_2_right_n_i      => '1',
         joy_2_fire_n_i       => '1',
         pot1_x_i             => (others => '0'),
         pot1_y_i             => (others => '0'),
         pot2_x_i             => (others => '0'),
         pot2_y_i             => (others => '0')
      );

   --------------------------------------------------------------------------------
   -- Connector
   --------------------------------------------------------------------------------
   x_dma <= c_dma;
   x_dma <= 'H';
   x_ba  <= cart_ba_o when cart_ctrl_oe = '1' else 'Z';
   x_ba  <= 'H';
   x_rw  <= cart_rw_o when cart_ctrl_oe = '1' else 'Z';
   x_rw  <= c_rw;
   x_rw  <= 'H';
   x_a   <= std_logic_vector(cart_a_o) when cart_addr_oe = '1' else (others => 'Z');
   x_a   <= c_a;
   x_a   <= (others => 'H');
   x_d   <= std_logic_vector(cart_d_o) when cart_data_oe = '1' else (others => 'Z');
   x_d   <= c_d;
   x_d   <= c_d_io;
   x_d   <= c_d_umax;
   x_d   <= (others => 'H');
   x_io2 <= cart_io2_o when cart_ctrl_oe = '1' else 'Z';
   x_io2 <= 'H';

   in_dma <= to_x01(x_dma);
   in_rw <= cart_rw_o when cart_ctrl_oe = '1' else to_x01(x_rw);
   in_a  <= cart_a_o  when cart_addr_oe = '1' else unsigned(to_x01(x_a));
   in_d  <= cart_d_o  when cart_data_oe = '1' else unsigned(to_x01(x_d));

   --------------------------------------------------------------------------------
   -- Cartridge model, after the Ultimate's slot_master_v4.vhd and slot_timing.vhd
   --------------------------------------------------------------------------------
   vic_cyc  <= ba_hist = "000";
   drive_al <= c_dma_n = '0' and phi2_rec = '1' and not vic_cyc;
   c_dma    <= '0' when c_dma_n = '0' else 'Z';
   c_a      <= cur_addr when drive_al else (others => 'Z');
   c_rw     <= cur_rw   when drive_al else 'Z';
   c_d      <= cur_data when drive_al and cur_rw = '0' and d_out_q = '1' else (others => 'Z');

   p_cart : process (cart_clk)
      variable rise, fall : boolean;
   begin
      if rising_edge(cart_clk) then
         phi2_s <= phi2_s(0) & to_x01(cart_phi2);
         phi2_d <= phi2_s(1);
         ba_s   <= ba_s(0) & to_x01(x_ba);
         d_hist <= to_x01(x_d) & d_hist(0 to 1);
         rise   := phi2_s(1) = '1' and phi2_d = '0';
         fall   := phi2_s(1) = '0' and phi2_d = '1';

         if rise then
            if ba_s(1) = '0' and c_dma_n = '0' then
               n_ba_lo <= n_ba_lo + 1;
            end if;
            ba_hist  <= ba_hist(1 downto 0) & ba_s(1);
            phase_h  <= 0;
            phi2_rec <= '1';
         elsif phase_h /= 63 then
            phase_h <= phase_h + 1;
         end if;
         if fall then
            phase_l  <= 0;
            phi2_rec <= '0';
            d_out    <= '0';
         elsif phase_l /= 63 then
            phase_l <= phase_l + 1;
         end if;
         if phase_h = 3 and phi2_rec = '1' then
            d_out <= '1';
         end if;
         d_out_q <= d_out;   -- one clock later, for hold time

         case c_state is
            when C_IDLE =>
               if want_dma and fall then
                  c_dma_n <= '0';
                  stopped <= true;
                  c_state <= C_STOPPED;
               end if;

            when C_STOPPED =>
               cur_addr <= x"FFFF";
               cur_rw   <= '1';
               if not want_dma then
                  if fall then
                     c_dma_n <= '1';
                     stopped <= false;
                     c_state <= C_IDLE;
                  end if;
               elsif req and not done and phase_l = 19 then   -- prepare_dma, 400 ns into PHI1
                  cur_addr <= req_addr;
                  cur_rw   <= req_rw;
                  cur_data <= req_data;
                  c_state  <= C_CHECK;
               end if;

            when C_CHECK =>
               if phase_h = C_ADDR_VALID + 1 and phi2_rec = '1' then
                  if cur_rw = '1' and ba_s(1) = '1' then
                     c_state <= C_DO;
                  elsif cur_rw = '0' and not vic_cyc then
                     if ba_hist(0) = '0' then
                        n_warn_wr <= n_warn_wr + 1;
                     end if;
                     c_state <= C_DO;
                  elsif cur_rw = '1' then
                     n_ba_wait <= n_ba_wait + 1;
                  end if;
               end if;

            when C_DO =>
               if fall then
                  rdata    <= d_hist(1);   -- the bus just before PHI2 fell
                  done     <= true;
                  n_access <= n_access + 1;
                  cur_addr <= x"FFFF";
                  cur_rw   <= '1';
                  c_state  <= C_STOPPED;
               end if;
         end case;

         if done and not req then
            done <= false;
         end if;
      end if;
   end process p_cart;

   -- The command interface drives a read while its address and /IO2 are on the bus ($DF1D reads
   -- $C9), and takes a write to $DF1C if /IO2 is low at do_sample_io.
   s_addr <= s_a(1) & s_al(1);
   io_rd  <= x"C9" when s_addr(1 downto 0) = "01" else x"00";
   c_d_io <= io_rd when s_addr(15 downto 2) = x"DF1" & "11" and s_io2(1) = '0' and (s_rw(1) = '1' or phi2_s(1) = '0')
             else (others => 'Z');

   p_io : process (cart_clk)
   begin
      if rising_edge(cart_clk) then
         s_a   <= to_x01(x_a(15 downto 8)) & s_a(0 to 1);
         s_al  <= to_x01(x_a(7 downto 0)) & s_al(0 to 1);
         s_rw  <= s_rw(0) & to_x01(x_rw);
         s_io2 <= s_io2(0) & to_x01(x_io2);
         s_d   <= to_x01(x_d) & s_d(0 to 1);

         if phase_h = C_SAMPLE_IO and phi2_rec = '1' then
            io_wr_cond <= s_rw(1) = '0' and s_io2(1) = '0';
            io_wr_addr <= s_addr;
            io_wr_data <= s_d(1);
         end if;
         if phi2_s(1) = '0' and phi2_d = '1' then   -- do_io_event
            io_wr_cond <= false;
            if io_wr_cond and io_wr_addr = x"DF1C" then
               uci_ctl  <= io_wr_data;
               n_uci_wr <= n_uci_wr + 1;
            end if;
         end if;
      end if;
   end process p_io;

   -- The Ultimax slave samples the address once, C_ADDR_VALID ticks after it sees PHI2 fall ("CPU
   -- Addr valid after PHI1"), and drives the byte C_MEM_LAT ticks later while /ROMH is low, until
   -- it sees PHI2 rise.
   c_d_umax <= umax_byte(u_addr) when u_dav and cart_romh = '0' else (others => 'Z');

   p_umax : process (cart_clk)
   begin
      if rising_edge(cart_clk) then
         if phi2_s(1) = '1' and phi2_d = '0' then
            u_dav  <= false;
            u_wait <= 0;
         elsif phase_l = C_ADDR_VALID + 1 and phi2_rec = '0' then
            if tb_game_n = '0' and s_addr(15 downto 13) = "111" then
               u_addr <= s_addr;
               u_wait <= C_MEM_LAT;
            end if;
         elsif u_wait = 1 then
            u_dav  <= true;
            u_wait <= 0;
         elsif u_wait /= 0 then
            u_wait <= u_wait - 1;
         end if;
      end if;
   end process p_umax;

   --------------------------------------------------------------------------------
   -- Boot, as in tb_c128_boot
   --------------------------------------------------------------------------------
   rom_load_proc : process
   begin
      rom_mem   <= load_bin_rom(BOOT0_PATH, C_BOOT0_SIZE);
      rom_ready <= '1';
      wait;
   end process;

   sysrom_preload_proc : process (clk_qnice)
      variable idx : natural := 0;
   begin
      if rising_edge(clk_qnice) then
         sysrom_we_a <= '0';
         if rom_ready = '1' and sysrom_ready = '0' then
            if idx < C_BOOT0_SIZE then
               qnice_sysrom_addr <= std_logic_vector(to_unsigned(idx, 17));
               sysrom_data_a     <= rom_mem(idx);
               sysrom_we_a       <= '1';
               idx := idx + 1;
            else
               sysrom_ready <= '1';
            end if;
         end if;
      end if;
   end process;

   --------------------------------------------------------------------------------
   -- Monitors
   --------------------------------------------------------------------------------
   p_ram_we : process (clk_main)
      variable last_we : std_logic := '0';
   begin
      if rising_edge(clk_main) then
         if core_released and ram_we = '1' and last_we = '0' then
            ram_we_count <= ram_we_count + 1;
         end if;
         last_we := ram_we;
      end if;
   end process p_ram_we;

   -- Address and data must never be driven from both sides. R/W briefly can be: BA and R/W
   -- share a level shifter, so main holds R/W high while it shows the cart BA.
   rw_fight  <= cart_ctrl_oe = '1' and c_rw /= 'Z' and c_rw /= cart_rw_o;
   bus_fight <= (cart_addr_oe = '1' and c_a(0) /= 'Z') or (cart_data_oe = '1' and c_d(0) /= 'Z') or
                (cart_data_oe = '1' and c_d_io(0) /= 'Z') or (c_d(0) /= 'Z' and c_d_io(0) /= 'Z') or
                (cart_data_oe = '1' and c_d_umax(0) /= 'Z');

   -- A Kung Fu Flash 2 holds RESET until it has timed 100 PHI2 periods in a row (c64_wait_valid_clock),
   -- so PHI2 must run while the core is in reset. If it stops, the boot below never comes.
   p_cart_reset : process
      variable t_last : time;
      variable n_ok   : natural := 0;
   begin
      wait until core_released;
      cart_rst_n <= '0';
      wait until rising_edge(cart_phi2);
      t_last := now;
      while n_ok < 100 loop
         wait until rising_edge(cart_phi2);
         if now - t_last > 990 ns and now - t_last < 1040 ns then
            n_ok := n_ok + 1;
         else
            n_ok := 0;
         end if;
         t_last := now;
      end loop;
      cart_rst_n    <= '1';
      cart_rst_free <= now;
      wait;
   end process p_cart_reset;

   -- Reported, not checked: a Kung Fu Flash 2 samples DMA read data ~120 ns before PHI2 falls
   -- (C64_DMA_DATA_READ at PAL_PHI2_LOW), the Ultimate only as it falls.
   p_rd_settle : process (x_d, cart_phi2)
      variable t_chg : time := 0 ns;
   begin
      if x_d'event then
         t_chg := now;
      end if;
      if falling_edge(cart_phi2) and c_state = C_DO and cur_rw = '1' then
         if now - t_chg < rd_settle_min then
            rd_settle_min <= now - t_chg;
         end if;
         if cur_addr(15 downto 12) /= x"D" and now - t_chg < rd_settle_ram then
            rd_settle_ram <= now - t_chg;
         end if;
      end if;
   end process p_rd_settle;

   p_phi2_t : process (cart_phi2)
   begin
      if rising_edge(cart_phi2) then
         phi2_rise_t <= now;
      end if;
   end process p_phi2_t;

   -- At 1 MHz a PLA selects ROML/ROMH only with AEC, so they may outlast the PHI2 fall by a clock
   -- at most, as /IO does. The boot reads neither, so p_test reads both from /NMI code.
   p_rom_phase : process (clk_main)
      variable n_lo  : natural := 0;   -- main clocks since PHI2 fell
      variable sel_d : boolean := false;
      variable sel   : boolean;
   begin
      if rising_edge(clk_main) then
         if cart_phi2 = '1' then
            n_lo := 0;
         elsif n_lo < 63 then
            n_lo := n_lo + 1;
         end if;
         sel := cart_roml = '0' or cart_romh = '0';
         if core_released then
            if sel and not sel_d then
               n_rom_sel <= n_rom_sel + 1;
            end if;
            if sel and n_lo > 1 and tb_game_n = '1' then   -- not the VIC's Ultimax fetch
               n_rom_phi1 <= n_rom_phi1 + 1;
            end if;
         end if;
         sel_d := sel;
      end if;
   end process p_rom_phase;

   -- The VIC latches its byte at the end of VIC3, a main clock after PHI2 rises at the slot. In
   -- that clock and the one before, the bus must hold the Ultimax slave's byte for the address on it.
   p_umax_chk : process (clk_main)
      variable phi2_q : std_logic := '0';
      variable ok     : boolean;
      variable ok_1   : boolean := false;
      variable l      : line;
   begin
      if rising_edge(clk_main) then
         ok := cart_romh = '0' and in_a(15 downto 12) = x"F" and
               std_logic_vector(in_d) = umax_byte(std_logic_vector(in_a));
         if cart_phi2 = '1' and phi2_q = '0' and tb_game_n = '0' and cart_romh = '0' then
            n_umax <= n_umax + 1;
            if not (ok and ok_1) then
               n_umax_bad <= n_umax_bad + 1;
               if n_umax_bad < 8 then
                  write(l, string'("Ultimax VIC fetch: $") & to_hstring(std_logic_vector(in_a)) & " has $" &
                           to_hstring(std_logic_vector(in_d)) & " on the bus, the slave sampled $" &
                           to_hstring(u_addr));
                  writeline(output, l);
               end if;
            end if;
         end if;
         ok_1   := ok;
         phi2_q := cart_phi2;
      end if;
   end process p_umax_chk;

   p_contention : process (rw_fight, bus_fight)
      variable rw_start : time := 0 ns;
      variable b_start  : time := 0 ns;
      variable b_tail   : boolean := false;
      variable l        : line;
   begin
      if core_released then
         if rw_fight'event and rw_fight then
            rw_start := now;
         elsif rw_fight'event and now > rw_start then   -- delta glitches are not contention
            n_rw_x <= n_rw_x + 1;
            if now - rw_start > rw_x_max then
               rw_x_max <= now - rw_start;
            end if;
         end if;
         if bus_fight'event and bus_fight then
            b_start := now;
            b_tail  := c_d_io(0) /= 'Z' and cart_phi2 = '0';
         elsif bus_fight'event and now > b_start and b_tail then
            n_tail <= n_tail + 1;
            if now - b_start > tail_max then
               tail_max <= now - b_start;
            end if;
         elsif bus_fight'event and now > b_start then
            n_bus_x <= n_bus_x + 1;
            if n_bus_x < 8 then
               write(l, string'("contention: ") & time'image(now - b_start) & " ending " &
                        time'image(now - phi2_rise_t) & " after PHI2 rose; main drives addr " &
                        std_logic'image(cart_addr_oe) & " data " & std_logic'image(cart_data_oe) &
                        ", main R/W " & std_logic'image(cart_rw_o) & ", cart master " &
                        boolean'image(c_d(0) /= 'Z') & ", cart I/O slave " & boolean'image(c_d_io(0) /= 'Z'));
               writeline(output, l);
            end if;
         end if;
      end if;
   end process p_contention;

   --------------------------------------------------------------------------------
   -- Test
   --------------------------------------------------------------------------------
   p_test : process
      variable errors : natural := 0;
      variable d      : std_logic_vector(7 downto 0);
      variable d30    : std_logic_vector(7 downto 0);
      variable n_wr0  : natural;
      variable n_ba0  : natural;
      variable ok     : boolean;
      variable n      : natural;
      variable l      : line;

      procedure check(cond : boolean; msg : string) is
      begin
         if not cond then
            report msg severity error;
            errors := errors + 1;
         end if;
      end procedure check;

      procedure dma(rw : std_logic; addr : std_logic_vector(15 downto 0);
                    wdata : std_logic_vector(7 downto 0); rd : out std_logic_vector(7 downto 0)) is
      begin
         req_addr <= addr;
         req_rw   <= rw;
         req_data <= wdata;
         req      <= true;
         wait until rising_edge(cart_clk) and done;
         rd  := rdata;
         req <= false;
         wait until rising_edge(cart_clk) and not done;
      end procedure dma;

      procedure dma_wr(addr : std_logic_vector(15 downto 0); data : std_logic_vector(7 downto 0)) is
         variable dummy : std_logic_vector(7 downto 0);
      begin
         dma('0', addr, data, dummy);
      end procedure dma_wr;

      procedure dma_rd(addr : std_logic_vector(15 downto 0); data : out std_logic_vector(7 downto 0)) is
      begin
         dma('1', addr, x"00", data);
      end procedure dma_rd;

      procedure freeze is
      begin
         want_dma <= true;
         wait until rising_edge(cart_clk) and stopped;
      end procedure freeze;

      procedure unfreeze is
      begin
         want_dma <= false;
         wait until rising_edge(cart_clk) and not stopped;
      end procedure unfreeze;

      -- RAM bank 0, straight from the BRAM's other port
      procedure ram_peek(addr : natural; data : out std_logic_vector(7 downto 0)) is
      begin
         ram_a_addr <= std_logic_vector(to_unsigned(addr, 18));
         wait until rising_edge(clk_qnice);
         wait until rising_edge(clk_qnice);
         wait until rising_edge(clk_qnice);
         data := ram_a_q;
      end procedure ram_peek;

      type t_code is array (0 to 16) of std_logic_vector(7 downto 0);
      constant nmi_code : t_code := (
         x"AD", x"1D", x"DF",    -- lda $df1d
         x"8D", x"80", x"0C",    -- sta $0c80
         x"A9", x"5A",           -- lda #$5a
         x"8D", x"1C", x"DF",    -- sta $df1c
         x"EE", x"81", x"0C",    -- inc $0c81
         x"4C", x"33", x"FF");   -- jmp $ff33

      -- From /NMI: read external function ROM at $8000 and $C000, for /ROML and /ROMH. Bank 15
      -- again before $FF33, which the external ROM would hide.
      type t_rom is array (0 to 18) of std_logic_vector(7 downto 0);
      constant rom_code : t_rom := (
         x"A9", x"28", x"8D", x"00", x"FF",    -- lda #$28 / sta $ff00
         x"AD", x"00", x"80",                  -- lda $8000
         x"AD", x"00", x"C0",                  -- lda $c000
         x"A9", x"00", x"8D", x"00", x"FF",    -- lda #$00 / sta $ff00
         x"4C", x"33", x"FF");                 -- jmp $ff33

      function pat(i : natural) return std_logic_vector is
      begin
         return std_logic_vector(to_unsigned((i * 37 + i / 256 + 5) mod 256, 8));
      end function pat;

      -- Write C_BURST bytes back to back from base, then check them through DMA and in RAM
      procedure burst(base : natural; seed : natural; what : string) is
         variable rd : std_logic_vector(7 downto 0);
         variable bad : natural := 0;
         variable lo0, lo1, wr0 : natural;
      begin
         dma_rd(x"D012", rd);
         report what & ": starts at raster $" & to_hstring(rd) & " at " & time'image(now);
         lo0 := n_ba_lo;
         wr0 := n_warn_wr;
         for i in 0 to C_BURST - 1 loop
            dma_wr(std_logic_vector(to_unsigned(base + i, 16)), pat(seed + i));
         end loop;
         lo1 := n_ba_lo;
         dma_rd(x"D012", rd);
         report what & ": writes saw BA low at " & integer'image(lo1 - lo0) & " PHI2 rises, " &
                integer'image(n_warn_wr - wr0) & " writes after BA fell; now raster $" & to_hstring(rd) &
                " at " & time'image(now);
         for i in 0 to C_BURST - 1 loop
            dma_rd(std_logic_vector(to_unsigned(base + i, 16)), rd);
            if rd /= pat(seed + i) then
               bad := bad + 1;
               if bad <= 8 then
                  report what & ": DMA read $" & to_hstring(to_unsigned(base + i, 16)) & " = $" &
                         to_hstring(rd) & ", expected $" & to_hstring(pat(seed + i)) severity error;
               end if;
            end if;
         end loop;
         check(bad = 0, what & ": " & integer'image(bad) & " DMA reads differ from what DMA wrote");
         report what & ": reads saw BA low at " & integer'image(n_ba_lo - lo1) & " PHI2 rises, ending at " & time'image(now);
         bad := 0;
         for i in 0 to C_BURST - 1 loop
            ram_peek(base + i, rd);
            if rd /= pat(seed + i) then
               bad := bad + 1;
            end if;
         end loop;
         check(bad = 0, what & ": " & integer'image(bad) & " bytes in RAM bank 0 differ from what DMA wrote");
      end procedure burst;

   begin
      wait until sysrom_ready = '1';
      reset_hard <= '1';
      wait for C_MENU_HOLD;
      reset_hard    <= '0';
      core_released <= true;
      if boot_z80_n /= '0' then
         wait until boot_z80_n = '0';
      end if;
      wait until boot_z80_n = '1';
      report "8502 has the bus at " & time'image(now);
      check(cart_rst_free > 0 ns and cart_rst_free < now, "the 8502 ran while the cart held RESET");
      wait for 2 ms;

      -- Freeze. The kernal has I/O banked in most of the time: retry until DMA sees the VIC.
      ok := false;
      for attempt in 1 to 10 loop
         freeze;
         dma_wr(x"D020", x"05");
         dma_rd(x"D020", d);
         if d = x"F5" then
            ok := true;
            exit;
         end if;
         unfreeze;
         wait for 500 us;
      end loop;
      if not ok then
         report "FAIL: DMA never saw the VIC at $D020 (last read $" & to_hstring(d) & ")" severity error;
         write(l, string'("1 CHECKS FAILED"));
         writeline(output, l);
         std.env.stop;
         wait;
      end if;

      -- The 8502 is halted: no RAM writes while the cart idles on the bus
      n := ram_we_count;
      wait for 200 us;
      check(ram_we_count = n, "RAM was written while the cart held /DMA: " &
            integer'image(ram_we_count - n) & " writes");

      -- VIC and CIA registers, back to back, so a stale read latch would show
      dma_wr(x"D020", x"03");
      dma_wr(x"D021", x"0C");
      dma_rd(x"D020", d);
      check(d = x"F3", "VIC $D020: $" & to_hstring(d));
      dma_rd(x"D021", d);
      check(d = x"FC", "VIC $D021: $" & to_hstring(d));
      dma_wr(x"DC02", x"5A");
      dma_rd(x"DC02", d);
      check(d = x"5A", "CIA $DC02 after $5A: $" & to_hstring(d));
      dma_wr(x"DC02", x"A5");
      dma_rd(x"DC02", d);
      check(d = x"A5", "CIA $DC02 after $A5: $" & to_hstring(d));
      dma_wr(x"DC02", x"FF");

      -- As the kernal left it. In FAST mode DMA also gets the CPUC slot, and must repeat the same access.
      dma_rd(x"D030", d30);
      dma_rd(x"D011", d);
      report "at the freeze: $D030 = $" & to_hstring(d30) & ", $D011 = $" & to_hstring(d);
      burst(C_BURST_BASE, 0, "burst as found");

      -- 1 MHz with the display on, from just above the display window, so the burst runs into badlines
      dma_wr(x"D030", x"00");
      dma_wr(x"D011", x"1B");
      n := 0;
      loop
         dma_rd(x"D011", d);
         exit when d(7) = '1';
         n := n + 1;
      end loop;
      loop
         dma_rd(x"D012", d);
         if unsigned(d) >= x"20" and unsigned(d) < x"28" then   -- clear of the wrap after line $137
            dma_rd(x"D011", d);
            exit when d(7) = '0';
         end if;
         n := n + 1;
         if n > 60000 then
            check(false, "raster never reached $20");
            exit;
         end if;
      end loop;
      n_wr0 := n_warn_wr;
      n_ba0 := n_ba_wait;
      burst(C_BURST_BASE + C_BURST, 7, "burst over badlines");
      check(n_warn_wr > n_wr0, "no write landed in the cycles after BA fell, so that path is untested");
      check(n_ba_wait > n_ba0, "no read waited for BA, so the badlines were missed");
      dma_wr(x"D030", d30);

      -- Let go: the 8502 runs again
      unfreeze;
      n := ram_we_count;
      wait for 2 ms;
      check(ram_we_count > n, "the 8502 did not resume after /DMA was released");

      -- The CPU on cartridge I/O: code run from the cart's /NMI reads the command interface's
      -- identification ($DF1D) and writes its control register ($DF1C). Kernal $FF05 maps bank 15
      -- and jumps through ($0318); $FF33 returns.
      freeze;
      for i in 0 to 16 loop
         dma_wr(std_logic_vector(to_unsigned(16#0C00# + i, 16)), nmi_code(i));
      end loop;
      dma_wr(x"0C80", x"00");
      dma_wr(x"0C81", x"00");
      dma_wr(x"0318", x"00");
      dma_wr(x"0319", x"0C");
      unfreeze;
      tb_nmi_n <= '0';
      wait for 3 us;
      tb_nmi_n <= '1';
      wait for 300 us;
      ram_peek(16#0C81#, d);
      check(d = x"01", "the /NMI code ran " & integer'image(to_integer(unsigned(d))) & " times");
      ram_peek(16#0C80#, d);
      check(d = x"C9", "the 8502 read $" & to_hstring(d) & " from $DF1D, not $C9");
      check(n_uci_wr = 1 and uci_ctl = x"5A", "the cart took " & integer'image(n_uci_wr) &
            " writes to $DF1C, last $" & to_hstring(uci_ctl) & ", not one of $5A");

      -- At 1 MHz /ROML and /ROMH must stay out of PHI1 (p_rom_phase), so read both from /NMI code
      freeze;
      for i in rom_code'range loop
         dma_wr(std_logic_vector(to_unsigned(16#0C40# + i, 16)), rom_code(i));
      end loop;
      dma_wr(x"0318", x"40");
      dma_wr(x"0319", x"0C");
      n := n_rom_sel;
      unfreeze;
      tb_nmi_n <= '0';
      wait for 3 us;
      tb_nmi_n <= '1';
      wait for 200 us;
      check(n_rom_sel >= n + 2, "the 8502 read external function ROM with " &
            integer'image(n_rom_sel - n) & " /ROML-/ROMH accesses, not 2");
      check(n_rom_phi1 = 0, "/ROML or /ROMH was low in PHI1 for " & integer'image(n_rom_phi1) & " clocks");

      -- Ultimax: GAME low. In C128 mode the 8502's map ignores it, but the VIC's idle and refresh
      -- fetches at $3Fxx go to ROMH, and the slave samples their address early in PHI1.
      n := n_umax;
      tb_game_n <= '0';
      wait for 300 us;
      tb_game_n <= '1';
      check(n_umax > n + 50, "the VIC fetched from ROMH in only " & integer'image(n_umax - n) & " phases");
      check(n_umax_bad = 0, "the VIC latched the wrong byte in " & integer'image(n_umax_bad) & " of " &
            integer'image(n_umax - n) & " Ultimax fetches");

      check(n_bus_x = 0, integer'image(n_bus_x) & " address/data contentions");
      -- The Ultimate takes PHI2 low as a read (slot_slave.vhd), so it drives while it still sees
      -- /IO2, which outlasts the fall by a main clock, as a PLA's does, plus its own sync.
      check(tail_max <= 2 * C_MAIN_PERIOD, "a write tail on the data bus lasted " & time'image(tail_max));
      check(rw_x_max < 200 ns, "R/W contention lasted " & time'image(rw_x_max));

      write(l, string'("cart: ") & integer'image(n_access) & " DMA accesses, " &
               integer'image(n_warn_wr) & " writes after BA fell, " &
               integer'image(n_ba_wait) & " cycles a read waited for BA, R/W contention " &
               integer'image(n_rw_x) & "x up to " & time'image(rw_x_max) & ", I/O write tails " &
               integer'image(n_tail) & "x up to " & time'image(tail_max) & ", read data settled " &
               time'image(rd_settle_min) & " before PHI2 fell (RAM " & time'image(rd_settle_ram) & "), " &
               integer'image(n_rom_sel) & " /ROML-/ROMH accesses, " & integer'image(n_umax) &
               " Ultimax VIC fetches");
      writeline(output, l);
      if errors = 0 then
         write(l, string'("ALL CHECKS PASSED"));
      else
         write(l, integer'image(errors) & string'(" CHECKS FAILED"));
      end if;
      writeline(output, l);
      std.env.stop;
      wait;
   end process p_test;

end architecture sim;
