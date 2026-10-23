----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Testbench for virtual cartridges: main and sw_cartridge_wrapper as mega65.vhd connects them,
-- a HyperRAM model, and the real boot0.rom. The QNICE side loads each test/crt cartridge the way
-- the Shell does: the header through the QNICE device, the rest straight into the model to
-- save time, then the CSRs start the parser. main resets the C128 once the file is parsed,
-- the cartridge's program reads its probes and writes what it read to $0500 and its verdict
-- to $0400, and this compares that with the cartridge's .exp file.
--
-- Covers a C128 function ROM (16 kB windows, an 8 kB chip mirrored), a C64 16 kB cartridge, a
-- 4 kB Ultimax chip mirrored at $E000, EasyFlash with more banks than the cache holds, the same
-- EasyFlash with the REU switched on (it must stay off, out of the cartridge's RAM at $DF00), and
-- a type the parser must refuse.
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

entity tb_crt is
end entity tb_crt;

architecture sim of tb_crt is

   constant C_BOOT0_SIZE   : natural := 73728;
   constant C_MAIN_PERIOD  : time    := 1 sec / CORE_CLK_SPEED;
   constant C_QNICE_PERIOD : time    := 20 ns;
   constant C_HR_PERIOD    : time    := 10 ns;
   constant C_HR_LATENCY   : natural := 12;                       -- clocks to a burst's first word
   constant C_HR_WORDS     : natural := 2**18;                    -- 512 kB from the CRT region on
   constant C_HR_BASE      : natural := to_integer(unsigned(C_HMAP_CRT)) * 4096;
   constant C_QNICE_BYTES  : natural := 64;                       -- the CRT header goes through QNICE
   -- BOOT0_PATH is <repo>/sdcard/c128/boot0.rom
   constant C_REPO         : string  := BOOT0_PATH(1 to BOOT0_PATH'length - 21);
   constant C_CRT_DIR      : string  := C_REPO & "test/crt/";

   signal clk_main         : std_logic := '0';
   signal clk_qnice        : std_logic := '0';
   signal clk_hr           : std_logic := '0';
   signal reset_hard       : std_logic := '1';
   signal rst_qnice        : std_logic := '1';
   signal rst_hr           : std_logic := '1';

   signal ram_addr         : unsigned(17 downto 0);
   signal ram_data_to      : unsigned(7 downto 0);
   signal ram_we           : std_logic;
   signal ram_data_from    : std_logic_vector(7 downto 0);
   signal sys_rom_addr     : std_logic_vector(16 downto 0);
   signal sys_rom_data     : std_logic_vector(7 downto 0);
   signal qnice_sysrom_addr: std_logic_vector(16 downto 0) := (others => '0');
   signal sysrom_data_a    : std_logic_vector(7 downto 0) := (others => '0');
   signal sysrom_we_a      : std_logic := '0';
   signal rom_mem          : byte_rom_t(0 to C_BOOT0_SIZE - 1) := (others => (others => '0'));
   signal rom_ready        : std_logic := '0';
   signal sysrom_ready     : std_logic := '0';
   signal osm_control      : std_logic_vector(255 downto 0) := (others => '0');
   signal reu_on           : std_logic := '0';

   -- main <-> sw_cartridge_wrapper
   signal crt_loaded       : std_logic;
   signal crt_loading      : std_logic;
   signal crt_id           : std_logic_vector(15 downto 0);
   signal crt_exrom        : std_logic_vector( 7 downto 0);
   signal crt_game         : std_logic_vector( 7 downto 0);
   signal crt_size         : std_logic_vector(22 downto 0);
   signal crt_c128         : std_logic;
   signal crt_subtype      : std_logic_vector( 7 downto 0);
   signal crt_bank_lo      : std_logic_vector( 6 downto 0);
   signal crt_bank_hi      : std_logic_vector( 6 downto 0);
   signal crt_wait         : std_logic;
   signal crt_addr         : std_logic_vector(15 downto 0);
   signal crt_data         : std_logic_vector( 7 downto 0);
   signal crt_lo_data      : std_logic_vector( 7 downto 0);
   signal crt_hi_data      : std_logic_vector( 7 downto 0);
   signal crt_ioe_we       : std_logic;
   signal crt_iof_we       : std_logic;
   signal crt_ram_we       : std_logic;
   signal crt_ioe_data     : std_logic_vector( 7 downto 0);
   signal crt_iof_data     : std_logic_vector( 7 downto 0);
   signal crt_ram_data     : std_logic_vector( 7 downto 0);

   -- QNICE device bus
   signal q_addr           : std_logic_vector(27 downto 0) := (others => '0');
   signal q_wdata          : std_logic_vector(15 downto 0) := (others => '0');
   signal q_ce             : std_logic := '0';
   signal q_we             : std_logic := '0';
   signal q_rdata          : std_logic_vector(15 downto 0);
   signal q_wait           : std_logic;

   -- HyperRAM
   signal hr_write         : std_logic;
   signal hr_read          : std_logic;
   signal hr_address       : std_logic_vector(31 downto 0);
   signal hr_writedata     : std_logic_vector(15 downto 0);
   signal hr_byteenable    : std_logic_vector( 1 downto 0);
   signal hr_burstcount    : std_logic_vector( 7 downto 0);
   signal hr_readdata      : std_logic_vector(15 downto 0) := (others => '0');
   signal hr_readdatavalid : std_logic := '0';
   signal hr_waitrequest   : std_logic := '0';

   type t_words is array (natural range <>) of natural range 0 to 65535;

   -- The HyperRAM's contents, shared by the model and the test's backdoor load
   type t_hyperram is protected
      impure function load(path : string; skip : natural) return natural;
      impure function get(addr : natural) return natural;
      procedure put(addr : natural; data : std_logic_vector(15 downto 0); be : std_logic_vector(1 downto 0));
   end protected t_hyperram;

   type t_hyperram is protected body
      variable mem : t_words(0 to C_HR_WORDS - 1) := (others => 65535);

      -- The file from the region's start, with its first skip bytes zeroed. Returns its length.
      impure function load(path : string; skip : natural) return natural is
         type t_char_file is file of character;
         file f     : t_char_file;
         variable c : character;
         variable n : natural := 0;
         variable b : natural;
      begin
         file_open(f, path, read_mode);
         while not endfile(f) loop
            read(f, c);
            b := character'pos(c);
            if n < skip then
               b := 0;
            end if;
            if n mod 2 = 0 then
               mem(n / 2) := b;
            else
               mem(n / 2) := (mem(n / 2) mod 256) + b * 256;
            end if;
            n := n + 1;
         end loop;
         file_close(f);
         return n;
      end function load;

      impure function get(addr : natural) return natural is
      begin
         if addr >= C_HR_BASE and addr < C_HR_BASE + C_HR_WORDS then
            return mem(addr - C_HR_BASE);
         end if;
         return 65535;
      end function get;

      procedure put(addr : natural; data : std_logic_vector(15 downto 0); be : std_logic_vector(1 downto 0)) is
         variable v : unsigned(15 downto 0);
      begin
         if addr >= C_HR_BASE and addr < C_HR_BASE + C_HR_WORDS then
            v := to_unsigned(mem(addr - C_HR_BASE), 16);
            if be(0) = '1' then
               v(7 downto 0) := unsigned(data(7 downto 0));
            end if;
            if be(1) = '1' then
               v(15 downto 8) := unsigned(data(15 downto 8));
            end if;
            mem(addr - C_HR_BASE) := to_integer(v);
         end if;
      end procedure put;
   end protected body t_hyperram;

   shared variable hyperram : t_hyperram;

   -- What the cartridge's program wrote to $0400-$0401 and $0500-$05FF
   type t_bytes is array (0 to 255) of std_logic_vector(7 downto 0);
   signal cap_clear        : boolean := false;
   signal results          : t_bytes := (others => (others => 'U'));
   signal n_results        : natural := 0;
   signal status           : std_logic_vector(7 downto 0) := (others => 'U');
   signal status_errors    : std_logic_vector(7 downto 0) := (others => 'U');
   signal status_seen      : boolean := false;
   signal n_waits          : natural := 0;         -- times the cache made the CPU wait
   signal crt_wait_d       : std_logic := '0';

begin

   clk_main  <= not clk_main  after C_MAIN_PERIOD / 2;
   clk_qnice <= not clk_qnice after C_QNICE_PERIOD / 2;
   clk_hr    <= not clk_hr    after C_HR_PERIOD / 2;

   osm_control(C_MENU_CART_CRT) <= '1';
   osm_control(C_MENU_REU_512K) <= reu_on;

   u_main_ram : entity work.dualport_2clk_ram
      generic map (
         ADDR_WIDTH => 18,
         DATA_WIDTH => 8,
         FALLING_A  => true
      )
      port map (
         clock_a         => clk_qnice,
         address_a       => (others => '0'),
         do_latch_addr_a => '0',
         data_a          => (others => '0'),
         wren_a          => '0',
         q_a             => open,
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
         G_BOARD         => "MEGA65_R6",
         G_VDNUM         => 0,
         G_EXP_RESET_LEN => CORE_CLK_SPEED / 1000     -- 1 ms instead of 50
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
         osm_control_i        => osm_control,
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
         boot_z80_n_o         => open,
         ram_addr_o           => ram_addr,
         ram_data_o           => ram_data_to,
         ram_we_o             => ram_we,
         ram_data_i           => unsigned(ram_data_from),
         sys_rom_addr_o       => sys_rom_addr,
         sys_rom_data_i       => sys_rom_data,
         cart_en_o            => open,
         cart_phi2_o          => open,
         cart_dotclock_o      => open,
         cart_dma_i           => '1',
         cart_reset_oe_o      => open,
         cart_reset_i         => '1',
         cart_reset_o         => open,
         cart_game_oe_o       => open,
         cart_game_i          => '1',
         cart_game_o          => open,
         cart_exrom_oe_o      => open,
         cart_exrom_i         => '1',
         cart_exrom_o         => open,
         cart_nmi_oe_o        => open,
         cart_nmi_i           => '1',
         cart_nmi_o           => open,
         cart_irq_oe_o        => open,
         cart_irq_i           => '1',
         cart_irq_o           => open,
         cart_roml_oe_o       => open,
         cart_roml_i          => '1',
         cart_roml_o          => open,
         cart_romh_oe_o       => open,
         cart_romh_i          => '1',
         cart_romh_o          => open,
         cart_ctrl_oe_o       => open,
         cart_ba_i            => '1',
         cart_rw_i            => '1',
         cart_io1_i           => '1',
         cart_io2_i           => '1',
         cart_ba_o            => open,
         cart_rw_o            => open,
         cart_io1_o           => open,
         cart_io2_o           => open,
         cart_addr_oe_o       => open,
         cart_a_i             => (others => '1'),
         cart_a_o             => open,
         cart_data_oe_o       => open,
         cart_d_i             => (others => '1'),
         cart_d_o             => open,
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
         pot2_y_i             => (others => '0'),
         crt_loaded_i         => crt_loaded,
         crt_loading_i        => crt_loading,
         crt_id_i             => crt_id,
         crt_exrom_i          => crt_exrom,
         crt_game_i           => crt_game,
         crt_size_i           => crt_size,
         crt_c128_i           => crt_c128,
         crt_subtype_i        => crt_subtype,
         crt_bank_lo_o        => crt_bank_lo,
         crt_bank_hi_o        => crt_bank_hi,
         crt_wait_i           => crt_wait,
         crt_addr_o           => crt_addr,
         crt_data_o           => crt_data,
         crt_lo_data_i        => crt_lo_data,
         crt_hi_data_i        => crt_hi_data,
         crt_ioe_we_o         => crt_ioe_we,
         crt_iof_we_o         => crt_iof_we,
         crt_ram_we_o         => crt_ram_we,
         crt_ioe_data_i       => crt_ioe_data,
         crt_iof_data_i       => crt_iof_data,
         crt_ram_data_i       => crt_ram_data
      );

   u_crt : entity work.sw_cartridge_wrapper
      generic map (
         G_BASE_ADDRESS => C_HMAP_CRT(9 downto 0) & x"000"
      )
      port map (
         qnice_clk_i        => clk_qnice,
         qnice_rst_i        => rst_qnice,
         qnice_addr_i       => q_addr,
         qnice_data_i       => q_wdata,
         qnice_ce_i         => q_ce,
         qnice_we_i         => q_we,
         qnice_data_o       => q_rdata,
         qnice_wait_o       => q_wait,
         main_clk_i         => clk_main,
         main_loaded_o      => crt_loaded,
         main_loading_o     => crt_loading,
         main_id_o          => crt_id,
         main_exrom_o       => crt_exrom,
         main_game_o        => crt_game,
         main_size_o        => crt_size,
         main_c128_o        => crt_c128,
         main_subtype_o     => crt_subtype,
         main_bank_lo_i     => crt_bank_lo,
         main_bank_hi_i     => crt_bank_hi,
         main_wait_o        => crt_wait,
         main_addr_i        => crt_addr,
         main_data_i        => crt_data,
         main_lo_data_o     => crt_lo_data,
         main_hi_data_o     => crt_hi_data,
         main_ioe_we_i      => crt_ioe_we,
         main_iof_we_i      => crt_iof_we,
         main_ram_we_i      => crt_ram_we,
         main_ioe_data_o    => crt_ioe_data,
         main_iof_data_o    => crt_iof_data,
         main_ram_data_o    => crt_ram_data,
         hr_clk_i           => clk_hr,
         hr_rst_i           => rst_hr,
         hr_write_o         => hr_write,
         hr_read_o          => hr_read,
         hr_address_o       => hr_address,
         hr_writedata_o     => hr_writedata,
         hr_byteenable_o    => hr_byteenable,
         hr_burstcount_o    => hr_burstcount,
         hr_readdata_i      => hr_readdata,
         hr_readdatavalid_i => hr_readdatavalid,
         hr_waitrequest_i   => hr_waitrequest
      );

   --------------------------------------------------------------------------------
   -- HyperRAM: single writes, read bursts after a fixed latency, one command at a time
   --------------------------------------------------------------------------------
   p_hyperram : process (clk_hr)
      variable busy : boolean := false;
      variable addr : natural;
      variable left : natural := 0;
      variable lat  : natural := 0;
   begin
      if rising_edge(clk_hr) then
         hr_readdatavalid <= '0';
         if busy then
            if lat /= 0 then
               lat := lat - 1;
            else
               hr_readdata      <= std_logic_vector(to_unsigned(hyperram.get(addr), 16));
               hr_readdatavalid <= '1';
               addr := addr + 1;
               left := left - 1;
               busy := left /= 0;
            end if;
         elsif hr_write = '1' then
            hyperram.put(to_integer(unsigned(hr_address(21 downto 0))), hr_writedata, hr_byteenable);
         elsif hr_read = '1' then
            addr := to_integer(unsigned(hr_address(21 downto 0)));
            left := to_integer(unsigned(hr_burstcount));
            lat  := C_HR_LATENCY;
            busy := true;
         end if;
         if busy then
            hr_waitrequest <= '1';
         else
            hr_waitrequest <= '0';
         end if;
      end if;
   end process p_hyperram;

   --------------------------------------------------------------------------------
   -- Boot ROM, as in tb_c128_boot
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
   -- What the program writes to RAM bank 0, and how often the cache stalls the CPU
   --------------------------------------------------------------------------------
   p_capture : process (clk_main)
      variable a : natural;
   begin
      if rising_edge(clk_main) then
         crt_wait_d <= crt_wait;
         if crt_wait = '1' and crt_wait_d = '0' then
            n_waits <= n_waits + 1;
         end if;
         if ram_we = '1' and ram_addr(17 downto 16) = "00" then
            a := to_integer(ram_addr(15 downto 0));
            if a >= 16#0500# and a <= 16#05FF# then
               results(a - 16#0500#) <= std_logic_vector(ram_data_to);
               if a - 16#0500# + 1 > n_results then
                  n_results <= a - 16#0500# + 1;
               end if;
            elsif a = 16#0401# then
               status_errors <= std_logic_vector(ram_data_to);
            elsif a = 16#0400# then
               status      <= std_logic_vector(ram_data_to);
               status_seen <= true;
            end if;
         end if;
         if cap_clear then
            results       <= (others => (others => 'U'));
            n_results     <= 0;
            status        <= (others => 'U');
            status_errors <= (others => 'U');
            status_seen   <= false;
            n_waits       <= 0;
         end if;
      end if;
   end process p_capture;

   --------------------------------------------------------------------------------
   -- Test
   --------------------------------------------------------------------------------
   p_test : process
      variable errors : natural := 0;
      variable l      : line;

      procedure check(cond : boolean; msg : string) is
      begin
         if not cond then
            report "FAIL: " & msg severity error;
            errors := errors + 1;
         end if;
      end procedure check;

      -- QNICE bus cycles as the CPU makes them, see C64MEGA65's qnice_sim.vhd
      procedure q_write(addr : std_logic_vector(27 downto 0); data : std_logic_vector(15 downto 0)) is
      begin
         q_addr  <= addr;
         q_wdata <= data;
         q_we    <= '1';
         q_ce    <= '1';
         wait until falling_edge(clk_qnice);
         while q_wait = '1' loop
            wait until falling_edge(clk_qnice);
         end loop;
         q_ce <= '0';
         q_we <= '0';
      end procedure q_write;

      procedure q_read(addr : std_logic_vector(27 downto 0); data : out std_logic_vector(15 downto 0)) is
      begin
         q_addr <= addr;
         q_we   <= '0';
         q_ce   <= '1';
         wait until falling_edge(clk_qnice);
         while q_wait = '1' loop
            wait until falling_edge(clk_qnice);
         end loop;
         data := q_rdata;
         q_ce <= '0';
      end procedure q_read;

      constant C_STATUS  : std_logic_vector(27 downto 0) := x"FFFF000";
      constant C_FS_LO   : std_logic_vector(27 downto 0) := x"FFFF001";
      constant C_FS_HI   : std_logic_vector(27 downto 0) := x"FFFF002";
      constant C_PARSEST : std_logic_vector(27 downto 0) := x"FFFF010";
      constant C_PARSEE1 : std_logic_vector(27 downto 0) := x"FFFF011";
      constant C_ERR_STR : std_logic_vector(27 downto 0) := x"FFFF100";

      -- Load a .crt as the Shell does and return the parser's final status (2 ready, 3 error)
      procedure load_crt(name : string; parse : out natural) is
         type t_char_file is file of character;
         file f     : t_char_file;
         variable c : character;
         variable n : natural;
         variable d : std_logic_vector(15 downto 0);
      begin
         q_write(C_STATUS, x"0001");                   -- loading
         n := hyperram.load(C_CRT_DIR & name, C_QNICE_BYTES);
         file_open(f, C_CRT_DIR & name, read_mode);
         for i in 0 to C_QNICE_BYTES - 1 loop
            read(f, c);
            q_write(std_logic_vector(to_unsigned(i, 28)), x"00" & std_logic_vector(to_unsigned(character'pos(c), 8)));
         end loop;
         file_close(f);
         q_write(C_FS_LO, std_logic_vector(to_unsigned(n mod 65536, 16)));
         q_write(C_FS_HI, std_logic_vector(to_unsigned(n / 65536, 16)));
         q_write(C_STATUS, x"0003");                   -- loaded: parse it
         for i in 1 to 10000 loop
            q_read(C_PARSEST, d);
            exit when d = x"0002" or d = x"0003";
            wait for 1 us;
         end loop;
         parse := to_integer(unsigned(d));
         report name & ": " & integer'image(n) & " bytes, parser status " & integer'image(parse);
      end procedure load_crt;

      -- Run a cartridge and compare what its program read with its .exp file
      procedure run_crt(name : string; timeout : time) is
         file f         : text;
         variable el    : line;
         variable want  : std_logic_vector(7 downto 0);
         variable n     : natural := 0;
         variable bad   : natural := 0;
         variable parse : natural;
      begin
         load_crt(name & ".crt", parse);
         check(parse = 2, name & ": the parser did not accept it");
         wait for 100 us;                              -- main is resetting the C128 by now
         cap_clear <= true;
         wait until rising_edge(clk_main);
         wait until rising_edge(clk_main);
         cap_clear <= false;
         wait until status_seen for timeout;
         check(status_seen, name & ": the program never finished");
         if not status_seen then
            return;
         end if;
         wait for 10 us;
         file_open(f, C_CRT_DIR & name & ".exp", read_mode);
         while not endfile(f) loop
            readline(f, el);
            hread(el, want);
            if results(n) /= want then
               report name & ": probe " & integer'image(n) & " read $" & to_hstring(results(n)) &
                      ", expected $" & to_hstring(want) severity error;
               bad := bad + 1;
            end if;
            n := n + 1;
         end loop;
         file_close(f);
         check(bad = 0, name & ": " & integer'image(bad) & " of " & integer'image(n) & " probes read wrong");
         check(n_results = n, name & ": the program stored " & integer'image(n_results) & " results, not " &
                              integer'image(n));
         check(status = x"10" and status_errors = x"00", name & ": the program reports '" &
               to_hstring(status) & "' with $" & to_hstring(status_errors) & " errors");
         report name & ": " & integer'image(n) & " probes checked, the cache stalled the CPU " &
                integer'image(n_waits) & " times";
      end procedure run_crt;

      variable parse : natural;
      variable d     : std_logic_vector(15 downto 0);
      variable s     : string(1 to 16);

   begin
      wait until sysrom_ready = '1';
      wait until rising_edge(clk_hr);
      rst_hr    <= '0';
      wait until rising_edge(clk_qnice);
      rst_qnice <= '0';
      wait for 500 us;
      reset_hard <= '0';
      wait for 2 ms;

      run_crt("crttest-c128",   20 ms);
      run_crt("crttest-16k",    20 ms);
      run_crt("crttest-umax4k", 20 ms);
      run_crt("crttest-ef",     30 ms);
      check(n_waits >= 10, "EasyFlash switched banks without the cache stalling the CPU");

      -- With the REU on, EasyFlash still owns $DF00-$DF1F: the REU stays off for it
      reu_on <= '1';
      wait for 3 ms;
      run_crt("crttest-ef",     30 ms);
      reu_on <= '0';
      wait for 3 ms;

      -- A type cartridge.vhd lacks must be refused with a reason
      load_crt("crttest-badtype.crt", parse);
      check(parse = 3, "crttest-badtype: the parser accepted it");
      q_read(C_PARSEE1, d);
      check(d = x"0006", "crttest-badtype: error code $" & to_hstring(d) & ", not 6");
      for i in 0 to 15 loop
         q_read(std_logic_vector(unsigned(C_ERR_STR) + i), d);
         s(i + 1) := character'val(to_integer(unsigned(d(7 downto 0))));
      end loop;
      check(s = "Unsupported type", "crttest-badtype: error string '" & s & "'");
      wait for 100 us;
      check(crt_loaded = '0', "crttest-badtype: main sees a cartridge");

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
