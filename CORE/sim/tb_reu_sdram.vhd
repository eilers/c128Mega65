----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Testbench for an REU in SDRAM beside the virtual cartridge (R4 and later): MiSTer's reu.sv
-- behind reu_mapper, avm_cache and avm_fifo as in main.vhd, and the cartridge's crt_reader,
-- sharing the real SDRAM controller through sdram_arbit, with tb_crt's model of the chip.
--
-- The C128 side is tb_reu's bus model with fpga64_sid_iec's 32-clock cycle: PHI2 high at
-- clocks 7-22, ext_cycle 4-7, dma_cycle 16-31. The CPU's address goes out at clock 24 and is
-- latched at the end of 23; in FAST mode it also reads at 24-31 (address at 24, latched at the
-- end of 31), so its other access starts at clock 0. The VIC fetches ROMH in Ultimax mode in
-- PHI1, asking at clock 25 and latching at the end of 7. The DMA latches at the end of 31.
-- Cartridge requests cross to crt_reader as sw_cartridge_wrapper's do, and nothing makes the
-- C128 wait for one: its byte must be there two clocks before it is latched.
--
-- The REU may start an SDRAM access only in a window of PHI2 low (G_REU_FROM..G_REU_TO, phi2_ph
-- as main.vhd counts it), which ends before PHI2 high's cartridge reads and the refresh window.
-- The defaults are main.vhd's C_REU_FROM and C_REU_TO. Sizes 512 KB, 2 MB and 16 MB.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

library work;
use work.globals.all;

entity tb_reu_sdram is
   generic (
      G_REU_FROM : natural := 7;    -- PHI2 low: the first phi2_ph the REU's window opens at
      G_REU_TO   : natural := 15    -- PHI2 low: the last
   );
end entity tb_reu_sdram;

architecture sim of tb_reu_sdram is

   constant C_MAIN_PERIOD : time := 31.718 ns;   -- 31.528 MHz
   constant C_HR_PERIOD   : time := 10 ns;       -- 100 MHz

   -- SDRAM word addresses, as mega65.vhd has them: the REU and the cartridge's file
   constant C_REU_W       : natural := to_integer(unsigned(C_SMAP_REU(12 downto 0))) * 4096;
   constant C_CRT_RADDR   : natural := to_integer(unsigned(C_SMAP_CRT(12 downto 0))) * 8192 + 16#40#;   -- ROML's image
   constant C_CRT_W       : natural := C_CRT_RADDR / 2;

   signal clk       : std_logic := '0';
   signal hr_clk    : std_logic := '0';
   signal rst       : std_logic := '1';
   signal hr_rst    : std_logic := '1';
   signal running   : boolean   := true;

   -- C128 bus model. bus_* is the CPU's side; sys_* is what the bus carries.
   signal cyc        : natural range 0 to 31 := 0;
   signal phi2       : std_logic;
   signal phi2_ph    : natural range 0 to 31;
   signal ext_cycle  : std_logic;
   signal dma_active : std_logic := '0';
   signal dma_cycle  : std_logic := '0';
   signal bus_addr   : std_logic_vector(15 downto 0) := x"0400";
   signal bus_we     : std_logic := '0';
   signal bus_wdata  : std_logic_vector(7 downto 0) := x"00";
   signal sys_addr   : std_logic_vector(15 downto 0);
   signal sys_we     : std_logic;
   signal cpu_cs     : std_logic;
   signal ram_q      : std_logic_vector(7 downto 0) := x"00";

   type ram_t is array (0 to 65535) of std_logic_vector(7 downto 0);

   function pat(a : natural) return std_logic_vector is
   begin
      return std_logic_vector(to_unsigned((a * 7 + a / 256) mod 256, 8));
   end function pat;

   function ram_init return ram_t is
      variable r : ram_t;
   begin
      for i in r'range loop
         r(i) := pat(i);
      end loop;
      return r;
   end function ram_init;

   signal c128_ram  : ram_t := ram_init;

   -- What the test runs: a cartridge (16 kB, or Ultimax with the VIC fetching ROMH), FAST mode
   signal crt_active : std_logic := '0';
   signal umax       : std_logic := '0';
   signal fast       : std_logic := '0';
   signal vic_on     : std_logic := '0';
   signal vic_addr   : std_logic_vector(15 downto 0) := x"3000";
   signal chk_armed  : boolean   := false;

   -- The cartridge's file: a 16 kB C64 image, its word at each SDRAM address
   function crt_word(w : natural) return natural is
   begin
      return (w * 251 + w / 256 + 16#5A3C#) mod 65536;
   end function crt_word;

   -- crt_reader's key: ROMH, bank, offset
   function crt_key(hi : std_logic; addr : std_logic_vector(15 downto 0)) return std_logic_vector is
   begin
      return hi & "0000000" & '0' & addr(12 downto 0);
   end function crt_key;

   function crt_byte(key : std_logic_vector(21 downto 0)) return std_logic_vector is
      variable off : natural := to_integer(unsigned(key(12 downto 0)));
      variable w   : natural := C_CRT_W + off / 2;
      variable v   : unsigned(15 downto 0);
   begin
      if key(21) = '1' then
         w := w + 16#1000#;
      end if;
      v := to_unsigned(crt_word(w), 16);
      if off mod 2 = 1 then
         return std_logic_vector(v(15 downto 8));
      end if;
      return std_logic_vector(v(7 downto 0));
   end function crt_byte;

   function is_io(a : std_logic_vector(15 downto 0)) return boolean is
   begin
      return a(15 downto 9) = "1101111";                      -- IO1, IO2
   end function is_io;

   function is_roml(a : std_logic_vector(15 downto 0)) return boolean is
   begin
      return a(15 downto 13) = "100";
   end function is_roml;

   function is_romh(a : std_logic_vector(15 downto 0); u : std_logic) return boolean is
   begin
      return (u = '0' and a(15 downto 13) = "101") or (u = '1' and a(15 downto 13) = "111");
   end function is_romh;

   -- SDRAM contents, shared by the chip model and the test: all 32 Mi words, a row-sized page
   -- made on its first write
   type t_page is array (0 to 1023) of natural range 0 to 65535;
   type t_page_ptr is access t_page;
   type t_pages is array (0 to 32767) of t_page_ptr;

   type t_sdram is protected
      procedure fill;
      impure function get(addr : natural) return natural;
      procedure put(addr : natural; data : std_logic_vector(15 downto 0));
   end protected t_sdram;

   type t_sdram is protected body
      variable pages : t_pages;

      procedure set(addr : natural; v : natural) is
      begin
         if pages(addr / 1024) = null then
            pages(addr / 1024) := new t_page'(others => 0);
         end if;
         pages(addr / 1024)(addr mod 1024) := v;
      end procedure set;

      procedure fill is
      begin
         for w in C_CRT_W to C_CRT_W + 16#1FFF# loop
            set(w, crt_word(w));
         end loop;
      end procedure fill;

      impure function get(addr : natural) return natural is
      begin
         if pages(addr / 1024) = null then
            return 0;
         end if;
         return pages(addr / 1024)(addr mod 1024);
      end function get;

      procedure put(addr : natural; data : std_logic_vector(15 downto 0)) is
      begin
         assert addr >= C_REU_W and addr < C_REU_W + 16#800000#
            report "SDRAM write outside the REU's region: " & to_hstring(to_unsigned(addr, 25)) severity error;
         set(addr, to_integer(unsigned(data)));
      end procedure put;
   end protected body t_sdram;

   shared variable sdram_mem : t_sdram;

   -- REU
   signal cfg       : std_logic_vector(1 downto 0) := "00";
   signal dma_req   : std_logic;
   signal dma_addr  : std_logic_vector(15 downto 0);
   signal dma_dout  : std_logic_vector(7 downto 0);
   signal dma_din   : std_logic_vector(7 downto 0) := x"00";
   signal dma_we    : std_logic;
   signal cpu_din   : std_logic_vector(7 downto 0);
   signal irq       : std_logic;
   signal ram_cycle : std_logic;
   signal ram_addr  : std_logic_vector(24 downto 0);
   signal ram_addr_m: std_logic_vector(24 downto 0);
   signal ram_dout  : std_logic_vector(7 downto 0);
   signal ram_din   : std_logic_vector(7 downto 0);
   signal ram_we    : std_logic;
   signal ram_cs    : std_logic;

   -- reu_mapper -> avm_cache (map_*) -> avm_fifo (c_*) -> sdram_arbit s1 (h_*)
   signal map_write, map_read, map_readdatavalid, map_waitrequest : std_logic;
   signal map_address    : std_logic_vector(31 downto 0);
   signal map_writedata  : std_logic_vector(15 downto 0);
   signal map_byteenable : std_logic_vector(1 downto 0);
   signal map_burstcount : std_logic_vector(7 downto 0);
   signal map_readdata   : std_logic_vector(15 downto 0);
   signal c_write, c_read, c_readdatavalid, c_waitrequest : std_logic;
   signal c_address      : std_logic_vector(31 downto 0);
   signal c_writedata    : std_logic_vector(15 downto 0);
   signal c_byteenable   : std_logic_vector(1 downto 0);
   signal c_burstcount   : std_logic_vector(7 downto 0);
   signal c_readdata     : std_logic_vector(15 downto 0);
   signal h_write, h_read, h_readdatavalid, h_waitrequest : std_logic;
   signal h_address      : std_logic_vector(31 downto 0);
   signal h_writedata    : std_logic_vector(15 downto 0);
   signal h_byteenable   : std_logic_vector(1 downto 0);
   signal h_burstcount   : std_logic_vector(7 downto 0);
   signal h_readdata     : std_logic_vector(15 downto 0);

   -- Cartridge requests, as sw_cartridge_wrapper makes them
   signal cpu_rd           : std_logic;
   signal cpu_hi           : std_logic;
   signal main_cpu_key     : std_logic_vector(21 downto 0);
   signal main_vic_key     : std_logic_vector(21 downto 0);
   signal main_cpu_key_r   : std_logic_vector(21 downto 0) := (others => '1');
   signal main_vic_key_r   : std_logic_vector(21 downto 0) := (others => '1');
   signal main_cpu_cnt     : unsigned(2 downto 0) := (others => '0');
   signal main_vic_cnt     : unsigned(2 downto 0) := (others => '0');
   signal main_cpu_req_r   : std_logic_vector(2 downto 0) := (others => '0');
   signal main_vic_req_r   : std_logic_vector(2 downto 0) := (others => '0');
   signal main_cpu_ok      : std_logic := '0';
   signal main_vic_ok      : std_logic := '0';
   signal hr_cpu_req_d     : std_logic_vector(2 downto 0) := (others => '0');
   signal hr_cpu_req       : std_logic_vector(2 downto 0) := (others => '0');
   signal hr_vic_req_d     : std_logic_vector(2 downto 0) := (others => '0');
   signal hr_vic_req       : std_logic_vector(2 downto 0) := (others => '0');
   signal hr_cpu_data      : std_logic_vector(7 downto 0);
   signal hr_cpu_done      : std_logic_vector(2 downto 0);
   signal hr_vic_data      : std_logic_vector(7 downto 0);
   signal hr_vic_done      : std_logic_vector(2 downto 0);

   -- Windows: refreshes as main.vhd opens them, and the REU's; then the wrapper's register and
   -- the crossing to hr_clk
   signal crt_rfsh_ok      : std_logic := '1';
   signal crt_reu_ok       : std_logic := '1';
   signal main_rfsh_ok_r   : std_logic := '1';
   signal main_reu_ok_r    : std_logic := '1';
   signal hr_rfsh_ok_d     : std_logic := '1';
   signal hr_rfsh_ok       : std_logic := '1';
   signal hr_reu_ok_d      : std_logic := '1';
   signal hr_reu_ok        : std_logic := '1';

   -- crt_reader's parser side
   signal cart_ready       : std_logic := '0';
   signal cart_loading     : std_logic := '0';
   signal cart_bank_wr     : std_logic := '0';

   -- crt_reader -> sdram_arbit s0 (k_*), sdram_arbit -> sdram (sr_*)
   signal k_write, k_read, k_readdatavalid, k_waitrequest : std_logic;
   signal k_address        : std_logic_vector(31 downto 0);
   signal k_writedata      : std_logic_vector(15 downto 0);
   signal k_byteenable     : std_logic_vector(1 downto 0);
   signal k_burstcount     : std_logic_vector(7 downto 0);
   signal k_readdata       : std_logic_vector(15 downto 0);
   signal sr_write, sr_read, sr_readdatavalid, sr_waitrequest : std_logic;
   signal sr_address       : std_logic_vector(31 downto 0);
   signal sr_writedata     : std_logic_vector(15 downto 0);
   signal sr_byteenable    : std_logic_vector(1 downto 0);
   signal sr_burstcount    : std_logic_vector(7 downto 0);
   signal sr_readdata      : std_logic_vector(15 downto 0);

   -- SDRAM pins and the chip's side of the board's delays
   constant C_TO_CHIP   : time := 4 ns;
   constant C_FROM_CHIP : time := 1.5 ns;
   constant C_TAC       : time := 5.4 ns;      -- clock to data out at CAS latency 3
   constant C_TOH       : time := 2.7 ns;      -- data out hold
   signal sd_a        : std_logic_vector(12 downto 0);
   signal sd_ba       : std_logic_vector( 1 downto 0);
   signal sd_cmd      : std_logic_vector( 3 downto 0);   -- CS#, RAS#, CAS#, WE#
   signal sd_dqm      : std_logic_vector( 1 downto 0);
   signal sd_clk      : std_logic;
   signal sd_dq_oe_n  : std_logic_vector(15 downto 0);
   signal sd_dq_out   : std_logic_vector(15 downto 0);
   signal sd_dq_w     : std_logic_vector(15 downto 0);
   signal sd_dq_in    : std_logic_vector(15 downto 0);
   signal c_clk       : std_logic := '0';
   signal c_a         : std_logic_vector(12 downto 0);
   signal c_ba        : std_logic_vector( 1 downto 0);
   signal c_cmd       : std_logic_vector( 3 downto 0) := "1111";
   signal c_dqm       : std_logic_vector( 1 downto 0) := "11";
   signal c_dq_w      : std_logic_vector(15 downto 0);
   signal c_dq_out    : std_logic_vector(15 downto 0) := (others => 'Z');

   -- Results
   signal n_cpu_chk      : natural := 0;
   signal n_cpu_bad      : natural := 0;
   signal n_vic_chk      : natural := 0;
   signal n_vic_bad      : natural := 0;
   signal cpu_wait_max   : natural := 0;   -- core clocks, any CPU or DMA read
   signal fast_wait_max  : natural := 0;   -- core clocks, FAST mode's read at clock 24
   signal vic_wait_max   : natural := 0;
   signal n_reu_cmd      : natural := 0;
   signal n_reu_outside  : natural := 0;
   signal n_reu_stray    : natural := 0;   -- REU accesses outside its memory
   signal n_reu_cross    : natural := 0;   -- REU read bursts that run past a row's end
   signal n_cart_cmd     : natural := 0;
   signal cart_hold_max  : natural := 0;   -- hr clocks a cartridge read waited for the SDRAM
   signal n_refresh      : natural := 0;

begin

   clk    <= not clk    after C_MAIN_PERIOD / 2 when running;
   hr_clk <= not hr_clk after C_HR_PERIOD / 2   when running;

   --------------------------------------------------------------------------------
   -- C128 bus model
   --------------------------------------------------------------------------------

   ext_cycle <= '1' when cyc >= 4 and cyc <= 7 else '0';
   phi2      <= '1' when cyc >= 7 and cyc <= 22 else '0';
   phi2_ph   <= (cyc + 25) mod 32 when phi2 = '1' else (cyc + 9) mod 32;
   sys_addr  <= dma_addr when dma_active = '1' else bus_addr;
   sys_we    <= dma_we   when dma_active = '1' else bus_we;
   cpu_cs    <= '1' when cyc >= 20 and cyc <= 23 and sys_addr(15 downto 5) = "11011111000" else '0';

   -- The DMA reads the cartridge's byte where the cartridge has the address, else RAM
   dma_din   <= hr_cpu_data when crt_active = '1' and (is_roml(dma_addr) or is_romh(dma_addr, umax)) else ram_q;

   p_bus : process (clk)
   begin
      if rising_edge(clk) then
         cyc <= (cyc + 1) mod 32;
         if (cyc = 1 or cyc = 9) and bus_we = '0' then
            dma_active <= dma_req;
         end if;
         if cyc = 15 then
            dma_cycle <= dma_active;
         end if;
         if cyc = 31 then
            dma_cycle <= '0';
         end if;
         if dma_we = '1' then
            c128_ram(to_integer(unsigned(dma_addr))) <= dma_dout;
         end if;
         ram_q <= c128_ram(to_integer(unsigned(dma_addr)));

         -- Ultimax: the VIC's PHI1 fetch from $3000-$3FFF (ROMH), a new address each cycle
         if cyc = 24 and vic_on = '1' then
            vic_addr <= x"3" & std_logic_vector(unsigned(vic_addr(11 downto 0)) + 37);
         end if;
      end if;
   end process p_bus;

   -- main.vhd's windows, registered as there: refreshes in the middle of PHI2 high, the REU in PHI2 low
   p_windows : process (clk)
   begin
      if rising_edge(clk) then
         crt_rfsh_ok <= '0';
         if crt_active = '0' or (phi2 = '1' and phi2_ph >= 3 and phi2_ph <= 10) then
            crt_rfsh_ok <= '1';
         end if;
         crt_reu_ok <= '0';
         if crt_active = '0' or (phi2 = '0' and phi2_ph >= G_REU_FROM and phi2_ph <= G_REU_TO) then
            crt_reu_ok <= '1';
         end if;
         main_rfsh_ok_r <= crt_rfsh_ok;
         main_reu_ok_r  <= crt_reu_ok;
      end if;
   end process p_windows;

   --------------------------------------------------------------------------------
   -- Cartridge requests, as in sw_cartridge_wrapper's sdram_gen
   --------------------------------------------------------------------------------

   cpu_rd       <= '1' when crt_active = '1' and (is_roml(sys_addr) or is_romh(sys_addr, umax) or is_io(sys_addr)) else '0';
   cpu_hi       <= '1' when is_romh(sys_addr, umax) else '0';
   main_cpu_key <= crt_key(cpu_hi, sys_addr);
   main_vic_key <= crt_key('1', vic_addr);

   p_request : process (clk)
      variable v_cnt : unsigned(2 downto 0);
   begin
      if rising_edge(clk) then
         if cpu_rd = '1' and (main_cpu_key /= main_cpu_key_r or main_cpu_ok = '0') then
            v_cnt          := main_cpu_cnt + 1;
            main_cpu_cnt   <= v_cnt;
            main_cpu_req_r <= std_logic_vector(v_cnt xor ('0' & v_cnt(2 downto 1)));
            main_cpu_key_r <= main_cpu_key;
            main_cpu_ok    <= '1';
         end if;
         if vic_on = '1' and crt_active = '1' and (main_vic_key /= main_vic_key_r or main_vic_ok = '0') then
            v_cnt          := main_vic_cnt + 1;
            main_vic_cnt   <= v_cnt;
            main_vic_req_r <= std_logic_vector(v_cnt xor ('0' & v_cnt(2 downto 1)));
            main_vic_key_r <= main_vic_key;
            main_vic_ok    <= '1';
         end if;
         if crt_active = '0' then
            main_cpu_ok <= '0';
            main_vic_ok <= '0';
         end if;
      end if;
   end process p_request;

   p_sync : process (hr_clk)
   begin
      if rising_edge(hr_clk) then
         hr_cpu_req_d <= main_cpu_req_r;
         hr_cpu_req   <= hr_cpu_req_d;
         hr_vic_req_d <= main_vic_req_r;
         hr_vic_req   <= hr_vic_req_d;
         hr_rfsh_ok_d <= main_rfsh_ok_r;
         hr_rfsh_ok   <= hr_rfsh_ok_d;
         hr_reu_ok_d  <= main_reu_ok_r;
         hr_reu_ok    <= hr_reu_ok_d;
      end if;
   end process p_sync;

   -- Two clocks before the C128 latches a byte, it must be the right one. Also the longest wait,
   -- counted as sw_cartridge_wrapper's p_latency does.
   p_check : process (clk)
      variable v_cpu    : natural := 0;
      variable v_vic    : natural := 0;
      variable v_req_q  : std_logic_vector(2 downto 0) := (others => '0');
      variable v_fast   : boolean := false;
   begin
      if rising_edge(clk) then
         if main_cpu_req_r /= v_req_q then
            v_req_q := main_cpu_req_r;
            v_fast  := fast = '1' and dma_active = '0' and cyc = 25;
         end if;
         if hr_cpu_done = main_cpu_req_r then
            v_cpu := 0;
         else
            v_cpu := v_cpu + 1;
         end if;
         if hr_vic_done = main_vic_req_r then
            v_vic := 0;
         else
            v_vic := v_vic + 1;
         end if;

         if chk_armed then
            if v_cpu > cpu_wait_max then
               cpu_wait_max <= v_cpu;
            end if;
            if v_fast and v_cpu > fast_wait_max then
               fast_wait_max <= v_cpu;
            end if;
            if v_vic > vic_wait_max then
               vic_wait_max <= v_vic;
            end if;

            if cpu_rd = '1' and ((dma_active = '1' and cyc = 29) or
                                 (dma_active = '0' and (cyc = 21 or (fast = '1' and cyc = 29)))) then
               n_cpu_chk <= n_cpu_chk + 1;
               if hr_cpu_done /= main_cpu_req_r or hr_cpu_data /= crt_byte(main_cpu_key_r) then
                  if n_cpu_bad < 8 then
                     report "cartridge read late or wrong at clock " & integer'image(cyc) &
                            ": $" & to_hstring(sys_addr) & " read $" & to_hstring(hr_cpu_data) &
                            ", expected $" & to_hstring(crt_byte(main_cpu_key_r)) severity error;
                  end if;
                  n_cpu_bad <= n_cpu_bad + 1;
               end if;
            end if;

            if vic_on = '1' and cyc = 5 then
               n_vic_chk <= n_vic_chk + 1;
               if hr_vic_done /= main_vic_req_r or hr_vic_data /= crt_byte(main_vic_key_r) then
                  if n_vic_bad < 8 then
                     report "VIC fetch late or wrong: $" & to_hstring(vic_addr) & " read $" &
                            to_hstring(hr_vic_data) & ", expected $" & to_hstring(crt_byte(main_vic_key_r))
                        severity error;
                  end if;
                  n_vic_bad <= n_vic_bad + 1;
               end if;
            end if;
         end if;
      end if;
   end process p_check;

   --------------------------------------------------------------------------------
   -- REU, wired as in main.vhd and mega65.vhd
   --------------------------------------------------------------------------------

   i_reu : entity work.reu
      generic map (
         REU_ADDR => 0
      )
      port map (
         clk       => clk,
         reset     => rst,
         cfg       => cfg,
         wrap      => '0',
         dma_req   => dma_req,
         dma_cycle => dma_cycle,
         dma_addr  => dma_addr,
         dma_dout  => dma_dout,
         dma_din   => dma_din,
         dma_we    => dma_we,
         ram_cycle => ram_cycle,
         ram_addr  => ram_addr,
         ram_dout  => ram_dout,
         ram_din   => ram_din,
         ram_we    => ram_we,
         ram_cs    => ram_cs,
         cpu_addr  => sys_addr,
         cpu_dout  => bus_wdata,
         cpu_din   => cpu_din,
         cpu_we    => sys_we,
         cpu_cs    => cpu_cs,
         irq       => irq
      ); -- i_reu

   ram_addr_m <= ram_addr and ("000000" & "111" & x"FFFF") when cfg = "01" else
                 ram_addr and ("0000" & "11111" & x"FFFF") when cfg = "10" else
                 ram_addr and ('0' & x"FFFFFF");

   i_reu_mapper : entity work.reu_mapper
      generic map (
         G_BASE_ADDRESS => std_logic_vector(to_unsigned(C_REU_W, 32))
      )
      port map (
         clk_i               => clk,
         rst_i               => rst,
         reu_ext_cycle_i     => ext_cycle,
         reu_ext_cycle_o     => ram_cycle,
         reu_addr_i          => ram_addr_m,
         reu_dout_i          => ram_dout,
         reu_din_o           => ram_din,
         reu_we_i            => ram_we,
         reu_cs_i            => ram_cs,
         avm_write_o         => map_write,
         avm_read_o          => map_read,
         avm_address_o       => map_address,
         avm_writedata_o     => map_writedata,
         avm_byteenable_o    => map_byteenable,
         avm_burstcount_o    => map_burstcount,
         avm_readdata_i      => map_readdata,
         avm_readdatavalid_i => map_readdatavalid,
         avm_waitrequest_i   => map_waitrequest
      ); -- i_reu_mapper

   i_avm_cache : entity work.avm_cache
      generic map (
         G_CACHE_SIZE   => 8,
         G_ADDRESS_SIZE => 32,
         G_DATA_SIZE    => 16
      )
      port map (
         clk_i                 => clk,
         rst_i                 => rst,
         s_avm_waitrequest_o   => map_waitrequest,
         s_avm_write_i         => map_write,
         s_avm_read_i          => map_read,
         s_avm_address_i       => map_address,
         s_avm_writedata_i     => map_writedata,
         s_avm_byteenable_i    => map_byteenable,
         s_avm_burstcount_i    => map_burstcount,
         s_avm_readdata_o      => map_readdata,
         s_avm_readdatavalid_o => map_readdatavalid,
         m_avm_waitrequest_i   => c_waitrequest,
         m_avm_write_o         => c_write,
         m_avm_read_o          => c_read,
         m_avm_address_o       => c_address,
         m_avm_writedata_o     => c_writedata,
         m_avm_byteenable_o    => c_byteenable,
         m_avm_burstcount_o    => c_burstcount,
         m_avm_readdata_i      => c_readdata,
         m_avm_readdatavalid_i => c_readdatavalid
      ); -- i_avm_cache

   i_avm_fifo : entity work.avm_fifo
      generic map (
         G_WR_DEPTH     => 16,
         G_RD_DEPTH     => 16,
         G_FILL_SIZE    => 1,
         G_ADDRESS_SIZE => 32,
         G_DATA_SIZE    => 16
      )
      port map (
         s_clk_i               => clk,
         s_rst_i               => rst,
         s_avm_waitrequest_o   => c_waitrequest,
         s_avm_write_i         => c_write,
         s_avm_read_i          => c_read,
         s_avm_address_i       => c_address,
         s_avm_writedata_i     => c_writedata,
         s_avm_byteenable_i    => c_byteenable,
         s_avm_burstcount_i    => c_burstcount,
         s_avm_readdata_o      => c_readdata,
         s_avm_readdatavalid_o => c_readdatavalid,
         m_clk_i               => hr_clk,
         m_rst_i               => hr_rst,
         m_avm_waitrequest_i   => h_waitrequest,
         m_avm_write_o         => h_write,
         m_avm_read_o          => h_read,
         m_avm_address_o       => h_address,
         m_avm_writedata_o     => h_writedata,
         m_avm_byteenable_o    => h_byteenable,
         m_avm_burstcount_o    => h_burstcount,
         m_avm_readdata_i      => h_readdata,
         m_avm_readdatavalid_i => h_readdatavalid
      ); -- i_avm_fifo

   --------------------------------------------------------------------------------
   -- Cartridge reader, the arbiter, the SDRAM controller and the chip
   --------------------------------------------------------------------------------

   i_crt_reader : entity work.crt_reader
      port map (
         clk_i                 => hr_clk,
         rst_i                 => hr_rst,
         cart_ready_i          => cart_ready,
         cart_size_i           => std_logic_vector(to_unsigned(16#4050#, 23)),
         cart_c128_i           => '0',
         cart_ef_i             => '0',
         cart_loading_i        => cart_loading,
         cart_bank_laddr_i     => x"8000",
         cart_bank_size_i      => x"4000",
         cart_bank_num_i       => x"0000",
         cart_bank_raddr_i     => std_logic_vector(to_unsigned(C_CRT_RADDR, 25)),
         cart_bank_wr_i        => cart_bank_wr,
         cpu_req_i             => hr_cpu_req,
         cpu_key_i             => main_cpu_key_r,
         cpu_data_o            => hr_cpu_data,
         cpu_done_o            => hr_cpu_done,
         vic_req_i             => hr_vic_req,
         vic_key_i             => main_vic_key_r,
         vic_data_o            => hr_vic_data,
         vic_done_o            => hr_vic_done,
         slot_ok_i             => hr_rfsh_ok,
         fl_req_i              => "00",
         fl_lo_job_i           => (others => '0'),
         fl_hi_job_i           => (others => '0'),
         fl_done_o             => open,
         fl_fail_o             => open,
         prefill_o             => open,
         s_avm_write_i         => '0',
         s_avm_read_i          => '0',
         s_avm_address_i       => (others => '0'),
         s_avm_writedata_i     => (others => '0'),
         s_avm_byteenable_i    => (others => '0'),
         s_avm_burstcount_i    => (others => '0'),
         s_avm_readdata_o      => open,
         s_avm_readdatavalid_o => open,
         s_avm_waitrequest_o   => open,
         m_avm_write_o         => k_write,
         m_avm_read_o          => k_read,
         m_avm_address_o       => k_address,
         m_avm_writedata_o     => k_writedata,
         m_avm_byteenable_o    => k_byteenable,
         m_avm_burstcount_o    => k_burstcount,
         m_avm_readdata_i      => k_readdata,
         m_avm_readdatavalid_i => k_readdatavalid,
         m_avm_waitrequest_i   => k_waitrequest
      ); -- i_crt_reader

   i_sdram_arbit : entity work.sdram_arbit
      port map (
         clk_i                  => hr_clk,
         rst_i                  => hr_rst,
         slot_ok_i              => hr_reu_ok,
         s0_avm_write_i         => k_write,
         s0_avm_read_i          => k_read,
         s0_avm_address_i       => k_address,
         s0_avm_writedata_i     => k_writedata,
         s0_avm_byteenable_i    => k_byteenable,
         s0_avm_burstcount_i    => k_burstcount,
         s0_avm_readdata_o      => k_readdata,
         s0_avm_readdatavalid_o => k_readdatavalid,
         s0_avm_waitrequest_o   => k_waitrequest,
         s1_avm_write_i         => h_write,
         s1_avm_read_i          => h_read,
         s1_avm_address_i       => h_address,
         s1_avm_writedata_i     => h_writedata,
         s1_avm_byteenable_i    => h_byteenable,
         s1_avm_burstcount_i    => h_burstcount,
         s1_avm_readdata_o      => h_readdata,
         s1_avm_readdatavalid_o => h_readdatavalid,
         s1_avm_waitrequest_o   => h_waitrequest,
         m_avm_write_o          => sr_write,
         m_avm_read_o           => sr_read,
         m_avm_address_o        => sr_address,
         m_avm_writedata_o      => sr_writedata,
         m_avm_byteenable_o     => sr_byteenable,
         m_avm_burstcount_o     => sr_burstcount,
         m_avm_readdata_i       => sr_readdata,
         m_avm_readdatavalid_i  => sr_readdatavalid,
         m_avm_waitrequest_i    => sr_waitrequest
      ); -- i_sdram_arbit

   i_sdram : entity work.sdram
      generic map (
         G_CLOCK_SPEED_MHZ => 100,
         G_REFRESH_HOLD    => 200
      )
      port map (
         clk_i               => hr_clk,
         rst_i               => hr_rst,
         refresh_ok_i        => hr_rfsh_ok,
         avm_waitrequest_o   => sr_waitrequest,
         avm_write_i         => sr_write,
         avm_read_i          => sr_read,
         avm_address_i       => sr_address,
         avm_writedata_i     => sr_writedata,
         avm_byteenable_i    => sr_byteenable,
         avm_burstcount_i    => sr_burstcount,
         avm_readdata_o      => sr_readdata,
         avm_readdatavalid_o => sr_readdatavalid,
         sdram_a_o           => sd_a,
         sdram_ba_o          => sd_ba,
         sdram_cas_n_o       => sd_cmd(1),
         sdram_cke_o         => open,
         sdram_clk_o         => sd_clk,
         sdram_cs_n_o        => sd_cmd(3),
         sdram_dq_in_i       => sd_dq_in,
         sdram_dqmh_o        => sd_dqm(1),
         sdram_dqml_o        => sd_dqm(0),
         sdram_dq_oe_n_o     => sd_dq_oe_n,
         sdram_dq_out_o      => sd_dq_out,
         sdram_ras_n_o       => sd_cmd(2),
         sdram_we_n_o        => sd_cmd(0)
      ); -- i_sdram

   sd_dq_w  <= sd_dq_out when sd_dq_oe_n(0) = '0' else (others => 'Z');
   c_clk    <= transport sd_clk  after C_TO_CHIP;
   c_a      <= transport sd_a    after C_TO_CHIP;
   c_ba     <= transport sd_ba   after C_TO_CHIP;
   c_cmd    <= transport sd_cmd  after C_TO_CHIP;
   c_dqm    <= transport sd_dqm  after C_TO_CHIP;
   c_dq_w   <= transport sd_dq_w after C_TO_CHIP;
   sd_dq_in <= transport c_dq_out after C_FROM_CHIP;

   -- tb_crt's IS42S16320 model. Full-page bursts, as the controller sets the chip up: a READ or
   -- WRITE runs on a column a clock until BURST STOP or PRECHARGE, and wraps within its row.
   p_chip : process (c_clk)
      type t_rows is array (0 to 3) of natural;
      type t_pipe is array (0 to 3) of integer;
      variable rows    : t_rows := (others => 0);
      variable cl      : natural := 3;
      variable rd_on   : boolean := false;
      variable wr_on   : boolean := false;
      variable bank    : natural := 0;
      variable col     : natural := 0;
      variable pipe    : t_pipe := (others => -1);   -- word addresses, -1 for none
      variable dqm_d   : std_logic_vector(1 downto 0) := "11";
      variable fetch   : integer;
      variable w       : std_logic_vector(15 downto 0);
      variable cmd     : std_logic_vector(2 downto 0);
   begin
      if rising_edge(c_clk) then
         fetch := -1;
         cmd   := c_cmd(2 downto 0);
         if c_cmd(3) = '0' and cmd /= "111" then
            rd_on := false;
            wr_on := false;
            case cmd is
               when "011" =>                                     -- ACTIVE
                  rows(to_integer(unsigned(c_ba))) := to_integer(unsigned(c_a));
               when "101" | "100" =>                             -- READ, WRITE
                  bank  := to_integer(unsigned(c_ba));
                  col   := to_integer(unsigned(c_a(9 downto 0)));
                  rd_on := c_cmd(0) = '1';
                  wr_on := c_cmd(0) = '0';
               when "000" =>                                     -- MODE REGISTER SET
                  cl := to_integer(unsigned(c_a(6 downto 4)));
                  assert cl = 3 and c_a(2 downto 0) = "111"
                     report "SDRAM model: expects CAS latency 3 and full-page bursts" severity failure;
               when "001" =>                                     -- AUTO REFRESH
                  n_refresh <= n_refresh + 1;
               when others =>                                    -- PRECHARGE, BURST STOP
                  null;
            end case;
         elsif rd_on or wr_on then
            col := (col + 1) mod 1024;
         end if;

         if rd_on then
            fetch := bank * 2**23 + rows(bank) * 2**10 + col;
         elsif wr_on then
            w := std_logic_vector(to_unsigned(sdram_mem.get(bank * 2**23 + rows(bank) * 2**10 + col), 16));
            if c_dqm(0) = '0' then
               w(7 downto 0) := c_dq_w(7 downto 0);
            end if;
            if c_dqm(1) = '0' then
               w(15 downto 8) := c_dq_w(15 downto 8);
            end if;
            sdram_mem.put(bank * 2**23 + rows(bank) * 2**10 + col, w);
         end if;

         -- Drive the word that is due at the next edge
         for i in 3 downto 1 loop
            pipe(i) := pipe(i - 1);
         end loop;
         pipe(0) := fetch;
         c_dq_out <= transport (others => 'X') after C_TOH;
         if pipe(cl - 1) >= 0 then
            w := std_logic_vector(to_unsigned(sdram_mem.get(pipe(cl - 1)), 16));
            if dqm_d(0) = '1' then
               w(7 downto 0) := (others => 'Z');
            end if;
            if dqm_d(1) = '1' then
               w(15 downto 8) := (others => 'Z');
            end if;
            c_dq_out <= transport w after C_TAC;
         else
            c_dq_out <= transport (others => 'Z') after C_TAC;
         end if;
         dqm_d := c_dqm;
      end if;
   end process p_chip;

   -- Every REU access must start in its window and stay in the REU's memory: its size, and the
   -- read cache's look-ahead of up to 8 words. And how long a cartridge read waits for the SDRAM
   -- once crt_reader asks: behind a refresh, or an REU access already under way.
   p_hr_mon : process (hr_clk)
      variable v_hold : natural := 0;
      variable v_addr : natural;
      variable v_size : natural;
   begin
      if rising_edge(hr_clk) then
         if (h_read = '1' or h_write = '1') and h_waitrequest = '0' then
            n_reu_cmd <= n_reu_cmd + 1;
            if hr_reu_ok = '0' then
               n_reu_outside <= n_reu_outside + 1;
            end if;
            case cfg is
               when "01"   => v_size := 16#40000#;
               when "10"   => v_size := 16#100000#;
               when others => v_size := 16#800000#;
            end case;
            v_addr := to_integer(unsigned(h_address(24 downto 0)));
            if h_read = '1' and v_addr mod 1024 + to_integer(unsigned(h_burstcount)) > 1024 then
               n_reu_cross <= n_reu_cross + 1;
            end if;
            if v_addr < C_REU_W or v_addr + to_integer(unsigned(h_burstcount)) > C_REU_W + v_size + 8 then
               if n_reu_stray < 4 then
                  report "REU access outside its memory: word $" & to_hstring(h_address(24 downto 0)) severity error;
               end if;
               n_reu_stray <= n_reu_stray + 1;
            end if;
         end if;
         if k_read = '1' or k_write = '1' then
            if k_waitrequest = '0' then
               n_cart_cmd <= n_cart_cmd + 1;
               if v_hold > cart_hold_max then
                  cart_hold_max <= v_hold;
               end if;
               v_hold := 0;
            else
               v_hold := v_hold + 1;
            end if;
         end if;
      end if;
   end process p_hr_mon;

   --------------------------------------------------------------------------------
   -- Test sequence
   --------------------------------------------------------------------------------

   p_test : process
      variable errors  : natural := 0;
      variable d       : std_logic_vector(7 downto 0);
      variable l       : line;
      variable slot_b  : boolean := false;                  -- FAST mode: the next access is at 24-31
      variable pc      : unsigned(15 downto 0) := x"0400";  -- where the CPU fetches its program
      variable n_row   : natural;

      procedure check(ok : boolean; msg : string) is
      begin
         if not ok then
            report msg severity error;
            errors := errors + 1;
         end if;
      end procedure check;

      -- Line the CPU's next access up with the cycle: it starts at the end of clock 23
      procedure sync_slots is
      begin
         wait until rising_edge(clk) and cyc = 23;
         slot_b := fast = '1';
      end procedure sync_slots;

      -- One CPU access, from the edge its address goes out to the edge it is latched. I/O only
      -- happens at 1 MHz, so in FAST mode it waits out a 24-31 slot with the old address.
      procedure cpu_step(addr : std_logic_vector(15 downto 0); we : std_logic;
                         wdata : std_logic_vector(7 downto 0);
                         rdata : out std_logic_vector(7 downto 0)) is
      begin
         if slot_b and is_io(addr) then
            wait until rising_edge(clk) and cyc = 31;
            slot_b := false;
         end if;
         bus_addr  <= addr;
         bus_we    <= we;
         bus_wdata <= wdata;
         if slot_b then
            wait until rising_edge(clk) and cyc = 31;
            slot_b := false;
         else
            wait until rising_edge(clk) and cyc = 23;
            slot_b := fast = '1';
         end if;
         if addr(15 downto 5) = "11011111000" then
            rdata := cpu_din;
         elsif crt_active = '1' and (is_roml(addr) or is_romh(addr, umax)) then
            rdata := hr_cpu_data;
         else
            rdata := c128_ram(to_integer(unsigned(addr)));
         end if;
         bus_we <= '0';
      end procedure cpu_step;

      -- The CPU runs its program: from the cartridge while one is in, else from RAM
      procedure exec(n : natural) is
         variable dummy : std_logic_vector(7 downto 0);
      begin
         for i in 1 to n loop
            cpu_step(std_logic_vector(pc), '0', x"00", dummy);
            pc := pc(15 downto 13) & (pc(12 downto 0) + 1);
         end loop;
      end procedure exec;

      procedure reu_wr(reg : natural; data : std_logic_vector(7 downto 0)) is
         variable dummy : std_logic_vector(7 downto 0);
      begin
         cpu_step(x"DF" & std_logic_vector(to_unsigned(reg, 8)), '1', data, dummy);
         exec(2);
      end procedure reu_wr;

      procedure setup(c128 : natural; reu : natural; len : natural) is
         variable a : unsigned(15 downto 0) := to_unsigned(c128, 16);
         variable r : unsigned(23 downto 0) := to_unsigned(reu, 24);
         variable n : unsigned(15 downto 0) := to_unsigned(len, 16);
      begin
         reu_wr(2, std_logic_vector(a( 7 downto  0)));
         reu_wr(3, std_logic_vector(a(15 downto  8)));
         reu_wr(4, std_logic_vector(r( 7 downto  0)));
         reu_wr(5, std_logic_vector(r(15 downto  8)));
         reu_wr(6, std_logic_vector(r(23 downto 16)));
         reu_wr(7, std_logic_vector(n( 7 downto  0)));
         reu_wr(8, std_logic_vector(n(15 downto  8)));
      end procedure setup;

      -- A real REU moves a byte per cycle, two for SWAP; allow a few cycles at the start
      procedure transfer(cmd : std_logic_vector(7 downto 0); what : string; len : natural;
                         per : natural := 1) is
         variable n     : natural := 0;
         variable dummy : std_logic_vector(7 downto 0);
      begin
         cpu_step(x"DF01", '1', cmd, dummy);
         check(dma_req = '1', what & ": no DMA request after the command");
         while dma_req = '1' or dma_active = '1' loop
            wait until rising_edge(clk) and cyc = 31;
            n := n + 1;
            if n > 20 * len + 100 then
               check(false, what & ": transfer did not finish");
               exit;
            end if;
         end loop;
         write(l, what & ": " & integer'image(len) & " bytes in " & integer'image(n) & " C128 cycles");
         writeline(output, l);
         check(n <= per * len + 4, what & ": too slow");
         sync_slots;
         exec(2);
      end procedure transfer;

      procedure compare(a : natural; b : natural; len : natural; what : string) is
         variable bad : natural := 0;
      begin
         for i in 0 to len - 1 loop
            if c128_ram(a + i) /= c128_ram(b + i) then
               if bad < 4 then
                  report what & ": $" & to_hstring(to_unsigned(a + i, 16)) & " = $" & to_hstring(c128_ram(a + i)) &
                         ", $" & to_hstring(to_unsigned(b + i, 16)) & " = $" & to_hstring(c128_ram(b + i))
                     severity error;
               end if;
               bad := bad + 1;
            end if;
         end loop;
         check(bad = 0, what & ": " & integer'image(bad) & " bytes differ");
      end procedure compare;

      -- RAM against the cartridge's ROML from offset off
      procedure compare_crt(a : natural; off : natural; len : natural; what : string) is
         variable bad : natural := 0;
         variable e   : std_logic_vector(7 downto 0);
      begin
         for i in 0 to len - 1 loop
            e := crt_byte(crt_key('0', std_logic_vector(to_unsigned(16#8000# + off + i, 16))));
            if c128_ram(a + i) /= e then
               if bad < 4 then
                  report what & ": $" & to_hstring(to_unsigned(a + i, 16)) & " = $" & to_hstring(c128_ram(a + i)) &
                         ", expected $" & to_hstring(e) severity error;
               end if;
               bad := bad + 1;
            end if;
         end loop;
         check(bad = 0, what & ": " & integer'image(bad) & " bytes differ");
      end procedure compare_crt;

      procedure reset_reu(new_cfg : std_logic_vector(1 downto 0)) is
      begin
         wait until rising_edge(clk);
         rst <= '1';
         cfg <= new_cfg;
         for i in 1 to 64 loop
            wait until rising_edge(clk);
         end loop;
         rst <= '0';
         sync_slots;
         exec(2);
      end procedure reset_reu;

      -- A new mode for the C128: cartridge in or out, Ultimax with the VIC fetching ROMH, FAST
      procedure mode(crt : std_logic; u : std_logic; f : std_logic; what : string) is
      begin
         write(l, string'("--- ") & what);
         writeline(output, l);
         chk_armed <= false;
         wait until rising_edge(clk) and cyc = 23;
         crt_active <= crt;
         umax       <= u;
         fast       <= f;
         vic_on     <= crt and u and not f;   -- the VIC has no PHI1 fetches in FAST mode
         if crt = '0' then
            pc := x"0400";
         elsif u = '1' then
            pc := x"E000";
         else
            pc := x"8000";
         end if;
         -- Let the windows and the first requests settle before anything is checked
         for i in 1 to 4 loop
            wait until rising_edge(clk) and cyc = 31;
         end loop;
         chk_armed <= crt = '1';
         sync_slots;
         exec(2);
      end procedure mode;

      -- The REU traffic every cartridge section runs. Nothing crosses an SDRAM row here.
      procedure reu_traffic(what : string; base : natural) is
      begin
         -- The DMA reads the cartridge: STASH 512 bytes of ROML, FETCH them back to RAM
         setup(16#8000#, base, 512);
         transfer(x"90", what & " STASH from the cartridge", 512);
         setup(16#4000#, base, 512);
         transfer(x"91", what & " FETCH it back", 512);
         compare_crt(16#4000#, 0, 512, what & " cartridge bytes through the REU");

         -- RAM through the REU, then VERIFY (equal, so it runs its full length) and SWAP
         setup(16#1000#, base + 16#800#, 256);
         transfer(x"90", what & " STASH from RAM", 256);
         setup(16#3000#, base + 16#800#, 256);
         transfer(x"91", what & " FETCH from RAM", 256);
         compare(16#1000#, 16#3000#, 256, what & " RAM through the REU");
         setup(16#1000#, base + 16#800#, 128);
         transfer(x"93", what & " VERIFY", 128);
         setup(16#5000#, base + 16#800#, 128);
         transfer(x"92", what & " SWAP", 128, 2);
      end procedure reu_traffic;

   begin
      sdram_mem.fill;
      hr_rst <= '1';
      rst    <= '1';
      wait for 1 us;
      wait until rising_edge(hr_clk);
      hr_rst <= '0';

      -- crt_reader: a 16 kB C64 cartridge in bank 0, its file where C_CRT_RADDR says
      cart_loading <= '1';
      for i in 1 to 4 loop
         wait until rising_edge(hr_clk);
      end loop;
      cart_loading <= '0';
      wait until rising_edge(hr_clk);
      cart_bank_wr <= '1';
      wait until rising_edge(hr_clk);
      cart_bank_wr <= '0';
      cart_ready   <= '1';

      -- The SDRAM controller's power-up takes 102 us
      wait for 110 us;
      reset_reu("10");

      ------------------------------------------------------------------------
      -- No cartridge: the REU has the SDRAM to itself
      ------------------------------------------------------------------------
      mode('0', '0', '0', "REU alone, 1 MHz");
      setup(16#1000#, 16#000100#, 256);
      transfer(x"90", "STASH", 256);
      setup(16#3000#, 16#000100#, 256);
      transfer(x"91", "FETCH", 256);
      compare(16#1000#, 16#3000#, 256, "REU alone");
      setup(16#5000#, 16#000100#, 128);
      transfer(x"92", "SWAP", 128, 2);

      -- From an odd byte in the row's last words across the 2 KiB row end: the first read is an
      -- 8-word burst from column 1018
      setup(16#1000#, 16#0007F5#, 64);
      transfer(x"90", "STASH across a row end", 64);
      -- Another read first, so the FETCH misses the read cache: the STASH's writes updated it
      setup(16#3300#, 16#040000#, 16);
      transfer(x"91", "FETCH elsewhere", 16);
      setup(16#3100#, 16#0007F5#, 64);
      transfer(x"91", "FETCH across a row end", 64);
      n_row := 0;
      for i in 0 to 63 loop
         if c128_ram(16#3100# + i) /= c128_ram(16#1000# + i) then
            n_row := n_row + 1;
         end if;
      end loop;
      write(l, string'("row crossing: ") & integer'image(n_row) & " of 64 bytes wrong, " &
               integer'image(n_reu_cross) & " read bursts ran past a row's end");
      writeline(output, l);
      check(n_row = 0, "FETCH across an SDRAM row end read the wrong row");
      check(n_reu_cross > 0, "no read burst ran past a row's end, so the row test proved nothing");

      ------------------------------------------------------------------------
      -- A cartridge in: 1 MHz, FAST, and Ultimax with the VIC fetching ROMH
      ------------------------------------------------------------------------
      mode('1', '0', '0', "16 kB cartridge, 1 MHz");
      reu_traffic("1 MHz", 16#010000#);

      mode('1', '0', '1', "16 kB cartridge, FAST");
      reu_traffic("FAST", 16#020000#);

      mode('1', '1', '0', "Ultimax, VIC fetching ROMH");
      reu_traffic("Ultimax", 16#030000#);

      -- FAST again from several REU addresses, so refreshes and the clocks' phase land everywhere
      mode('1', '0', '1', "FAST, more addresses");
      for i in 0 to 5 loop
         setup(16#1000# + i * 64, 16#040000# + i * 16#1000# + i * 6, 192);
         transfer(x"90", "FAST STASH " & integer'image(i), 192);
         setup(16#6000#, 16#040000# + i * 16#1000# + i * 6, 192);
         transfer(x"91", "FAST FETCH " & integer'image(i), 192);
         compare(16#1000# + i * 64, 16#6000#, 192, "FAST pass " & integer'image(i));
      end loop;

      ------------------------------------------------------------------------
      -- 16 MB: high addresses, a transfer across the top that wraps to the start, and the
      -- cartridge beside it. Then 512 KB across its top.
      ------------------------------------------------------------------------
      mode('0', '0', '0', "16 MB REU alone");
      reset_reu("11");
      setup(16#1000#, 16#A54321#, 256);
      transfer(x"90", "16 MB STASH high", 256);
      setup(16#3000#, 16#A54321#, 256);
      transfer(x"91", "16 MB FETCH high", 256);
      compare(16#1000#, 16#3000#, 256, "16 MB high");
      setup(16#5000#, 16#A54321#, 128);
      transfer(x"92", "16 MB SWAP high", 128, 2);
      setup(16#1200#, 16#FFFFC0#, 128);
      transfer(x"90", "16 MB STASH across the top", 128);
      setup(16#3200#, 16#FFFFC0#, 128);
      transfer(x"91", "16 MB FETCH across the top", 128);
      compare(16#1200#, 16#3200#, 128, "16 MB across the top");
      check(std_logic_vector(to_unsigned(sdram_mem.get(C_REU_W) mod 256, 8)) = c128_ram(16#1240#),
            "16 MB: REU $000000 is not where the transfer wrapped to");

      mode('1', '0', '1', "16 MB REU, FAST cartridge");
      reu_traffic("16 MB FAST", 16#C00000#);

      mode('0', '0', '0', "512 KB REU across its top");
      reset_reu("01");
      setup(16#1400#, 16#07FFE0#, 64);
      transfer(x"90", "512 KB STASH across the top", 64);
      setup(16#3400#, 16#07FFE0#, 64);
      transfer(x"91", "512 KB FETCH across the top", 64);
      compare(16#1400#, 16#3400#, 64, "512 KB across the top");
      check(std_logic_vector(to_unsigned(sdram_mem.get(C_REU_W) mod 256, 8)) = c128_ram(16#1420#),
            "512 KB: REU $000000 is not where the transfer wrapped to");

      chk_armed <= false;
      wait until rising_edge(clk);

      ------------------------------------------------------------------------
      write(l, string'("cartridge reads: CPU and DMA ") & integer'image(n_cpu_chk) & " checked, " &
               integer'image(n_cpu_bad) & " late or wrong; VIC " & integer'image(n_vic_chk) &
               " checked, " & integer'image(n_vic_bad) & " late or wrong");
      writeline(output, l);
      write(l, string'("longest waits in core clocks: FAST read at 24 ") & integer'image(fast_wait_max) &
               " (limit 5), any CPU or DMA read " & integer'image(cpu_wait_max) &
               ", VIC " & integer'image(vic_wait_max));
      writeline(output, l);
      write(l, string'("SDRAM: ") & integer'image(n_cart_cmd) & " cartridge accesses (longest hold " &
               integer'image(cart_hold_max) & " hr clocks), " & integer'image(n_reu_cmd) &
               " REU accesses, " & integer'image(n_reu_outside) & " outside the window, " &
               integer'image(n_reu_stray) & " outside its memory, " &
               integer'image(n_refresh) & " refreshes");
      writeline(output, l);
      check(n_cpu_bad = 0, "cartridge reads were late or wrong");
      check(n_vic_bad = 0, "VIC fetches were late or wrong");
      check(fast_wait_max <= 5, "a FAST read waited longer than 5 core clocks");
      check(n_reu_outside = 0, "the REU started an access outside its window");
      check(n_reu_stray = 0, "the REU reached outside its memory");
      check(n_cpu_chk > 1000 and n_vic_chk > 1000, "too few cartridge reads were checked");

      if errors = 0 then
         write(l, string'("ALL CHECKS PASSED"));
         writeline(output, l);
      else
         write(l, string'("CHECKS FAILED: ") & integer'image(errors));
         writeline(output, l);
      end if;

      running <= false;
      wait;
   end process p_test;

end architecture sim;
