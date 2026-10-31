----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Testbench for the system CPU's bus DMA (CORE/vhdl/c128_dma.vhd). A model of the core's
-- DMA port, as fpga64_sid_iec.vhd has it: 32 clocks a C128 cycle, the request taken once a
-- cycle, then a 16-clock window in which the address is decoded, a read's data is there and
-- a write lands at its end. A firmware process drives the device bus as rv_soc.vhd does.
-- Checks:
--   1. idle: no request, status 0; the Z80 bit follows the core
--   2. hold: the request goes up, the CPU-stopped bit once a window came; down again with 0
--   3. a read job of 256 bytes: the buffer holds memory; busy right after GO, about 1 us a byte
--   4. a write job of 2048 bytes up to $FFFF: memory holds the buffer, the byte before is kept
--   5. a job's address and write enable stay for its whole window; no write outside one
--   6. another master (REU, cartridge) takes the windows for a while: the job waits, then ends
--      with the right bytes
--   7. stopping a 2048-byte job ends it at once and drops the request
--   8. a C128 reset clears hold
--   9. with the Z80 running no window comes: the job waits, and stopping it still works
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_c128_dma is
end entity tb_c128_dma;

architecture sim of tb_c128_dma is

   constant C_MAIN_PERIOD  : time := 31.7 ns;   -- 31.53 MHz
   constant C_QNICE_PERIOD : time := 20 ns;     -- 50 MHz

   constant W_HOLD   : natural := 16#000#;
   constant W_ADDR   : natural := 16#001#;
   constant W_LEN    : natural := 16#002#;
   constant W_GO     : natural := 16#003#;
   constant W_BUF    : natural := 16#800#;

   type t_mem is array (0 to 65535) of std_logic_vector(7 downto 0);

   function pattern(i : natural; seed : natural) return natural is
   begin
      return (i * 7 + seed + i / 256) mod 256;
   end function pattern;

   function init_mem return t_mem is
      variable m : t_mem;
   begin
      for i in m'range loop
         m(i) := std_logic_vector(to_unsigned(pattern(i, 3), 8));
      end loop;
      return m;
   end function init_mem;

   signal main_clk  : std_logic := '0';
   signal qnice_clk : std_logic := '0';
   signal main_rst  : std_logic := '1';
   signal qnice_rst : std_logic := '1';
   signal done      : boolean := false;

   signal c128_rst  : std_logic := '0';
   signal req       : std_logic;
   signal cycle     : std_logic := '0';           -- the window, as main.vhd hands it on
   signal addr      : unsigned(15 downto 0);
   signal dout      : unsigned(7 downto 0);
   signal we        : std_logic;
   signal din       : unsigned(7 downto 0) := x"FF";
   signal z80       : std_logic := '0';
   signal steal     : boolean := false;           -- the REU or a cartridge has the port

   signal q_addr    : std_logic_vector(11 downto 0) := (others => '0');
   signal q_wdata   : std_logic_vector(15 downto 0) := (others => '0');
   signal q_rdata   : std_logic_vector(15 downto 0);
   signal q_ce      : std_logic := '0';
   signal q_we      : std_logic := '0';

   signal mem        : t_mem := init_mem;
   signal windows    : natural := 0;              -- windows the engine was handed
   signal bad_hold   : boolean := false;          -- address or write enable moved in a window
   signal bad_write  : boolean := false;          -- a write outside a window

begin

   main_clk  <= not main_clk  after C_MAIN_PERIOD / 2  when not done;
   qnice_clk <= not qnice_clk after C_QNICE_PERIOD / 2 when not done;

   i_dut : entity work.c128_dma
      port map (
         main_clk_i      => main_clk,
         main_rst_i      => main_rst,
         main_c128_rst_i => c128_rst,
         main_req_o      => req,
         main_cycle_i    => cycle,
         main_addr_o     => addr,
         main_dout_o     => dout,
         main_we_o       => we,
         main_din_i      => din,
         main_z80_i      => z80,
         qnice_clk_i     => qnice_clk,
         qnice_rst_i     => qnice_rst,
         qnice_addr_i    => q_addr,
         qnice_data_i    => q_wdata,
         qnice_ce_i      => q_ce,
         qnice_we_i      => q_we,
         qnice_data_o    => q_rdata
      ); -- i_dut

   ---------------------------------------------------------------------------------------------
   -- The core: the request is taken at clock 4 of a cycle, the window is clocks 16-31
   ---------------------------------------------------------------------------------------------

   p_core : process (main_clk)
      variable sys     : natural range 0 to 31 := 0;
      variable active  : boolean := false;
      variable w_addr  : unsigned(15 downto 0);
      variable w_we    : std_logic;
      variable in_win  : boolean := false;
   begin
      if rising_edge(main_clk) then
         sys := (sys + 1) mod 32;
         if sys = 4 then
            active := req = '1' and z80 = '0';
         end if;

         -- What the engine showed in the clock that just ended; a write lands at the
         -- window's end, as the CPU's does at PHI2's fall
         if cycle = '1' then
            if not in_win then
               w_addr := addr;
               w_we   := we;
               in_win := true;
            elsif addr /= w_addr or we /= w_we then
               bad_hold <= true;
            end if;
            din <= unsigned(mem(to_integer(addr)));
            if sys = 0 then
               if we = '1' then
                  mem(to_integer(addr)) <= std_logic_vector(dout);
               end if;
               in_win := false;
            end if;
         elsif we = '1' then
            bad_write <= true;
         end if;

         -- Another master takes whole windows
         if sys = 16 and active and not steal then
            cycle   <= '1';
            windows <= windows + 1;
         elsif sys = 0 then
            cycle   <= '0';
         end if;
      end if;
   end process p_core;

   ---------------------------------------------------------------------------------------------
   -- Firmware
   ---------------------------------------------------------------------------------------------

   p_fw : process
      -- rv_soc: ce from a rising edge, devices act on the falling edge, ce low after
      procedure dev(a : natural; w : std_logic; d : natural; r : out natural) is
      begin
         wait until rising_edge(qnice_clk);
         q_ce    <= '1';
         q_we    <= w;
         q_addr  <= std_logic_vector(to_unsigned(a, 12));
         q_wdata <= std_logic_vector(to_unsigned(d, 16));
         wait until rising_edge(qnice_clk);
         r    := to_integer(unsigned(q_rdata));
         q_ce <= '0';
         q_we <= '0';
      end procedure dev;

      procedure wr(a : natural; d : natural) is
         variable dummy : natural;
      begin
         dev(a, '1', d, dummy);
      end procedure wr;

      variable n, st : natural;
      variable t0    : time;

      procedure job(a : natural; len : natural; write : boolean) is
      begin
         wr(W_ADDR, a);
         wr(W_LEN, len);
         if write then
            wr(W_GO, 1);
         else
            wr(W_GO, 0);
         end if;
      end procedure job;

      procedure wait_idle(limit : time; what : string) is
         variable t : time := now;
      begin
         loop
            dev(W_GO, '0', 0, st);
            exit when st mod 2 = 0;
            assert now - t < limit report what & ": still busy after " & time'image(now - t)
               severity failure;
         end loop;
      end procedure wait_idle;

   begin
      wait for 200 ns;
      main_rst  <= '0';
      qnice_rst <= '0';
      for i in 1 to 5 loop
         wait until rising_edge(qnice_clk);
      end loop;

      -- 1.
      dev(W_GO, '0', 0, st);
      assert st = 0 report "1: status " & integer'image(st) severity error;
      assert req = '0' report "1: request while idle" severity error;
      z80 <= '1';
      wait for 200 ns;
      dev(W_GO, '0', 0, st);
      assert st = 4 report "1: Z80 status " & integer'image(st) severity error;
      z80 <= '0';
      wait for 200 ns;

      -- 2.
      wr(W_HOLD, 1);
      wait for 200 ns;
      assert req = '1' report "2: no request with hold" severity error;
      wait for 2 us;
      dev(W_GO, '0', 0, st);
      assert st = 2 report "2: status with hold " & integer'image(st) severity error;
      dev(W_HOLD, '0', 0, n);
      assert n = 1 report "2: hold reads " & integer'image(n) severity error;
      wr(W_HOLD, 0);
      wait for 200 ns;
      assert req = '0' report "2: request after hold 0" severity error;
      dev(W_GO, '0', 0, st);
      assert st = 0 report "2: status after hold " & integer'image(st) severity error;

      -- 3.
      t0 := now;
      job(16#1000#, 256, false);
      dev(W_GO, '0', 0, st);
      assert st mod 2 = 1 report "3: not busy right after GO" severity error;
      wait_idle(1 ms, "3");
      report "3: 256 bytes in " & time'image(now - t0);
      assert now - t0 > 250 us and now - t0 < 270 us report "3: took " & time'image(now - t0)
         severity error;
      for i in 0 to 255 loop
         dev(W_BUF + i, '0', 0, n);
         assert n = pattern(16#1000# + i, 3)
            report "3: byte " & integer'image(i) & " = " & integer'image(n) severity error;
      end loop;
      assert req = '0' report "3: request after the job" severity error;

      -- 4.
      for i in 0 to 2047 loop
         wr(W_BUF + i, pattern(i, 99));
      end loop;
      job(16#F800#, 2048, true);
      wait_idle(3 ms, "4");
      for i in 0 to 2047 loop
         assert to_integer(unsigned(mem(16#F800# + i))) = pattern(i, 99)
            report "4: memory at " & integer'image(16#F800# + i) severity error;
      end loop;
      assert to_integer(unsigned(mem(16#F7FF#))) = pattern(16#F7FF#, 3)
         report "4: the byte before the job changed" severity error;

      -- 6.
      job(16#2000#, 64, false);
      wait for 20 us;
      steal <= true;
      n := windows;
      wait for 30 us;
      assert windows = n report "6: windows while another master has the port" severity error;
      dev(W_GO, '0', 0, st);
      assert st mod 2 = 1 report "6: done while another master had the port" severity error;
      steal <= false;
      wait_idle(1 ms, "6");
      for i in 0 to 63 loop
         dev(W_BUF + i, '0', 0, n);
         assert n = pattern(16#2000# + i, 3)
            report "6: byte " & integer'image(i) & " = " & integer'image(n) severity error;
      end loop;

      -- 7.
      job(16#0000#, 2048, false);
      wait for 50 us;
      wr(W_GO, 2);
      wait for 300 ns;
      dev(W_GO, '0', 0, st);
      assert st = 0 report "7: status after stop " & integer'image(st) severity error;
      assert req = '0' report "7: request after stop" severity error;
      -- and the next job runs as usual
      job(16#3000#, 16, false);
      wait_idle(100 us, "7");
      for i in 0 to 15 loop
         dev(W_BUF + i, '0', 0, n);
         assert n = pattern(16#3000# + i, 3) report "7: next job's byte " & integer'image(i)
            severity error;
      end loop;

      -- 8.
      wr(W_HOLD, 1);
      wait for 2 us;
      wait until rising_edge(main_clk);
      c128_rst <= '1';
      wait until rising_edge(main_clk);
      c128_rst <= '0';
      wait for 300 ns;
      dev(W_HOLD, '0', 0, n);
      assert n = 0 report "8: hold after a C128 reset" severity error;
      assert req = '0' report "8: request after a C128 reset" severity error;

      -- 9.
      z80 <= '1';
      job(16#4000#, 8, false);
      wait for 50 us;
      dev(W_GO, '0', 0, st);
      assert st = 5 report "9: status with the Z80 " & integer'image(st) severity error;
      wr(W_GO, 2);
      wait for 300 ns;
      dev(W_GO, '0', 0, st);
      assert st = 4 report "9: status after stop " & integer'image(st) severity error;
      z80 <= '0';

      -- 5.
      assert not bad_hold report "5: address or write enable moved in a window" severity error;
      assert not bad_write report "5: a write outside a window" severity error;

      report "ALL CHECKS PASSED";
      done <= true;
      wait;
   end process p_fw;

end architecture sim;
