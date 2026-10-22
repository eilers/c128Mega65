----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Testbench for the REU: MiSTer's reu.sv behind C64MEGA65's HyperRAM path
-- (reu_mapper, avm_cache, avm_fifo), wired as in main.vhd and mega65.vhd, against a
-- slow memory model on its own 100 MHz clock.
--
-- The C128 side is a bus model with fpga64_sid_iec's 32-clock cycle: DMA takes the bus
-- at EXT1/EXT5 unless the CPU is writing, dma_cycle spans CPU0..CPUF, ext_cycle is
-- VIC0..VIC3, the CPU's I/O select is CPU4..CPU7, and the CPU's cycle ends at CPU7.
-- The VIC latches register reads at clock 15 and the CIA at clock 17, as in the core.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

library work;
use work.globals.all;

entity tb_reu is
   generic (
      G_REQ_PAUSE  : natural := 20;   -- memory model: hr_clk cycles before a request is taken
      G_RESP_PAUSE : natural := 20;   -- memory model: hr_clk cycles until read data returns
      G_EXT_START  : natural := 4     -- first clock of the REU's RAM window (ext_cycle)
   );
end entity tb_reu;

architecture sim of tb_reu is

   constant C_MAIN_PERIOD : time := 31.718 ns;   -- 31.528 MHz
   constant C_HR_PERIOD   : time := 10 ns;       -- 100 MHz
   constant C_BASE_SLV    : std_logic_vector(31 downto 0) := "0000000000" & C_HMAP_REU(9 downto 0) & x"000";
   constant C_BASE        : unsigned(31 downto 0) := unsigned(C_BASE_SLV);

   signal clk       : std_logic := '0';
   signal hr_clk    : std_logic := '0';
   signal rst       : std_logic := '1';
   signal hr_rst    : std_logic := '1';
   signal running   : boolean   := true;

   -- C128 bus model. bus_* is the CPU's side; sys_* is what the bus carries.
   signal cyc        : natural range 0 to 31 := 0;
   signal ext_cycle  : std_logic;
   signal dma_active : std_logic := '0';
   signal dma_cycle  : std_logic := '0';
   signal bus_addr   : std_logic_vector(15 downto 0) := x"0400";
   signal bus_we     : std_logic := '0';
   signal bus_wdata  : std_logic_vector(7 downto 0) := x"00";
   signal sys_addr   : std_logic_vector(15 downto 0);
   signal sys_we     : std_logic;
   signal cpu_cs     : std_logic;

   -- VIC border and background colours, CIA 1's DDRs, and the chips' read latches
   signal vic_border : std_logic_vector(3 downto 0) := x"7";
   signal vic_bg     : std_logic_vector(3 downto 0) := x"6";
   signal vic_do     : std_logic_vector(7 downto 0) := x"FF";
   signal cia_do     : std_logic_vector(7 downto 0) := x"FF";
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
   signal poke_en   : std_logic := '0';
   signal poke_addr : natural range 0 to 65535 := 0;
   signal poke_data : std_logic_vector(7 downto 0) := x"00";

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

   -- reu_mapper -> avm_cache (map_*) -> avm_fifo (c_*) -> hyperram_errata (h_*) -> memory (m_*)
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
   signal m_write, m_read, m_readdatavalid, m_waitrequest : std_logic;
   signal m_address      : std_logic_vector(31 downto 0);
   signal m_writedata    : std_logic_vector(15 downto 0);
   signal m_byteenable   : std_logic_vector(1 downto 0);
   signal m_burstcount   : std_logic_vector(7 downto 0);
   signal m_readdata     : std_logic_vector(15 downto 0);

   -- Monitor: every access must stay in the REU's window, and an REU that is off is silent
   signal mon_armed      : boolean := false;
   signal mon_errors     : natural := 0;
   signal mon_accesses   : natural := 0;

begin

   clk    <= not clk    after C_MAIN_PERIOD / 2 when running;
   hr_clk <= not hr_clk after C_HR_PERIOD / 2   when running;

   --------------------------------------------------------------------------------
   -- C128 bus model
   --------------------------------------------------------------------------------

   ext_cycle <= '1' when cyc >= G_EXT_START and cyc <= G_EXT_START + 3 else '0';
   sys_addr  <= dma_addr when dma_active = '1' else bus_addr;
   sys_we    <= dma_we   when dma_active = '1' else bus_we;
   cpu_cs    <= '1' when cyc >= 20 and cyc <= 23 and sys_addr(15 downto 5) = "11011111000" else '0';

   dma_din   <= vic_do when dma_addr(15 downto 10) = "110100" else
                cia_do when dma_addr(15 downto 8) = x"DC" else
                ram_q;

   p_bus : process (clk)
      variable v_data : std_logic_vector(7 downto 0);
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

         if cyc = 15 and sys_we = '0' then
            if sys_addr = x"D020" then
               vic_do <= x"F" & vic_border;
            elsif sys_addr = x"D021" then
               vic_do <= x"F" & vic_bg;
            end if;
         end if;
         if cyc = 17 and sys_we = '0' then
            if sys_addr = x"DC02" then
               cia_do <= x"FF";
            elsif sys_addr = x"DC03" then
               cia_do <= x"00";
            end if;
         end if;
         if dma_active = '1' then
            v_data := dma_dout;
         else
            v_data := bus_wdata;
         end if;
         if cyc = 23 and sys_we = '1' and sys_addr = x"D020" then
            vic_border <= v_data(3 downto 0);
         end if;

         if dma_we = '1' then
            c128_ram(to_integer(unsigned(dma_addr))) <= dma_dout;
         end if;
         if poke_en = '1' then
            c128_ram(poke_addr) <= poke_data;
         end if;
         ram_q <= c128_ram(to_integer(unsigned(dma_addr)));
      end if;
   end process p_bus;

   --------------------------------------------------------------------------------
   -- Device under test, wired as in main.vhd
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
                 ram_addr and ("0000" & "11111" & x"FFFF");

   i_reu_mapper : entity work.reu_mapper
      generic map (
         G_BASE_ADDRESS => std_logic_vector(C_BASE)
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

   i_hyperram_errata : entity work.hyperram_errata
      port map (
         clk_i                 => hr_clk,
         rst_i                 => hr_rst,
         s_avm_waitrequest_o   => h_waitrequest,
         s_avm_write_i         => h_write,
         s_avm_read_i          => h_read,
         s_avm_address_i       => h_address,
         s_avm_writedata_i     => h_writedata,
         s_avm_byteenable_i    => h_byteenable,
         s_avm_burstcount_i    => h_burstcount,
         s_avm_readdata_o      => h_readdata,
         s_avm_readdatavalid_o => h_readdatavalid,
         m_avm_waitrequest_i   => m_waitrequest,
         m_avm_write_o         => m_write,
         m_avm_read_o          => m_read,
         m_avm_address_o       => m_address,
         m_avm_writedata_o     => m_writedata,
         m_avm_byteenable_o    => m_byteenable,
         m_avm_burstcount_o    => m_burstcount,
         m_avm_readdata_i      => m_readdata,
         m_avm_readdatavalid_i => m_readdatavalid
      ); -- i_hyperram_errata

   -- 1 MW covers a 2 MiB REU; the window's base bit (20) falls away
   i_memory : entity work.avm_memory_pause
      generic map (
         G_REQ_PAUSE    => G_REQ_PAUSE,
         G_RESP_PAUSE   => G_RESP_PAUSE,
         G_ADDRESS_SIZE => 20,
         G_DATA_SIZE    => 16
      )
      port map (
         clk_i               => hr_clk,
         rst_i               => hr_rst,
         avm_write_i         => m_write,
         avm_read_i          => m_read,
         avm_address_i       => m_address(19 downto 0),
         avm_writedata_i     => m_writedata,
         avm_byteenable_i    => m_byteenable,
         avm_burstcount_i    => m_burstcount,
         avm_readdata_o      => m_readdata,
         avm_readdatavalid_o => m_readdatavalid,
         avm_waitrequest_o   => m_waitrequest
      ); -- i_memory

   p_monitor : process (clk)
      variable v_words : unsigned(31 downto 0);
      variable v_a     : unsigned(31 downto 0);
   begin
      if rising_edge(clk) then
         if cfg = "01" then
            v_words := to_unsigned(16#40000#, 32);    -- 512 KiB
         else
            v_words := to_unsigned(16#100000#, 32);   -- 2 MiB
         end if;
         if mon_armed then
            if map_write = '1' or map_read = '1' then
               mon_accesses <= mon_accesses + 1;
               v_a := unsigned(map_address);
               if cfg = "00" then
                  report "REU is off but accessed HyperRAM at " & to_hstring(map_address) severity error;
                  mon_errors <= mon_errors + 1;
               elsif v_a < C_BASE or v_a >= C_BASE + v_words then
                  report "REU access outside its window: " & to_hstring(map_address) severity error;
                  mon_errors <= mon_errors + 1;
               end if;
            end if;
            -- The cache may read a line past the window's end, but must never write there
            if c_write = '1' then
               v_a := unsigned(c_address);
               if v_a < C_BASE or v_a >= C_BASE + v_words then
                  report "HyperRAM write outside the REU's window: " & to_hstring(c_address) severity error;
                  mon_errors <= mon_errors + 1;
               end if;
            end if;
            if c_read = '1' then
               v_a := unsigned(c_address);
               if v_a < C_BASE or v_a >= C_BASE + v_words + 8 then
                  report "HyperRAM read outside the REU's window: " & to_hstring(c_address) severity error;
                  mon_errors <= mon_errors + 1;
               end if;
            end if;
         end if;
      end if;
   end process p_monitor;

   --------------------------------------------------------------------------------
   -- Test sequence
   --------------------------------------------------------------------------------

   p_test : process
      variable errors : natural := 0;
      variable d      : std_logic_vector(7 downto 0);
      variable l      : line;

      procedure check(ok : boolean; msg : string) is
      begin
         if not ok then
            report msg severity error;
            errors := errors + 1;
         end if;
      end procedure check;

      -- One CPU cycle: from one CPU7 to the next, where the CPU takes its data
      procedure cpu_access(addr : std_logic_vector(15 downto 0); we : std_logic;
                           wdata : std_logic_vector(7 downto 0);
                           rdata : out std_logic_vector(7 downto 0)) is
      begin
         wait until rising_edge(clk) and cyc = 23;
         bus_addr  <= addr;
         bus_we    <= we;
         bus_wdata <= wdata;
         wait until rising_edge(clk) and cyc = 23;
         if addr(15 downto 5) = "11011111000" then
            rdata := cpu_din;
         elsif addr(15 downto 10) = "110100" then
            rdata := vic_do;
         elsif addr(15 downto 8) = x"DC" then
            rdata := cia_do;
         else
            rdata := c128_ram(to_integer(unsigned(addr)));
         end if;
         bus_addr  <= x"0400";
         bus_we    <= '0';
      end procedure cpu_access;

      procedure reu_wr(reg : natural; data : std_logic_vector(7 downto 0)) is
         variable dummy : std_logic_vector(7 downto 0);
      begin
         cpu_access(x"DF" & std_logic_vector(to_unsigned(reg, 8)), '1', data, dummy);
      end procedure reu_wr;

      procedure reu_rd(reg : natural; data : out std_logic_vector(7 downto 0)) is
      begin
         cpu_access(x"DF" & std_logic_vector(to_unsigned(reg, 8)), '0', x"00", data);
      end procedure reu_rd;

      procedure expect_reg(reg : natural; value : std_logic_vector(7 downto 0); what : string) is
         variable v : std_logic_vector(7 downto 0);
      begin
         reu_rd(reg, v);
         check(v = value, what & ": $DF" & to_hstring(to_unsigned(reg, 8)) & " read $" & to_hstring(v) &
                          ", expected $" & to_hstring(value));
      end procedure expect_reg;

      procedure idle_cycles(n : natural) is
         variable dummy : std_logic_vector(7 downto 0);
      begin
         for i in 1 to n loop
            cpu_access(x"0400", '0', x"00", dummy);
         end loop;
      end procedure idle_cycles;

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

      -- Waits out a transfer that the last register write started. A real REU moves a byte
      -- per cycle, two for SWAP; allow a few cycles for HyperRAM to catch up at the start.
      procedure wait_dma(what : string; len : natural; per : natural := 1) is
         variable n : natural := 0;
      begin
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
      end procedure wait_dma;

      procedure transfer(cmd : std_logic_vector(7 downto 0); what : string; len : natural;
                         per : natural := 1) is
      begin
         reu_wr(1, cmd);
         check(dma_req = '1', what & ": no DMA request after the command");
         wait_dma(what, len, per);
      end procedure transfer;

      procedure poke(a : natural; v : std_logic_vector(7 downto 0)) is
      begin
         wait until rising_edge(clk);
         poke_addr <= a;
         poke_data <= v;
         poke_en   <= '1';
         wait until rising_edge(clk);
         poke_en   <= '0';
      end procedure poke;

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

      procedure reset_reu(new_cfg : std_logic_vector(1 downto 0)) is
      begin
         mon_armed <= false;
         wait until rising_edge(clk);
         rst    <= '1';
         cfg    <= new_cfg;
         for i in 1 to 64 loop
            wait until rising_edge(clk);
         end loop;
         rst    <= '0';
         idle_cycles(2);
         mon_armed <= true;
      end procedure reset_reu;

   begin
      hr_rst <= '1';
      rst    <= '1';
      wait for 1 us;
      wait until rising_edge(hr_clk);
      hr_rst <= '0';

      ------------------------------------------------------------------------
      -- Off: registers read $FF, a command does nothing, HyperRAM stays idle
      ------------------------------------------------------------------------
      reset_reu("00");
      setup(16#1000#, 16#012345#, 16);
      reu_wr(1, x"90");
      idle_cycles(4);
      check(dma_req = '0', "off: the REU started a transfer");
      expect_reg(0, x"FF", "off");
      expect_reg(4, x"FF", "off");
      check(mon_accesses = 0, "off: " & integer'image(mon_accesses) & " HyperRAM accesses");

      ------------------------------------------------------------------------
      -- 2 MB
      ------------------------------------------------------------------------
      reset_reu("10");
      expect_reg(0, x"10", "2 MB status after reset");

      -- Register file. The bank register's top 5 bits read back as 1, as on the 8726.
      setup(16#1234#, 16#1D5678#, 16#ABCD#);
      expect_reg(2, x"34", "2 MB registers");
      expect_reg(3, x"12", "2 MB registers");
      expect_reg(4, x"78", "2 MB registers");
      expect_reg(5, x"56", "2 MB registers");
      expect_reg(6, x"FD", "2 MB registers");
      expect_reg(7, x"CD", "2 MB registers");
      expect_reg(8, x"AB", "2 MB registers");

      -- STASH 256 bytes across the 512 KiB boundary, with the end-of-block IRQ on
      reu_wr(9, x"C0");
      reu_wr(10, x"00");
      setup(16#1000#, 16#07FFC0#, 256);
      transfer(x"90", "STASH", 256);
      check(irq = '1', "STASH: no end-of-block IRQ");
      expect_reg(0, x"D0", "STASH status");
      check(irq = '0', "STASH: reading the status did not clear the IRQ");
      expect_reg(0, x"10", "STASH status after read");
      expect_reg(2, x"00", "STASH end address");
      expect_reg(3, x"11", "STASH end address");
      expect_reg(4, x"C0", "STASH end address");
      expect_reg(5, x"00", "STASH end address");
      expect_reg(6, x"F8", "STASH end address");
      expect_reg(7, x"01", "STASH end length");
      expect_reg(8, x"00", "STASH end length");
      reu_wr(9, x"00");

      -- FETCH it back to $3000
      setup(16#3000#, 16#07FFC0#, 256);
      transfer(x"91", "FETCH", 256);
      compare(16#1000#, 16#3000#, 256, "FETCH");

      -- VERIFY: equal, then with one byte changed
      setup(16#1000#, 16#07FFC0#, 256);
      transfer(x"93", "VERIFY", 256);
      expect_reg(0, x"50", "VERIFY equal status");
      poke(16#1080#, not c128_ram(16#1080#));
      setup(16#1000#, 16#07FFC0#, 256);
      transfer(x"93", "VERIFY changed", 256);
      expect_reg(0, x"70", "VERIFY changed status");
      poke(16#1080#, pat(16#1080#));

      -- SWAP 64 bytes: $1000 goes to REU $000100, which then swaps with $2000
      setup(16#1000#, 16#000100#, 64);
      transfer(x"90", "STASH for SWAP", 64);
      setup(16#2000#, 16#000100#, 64);
      transfer(x"92", "SWAP", 64, 2);
      compare(16#1000#, 16#2000#, 64, "SWAP to the C128");
      setup(16#4000#, 16#000100#, 64);
      transfer(x"91", "FETCH after SWAP", 64);
      for i in 0 to 63 loop
         check(c128_ram(16#4000# + i) = pat(16#2000# + i), "SWAP to the REU: byte " & integer'image(i));
      end loop;

      -- $FF00 trigger: FETCH 32 bytes to $5000, armed with bit 4 clear
      setup(16#5000#, 16#07FFC0#, 32);
      reu_wr(1, x"81");
      idle_cycles(8);
      check(dma_req = '0', "$FF00: the transfer started before the write to $FF00");
      cpu_access(x"FF00", '1', x"00", d);
      check(dma_req = '1', "$FF00: the write did not start the transfer");
      wait_dma("FETCH via $FF00", 32);
      compare(16#1000#, 16#5000#, 32, "FETCH via $FF00");

      -- DMA into the VIC, and out of the VIC and CIA after the CPU read another register:
      -- both chips latch their reads before the DMA window, so DMA must have the bus by then
      poke(16#8200#, x"F3");
      setup(16#8200#, 16#006000#, 1);
      transfer(x"90", "STASH for the VIC", 1);
      setup(16#D020#, 16#006000#, 1);
      transfer(x"91", "FETCH into the VIC", 1);
      check(vic_border = x"3", "DMA into the VIC: border $" & to_hstring(vic_border));
      cpu_access(x"D021", '0', x"00", d);
      setup(16#D020#, 16#006100#, 1);
      transfer(x"90", "STASH from the VIC", 1);
      setup(16#8300#, 16#006100#, 1);
      transfer(x"91", "FETCH it back", 1);
      check(c128_ram(16#8300#) = x"F3", "DMA from the VIC: $" & to_hstring(c128_ram(16#8300#)));
      cpu_access(x"D021", '0', x"00", d);
      reu_wr(10, x"80");
      setup(16#D020#, 16#006200#, 4);
      transfer(x"90", "STASH 4 from the VIC", 4);
      reu_wr(10, x"00");
      setup(16#8400#, 16#006200#, 4);
      transfer(x"91", "FETCH them back", 4);
      for i in 0 to 3 loop
         check(c128_ram(16#8400# + i) = x"F3", "DMA from the VIC, byte " & integer'image(i) & ": $" &
               to_hstring(c128_ram(16#8400# + i)));
      end loop;
      cpu_access(x"DC03", '0', x"00", d);
      setup(16#DC02#, 16#006300#, 1);
      transfer(x"90", "STASH from the CIA", 1);
      setup(16#8500#, 16#006300#, 1);
      transfer(x"91", "FETCH it back", 1);
      check(c128_ram(16#8500#) = x"FF", "DMA from the CIA: $" & to_hstring(c128_ram(16#8500#)));

      -- Autoload: the registers return to their start values
      setup(16#1000#, 16#000200#, 16);
      transfer(x"B0", "STASH with autoload", 16);
      expect_reg(2, x"00", "autoload");
      expect_reg(3, x"10", "autoload");
      expect_reg(4, x"00", "autoload");
      expect_reg(5, x"02", "autoload");
      expect_reg(6, x"F8", "autoload");
      expect_reg(7, x"10", "autoload");
      expect_reg(8, x"00", "autoload");

      -- Fixed C128 address: fill REU $000300-$00030F with one byte
      reu_wr(10, x"80");
      setup(16#1000#, 16#000300#, 16);
      transfer(x"90", "STASH from a fixed address", 16);
      reu_wr(10, x"00");
      setup(16#6000#, 16#000300#, 16);
      transfer(x"91", "FETCH the fill", 16);
      for i in 0 to 15 loop
         check(c128_ram(16#6000# + i) = pat(16#1000#), "fixed address fill: byte " & integer'image(i));
      end loop;

      -- The top of the window
      setup(16#1000#, 16#1FFFF0#, 16);
      transfer(x"90", "STASH at the top", 16);
      setup(16#7000#, 16#1FFFF0#, 16);
      transfer(x"91", "FETCH at the top", 16);
      compare(16#1000#, 16#7000#, 16, "top of the window");

      ------------------------------------------------------------------------
      -- 512 KB: bank bits, and a transfer that wraps at the end
      ------------------------------------------------------------------------
      reset_reu("01");
      setup(16#1000#, 16#07FFFF#, 2);
      expect_reg(6, x"FF", "512 KB bank register");
      transfer(x"90", "512 KB STASH across the end", 2);
      setup(16#8000#, 16#000000#, 1);
      transfer(x"91", "512 KB FETCH from 0", 1);
      check(c128_ram(16#8000#) = pat(16#1001#), "512 KB: the second byte did not wrap to 0");
      -- Software that probes for larger REUs writes bank bits the 1750 does not have
      setup(16#1000#, 16#FF0000#, 1);
      idle_cycles(16);

      ------------------------------------------------------------------------
      write(l, string'("monitor: ") & integer'image(mon_accesses) & " HyperRAM accesses, " &
               integer'image(mon_errors) & " errors");
      writeline(output, l);
      errors := errors + mon_errors;

      write(l, string'("{""sessionId"":""reu"",""runId"":""sim-reu"","
                     & """location"":""tb_reu:summary"",""message"":""simulation finished"",""data"":{""errors"":"));
      write(l, errors);
      write(l, string'(",""pass"":"));
      if errors = 0 then
         write(l, string'("true}}"));
         writeline(output, l);
         write(l, string'("ALL CHECKS PASSED"));
         writeline(output, l);
      else
         write(l, string'("false}}"));
         writeline(output, l);
      end if;

      assert errors = 0 report "REU checks failed" severity failure;

      running <= false;
      wait;
   end process p_test;

end architecture sim;
