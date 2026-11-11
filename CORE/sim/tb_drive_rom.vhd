----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65 (c128mega65)
--
-- Testbench for the drive DOS ROM handover: drive_rom_server.vhd and a model of the
-- firmware's feed (drvrom.c) with rv_soc's device bus timing, into two iecdrv_rom as
-- iec_drive arbitrates them (the higher drive first). xsim runs it with the real
-- iecdrv_rom.sv (run_drive_rom_sim.tcl), nvc with iecdrv_rom_model.vhd (run_drive_rom_nvc.sh).
--
-- iecdrv_rom sets rom_valid after 32768 bytes whichever bytes came, and a drive with a
-- shifted or mixed ROM shows only as "device not present". So every byte a drive takes is
-- checked as it takes it, and each ROM is read back once the drive has it.
--
-- Cases: one drive; both drives at once; reloads in the middle of a load (REST's load_rom);
-- a new model in the middle of a load; the other drive starting in the middle of a load, so
-- the first goes on from an odd address; a drive reset and the core's reset in the middle
-- of a load; the core's reset with both drives running.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity tb_drive_rom is
end entity tb_drive_rom;

architecture sim of tb_drive_rom is

   constant C_CLK_PERIOD : time    := 20 ns;            -- 50 MHz QNICE clock
   constant C_BANK       : natural := 32768;
   constant C_CHUNK      : natural := 256;              -- drvrom.c's CHUNK
   constant C_WINDOW     : std_logic_vector(15 downto 0) := x"01C0";
   constant C_LIMIT      : natural := 2_000_000;        -- clocks a load may take here
   constant C_ROMS       : natural := 11;               -- ROMs the scenarios read back

   type t_gens  is array (0 to 5) of natural;
   type t_banks is array (0 to 1) of natural range 0 to 5;
   type t_a15   is array (0 to 1) of std_logic_vector(14 downto 0);
   type t_a4    is array (0 to 1) of std_logic_vector(3 downto 0);
   type t_d8    is array (0 to 1) of std_logic_vector(7 downto 0);
   type t_nat2  is array (0 to 1) of natural;

   signal clk         : std_logic := '0';
   signal running     : boolean   := true;

   -- Device bus (the firmware model drives it, as rv_soc: from rising edges)
   signal dev_ce      : std_logic := '0';
   signal dev_we      : std_logic := '0';
   signal dev_addr    : std_logic_vector(27 downto 0) := (others => '0');
   signal dev_wdata   : std_logic_vector(15 downto 0) := (others => '0');
   signal dev_rdata   : std_logic_vector(15 downto 0);

   -- iec_drive
   signal rom_loading : std_logic := '1';
   signal drv_reset   : std_logic_vector(1 downto 0) := "11";
   signal rom_bank    : t_banks := (2, 3);
   signal rom_reload  : std_logic_vector(1 downto 0);
   signal rom_ready   : std_logic_vector(1 downto 0);
   signal rom_req_d   : std_logic_vector(1 downto 0);
   signal rom_addr_d  : t_a15;
   signal rom_wr_en   : std_logic_vector(1 downto 0);
   signal rom_wr_i    : std_logic_vector(1 downto 0);   -- each drive's strobe
   signal rom_bank_v  : t_a4;
   signal rom_req     : std_logic;
   signal rom_addr    : std_logic_vector(18 downto 0);
   signal rom_data    : std_logic_vector(7 downto 0);
   signal rom_wr      : std_logic;
   signal mem_a       : t_a15 := (others => (others => '0'));
   signal rom_do      : t_d8;
   signal empty8k     : std_logic_vector(1 downto 0);

   -- The library's contents: a new ROM in a bank each time REST loads one
   signal lib_gen     : t_gens := (others => 0);

   -- The scenario's requests to the firmware model
   signal fw_on       : boolean := false;
   signal reload_tog  : boolean := false;               -- a reload of reload_mask, the
   signal reload_mask : std_logic_vector(1 downto 0) := "00";
   signal reload_bank : integer range -1 to 5 := -1;    -- library bank renewed first, or -1
   signal reload_done : boolean := false;               -- toggles once done

   signal taken       : t_nat2 := (0, 0);               -- bytes each drive took
   signal bad_bytes   : natural := 0;
   signal bus_errors  : natural := 0;

   function rom_byte (addr : natural; g : natural) return std_logic_vector is
   begin
      return std_logic_vector(to_unsigned((addr * 7 + (addr / 256) * 3 + 1 + g * 41) mod 256, 8));
   end function rom_byte;

begin

   clk <= not clk after C_CLK_PERIOD / 2 when running else '0';

   ---------------------------------------------------------------------------------
   -- Device under test
   ---------------------------------------------------------------------------------

   i_server : entity work.drive_rom_server
      generic map (
         G_WINDOW      => C_WINDOW
      )
      port map (
         clk_i         => clk,
         rst_i         => '0',
         rom_loading_i => rom_loading,
         qnice_addr_i  => dev_addr,
         qnice_data_i  => dev_wdata,
         qnice_ce_i    => dev_ce,
         qnice_we_i    => dev_we,
         qnice_data_o  => dev_rdata,
         rom_ready_i   => rom_ready,
         rom_reload_o  => rom_reload,
         rom_req_i     => rom_req,
         rom_addr_i    => rom_addr,
         rom_data_o    => rom_data,
         rom_wr_o      => rom_wr
      ); -- i_server

   ---------------------------------------------------------------------------------
   -- iec_drive's two drives, as iec_drive.sv arbitrates them
   ---------------------------------------------------------------------------------

   rom_wr_en(1) <= rom_req_d(1);
   rom_wr_en(0) <= rom_req_d(0) and not rom_wr_en(1);
   rom_req      <= rom_req_d(0) or rom_req_d(1);
   rom_addr     <= rom_bank_v(1) & rom_addr_d(1) when rom_wr_en(1) = '1' else
                   rom_bank_v(0) & rom_addr_d(0) when rom_wr_en(0) = '1' else
                   (others => 'X');

   g_drives : for i in 0 to 1 generate
      rom_wr_i(i)   <= rom_wr and rom_wr_en(i);
      rom_bank_v(i) <= std_logic_vector(to_unsigned(rom_bank(i), 4));

      -- Plain signals only: xsim's binding to the Verilog module takes no expressions
      i_iecdrv_rom : entity work.iecdrv_rom
         port map (
            clk_sys     => clk,
            clk         => clk,
            reset       => drv_reset(i),
            rom_loading => rom_loading,
            rom_reload  => rom_reload(i),
            empty8k     => empty8k(i),
            rom_valid   => rom_ready(i),
            rom_bank    => rom_bank_v(i),
            mem_a       => mem_a(i),
            rom_do      => rom_do(i),
            rom_req     => rom_req_d(i),
            rom_addr    => rom_addr_d(i),
            rom_wr      => rom_wr_i(i),
            rom_data    => rom_data
         ); -- i_iecdrv_rom
   end generate g_drives;

   -- Every byte a drive takes is the library's byte for its bank and address
   p_taken : process (clk)
      variable l : line;
      variable a : natural;
   begin
      if rising_edge(clk) then
         for i in 0 to 1 loop
            if rom_wr = '1' and rom_wr_en(i) = '1' and rom_req_d(i) = '1' then
               taken(i) <= taken(i) + 1;
               a := rom_bank(i) * C_BANK + to_integer(unsigned(rom_addr_d(i)));
               if rom_data /= rom_byte(a, lib_gen(rom_bank(i))) then
                  if bad_bytes < 8 then
                     write(l, string'("MISMATCH drive ")); write(l, i);
                     write(l, string'(" took 0x")); hwrite(l, rom_data);
                     write(l, string'(" at 0x")); hwrite(l, std_logic_vector(to_unsigned(a, 20)));
                     writeline(output, l);
                  end if;
                  bad_bytes <= bad_bytes + 1;
               end if;
            end if;
         end loop;
         assert not (rom_wr = '1' and rom_req = '0')
            report "a byte was offered while no drive asked" severity error;
      end if;
   end process p_taken;

   ---------------------------------------------------------------------------------
   -- The firmware: drvrom_poll in a main loop, with rv_soc's device bus timing
   ---------------------------------------------------------------------------------

   p_fw : process
      constant C_CSR   : unsigned(27 downto 0) := x"FFFF000";
      variable seed    : unsigned(15 downto 0) := x"ACE1";
      variable rd      : std_logic_vector(15 downto 0);
      variable r32     : std_logic_vector(31 downto 0);
      variable a, stop : natural;
      variable seen    : boolean := false;
      variable l       : line;

      -- 1 to 4 clocks with ce low: rv_soc's Q_IDLE after the CPU's next request
      impure function gap return natural is
      begin
         seed := seed(14 downto 0) & (seed(15) xor seed(13) xor seed(12) xor seed(10));
         return 1 + to_integer(seed(1 downto 0));
      end function gap;

      -- One device access: ce for one clock, read at the rising edge that ends it
      procedure access16(we : std_logic; reg : natural; wdata : std_logic_vector(15 downto 0);
                         idle : natural; rdata : out std_logic_vector(15 downto 0)) is
      begin
         for i in 1 to idle loop
            wait until rising_edge(clk);
         end loop;
         dev_ce    <= '1';
         dev_we    <= we;
         dev_addr  <= std_logic_vector(C_CSR + reg);
         dev_wdata <= wdata;
         wait until rising_edge(clk);
         rdata     := dev_rdata;
         dev_ce    <= '0';
         dev_we    <= '0';
      end procedure access16;

      -- A 32-bit access: the low half, one clock of Q_GAP, the high half
      procedure access32(we : std_logic; reg : natural; wdata : std_logic_vector(31 downto 0);
                         rdata : out std_logic_vector(31 downto 0)) is
         variable lo, hi : std_logic_vector(15 downto 0);
      begin
         access16(we, reg, wdata(15 downto 0), gap, lo);
         access16(we, reg + 1, wdata(31 downto 16), 1, hi);
         rdata := hi & lo;
      end procedure access32;

      -- A REST load_rom or a ROM pick: the bank renewed in the library, then the reload
      procedure do_reload is
      begin
         if reload_tog /= seen then
            seen := reload_tog;
            for i in 1 to 4 loop
               wait until rising_edge(clk);
            end loop;
            if reload_bank >= 0 then
               lib_gen(reload_bank) <= lib_gen(reload_bank) + 1;
            end if;
            access16('1', 0, x"000" & "00" & reload_mask, gap, rd);
            reload_done <= not reload_done;
         end if;
      end procedure do_reload;

   begin
      wait until fw_on;
      access16('0', 1, x"0000", 2, rd);
      if rd /= C_WINDOW then
         report "register 1 is not the library's window" severity error;
         bus_errors <= bus_errors + 1;
      end if;

      loop
         do_reload;
         if not fw_on then
            wait until fw_on;
         end if;
         access32('0', 2, x"00000000", r32);
         if r32(31) = '0' then
            -- The main loop's other modules, and the ready bits as the firmware reads them
            access16('0', 0, x"0000", 10 + gap, rd);
            if rd(1 downto 0) /= rom_ready then
               report "register 0 is not the drives' rom_valid" severity error;
               bus_errors <= bus_errors + 1;
            end if;
         else
            a    := to_integer(unsigned(r32(18 downto 2))) * 4;
            stop := (a / C_CHUNK + 1) * C_CHUNK;
            while a < stop loop
               do_reload;
               access32('1', 4, rom_byte(a + 3, lib_gen(a / C_BANK)) & rom_byte(a + 2, lib_gen(a / C_BANK)) &
                                rom_byte(a + 1, lib_gen(a / C_BANK)) & rom_byte(a, lib_gen(a / C_BANK)), r32);
               a := a + 4;
            end loop;
         end if;
      end loop;
   end process p_fw;

   ---------------------------------------------------------------------------------
   -- Scenarios
   ---------------------------------------------------------------------------------

   p_stim : process
      variable l       : line;
      variable errors  : natural := 0;
      variable checked : natural := 0;
      variable sum     : natural;

      procedure clocks(n : natural) is
      begin
         for i in 1 to n loop
            wait until rising_edge(clk);
         end loop;
      end procedure clocks;

      procedure fail(msg : string) is
      begin
         write(l, string'("FAIL: ") & msg);
         writeline(output, l);
         errors := errors + 1;
      end procedure fail;

      -- Wait for drive d's ROM, then read it back through the drive's port
      procedure await_rom(d : natural; msg : string) is
         variable n    : natural := 0;
         variable bad  : natural := 0;
         variable want : std_logic_vector(7 downto 0);
      begin
         while rom_ready(d) /= '1' and n < C_LIMIT loop
            wait until rising_edge(clk);
            n := n + 1;
         end loop;
         if rom_ready(d) /= '1' then
            fail(msg & ": drive " & integer'image(d) & " never got its ROM");
            return;
         end if;
         for i in 0 to C_BANK - 1 loop
            mem_a(d) <= std_logic_vector(to_unsigned(i, 15));
            clocks(3);
            wait for 1 ns;
            want := rom_byte(rom_bank(d) * C_BANK + i, lib_gen(rom_bank(d)));
            if rom_do(d) /= want then
               if bad < 4 then
                  write(l, string'("MISMATCH drive ")); write(l, d);
                  write(l, string'(" offset 0x")); hwrite(l, std_logic_vector(to_unsigned(i, 16)));
                  write(l, string'(" expected 0x")); hwrite(l, want);
                  write(l, string'(" got 0x")); hwrite(l, rom_do(d));
                  writeline(output, l);
               end if;
               bad := bad + 1;
            end if;
         end loop;
         if bad /= 0 then
            fail(msg & ": drive " & integer'image(d) & " has " & integer'image(bad) & " wrong bytes");
         end if;
         checked := checked + 1;
         write(l, msg & ": drive " & integer'image(d) & " bank " & integer'image(rom_bank(d)) &
                  " loaded, " & integer'image(n) & " clocks after the wait began");
         writeline(output, l);
      end procedure await_rom;

      -- Drive d at an address (that offset in its bank, or past it) in the middle of a load
      procedure await_addr(d : natural; at : natural; msg : string) is
         variable n : natural := 0;
      begin
         while not (rom_req_d(d) = '1' and to_integer(unsigned(rom_addr_d(d))) >= at) and n < C_LIMIT loop
            wait until rising_edge(clk);
            n := n + 1;
         end loop;
         if n = C_LIMIT then
            fail(msg & ": drive " & integer'image(d) & " never reached 0x" & integer'image(at));
         end if;
      end procedure await_addr;

      procedure reload(mask : std_logic_vector(1 downto 0); bank : integer) is
      begin
         reload_mask <= mask;
         reload_bank <= bank;
         reload_tog  <= not reload_tog;
         wait on reload_done;
         clocks(3);
      end procedure reload;

      procedure expect_invalid(d : natural; msg : string) is
      begin
         if rom_ready(d) /= '0' then
            fail(msg & ": drive " & integer'image(d) & " kept rom_valid");
         end if;
      end procedure expect_invalid;

   begin
      write(l, string'("{""sessionId"":""drvrom"",""runId"":""sim-drvrom"","
                     & """location"":""tb_drive_rom:stim"",""message"":""simulation start""}"));
      writeline(output, l);

      -- 1. One drive: drive 8 as a 1571, drive 9 off
      clocks(10);
      rom_loading  <= '0';
      fw_on        <= true;
      clocks(50);
      drv_reset(0) <= '0';
      await_rom(0, "one drive");
      expect_invalid(1, "one drive");

      -- 2. Both at once: drive 8 as a 1541, drive 9 a 1571
      drv_reset    <= "11";
      rom_bank(0)  <= 0;
      clocks(5);
      drv_reset    <= "00";
      await_rom(1, "both at once");
      await_rom(0, "both at once");

      -- 3. load_rom: a new ROM in drive 8's bank and a reload; again in the middle of the load
      reload("01", 0);
      expect_invalid(0, "reload");
      await_addr(0, 16#3001#, "reload");
      reload("01", 0);
      await_rom(0, "reload in the middle of a load");

      -- 4. A new model in the middle of a load: drive 9 1571 -> 1581 -> 1541
      rom_bank(1) <= 5;
      clocks(2);
      expect_invalid(1, "new model");
      await_addr(1, 16#2001#, "new model");
      rom_bank(1) <= 1;
      await_rom(1, "new model in the middle of a load");

      -- 5. Drive 9 starts in the middle of drive 8's load, which goes on from an odd address
      drv_reset(1) <= '1';
      reload("10", -1);
      reload("01", -1);
      await_addr(0, 16#4321#, "takeover");
      drv_reset(1) <= '0';
      clocks(1);
      write(l, string'("takeover: drive 8 left at 0x")); hwrite(l, rom_addr_d(0));
      writeline(output, l);
      await_rom(1, "takeover");
      await_rom(0, "takeover, drive 8 again");

      -- 6. A drive reset in the middle of a load
      reload("01", 0);
      await_addr(0, 16#5003#, "drive reset");
      drv_reset(0) <= '1';
      clocks(60);
      expect_invalid(0, "drive reset");
      drv_reset(0) <= '0';
      await_rom(0, "drive reset in the middle of a load");

      -- 7. The core's reset in the middle of a load, the firmware still polling
      reload("10", 1);
      await_addr(1, 16#6002#, "core reset");
      rom_loading <= '1';
      clocks(300);
      expect_invalid(1, "core reset");
      rom_loading <= '0';
      await_rom(1, "core reset in the middle of a load");

      -- 8. The core's reset with both drives running: both load again
      rom_loading <= '1';
      clocks(20);
      expect_invalid(0, "core reset, both");
      expect_invalid(1, "core reset, both");
      rom_loading <= '0';
      await_rom(1, "core reset, both");
      await_rom(0, "core reset, both");

      -- Idle: the firmware still polls, and the drives take nothing more
      sum := taken(0) + taken(1);
      clocks(2000);
      if taken(0) + taken(1) /= sum or rom_ready /= "11" then
         fail("idle: a drive took bytes or lost its ROM");
      end if;

      fw_on <= false;
      errors := errors + bad_bytes + bus_errors;
      write(l, string'("bytes taken: drive 8 ")); write(l, taken(0));
      write(l, string'(", drive 9 ")); write(l, taken(1));
      write(l, string'(", wrong ")); write(l, bad_bytes);
      writeline(output, l);

      write(l, string'("{""sessionId"":""drvrom"",""runId"":""sim-drvrom"","
                     & """location"":""tb_drive_rom:summary"",""message"":""simulation finished"",""data"":{""roms"":"));
      write(l, checked);
      write(l, string'(",""errors"":"));
      write(l, errors);
      write(l, string'(",""pass"":"));
      if errors = 0 and checked = C_ROMS then
         write(l, string'("true}}"));
      else
         write(l, string'("false}}"));
      end if;
      writeline(output, l);
      if errors = 0 and checked = C_ROMS then
         write(l, string'("ALL CHECKS PASSED"));
         writeline(output, l);
      end if;

      assert errors = 0 and checked = C_ROMS report "drive ROM handover failed" severity failure;
      running <= false;
      wait;
   end process p_stim;

end architecture sim;
