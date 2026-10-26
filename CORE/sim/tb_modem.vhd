----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Testbench for the SwiftLink modem (CORE/vhdl/modem): the real firmware
-- (CORE/modem-fw/build/modem.bin) boots from a HyperRAM model, loaded one byte per
-- 16-bit word as the Shell does. A C128 bus model programs the 6551 and types AT
-- commands; the checks look for the echo and the modem's replies. Ethernet is idle,
-- so the modem never gets an IP address.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_modem is
   generic (
      G_IMAGE : string := "modem.bin"
   );
end entity tb_modem;

architecture sim of tb_modem is

   constant C_MAIN_PERIOD  : time    := 31.718 ns;   -- 31.528 MHz
   constant C_MODEM_PERIOD : time    := 20 ns;       -- 50 MHz
   constant C_HR_PERIOD    : time    := 10 ns;       -- 100 MHz
   constant C_HR_BASE      : natural := 16#300000#;
   constant C_MEM_WORDS    : natural := 2 ** 17;
   constant C_HR_LATENCY   : natural := 14;          -- hr_clk cycles before read data

   signal main_clk  : std_logic := '0';
   signal modem_clk : std_logic := '0';
   signal hr_clk    : std_logic := '0';
   signal main_rst  : std_logic := '1';
   signal modem_rst : std_logic := '1';
   signal hr_rst    : std_logic := '1';
   signal enable    : std_logic := '0';

   signal cs        : std_logic := '0';
   signal we        : std_logic := '0';
   signal addr      : std_logic_vector(2 downto 0) := "000";
   signal wdata     : std_logic_vector(7 downto 0) := x"00";
   signal rdata     : std_logic_vector(7 downto 0);
   signal irq       : std_logic;

   signal hr_write, hr_read        : std_logic;
   signal hr_address               : std_logic_vector(31 downto 0);
   signal hr_writedata             : std_logic_vector(15 downto 0);
   signal hr_byteenable            : std_logic_vector(1 downto 0);
   signal hr_burstcount            : std_logic_vector(7 downto 0);
   signal hr_readdata              : std_logic_vector(15 downto 0) := (others => '0');
   signal hr_readdatavalid         : std_logic := '0';
   signal hr_waitrequest           : std_logic;

   signal eth_txd   : std_logic_vector(1 downto 0);
   signal eth_txen  : std_logic;
   signal eth_mdio  : std_logic;

   signal hr_stall  : boolean := false;    -- the next HyperRAM read waits 2 ms
   signal errors    : natural := 0;
   signal irq_seen  : boolean := false;
   signal done      : boolean := false;

begin

   main_clk  <= not main_clk  after C_MAIN_PERIOD / 2  when not done;
   modem_clk <= not modem_clk after C_MODEM_PERIOD / 2 when not done;
   hr_clk    <= not hr_clk    after C_HR_PERIOD / 2    when not done;
   modem_rst <= '0' after 200 ns;
   hr_rst    <= '0' after 200 ns;

   i_dut : entity work.modem
      generic map (
         G_MAIN_CLK_HZ => 31_527_778,
         G_HR_BASE     => C_HR_BASE,
         G_SIM         => true
      )
      port map (
         clk_i              => modem_clk,
         rst_i              => modem_rst,
         main_clk_i         => main_clk,
         main_rst_i         => main_rst,
         enable_i           => enable,
         cs_i               => cs,
         we_i               => we,
         addr_i             => addr,
         data_i             => wdata,
         data_o             => rdata,
         irq_o              => irq,
         hr_clk_i           => hr_clk,
         hr_rst_i           => hr_rst,
         hr_write_o         => hr_write,
         hr_read_o          => hr_read,
         hr_address_o       => hr_address,
         hr_writedata_o     => hr_writedata,
         hr_byteenable_o    => hr_byteenable,
         hr_burstcount_o    => hr_burstcount,
         hr_readdata_i      => hr_readdata,
         hr_readdatavalid_i => hr_readdatavalid,
         hr_waitrequest_i   => hr_waitrequest,
         eth_clock_o        => open,
         eth_reset_o        => open,
         eth_mdc_o          => open,
         eth_mdio_io        => eth_mdio,
         eth_rxd_i          => "00",
         eth_rxdv_i         => '0',
         eth_rxer_i         => '0',
         eth_txd_o          => eth_txd,
         eth_txen_o         => eth_txen
      ); -- i_dut

   -- HyperRAM: the firmware image, one byte per word as the Shell loads it
   hyperram_proc : process
      type mem_t is array (0 to C_MEM_WORDS - 1) of std_logic_vector(15 downto 0);
      type char_file_t is file of character;
      file f            : char_file_t;
      variable mem      : mem_t := (others => (others => '0'));
      variable c        : character;
      variable n        : natural := 0;
      variable a        : natural;
      variable count    : natural;
      variable status   : file_open_status;
   begin
      file_open(status, f, G_IMAGE, read_mode);
      assert status = open_ok report "cannot open " & G_IMAGE severity failure;
      while not endfile(f) loop
         read(f, c);
         mem(n) := x"00" & std_logic_vector(to_unsigned(character'pos(c), 8));
         n := n + 1;
      end loop;
      file_close(f);
      report "HyperRAM model: " & integer'image(n) & " bytes of firmware";

      hr_waitrequest <= '0';
      loop
         wait until rising_edge(hr_clk);
         hr_readdatavalid <= '0';
         assert hr_write /= '1' report "modem wrote to HyperRAM" severity error;
         if hr_read = '1' then
            a     := to_integer(unsigned(hr_address)) - C_HR_BASE;
            count := to_integer(unsigned(hr_burstcount));
            assert a >= 0 and a + count <= C_MEM_WORDS report "HyperRAM read outside the window" severity failure;
            assert count <= 16 report "HyperRAM burst longer than 16 words" severity error;
            hr_waitrequest <= '1';
            if hr_stall then
               wait for 2 ms;                    -- beyond the CPU's 1.3 ms bus timeout
            end if;
            for i in 1 to C_HR_LATENCY loop
               wait until rising_edge(hr_clk);
            end loop;
            for i in 0 to count - 1 loop
               hr_readdata      <= mem(a + i);
               hr_readdatavalid <= '1';
               wait until rising_edge(hr_clk);
            end loop;
            hr_readdatavalid <= '0';
            hr_waitrequest   <= '0';
         end if;
      end loop;
   end process hyperram_proc;

   irq_proc : process (main_clk)
   begin
      if rising_edge(main_clk) and irq = '1' then
         irq_seen <= true;
      end if;
   end process irq_proc;

   c128_proc : process
      variable rx      : string(1 to 400);
      variable rx_len  : natural := 0;
      variable v       : std_logic_vector(7 downto 0);

      -- One C128 cycle: the select is high for 4 of its 32 clocks, the access is on its rising edge
      procedure access_reg(a : natural; w : boolean; d : std_logic_vector(7 downto 0)) is
      begin
         wait until rising_edge(main_clk);
         addr  <= std_logic_vector(to_unsigned(a, 3));
         we    <= '1' when w else '0';
         wdata <= d;
         cs    <= '1';
         for i in 1 to 4 loop
            wait until rising_edge(main_clk);
         end loop;
         v  := rdata;
         cs <= '0';
         for i in 1 to 27 loop
            wait until rising_edge(main_clk);
         end loop;
      end procedure access_reg;

      procedure poke(a : natural; d : natural) is
      begin
         access_reg(a, true, std_logic_vector(to_unsigned(d, 8)));
      end procedure poke;

      procedure peek(a : natural) is
      begin
         access_reg(a, false, x"00");
      end procedure peek;

      -- Read whatever has arrived, until the text contains want or the time is up
      procedure receive(want : string; timeout : time) is
         variable t0 : time := now;
         variable found : boolean := false;
      begin
         while not found and now - t0 < timeout loop
            peek(1);
            if v(3) = '1' then
               peek(0);
               if rx_len < rx'high then
                  rx_len := rx_len + 1;
                  rx(rx_len) := character'val(to_integer(unsigned(v)));
               end if;
               for i in 1 to rx_len - want'length + 1 loop
                  if rx(i to i + want'length - 1) = want then
                     found := true;
                  end if;
               end loop;
            end if;
         end loop;
         if not found then
            report "no """ & want & """ from the modem; got: " & rx(1 to rx_len) severity error;
            errors <= errors + 1;
         else
            report "modem said: " & rx(1 to rx_len);
         end if;
         rx_len := 0;
      end procedure receive;

      -- Read and drop whatever arrives until the line has been quiet for a while
      procedure drain is
         variable t0 : time := now;
      begin
         while now - t0 < 500 us loop
            peek(1);
            if v(3) = '1' then
               peek(0);
               t0 := now;
            end if;
         end loop;
      end procedure drain;

      procedure send(s : string) is
      begin
         for i in s'range loop
            loop
               peek(1);
               exit when v(4) = '1';             -- transmit data register empty
            end loop;
            poke(0, character'pos(s(i)));
         end loop;
      end procedure send;

      constant CR : character := character'val(13);
      constant LF : character := character'val(10);
   begin
      for i in 1 to 20 loop
         wait until rising_edge(main_clk);
      end loop;
      main_rst <= '0';
      enable   <= '1';

      -- 230400 baud via Turbo232, then DTR on, receive IRQ on, RTS asserted
      poke(3, 16#10#);
      poke(7, 0);
      poke(2, 16#09#);
      peek(3);
      assert v = x"10" report "control register reads back wrong" severity error;
      peek(7);
      assert v = x"04" report "Turbo232 register reads back wrong" severity error;

      send("AT" & CR);
      receive(CR & LF & "OK" & CR & LF, 20 ms);

      send("ATI" & CR);
      receive("MEGA65 C128 MODEM", 5 ms);
      receive("OK", 5 ms);

      send("ATY" & CR);
      receive("ERROR", 5 ms);

      send("ATDT1.2.3.4:23" & CR);
      receive("NO DIALTONE", 5 ms);

      -- CCGMS starts with DTR on and the receive IRQ off while it loads its config, then
      -- turns the IRQ on. A byte that came in between must interrupt then.
      drain;
      poke(2, 16#0B#);
      send("AT" & CR);
      wait for 1 ms;
      if irq /= '0' then
         report "IRQ with the receive IRQ off" severity error;
         errors <= errors + 1;
      end if;
      poke(2, 16#09#);
      if irq /= '1' then
         report "no IRQ for the byte waiting when the receive IRQ came on" severity error;
         errors <= errors + 1;
      end if;
      receive("OK" & CR & LF, 5 ms);

      -- CCGMS's send routine reads the status, which clears the IRQ. If that comes before the
      -- CPU has seen the NMI, the byte must interrupt again rather than wait for good.
      drain;
      send("AT" & CR);
      wait until irq = '1' for 5 ms;
      peek(1);
      wait for 1 us;
      if irq /= '0' or v(3) /= '1' then
         report "status read did not clear the IRQ with a byte waiting" severity error;
         errors <= errors + 1;
      end if;
      wait until irq = '1' for 1 ms;
      if irq /= '1' then
         report "a byte left unread after a status read never interrupted again" severity error;
         errors <= errors + 1;
      end if;
      receive("OK" & CR & LF, 5 ms);

      -- With DTR off the receiver is off: what the modem says then is lost
      drain;
      poke(2, 16#0A#);
      send("AT" & CR);
      wait for 2 ms;
      poke(2, 16#09#);
      wait for 100 us;
      peek(1);
      if v(3) /= '0' then
         report "a byte from while DTR was off came through" severity error;
         errors <= errors + 1;
      end if;

      -- A C128 reset in the middle of an answer: the next program finds nothing stale
      send("ATI" & CR);
      wait for 300 us;
      main_rst <= '1';
      wait for 20 us;
      wait until rising_edge(main_clk);
      main_rst <= '0';
      wait for 3 ms;
      poke(3, 16#10#);
      poke(7, 0);
      poke(2, 16#09#);
      wait for 100 us;
      peek(1);
      if v(3) /= '0' then
         report "a byte from before the reset came through" severity error;
         errors <= errors + 1;
      end if;
      send("AT" & CR);
      receive(CR & LF & "OK" & CR & LF, 20 ms);

      -- A trap restarts the firmware, which says so; AT*T reads from nowhere
      send("AT*T" & CR);
      receive("MODEM RESTARTED", 50 ms);
      send("AT" & CR);
      receive(CR & LF & "OK" & CR & LF, 20 ms);

      -- HyperRAM too slow for the CPU: it gives up on the access and restarts, and the
      -- bridge drops the late answer instead of handing it to the next access
      hr_stall <= true;
      wait for 50 us;
      hr_stall <= false;
      receive("MODEM RESTARTED", 50 ms);
      send("AT" & CR);
      receive(CR & LF & "OK" & CR & LF, 20 ms);
      send("ATI1" & CR);
      receive("RESTARTS", 20 ms);
      receive("MISSED NMIS", 5 ms);
      receive("OK", 5 ms);

      if not irq_seen then
         report "6551 never raised its IRQ" severity error;
         errors <= errors + 1;
      end if;

      wait for 1 us;
      if errors = 0 then
         report "ALL CHECKS PASSED";
      else
         report integer'image(errors) & " CHECKS FAILED" severity error;
      end if;
      done <= true;
      wait;
   end process c128_proc;

end architecture sim;
