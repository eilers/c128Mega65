----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Testbench for the network device (CORE/vhdl/net/net.vhd). A firmware model on the device
-- bus, with rv_soc's timing, reads the DNA, sends two Ethernet frames through an RMII
-- loopback and reads them back, then echoes the C128's bytes as the modem would. A C128
-- bus model programs the 6551 and repeats the old modem testbench's IRQ checks against
-- the echo.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_net is
end entity tb_net;

architecture sim of tb_net is

   constant C_MAIN_PERIOD : time := 31.718 ns;   -- 31.528 MHz
   constant C_NET_PERIOD  : time := 20 ns;       -- 50 MHz, qnice_clk's phase

   signal main_clk  : std_logic := '0';
   signal clk       : std_logic := '0';
   signal main_rst  : std_logic := '1';
   signal rst       : std_logic := '1';
   signal enable    : std_logic := '0';

   signal cs        : std_logic := '0';
   signal we        : std_logic := '0';
   signal addr      : std_logic_vector(2 downto 0) := "000";
   signal wdata     : std_logic_vector(7 downto 0) := x"00";
   signal rdata     : std_logic_vector(7 downto 0);
   signal irq       : std_logic;

   signal dev_ce    : std_logic := '0';
   signal dev_we    : std_logic := '0';
   signal dev_addr  : std_logic_vector(11 downto 0) := (others => '0');
   signal dev_wdata : std_logic_vector(15 downto 0) := (others => '0');
   signal dev_rdata : std_logic_vector(15 downto 0);

   signal eth_reset : std_logic;
   signal eth_txd   : std_logic_vector(1 downto 0);
   signal eth_txen  : std_logic;
   signal eth_mdio  : std_logic;

   signal fw_errors  : natural := 0;
   signal c_errors   : natural := 0;
   signal programmed : boolean := false;   -- the C128 has set up the 6551
   signal echoing    : boolean := false;   -- the firmware model echoes from now on
   signal off_check  : boolean := false;   -- the C128 side switched the modem off
   signal off_done   : boolean := false;
   signal irq_seen   : boolean := false;
   signal done       : boolean := false;

   constant R_ACIA_RX     : natural := 16#00#;
   constant R_ACIA_TX     : natural := 16#01#;
   constant R_ACIA_CMD    : natural := 16#02#;
   constant R_ACIA_STATE  : natural := 16#03#;
   constant R_ACIA_LINES  : natural := 16#04#;
   constant R_ACIA_EVENTS : natural := 16#05#;
   constant R_ETH_RX      : natural := 16#08#;
   constant R_ETH_TX      : natural := 16#09#;
   constant R_ETH_STATUS  : natural := 16#0A#;
   constant R_ETH_DROPS   : natural := 16#0B#;
   constant R_ETH_CTRL    : natural := 16#0C#;
   constant R_DNA         : natural := 16#10#;

begin

   main_clk <= not main_clk after C_MAIN_PERIOD / 2 when not done;
   clk      <= not clk      after C_NET_PERIOD / 2  when not done;
   rst      <= '0' after 200 ns;

   watchdog_proc : process
   begin
      wait for 30 ms;
      assert done report "TIMEOUT" severity failure;
      wait;
   end process watchdog_proc;

   i_dut : entity work.net
      generic map (
         G_MAIN_CLK_HZ => 31_527_778,
         G_SIM         => true
      )
      port map (
         clk_i       => clk,
         rst_i       => rst,
         main_clk_i  => main_clk,
         main_rst_i  => main_rst,
         enable_i    => enable,
         cs_i        => cs,
         we_i        => we,
         addr_i      => addr,
         data_i      => wdata,
         data_o      => rdata,
         irq_o       => irq,
         dev_ce_i    => dev_ce,
         dev_we_i    => dev_we,
         dev_addr_i  => dev_addr,
         dev_data_i  => dev_wdata,
         dev_data_o  => dev_rdata,
         eth_clock_o => open,
         eth_reset_o => eth_reset,
         eth_mdc_o   => open,
         eth_mdio_io => eth_mdio,
         eth_rxd_i   => eth_txd,         -- loopback
         eth_rxdv_i  => eth_txen,
         eth_rxer_i  => '0',
         eth_txd_o   => eth_txd,
         eth_txen_o  => eth_txen
      ); -- i_dut

   ---------------------------------------------------------------------------------------------
   -- The firmware, on the device bus
   ---------------------------------------------------------------------------------------------

   fw_proc : process
      variable r     : std_logic_vector(15 downto 0);
      variable frame : std_logic_vector(7 downto 0);
      variable n     : natural;
      variable t0    : time;

      -- As rv_soc: ce from one rising edge to the next, low for a clock between accesses
      procedure dev(a : natural; w : boolean; d : natural) is
      begin
         wait until rising_edge(clk);
         dev_addr  <= std_logic_vector(to_unsigned(a, 12));
         dev_we    <= '1' when w else '0';
         dev_wdata <= std_logic_vector(to_unsigned(d, 16));
         dev_ce    <= '1';
         wait until rising_edge(clk);
         r      := dev_rdata;
         dev_ce <= '0';
         dev_we <= '0';
      end procedure dev;

      procedure expect(a : natural; want : natural; what : string) is
      begin
         dev(a, false, 0);
         if unsigned(r) /= want then
            report what & ": " & to_hstring(r) & ", want " & to_hstring(to_unsigned(want, 16)) severity error;
            fw_errors <= fw_errors + 1;
         end if;
      end procedure expect;

      -- Byte i of frame f
      function frame_byte(f, i : natural) return natural is
      begin
         if i < 6 then
            return 16#FF#;                       -- broadcast
         elsif i < 12 then
            return 16#02# + i;
         elsif i = 12 then
            return 16#08#;
         elsif i = 13 then
            return 16#00#;
         end if;
         return (i * 7 + f * 31) mod 256;
      end function frame_byte;

      constant C_LEN : natural := 64;
   begin
      wait until rst = '0';
      for i in 1 to 100 loop                     -- the DNA shifts out
         wait until rising_edge(clk);
      end loop;

      -- The DNA, as SIM_DNA_VALUE x"0C12800000ABCDE"
      expect(R_DNA + 0, 16#BCDE#, "DNA bits 15..0");
      expect(R_DNA + 1, 16#000A#, "DNA bits 31..16");
      expect(R_DNA + 2, 16#2800#, "DNA bits 47..32");
      expect(R_DNA + 3, 16#80C1#, "DNA bits 56..48 and valid");

      -- Ethernet: the PHY stays in reset until the firmware runs it
      expect(R_ETH_CTRL, 0, "ETH_CTRL after reset");
      if eth_reset /= '0' then
         report "PHY out of reset before the firmware ran it" severity error;
         fw_errors <= fw_errors + 1;
      end if;
      dev(R_ETH_CTRL, true, 1);
      expect(R_ETH_CTRL, 1, "ETH_CTRL after run");
      wait until eth_reset = '1' for 10 us;
      if eth_reset /= '1' then
         report "PHY still in reset" severity error;
         fw_errors <= fw_errors + 1;
      end if;
      expect(R_ETH_RX, 0, "ETH_RX with nothing received");

      -- Two frames out, back through the loopback
      report "frames";
      for f in 0 to 1 loop
         expect(R_ETH_STATUS, 1, "room for a frame");
         for i in 0 to C_LEN - 1 loop
            if i = C_LEN - 1 then
               dev(R_ETH_TX, true, frame_byte(f, i) + 256);   -- the frame's last byte
            else
               dev(R_ETH_TX, true, frame_byte(f, i));
            end if;
         end loop;
      end loop;
      for f in 0 to 1 loop
         t0 := now;
         loop
            dev(R_ETH_RX, false, 0);
            exit when r(15) = '1' or now - t0 > 100 us;
         end loop;
         n := 0;
         while r(15) = '1' loop
            if n < C_LEN and unsigned(r(7 downto 0)) /= frame_byte(f, n) then
               report "frame " & integer'image(f) & " byte " & integer'image(n) & ": " &
                      to_hstring(r(7 downto 0)) severity error;
               fw_errors <= fw_errors + 1;
            end if;
            n := n + 1;
            exit when r(8) = '1';
            dev(R_ETH_RX, false, 0);
         end loop;
         if n /= C_LEN then
            report "frame " & integer'image(f) & ": " & integer'image(n) & " bytes, want " &
                   integer'image(C_LEN) severity error;
            fw_errors <= fw_errors + 1;
         end if;
      end loop;
      expect(R_ETH_RX, 0, "ETH_RX after both frames");
      expect(R_ETH_DROPS, 0, "frames dropped");

      -- The 6551 as the C128 set it up: 230400 baud via Turbo232, DTR on, receive IRQ on
      if not programmed then
         wait until programmed;
      end if;
      report "6551";
      expect(R_ACIA_CMD, 16#1009#, "ACIA_CMD");
      expect(R_ACIA_STATE, 16#0100#, "ACIA_STATE");
      expect(R_ACIA_EVENTS, 2#111#, "ACIA_EVENTS");   -- the C128's reset, command, control
      dev(R_ACIA_EVENTS, true, 2#111#);
      expect(R_ACIA_EVENTS, 0, "ACIA_EVENTS cleared");
      dev(R_ACIA_LINES, true, 2#111#);
      expect(R_ACIA_LINES, 2#111#, "ACIA_LINES");
      expect(R_ACIA_RX, 63, "free entries");

      -- The modem's echo, until the C128 side is done; once a byte is taken it's gone
      echoing <= true;
      while not done loop
         if off_check and not off_done then
            wait for 1 us;
            expect(R_ACIA_STATE, 0, "ACIA_STATE switched off");
            expect(R_ACIA_RX, 63, "free entries switched off");
            expect(R_ACIA_LINES, 0, "ACIA_LINES switched off");
            off_done <= true;
         end if;
         dev(R_ACIA_TX, false, 0);
         if r(8) = '1' then
            frame := r(7 downto 0);
            loop
               dev(R_ACIA_RX, false, 0);
               exit when unsigned(r(7 downto 0)) /= 0;
            end loop;
            dev(R_ACIA_RX, true, to_integer(unsigned(frame)));
         end if;
      end loop;
      wait;
   end process fw_proc;

   ---------------------------------------------------------------------------------------------
   -- The C128
   ---------------------------------------------------------------------------------------------

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

      procedure fail(s : string) is
      begin
         report s severity error;
         c_errors <= c_errors + 1;
      end procedure fail;

      -- Read whatever has arrived, until the text is exactly want or the time is up
      procedure receive(want : string; timeout : time) is
         variable t0 : time := now;
      begin
         while rx_len < want'length and now - t0 < timeout loop
            peek(1);
            if v(3) = '1' then
               peek(0);
               rx_len := rx_len + 1;
               rx(rx_len) := character'val(to_integer(unsigned(v)));
            end if;
         end loop;
         if rx(1 to rx_len) /= want then
            fail("echo """ & rx(1 to rx_len) & """, want """ & want & """");
         end if;
         rx_len := 0;
      end procedure receive;

      -- Read and drop whatever arrives until the line has been quiet for a while
      procedure drain is
         variable t0 : time := now;
      begin
         while now - t0 < 200 us loop
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

      procedure program is
      begin
         poke(3, 16#10#);                        -- 230400 baud via Turbo232
         poke(7, 0);
         poke(2, 16#09#);                        -- DTR on, receive IRQ on, RTS asserted
      end procedure program;

      constant CR : character := character'val(13);
   begin
      for i in 1 to 20 loop
         wait until rising_edge(main_clk);
      end loop;
      enable   <= '1';                           -- switched on, then the C128 starts
      wait for 1 us;
      main_rst <= '0';
      wait for 1 us;

      program;
      peek(3);
      if v /= x"10" then
         fail("control register reads back " & to_hstring(v));
      end if;
      programmed <= true;
      if not echoing then
         wait until echoing;
      end if;

      report "Every byte once, in order";
      -- Every byte once, in order
      send("AT" & CR);
      receive("AT" & CR, 2 ms);
      send("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ");
      receive("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ", 5 ms);

      report "CCGMS starts with DTR on";
      -- CCGMS starts with DTR on and the receive IRQ off while it loads its config, then
      -- turns the IRQ on. A byte that came in between must interrupt then.
      drain;
      poke(2, 16#0B#);
      send("AT" & CR);
      wait for 1 ms;
      if irq /= '0' then
         fail("IRQ with the receive IRQ off");
      end if;
      poke(2, 16#09#);
      if irq /= '1' then
         fail("no IRQ for the byte waiting when the receive IRQ came on");
      end if;
      receive("AT" & CR, 2 ms);

      report "CCGMS's send routine";
      -- CCGMS's send routine reads the status, which clears the IRQ. If that comes before the
      -- CPU has seen the NMI, the byte must interrupt again rather than wait for good.
      drain;
      send("A");
      wait until irq = '1' for 1 ms;
      peek(1);
      wait for 1 us;
      if irq /= '0' or v(3) /= '1' then
         fail("status read did not clear the IRQ with a byte waiting");
      end if;
      wait until irq = '1' for 1 ms;
      if irq /= '1' then
         fail("a byte left unread after a status read never interrupted again");
      end if;
      receive("A", 1 ms);

      report "With DTR off the receiver";
      -- With DTR off the receiver is off: what comes back then is lost
      drain;
      poke(2, 16#0A#);
      send("AT" & CR);
      wait for 1 ms;
      poke(2, 16#09#);
      wait for 100 us;
      peek(1);
      if v(3) /= '0' then
         fail("a byte from while DTR was off came through");
      end if;

      report "A C128 reset in the middle";
      -- A C128 reset in the middle of a line: the next program finds nothing stale
      send("0123456789");
      main_rst <= '1';
      wait for 20 us;
      wait until rising_edge(main_clk);
      main_rst <= '0';
      wait for 1 ms;
      program;
      wait for 100 us;
      peek(1);
      if v(3) /= '0' then
         fail("a byte from before the reset came through");
      end if;
      send("AT" & CR);
      receive("AT" & CR, 2 ms);

      report "Switched off: invisible";
      -- Switched off: invisible, no IRQ, the firmware sees it and the FIFOs empty
      send("ATZ");
      enable <= '0';
      off_check <= true;
      wait until off_done for 1 ms;
      if not off_done then
         fail("the firmware model never checked the modem switched off");
      end if;
      if irq /= '0' then
         fail("IRQ while switched off");
      end if;

      if not irq_seen then
         fail("6551 never raised its IRQ");
      end if;

      wait for 1 us;
      if fw_errors + c_errors = 0 then
         report "ALL CHECKS PASSED";
      else
         report integer'image(fw_errors + c_errors) & " CHECKS FAILED" severity error;
      end if;
      done <= true;
      wait;
   end process c128_proc;

end architecture sim;
