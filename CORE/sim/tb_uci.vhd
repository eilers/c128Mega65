----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Testbench for the Ultimate Command Interface (CORE/vhdl/uci/uci.vhd). The C128 side
-- reads and writes $DF18-$DF1F as the core's bus does (address in PHI1, select in PHI2);
-- a firmware process on the QNICE clock drives the device bus as rv_soc.vhd does and
-- answers commands the way Gideon's command_intf.cc does. Checks:
--   1. idle status $00, identification $C9, $DF18-$DF1A read $FF; the buffer offsets
--   2. a command and its one-block reply, data and status, then idle again
--   3. a reply in two blocks (Data More, then Data Last after DATA_ACC)
--   4. a command of 900 bytes stops at 895; its 895-byte echo comes back whole
--   5. a command with the no-reply bit goes straight back to idle
--   6. a push while busy sets ERROR, CLR_ERR clears it; ABORT ends the command
--   7. the IRQ bit: IRQ with the reply, $DF1D reads $49, reading the reply ends it
--   8. the DMA bit holds the CPU until the reply; TRIGGER waits for a write to $FF00
--   9. a reset of the C128 sets the flag at device word 0x010, and writing 1 clears it
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_uci is
end entity tb_uci;

architecture sim of tb_uci is

   constant C_MAIN_PERIOD  : time := 31.7 ns;   -- 31.53 MHz
   constant C_QNICE_PERIOD : time := 20 ns;     -- 50 MHz

   -- The protocol's registers (command_intf.h)
   constant R_SLOT_BASE    : natural := 16#0#;
   constant R_SLOT_ENABLE  : natural := 16#1#;
   constant R_HANDSHAKE    : natural := 16#2#;
   constant R_STATUSBYTE   : natural := 16#3#;
   constant R_CMD_START    : natural := 16#4#;
   constant R_RESP_START   : natural := 16#6#;
   constant R_STAT_START   : natural := 16#8#;
   constant R_STAT_LEN     : natural := 16#A#;
   constant R_RESP_LEN_L   : natural := 16#C#;
   constant R_RESP_LEN_H   : natural := 16#D#;
   constant R_CMD_LEN_L    : natural := 16#E#;
   constant R_CMD_LEN_H    : natural := 16#F#;
   constant R_IRQMASK_CLR  : natural := 16#5#;
   constant R_FLAGS        : natural := 16#10#;
   constant RAM_CMD        : natural := 16#800#;
   constant RAM_RESP       : natural := 16#800# + 896;
   constant RAM_STAT       : natural := 16#800# + 1792;

   signal main_clk  : std_logic := '0';
   signal qnice_clk : std_logic := '0';
   signal main_rst  : std_logic := '1';
   signal qnice_rst : std_logic := '1';
   signal done      : boolean := false;

   signal c128_rst  : std_logic := '0';
   signal cs        : std_logic := '0';
   signal we        : std_logic := '0';
   signal addr      : std_logic_vector(15 downto 0) := x"0000";
   signal wdata     : std_logic_vector(7 downto 0) := x"00";
   signal rdata     : std_logic_vector(7 downto 0);
   signal irq       : std_logic;
   signal dma       : std_logic;

   signal q_addr    : std_logic_vector(11 downto 0) := (others => '0');
   signal q_wdata   : std_logic_vector(15 downto 0) := (others => '0');
   signal q_rdata   : std_logic_vector(15 downto 0);
   signal q_ce      : std_logic := '0';
   signal q_we      : std_logic := '0';
   signal q_wait    : std_logic;

   -- What the firmware saw, for the C128 side's checks
   signal fw_ready      : boolean := false;
   signal fw_resets     : natural := 0;
   signal fw_reset_kept : boolean := false;   -- the flag read 1 right after a clear (reset still on)

begin

   main_clk  <= not main_clk  after C_MAIN_PERIOD / 2  when not done;
   qnice_clk <= not qnice_clk after C_QNICE_PERIOD / 2 when not done;

   i_dut : entity work.uci
      port map (
         main_clk_i      => main_clk,
         main_rst_i      => main_rst,
         main_c128_rst_i => c128_rst,
         main_cs_i       => cs,
         main_we_i       => we,
         main_addr_i     => addr,
         main_data_i     => wdata,
         main_data_o     => rdata,
         main_irq_o      => irq,
         main_dma_o      => dma,
         qnice_clk_i     => qnice_clk,
         qnice_rst_i     => qnice_rst,
         qnice_addr_i    => q_addr,
         qnice_data_i    => q_wdata,
         qnice_ce_i      => q_ce,
         qnice_we_i      => q_we,
         qnice_data_o    => q_rdata,
         qnice_wait_o    => q_wait
      ); -- i_dut

   ---------------------------------------------------------------------------------------------
   -- Firmware: Gideon's run_task, polled
   ---------------------------------------------------------------------------------------------

   p_fw : process
      variable v       : std_logic_vector(7 downto 0);
      variable len     : natural;
      variable cmd     : natural;
      variable stalled : boolean := false;
      variable second  : boolean := false;

      -- rv_soc: ce from a rising edge; the next rising edge without wait ends the access
      procedure dev(a : natural; w : std_logic; d : natural; r : out std_logic_vector(7 downto 0)) is
      begin
         wait until rising_edge(qnice_clk);
         q_ce    <= '1';
         q_we    <= w;
         q_addr  <= std_logic_vector(to_unsigned(a, 12));
         q_wdata <= std_logic_vector(to_unsigned(d, 16));
         loop
            wait until rising_edge(qnice_clk);
            exit when q_wait = '0';
         end loop;
         r    := q_rdata(7 downto 0);
         q_ce <= '0';
         q_we <= '0';
      end procedure dev;

      procedure wr(a : natural; d : natural) is
         variable dummy : std_logic_vector(7 downto 0);
      begin
         dev(a, '1', d, dummy);
      end procedure wr;

      procedure rdv(a : natural; r : out natural) is
         variable d : std_logic_vector(7 downto 0);
      begin
         dev(a, '0', 0, d);
         r := to_integer(unsigned(d));
      end procedure rdv;

      procedure reply(data_len : natural; first : natural; stat : string; last : boolean) is
      begin
         for i in 0 to data_len - 1 loop
            wr(RAM_RESP + i, (first + i) mod 256);
         end loop;
         for i in stat'range loop
            wr(RAM_STAT + i - stat'low, character'pos(stat(i)));
         end loop;
         wr(R_RESP_LEN_H, data_len / 256);
         wr(R_RESP_LEN_L, data_len mod 256);
         wr(R_STAT_LEN, stat'length);
         if last then
            wr(R_HANDSHAKE, 16#10#);
         else
            wr(R_HANDSHAKE, 16#30#);
         end if;
      end procedure reply;

      variable n : natural;
   begin
      wait until qnice_rst = '0';
      for i in 1 to 5 loop
         wait until rising_edge(qnice_clk);
      end loop;

      -- 1. buffer offsets in units of 8 bytes, then as command_intf.cc sets up
      rdv(R_CMD_START, n);
      assert n = 0 report "1: command buffer at " & integer'image(n) severity error;
      rdv(R_RESP_START, n);
      assert n = 896 / 8 report "1: response buffer at " & integer'image(n) severity error;
      rdv(R_STAT_START, n);
      assert n = 1792 / 8 report "1: status buffer at " & integer'image(n) severity error;
      wr(R_SLOT_BASE, 16#47#);                   -- $DF1C
      wr(R_SLOT_ENABLE, 1);
      wr(R_HANDSHAKE, 16#87#);                   -- HANDSHAKE_RESET
      wr(R_IRQMASK_CLR, 7);
      wr(R_FLAGS, 1);
      fw_ready <= true;

      loop
         rdv(R_FLAGS, n);
         if n mod 2 = 1 then
            fw_resets <= fw_resets + 1;
            wr(R_FLAGS, 1);
            rdv(R_FLAGS, n);
            fw_reset_kept <= n mod 2 = 1;
         end if;

         rdv(R_STATUSBYTE, n);
         v := std_logic_vector(to_unsigned(n, 8));
         if v(2) = '1' then                      -- abort
            stalled := false;
            wr(R_HANDSHAKE, 16#87#);
         end if;
         if v(1) = '1' then                      -- data accepted: the second block of command 2
            if second then
               second := false;
               reply(10, 16#B0#, "00,OK", true);
            end if;
            wr(R_HANDSHAKE, 16#02#);
         end if;
         if v(0) = '1' and not stalled then      -- new command
            rdv(R_CMD_LEN_L, len);
            rdv(R_CMD_LEN_H, n);
            len := len + 256 * n;
            rdv(RAM_CMD + 1, cmd);
            rdv(RAM_CMD, n);
            if n >= 16#80# then                  -- no reply
               wr(R_HANDSHAKE, 16#01#);
               wr(R_HANDSHAKE, 16#87#);
            elsif cmd = 1 then
               wr(R_HANDSHAKE, 16#01#);
               reply(5, 16#41#, "00,OK", true);
            elsif cmd = 2 then
               wr(R_HANDSHAKE, 16#01#);
               second := true;
               reply(300, 16#20#, "00,MORE", false);
            elsif cmd = 3 then                   -- echo
               wr(R_HANDSHAKE, 16#01#);
               for i in 0 to len - 1 loop
                  rdv(RAM_CMD + i, n);
                  wr(RAM_RESP + i, n);
               end loop;
               wr(R_RESP_LEN_H, len / 256);
               wr(R_RESP_LEN_L, len mod 256);
               wr(R_STAT_LEN, 0);
               wr(R_HANDSHAKE, 16#10#);
            elsif cmd = 4 then                   -- no answer until an abort
               wr(R_HANDSHAKE, 16#01#);
               stalled := true;
            elsif cmd = 5 then                   -- answer 40 us later
               wr(R_HANDSHAKE, 16#01#);
               wait for 40 us;
               reply(1, 16#55#, "00,OK", true);
            end if;
         end if;
      end loop;
   end process p_fw;

   ---------------------------------------------------------------------------------------------
   -- C128
   ---------------------------------------------------------------------------------------------

   p_c128 : process
      variable st  : std_logic_vector(7 downto 0);
      variable b   : std_logic_vector(7 downto 0);
      variable cnt : natural;

      -- One bus cycle: the address in PHI1 (8 clocks), the select in PHI2 (12 clocks)
      procedure bus_cycle(a : std_logic_vector(15 downto 0); w : std_logic;
                          d : std_logic_vector(7 downto 0); r : out std_logic_vector(7 downto 0)) is
      begin
         wait until rising_edge(main_clk);
         addr  <= a;
         we    <= w;
         wdata <= d;
         for i in 1 to 8 loop
            wait until rising_edge(main_clk);
         end loop;
         cs <= '1' when a(15 downto 3) = x"DF1" & '1' else '0';
         for i in 1 to 12 loop
            wait until rising_edge(main_clk);
         end loop;
         r  := rdata;
         cs <= '0';
         we <= '0';
      end procedure bus_cycle;

      procedure poke(a : std_logic_vector(15 downto 0); d : natural) is
         variable dummy : std_logic_vector(7 downto 0);
      begin
         bus_cycle(a, '1', std_logic_vector(to_unsigned(d, 8)), dummy);
      end procedure poke;

      procedure peek(a : std_logic_vector(15 downto 0); r : out std_logic_vector(7 downto 0)) is
      begin
         bus_cycle(a, '0', x"00", r);
      end procedure peek;

      -- Wait for a state (status bits 5-4)
      procedure wait_state(s : std_logic_vector(1 downto 0); tag : string) is
         variable v : std_logic_vector(7 downto 0);
      begin
         for i in 1 to 20000 loop
            peek(x"DF1C", v);
            if v(5 downto 4) = s then
               return;
            end if;
         end loop;
         report tag & ": state never became " & to_string(s) & " (status " & to_hstring(v) & ")" severity error;
      end procedure wait_state;

      -- Read the response queue until DATA_AV drops; count the bytes and check them
      procedure read_resp(first : natural; expect : natural; tag : string) is
         variable v : std_logic_vector(7 downto 0);
         variable k : natural := 0;
         variable bad : natural := 0;
      begin
         loop
            peek(x"DF1C", v);
            exit when v(7) = '0' or k > 1000;
            peek(x"DF1E", v);
            if to_integer(unsigned(v)) /= (first + k) mod 256 then
               bad := bad + 1;
            end if;
            k := k + 1;
         end loop;
         assert k = expect report tag & ": " & integer'image(k) & " response bytes, expected " &
                                  integer'image(expect) severity error;
         assert bad = 0 report tag & ": " & integer'image(bad) & " response bytes wrong" severity error;
      end procedure read_resp;

      procedure read_stat(expect : string; tag : string) is
         variable v : std_logic_vector(7 downto 0);
         variable s : string(1 to 40);
         variable k : natural := 0;
      begin
         loop
            peek(x"DF1C", v);
            exit when v(6) = '0' or k = 40;
            peek(x"DF1F", v);
            k    := k + 1;
            s(k) := character'val(to_integer(unsigned(v)));
         end loop;
         assert s(1 to k) = expect report tag & ": status """ & s(1 to k) & """, expected """ &
                                          expect & """" severity error;
      end procedure read_stat;

      procedure push(cmd : natural; ctrl : natural) is
      begin
         poke(x"DF1D", 1);          -- target 1
         poke(x"DF1D", cmd);
         poke(x"DF1C", ctrl);
      end procedure push;

   begin
      wait for 200 ns;
      wait until rising_edge(main_clk);
      main_rst  <= '0';
      qnice_rst <= '0';
      wait until fw_ready;

      -- 1.
      peek(x"DF1C", st);
      assert st = x"00" report "1: status " & to_hstring(st) & ", expected 00" severity error;
      peek(x"DF1D", st);
      assert st = x"C9" report "1: identification " & to_hstring(st) & ", expected C9" severity error;
      for a in 16#18# to 16#1A# loop
         peek(x"DF" & std_logic_vector(to_unsigned(a, 8)), st);
         assert st = x"FF" report "1: $DF" & to_hstring(to_unsigned(a, 8)) & " reads " & to_hstring(st)
                                  severity error;
      end loop;

      -- 2.
      push(1, 16#01#);
      wait_state("10", "2");
      peek(x"DF1C", st);
      assert st(7 downto 6) = "11" report "2: status " & to_hstring(st) & " without DATA_AV and STAT_AV"
                                   severity error;
      read_resp(16#41#, 5, "2");
      read_stat("00,OK", "2");
      poke(x"DF1C", 16#02#);
      peek(x"DF1C", st);
      assert st = x"00" report "2: status " & to_hstring(st) & " after DATA_ACC" severity error;

      -- 3.
      push(2, 16#01#);
      wait_state("11", "3");
      read_resp(16#20#, 300, "3 first block");
      read_stat("00,MORE", "3 first block");
      poke(x"DF1C", 16#02#);
      wait_state("10", "3");
      read_resp(16#B0#, 10, "3 second block");
      read_stat("00,OK", "3 second block");
      poke(x"DF1C", 16#02#);
      peek(x"DF1C", st);
      assert st = x"00" report "3: status " & to_hstring(st) & " at the end" severity error;

      -- 4. bytes 0 and 1 are the target and 3; the rest count on from 2
      poke(x"DF1D", 1);
      poke(x"DF1D", 3);
      for i in 2 to 899 loop
         poke(x"DF1D", i mod 256);
      end loop;
      poke(x"DF1C", 16#01#);
      wait_state("10", "4");
      peek(x"DF1E", st);
      assert st = x"01" report "4: echo byte 0 is " & to_hstring(st) severity error;
      peek(x"DF1E", st);
      assert st = x"03" report "4: echo byte 1 is " & to_hstring(st) severity error;
      read_resp(2, 895 - 2, "4");
      poke(x"DF1C", 16#02#);

      -- 5.
      poke(x"DF1D", 16#81#);
      poke(x"DF1D", 1);
      poke(x"DF1C", 16#01#);
      wait_state("00", "5");
      peek(x"DF1C", st);
      assert st = x"00" report "5: status " & to_hstring(st) severity error;

      -- 6.
      push(4, 16#01#);
      wait_state("01", "6");
      push(1, 16#01#);
      peek(x"DF1C", st);
      assert st(3) = '1' report "6: no ERROR after a push while busy (status " & to_hstring(st) & ")"
                         severity error;
      poke(x"DF1C", 16#08#);
      peek(x"DF1C", st);
      assert st(3) = '0' report "6: CLR_ERR left ERROR set" severity error;
      poke(x"DF1C", 16#04#);
      wait_state("00", "6");
      for i in 1 to 50 loop
         peek(x"DF1C", st);
         exit when st = x"00";
      end loop;
      assert st = x"00" report "6: status " & to_hstring(st) & " after ABORT" severity error;

      -- 7.
      assert irq = '0' report "7: IRQ before the test" severity error;
      push(1, 16#21#);
      wait_state("10", "7");
      assert irq = '1' report "7: no IRQ with the reply" severity error;
      peek(x"DF1D", st);
      assert st = x"49" report "7: identification " & to_hstring(st) & " with IRQ, expected 49" severity error;
      peek(x"DF1E", st);
      assert irq = '0' report "7: reading the reply left IRQ on" severity error;
      peek(x"DF1D", st);
      assert st = x"C9" report "7: identification " & to_hstring(st) & " after the IRQ" severity error;
      poke(x"DF1C", 16#02#);
      wait_state("00", "7");

      -- 8.
      push(5, 16#81#);
      assert dma = '1' report "8: the DMA bit does not hold the CPU" severity error;
      wait_state("10", "8");
      assert dma = '0' report "8: DMA still on after the reply" severity error;
      poke(x"DF1C", 16#02#);
      push(5, 16#41#);
      poke(x"1234", 0);
      assert dma = '0' report "8: TRIGGER held the CPU before $FF00" severity error;
      poke(x"FF00", 0);
      wait until rising_edge(main_clk);
      assert dma = '1' report "8: TRIGGER did not hold the CPU at $FF00" severity error;
      wait_state("10", "8");
      assert dma = '0' report "8: TRIGGER: DMA still on after the reply" severity error;
      poke(x"DF1C", 16#02#);
      wait_state("00", "8");

      -- 9.
      cnt := fw_resets;
      wait until rising_edge(main_clk);
      c128_rst <= '1';
      for i in 1 to 10 loop
         wait until rising_edge(main_clk);
      end loop;
      c128_rst <= '0';
      wait for 20 us;
      assert fw_resets > cnt report "9: the firmware did not see the reset" severity error;
      assert not fw_reset_kept report "9: writing 1 did not clear the flag" severity error;

      report "ALL CHECKS PASSED";
      done <= true;
      wait;
   end process p_c128;

end architecture sim;
