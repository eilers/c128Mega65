----------------------------------------------------------------------------------
-- MEGA65 port of the C128 core
--
-- The C128's side of the emulated SwiftLink: a 6551 ACIA with SwiftLink's
-- doubled rates and the Turbo232 speed register at base+7. There is no serial
-- line; bytes come from and go to the modem through two FIFOs, one per
-- character time at the rate the C128 has set.
--
-- The behaviour follows the Ultimate's acia6551.vhd (1541ultimate,
-- fpga/io/acia), whose author measured it on a real 6551. Unlike a 6551 it
-- never overruns: the next byte waits until the C128 has read the last one.
--
-- GPL-3.0-or-later
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity acia6551 is
   generic (
      G_CLK_HZ   : natural                -- clk_i, for the character times
   );
   port (
      clk_i      : in  std_logic;
      rst_i      : in  std_logic;         -- C128 reset
      enable_i   : in  std_logic;         -- switched on in the menu; off = invisible, no IRQ

      -- C128 bus. cs_i is the select strobe for part of PHI2; the access happens on its rising edge.
      cs_i       : in  std_logic;
      we_i       : in  std_logic;
      addr_i     : in  std_logic_vector(2 downto 0);
      data_i     : in  std_logic_vector(7 downto 0);
      data_o     : out std_logic_vector(7 downto 0);
      irq_o      : out std_logic;

      -- Bytes from the modem (FIFO read side, first word fall through)
      rx_data_i  : in  std_logic_vector(7 downto 0);
      rx_valid_i : in  std_logic;
      rx_pop_o   : out std_logic;

      -- Bytes to the modem (FIFO write side)
      tx_data_o  : out std_logic_vector(7 downto 0);
      tx_push_o  : out std_logic;
      tx_ready_i : in  std_logic;

      -- Lines the modem drives, '1' = asserted
      dcd_i      : in  std_logic;
      dsr_i      : in  std_logic;
      cts_i      : in  std_logic;

      -- Registers and events for the modem
      command_o  : out std_logic_vector(7 downto 0);
      control_o  : out std_logic_vector(7 downto 0);
      turbo_o    : out std_logic_vector(1 downto 0);
      ev_ctrl_o  : out std_logic;         -- control register written
      ev_cmd_o   : out std_logic;         -- command register written
      ev_reset_o : out std_logic          -- reset or programmed reset
   );
end entity acia6551;

architecture synthesis of acia6551 is

   -- Clocks per character (start, 8 data, stop) at each rate, worked out at elaboration.
   -- Rate 0 is the Turbo232 extension, which takes its rate from the register at base+7.
   type t_times is array (natural range <>) of natural;

   function char_times(rates : t_times) return t_times is
      variable result : t_times(rates'range);
   begin
      for i in rates'range loop
         result(i) := G_CLK_HZ / rates(i) * 10;
      end loop;
      return result;
   end function;

   constant C_RATE_CLOCKS  : t_times(0 to 15) := char_times((38400, 100, 150, 220, 269, 300, 600, 1200, 2400,
                                                             3600, 4800, 7200, 9600, 14400, 19200, 38400));
   constant C_TURBO_CLOCKS : t_times(0 to 3)  := char_times((230400, 115200, 57600, 28800));

   signal char_clocks : natural range 0 to G_CLK_HZ / 10;
   signal rx_timer    : natural range 0 to G_CLK_HZ / 10 := 0;
   signal tx_timer    : natural range 0 to G_CLK_HZ / 10 := 0;

   signal cs_d        : std_logic := '0';
   signal command     : std_logic_vector(7 downto 0) := x"02";
   signal control     : std_logic_vector(7 downto 0) := x"00";
   signal turbo       : std_logic_vector(1 downto 0) := "00";
   signal rdr         : std_logic_vector(7 downto 0) := x"00";
   signal tdr         : std_logic_vector(7 downto 0) := x"00";
   signal irq         : std_logic := '0';
   signal tdre        : std_logic := '1';
   signal rdrf        : std_logic := '0';
   signal dcd_d       : std_logic := '0';
   signal dsr_d       : std_logic := '0';

   alias dtr          : std_logic is command(0);
   alias rx_irq_off   : std_logic is command(1);
   alias tx_ctrl      : std_logic_vector(1 downto 0) is command(3 downto 2);

begin

   command_o <= command;
   control_o <= control;
   turbo_o   <= turbo;
   irq_o     <= irq and enable_i;

   char_clocks <= C_TURBO_CLOCKS(to_integer(unsigned(turbo))) when control(3 downto 0) = "0000" else
                  C_RATE_CLOCKS(to_integer(unsigned(control(3 downto 0))));

   acia_proc : process (clk_i)
      variable v_status : std_logic_vector(7 downto 0);
   begin
      if rising_edge(clk_i) then
         cs_d       <= cs_i;
         rx_pop_o   <= '0';
         tx_push_o  <= '0';
         ev_ctrl_o  <= '0';
         ev_cmd_o   <= '0';
         ev_reset_o <= '0';

         if rx_timer /= 0 then
            rx_timer <= rx_timer - 1;
         end if;
         if tx_timer /= 0 then
            tx_timer <= tx_timer - 1;
         end if;

         -- Receive with DTR on and RTS asserted (RTS is how terminal programs pause the modem)
         if rx_valid_i = '1' and rx_timer = 0 and rdrf = '0' and dtr = '1' and tx_ctrl /= "00" then
            rdr      <= rx_data_i;
            rdrf     <= '1';
            rx_pop_o <= '1';
            rx_timer <= char_clocks;
            if rx_irq_off = '0' then
               irq <= '1';
            end if;
         end if;

         -- Transmit: the data register goes to the modem, one character time apart
         if tdre = '0' and tx_timer = 0 and tx_ready_i = '1' and cts_i = '1' then
            tx_data_o <= tdr;
            tx_push_o <= '1';
            tdre      <= '1';
            tx_timer  <= char_clocks;
         end if;

         -- With DTR on: the transmit IRQ while the data register is empty, and changes of DCD and DSR
         if dtr = '1' then
            if tdre = '1' and tx_ctrl = "01" then
               irq <= '1';
            end if;
            if dcd_i /= dcd_d or dsr_i /= dsr_d then
               irq <= '1';
            end if;
         end if;
         dcd_d <= dcd_i;
         dsr_d <= dsr_i;

         if cs_i = '1' and cs_d = '0' and enable_i = '1' then
            if we_i = '1' then
               case addr_i is
                  when "000" =>
                     tdr  <= data_i;
                     tdre <= '0';
                  when "001" =>                           -- programmed reset
                     command(4 downto 0) <= "00010";
                     ev_reset_o <= '1';
                  when "010" =>
                     command  <= data_i;
                     ev_cmd_o <= '1';
                  when "011" =>
                     control   <= data_i;
                     ev_ctrl_o <= '1';
                  when "111" =>                           -- Turbo232: only while the rate is 0
                     if control(3 downto 0) = "0000" then
                        turbo     <= data_i(1 downto 0);
                        ev_ctrl_o <= '1';
                     end if;
                  when others =>
                     null;
               end case;
            else
               v_status := irq & not dsr_i & not dcd_i & tdre & rdrf & "000";
               case addr_i is
                  when "000" =>
                     data_o <= rdr;
                     rdrf   <= '0';
                  when "001" =>
                     data_o <= v_status;
                     irq    <= '0';
                  when "010" =>
                     data_o <= command;
                  when "011" =>
                     data_o <= control;
                  when "111" =>
                     data_o <= "00000" & (control(3 downto 0) ?= "0000") & turbo;
                  when others =>
                     data_o <= x"FF";
               end case;
            end if;
         end if;

         if enable_i = '0' then
            irq <= '0';
         end if;

         if rst_i = '1' then
            command    <= x"02";
            control    <= x"00";
            turbo      <= "00";
            tdre       <= '1';
            rdrf       <= '0';
            irq        <= '0';
            rx_timer   <= 0;
            tx_timer   <= 0;
            ev_reset_o <= '1';
            rx_pop_o   <= rx_valid_i;                     -- empty out stale modem output
         end if;
      end if;
   end process acia_proc;

end architecture synthesis;
