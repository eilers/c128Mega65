----------------------------------------------------------------------------------
-- MEGA65 port of the C128 core
--
-- The network hardware, a device on the system CPU's device bus (C_DEV_NET): the
-- SwiftLink's 6551 (acia6551) and its FIFOs, the Ethernet MAC (eth_wrapper) and the
-- device DNA for the MAC address. lwIP, the Hayes modem, UCI target 3 and the REST
-- server run in the system firmware's main loop.
--
-- One clock, clk_i (the Ethernet's 50 MHz). The device bus comes from qnice_clk, the
-- same MMCM's 50 MHz with phase 0, so its paths here are timed like any other device's:
-- an access acts on the falling edge, its read is taken at the next rising edge.
--
-- Registers, 16-bit words (M2M/firmware/include/net_hw.h). A 32-bit read of words 2
-- and 16/18 gives the layouts the modem's own CPU had.
--   0  ACIA_RX      w: a byte for the C128            r: free entries in its FIFO (0..63)
--   1  ACIA_TX      r: bit 8 valid, 7..0 the C128's next byte; the read takes it
--   2  ACIA_CMD     r: 6551 control (15..8), command (7..0)
--   3  ACIA_STATE   r: bit 8 switched on in the menu, Turbo232 rate (1..0)
--   4  ACIA_LINES   r/w: CTS 2, DSR 1, DCD 0
--   5  ACIA_EVENTS  r: reset 2, command 1, control 0 written; w: 1s clear them
--   6  ACIA_AGAINS  r: receive IRQs raised again for a byte left unread, since power-up
--   8  ETH_RX       r: bit 15 valid, bit 8 last of the frame, 7..0 the byte; the read takes it
--   9  ETH_TX       w: bit 8 last of the frame, 7..0 the byte
--  10  ETH_STATUS   r: bit 0 a whole frame fits in the transmit buffer
--  11  ETH_DROPS    r: frames received with errors or without room
--  12  ETH_CTRL     r/w: bit 0 run (the MAC and PHY leave reset)
--  16..19 DNA       r: the device DNA's 57 bits, 16 a word; word 19 bit 15 valid
--
-- GPL-3.0-or-later
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library unisim;
use unisim.vcomponents.all;

entity net is
   generic (
      G_MAIN_CLK_HZ : natural;
      G_SIM         : boolean := false
   );
   port (
      clk_i          : in  std_logic;     -- 50 MHz, with qnice_clk's phase
      rst_i          : in  std_logic;

      -- C128 side, main clock
      main_clk_i     : in  std_logic;
      main_rst_i     : in  std_logic;     -- C128 reset
      enable_i       : in  std_logic;     -- modem switched on in the menu
      cs_i           : in  std_logic;
      we_i           : in  std_logic;
      addr_i         : in  std_logic_vector(2 downto 0);
      data_i         : in  std_logic_vector(7 downto 0);
      data_o         : out std_logic_vector(7 downto 0);
      irq_o          : out std_logic;

      -- Device bus, from qnice_clk
      dev_ce_i       : in  std_logic;
      dev_we_i       : in  std_logic;
      dev_addr_i     : in  std_logic_vector(11 downto 0);
      dev_data_i     : in  std_logic_vector(15 downto 0);
      dev_data_o     : out std_logic_vector(15 downto 0);

      -- Ethernet PHY (RMII)
      eth_clock_o    : out   std_logic;
      eth_reset_o    : out   std_logic;
      eth_mdc_o      : out   std_logic;
      eth_mdio_io    : inout std_logic;
      eth_rxd_i      : in    std_logic_vector(1 downto 0);
      eth_rxdv_i     : in    std_logic;
      eth_rxer_i     : in    std_logic;
      eth_txd_o      : out   std_logic_vector(1 downto 0);
      eth_txen_o     : out   std_logic
   );
end entity net;

architecture synthesis of net is

   -- Clock domain crossing
   signal enable_m    : std_logic := '0';
   signal enable_s    : std_logic := '0';
   signal acia_rst    : std_logic := '1';
   signal lines       : std_logic_vector(2 downto 0) := "000";   -- cts, dsr, dcd
   signal lines_m     : std_logic_vector(2 downto 0) := "000";
   signal lines_s     : std_logic_vector(2 downto 0) := "000";
   signal state_main  : std_logic_vector(17 downto 0);
   signal state_m     : std_logic_vector(17 downto 0) := (others => '0');
   signal state_s     : std_logic_vector(17 downto 0) := (others => '0');
   signal ev_tog      : std_logic_vector(2 downto 0) := "000";   -- toggles on main_clk
   signal ev_tog_m    : std_logic_vector(2 downto 0) := "000";
   signal ev_tog_s    : std_logic_vector(2 downto 0) := "000";
   signal ev_tog_d    : std_logic_vector(2 downto 0) := "000";
   signal events      : std_logic_vector(2 downto 0) := "000";
   signal ev_clear    : std_logic_vector(2 downto 0) := "000";
   signal again_tog   : std_logic := '0';                        -- toggles on main_clk
   signal again_tog_m : std_logic := '0';
   signal again_tog_s : std_logic := '0';
   signal again_tog_d : std_logic := '0';
   signal irq_agains  : unsigned(15 downto 0) := (others => '0'); -- since power-up

   attribute async_reg : string;
   attribute async_reg of enable_m, enable_s : signal is "true";
   attribute async_reg of lines_m, lines_s   : signal is "true";
   attribute async_reg of state_m, state_s   : signal is "true";
   attribute async_reg of ev_tog_m, ev_tog_s : signal is "true";
   attribute async_reg of again_tog_m, again_tog_s : signal is "true";

   -- 6551 and its FIFOs
   signal acia_command : std_logic_vector(7 downto 0);
   signal acia_control : std_logic_vector(7 downto 0);
   signal acia_turbo   : std_logic_vector(1 downto 0);
   signal acia_ev_ctrl, acia_ev_cmd, acia_ev_reset, acia_ev_again : std_logic;
   signal main_rx_data  : std_logic_vector(7 downto 0);
   signal main_rx_valid : std_logic;
   signal main_rx_pop   : std_logic;
   signal main_tx_data  : std_logic_vector(7 downto 0);
   signal main_tx_push  : std_logic;
   signal main_tx_ready : std_logic;

   signal rx_push  : std_logic := '0';
   signal rx_data  : std_logic_vector(7 downto 0) := x"00";
   signal rx_fill  : natural range 0 to 64;
   signal tx_valid : std_logic;
   signal tx_pop   : std_logic := '0';
   signal tx_data  : std_logic_vector(7 downto 0);

   -- Device bus: an access acts once, on its first falling edge
   signal dev_ce_d  : std_logic := '0';
   signal acia_rd   : std_logic_vector(8 downto 0) := (others => '0');   -- ACIA_TX as read
   signal eth_rd    : std_logic_vector(15 downto 0) := (others => '0');  -- ETH_RX as read

   -- Ethernet
   signal eth_run      : std_logic := '0';
   signal eth_rst      : std_logic;
   signal eth_rx_ready : std_logic := '0';
   signal eth_rx_valid : std_logic;
   signal eth_rx_last  : std_logic;
   signal eth_rx_data  : std_logic_vector(7 downto 0);
   signal eth_tx_valid : std_logic := '0';
   signal eth_tx_last  : std_logic := '0';
   signal eth_tx_data  : std_logic_vector(7 downto 0) := x"00";
   signal eth_tx_room  : std_logic;
   signal eth_drops    : std_logic_vector(15 downto 0);

   -- Device DNA, for the MAC address
   signal dna        : std_logic_vector(56 downto 0) := (others => '0');
   signal dna_valid  : std_logic := '0';
   signal dna_count  : natural range 0 to 63 := 0;
   signal dna_read   : std_logic := '0';
   signal dna_shift  : std_logic := '0';
   signal dna_dout   : std_logic;

begin

   -----------------------------------------------------------------------------
   -- C128 side (main clock)
   -----------------------------------------------------------------------------

   i_acia : entity work.acia6551
      generic map (
         G_CLK_HZ => G_MAIN_CLK_HZ
      )
      port map (
         clk_i      => main_clk_i,
         rst_i      => main_rst_i,
         enable_i   => enable_i,
         cs_i       => cs_i,
         we_i       => we_i,
         addr_i     => addr_i,
         data_i     => data_i,
         data_o     => data_o,
         irq_o      => irq_o,
         rx_data_i  => main_rx_data,
         rx_valid_i => main_rx_valid,
         rx_pop_o   => main_rx_pop,
         tx_data_o  => main_tx_data,
         tx_push_o  => main_tx_push,
         tx_ready_i => main_tx_ready,
         dcd_i      => lines_s(0),
         dsr_i      => lines_s(1),
         cts_i      => lines_s(2),
         command_o  => acia_command,
         control_o  => acia_control,
         turbo_o    => acia_turbo,
         ev_ctrl_o  => acia_ev_ctrl,
         ev_cmd_o   => acia_ev_cmd,
         ev_reset_o => acia_ev_reset,
         ev_again_o => acia_ev_again
      ); -- i_acia

   state_main <= acia_turbo & acia_control & acia_command;

   main_cdc_proc : process (main_clk_i)
   begin
      if rising_edge(main_clk_i) then
         lines_m <= lines;
         lines_s <= lines_m;
         ev_tog  <= ev_tog xor (acia_ev_reset & acia_ev_cmd & acia_ev_ctrl);
         again_tog <= again_tog xor acia_ev_again;
      end if;
   end process main_cdc_proc;

   -- Modem to C128; empty while the modem is switched off
   i_rx_fifo : entity work.axis_fifo_async
      generic map (
         G_ADDR_BITS => 6,
         G_DATA_BITS => 8,
         G_RAM_STYLE => "distributed"
      )
      port map (
         async_rst_i => acia_rst,
         s_clk_i     => clk_i,
         s_ready_o   => open,
         s_valid_i   => rx_push,
         s_data_i    => rx_data,
         s_fill_o    => rx_fill,
         m_clk_i     => main_clk_i,
         m_ready_i   => main_rx_pop,
         m_valid_o   => main_rx_valid,
         m_data_o    => main_rx_data,
         m_fill_o    => open
      ); -- i_rx_fifo

   -- C128 to modem
   i_tx_fifo : entity work.axis_fifo_async
      generic map (
         G_ADDR_BITS => 6,
         G_DATA_BITS => 8,
         G_RAM_STYLE => "distributed"
      )
      port map (
         async_rst_i => acia_rst,
         s_clk_i     => main_clk_i,
         s_ready_o   => main_tx_ready,
         s_valid_i   => main_tx_push,
         s_data_i    => main_tx_data,
         s_fill_o    => open,
         m_clk_i     => clk_i,
         m_ready_i   => tx_pop,
         m_valid_o   => tx_valid,
         m_data_o    => tx_data,
         m_fill_o    => open
      ); -- i_tx_fifo

   -----------------------------------------------------------------------------
   -- Network side (clk_i)
   -----------------------------------------------------------------------------

   cdc_proc : process (clk_i)
   begin
      if rising_edge(clk_i) then
         enable_m <= enable_i;
         enable_s <= enable_m;
         state_m  <= state_main;
         state_s  <= state_m;
         ev_tog_m <= ev_tog;
         ev_tog_s <= ev_tog_m;
         ev_tog_d <= ev_tog_s;
         again_tog_m <= again_tog;
         again_tog_s <= again_tog_m;
         again_tog_d <= again_tog_s;
         if again_tog_s /= again_tog_d then
            irq_agains <= irq_agains + 1;
         end if;
         acia_rst <= rst_i or not enable_s;

         events <= (events or (ev_tog_s xor ev_tog_d)) and not ev_clear;
         if acia_rst = '1' then
            events <= "000";
         end if;
      end if;
   end process cdc_proc;

   -- Writes, and the reads that take a byte, once per access. Their strobes last one clock
   -- from this falling edge, so the FIFOs and the MAC see one rising edge of each.
   dev_proc : process (clk_i)
   begin
      if falling_edge(clk_i) then
         dev_ce_d     <= dev_ce_i;
         rx_push      <= '0';
         tx_pop       <= '0';
         eth_rx_ready <= '0';
         eth_tx_valid <= '0';
         ev_clear     <= "000";

         if dev_ce_i = '1' and dev_ce_d = '0' then
            if dev_we_i = '1' then
               case dev_addr_i is
                  when x"000" =>
                     rx_push <= '1';
                     rx_data <= dev_data_i(7 downto 0);
                  when x"004" => lines    <= dev_data_i(2 downto 0);
                  when x"005" => ev_clear <= dev_data_i(2 downto 0);
                  when x"009" =>
                     eth_tx_valid <= '1';
                     eth_tx_data  <= dev_data_i(7 downto 0);
                     eth_tx_last  <= dev_data_i(8);
                  when x"00C" => eth_run  <= dev_data_i(0);
                  when others => null;
               end case;
            else
               case dev_addr_i is
                  when x"001" =>
                     acia_rd <= (others => '0');
                     if tx_valid = '1' then
                        acia_rd <= '1' & tx_data;
                        tx_pop  <= '1';
                     end if;
                  when x"008" =>
                     eth_rd <= (others => '0');
                     if eth_rx_valid = '1' then
                        eth_rd       <= "1000000" & eth_rx_last & eth_rx_data;
                        eth_rx_ready <= '1';
                     end if;
                  when others => null;
               end case;
            end if;
         end if;

         if acia_rst = '1' then
            lines <= "000";
         end if;
         if rst_i = '1' then
            eth_run <= '0';
         end if;
      end if;
   end process dev_proc;

   read_proc : process (all)
   begin
      dev_data_o <= (others => '0');
      case dev_addr_i is
         when x"000" => dev_data_o(7 downto 0)  <= std_logic_vector(to_unsigned(63 - minimum(rx_fill, 63), 8));
         when x"001" => dev_data_o(8 downto 0)  <= acia_rd;
         when x"002" => dev_data_o              <= state_s(15 downto 0);
         when x"003" =>
            dev_data_o(1 downto 0) <= state_s(17 downto 16);
            dev_data_o(8)          <= enable_s;
         when x"004" => dev_data_o(2 downto 0)  <= lines;
         when x"005" => dev_data_o(2 downto 0)  <= events;
         when x"006" => dev_data_o              <= std_logic_vector(irq_agains);
         when x"008" => dev_data_o              <= eth_rd;
         when x"00A" => dev_data_o(0)           <= eth_tx_room;
         when x"00B" => dev_data_o              <= eth_drops;
         when x"00C" => dev_data_o(0)           <= eth_run;
         when x"010" => dev_data_o              <= dna(15 downto 0);
         when x"011" => dev_data_o              <= dna(31 downto 16);
         when x"012" => dev_data_o              <= dna(47 downto 32);
         when x"013" =>
            dev_data_o(8 downto 0) <= dna(56 downto 48);
            dev_data_o(15)         <= dna_valid;
         when others => null;
      end case;
   end process read_proc;

   -----------------------------------------------------------------------------
   -- Ethernet, in reset until the firmware runs it
   -----------------------------------------------------------------------------

   eth_rst <= rst_i or not eth_run;

   i_eth : entity work.eth_wrapper
      generic map (
         G_SIM          => G_SIM,
         G_RX_ADDR_BITS => 12,
         G_TX_ADDR_BITS => 11
      )
      port map (
         core_clk_i         => clk_i,
         core_rst_i         => eth_rst,
         core_rx_ready_i    => eth_rx_ready,
         core_rx_valid_o    => eth_rx_valid,
         core_rx_last_o     => eth_rx_last,
         core_rx_data_o     => eth_rx_data,
         core_tx_ready_o    => open,
         core_tx_valid_i    => eth_tx_valid,
         core_tx_last_i     => eth_tx_last,
         core_tx_data_i     => eth_tx_data,
         core_rx_cnt_drop_o => eth_drops,
         core_tx_room_o     => eth_tx_room,
         eth_clk_i          => clk_i,
         eth_rst_i          => eth_rst,
         eth_clk_o          => eth_clock_o,
         eth_rst_n_o        => eth_reset_o,
         eth_led2_o         => open,
         eth_mdc_o          => eth_mdc_o,
         eth_mdio_io        => eth_mdio_io,
         eth_rx_d_i         => eth_rxd_i,
         eth_crs_dv_i       => eth_rxdv_i,
         eth_rxer_i         => eth_rxer_i,
         eth_tx_d_o         => eth_txd_o,
         eth_tx_en_o        => eth_txen_o
      ); -- i_eth

   -----------------------------------------------------------------------------
   -- Device DNA: 57 bits, shifted out once after power-up
   -----------------------------------------------------------------------------

   i_dna : DNA_PORT
      generic map (
         SIM_DNA_VALUE => x"0C12800000ABCDE"
      )
      port map (
         clk   => clk_i,
         read  => dna_read,
         shift => dna_shift,
         din   => '0',
         dout  => dna_dout
      ); -- i_dna

   dna_proc : process (clk_i)
   begin
      if rising_edge(clk_i) then
         dna_read  <= '0';
         dna_shift <= '0';
         if dna_valid = '0' then
            if dna_count = 0 then
               dna_read  <= '1';
               dna_count <= 1;
            elsif dna_count <= 57 then
               if dna_count > 1 then
                  dna <= dna(55 downto 0) & dna_dout;
               end if;
               dna_shift <= '1';
               dna_count <= dna_count + 1;
            else
               dna       <= dna(55 downto 0) & dna_dout;
               dna_valid <= '1';
            end if;
         end if;
      end if;
   end process dna_proc;

end architecture synthesis;
