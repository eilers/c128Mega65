----------------------------------------------------------------------------------
-- MEGA65 port of the C128 core
--
-- Internet modem behind an emulated SwiftLink. The C128 sees a 6551 (acia6551);
-- a NEORV32 runs lwIP and a Hayes command set (CORE/modem-fw) on the MEGA65's
-- Ethernet port. The firmware runs from HyperRAM, where the Shell loads
-- /c128/modem.bin; the CPU sits in reset while the modem is switched off.
--
-- CPU address map: 0x40000000 firmware (HyperRAM, i-cache), 0x80000000 16 KB
-- data RAM, 0xF0000000 the registers below (see CORE/modem-fw/src/hw.h), 0xF0003000
-- the boot code (CORE/modem-fw/boot), where the CPU starts.
--
-- GPL-3.0-or-later
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library neorv32;
use neorv32.neorv32_package.all;
use neorv32.neorv32_bootrom_image.all;

library unisim;
use unisim.vcomponents.all;

entity modem is
   generic (
      G_MAIN_CLK_HZ : natural;
      G_HR_BASE     : natural;            -- HyperRAM word address of the firmware window
      G_SIM         : boolean := false
   );
   port (
      -- Modem clock (50 MHz): CPU and Ethernet
      clk_i          : in  std_logic;
      rst_i          : in  std_logic;

      -- C128 side, main clock
      main_clk_i     : in  std_logic;
      main_rst_i     : in  std_logic;     -- C128 reset
      enable_i       : in  std_logic;     -- switched on in the menu
      cs_i           : in  std_logic;
      we_i           : in  std_logic;
      addr_i         : in  std_logic_vector(2 downto 0);
      data_i         : in  std_logic_vector(7 downto 0);
      data_o         : out std_logic_vector(7 downto 0);
      irq_o          : out std_logic;

      -- HyperRAM, hr_clk
      hr_clk_i           : in  std_logic;
      hr_rst_i           : in  std_logic;
      hr_write_o         : out std_logic;
      hr_read_o          : out std_logic;
      hr_address_o       : out std_logic_vector(31 downto 0);
      hr_writedata_o     : out std_logic_vector(15 downto 0);
      hr_byteenable_o    : out std_logic_vector(1 downto 0);
      hr_burstcount_o    : out std_logic_vector(7 downto 0);
      hr_readdata_i      : in  std_logic_vector(15 downto 0);
      hr_readdatavalid_i : in  std_logic;
      hr_waitrequest_i   : in  std_logic;

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
end entity modem;

architecture synthesis of modem is

   constant C_CLK_HZ     : natural := 50_000_000;
   constant C_LINE_BYTES : natural := 64;

   -- Clock domain crossing
   signal enable_m    : std_logic := '0';
   signal enable_s    : std_logic := '0';
   signal cpu_rstn    : std_logic := '0';
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

   attribute async_reg : string;
   attribute async_reg of enable_m, enable_s : signal is "true";
   attribute async_reg of lines_m, lines_s   : signal is "true";
   attribute async_reg of state_m, state_s   : signal is "true";
   attribute async_reg of ev_tog_m, ev_tog_s : signal is "true";

   -- 6551 and its FIFOs
   signal acia_command : std_logic_vector(7 downto 0);
   signal acia_control : std_logic_vector(7 downto 0);
   signal acia_turbo   : std_logic_vector(1 downto 0);
   signal acia_ev_ctrl, acia_ev_cmd, acia_ev_reset : std_logic;
   signal main_rx_data  : std_logic_vector(7 downto 0);
   signal main_rx_valid : std_logic;
   signal main_rx_pop   : std_logic;
   signal main_tx_data  : std_logic_vector(7 downto 0);
   signal main_tx_push  : std_logic;
   signal main_tx_ready : std_logic;
   signal fifo_rst      : std_logic;

   signal rx_push  : std_logic := '0';
   signal rx_data  : std_logic_vector(7 downto 0);
   signal rx_fill  : natural range 0 to 64;
   signal tx_valid : std_logic;
   signal tx_pop   : std_logic;
   signal tx_data  : std_logic_vector(7 downto 0);

   -- CPU bus
   signal xbus_adr   : std_ulogic_vector(31 downto 0);
   signal xbus_dat_o : std_ulogic_vector(31 downto 0);
   signal xbus_cti   : std_ulogic_vector(2 downto 0);
   signal xbus_we    : std_ulogic;
   signal xbus_sel   : std_ulogic_vector(3 downto 0);
   signal xbus_stb   : std_ulogic;
   signal xbus_dat_i : std_ulogic_vector(31 downto 0);
   signal xbus_ack   : std_ulogic;
   signal xbus_err   : std_ulogic;

   signal hr_stb, hr_ack, hr_err : std_logic;
   signal hr_dat     : std_logic_vector(31 downto 0);
   signal reg_ack    : std_logic := '0';
   signal reg_err    : std_logic := '0';
   signal reg_dat    : std_logic_vector(31 downto 0) := (others => '0');

   -- Ethernet
   signal eth_rx_ready : std_logic;
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
         ev_reset_o => acia_ev_reset
      ); -- i_acia

   state_main <= acia_turbo & acia_control & acia_command;

   main_cdc_proc : process (main_clk_i)
   begin
      if rising_edge(main_clk_i) then
         lines_m <= lines;
         lines_s <= lines_m;
         ev_tog  <= ev_tog xor (acia_ev_reset & acia_ev_cmd & acia_ev_ctrl);
      end if;
   end process main_cdc_proc;

   -- The FIFOs empty while the CPU is in reset
   fifo_rst <= not cpu_rstn;

   -- Modem to C128
   i_rx_fifo : entity work.axis_fifo_async
      generic map (
         G_ADDR_BITS => 6,
         G_DATA_BITS => 8,
         G_RAM_STYLE => "distributed"
      )
      port map (
         async_rst_i => fifo_rst,
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
         async_rst_i => fifo_rst,
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
   -- Modem side (modem clock)
   -----------------------------------------------------------------------------

   modem_cdc_proc : process (clk_i)
   begin
      if rising_edge(clk_i) then
         enable_m <= enable_i;
         enable_s <= enable_m;
         state_m  <= state_main;
         state_s  <= state_m;
         ev_tog_m <= ev_tog;
         ev_tog_s <= ev_tog_m;
         ev_tog_d <= ev_tog_s;
         cpu_rstn <= enable_s and not rst_i;
      end if;
   end process modem_cdc_proc;

   i_cpu : entity neorv32.neorv32_top
      generic map (
         CLOCK_FREQUENCY   => C_CLK_HZ,
         BOOT_MODE_SELECT  => 1,           -- start at the boot code below, a LUT ROM; NEORV32's boot ROM took a RAMB18
         BOOT_ADDR_CUSTOM  => x"F0003000",
         RISCV_ISA_C       => true,
         RISCV_ISA_M       => true,
         RISCV_ISA_Zicntr  => true,
         CPU_RF_ARCH_SEL   => 2,           -- register file in flip-flops; the RAM styles took a RAMB36
         DMEM_EN           => true,
         DMEM_SIZE         => 16 * 1024,
         -- The caches split into byte lanes of at least a RAMB18 each: 8 KB of i-cache costs
         -- no more than 4 KB, and even 1 KB of d-cache would take 2 tiles. Without one, the
         -- firmware's constants come from HyperRAM a word at a time.
         ICACHE_EN         => true,
         ICACHE_NUM_BLOCKS => 128,         -- 8 KB
         DCACHE_EN         => false,
         CACHE_BLOCK_SIZE  => C_LINE_BYTES,
         CACHE_BURSTS_EN   => true,
         CACHE_UC_BASE     => x"F0000000",
         XBUS_EN           => true,
         XBUS_TIMEOUT      => 8192,
         IO_CLINT_EN       => true
      )
      port map (
         clk_i      => clk_i,
         rstn_i     => cpu_rstn,
         xbus_adr_o => xbus_adr,
         xbus_dat_o => xbus_dat_o,
         xbus_cti_o => xbus_cti,
         xbus_we_o  => xbus_we,
         xbus_sel_o => xbus_sel,
         xbus_stb_o => xbus_stb,
         xbus_dat_i => xbus_dat_i,
         xbus_ack_i => xbus_ack,
         xbus_err_i => xbus_err
      ); -- i_cpu

   -- 0x4xxxxxxx: HyperRAM. 0xF000xxxx: registers. Anything else is a bus error.
   hr_stb     <= xbus_stb when xbus_adr(31 downto 28) = x"4" else '0';
   xbus_ack   <= hr_ack or reg_ack;
   xbus_err   <= hr_err or reg_err;
   xbus_dat_i <= std_ulogic_vector(hr_dat) when hr_ack = '1' else std_ulogic_vector(reg_dat);

   i_hr_bridge : entity work.modem_hr_bridge
      generic map (
         G_BASE       => G_HR_BASE,
         G_LINE_BYTES => C_LINE_BYTES
      )
      port map (
         clk_i              => clk_i,
         rst_i              => not cpu_rstn,
         stb_i              => hr_stb,
         adr_i              => std_logic_vector(xbus_adr(19 downto 0)),
         we_i               => xbus_we,
         cti_i              => std_logic_vector(xbus_cti),
         dat_o              => hr_dat,
         ack_o              => hr_ack,
         err_o              => hr_err,
         hr_clk_i           => hr_clk_i,
         hr_rst_i           => hr_rst_i,
         hr_write_o         => hr_write_o,
         hr_read_o          => hr_read_o,
         hr_address_o       => hr_address_o,
         hr_writedata_o     => hr_writedata_o,
         hr_byteenable_o    => hr_byteenable_o,
         hr_burstcount_o    => hr_burstcount_o,
         hr_readdata_i      => hr_readdata_i,
         hr_readdatavalid_i => hr_readdatavalid_i,
         hr_waitrequest_i   => hr_waitrequest_i
      ); -- i_hr_bridge

   -- Registers. Reads of ACIA_TX and ETH_RX pop their FIFO.
   tx_pop       <= '1' when xbus_stb = '1' and xbus_we = '0' and xbus_adr = x"F0000004" and tx_valid = '1' else '0';
   eth_rx_ready <= '1' when xbus_stb = '1' and xbus_we = '0' and xbus_adr = x"F0001000" and eth_rx_valid = '1' else '0';

   regs_proc : process (clk_i)
      variable v_ev : std_logic_vector(2 downto 0);
   begin
      if rising_edge(clk_i) then
         reg_ack      <= '0';
         reg_err      <= '0';
         rx_push      <= '0';
         eth_tx_valid <= '0';
         v_ev         := events or (ev_tog_s xor ev_tog_d);

         if xbus_stb = '1' and xbus_adr(31 downto 28) /= x"4" then
            reg_ack <= '1';
            reg_dat <= (others => '0');
            if xbus_adr(31 downto 16) /= x"F000" then
               reg_err <= '1';
            elsif xbus_we = '1' then
               case xbus_adr(15 downto 0) is
                  when x"0000" =>                                    -- ACIA_RX
                     rx_push <= '1';
                     rx_data <= std_logic_vector(xbus_dat_o(7 downto 0));
                  when x"000C" =>                                    -- ACIA_LINES
                     lines <= std_logic_vector(xbus_dat_o(2 downto 0));
                  when x"0010" =>                                    -- ACIA_EVENTS
                     v_ev := v_ev and not std_logic_vector(xbus_dat_o(2 downto 0));
                  when x"1004" =>                                    -- ETH_TX
                     eth_tx_valid <= '1';
                     eth_tx_data  <= std_logic_vector(xbus_dat_o(7 downto 0));
                     eth_tx_last  <= xbus_dat_o(8);
                  when others =>
                     null;
               end case;
            elsif xbus_adr(15 downto 8) = x"30" then                -- boot code
               if to_integer(unsigned(xbus_adr(7 downto 2))) < image_size_c / 4 then
                  reg_dat <= std_logic_vector(image_data_c(to_integer(unsigned(xbus_adr(7 downto 2)))));
               end if;
            else
               case xbus_adr(15 downto 0) is
                  when x"0000" =>                                    -- ACIA_RX: free entries
                     reg_dat(7 downto 0) <= std_logic_vector(to_unsigned(63 - minimum(rx_fill, 63), 8));
                  when x"0004" =>                                    -- ACIA_TX
                     reg_dat(8 downto 0) <= tx_valid & tx_data;
                  when x"0008" =>                                    -- ACIA_STATE
                     reg_dat(17 downto 0) <= state_s;
                     reg_dat(24)          <= enable_s;
                  when x"000C" =>
                     reg_dat(2 downto 0) <= lines;
                  when x"0010" =>
                     reg_dat(2 downto 0) <= v_ev;
                  when x"1000" =>                                    -- ETH_RX
                     reg_dat(31)         <= eth_rx_valid;
                     reg_dat(8 downto 0) <= eth_rx_last & eth_rx_data;
                  when x"1008" =>                                    -- ETH_STATUS
                     reg_dat(0)            <= eth_tx_room;
                     reg_dat(31 downto 16) <= eth_drops;
                  when x"2000" =>                                    -- SYS_DNA_LO
                     reg_dat <= dna(31 downto 0);
                  when x"2004" =>
                     reg_dat(24 downto 0) <= dna(56 downto 32);
                     reg_dat(31)          <= dna_valid;
                  when others =>
                     null;
               end case;
            end if;
         end if;
         events <= v_ev;

         if cpu_rstn = '0' then
            lines  <= "000";
            events <= "000";
         end if;
      end if;
   end process regs_proc;

   -----------------------------------------------------------------------------
   -- Ethernet
   -----------------------------------------------------------------------------

   i_eth : entity work.eth_wrapper
      generic map (
         G_SIM          => G_SIM,
         G_RX_ADDR_BITS => 12,
         G_TX_ADDR_BITS => 11
      )
      port map (
         core_clk_i         => clk_i,
         core_rst_i         => not cpu_rstn,
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
         eth_rst_i          => not cpu_rstn,
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
