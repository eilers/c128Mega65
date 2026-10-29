----------------------------------------------------------------------------------
-- MiSTer2MEGA65 Framework
--
-- RISC-V system CPU: a NEORV32 in place of the QNICE co-processor (QNICE/qnice.vhd).
-- Same ports as QNICE, plus HyperRAM and SDRAM masters, so qnice_wrapper.vhd can take either.
-- The firmware is in M2M/firmware; its view of this hardware is M2M/firmware/include/m2m_rv.h.
--
-- CPU address map:
--   0x00000000  boot ROM (block RAM, through the i-cache)
--   0x40000000  HyperRAM, 16 MB: byte A is word A/2, low byte first (rv_hr_bridge.vhd)
--   0x50000000  SDRAM, 64 MB, the same way (G_SDRAM; else a bus error)
--   0x80000000  data RAM (NEORV32 DMEM)
--   0xC0000000  QNICE device bus: 16-bit word N of the device in RAMROM_DEV at 0xC0000000 + 2N
--   0xE0000000  registers: CSR, OSM, ascal, keys, control flags (below)
--   0xE0000100  SD card controller registers 0..7 (sdhost.vhd), one per 32-bit word
--   0xFFE00000  NEORV32's own: UART0 (the serial port), CLINT
--
-- Clocks: the CPU, the boot ROM, HyperRAM and SDRAM run at 100 MHz (clk100_i, the HyperRAM
-- clock). The device bus, the registers and the SD controllers stay at 50 MHz
-- (clk50_i), where QNICE was. Both come from one MMCM with phase 0, so every 50 MHz
-- edge is a 100 MHz edge and the paths between them are timed: a request crosses
-- as a toggle with its address and data beside it, an answer the same way back.
-- The device bus keeps QNICE's timing: the devices act on the falling edge of the
-- 50 MHz clock and are read at the next rising edge, after their wait.
--
-- C128 core: RISC-V replacement for QNICE, licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

library neorv32;
use neorv32.neorv32_package.all;

use work.sdhost_pkg.all;

entity rv_soc is
generic (
   G_BOOT_ROM        : string;                           -- boot ROM image: one 32-bit word per line, in binary
   G_VGA_DX          : natural;                          -- output screen width in pixel
   G_VGA_DY          : natural;                          -- ditto height
   G_FONT_DX         : natural;                          -- character width in pixel
   G_FONT_DY         : natural;                          -- ditto height
   G_SDRAM           : boolean := false                  -- the board has SDRAM at 0x50000000
);
port (
   clk50_i              : in std_logic;            -- 50 MHz: device bus, registers, SD cards
   clk100_i             : in std_logic;            -- 100 MHz from the same MMCM, phase 0: CPU, HyperRAM
   reset_n_i            : in std_logic;            -- clk50_i

   uart_rxd_i           : in std_logic;            -- serial port: the firmware sets the speed
   uart_txd_o           : out std_logic;

   -- Micro SD Connector (external slot at back of the cover)
   sd_clk_o             : out std_logic;
   sd_cmd_o             : out std_logic;
   sd_cmd_oe_o          : out std_logic;
   sd_cmd_i             : in  std_logic;
   sd_dat_o             : out std_logic_vector(3 downto 0);
   sd_dat_oe_o          : out std_logic_vector(3 downto 0);
   sd_dat_i             : in  std_logic_vector(3 downto 0);
   sd_cd_i              : in std_logic;

   -- SD Connector (this is the slot at the bottom side of the case under the cover)
   sd2_clk_o            : out std_logic;
   sd2_cmd_o            : out std_logic;
   sd2_cmd_oe_o         : out std_logic;
   sd2_cmd_i            : in  std_logic;
   sd2_dat_o            : out std_logic_vector(3 downto 0);
   sd2_dat_oe_o         : out std_logic_vector(3 downto 0);
   sd2_dat_i            : in  std_logic_vector(3 downto 0);
   sd2_cd_i             : in std_logic;

   -- Engine port of the active SD card's controller: block transfers for the core
   sd_eng_i             : in  t_sd_c2q := (others => '0');
   sd_eng_o             : out t_sd_q2c;

   -- The registers QNICE had at 0xFFE0 and up (QNICE's sysdef.asm), clk50_i
   csr_reset_o          : out std_logic;
   csr_pause_o          : out std_logic;
   csr_osm_o            : out std_logic;
   csr_keyboard_o       : out std_logic;
   csr_joy1_o           : out std_logic;
   csr_joy2_o           : out std_logic;
   osm_xy_o             : out std_logic_vector(15 downto 0);
   osm_dxdy_o           : out std_logic_vector(15 downto 0);
   ascal_mode_i         : in std_logic_vector(4 downto 0);
   ascal_mode_o         : out std_logic_vector(4 downto 0);
   keys_n_i             : in std_logic_vector(15 downto 0);
   control_d_o          : out std_logic_vector(255 downto 0);
   control_m_o          : out std_logic_vector(255 downto 0);
   special_i            : in std_logic_vector(15 downto 0);
   general_i            : in std_logic_vector(15 downto 0);

   -- QNICE device bus, clk50_i
   ramrom_dev_o         : out std_logic_vector(15 downto 0);
   ramrom_addr_o        : out std_logic_vector(27 downto 0);
   ramrom_data_o        : out std_logic_vector(15 downto 0);
   ramrom_data_i        : in std_logic_vector(15 downto 0);
   ramrom_ce_o          : out std_logic;
   ramrom_wait_i        : in std_logic;
   ramrom_we_o          : out std_logic;

   -- HyperRAM, clk100_i
   hr_write_o           : out std_logic;
   hr_read_o            : out std_logic;
   hr_address_o         : out std_logic_vector(31 downto 0);
   hr_writedata_o       : out std_logic_vector(15 downto 0);
   hr_byteenable_o      : out std_logic_vector(1 downto 0);
   hr_burstcount_o      : out std_logic_vector(7 downto 0);
   hr_readdata_i        : in  std_logic_vector(15 downto 0);
   hr_readdatavalid_i   : in  std_logic;
   hr_waitrequest_i     : in  std_logic;
   hr_rst_i             : in  std_logic := '0'; -- the HyperRAM and SDRAM side's reset, with the core's

   -- SDRAM, clk100_i: the core shares it out (G_SDRAM)
   sr_write_o           : out std_logic;
   sr_read_o            : out std_logic;
   sr_address_o         : out std_logic_vector(31 downto 0);
   sr_writedata_o       : out std_logic_vector(15 downto 0);
   sr_byteenable_o      : out std_logic_vector(1 downto 0);
   sr_burstcount_o      : out std_logic_vector(7 downto 0);
   sr_readdata_i        : in  std_logic_vector(15 downto 0) := (others => '0');
   sr_readdatavalid_i   : in  std_logic := '0';
   sr_waitrequest_i     : in  std_logic := '1'
);
end entity rv_soc;

architecture synthesis of rv_soc is

   -- As QNICE: the core stays in reset until the firmware starts it; keyboard and
   -- joysticks on; ascal follows the core (see qnice.vhd)
   constant C_CSR_DEFAULT  : std_logic_vector(15 downto 0) := x"0839";
   constant C_CHARS_DX     : natural := G_VGA_DX / G_FONT_DX;
   constant C_CHARS_DY     : natural := G_VGA_DY / G_FONT_DY;

   constant C_ROM_WORDS    : natural := 2048;       -- 8 KB
   constant C_LINE_BYTES   : natural := 64;         -- cache line
   -- 2.7 s at 100 MHz: longer than a reset of the core, during which HyperRAM waits (a
   -- short press of the reset button lasts as long as it is held, up to 1.5 s)
   constant C_XBUS_TIMEOUT : natural := 2 ** 28;
   constant C_QBUS_TIMEOUT : natural := 50_000;     -- 1 ms at 50 MHz: a device that never ends its wait

   type t_rom is array (0 to C_ROM_WORDS - 1) of bit_vector(31 downto 0);

   impure function read_rom(name : string) return t_rom is
      file     f : text open read_mode is name;
      variable l : line;
      variable r : t_rom := (others => (others => '0'));
   begin
      for i in t_rom'range loop
         exit when endfile(f);
         readline(f, l);
         read(l, r(i));
      end loop;
      assert endfile(f) report "rv_soc: boot ROM image larger than the ROM: " & name severity failure;
      return r;
   end function read_rom;

   signal rom : t_rom := read_rom(G_BOOT_ROM);

   ------------------------------------------------------------------------------
   -- 100 MHz
   ------------------------------------------------------------------------------

   signal rst100_m   : std_logic := '1';
   signal rst100     : std_logic := '1';

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

   signal adr        : std_logic_vector(31 downto 0);
   signal wdat       : std_logic_vector(31 downto 0);
   signal sel        : std_logic_vector(3 downto 0);
   signal rom_sel    : std_logic;
   signal hr_sel     : std_logic;
   signal sr_sel     : std_logic;

   signal rom_ack    : std_logic := '0';
   signal rom_err    : std_logic := '0';
   signal rom_dat    : bit_vector(31 downto 0);
   signal hr_stb     : std_logic;
   signal hr_ack     : std_logic;
   signal hr_err     : std_logic;
   signal hr_dat     : std_logic_vector(31 downto 0);
   signal sr_stb     : std_logic;
   signal sr_ack     : std_logic;
   signal sr_err     : std_logic;
   signal sr_dat     : std_logic_vector(31 downto 0);

   -- Requests for the 50 MHz side, and its answers
   signal req_tog    : std_logic := '0';
   signal req_adr    : std_logic_vector(31 downto 0) := (others => '0');
   signal req_wdat   : std_logic_vector(31 downto 0) := (others => '0');
   signal req_sel    : std_logic_vector(3 downto 0) := (others => '0');
   signal req_we     : std_logic := '0';
   signal q_taken    : std_logic := '0';
   signal reg_taken  : std_logic := '0';
   signal slow_ack   : std_logic := '0';
   signal slow_err   : std_logic := '0';
   signal slow_dat   : std_logic_vector(31 downto 0) := (others => '0');

   ------------------------------------------------------------------------------
   -- 50 MHz
   ------------------------------------------------------------------------------

   signal rst        : std_logic;
   signal req_seen   : std_logic := '0';
   signal req_new    : std_logic;
   signal r_dev      : std_logic;                              -- the request is for the device bus,
   signal r_sd       : std_logic;                              -- the SD registers,
   signal r_reg      : std_logic;                              -- or the registers

   -- Device bus and SD registers, accessed as QNICE did
   type t_qstate is (Q_IDLE, Q_ACCESS, Q_GAP);
   signal q_state    : t_qstate := Q_IDLE;
   signal q_sd       : std_logic := '0';                       -- the SD registers, else the device bus
   signal q_we       : std_logic := '0';
   signal q_addr     : unsigned(27 downto 0) := (others => '0');
   signal q_wdata    : std_logic_vector(31 downto 0) := (others => '0');
   signal q_two      : std_logic := '0';                       -- a 32-bit access: two device words
   signal q_hi       : std_logic := '0';                       -- this access is the upper half
   signal q_rdata    : std_logic_vector(31 downto 0) := (others => '0');
   signal q_timer    : natural range 0 to C_QBUS_TIMEOUT := 0;
   signal q_tog      : std_logic := '0';                       -- toggles with each answer
   signal q_err      : std_logic := '0';
   signal q_dat      : std_logic_vector(31 downto 0) := (others => '0');
   signal q_wdata16  : std_logic_vector(15 downto 0);
   signal q_wait     : std_logic;
   signal q_rdata16  : std_logic_vector(15 downto 0);

   -- Registers
   signal reg_tog      : std_logic := '0';                     -- toggles with each answer
   signal reg_err      : std_logic := '0';
   signal reg_dat      : std_logic_vector(31 downto 0) := (others => '0');
   signal reg_csr      : std_logic_vector(15 downto 0) := C_CSR_DEFAULT;
   signal ascal_usage  : std_logic;

   -- SD card
   signal sd_en        : std_logic;
   signal sd_we        : std_logic;
   signal sd_data_out  : std_logic_vector(15 downto 0);
   signal sd_inuse_rd  : std_logic;
   signal sd_cd_int    : std_logic;
   signal sd_cd_ext    : std_logic;

begin

   rst <= not reset_n_i;

   p_rst100 : process (clk100_i)
   begin
      if rising_edge(clk100_i) then
         rst100_m <= rst;
         rst100   <= rst100_m;
      end if;
   end process p_rst100;

   -----------------------------------------------------------------------------
   -- CPU (100 MHz)
   -----------------------------------------------------------------------------

   i_cpu : entity neorv32.neorv32_top
      generic map (
         CLOCK_FREQUENCY   => 100_000_000,
         BOOT_MODE_SELECT  => 1,               -- start in the boot ROM
         BOOT_ADDR_CUSTOM  => x"00000000",
         RISCV_ISA_C       => true,
         RISCV_ISA_M       => true,
         RISCV_ISA_Zicntr  => true,
         CPU_RF_ARCH_SEL   => 2,               -- register file in flip-flops (the modem's choice: a RAM style took a RAMB36)
         DMEM_EN           => true,
         DMEM_SIZE         => 16 * 1024,
         ICACHE_EN         => true,
         ICACHE_NUM_BLOCKS => 128,             -- 8 KB
         DCACHE_EN         => false,
         CACHE_BLOCK_SIZE  => C_LINE_BYTES,
         CACHE_BURSTS_EN   => true,
         CACHE_UC_BASE     => x"C0000000",     -- the device bus and the registers
         XBUS_EN           => true,
         XBUS_TIMEOUT      => C_XBUS_TIMEOUT,
         IO_CLINT_EN       => true,
         IO_UART0_EN       => true,
         IO_UART0_RX_FIFO  => 16,
         IO_UART0_TX_FIFO  => 16
      )
      port map (
         clk_i       => clk100_i,
         rstn_i      => not rst100,
         xbus_adr_o  => xbus_adr,
         xbus_dat_o  => xbus_dat_o,
         xbus_cti_o  => xbus_cti,
         xbus_we_o   => xbus_we,
         xbus_sel_o  => xbus_sel,
         xbus_stb_o  => xbus_stb,
         xbus_dat_i  => xbus_dat_i,
         xbus_ack_i  => xbus_ack,
         xbus_err_i  => xbus_err,
         uart0_txd_o => uart_txd_o,
         uart0_rxd_i => uart_rxd_i
      ); -- i_cpu

   adr  <= std_logic_vector(xbus_adr);
   wdat <= std_logic_vector(xbus_dat_o);
   sel  <= std_logic_vector(xbus_sel);

   rom_sel <= '1' when adr(31 downto 28) = x"0" else '0';
   hr_sel  <= '1' when adr(31 downto 24) = x"40" else '0';
   sr_sel  <= '1' when G_SDRAM and adr(31 downto 26) = "010100" else '0';

   xbus_ack   <= rom_ack or hr_ack or sr_ack or slow_ack;
   xbus_err   <= rom_err or hr_err or sr_err or slow_err;
   xbus_dat_i <= std_ulogic_vector(to_stdlogicvector(rom_dat)) when rom_ack = '1' else
                 std_ulogic_vector(hr_dat)                     when hr_ack = '1' else
                 std_ulogic_vector(sr_dat)                     when sr_ack = '1' else
                 std_ulogic_vector(slow_dat);

   -----------------------------------------------------------------------------
   -- Boot ROM (100 MHz)
   -----------------------------------------------------------------------------

   p_rom : process (clk100_i)
   begin
      if rising_edge(clk100_i) then
         rom_ack <= '0';
         rom_err <= '0';
         if xbus_stb = '1' and rom_sel = '1' then
            rom_ack <= '1';
            rom_err <= xbus_we;
            rom_dat <= rom(to_integer(unsigned(adr(12 downto 2))));
         end if;
      end if;
   end process p_rom;

   -----------------------------------------------------------------------------
   -- HyperRAM (100 MHz)
   -----------------------------------------------------------------------------

   hr_stb <= xbus_stb and hr_sel;

   i_hr_bridge : entity work.rv_hr_bridge
      generic map (
         G_LINE_BYTES => C_LINE_BYTES
      )
      port map (
         clk_i               => clk100_i,
         rst_i               => rst100,
         hr_rst_i            => hr_rst_i,
         stb_i               => hr_stb,
         adr_i               => adr(23 downto 0),
         we_i                => xbus_we,
         sel_i               => sel,
         dat_i               => wdat,
         cti_i               => std_logic_vector(xbus_cti),
         dat_o               => hr_dat,
         ack_o               => hr_ack,
         err_o               => hr_err,
         avm_write_o         => hr_write_o,
         avm_read_o          => hr_read_o,
         avm_address_o       => hr_address_o,
         avm_writedata_o     => hr_writedata_o,
         avm_byteenable_o    => hr_byteenable_o,
         avm_burstcount_o    => hr_burstcount_o,
         avm_readdata_i      => hr_readdata_i,
         avm_readdatavalid_i => hr_readdatavalid_i,
         avm_waitrequest_i   => hr_waitrequest_i
      ); -- i_hr_bridge

   -----------------------------------------------------------------------------
   -- SDRAM (100 MHz): bursts of 2 words, a CPU word, so an access is never longer than
   -- the REU's, whose turns it shares (sdram_arbit.vhd)
   -----------------------------------------------------------------------------

   sr_stb <= xbus_stb and sr_sel;

   i_sr_bridge : entity work.rv_hr_bridge
      generic map (
         G_LINE_BYTES => C_LINE_BYTES,
         G_ADDR_BITS  => 26,
         G_BURST      => 2
      )
      port map (
         clk_i               => clk100_i,
         rst_i               => rst100,
         hr_rst_i            => hr_rst_i,
         stb_i               => sr_stb,
         adr_i               => adr(25 downto 0),
         we_i                => xbus_we,
         sel_i               => sel,
         dat_i               => wdat,
         cti_i               => std_logic_vector(xbus_cti),
         dat_o               => sr_dat,
         ack_o               => sr_ack,
         err_o               => sr_err,
         avm_write_o         => sr_write_o,
         avm_read_o          => sr_read_o,
         avm_address_o       => sr_address_o,
         avm_writedata_o     => sr_writedata_o,
         avm_byteenable_o    => sr_byteenable_o,
         avm_burstcount_o    => sr_burstcount_o,
         avm_readdata_i      => sr_readdata_i,
         avm_readdatavalid_i => sr_readdatavalid_i,
         avm_waitrequest_i   => sr_waitrequest_i
      ); -- i_sr_bridge

   -----------------------------------------------------------------------------
   -- Everything else goes to the 50 MHz side (100 MHz)
   -- The CPU waits for each answer, so there is one request at a time.
   -----------------------------------------------------------------------------

   p_slow100 : process (clk100_i)
   begin
      if rising_edge(clk100_i) then
         slow_ack <= '0';
         slow_err <= '0';

         if xbus_stb = '1' and rom_sel = '0' and hr_sel = '0' and sr_sel = '0' then
            req_adr  <= adr;
            req_wdat <= wdat;
            req_sel  <= sel;
            req_we   <= xbus_we;
            req_tog  <= not req_tog;
         end if;

         if q_tog /= q_taken then
            q_taken  <= q_tog;
            slow_ack <= '1';
            slow_err <= q_err;
            slow_dat <= q_dat;
         elsif reg_tog /= reg_taken then
            reg_taken <= reg_tog;
            slow_ack  <= '1';
            slow_err  <= reg_err;
            slow_dat  <= reg_dat;
         end if;

         -- An answer to a request from before the reset is dropped
         if rst100 = '1' then
            q_taken   <= q_tog;
            reg_taken <= reg_tog;
         end if;
      end if;
   end process p_slow100;

   -----------------------------------------------------------------------------
   -- The request, as the 50 MHz side sees it: new for one 50 MHz clock
   -----------------------------------------------------------------------------

   req_new <= req_tog xor req_seen;
   r_dev   <= '1' when req_adr(31 downto 29) = "110" else '0';
   r_sd    <= '1' when req_adr(31 downto 5) = x"E00001" & "000" else '0';
   r_reg   <= '1' when req_adr(31 downto 8) = x"E00000" else '0';

   p_req50 : process (clk50_i)
   begin
      if rising_edge(clk50_i) then
         req_seen <= req_tog;
      end if;
   end process p_req50;

   -----------------------------------------------------------------------------
   -- Device bus and SD registers (50 MHz)
   -- A 16-bit access is one device access, a 32-bit one two (low half first).
   -- Each access starts at a rising edge, the device acts on the falling edge, and
   -- the next rising edge without wait ends it. Between two accesses ce is low for
   -- at least one falling edge, so devices see each access as a new one.
   -----------------------------------------------------------------------------

   ramrom_ce_o   <= '1' when q_state = Q_ACCESS and q_sd = '0' else '0';
   ramrom_we_o   <= '1' when q_state = Q_ACCESS and q_sd = '0' and q_we = '1' else '0';
   ramrom_addr_o <= std_logic_vector(q_addr);
   ramrom_data_o <= q_wdata16;
   sd_en         <= '1' when q_state = Q_ACCESS and q_sd = '1' else '0';
   sd_we         <= '1' when q_state = Q_ACCESS and q_sd = '1' and q_we = '1' else '0';

   q_wdata16 <= q_wdata(31 downto 16) when q_hi = '1' else q_wdata(15 downto 0);
   q_wait    <= ramrom_wait_i when q_sd = '0' else '0';
   q_rdata16 <= sd_data_out when q_sd = '1' else ramrom_data_i;

   p_qbus : process (clk50_i)
      variable v_rdata : std_logic_vector(31 downto 0);
   begin
      if rising_edge(clk50_i) then
         case q_state is
            when Q_IDLE =>
               if req_new = '1' and (r_dev = '1' or r_sd = '1') then
                  q_we    <= req_we;
                  q_wdata <= req_wdat;
                  q_rdata <= (others => '0');
                  q_timer <= 0;
                  q_state <= Q_ACCESS;
                  q_two   <= '0';
                  q_hi    <= '0';
                  if r_sd = '1' then
                     q_sd   <= '1';
                     q_addr <= x"000000" & '0' & unsigned(req_adr(4 downto 2));
                     if req_sel /= "1111" then
                        q_err   <= '1';
                        q_dat   <= (others => '0');
                        q_tog   <= not q_tog;
                        q_state <= Q_IDLE;
                     end if;
                  else
                     q_sd   <= '0';
                     q_addr <= unsigned(req_adr(28 downto 1));
                     if req_sel = "1111" then
                        q_two <= '1';
                     elsif req_sel = "1100" then
                        q_hi  <= '1';
                     elsif req_sel /= "0011" then
                        q_err   <= '1';              -- bytes are not supported
                        q_dat   <= (others => '0');
                        q_tog   <= not q_tog;
                        q_state <= Q_IDLE;
                     end if;
                  end if;
               end if;

            when Q_ACCESS =>
               if q_wait = '0' then
                  v_rdata := q_rdata;
                  if q_hi = '1' then
                     v_rdata(31 downto 16) := q_rdata16;
                  else
                     v_rdata(15 downto 0)  := q_rdata16;
                  end if;
                  q_rdata <= v_rdata;
                  if q_two = '1' and q_hi = '0' then
                     q_hi    <= '1';
                     q_addr  <= q_addr + 1;
                     q_state <= Q_GAP;
                  else
                     q_err   <= '0';
                     q_dat   <= v_rdata;
                     q_tog   <= not q_tog;
                     q_state <= Q_IDLE;
                  end if;
               elsif q_timer = C_QBUS_TIMEOUT then
                  q_err   <= '1';
                  q_dat   <= (others => '0');
                  q_tog   <= not q_tog;
                  q_state <= Q_IDLE;
               else
                  q_timer <= q_timer + 1;
               end if;

            when Q_GAP =>
               q_timer <= 0;
               q_state <= Q_ACCESS;
         end case;

         if rst = '1' then
            q_state <= Q_IDLE;
         end if;
      end if;
   end process p_qbus;

   -----------------------------------------------------------------------------
   -- Registers at 0xE0000000, 32-bit accesses only (50 MHz)
   --   0x00 CSR (sysdef.asm M2M$CSR)    0x04 OSM x|y      0x08 OSM dx|dy
   --   0x0C ascal mode                  0x10 special (ro) 0x14 general (ro)
   --   0x18 keys, low active (ro)       0x1C RAMROM_DEV
   --   0x40-0x5C control_d, 32 bits per register (bit 0 of 0x40 is bit 0)
   --   0x60-0x7C control_m, ditto
   -- Any other address the CPU uses is a bus error.
   -----------------------------------------------------------------------------

   csr_reset_o    <= reg_csr(0);
   csr_pause_o    <= reg_csr(1);
   csr_osm_o      <= reg_csr(2);
   csr_keyboard_o <= reg_csr(3);
   csr_joy1_o     <= reg_csr(4);
   csr_joy2_o     <= reg_csr(5);
   ascal_usage    <= reg_csr(11);

   p_regs : process (clk50_i)
      variable v_idx : natural range 0 to 7;
      variable v_err : std_logic;
   begin
      if rising_edge(clk50_i) then
         v_idx := to_integer(unsigned(req_adr(4 downto 2)));

         if req_new = '1' and r_dev = '0' and r_sd = '0' then
            v_err   := '0';
            reg_dat <= (others => '0');
            if r_reg = '0' or req_sel /= "1111" then
               v_err := '1';
            elsif req_we = '1' then
               case req_adr(7 downto 2) is
                  when "000000" => reg_csr <= req_wdat(15 downto 0);
                  when "000001" => osm_xy_o <= req_wdat(15 downto 0);
                  when "000010" => osm_dxdy_o <= req_wdat(15 downto 0);
                  when "000011" =>
                     if ascal_usage = '0' then
                        ascal_mode_o <= req_wdat(4 downto 0);
                     end if;
                  when "000111" => ramrom_dev_o <= req_wdat(15 downto 0);
                  when "010000" | "010001" | "010010" | "010011" | "010100" | "010101" | "010110" | "010111" =>
                     control_d_o(32 * v_idx + 31 downto 32 * v_idx) <= req_wdat;
                  when "011000" | "011001" | "011010" | "011011" | "011100" | "011101" | "011110" | "011111" =>
                     control_m_o(32 * v_idx + 31 downto 32 * v_idx) <= req_wdat;
                  when others => v_err := '1';
               end case;
            else
               case req_adr(7 downto 2) is
                  when "000000" => reg_dat(15 downto 0) <= "0000" & reg_csr(11) & sd_cd_ext & sd_cd_int & sd_inuse_rd &
                                                           reg_csr(7 downto 0);
                  when "000001" => reg_dat(15 downto 0) <= osm_xy_o;
                  when "000010" => reg_dat(15 downto 0) <= osm_dxdy_o;
                  when "000011" => reg_dat(4 downto 0)  <= ascal_mode_o;
                  when "000100" => reg_dat(15 downto 0) <= special_i;
                  when "000101" => reg_dat(15 downto 0) <= general_i;
                  when "000110" => reg_dat(15 downto 0) <= keys_n_i;
                  when "000111" => reg_dat(15 downto 0) <= ramrom_dev_o;
                  when "010000" | "010001" | "010010" | "010011" | "010100" | "010101" | "010110" | "010111" =>
                     reg_dat <= control_d_o(32 * v_idx + 31 downto 32 * v_idx);
                  when "011000" | "011001" | "011010" | "011011" | "011100" | "011101" | "011110" | "011111" =>
                     reg_dat <= control_m_o(32 * v_idx + 31 downto 32 * v_idx);
                  when others => v_err := '1';
               end case;
            end if;
            reg_err <= v_err;
            reg_tog <= not reg_tog;
         end if;

         if ascal_usage = '1' then
            ascal_mode_o <= ascal_mode_i;
         end if;

         if rst = '1' then
            reg_csr      <= C_CSR_DEFAULT;
            ascal_mode_o <= "00000";
            osm_xy_o     <= x"0000";
            osm_dxdy_o   <= std_logic_vector(to_unsigned(C_CHARS_DX * 256 + C_CHARS_DY, 16));
            control_d_o  <= (others => '0');
            control_m_o  <= (others => '0');
            ramrom_dev_o <= x"0000";
         end if;
      end if;
   end process p_regs;

   -----------------------------------------------------------------------------
   -- SD cards: the controllers and the slot multiplexer, as in QNICE
   -----------------------------------------------------------------------------

   i_sdmux : entity work.sdmux
      port map (
         sysclk50Mhz_i     => clk50_i,
         clk100_i          => clk100_i,
         sysreset_i        => rst,
         mode_i            => reg_csr(6),
         active_o          => sd_inuse_rd,
         force_i           => reg_csr(7),
         detected_int_o    => sd_cd_int,
         detected_ext_o    => sd_cd_ext,
         en_i              => sd_en,
         we_i              => sd_we,
         reg_i             => std_logic_vector(q_addr(2 downto 0)),
         data_i            => q_wdata16,
         data_o            => sd_data_out,
         eng_i             => sd_eng_i,
         eng_o             => sd_eng_o,
         sd_tray_detect_i  => sd2_cd_i,
         sd_tray_clk_o     => sd2_clk_o,
         sd_tray_cmd_o     => sd2_cmd_o,
         sd_tray_cmd_oe_o  => sd2_cmd_oe_o,
         sd_tray_cmd_i     => sd2_cmd_i,
         sd_tray_dat_o     => sd2_dat_o,
         sd_tray_dat_oe_o  => sd2_dat_oe_o,
         sd_tray_dat_i     => sd2_dat_i,
         sd_back_detect_i  => sd_cd_i,
         sd_back_clk_o     => sd_clk_o,
         sd_back_cmd_o     => sd_cmd_o,
         sd_back_cmd_oe_o  => sd_cmd_oe_o,
         sd_back_cmd_i     => sd_cmd_i,
         sd_back_dat_o     => sd_dat_o,
         sd_back_dat_oe_o  => sd_dat_oe_o,
         sd_back_dat_i     => sd_dat_i
      ); -- i_sdmux

end architecture synthesis;
