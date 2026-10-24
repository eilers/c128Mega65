----------------------------------------------------------------------------------
-- MiSTer2MEGA65 Framework
--
-- Smart MEGA65 SD Card multiplexer
--
-- Activate the bottom tray's SD card, if there is no SD card in the slot on the
-- machine's back side. Otherwise the back side slot has precedence. It is
-- possible to overwrite the automatic behavior.
--
-- The smart multiplexer also makes sure that the QNICE SD card controller is
-- being reset as soon as the SD card is switched.
--
-- C128 core: one sdhost.vhd controller per slot, so that each one's pin registers
-- sit on its own pins (native 4-bit SD at up to 50 MHz). Only the active one runs;
-- QNICE's registers and the engine port go to it, the other is held in reset.
--
-- CAVEAT: RIGHT NOW WE CANNOT DETECT THE TRAY SD CARD on R3 machines. This
-- is a PCB bug and it has been fixed on R3A machines.
--
-- MiSTer2MEGA65 done by sy2002 and MJoergen in 2022 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.sdhost_pkg.all;

entity sdmux is
   port (
      -- QNICE system interface
      sysclk50Mhz_i     : in std_logic;   -- QNICE system clock
      clk100_i          : in std_logic;   -- the controllers' clock: 100 MHz, same MMCM, phase 0
      sysreset_i        : in std_logic;   -- QNICE system reset

      -- Configuration lines to control the behavior of the multiplexer
      mode_i            : in std_logic;   -- SD Card mode: 0=Auto: SD card switches between the internal card (bottom tray)
                                          -- and the external card (back slot) automatically: External has higher precedence
      active_o          : out std_logic;  -- Currently active SD card: 0=internal / 1=external
      force_i           : in std_logic;   -- if mode_i=1 then use this to force internal (0) or external (1)
      detected_int_o    : out std_logic;  -- 1=internal SD card detected
      detected_ext_o    : out std_logic;  -- 1=external SD card detected

      -- QNICE registers 0xFF20..0xFF26 of the active controller (see sdhost.vhd)
      en_i              : in  std_logic;
      we_i              : in  std_logic;
      reg_i             : in  std_logic_vector(2 downto 0);
      data_i            : in  std_logic_vector(15 downto 0);
      data_o            : out std_logic_vector(15 downto 0);

      -- engine port of the active controller (C2Q_SLOT is ignored)
      eng_i             : in  t_sd_c2q;
      eng_o             : out t_sd_q2c;

      -- interface to bottom tray's SD card
      sd_tray_detect_i  : in  std_logic;  -- low active
      sd_tray_clk_o     : out std_logic;
      sd_tray_cmd_o     : out std_logic;
      sd_tray_cmd_oe_o  : out std_logic;
      sd_tray_cmd_i     : in  std_logic;
      sd_tray_dat_o     : out std_logic_vector(3 downto 0);
      sd_tray_dat_oe_o  : out std_logic_vector(3 downto 0);
      sd_tray_dat_i     : in  std_logic_vector(3 downto 0);

      -- interface to the SD card in the back slot
      sd_back_detect_i  : in  std_logic;  -- low active
      sd_back_clk_o     : out std_logic;
      sd_back_cmd_o     : out std_logic;
      sd_back_cmd_oe_o  : out std_logic;
      sd_back_cmd_i     : in  std_logic;
      sd_back_dat_o     : out std_logic_vector(3 downto 0);
      sd_back_dat_oe_o  : out std_logic_vector(3 downto 0);
      sd_back_dat_i     : in  std_logic_vector(3 downto 0)
   );
end sdmux;

architecture beh of sdmux is

constant CPU_CLOCK_HZ       : integer := 50_000_000;
constant TIME_CTRL_RESET_MS : integer := 100;   -- SD card controller reset time in milliseconds

constant RESET_MAX_CNT      : integer := (CPU_CLOCK_HZ / (1000 / TIME_CTRL_RESET_MS)) - 1;

type tSDMux_States is ( s_reset,
                        s_select_card,
                        s_reset_controller,
                        s_idle
                      );

signal mux_state        : tSDMux_States;

signal active           : std_logic;         -- active SD card: 0=internal / 1=external
signal counter          : integer range 0 to RESET_MAX_CNT;
signal current_mode     : std_logic;

signal fsm_nextstate    : tSDMux_States;
signal fsm_active       : std_logic;
signal fsm_counter      : integer range 0 to RESET_MAX_CNT;
signal fsm_current_mode : std_logic;

-- controller 0 = bottom tray, 1 = back slot
type t_slv16 is array (0 to 1) of std_logic_vector(15 downto 0);
type t_slv4  is array (0 to 1) of std_logic_vector(3 downto 0);
type t_q2c   is array (0 to 1) of t_sd_q2c;

signal ctrl_rst         : std_logic;         -- the active controller is held in reset
signal rst              : std_logic_vector(1 downto 0);
signal en_c             : std_logic_vector(1 downto 0);
signal start_c          : std_logic_vector(1 downto 0);
signal data_c           : t_slv16;
signal q2c              : t_q2c;
signal clk_c            : std_logic_vector(1 downto 0);
signal cmd_c, cmd_oe_c  : std_logic_vector(1 downto 0);
signal cmd_in           : std_logic_vector(1 downto 0);
signal dat_c, dat_oe_c  : t_slv4;
signal dat_in           : t_slv4;

-- A running engine transfer stays with its controller until its done pulse, even if
-- the active card changes meanwhile (the reset of that controller ends it with an error).
signal eng_act          : std_logic := '0';
signal eng_slot         : std_logic := '0';
signal eng_cur          : std_logic;
signal eng_fail         : std_logic := '0';  -- a start that reached a controller in reset

-- Card generation (Q2C_CARD): counts every reset of the active controller and every
-- removal of the active card, so a user of the engine port notices a different card
constant C_ABSENT       : natural := 50_000; -- 1 ms without a card is a removal
signal card_gen         : unsigned(7 downto 0) := (others => '0');
signal ctrl_rst_d       : std_logic := '1';
signal cd_s1, cd_s2     : std_logic_vector(1 downto 0) := "00";   -- 1 = card present
signal absent_cnt       : natural range 0 to C_ABSENT := 0;

begin
   active_o          <= active;
   detected_int_o    <= not sd_tray_detect_i;
   detected_ext_o    <= not sd_back_detect_i;

   ctrl_rst <= '1' when sysreset_i = '1' or mux_state /= s_idle else '0';
   rst(0)   <= ctrl_rst or active;
   rst(1)   <= ctrl_rst or not active;

   -- QNICE registers: only the active controller is enabled; an idle one answers 0
   en_c(0) <= en_i when active = '0' else '0';
   en_c(1) <= en_i when active = '1' else '0';
   data_o  <= data_c(0) or data_c(1);

   -- engine port
   start_c(0) <= eng_i(C2Q_START) when active = '0' else '0';
   start_c(1) <= eng_i(C2Q_START) when active = '1' else '0';
   eng_cur    <= eng_slot when eng_act = '1' else active;

   eng_out : process(all)
   begin
      if eng_cur = '1' then
         eng_o <= q2c(1);
      else
         eng_o <= q2c(0);
      end if;
      -- sdhost drops a start that arrives while it is held in reset: answer it here
      if eng_fail = '1' then
         eng_o(Q2C_DONE)  <= '1';
         eng_o(Q2C_ERROR) <= '1';
      end if;
      eng_o(Q2C_CARD) <= std_logic_vector(card_gen);
   end process eng_out;

   card_gen_proc : process(sysclk50Mhz_i)
      variable present : std_logic;
   begin
      if rising_edge(sysclk50Mhz_i) then
         cd_s1      <= (not sd_back_detect_i) & (not sd_tray_detect_i);
         cd_s2      <= cd_s1;
         ctrl_rst_d <= ctrl_rst;

         present := cd_s2(0);
         if active = '1' then
            present := cd_s2(1);
         end if;
         if present = '1' or ctrl_rst = '1' then
            absent_cnt <= 0;
         elsif absent_cnt /= C_ABSENT then
            absent_cnt <= absent_cnt + 1;
         end if;

         if (ctrl_rst = '1' and ctrl_rst_d = '0') or absent_cnt = C_ABSENT - 1 then
            card_gen <= card_gen + 1;
         end if;
      end if;
   end process card_gen_proc;

   eng_latch : process(sysclk50Mhz_i)
   begin
      if rising_edge(sysclk50Mhz_i) then
         eng_fail <= '0';
         if eng_act = '0' then
            if eng_i(C2Q_START) = '1' then
               if ctrl_rst = '1' then
                  eng_fail <= '1';
               else
                  eng_act  <= '1';
                  eng_slot <= active;
               end if;
            end if;
         elsif (eng_slot = '0' and q2c(0)(Q2C_DONE) = '1') or
               (eng_slot = '1' and q2c(1)(Q2C_DONE) = '1') then
            eng_act <= '0';
         end if;
         if sysreset_i = '1' then
            eng_act <= '0';
         end if;
      end if;
   end process eng_latch;

   -- the pins
   sd_tray_clk_o    <= clk_c(0);
   sd_tray_cmd_o    <= cmd_c(0);
   sd_tray_cmd_oe_o <= cmd_oe_c(0);
   cmd_in(0)        <= sd_tray_cmd_i;
   sd_tray_dat_o    <= dat_c(0);
   sd_tray_dat_oe_o <= dat_oe_c(0);
   dat_in(0)        <= sd_tray_dat_i;
   sd_back_clk_o    <= clk_c(1);
   sd_back_cmd_o    <= cmd_c(1);
   sd_back_cmd_oe_o <= cmd_oe_c(1);
   cmd_in(1)        <= sd_back_cmd_i;
   sd_back_dat_o    <= dat_c(1);
   sd_back_dat_oe_o <= dat_oe_c(1);
   dat_in(1)        <= sd_back_dat_i;

   g_ctrl : for i in 0 to 1 generate
      q2c(i)(Q2C_CARD) <= (others => '0');      -- set in eng_out

      i_sdhost : entity work.sdhost
         port map (
            clk_i       => sysclk50Mhz_i,
            clk2x_i     => clk100_i,
            reset_i     => rst(i),
            en_i        => en_c(i),
            we_i        => we_i,
            reg_i       => reg_i,
            data_i      => data_i,
            data_o      => data_c(i),
            e_ready_o   => q2c(i)(Q2C_READY),
            e_start_i   => start_c(i),
            e_write_i   => eng_i(C2Q_WRITE),
            e_addr_i    => eng_i(C2Q_ADDR),
            e_count_i   => eng_i(C2Q_COUNT),
            e_limit_i   => eng_i(C2Q_LIMIT),
            e_abort_i   => eng_i(C2Q_ABORT),
            e_busy_o    => q2c(i)(Q2C_BUSY),
            e_done_o    => q2c(i)(Q2C_DONE),
            e_error_o   => q2c(i)(Q2C_ERROR),
            e_blk_o     => q2c(i)(Q2C_BLK),
            e_rd_we_o   => q2c(i)(Q2C_RDWE),
            e_rd_blk_o  => q2c(i)(Q2C_RDBLK),
            e_rd_idx_o  => q2c(i)(Q2C_RDIDX),
            e_rd_data_o => q2c(i)(Q2C_RDDATA),
            e_wr_blk_o  => q2c(i)(Q2C_WRBLK),
            e_wr_idx_o  => q2c(i)(Q2C_WRIDX),
            e_wr_data_i => eng_i(C2Q_WRDATA),
            sd_clk_o    => clk_c(i),
            cmd_o       => cmd_c(i),
            cmd_oe_o    => cmd_oe_c(i),
            cmd_i       => cmd_in(i),
            dat_o       => dat_c(i),
            dat_oe_o    => dat_oe_c(i),
            dat_i       => dat_in(i)
         );
   end generate g_ctrl;

   -- set the state machine's registers
   fsm_advance_state : process (sysclk50Mhz_i)
   begin
      if rising_edge(sysclk50Mhz_i) then
         if sysreset_i = '1' then
            mux_state      <= s_reset;
            active         <= '0';           -- default: internal card (bottom tray)
            counter        <= RESET_MAX_CNT;
            current_mode   <= '0';
         else
            mux_state      <= fsm_nextstate;
            active         <= fsm_active;
            counter        <= fsm_counter;
            current_mode   <= fsm_current_mode;
         end if;
      end if;
   end process;

   -- the actual state machine
   fsm_output_decode : process(all)
   begin
      fsm_nextstate     <= mux_state;
      fsm_active        <= active;
      fsm_counter       <= counter;
      fsm_current_mode  <= current_mode;

      case mux_state is
         when s_reset =>
            fsm_nextstate     <= s_select_card;

            fsm_active        <= '0';
            fsm_counter       <= RESET_MAX_CNT;
            fsm_current_mode  <= mode_i;

         when s_select_card =>
            fsm_nextstate  <= s_reset_controller;

            -- automatic mode
            if current_mode = '0' then
               -- if external card detected: take it
               -- CAVEAT / TODO: Due to a non working detection pin for the internal card, we cannot be smarter here
               fsm_active <= detected_ext_o;
            -- manual mode
            else
               fsm_active <= force_i; -- use the card specified by QNICE
            end if;

         when s_reset_controller =>
            fsm_nextstate  <= s_idle;
            if counter = 0 then
               fsm_nextstate <= s_idle;
            else
               fsm_counter <= counter - 1;
            end if;

         when s_idle =>
            -- if something has changed
            if (mode_i = '0' and active /= detected_ext_o) or
               (mode_i = '1' and active /= force_i) then
                  fsm_nextstate <= s_reset;
            end if;

         when others => null;
      end case;
   end process;

end beh;
