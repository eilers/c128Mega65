----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- cmdhd: a CMD HD on the IEC bus, with one SCSI disk (ID 0) whose image is a file on the
-- SD card. The controller board runs on the core clock (cmdhd_drive); the firmware answers
-- the disk's commands (VICE's scsi.c in C) through this device and moves blocks between the
-- card and the disk's buffer through the SD controller's engine port.
--
-- Device (C_DEV_CMDHD), word addresses:
--   0x0000-0x3FFF  the boot ROM, a byte per word (16 KB; the firmware loads it)
--   0x4000-0x47FF  the disk's buffer: 0x000-0x3FF data (two 512-byte halves), 0x400- the
--                  command bytes of the last command
--   window 0xFFFF  CSR:
--     0x000  R: magic 0x5D48
--     0x001  R: events, bit 0 a command waits, bit 1 a data phase ended, bit 2 bus free (or
--               a bus reset), bit 3 the RAM is clear; W: 1s clear them
--     0x002  R: bit 0 the drive runs, 1 BSY, 2 RAM clear busy, 3 activity LED, 4 error LED,
--               bits 11..8 the command's length
--     0x003  W: go, bits 1..0: 0 DATA IN, 1 DATA OUT, 2 STATUS (0x004-0x006 first)
--     0x004  W: bytes of the data phase (1-1024)
--     0x005  W: where in the buffer it starts (0-1023)
--     0x006  W: the status byte
--     0x008  W/R: bit 0 run (else held in reset), 1 WRITE PROTECT, 2 SWAP 8, 3 SWAP 9 pressed,
--               4 clear the RAM (power-up): set it while held in reset, wait for event 3
--     0x009  R: the drive CPU's program counter
--     0x010  W: SD block, low word   0x011  high word
--     0x012  W: buffer half (bit 0)
--     0x013  W: start a block transfer, bit 0 = 1 buffer to card, 0 card to buffer
--            R: bit 0 busy, bit 1 the last transfer failed
--     0x014  W: the card the image is on (Q2C_CARD at the mount); R: the card now
--
-- Clock crossings: the controls, the go toggle (its values are stable before it flips) and
-- the events go through ASYNC_REG pairs; the buffer and the ROM are dual-clock RAMs; the
-- status levels through ASYNC_REG pairs, the program counter when two samples agree.
--
-- Done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.sdhost_pkg.all;

entity cmdhd is
generic (
   G_CLK_HZ : natural := 31_528_000
);
port (
   -- core clock
   clk_main_i    : in  std_logic;
   rst_main_i    : in  std_logic;                    -- the C128's reset (a warm reset here)
   ce_i          : in  std_logic;                    -- 16 MHz
   pause_i       : in  std_logic;
   iec_atn_i     : in  std_logic;
   iec_clk_i     : in  std_logic;
   iec_data_i    : in  std_logic;
   iec_fclk_i    : in  std_logic;
   iec_clk_o     : out std_logic;
   iec_data_o    : out std_logic;
   iec_fclk_o    : out std_logic;
   rtc_i         : in  std_logic_vector(64 downto 0);
   running_o     : out std_logic;
   led_act_o     : out std_logic;
   led_err_o     : out std_logic;

   -- firmware clock
   clk_qnice_i   : in  std_logic;
   rst_qnice_i   : in  std_logic;
   qnice_addr_i  : in  std_logic_vector(27 downto 0);
   qnice_data_i  : in  std_logic_vector(15 downto 0);
   qnice_ce_i    : in  std_logic;
   qnice_we_i    : in  std_logic;
   qnice_data_o  : out std_logic_vector(15 downto 0);
   qnice_wait_o  : out std_logic;
   eng_want_o    : out std_logic;
   eng_o         : out t_sd_c2q;
   eng_i         : in  t_sd_q2c
);
end entity cmdhd;

architecture synthesis of cmdhd is

   attribute async_reg : string;

   ---------------------------------------------------------------------------------------
   -- core clock
   ---------------------------------------------------------------------------------------
   signal ctl_s1, ctl_s2  : std_logic_vector(4 downto 0) := (others => '0');
   attribute async_reg of ctl_s1 : signal is "true";
   attribute async_reg of ctl_s2 : signal is "true";

   signal drv_rst         : std_logic;
   signal clear_busy      : std_logic;
   signal m_rom_addr      : std_logic_vector(13 downto 0);
   signal m_rom_data      : std_logic_vector(7 downto 0);
   signal m_buf_addr      : std_logic_vector(10 downto 0);
   signal m_buf_we        : std_logic;
   signal m_buf_wdata     : std_logic_vector(7 downto 0);
   signal m_buf_rdata     : std_logic_vector(7 downto 0);
   signal cmd_tgl, done_tgl, free_tgl, clear_tgl : std_logic;
   signal cdb_len         : std_logic_vector(3 downto 0);
   signal scsi_busy       : std_logic;
   signal led_act, led_err : std_logic;
   signal pc              : std_logic_vector(15 downto 0);

   ---------------------------------------------------------------------------------------
   -- firmware clock
   ---------------------------------------------------------------------------------------
   signal ctl             : std_logic_vector(4 downto 0) := (others => '0');
   signal go_tgl          : std_logic := '0';
   signal go_phase        : std_logic_vector(1 downto 0) := "00";
   signal go_count        : std_logic_vector(10 downto 0) := (others => '0');
   signal go_base         : std_logic_vector(9 downto 0) := (others => '0');
   signal go_status       : std_logic_vector(7 downto 0) := (others => '0');

   signal ev_s1, ev_s2, ev_d : std_logic_vector(3 downto 0) := (others => '0');
   attribute async_reg of ev_s1 : signal is "true";
   attribute async_reg of ev_s2 : signal is "true";
   signal events          : std_logic_vector(3 downto 0) := (others => '0');

   signal st_s1, st_s2    : std_logic_vector(4 downto 0) := (others => '0');
   attribute async_reg of st_s1 : signal is "true";
   attribute async_reg of st_s2 : signal is "true";
   signal pc_s1, pc_s2, pc_q : std_logic_vector(15 downto 0) := (others => '0');
   attribute async_reg of pc_s1 : signal is "true";
   attribute async_reg of pc_s2 : signal is "true";

   signal q_rom_we        : std_logic;
   signal q_rom_data      : std_logic_vector(7 downto 0);
   signal q_buf_addr      : std_logic_vector(10 downto 0);
   signal q_buf_we        : std_logic;
   signal q_buf_wdata     : std_logic_vector(7 downto 0);
   signal q_buf_rdata     : std_logic_vector(7 downto 0);

   signal sd_blk          : unsigned(31 downto 0) := (others => '0');
   signal sd_half         : std_logic := '0';
   signal sd_card         : std_logic_vector(7 downto 0) := (others => '0');
   signal sd_wr           : std_logic := '0';
   signal sd_pend         : std_logic := '0';   -- a transfer to do
   signal sd_act          : std_logic := '0';   -- the engine has it
   signal sd_got          : std_logic := '0';   -- its block went through
   signal sd_err          : std_logic := '0';
   signal eng_start       : std_logic := '0';

   signal win_rom, win_buf, win_csr : std_logic;
   signal rd_held         : std_logic := '0';

begin

   ---------------------------------------------------------------------------------------
   -- core clock: the drive
   ---------------------------------------------------------------------------------------

   p_ctl_sync : process (clk_main_i)
   begin
      if rising_edge(clk_main_i) then
         ctl_s1 <= ctl;
         ctl_s2 <= ctl_s1;
      end if;
   end process p_ctl_sync;

   drv_rst   <= rst_main_i or not ctl_s2(0) or clear_busy;
   running_o <= not drv_rst;
   led_act_o <= led_act and not drv_rst;
   led_err_o <= led_err and not drv_rst;

   i_drive : entity work.cmdhd_drive
      generic map (
         G_CLK_HZ => G_CLK_HZ
      )
      port map (
         clk_i        => clk_main_i,
         rst_i        => drv_rst,
         ce_i         => ce_i,
         pause_i      => pause_i,
         iec_atn_i    => iec_atn_i,
         iec_clk_i    => iec_clk_i,
         iec_data_i   => iec_data_i,
         iec_fclk_i   => iec_fclk_i,
         iec_clk_o    => iec_clk_o,
         iec_data_o   => iec_data_o,
         iec_fclk_o   => iec_fclk_o,
         btn_wp_i     => ctl_s2(1),
         btn_swap8_i  => ctl_s2(2),
         btn_swap9_i  => ctl_s2(3),
         led_act_o    => led_act,
         led_err_o    => led_err,
         rtc_i        => rtc_i,
         rom_addr_o   => m_rom_addr,
         rom_data_i   => m_rom_data,
         clear_i      => ctl_s2(4),
         clear_busy_o => clear_busy,
         clear_tgl_o  => clear_tgl,
         buf_addr_o   => m_buf_addr,
         buf_we_o     => m_buf_we,
         buf_wdata_o  => m_buf_wdata,
         buf_rdata_i  => m_buf_rdata,
         cmd_tgl_o    => cmd_tgl,
         cdb_len_o    => cdb_len,
         done_tgl_o   => done_tgl,
         free_tgl_o   => free_tgl,
         scsi_busy_o  => scsi_busy,
         go_tgl_i     => go_tgl,
         go_phase_i   => go_phase,
         go_count_i   => go_count,
         go_base_i    => go_base,
         go_status_i  => go_status,
         pc_o         => pc
      );

   ---------------------------------------------------------------------------------------
   -- the two dual-clock RAMs: port A the drive, port B the firmware and the engine
   ---------------------------------------------------------------------------------------

   i_rom : entity work.tdp_ram
      generic map (
         ADDR_WIDTH => 14,
         DATA_WIDTH => 8
      )
      port map (
         clock_a   => clk_main_i,
         address_a => m_rom_addr,
         data_a    => x"00",
         wren_a    => '0',
         q_a       => m_rom_data,
         clock_b   => clk_qnice_i,
         address_b => qnice_addr_i(13 downto 0),
         data_b    => qnice_data_i(7 downto 0),
         wren_b    => q_rom_we,
         q_b       => q_rom_data
      );

   i_buf : entity work.tdp_ram
      generic map (
         ADDR_WIDTH => 11,
         DATA_WIDTH => 8
      )
      port map (
         clock_a   => clk_main_i,
         address_a => m_buf_addr,
         data_a    => m_buf_wdata,
         wren_a    => m_buf_we,
         q_a       => m_buf_rdata,
         clock_b   => clk_qnice_i,
         address_b => q_buf_addr,
         data_b    => q_buf_wdata,
         wren_b    => q_buf_we,
         q_b       => q_buf_rdata
      );

   ---------------------------------------------------------------------------------------
   -- firmware clock: the device
   ---------------------------------------------------------------------------------------

   win_rom <= '1' when qnice_addr_i(27 downto 14) = "00000000000000" else '0';
   win_buf <= '1' when qnice_addr_i(27 downto 11) = "00000000000001000" else '0';   -- 0x4000-0x47FF
   win_csr <= '1' when qnice_addr_i(27 downto 12) = x"FFFF" else '0';

   q_rom_we <= qnice_ce_i and qnice_we_i and win_rom;

   -- A read of the ROM or the buffer waits a clock: their RAMs answer at the next rising
   -- edge, and the bus takes the data at the first one without wait
   p_wait : process (clk_qnice_i)
   begin
      if rising_edge(clk_qnice_i) then
         rd_held <= qnice_ce_i and not qnice_we_i;
      end if;
   end process p_wait;
   qnice_wait_o <= qnice_ce_i and not qnice_we_i and (win_rom or win_buf) and not rd_held;

   -- port B of the buffer: the engine while it moves a block, else the firmware
   p_qbuf : process (all)
   begin
      q_buf_addr  <= qnice_addr_i(10 downto 0);
      q_buf_wdata <= qnice_data_i(7 downto 0);
      q_buf_we    <= qnice_ce_i and qnice_we_i and win_buf;
      if sd_act = '1' then
         q_buf_we <= '0';
         if sd_wr = '1' then
            q_buf_addr <= '0' & sd_half & eng_i(Q2C_WRIDX);
         else
            q_buf_addr  <= '0' & sd_half & eng_i(Q2C_RDIDX);
            q_buf_wdata <= eng_i(Q2C_RDDATA);
            q_buf_we    <= eng_i(Q2C_RDWE);
         end if;
      end if;
   end process p_qbuf;

   p_eng_o : process (all)
   begin
      eng_o             <= (others => '0');
      eng_o(C2Q_START)  <= eng_start;
      eng_o(C2Q_WRITE)  <= sd_wr;
      eng_o(C2Q_ADDR)   <= std_logic_vector(sd_blk);
      eng_o(C2Q_COUNT)  <= x"0001";
      eng_o(C2Q_LIMIT)  <= x"FFFF";
      eng_o(C2Q_WRDATA) <= q_buf_rdata;
   end process p_eng_o;

   eng_want_o <= sd_pend;

   p_read : process (all)
   begin
      qnice_data_o <= (others => '0');
      if win_rom = '1' then
         qnice_data_o <= x"00" & q_rom_data;
      elsif win_buf = '1' then
         qnice_data_o <= x"00" & q_buf_rdata;
      elsif win_csr = '1' then
         case qnice_addr_i(11 downto 0) is
            when x"000" => qnice_data_o <= x"5D48";
            when x"001" => qnice_data_o <= x"000" & events;
            when x"002" => qnice_data_o <= x"0" & cdb_len & "000" & st_s2;
            when x"008" => qnice_data_o <= x"00" & "000" & ctl;
            when x"009" => qnice_data_o <= pc_q;
            when x"013" => qnice_data_o <= x"000" & "00" & sd_err & sd_pend;
            when x"014" => qnice_data_o <= x"00" & eng_i(Q2C_CARD);
            when others => null;
         end case;
      end if;
   end process p_read;

   p_csr : process (clk_qnice_i)
      variable ev_clr : std_logic_vector(3 downto 0);
   begin
      if rising_edge(clk_qnice_i) then
         eng_start <= '0';

         -- events from the drive; one that comes with the firmware's clear is kept
         ev_clr := (others => '0');
         if qnice_ce_i = '1' and qnice_we_i = '1' and win_csr = '1' and
            qnice_addr_i(11 downto 0) = x"001" then
            ev_clr := qnice_data_i(3 downto 0);
         end if;
         ev_s1  <= clear_tgl & free_tgl & done_tgl & cmd_tgl;
         ev_s2  <= ev_s1;
         ev_d   <= ev_s2;
         events <= (events and not ev_clr) or (ev_s2 xor ev_d);

         -- status from the drive
         st_s1 <= led_err & led_act & clear_busy & scsi_busy & not drv_rst;
         st_s2 <= st_s1;
         pc_s1 <= pc;
         pc_s2 <= pc_s1;
         if pc_s2 = pc_s1 then
            pc_q <= pc_s2;
         end if;

         if qnice_ce_i = '1' and qnice_we_i = '1' and win_csr = '1' then
            case qnice_addr_i(11 downto 0) is
               when x"003" =>
                  go_phase <= qnice_data_i(1 downto 0);
                  go_tgl   <= not go_tgl;
               when x"004" => go_count  <= qnice_data_i(10 downto 0);
               when x"005" => go_base   <= qnice_data_i(9 downto 0);
               when x"006" => go_status <= qnice_data_i(7 downto 0);
               when x"008" => ctl       <= qnice_data_i(4 downto 0);
               when x"010" => sd_blk(15 downto 0)  <= unsigned(qnice_data_i);
               when x"011" => sd_blk(31 downto 16) <= unsigned(qnice_data_i);
               when x"012" => sd_half <= qnice_data_i(0);
               when x"013" =>
                  sd_wr   <= qnice_data_i(0);
                  sd_pend <= '1';
                  sd_got  <= '0';
                  sd_err  <= '0';
               when x"014" => sd_card <= qnice_data_i(7 downto 0);
               when others => null;
            end case;
         end if;

         -- the engine: one block; a firmware SD command may end it early, then again
         if sd_act = '1' then
            if eng_i(Q2C_BLK) = '1' then
               sd_got <= '1';
            end if;
            if eng_i(Q2C_DONE) = '1' then
               sd_act <= '0';
               if eng_i(Q2C_ERROR) = '1' then
                  sd_err  <= '1';
                  sd_pend <= '0';
               elsif sd_got = '1' or eng_i(Q2C_BLK) = '1' then
                  sd_pend <= '0';
               end if;
            end if;
         elsif sd_pend = '1' and eng_start = '0' then
            if eng_i(Q2C_CARD) /= sd_card then
               sd_err  <= '1';                       -- another card: never touch it
               sd_pend <= '0';
            elsif eng_i(Q2C_BUSY) = '0' then
               eng_start <= '1';
               sd_act    <= '1';
            end if;
         end if;

         if rst_qnice_i = '1' then
            ctl     <= (others => '0');
            events  <= (others => '0');
            sd_pend <= '0';
            sd_act  <= '0';
            sd_err  <= '0';
         end if;
      end if;
   end process p_csr;

end architecture synthesis;
