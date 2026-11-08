----------------------------------------------------------------------------------
-- MiSTer2MEGA65 Framework - Megamiga fork
--
-- sdhost: SD card host controller, a drop-in replacement for QNICE's `sdcard`
-- (M2M-UPSTREAM sd-host, exception 18 in the Megamiga AGENTS.md).
--
-- C128 core: taken from github.com/johansmolinski/Megamiga at 33df1e8. Our changes:
-- the ctrl process is sensitive to clk2x_i, the clock it runs on; Q2C_CARD.
--
-- Why: QNICE's controller (SimpleSDHC) runs the card in SPI mode at 12.5 MHz and
-- moves one block per command into a buffer that the CPU copies byte by byte.
-- That caps everything behind it at a few hundred KB/s. This controller
--
--   * first tries the card's native SD bus, 4 data lines at 25 MHz (12.5 MB/s raw),
--     and then switches the card to high speed mode, 50 MHz (25 MB/s raw; Megamiga
--     0.7, see "High speed" below),
--   * falls back to SPI mode at 25 MHz (CRC on, 3.1 MB/s raw) when the card does not
--     answer in SD mode (a card that was used in SPI mode since power-up - for example
--     by the MEGA65 boot ROM - stays in SPI mode until it is powered off), when the
--     test read after the native init fails, or when a native transfer fails,
--   * drops to 12.5 MHz in SPI mode when a transfer at 25 MHz fails,
--   * keeps QNICE's register interface (below), so the monitor's sd_library and the
--     FAT32 code run unchanged, and
--   * has a second port, the ENGINE PORT, through which a core streams consecutive
--     blocks (CMD18/CMD25) straight from/to its own buffers. QNICE commands have
--     priority: an engine transfer ends at the next block boundary when QNICE issues
--     a command, and the engine issues the rest again (it counts e_blk_o pulses).
--
-- QNICE registers (as sdcard.vhd):
--   0 : low word of the 32-bit block address
--   1 : high word of the 32-bit block address
--   2 : cursor into the 512-byte buffer
--   3 : read/write 1 byte of the buffer
--   4 : error code of the last operation (read only)
--   5 : command and status: write 0 = reset (re-initialise the card), 1 = read the
--       block into the buffer, 2 = write the buffer to the block; read: bit 15 busy,
--       bit 14 error (sticky until a reset), bits 13..12 card type (00 none/unknown,
--       01 SD V1, 10 SD V2, 11 SDHC)
--   6 : (new) read: bit 0 native SD bus, bit 1 SPI at 12.5 MHz, bit 2 ready, bit 3
--       SPI only, bit 4 high speed (50 MHz), bits 6..5 its sample point, bits 15..8
--       the number of transfers that were retried (mode fallbacks); write: bit 0 = SPI
--       only (the native bus is not tried at the next init: write it, then the reset
--       command 0x0000 to the CSR)
-- 512-byte block addressing on all card types.
--
-- Pins (before the slot multiplexer): sd_clk_o, CMD (cmd_*), DAT3..DAT0 (dat_*), each
-- with a separate output, output enable and input. In SPI mode CMD is MOSI (always
-- driven), DAT0 is MISO, DAT3 is chip select (always driven).
--
-- Clocks (Megamiga 0.7): the QNICE registers, the buffer and the engine port are in the
-- QNICE clock domain (clk_i, 50 MHz) as before; the controller itself (process ctrl)
-- runs on clk2x_i, 100 MHz from the same MMCM with phase 0 (synchronous: every clk_i
-- edge is a clk2x_i edge), so that it can make a 50 MHz SD clock. Between the domains:
-- what QNICE reads is copied into clk_i registers first; the controller's writes into
-- the buffer and its engine pulses last two clk2x_i clocks (one clk_i edge takes each);
-- the engine outputs are clk_i registers; the engine's write data is taken into a clk_i
-- register (see N_WR_GAP for what that means for the address).
--
-- Bit timing (default speed): the controller changes its outputs together with the
-- falling edge of SD_CLK and samples the inputs just before the NEXT falling edge, i.e.
-- one full SD clock after the card launched them (the card changes its outputs after
-- a falling edge and holds them until the next one). At 25 MHz that leaves ~25 ns
-- for the card's output delay (14 ns max) plus the board. Consequence for the state
-- machine: at a "step" (the clock after a falling edge) it sees the input bit of the
-- PREVIOUS SD cycle and decides the output bit of the NEXT one. The clock stops HIGH
-- whenever the controller pauses (flow control between blocks, idle).
--
-- High speed (Megamiga 0.7): after the native init and its test read at 25 MHz, CMD6
-- switches the card to high speed (its 64-byte status block says whether it did) and the
-- clock goes to 50 MHz. A card in high speed mode changes its outputs after the RISING
-- edge, up to 14 ns later, and holds them at least 2.5 ns after the next rising edge;
-- with the MEGA65's pads and traces on top that window is not known in advance. So the
-- input is sampled at one of four points, 10, 20, 25 or 30 ns after the controller's
-- clock edge that makes SD_CLK rise (sample point 0..3; point 0 is the default speed
-- sample; points 1..3 see each bit one step later, which the state machine does not
-- notice: every receiver looks for a
-- start bit, and the delayed samples are taken at the clock after each step, so a
-- paused clock never skips a bit). A calibration reads block 0 four times at every
-- point and keeps the middle of the points that passed (C_SAMP_PICK); none passed, or
-- the card did not switch: a new init at 25 MHz. A transfer that fails at 50 MHz also
-- goes back to 25 MHz (before the SPI fallbacks below).
--
-- Done by the Megamiga fork in 2026 and licensed under GPL v3.
----------------------------------------------------------------------------------

-- The engine port packed into two vectors, so that it can be threaded through
-- qnice -> qnice_wrapper -> framework -> board top -> core with two ports
-- (M2M-UPSTREAM sd-host). to the controller: C2Q, from the controller: Q2C.
library ieee;
use ieee.std_logic_1164.all;

package sdhost_pkg is
   subtype t_sd_c2q is std_logic_vector(75 downto 0);
   subtype t_sd_q2c is std_logic_vector(71 downto 0);
   -- C2Q
   constant C2Q_START   : natural := 0;
   constant C2Q_WRITE   : natural := 1;
   constant C2Q_ABORT   : natural := 2;
   subtype  C2Q_ADDR    is natural range 34 downto 3;
   subtype  C2Q_COUNT   is natural range 50 downto 35;
   subtype  C2Q_LIMIT   is natural range 66 downto 51;
   subtype  C2Q_WRDATA  is natural range 74 downto 67;
   constant C2Q_SLOT    : natural := 75;   -- two slots: 0 = bottom tray, 1 = back slot (qnice.vhd)
   -- Q2C
   constant Q2C_READY   : natural := 0;
   constant Q2C_BUSY    : natural := 1;
   constant Q2C_DONE    : natural := 2;
   constant Q2C_ERROR   : natural := 3;
   constant Q2C_BLK     : natural := 4;
   constant Q2C_RDWE    : natural := 5;
   subtype  Q2C_RDBLK   is natural range 21 downto 6;
   subtype  Q2C_RDIDX   is natural range 30 downto 22;
   subtype  Q2C_RDDATA  is natural range 38 downto 31;
   subtype  Q2C_WRBLK   is natural range 54 downto 39;
   subtype  Q2C_WRIDX   is natural range 63 downto 55;
   -- C128 core (sdmux.vhd): changes whenever the active card may have changed, so that
   -- block addresses taken from one card are never used on another
   subtype  Q2C_CARD    is natural range 71 downto 64;
end package sdhost_pkg;

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity sdhost is
   generic (
      G_NATIVE    : boolean := true;     -- try the native SD bus first
      G_HS        : boolean := true;     -- then high speed (50 MHz)
      -- half SD clock periods in clk2x_i clocks (Megamiga 0.7: all doubled)
      G_HALF_INIT : natural := 126;      -- 100 MHz / (2 x 126) = 397 kHz during the init
      G_HALF_FAST : natural := 2;        -- 25 MHz
      G_HALF_SLOW : natural := 4;        -- 12.5 MHz (SPI fallback after errors)
      G_HALF_HS   : natural := 1;        -- 50 MHz (high speed)
      G_MS        : natural := 100_000   -- clk2x_i clocks per millisecond
   );
   port (
      clk_i       : in  std_logic;       -- QNICE clock (50 MHz)
      clk2x_i     : in  std_logic;       -- the controller's clock (100 MHz, from clk_i's MMCM, phase 0)
      reset_i     : in  std_logic;       -- synchronous, high active: re-initialise

      -- QNICE registers (same timing as sdcard.vhd: writes and the buffer read on the
      -- falling edge, data_o combinational and 0 when not selected)
      en_i        : in  std_logic;
      we_i        : in  std_logic;
      reg_i       : in  std_logic_vector(2 downto 0);
      data_i      : in  std_logic_vector(15 downto 0);
      data_o      : out std_logic_vector(15 downto 0);

      -- engine port (clk_i, rising edge; all outputs are clk_i registers)
      e_ready_o   : out std_logic;                       -- card initialised
      e_start_i   : in  std_logic;                       -- pulse, only while e_busy_o = '0'
      e_write_i   : in  std_logic;
      e_addr_i    : in  std_logic_vector(31 downto 0);   -- first block
      e_count_i   : in  std_logic_vector(15 downto 0);   -- blocks, >= 1
      e_limit_i   : in  std_logic_vector(15 downto 0);   -- blocks < limit may be moved now
      e_abort_i   : in  std_logic;                       -- end at the next block boundary
      e_busy_o    : out std_logic;                       -- from the start pulse to e_done_o
      e_done_o    : out std_logic;                       -- pulse: transfer over
      e_error_o   : out std_logic;                       -- valid with e_done_o
      e_blk_o     : out std_logic;                       -- pulse: one more block is done
      -- read data: one strobe per byte (block index relative to e_addr_i)
      e_rd_we_o   : out std_logic;
      e_rd_blk_o  : out std_logic_vector(15 downto 0);
      e_rd_idx_o  : out std_logic_vector(8 downto 0);
      e_rd_data_o : out std_logic_vector(7 downto 0);
      -- write data: e_wr_data_i is the byte at (e_wr_blk_o, e_wr_idx_o) one clk_i clock
      -- after the address changed (a block RAM: address in, data out; exactly one clock -
      -- at 50 MHz the controller relies on it, see N_WR_GAP)
      e_wr_blk_o  : out std_logic_vector(15 downto 0);
      e_wr_idx_o  : out std_logic_vector(8 downto 0);
      e_wr_data_i : in  std_logic_vector(7 downto 0);

      -- card
      sd_clk_o    : out std_logic;
      cmd_o       : out std_logic;
      cmd_oe_o    : out std_logic;
      cmd_i       : in  std_logic;
      dat_o       : out std_logic_vector(3 downto 0);
      dat_oe_o    : out std_logic_vector(3 downto 0);
      dat_i       : in  std_logic_vector(3 downto 0)
   );
end entity sdhost;

architecture synthesis of sdhost is

   ---------------------------------------------------------------------------------------
   -- CRC helpers (serial, MSB first)
   ---------------------------------------------------------------------------------------

   function crc7_bit(c : std_logic_vector(6 downto 0); b : std_logic) return std_logic_vector is
      variable r : std_logic_vector(6 downto 0);
   begin
      r := c(5 downto 0) & '0';
      if (b xor c(6)) = '1' then
         r := r xor "0001001";
      end if;
      return r;
   end function;

   function crc16_bit(c : std_logic_vector(15 downto 0); b : std_logic) return std_logic_vector is
      variable r : std_logic_vector(15 downto 0);
   begin
      r := c(14 downto 0) & '0';
      if (b xor c(15)) = '1' then
         r := r xor x"1021";
      end if;
      return r;
   end function;

   function crc16_byte(c : std_logic_vector(15 downto 0); b : std_logic_vector(7 downto 0)) return std_logic_vector is
      variable r : std_logic_vector(15 downto 0) := c;
   begin
      for i in 7 downto 0 loop
         r := crc16_bit(r, b(i));
      end loop;
      return r;
   end function;

   -- the 48-bit command frame with its CRC7 and end bit
   function cmd_frame(idx : natural; arg : std_logic_vector(31 downto 0)) return std_logic_vector is
      variable f : std_logic_vector(47 downto 0);
      variable c : std_logic_vector(6 downto 0) := (others => '0');
   begin
      f(47 downto 46) := "01";
      f(45 downto 40) := std_logic_vector(to_unsigned(idx, 6));
      f(39 downto 8)  := arg;
      for i in 47 downto 8 loop
         c := crc7_bit(c, f(i));
      end loop;
      f(7 downto 1) := c;
      f(0)          := '1';
      return f;
   end function;

   ---------------------------------------------------------------------------------------
   -- constants
   ---------------------------------------------------------------------------------------

   type t_resp is (R_NONE, R_R1, R_R1B, R_R2, R_R3, R_R7);

   -- error codes (register 4)
   constant C_ERR_INIT    : std_logic_vector(15 downto 0) := x"EE31";
   constant C_ERR_DATA    : std_logic_vector(15 downto 0) := x"EE33";
   constant C_ERR_NOCARD  : std_logic_vector(15 downto 0) := x"EE34";

   -- card status bits that make an R1 (native) a failure
   constant C_R1_ERRMASK  : std_logic_vector(31 downto 0) := x"FDF80000";

   constant C_TMO_INIT    : natural := 1000 * G_MS;   -- ACMD41 loop
   constant C_TMO_READ    : natural := 250 * G_MS;    -- data start / token
   constant C_TMO_CAL     : natural := 20 * G_MS;     -- data start in the calibration
   constant C_CAL_READS   : natural := 4;             -- test reads per sample point
   constant C_CAL_DRAIN   : natural := 2 * G_MS;      -- clock after a failed test read (and
                                                       -- at least 2047 SD clocks: a block)

   -- the sample point from the points that passed the calibration (bit n = point n):
   -- the middle of the passing points; next to a failing point, the one farther away
   type t_pick is array (0 to 15) of natural range 0 to 3;
   constant C_SAMP_PICK   : t_pick := (
      0,          -- 0000: none (not used: back to 25 MHz)
      0,          -- 0001
      1,          -- 0010
      0,          -- 0011: point 2 fails, so 0
      2,          -- 0100
      0,          -- 0101 (not contiguous: the first)
      1,          -- 0110 (both neighbours fail)
      1,          -- 0111
      3,          -- 1000
      0,          -- 1001 (not contiguous: the first)
      1,          -- 1010 (not contiguous: the first)
      0,          -- 1011 (not contiguous: the run 0..1, point 2 fails)
      3,          -- 1100: point 1 fails, so 3
      3,          -- 1101 (not contiguous: the run 2..3, point 1 fails)
      2,          -- 1110
      1           -- 1111
   );
   constant C_TMO_BUSY    : natural := 600 * G_MS;    -- programming busy
   constant C_NCR_MAX     : natural := 255;           -- SD clocks until a response starts

   ---------------------------------------------------------------------------------------
   -- state
   ---------------------------------------------------------------------------------------

   type t_state is (
      S_RESET, S_INIT, S_IDLE, S_DISPATCH, S_FAIL, S_TAIL,
      -- native SD
      N_PRE, N_INIT, N_CMD_TX, N_CMD_RX, N_BUSY0, N_BUSY1,
      N_RD_CMD, N_RD_RESP, N_RD_START, N_RD_DATA, N_RD_CRC, N_RD_NEXT, N_STOP, N_STOP_DONE,
      N_WR_CMD, N_WR_RESP, N_WR_GAP, N_WR_DATA, N_WR_CRC, N_WR_END, N_WR_STAT,
      N_WR_STAT_BITS, N_WR_NEXT,
      -- SPI
      P_PRE, P_INIT, P_CMD, P_RESP, P_RESP_X, P_BUSY, P_DESEL,
      P_RD_CMD, P_RD_RESP, P_RD_TOKEN, P_RD_DATA, P_RD_NEXT, P_RD_END, P_RD_END2,
      P_WR_CMD, P_WR_RESP, P_WR_TOKEN, P_WR_DATA, P_WR_DRESP, P_WR_NEXT, P_WR_END,
      P_FIN, P_FIN2
   );
   signal st, ret_st : t_state := S_RESET;

   -- SD clock
   signal half      : natural range 1 to 127 := G_HALF_INIT;
   signal half_cnt  : natural range 0 to 127 := 0;
   signal clk_r     : std_logic := '1';
   signal clk_q     : std_logic := '1';   -- clk_r's copy for the pin (an IOB register)
   signal run       : std_logic := '0';
   signal tick      : std_logic := '0';   -- the clock after a falling edge: a "step"
   signal ph        : unsigned(2 downto 0) := "111";   -- SPI: bit phase of the cycle
   signal post      : std_logic := '0';   -- the clock after a step

   -- pins
   signal cmd_r, cmd_oe_r, cmd_nx, cmd_oe_nx : std_logic := '1';
   signal dat_r, dat_nx                      : std_logic_vector(3 downto 0) := "1111";
   signal dat_oe_r, dat_oe_nx                : std_logic_vector(3 downto 0) := "1000";

   -- Megamiga 0.7 (50 MHz): the pin registers in the I/O blocks, so that SD_CLK and the
   -- lines leave with the same short delay (the card's setup/hold at 50 MHz are 6/2 ns of
   -- a 10/10 ns budget) and the input sample (cmd_ri, dat_ri) is taken at the pad
   attribute IOB : string;
   attribute IOB of clk_q, cmd_r, cmd_oe_r, dat_r, dat_oe_r : signal is "TRUE";
   -- inputs: what a step sees (cmd_in/dat_in), from the samples (see "High speed")
   signal cmd_in : std_logic;
   signal dat_in : std_logic_vector(3 downto 0);
   signal cmd_ri, cmd_fi, cmd_l1, cmd_l2, cmd_l3 : std_logic := '1';     -- rising, falling, lagged
   signal dat_ri, dat_fi, dat_l1, dat_l2, dat_l3 : std_logic_vector(3 downto 0) := "1111";
   attribute IOB of cmd_ri, dat_ri : signal is "TRUE";
   signal spi_only  : std_logic := '0';     -- register 6 bit 0: never try the native bus

   -- mode
   signal native    : std_logic := '0';
   signal slow      : std_logic := '0';
   signal hc        : std_logic := '0';
   signal v2        : std_logic := '0';
   signal ctype     : std_logic_vector(1 downto 0) := "00";
   signal ready     : std_logic := '0';
   signal rca       : std_logic_vector(15 downto 0) := (others => '0');
   signal native_ok : std_logic := '1';     -- native allowed in this init
   signal retrying  : std_logic := '0';     -- the init is part of a retry
   signal retries   : unsigned(7 downto 0) := (others => '0');
   signal hs        : std_logic := '0';     -- high speed (50 MHz)
   signal hs_ok     : std_logic := '1';     -- high speed allowed in this init
   signal samp      : unsigned(1 downto 0) := "00";       -- the sample point
   signal cal_pass  : std_logic_vector(3 downto 0) := (others => '0');
   signal cal_n     : natural range 0 to C_CAL_READS := 0;
   signal sw_res    : std_logic_vector(3 downto 0) := (others => '0');   -- CMD6: group 1

   -- generic counters
   signal cnt       : natural range 0 to 2047 := 0;
   signal skip      : natural range 0 to 7 := 0;
   signal istep     : natural range 0 to 31 := 0;
   signal tries     : natural range 0 to 15 := 0;
   signal tmo       : natural range 0 to C_TMO_INIT := 0;

   -- command
   signal c_frame   : std_logic_vector(47 downto 0) := (others => '1');
   signal c_resp    : t_resp := R_NONE;
   signal c_idx     : natural range 0 to 63 := 0;
   signal c_started : std_logic := '0';
   signal c_ok      : std_logic := '0';     -- result of the last command
   signal r_sr      : std_logic_vector(47 downto 0) := (others => '0');   -- native response
   signal r_crc     : std_logic_vector(6 downto 0) := (others => '0');

   -- SPI byte machinery
   signal p_txsr    : std_logic_vector(7 downto 0) := x"FF";
   signal p_rxsr    : std_logic_vector(7 downto 0) := x"FF";
   signal p_byte    : std_logic_vector(7 downto 0) := x"FF";  -- last complete rx byte
   signal p_new     : std_logic := '0';                       -- p_byte is new
   signal p_r1      : std_logic_vector(7 downto 0) := x"FF";
   signal p_ext     : std_logic_vector(31 downto 0) := (others => '0');
   signal p_wait    : natural range 0 to 15 := 0;
   signal p_ok      : std_logic := '0';     -- outcome of the transfer being ended
   signal cs        : std_logic := '1';

   -- operation
   signal op_eng    : std_logic := '0';     -- engine transfer (else QNICE)
   signal op_wr     : std_logic := '0';
   signal op_addr   : unsigned(31 downto 0) := (others => '0');
   signal op_count  : unsigned(15 downto 0) := (others => '0');
   signal op_done   : unsigned(15 downto 0) := (others => '0');   -- blocks finished
   signal op_multi  : std_logic := '0';
   signal op_try    : natural range 0 to 3 := 0;
   signal op_verify : std_logic := '0';     -- the test read of the native init
   signal op_sw     : std_logic := '0';     -- the CMD6 status block (64 bytes)
   signal op_cal    : std_logic := '0';     -- a test read of the calibration
   signal fail_stop : std_logic := '0';     -- S_FAIL sent its CMD12
   signal blk_k     : unsigned(15 downto 0) := (others => '0');   -- block being moved
   signal bidx      : unsigned(9 downto 0) := (others => '0');    -- byte / nibble index
   signal hinib     : std_logic_vector(3 downto 0) := (others => '0');
   signal txbyte    : std_logic_vector(7 downto 0) := (others => '0');
   signal crc_l     : std_logic_vector(63 downto 0) := (others => '0');  -- 4 x CRC16
   signal crc_rx    : std_logic_vector(63 downto 0) := (others => '0');
   signal stat      : std_logic_vector(2 downto 0) := (others => '0');

   -- QNICE side
   type t_buf is array (0 to 511) of std_logic_vector(7 downto 0);
   signal qbuf      : t_buf := (others => (others => '0'));
   signal q_addr_lo, q_addr_hi, q_pos : std_logic_vector(15 downto 0) := (others => '0');
   signal q_rd      : std_logic_vector(7 downto 0) := (others => '0');
   signal q_req_rst, q_req_rd, q_req_wr : std_logic := '0';   -- falling-edge toggles
   signal q_seen_rst, q_seen_rd, q_seen_wr : std_logic := '0';
   signal q_busy    : std_logic := '1';
   signal q_error   : std_logic := '0';
   signal q_errcode : std_logic_vector(15 downto 0) := (others => '0');
   -- what QNICE reads, copied into the clk_i domain
   signal c_busy, c_error, c_ready, c_slow, c_native, c_hs : std_logic := '0';
   signal c_seen_rd, c_seen_wr, c_seen_rst : std_logic := '0';
   signal c_errcode : std_logic_vector(15 downto 0) := (others => '0');
   signal c_ctype   : std_logic_vector(1 downto 0) := "00";
   signal c_samp    : unsigned(1 downto 0) := "00";
   signal c_retries : unsigned(7 downto 0) := (others => '0');

   -- buffer write from the controller (applied on the falling clk_i edge; cb_we_d holds
   -- the strobe a second clk2x_i clock)
   signal cb_we     : std_logic := '0';
   signal cb_we_d   : std_logic := '0';
   signal cb_addr   : unsigned(8 downto 0) := (others => '0');
   signal cb_data   : std_logic_vector(7 downto 0) := (others => '0');

   -- data source (QNICE buffer or engine), valid two clocks after src_idx changed
   signal src_idx   : unsigned(8 downto 0) := (others => '0');
   signal src_q     : std_logic_vector(7 downto 0) := (others => '0');
   signal src_data  : std_logic_vector(7 downto 0);

   -- engine port
   signal e_pend    : std_logic := '0';
   signal e_busy    : std_logic := '0';
   signal e_waddr   : std_logic_vector(31 downto 0) := (others => '0');
   signal e_wcount  : std_logic_vector(15 downto 0) := (others => '0');
   signal e_wwrite  : std_logic := '0';
   signal e_rd_we   : std_logic := '0';
   signal e_rd_data : std_logic_vector(7 downto 0) := (others => '0');
   signal e_rd_idx  : unsigned(8 downto 0) := (others => '0');
   signal e_done    : std_logic := '0';
   signal e_error   : std_logic := '0';
   signal e_blk     : std_logic := '0';
   signal e_rd_we_d, e_done_d, e_blk_d : std_logic := '0';   -- the strobes' second clock
   signal e_wr_q    : std_logic_vector(7 downto 0) := (others => '0');   -- e_wr_data_i, clk_i reg
   -- e_limit_i and e_abort_i, clk_i registers (the engine's limit comes out of adders: a
   -- clk_i register gives the controller's compare the whole clk2x_i clock; one clock
   -- late only ever allows less, and the first look comes many SD clocks after a start)
   signal e_limit_q : std_logic_vector(15 downto 0) := (others => '0');
   signal e_abort_q : std_logic := '0';
   signal wr2       : std_logic;            -- write data address two bytes ahead

   signal qn_pending : std_logic;           -- a QNICE command waits
   signal preempt    : std_logic;           -- an engine transfer has to end now
   signal e_room     : std_logic;           -- block blk_k may be moved now

begin

   ---------------------------------------------------------------------------------------
   -- outputs
   ---------------------------------------------------------------------------------------

   sd_clk_o    <= clk_q;
   cmd_o       <= cmd_r;
   cmd_oe_o    <= cmd_oe_r;
   dat_o       <= dat_r;
   dat_oe_o    <= dat_oe_r;

   -- the engine port: clk_i registers (Megamiga 0.7: the controller runs on clk2x_i;
   -- its strobes last two clk2x_i clocks, so each one is taken exactly once)
   e_out : process (clk_i)
   begin
      if rising_edge(clk_i) then
         e_ready_o   <= ready;
         e_busy_o    <= e_busy;
         e_done_o    <= e_done or e_done_d;
         e_error_o   <= e_error;
         e_blk_o     <= e_blk or e_blk_d;
         e_rd_we_o   <= e_rd_we or e_rd_we_d;
         e_rd_blk_o  <= std_logic_vector(blk_k);
         e_rd_idx_o  <= std_logic_vector(e_rd_idx);
         e_rd_data_o <= e_rd_data;
         e_wr_blk_o  <= std_logic_vector(blk_k);
         e_wr_idx_o  <= std_logic_vector(src_idx);
         e_wr_q      <= e_wr_data_i;
         e_limit_q   <= e_limit_i;
         e_abort_q   <= e_abort_i;
      end if;
   end process e_out;

   src_data    <= e_wr_q when op_eng = '1' else src_q;
   wr2         <= op_eng and hs;

   qn_pending  <= '1' when q_req_rd /= q_seen_rd or q_req_wr /= q_seen_wr or q_req_rst /= q_seen_rst else '0';
   preempt     <= op_eng and (qn_pending or e_abort_q);
   e_room      <= '1' when op_eng = '0' or blk_k < unsigned(e_limit_q) else '0';

   -- the inputs a step sees: default speed: sampled at the clock edge before the step
   -- (the falling SD edge); high speed: at sample point samp (see "High speed")
   cmd_in <= cmd_ri when hs = '0' or samp = 0 else
             cmd_l1 when samp = 1 else
             cmd_l2 when samp = 2 else
             cmd_l3;
   dat_in <= dat_ri when hs = '0' or samp = 0 else
             dat_l1 when samp = 1 else
             dat_l2 when samp = 2 else
             dat_l3;

   -- the falling clk2x_i edge samples (sample point 2; only into cmd_l2/dat_l2: a direct
   -- falling-to-rising path, half a clock)
   samples_f : process (clk2x_i)
   begin
      if falling_edge(clk2x_i) then
         cmd_fi <= cmd_i;
         dat_fi <= dat_i;
      end if;
   end process samples_f;

   -- what QNICE reads from the controller, in the clk_i domain
   q_copy : process (clk_i)
   begin
      if rising_edge(clk_i) then
         c_busy     <= q_busy;
         c_error    <= q_error;
         c_errcode  <= q_errcode;
         c_ctype    <= ctype;
         c_ready    <= ready;
         c_slow     <= slow;
         c_native   <= native;
         c_hs       <= hs;
         c_samp     <= samp;
         c_retries  <= retries;
         c_seen_rd  <= q_seen_rd;
         c_seen_wr  <= q_seen_wr;
         c_seen_rst <= q_seen_rst;
      end if;
   end process q_copy;

   ---------------------------------------------------------------------------------------
   -- QNICE registers and buffer (falling edge, like sdcard.vhd)
   ---------------------------------------------------------------------------------------

   q_regs : process (clk_i)
   begin
      if falling_edge(clk_i) then
         if en_i = '1' and we_i = '1' then
            case reg_i is
               when "000" => q_addr_lo <= data_i;
               when "001" => q_addr_hi <= data_i;
               when "010" => q_pos     <= data_i;
               when "110" => spi_only  <= data_i(0);
               when "101" =>
                  -- a command toggles its request; the controller copies the toggle
                  -- (q_seen_*) when it takes the command, the busy bit covers the gap
                  if data_i = x"0000" then
                     q_req_rst <= not q_req_rst;
                  elsif data_i = x"0001" then
                     q_req_rd <= not q_req_rd;
                  elsif data_i = x"0002" then
                     q_req_wr <= not q_req_wr;
                  end if;
               when others => null;
            end case;
         end if;
      end if;
   end process q_regs;

   -- the buffer: one write port on the falling edge (the controller during a QNICE
   -- read, else QNICE while idle), asynchronous reads
   q_buffer : process (clk_i)
   begin
      if falling_edge(clk_i) then
         if cb_we = '1' or cb_we_d = '1' then
            qbuf(to_integer(cb_addr)) <= cb_data;
         elsif en_i = '1' and we_i = '1' and reg_i = "011" and c_busy = '0' then
            qbuf(to_integer(unsigned(q_pos(8 downto 0)))) <= data_i(7 downto 0);
         end if;
         q_rd <= qbuf(to_integer(unsigned(q_pos(8 downto 0))));
      end if;
   end process q_buffer;

   q_read : process (all)
      variable csr  : std_logic_vector(15 downto 0);
      variable pend : std_logic;
   begin
      data_o <= (others => '0');
      if en_i = '1' and we_i = '0' then
         if q_req_rd /= c_seen_rd or q_req_wr /= c_seen_wr or q_req_rst /= c_seen_rst then
            pend := '1';
         else
            pend := '0';
         end if;
         csr := (others => '0');
         csr(15) := c_busy or pend;
         csr(14) := c_error and not (c_busy or pend);
         csr(13 downto 12) := c_ctype;
         case reg_i is
            when "000" => data_o <= q_addr_lo;
            when "001" => data_o <= q_addr_hi;
            when "010" => data_o <= q_pos;
            when "011" => data_o <= x"00" & q_rd;
            when "100" => data_o <= c_errcode;
            when "101" => data_o <= csr;
            when "110" => data_o <= std_logic_vector(c_retries) & '0' & std_logic_vector(c_samp) & c_hs &
                                    spi_only & c_ready & c_slow & c_native;
            when others => null;
         end case;
      end if;
   end process q_read;

   ---------------------------------------------------------------------------------------
   -- the controller (rising edge of clk2x_i)
   ---------------------------------------------------------------------------------------

   ctrl : process (clk2x_i)

      -- start a command (native or SPI) and come back to `ret`
      procedure do_cmd(idx : natural; arg : std_logic_vector(31 downto 0); resp : t_resp; ret : t_state) is
      begin
         c_frame <= cmd_frame(idx, arg);
         c_idx   <= idx;
         c_resp  <= resp;
         ret_st  <= ret;
         cnt     <= 0;
         if native = '1' then
            st <= N_CMD_TX;
         else
            st <= P_CMD;
         end if;
      end procedure;

      -- the card address of block op_addr + k
      function card_addr(a : unsigned(31 downto 0); k : unsigned(15 downto 0); h : std_logic) return std_logic_vector is
         variable bb : unsigned(31 downto 0);
      begin
         bb := a + resize(k, 32);
         if h = '1' then
            return std_logic_vector(bb);
         else
            return std_logic_vector(bb(22 downto 0) & "000000000");
         end if;
      end function;

      function r1_bad(r : std_logic_vector(47 downto 0)) return boolean is
      begin
         return (r(39 downto 8) and C_R1_ERRMASK) /= x"00000000";
      end function;

      variable b     : std_logic_vector(7 downto 0);
      variable nib   : std_logic_vector(3 downto 0);
      variable crc   : std_logic_vector(63 downto 0);
      variable ld    : boolean;                        -- SPI: load the next tx byte now
      variable txv   : std_logic_vector(7 downto 0);   -- SPI: the byte to load
      variable cs_v  : std_logic;                      -- SPI: chip select (at byte edges)

   begin
      if rising_edge(clk2x_i) then

         -- input samples: ONE flip-flop. The register captures the lines at the edge
         -- that starts the next SD cycle (the falling SD clock edge), so a step
         -- sees the card's bit of the cycle that just ended: launched one full SD
         -- clock earlier. (No synchroniser: the card's outputs are synchronous to
         -- the clock we drive; a second stage would move the sample point half an
         -- SD clock earlier, to the rising edge, and eat the card's output delay.)
         cmd_ri <= cmd_i;
         dat_ri <= dat_i;

         -- high speed: the samples taken at the step (point 1), half a clock after it
         -- (point 2) and one clock after it (point 3), for the next step
         post <= tick;
         if post = '1' then
            cmd_l1 <= cmd_ri;
            dat_l1 <= dat_ri;
            cmd_l2 <= cmd_fi;
            dat_l2 <= dat_fi;
            cmd_l3 <= cmd_i;
            dat_l3 <= dat_i;
         end if;

         -- one-clock pulses (and their second clock for the clk_i side)
         cb_we_d   <= cb_we;
         e_rd_we_d <= e_rd_we;
         e_done_d  <= e_done;
         e_blk_d   <= e_blk;
         tick    <= '0';
         cb_we   <= '0';
         e_rd_we <= '0';
         e_done  <= '0';
         e_blk   <= '0';

         -- data source: QNICE buffer, registered (the engine's has its own latency)
         src_q <= qbuf(to_integer(src_idx));

         -- engine start requests
         if e_start_i = '1' and e_busy = '0' and e_pend = '0' then
            e_pend   <= '1';
            e_waddr  <= e_addr_i;
            e_wcount <= e_count_i;
            e_wwrite <= e_write_i;
         end if;

         if tmo /= 0 then
            tmo <= tmo - 1;
         end if;

         ---------------------------------------------------------------------------------
         -- SD clock: a falling edge starts a cycle, only while run = '1'
         ---------------------------------------------------------------------------------
         if half_cnt /= 0 then
            half_cnt <= half_cnt - 1;
         elsif clk_r = '0' then
            clk_r    <= '1';
            clk_q    <= '1';
            half_cnt <= half - 1;
         elsif run = '1' then
            clk_r    <= '0';
            clk_q    <= '0';
            half_cnt <= half - 1;
            tick     <= '1';
            ph       <= ph + 1;
            cmd_r    <= cmd_nx;
            cmd_oe_r <= cmd_oe_nx;
            dat_r    <= dat_nx;
            dat_oe_r <= dat_oe_nx;
         end if;

         -- SPI: at the step of bit phase 7 the next cycle starts a byte
         ld   := native = '0' and tick = '1' and ph = 7;
         txv  := x"FF";
         cs_v := cs;

         ---------------------------------------------------------------------------------
         -- main state machine
         ---------------------------------------------------------------------------------
         case st is

            when S_RESET =>
               run       <= '0';
               ready     <= '0';
               native    <= '0';
               slow      <= '0';
               hs        <= '0';
               hs_ok     <= '1';
               op_sw     <= '0';
               op_cal    <= '0';
               ctype     <= "00";
               half      <= G_HALF_INIT;
               cs_v        := '1';
               cmd_nx <= '1'; cmd_oe_nx <= '1';
               dat_nx <= "1111"; dat_oe_nx <= "1000";
               cmd_r  <= '1'; cmd_oe_r <= '1';
               dat_r  <= "1111"; dat_oe_r <= "1000";
               if G_NATIVE and spi_only = '0' then
                  native_ok <= '1';
               else
                  native_ok <= '0';
               end if;
               retrying  <= '0';
               q_busy    <= '1';
               tmo       <= 2 * G_MS;              -- let the card settle
               st        <= S_INIT;

            when S_INIT =>
               fail_stop <= '0';
               if tmo = 0 then
                  ready  <= '0';
                  half   <= G_HALF_INIT;
                  hs     <= '0';
                  samp   <= "00";
                  op_sw  <= '0';
                  op_cal <= '0';
                  cnt    <= 0;
                  istep  <= 0;
                  tries  <= 0;
                  run    <= '1';
                  cs_v     := '1';
                  cmd_nx <= '1';
                  dat_nx <= "1111"; dat_oe_nx <= "1000";
                  if native_ok = '1' then
                     -- DAT3 stays high (driven) so that CMD0 keeps the card in SD mode
                     native    <= '1';
                     cmd_oe_nx <= '0';
                     st        <= N_PRE;
                  else
                     native    <= '0';
                     cmd_oe_nx <= '1';
                     ph        <= "111";
                     p_txsr    <= x"FF";
                     st        <= P_PRE;
                  end if;
               end if;

            --------------------------------------------------------------------------- idle
            when S_IDLE =>
               run <= '0';
               if q_req_rst /= q_seen_rst then
                  q_seen_rst <= q_req_rst;
                  q_seen_rd  <= q_req_rd;          -- a reset cancels queued commands
                  q_seen_wr  <= q_req_wr;
                  q_error    <= '0';
                  q_errcode  <= (others => '0');
                  q_busy     <= '1';
                  if G_NATIVE and spi_only = '0' then
                     native_ok <= '1';
                  else
                     native_ok <= '0';
                  end if;
                  retrying   <= '0';
                  slow       <= '0';
                  hs_ok      <= '1';
                  tmo        <= 0;
                  st         <= S_INIT;
               elsif q_req_rd /= q_seen_rd or q_req_wr /= q_seen_wr then
                  if q_req_rd /= q_seen_rd then
                     q_seen_rd <= q_req_rd;
                     op_wr     <= '0';
                  else
                     q_seen_wr <= q_req_wr;
                     op_wr     <= '1';
                  end if;
                  if q_error = '1' then
                     q_busy <= '0';                -- sticky error: ignored until a reset
                  elsif ready = '0' then
                     q_error   <= '1';
                     q_errcode <= C_ERR_NOCARD;
                     q_busy    <= '0';
                  else
                     q_busy    <= '1';
                     op_eng    <= '0';
                     op_addr   <= unsigned(std_logic_vector'(q_addr_hi & q_addr_lo));
                     op_count  <= to_unsigned(1, 16);
                     op_done   <= (others => '0');
                     op_try    <= 0;
                     op_verify <= '0';
                     st        <= S_DISPATCH;
                  end if;
               elsif e_pend = '1' then
                  e_pend    <= '0';
                  blk_k     <= (others => '0');
                  src_idx   <= (others => '0');
                  op_eng    <= '1';
                  op_wr     <= e_wwrite;
                  op_addr   <= unsigned(e_waddr);
                  op_count  <= unsigned(e_wcount);
                  op_done   <= (others => '0');
                  op_try    <= 0;
                  op_verify <= '0';
                  if ready = '0' or unsigned(e_wcount) = 0 then
                     e_done  <= '1';
                     e_error <= '1';
                  else
                     e_busy <= '1';
                     st     <= S_DISPATCH;
                  end if;
               end if;

            -- (re)start the remaining blocks of the operation, or finish it
            when S_DISPATCH =>
               fail_stop <= '0';
               if e_blk = '1' or e_blk_d = '1' then
                  -- the engine must see a block's e_blk_o and the e_done_o in different
                  -- clk_i clocks (ide_engine: "never in the same clock"): wait until the
                  -- stretched e_blk is over
                  null;
               elsif op_done = op_count or preempt = '1' then
                  if op_eng = '1' then
                     e_busy  <= '0';
                     e_done  <= '1';
                     e_error <= '0';
                  else
                     q_busy  <= '0';
                  end if;
                  cnt <= 0;
                  st  <= S_TAIL;
               else
                  blk_k    <= op_done;
                  if op_count - op_done > 1 then
                     op_multi <= '1';
                  else
                     op_multi <= '0';
                  end if;
                  run      <= '1';
                  if native = '1' then
                     if op_wr = '1' then
                        st <= N_WR_CMD;
                     else
                        st <= N_RD_CMD;
                     end if;
                  else
                     if op_wr = '1' then
                        st <= P_WR_CMD;
                     else
                        st <= P_RD_CMD;
                     end if;
                  end if;
               end if;

            -- 8 more clocks after the last command or data, then stop the clock
            when S_TAIL =>
               run <= '1';
               if native = '1' then
                  cmd_oe_nx <= '0';
                  dat_oe_nx <= "0000";
               end if;
               if tick = '1' then
                  if cnt = 8 then
                     cnt <= 0;
                     st  <= S_IDLE;
                  else
                     cnt <= cnt + 1;
                  end if;
               end if;

            -- a transfer failed: (50 MHz ->) native -> SPI 25 MHz -> SPI 12.5 MHz, then give up
            when S_FAIL =>
               run <= '0';
               if native = '1' and op_multi = '1' and fail_stop = '0' then
                  -- a native multi-block transfer: CMD12 first (its result does not matter),
                  -- else the card goes on sending blocks while the new init drives DAT3
                  fail_stop <= '1';
                  run       <= '1';
                  do_cmd(12, x"00000000", R_R1B, S_FAIL);
               elsif op_sw = '1' or op_cal = '1' then
                  -- the CMD6 status block or a calibration read: N_INIT decides
                  c_ok <= '0';
                  st   <= N_INIT;
               elsif hs = '1' then
                  -- 50 MHz failed: a new native init without high speed (does not count
                  -- as a try: hs_ok stays off until a QNICE reset)
                  hs_ok    <= '0';
                  retries  <= retries + 1;
                  retrying <= '1';
                  tmo      <= 0;
                  st       <= S_INIT;
               elsif op_verify = '1' then
                  -- the test read of the native init: continue with SPI
                  op_verify <= '0';
                  native_ok <= '0';
                  tmo       <= 0;
                  st        <= S_INIT;
               elsif op_try < 2 and (native = '1' or slow = '0') then
                  op_try   <= op_try + 1;
                  retries  <= retries + 1;
                  retrying <= '1';
                  if native = '1' then
                     native_ok <= '0';
                  else
                     slow <= '1';
                  end if;
                  tmo <= 0;
                  st  <= S_INIT;                   -- P_INIT resumes at S_DISPATCH
               else
                  if op_eng = '1' then
                     e_busy  <= '0';
                     e_done  <= '1';
                     e_error <= '1';
                  else
                     q_error   <= '1';
                     q_errcode <= C_ERR_DATA;
                     q_busy    <= '0';
                  end if;
                  cnt <= 0;
                  st  <= S_TAIL;
               end if;

            ---------------------------------------------------------------------------
            -- native SD: init
            ---------------------------------------------------------------------------

            when N_PRE =>                          -- 80 clocks with DAT3 high
               if tick = '1' then
                  if cnt = 80 then
                     cnt   <= 0;
                     istep <= 0;
                     st    <= N_INIT;
                  else
                     cnt <= cnt + 1;
                  end if;
               end if;

            when N_INIT =>
               case istep is
                  when 0 =>                        -- CMD0: go idle (no response)
                     istep <= 1;
                     do_cmd(0, x"00000000", R_NONE, N_INIT);
                  when 1 =>                        -- release DAT3, a few idle clocks
                     dat_oe_nx <= "0000";
                     cmd_oe_nx <= '0';
                     if tick = '1' then
                        if cnt = 8 then
                           istep <= 2;
                           do_cmd(8, x"000001AA", R_R7, N_INIT);
                        else
                           cnt <= cnt + 1;
                        end if;
                     end if;
                  when 2 =>                        -- CMD8 answered: V2 (else V1)
                     if c_ok = '1' and r_sr(19 downto 8) = x"1AA" then
                        v2 <= '1';
                     else
                        v2 <= '0';
                     end if;
                     tmo   <= C_TMO_INIT;
                     istep <= 3;
                  when 3 =>                        -- ACMD41 loop
                     istep <= 4;
                     do_cmd(55, x"00000000", R_R1, N_INIT);
                  when 4 =>
                     if c_ok = '0' then
                        istep <= 30;               -- no SD-mode card: SPI
                     else
                        istep <= 5;
                        if v2 = '1' then
                           do_cmd(41, x"40FF8000", R_R3, N_INIT);
                        else
                           do_cmd(41, x"00FF8000", R_R3, N_INIT);
                        end if;
                     end if;
                  when 5 =>
                     if c_ok = '0' then
                        istep <= 30;
                     elsif r_sr(39) = '1' then     -- power-up done
                        hc    <= r_sr(38) and v2;
                        istep <= 6;
                     elsif tmo = 0 then
                        istep <= 30;
                     else
                        istep <= 3;
                     end if;
                  when 6 =>
                     istep <= 7;
                     do_cmd(2, x"00000000", R_R2, N_INIT);
                  when 7 =>
                     if c_ok = '0' then
                        istep <= 30;
                     else
                        istep <= 8;
                        do_cmd(3, x"00000000", R_R1, N_INIT);   -- R6: RCA in bits 39..24
                     end if;
                  when 8 =>
                     if c_ok = '0' then
                        istep <= 30;
                     else
                        rca   <= r_sr(39 downto 24);
                        half  <= G_HALF_FAST;      -- data transfer mode: 25 MHz
                        istep <= 9;
                        do_cmd(7, r_sr(39 downto 24) & x"0000", R_R1B, N_INIT);
                     end if;
                  when 9 =>
                     if c_ok = '0' then
                        istep <= 30;
                     else
                        istep <= 10;
                        do_cmd(55, rca & x"0000", R_R1, N_INIT);
                     end if;
                  when 10 =>
                     if c_ok = '0' then
                        istep <= 30;
                     else
                        istep <= 11;
                        do_cmd(6, x"00000002", R_R1, N_INIT);   -- 4-bit bus
                     end if;
                  when 11 =>
                     if c_ok = '0' then
                        istep <= 30;
                     elsif hc = '0' then
                        istep <= 12;
                        do_cmd(16, x"00000200", R_R1, N_INIT);
                     else
                        istep <= 13;
                     end if;
                  when 12 =>
                     if c_ok = '0' then
                        istep <= 30;
                     else
                        istep <= 13;
                     end if;
                  when 13 =>                       -- test read of block 0 on all 4 lines
                     if hc = '1' then
                        ctype <= "11";
                     elsif v2 = '1' then
                        ctype <= "10";
                     else
                        ctype <= "01";
                     end if;
                     ready     <= '1';
                     if retrying = '1' then
                        -- a retry after a failure at 50 MHz (S_FAIL): the bus was tested at
                        -- the first init, and the test read would overwrite the operation
                        -- (op_addr, op_count, op_done) that is to be resumed
                        istep <= 22;
                     else
                        op_verify <= '1';
                        op_wr     <= '0';
                        op_addr   <= (others => '0');
                        op_count  <= to_unsigned(1, 16);
                        op_done   <= (others => '0');
                        op_multi  <= '0';
                        blk_k     <= (others => '0');
                        op_eng    <= '0';
                        istep     <= 14;
                        st        <= N_RD_CMD;
                     end if;
                  when 14 =>                       -- passed (failures go to S_FAIL)
                     if G_HS and hs_ok = '1' then
                        -- CMD6 mode 1, group 1 = function 1 (high speed); the 64-byte status
                        -- block comes like a read block (N_RD_*, op_sw: 128 nibbles)
                        op_sw   <= '1';
                        c_ok    <= '1';
                        blk_k   <= (others => '0');
                        istep   <= 15;
                        do_cmd(6, x"80FFFFF1", R_R1, N_RD_RESP);
                     else
                        istep <= 22;
                     end if;
                  when 15 =>                       -- the status block (or S_FAIL: c_ok = 0)
                     op_sw <= '0';
                     cnt   <= 0;
                     if c_ok = '1' and sw_res = "0001" then
                        istep <= 16;               -- switched
                     elsif c_ok = '1' and sw_res = "1111" then
                        istep <= 22;               -- no high speed: stay at 25 MHz
                     else
                        istep <= 21;               -- unknown state: a new init without it
                     end if;
                  when 16 =>                       -- 8 clocks at 25 MHz, then 50 MHz
                     run       <= '1';
                     cmd_oe_nx <= '0';
                     dat_oe_nx <= "0000";
                     if tick = '1' then
                        if cnt = 8 then
                           half     <= G_HALF_HS;
                           hs       <= '1';
                           samp     <= "00";
                           cal_pass <= (others => '0');
                           cal_n    <= 0;
                           istep    <= 17;
                        else
                           cnt <= cnt + 1;
                        end if;
                     end if;
                  when 17 =>                       -- a test read of block 0 at sample point samp
                     op_cal   <= '1';
                     c_ok     <= '1';
                     op_wr    <= '0';
                     op_addr  <= (others => '0');
                     op_count <= to_unsigned(1, 16);
                     op_done  <= (others => '0');
                     op_multi <= '0';
                     blk_k    <= (others => '0');
                     istep    <= 18;
                     st       <= N_RD_CMD;
                  when 18 =>                       -- its result (S_FAIL: c_ok = 0)
                     op_cal <= '0';
                     tmo    <= C_CAL_DRAIN;
                     cnt    <= 0;
                     if c_ok = '0' then
                        istep <= 19;               -- failed: let the card finish first
                     elsif cal_n + 1 = C_CAL_READS then
                        cal_pass(to_integer(samp)) <= '1';
                        istep <= 20;
                     else
                        cal_n <= cal_n + 1;
                        istep <= 17;
                     end if;
                  when 19 =>                       -- drain: clock with the lines released
                     -- (the card may still be sending the block: a new command would be
                     -- illegal in its data state)
                     run       <= '1';
                     cmd_oe_nx <= '0';
                     dat_oe_nx <= "0000";
                     if tick = '1' and cnt /= 2047 then
                        cnt <= cnt + 1;
                     end if;
                     if tmo = 0 and cnt = 2047 then
                        istep <= 20;
                     end if;
                  when 20 =>                       -- the next sample point, or the choice
                     cal_n <= 0;
                     if samp /= 3 then
                        samp  <= samp + 1;
                        istep <= 17;
                     elsif cal_pass = "0000" then
                        istep <= 21;
                     else
                        samp  <= to_unsigned(C_SAMP_PICK(to_integer(unsigned(cal_pass))), 2);
                        istep <= 22;
                     end if;
                  when 21 =>                       -- a new native init without high speed
                     hs_ok     <= '0';
                     hs        <= '0';
                     op_verify <= '0';
                     ready <= '0';
                     tmo   <= 0;
                     st    <= S_INIT;
                  when 22 =>                       -- done
                     op_verify <= '0';
                     if retrying = '1' then
                        -- a retry without high speed (S_FAIL)
                        retrying <= '0';
                        st       <= S_DISPATCH;
                     else
                        q_busy <= '0';
                        cnt    <= 0;
                        st     <= S_TAIL;
                     end if;
                  when others =>                   -- 30: native failed, init in SPI mode
                     native_ok <= '0';
                     ready     <= '0';
                     tmo       <= 0;
                     st        <= S_INIT;
               end case;

            ---------------------------------------------------------------------------
            -- native SD: send a command, receive its response
            ---------------------------------------------------------------------------

            when N_CMD_TX =>
               cmd_oe_nx <= '1';
               if tick = '1' then
                  cmd_nx  <= c_frame(47);
                  c_frame <= c_frame(46 downto 0) & '1';
                  if cnt = 47 then
                     -- the end bit is queued now; skip our own last two bits
                     cnt       <= 0;
                     skip      <= 2;
                     c_started <= '0';
                     c_ok      <= '1';
                     if c_resp = R_NONE then
                        st <= ret_st;
                     else
                        st <= N_CMD_RX;
                     end if;
                  else
                     cnt <= cnt + 1;
                  end if;
               end if;

            when N_CMD_RX =>
               if tick = '1' then
                  cmd_oe_nx <= '0';
                  if skip /= 0 then
                     skip <= skip - 1;
                  elsif c_started = '0' then
                     if cmd_in = '0' then
                        c_started <= '1';
                        cnt       <= 1;
                        r_sr      <= (others => '0');
                        r_crc     <= crc7_bit("0000000", '0');
                     elsif cnt = C_NCR_MAX then
                        c_ok <= '0';               -- no response
                        st   <= ret_st;
                     else
                        cnt <= cnt + 1;
                     end if;
                  else
                     -- cnt = number of bits received so far: this is frame bit 47 - cnt
                     r_sr <= r_sr(46 downto 0) & cmd_in;
                     if cnt < 40 then
                        r_crc <= crc7_bit(r_crc, cmd_in);
                     end if;
                     if (c_resp = R_R2 and cnt = 135) or (c_resp /= R_R2 and cnt = 47) then
                        -- end bit; r_sr(45 downto 0) = frame bits 46..1
                        cnt <= 0;
                        if c_resp = R_R1 or c_resp = R_R1B or c_resp = R_R7 then
                           if r_sr(6 downto 0) /= r_crc or
                              r_sr(44 downto 39) /= std_logic_vector(to_unsigned(c_idx, 6)) then
                              c_ok <= '0';
                           end if;
                        end if;
                        if c_resp = R_R1B then
                           st <= N_BUSY0;
                        else
                           st <= ret_st;
                        end if;
                     else
                        cnt <= cnt + 1;
                     end if;
                  end if;
               end if;

            -- busy on DAT0: wait up to 16 clocks for low, then for high
            when N_BUSY0 =>
               if tick = '1' then
                  if dat_in(0) = '0' then
                     tmo <= C_TMO_BUSY;
                     st  <= N_BUSY1;
                  elsif cnt = 16 then
                     cnt <= 0;
                     st  <= ret_st;
                  else
                     cnt <= cnt + 1;
                  end if;
               end if;

            when N_BUSY1 =>
               if tick = '1' and dat_in(0) = '1' then
                  cnt <= 0;
                  st  <= ret_st;
               elsif tmo = 0 then
                  c_ok <= '0';
                  cnt  <= 0;
                  st   <= ret_st;
               end if;

            ---------------------------------------------------------------------------
            -- native SD: read blocks
            ---------------------------------------------------------------------------

            when N_RD_CMD =>
               if op_multi = '1' then
                  do_cmd(18, card_addr(op_addr, blk_k, hc), R_R1, N_RD_RESP);
               else
                  do_cmd(17, card_addr(op_addr, blk_k, hc), R_R1, N_RD_RESP);
               end if;

            when N_RD_RESP =>
               if c_ok = '0' or r1_bad(r_sr) then
                  st <= S_FAIL;
               else
                  if op_cal = '1' then
                     tmo <= C_TMO_CAL;
                  else
                     tmo <= C_TMO_READ;
                  end if;
                  st  <= N_RD_START;
               end if;

            when N_RD_START =>
               -- flow control: the clock only runs while block blk_k may be stored
               if e_room = '0' then
                  run <= '0';
                  tmo <= C_TMO_READ;
                  if preempt = '1' then
                     st <= N_STOP;
                  end if;
               else
                  run <= '1';
                  if tick = '1' and dat_in(0) = '0' then
                     bidx  <= (others => '0');
                     crc_l <= (others => '0');
                     st    <= N_RD_DATA;
                  elsif tmo = 0 then
                     st <= S_FAIL;
                  end if;
               end if;

            when N_RD_DATA =>                      -- 1024 nibbles, high nibble first
               if tick = '1' then
                  nib := dat_in;
                  crc := crc_l;
                  for l in 0 to 3 loop
                     crc(16*l+15 downto 16*l) := crc16_bit(crc_l(16*l+15 downto 16*l), nib(l));
                  end loop;
                  crc_l <= crc;
                  if op_sw = '1' and bidx = 33 then
                     sw_res <= nib;                -- status bits 379..376: group 1 result
                  end if;
                  if bidx(0) = '0' then
                     hinib <= nib;
                  else
                     b := hinib & nib;
                     if op_eng = '1' then
                        e_rd_we   <= '1';
                        e_rd_idx  <= bidx(9 downto 1);
                        e_rd_data <= b;
                     elsif op_verify = '0' then
                        cb_we   <= '1';
                        cb_addr <= bidx(9 downto 1);
                        cb_data <= b;
                     end if;
                  end if;
                  if bidx = 1023 or (op_sw = '1' and bidx = 127) then
                     cnt    <= 0;
                     crc_rx <= (others => '0');
                     st     <= N_RD_CRC;
                  end if;
                  bidx <= bidx + 1;
               end if;

            when N_RD_CRC =>                       -- 16 CRC bits per line, then the end bit
               if tick = '1' then
                  if cnt < 16 then
                     for l in 0 to 3 loop
                        crc_rx(16*l+15 downto 16*l) <= crc_rx(16*l+14 downto 16*l) & dat_in(l);
                     end loop;
                     cnt <= cnt + 1;
                  else
                     cnt <= 0;
                     if crc_rx /= crc_l then
                        st <= S_FAIL;
                     else
                        st <= N_RD_NEXT;
                     end if;
                  end if;
               end if;

            when N_RD_NEXT =>
               op_done <= op_done + 1;
               blk_k   <= blk_k + 1;
               e_blk   <= op_eng;
               tmo     <= C_TMO_READ;
               if op_verify = '1' then
                  st <= N_INIT;                    -- istep 14
               elsif op_multi = '0' then
                  st <= S_DISPATCH;
               elsif blk_k + 1 = op_count or preempt = '1' then
                  st <= N_STOP;
               else
                  st <= N_RD_START;
               end if;

            -- end a multi-block transfer (read or write) with CMD12
            when N_STOP =>
               run <= '1';
               do_cmd(12, x"00000000", R_R1B, N_STOP_DONE);

            when N_STOP_DONE =>
               if c_ok = '0' then
                  st <= S_FAIL;
               else
                  st <= S_DISPATCH;
               end if;

            ---------------------------------------------------------------------------
            -- native SD: write blocks
            ---------------------------------------------------------------------------

            when N_WR_CMD =>
               if op_multi = '1' then
                  do_cmd(25, card_addr(op_addr, blk_k, hc), R_R1, N_WR_RESP);
               else
                  do_cmd(24, card_addr(op_addr, blk_k, hc), R_R1, N_WR_RESP);
               end if;

            when N_WR_RESP =>
               if c_ok = '0' or r1_bad(r_sr) then
                  st <= S_FAIL;
               else
                  cnt <= 0;
                  st  <= N_WR_GAP;
               end if;

            when N_WR_GAP =>
               -- wait until the data is there (flow control), then at least two idle
               -- clocks before the start bit.
               -- The data address: byte 0 now. At 50 MHz from the engine (wr2) a byte goes
               -- out every 4 clocks, and the engine's byte arrives 6 or 7 clocks after its
               -- address changed (our clk_i output register, its block RAM, our clk_i input
               -- register; 6 or 7 by the clk_i phase) and stays until 5 or 6 clocks after
               -- the next change: so the address runs two bytes ahead (byte n + 2 when byte
               -- n is taken, 8 clocks before byte n + 2 is needed), and byte 1 goes out at
               -- the step 4 clocks before the start bit's step + 2 (cnt = 3). Otherwise one
               -- byte ahead (the QNICE buffer: 2 clocks).
               if cnt < 3 then
                  src_idx <= (others => '0');
               elsif tick = '1' and cnt = 3 and wr2 = '1' then
                  src_idx <= to_unsigned(1, 9);
               end if;
               if e_room = '0' then
                  run <= '0';
                  cnt <= 0;
                  if preempt = '1' then
                     st <= N_STOP;                 -- (op_multi: a single never pauses)
                  end if;
               else
                  run <= '1';
                  if tick = '1' then
                     if cnt = 4 then
                        dat_nx    <= "0000";       -- the start bit goes out next
                        dat_oe_nx <= "1111";
                        bidx      <= (others => '0');
                        crc_l     <= (others => '0');
                        st        <= N_WR_DATA;
                     else
                        cnt <= cnt + 1;
                     end if;
                  end if;
               end if;

            when N_WR_DATA =>
               if tick = '1' then
                  if bidx(0) = '0' then
                     nib     := src_data(7 downto 4);
                     txbyte  <= src_data;
                     if wr2 = '1' then
                        src_idx <= bidx(9 downto 1) + 2;
                     else
                        src_idx <= bidx(9 downto 1) + 1;
                     end if;
                  else
                     nib := txbyte(3 downto 0);
                  end if;
                  dat_nx <= nib;
                  crc := crc_l;
                  for l in 0 to 3 loop
                     crc(16*l+15 downto 16*l) := crc16_bit(crc_l(16*l+15 downto 16*l), nib(l));
                  end loop;
                  crc_l <= crc;
                  if bidx = 1023 then
                     cnt <= 0;
                     st  <= N_WR_CRC;
                  end if;
                  bidx <= bidx + 1;
               end if;

            when N_WR_CRC =>
               if tick = '1' then
                  for l in 0 to 3 loop
                     dat_nx(l) <= crc_l(16*l+15);
                     crc_l(16*l+15 downto 16*l) <= crc_l(16*l+14 downto 16*l) & '0';
                  end loop;
                  if cnt = 15 then
                     st <= N_WR_END;
                  else
                     cnt <= cnt + 1;
                  end if;
               end if;

            when N_WR_END =>                       -- the end bit
               if tick = '1' then
                  dat_nx <= "1111";
                  cnt    <= 0;
                  skip   <= 2;
                  st     <= N_WR_STAT;
               end if;

            when N_WR_STAT =>                      -- the CRC status token: 0 sss 1
               if tick = '1' then
                  dat_oe_nx <= "0000";
                  if skip /= 0 then
                     skip <= skip - 1;
                  elsif dat_in(0) = '0' then
                     cnt <= 0;
                     st  <= N_WR_STAT_BITS;
                  elsif cnt = 16 then
                     st <= S_FAIL;
                  else
                     cnt <= cnt + 1;
                  end if;
               end if;

            when N_WR_STAT_BITS =>
               if tick = '1' then
                  stat <= stat(1 downto 0) & dat_in(0);
                  if cnt = 2 then
                     cnt <= 0;
                     if (stat(1 downto 0) & dat_in(0)) /= "010" then
                        st <= S_FAIL;
                     else
                        c_ok   <= '1';
                        ret_st <= N_WR_NEXT;
                        st     <= N_BUSY0;
                     end if;
                  else
                     cnt <= cnt + 1;
                  end if;
               end if;

            when N_WR_NEXT =>
               if c_ok = '0' then
                  st <= S_FAIL;
               else
                  op_done <= op_done + 1;
                  blk_k   <= blk_k + 1;
                  e_blk   <= op_eng;
                  cnt     <= 0;
                  if op_multi = '0' then
                     st <= S_DISPATCH;
                  elsif blk_k + 1 = op_count or preempt = '1' then
                     st <= N_STOP;
                  else
                     st <= N_WR_GAP;
                  end if;
               end if;

            ---------------------------------------------------------------------------
            -- SPI. Bytes go out MSB first: at every step with ld (bit phase 7) the
            -- byte in txv is loaded (FF unless the state sets another). A received
            -- byte completes at the step after a load (p_new); the byte that completes
            -- right after byte n was loaded is byte n - 1.
            ---------------------------------------------------------------------------

            when P_PRE =>                          -- 80 clocks with CS and MOSI high
               if tick = '1' then
                  if cnt = 80 then
                     cnt   <= 0;
                     istep <= 0;
                     tries <= 0;
                     st    <= P_INIT;
                  else
                     cnt <= cnt + 1;
                  end if;
               end if;

            when P_INIT =>
               case istep is
                  when 0 =>
                     if ld then
                        cs_v  := '0';
                        istep <= 1;
                        do_cmd(0, x"00000000", R_R1, P_INIT);
                     end if;
                  when 1 =>
                     if c_ok = '1' and p_r1 = x"01" then
                        istep <= 2;
                        do_cmd(8, x"000001AA", R_R7, P_INIT);
                     elsif tries = 9 then
                        istep <= 30;
                     else
                        tries <= tries + 1;
                        do_cmd(0, x"00000000", R_R1, P_INIT);
                     end if;
                  when 2 =>
                     tmo <= C_TMO_INIT;
                     if c_ok = '1' and p_r1 = x"01" and p_ext(11 downto 0) = x"1AA" then
                        v2    <= '1';
                        istep <= 3;
                     elsif c_ok = '1' and p_r1(2) = '1' then
                        v2    <= '0';              -- illegal command: version 1
                        istep <= 3;
                     else
                        istep <= 30;
                     end if;
                  when 3 =>
                     istep <= 4;
                     do_cmd(55, x"00000000", R_R1, P_INIT);
                  when 4 =>
                     istep <= 5;
                     if v2 = '1' then
                        do_cmd(41, x"40000000", R_R1, P_INIT);
                     else
                        do_cmd(41, x"00000000", R_R1, P_INIT);
                     end if;
                  when 5 =>
                     if c_ok = '1' and p_r1 = x"00" then
                        istep <= 6;
                     elsif c_ok = '1' and p_r1 = x"01" and tmo /= 0 then
                        istep <= 3;
                     else
                        istep <= 30;
                     end if;
                  when 6 =>
                     if v2 = '1' then
                        istep <= 7;
                        do_cmd(58, x"00000000", R_R3, P_INIT);
                     else
                        hc    <= '0';
                        istep <= 8;
                     end if;
                  when 7 =>
                     if c_ok = '1' and p_r1 = x"00" then
                        hc    <= p_ext(30);
                        istep <= 8;
                     else
                        istep <= 30;
                     end if;
                  when 8 =>                        -- CRC on for all data transfers
                     istep <= 9;
                     do_cmd(59, x"00000001", R_R1, P_INIT);
                  when 9 =>
                     if c_ok = '1' and p_r1 = x"00" then
                        if hc = '0' then
                           istep <= 10;
                           do_cmd(16, x"00000200", R_R1, P_INIT);
                        else
                           istep <= 11;
                        end if;
                     else
                        istep <= 30;
                     end if;
                  when 10 =>
                     if c_ok = '1' and p_r1 = x"00" then
                        istep <= 11;
                     else
                        istep <= 30;
                     end if;
                  when 11 =>
                     if hc = '1' then
                        ctype <= "11";
                     elsif v2 = '1' then
                        ctype <= "10";
                     else
                        ctype <= "01";
                     end if;
                     if slow = '1' then
                        half <= G_HALF_SLOW;
                     else
                        half <= G_HALF_FAST;
                     end if;
                     ready  <= '1';
                     istep  <= 12;
                     cnt    <= 0;
                     ret_st <= P_INIT;
                     st     <= P_DESEL;
                  when 12 =>
                     if retrying = '1' then
                        retrying <= '0';
                        st       <= S_DISPATCH;    -- resume the operation
                     else
                        q_busy <= '0';
                        st     <= S_IDLE;
                     end if;
                  when others =>                   -- 30: no card / init failed
                     ready     <= '0';
                     ctype     <= "00";
                     cs_v        := '1';
                     run       <= '0';
                     q_busy    <= '0';
                     q_error   <= '1';
                     q_errcode <= C_ERR_INIT;
                     if retrying = '1' and op_eng = '1' then
                        e_busy  <= '0';
                        e_done  <= '1';
                        e_error <= '1';
                     end if;
                     retrying <= '0';
                     st       <= S_IDLE;
               end case;

            -- SPI command: FF, 6 command bytes, then FF while looking for R1 (bit 7 =
            -- 0); R3/R7 add 4 bytes, R1b a busy phase
            when P_CMD =>
               if ld then
                  if cnt /= 0 then
                     txv     := c_frame(47 downto 40);
                     c_frame <= c_frame(39 downto 0) & x"FF";
                  end if;
                  if cnt = 6 then
                     -- the last command byte is being loaded: look for R1 from now on.
                     -- The next two completed bytes were sent during command bytes 5
                     -- and 6 (CMD12: plus the stuff byte) and are skipped.
                     p_wait <= 0;
                     p_new  <= '0';
                     st     <= P_RESP;
                  end if;
                  cnt <= cnt + 1;
               end if;

            when P_RESP =>
               if p_new = '1' then
                  p_new <= '0';
                  if p_wait < 2 or (c_idx = 12 and p_wait < 3) then
                     p_wait <= p_wait + 1;
                  elsif p_byte(7) = '0' then
                     p_r1  <= p_byte;
                     c_ok  <= '1';
                     cnt   <= 0;
                     p_ext <= (others => '0');
                     if c_resp = R_R3 or c_resp = R_R7 then
                        st <= P_RESP_X;
                     elsif c_resp = R_R1B then
                        tmo <= C_TMO_BUSY;
                        st  <= P_BUSY;
                     else
                        st <= ret_st;
                     end if;
                  elsif p_wait = 12 then
                     c_ok <= '0';
                     p_r1 <= x"FF";
                     st   <= ret_st;
                  else
                     p_wait <= p_wait + 1;
                  end if;
               end if;

            when P_RESP_X =>                       -- four more bytes (OCR / R7)
               if p_new = '1' then
                  p_new <= '0';
                  p_ext <= p_ext(23 downto 0) & p_byte;
                  if cnt = 3 then
                     st <= ret_st;
                  else
                     cnt <= cnt + 1;
                  end if;
               end if;

            when P_BUSY =>                         -- until MISO is high again
               if p_new = '1' then
                  p_new <= '0';
                  if p_byte /= x"00" then
                     st <= ret_st;
                  end if;
               end if;
               if tmo = 0 then
                  c_ok <= '0';
                  st   <= ret_st;
               end if;

            -- deselect: CS high for two bytes, then return
            when P_DESEL =>
               run <= '1';
               if ld then
                  cs_v := '1';
                  if cnt = 2 then
                     cnt <= 0;
                     st  <= ret_st;
                  else
                     cnt <= cnt + 1;
                  end if;
               end if;

            -- end of a transfer: deselect, then go on (p_ok) or fail
            when P_FIN =>
               cnt    <= 0;
               ret_st <= P_FIN2;
               st     <= P_DESEL;

            when P_FIN2 =>
               if p_ok = '1' then
                  st <= S_DISPATCH;
               else
                  st <= S_FAIL;
               end if;

            ---------------------------------------------------------------------------
            -- SPI: read blocks
            ---------------------------------------------------------------------------

            when P_RD_CMD =>
               if ld then                          -- CS low from a byte boundary on
                  cs_v := '0';
                  if op_multi = '1' then
                     do_cmd(18, card_addr(op_addr, blk_k, hc), R_R1, P_RD_RESP);
                  else
                     do_cmd(17, card_addr(op_addr, blk_k, hc), R_R1, P_RD_RESP);
                  end if;
               end if;

            when P_RD_RESP =>
               if c_ok = '0' or p_r1 /= x"00" then
                  p_ok <= '0';
                  st   <= P_RD_END;
               else
                  tmo <= C_TMO_READ;
                  st  <= P_RD_TOKEN;
               end if;

            when P_RD_TOKEN =>
               -- flow control at byte boundaries: no clock while block blk_k has no room
               if e_room = '0' then
                  if ld then
                     run <= '0';
                  elsif run = '0' then
                     tmo <= C_TMO_READ;
                     if preempt = '1' then
                        p_ok <= '1';
                        st   <= P_RD_END;
                     end if;
                  end if;
               else
                  run <= '1';
               end if;
               if p_new = '1' then
                  p_new <= '0';
                  if p_byte = x"FE" then
                     bidx  <= (others => '0');
                     crc_l <= (others => '0');
                     st    <= P_RD_DATA;
                  elsif p_byte(7 downto 4) = "0000" then
                     p_ok <= '0';                  -- data error token
                     st   <= P_RD_END;
                  end if;
               end if;
               if tmo = 0 and run = '1' then
                  p_ok <= '0';
                  st   <= P_RD_END;
               end if;

            when P_RD_DATA =>                      -- 512 data bytes, 2 CRC bytes
               if p_new = '1' then
                  p_new <= '0';
                  if bidx < 512 then
                     crc_l(15 downto 0) <= crc16_byte(crc_l(15 downto 0), p_byte);
                     if op_eng = '1' then
                        e_rd_we   <= '1';
                        e_rd_idx  <= bidx(8 downto 0);
                        e_rd_data <= p_byte;
                     else
                        cb_we   <= '1';
                        cb_addr <= bidx(8 downto 0);
                        cb_data <= p_byte;
                     end if;
                  else
                     crc_rx(15 downto 0) <= crc_rx(7 downto 0) & p_byte;
                  end if;
                  if bidx = 513 then
                     if (crc_rx(7 downto 0) & p_byte) /= crc_l(15 downto 0) then
                        p_ok <= '0';
                        st   <= P_RD_END;
                     else
                        st <= P_RD_NEXT;
                     end if;
                  end if;
                  bidx <= bidx + 1;
               end if;

            when P_RD_NEXT =>
               op_done <= op_done + 1;
               blk_k   <= blk_k + 1;
               e_blk   <= op_eng;
               p_ok    <= '1';
               if op_multi = '1' and blk_k + 1 /= op_count and preempt = '0' then
                  tmo <= C_TMO_READ;
                  st  <= P_RD_TOKEN;
               else
                  st <= P_RD_END;
               end if;

            -- end of a read: CMD12 for a multi-block read, then deselect
            when P_RD_END =>
               run <= '1';
               if op_multi = '1' then
                  do_cmd(12, x"00000000", R_R1B, P_RD_END2);
               else
                  st <= P_FIN;
               end if;

            when P_RD_END2 =>
               if c_ok = '0' then
                  p_ok <= '0';
               end if;
               st <= P_FIN;

            ---------------------------------------------------------------------------
            -- SPI: write blocks
            ---------------------------------------------------------------------------

            when P_WR_CMD =>
               if ld then
                  cs_v := '0';
                  if op_multi = '1' then
                     do_cmd(25, card_addr(op_addr, blk_k, hc), R_R1, P_WR_RESP);
                  else
                     do_cmd(24, card_addr(op_addr, blk_k, hc), R_R1, P_WR_RESP);
                  end if;
               end if;

            when P_WR_RESP =>
               if c_ok = '0' or p_r1 /= x"00" then
                  p_ok <= '0';
                  st   <= P_WR_END;
               else
                  cnt <= 0;
                  st  <= P_WR_TOKEN;
               end if;

            when P_WR_TOKEN =>
               -- (flow control) one FF, then the start token
               src_idx <= (others => '0');
               if e_room = '0' then
                  if ld then
                     run <= '0';
                  elsif run = '0' and preempt = '1' then
                     p_ok <= '1';
                     st   <= P_WR_END;
                  end if;
               else
                  run <= '1';
                  if ld then
                     if cnt = 1 then
                        if op_multi = '1' then
                           txv := x"FC";
                        else
                           txv := x"FE";
                        end if;
                        bidx  <= (others => '0');
                        crc_l <= (others => '0');
                        st    <= P_WR_DATA;
                     else
                        cnt <= cnt + 1;
                     end if;
                  end if;
               end if;

            when P_WR_DATA =>                      -- 512 bytes, 2 CRC bytes
               if ld then
                  if bidx < 512 then
                     txv := src_data;
                     crc_l(15 downto 0) <= crc16_byte(crc_l(15 downto 0), src_data);
                     src_idx <= bidx(8 downto 0) + 1;
                  elsif bidx = 512 then
                     txv := crc_l(15 downto 8);
                  else
                     txv := crc_l(7 downto 0);
                     -- the data response is the byte right after this one: the next
                     -- two completed bytes (during CRC bytes 1 and 2) are skipped
                     p_wait <= 0;
                     p_new  <= '0';
                     cnt    <= 0;
                     tmo    <= C_TMO_BUSY;
                     st     <= P_WR_DRESP;
                  end if;
                  bidx <= bidx + 1;
               end if;

            when P_WR_DRESP =>
               -- data response xxx0sss1, then busy (00) until a non-zero byte
               if p_new = '1' then
                  p_new <= '0';
                  if cnt = 0 then
                     if p_wait < 2 then
                        p_wait <= p_wait + 1;
                     elsif p_byte(4) = '0' and p_byte(0) = '1' then
                        if p_byte(3 downto 1) = "010" then
                           cnt <= 1;
                        else
                           p_ok <= '0';
                           st   <= P_WR_END;
                        end if;
                     elsif p_wait = 10 then
                        p_ok <= '0';
                        st   <= P_WR_END;
                     else
                        p_wait <= p_wait + 1;
                     end if;
                  elsif p_byte /= x"00" then
                     st <= P_WR_NEXT;
                  end if;
               end if;
               if tmo = 0 then
                  p_ok <= '0';
                  st   <= P_WR_END;
               end if;

            when P_WR_NEXT =>
               op_done <= op_done + 1;
               blk_k   <= blk_k + 1;
               e_blk   <= op_eng;
               p_ok    <= '1';
               cnt     <= 0;
               if op_multi = '1' and blk_k + 1 /= op_count and preempt = '0' then
                  st <= P_WR_TOKEN;
               else
                  istep <= 0;
                  st    <= P_WR_END;
               end if;

            -- end of a write: a multi-block write ends with the stop token FD, one
            -- byte, and the busy; then deselect
            when P_WR_END =>
               run <= '1';
               if op_multi = '0' then
                  st <= P_FIN;
               elsif istep = 0 then
                  if ld then
                     txv    := x"FD";
                     istep  <= 1;
                     p_wait <= 0;
                     p_new  <= '0';
                     tmo    <= C_TMO_BUSY;
                  end if;
               else
                  -- skip the bytes completing during FD and the one after it
                  if p_new = '1' then
                     p_new <= '0';
                     if p_wait < 3 then
                        p_wait <= p_wait + 1;
                     elsif p_byte /= x"00" then
                        istep <= 0;
                        st    <= P_FIN;
                     end if;
                  end if;
                  if tmo = 0 then
                     p_ok  <= '0';
                     istep <= 0;
                     st    <= P_FIN;
                  end if;
               end if;

         end case;
         cs <= cs_v;

         ---------------------------------------------------------------------------------
         -- SPI byte machinery (after the case: a state's p_new <= '0' never hides a byte,
         -- bytes complete at least 8 steps apart)
         ---------------------------------------------------------------------------------
         if native = '0' and tick = '1' then
            -- transmit: load a byte at phase 7, else shift
            if ld then
               cmd_nx <= txv(7);
               p_txsr <= txv(6 downto 0) & '1';
            else
               cmd_nx <= p_txsr(7);
               p_txsr <= p_txsr(6 downto 0) & '1';
            end if;
            cmd_oe_nx <= '1';
            dat_nx    <= cs_v & "111";
            dat_oe_nx <= "1000";
            -- receive: MISO of the previous cycle (its bit phase is ph - 1)
            p_rxsr <= p_rxsr(6 downto 0) & dat_in(0);
            if ph = 0 then
               p_byte <= p_rxsr(6 downto 0) & dat_in(0);
               p_new  <= '1';
            end if;
         end if;

         ---------------------------------------------------------------------------------
         -- reset (QNICE reset, no card in the slot)
         ---------------------------------------------------------------------------------
         if reset_i = '1' then
            st         <= S_RESET;
            run        <= '0';
            clk_r      <= '1';
            clk_q      <= '1';
            half_cnt   <= 0;
            q_seen_rst <= q_req_rst;
            q_seen_rd  <= q_req_rd;
            q_seen_wr  <= q_req_wr;
            q_error    <= '0';
            q_errcode  <= (others => '0');
            q_busy     <= '1';
            op_try     <= 0;
            retries    <= (others => '0');
            if e_busy = '1' or e_pend = '1' then
               e_done  <= '1';
               e_error <= '1';
            end if;
            e_busy     <= '0';
            e_pend     <= '0';
         end if;
      end if;
   end process ctrl;

end architecture synthesis;
