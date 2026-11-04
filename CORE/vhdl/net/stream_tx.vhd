----------------------------------------------------------------------------------
-- MEGA65 port of the C128 core
--
-- Gideon's U64 data streams 0 (VIC video) and 1 (audio), as UDP frames for the MAC.
--
-- Video: the VIC's 384x272 PAL window (rasterX 0..383, raster 16..287: 32 pixels and 35
-- lines of border around the display window, as VICE's test images), 4 lines a packet,
-- 4-bit colour indices with the left pixel in the low nibble. Each packet carries his
-- 12-byte header: sequence, frame, line (bit 15 on the frame's last), 384, 4, 4, 0.
-- Audio: what HDMI gets, sampled at the main clock x 15/9856 (the CPU clock x 15/308, his
-- 47983 Hz), 192 stereo frames of s16le a packet after a 2-byte sequence.
--
-- The firmware writes each stream's 42-byte Ethernet/IP/UDP header, lengths and checksum
-- final; it goes out first, as written. Sequence and frame count from 0 at enable. A group
-- of lines or samples whose slot is still waiting is dropped and counted, its sequence
-- number skipped so a receiver sees the gap.
--
-- Two slots a stream in LUTRAM, written on the main clock and read on clk_i; a toggle per
-- slot says it is full, another that it went out.
--
-- GPL-3.0-or-later
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity stream_tx is
   port (
      -- C128 side
      main_clk_i  : in  std_logic;
      vic_ce_i    : in  std_logic;                       -- a pixel: index, x, y as registered with it
      vic_index_i : in  unsigned(3 downto 0);
      vic_x_i     : in  unsigned(9 downto 0);
      vic_y_i     : in  unsigned(8 downto 0);
      audio_l_i   : in  signed(15 downto 0);
      audio_r_i   : in  signed(15 downto 0);

      -- Network side
      clk_i       : in  std_logic;
      rst_i       : in  std_logic;
      enable_i    : in  std_logic_vector(1 downto 0);    -- 0 video, 1 audio
      busy_o      : out std_logic_vector(1 downto 0);    -- a packet of the stream is going out
      hdr_we_i    : in  std_logic;
      hdr_addr_i  : in  std_logic_vector(5 downto 0);    -- stream (5), word (4..0)
      hdr_data_i  : in  std_logic_vector(15 downto 0);   -- bytes 2w (7..0) and 2w+1 (15..8)
      vsent_o     : out unsigned(15 downto 0);
      asent_o     : out unsigned(15 downto 0);
      vdrop_o     : out unsigned(15 downto 0);
      adrop_o     : out unsigned(15 downto 0);

      -- Frames for the MAC, without preamble and FCS
      m_valid_o   : out std_logic;
      m_ready_i   : in  std_logic;
      m_data_o    : out std_logic_vector(7 downto 0);
      m_last_o    : out std_logic
   );
end entity stream_tx;

architecture synthesis of stream_tx is

   constant C_WIDTH     : natural := 384;
   constant C_Y_FIRST   : natural := 16;     -- PAL
   constant C_LINES     : natural := 272;
   constant C_PAYLOAD   : natural := 768;
   constant C_HDR_BYTES : natural := 42;
   constant C_SAMPLES   : natural := 192;
   constant C_ACC_STEP  : natural := 15;
   constant C_ACC_WRAP  : natural := 308 * 32;   -- main clock = 32 x CPU clock

   type vram_t is array (0 to 2 * C_PAYLOAD - 1) of std_logic_vector(7 downto 0);
   type aram_t is array (0 to 2 * C_SAMPLES - 1) of std_logic_vector(31 downto 0);
   type hram_t is array (0 to 63) of std_logic_vector(15 downto 0);
   signal vram : vram_t;
   signal aram : aram_t;
   signal hram : hram_t := (others => (others => '0'));
   attribute ram_style : string;
   attribute ram_style of vram : signal is "distributed";
   attribute ram_style of aram : signal is "distributed";
   attribute ram_style of hram : signal is "distributed";

   type u16_pair is array (0 to 1) of unsigned(15 downto 0);
   type u9_pair  is array (0 to 1) of unsigned(8 downto 0);

   attribute async_reg : string;

   -- Main clock: enables in, toggles out
   signal en_m, en_s : std_logic_vector(1 downto 0) := "00";
   attribute async_reg of en_m, en_s : signal is "true";

   -- Video capture
   signal v_active   : std_logic := '0';
   signal v_slot     : natural range 0 to 1 := 0;
   signal v_store    : std_logic := '0';
   signal v_lo       : std_logic_vector(3 downto 0) := x"0";
   signal v_seq      : unsigned(15 downto 0) := (others => '0');
   signal v_frame    : unsigned(15 downto 0) := (others => '0');
   signal v_fill_tog : std_logic_vector(1 downto 0) := "00";
   signal v_drop_tog : std_logic := '0';
   signal v_tag_seq  : u16_pair := (others => (others => '0'));
   signal v_tag_frm  : u16_pair := (others => (others => '0'));
   signal v_tag_line : u9_pair := (others => (others => '0'));
   signal v_tag_last : std_logic_vector(1 downto 0) := "00";
   signal v_sent_m, v_sent_s : std_logic_vector(1 downto 0) := "00";
   attribute async_reg of v_sent_m, v_sent_s : signal is "true";

   -- Audio capture
   signal a_acc      : natural range 0 to C_ACC_WRAP - 1 := 0;    -- a sample when it wraps
   signal a_active   : std_logic := '0';
   signal a_slot     : natural range 0 to 1 := 0;
   signal a_store    : std_logic := '0';
   signal a_idx      : natural range 0 to C_SAMPLES - 1 := 0;
   signal a_seq      : unsigned(15 downto 0) := (others => '0');
   signal a_fill_tog : std_logic_vector(1 downto 0) := "00";
   signal a_drop_tog : std_logic := '0';
   signal a_tag_seq  : u16_pair := (others => (others => '0'));
   signal a_sent_m, a_sent_s : std_logic_vector(1 downto 0) := "00";
   attribute async_reg of a_sent_m, a_sent_s : signal is "true";

   -- Network side: toggles in
   signal v_fill_m, v_fill_s : std_logic_vector(1 downto 0) := "00";
   signal a_fill_m, a_fill_s : std_logic_vector(1 downto 0) := "00";
   signal drop_m, drop_s     : std_logic_vector(1 downto 0) := "00";   -- video, audio
   attribute async_reg of v_fill_m, v_fill_s, a_fill_m, a_fill_s, drop_m, drop_s : signal is "true";
   signal drop_d             : std_logic_vector(1 downto 0) := "00";
   signal v_sent_tog, a_sent_tog : std_logic_vector(1 downto 0) := "00";

   -- Sender
   type state_t is (S_IDLE, S_HDR, S_SUB, S_PAY);
   signal state     : state_t := S_IDLE;
   signal cur_audio : std_logic := '0';
   signal cur_slot  : natural range 0 to 1 := 0;
   signal tx_vslot  : natural range 0 to 1 := 0;
   signal tx_aslot  : natural range 0 to 1 := 0;
   signal cnt       : natural range 0 to C_PAYLOAD - 1 := 0;
   signal m_valid   : std_logic := '0';
   signal m_data    : std_logic_vector(7 downto 0) := x"00";
   signal m_last    : std_logic := '0';
   signal vsent, asent, vdrop, adrop : unsigned(15 downto 0) := (others => '0');

begin

   -----------------------------------------------------------------------------
   -- C128 side
   -----------------------------------------------------------------------------

   video_proc : process (main_clk_i)
      variable yl     : natural range 0 to 511;
      variable row    : natural range 0 to 3;
      variable active : std_logic;
      variable store  : std_logic;
   begin
      if rising_edge(main_clk_i) then
         en_m     <= enable_i;
         en_s     <= en_m;
         v_sent_m <= v_sent_tog;
         v_sent_s <= v_sent_m;

         if en_s(0) = '0' then
            v_active <= '0';
         elsif vic_ce_i = '1' and vic_y_i >= C_Y_FIRST and vic_y_i < C_Y_FIRST + C_LINES
               and vic_x_i < C_WIDTH then
            yl     := to_integer(vic_y_i) - C_Y_FIRST;
            row    := yl mod 4;
            active := v_active;
            store  := v_store;

            -- A frame starts at its first pixel; the first one after enable counts from 0
            if vic_x_i = 0 and yl = 0 then
               if active = '0' then
                  v_seq   <= (others => '0');
                  v_frame <= (others => '0');
               else
                  v_frame <= v_frame + 1;
               end if;
               active   := '1';
               v_active <= '1';
            end if;

            -- A group of 4 lines goes into the slot if that one has gone out
            if vic_x_i = 0 and row = 0 and active = '1' then
               if v_fill_tog(v_slot) = v_sent_s(v_slot) then
                  store := '1';
               else
                  store := '0';
               end if;
               v_store <= store;
            end if;

            if active = '1' and store = '1' then
               if vic_x_i(0) = '0' then
                  v_lo <= std_logic_vector(vic_index_i);
               else
                  vram(v_slot * C_PAYLOAD + row * (C_WIDTH / 2) + to_integer(vic_x_i(9 downto 1))) <=
                     std_logic_vector(vic_index_i) & v_lo;
               end if;
            end if;

            if vic_x_i = C_WIDTH - 1 and row = 3 and active = '1' then
               if store = '1' then
                  v_tag_seq(v_slot)  <= v_seq;
                  v_tag_frm(v_slot)  <= v_frame;
                  v_tag_line(v_slot) <= to_unsigned(yl - 3, 9);
                  if yl = C_LINES - 1 then
                     v_tag_last(v_slot) <= '1';
                  else
                     v_tag_last(v_slot) <= '0';
                  end if;
                  v_fill_tog(v_slot) <= not v_fill_tog(v_slot);
                  v_slot             <= 1 - v_slot;
               else
                  v_drop_tog <= not v_drop_tog;
               end if;
               v_seq <= v_seq + 1;
            end if;
         end if;
      end if;
   end process video_proc;

   audio_proc : process (main_clk_i)
      variable idx   : natural range 0 to C_SAMPLES - 1;
      variable seq   : unsigned(15 downto 0);
      variable store : std_logic;
   begin
      if rising_edge(main_clk_i) then
         a_sent_m <= a_sent_tog;
         a_sent_s <= a_sent_m;

         if a_acc >= C_ACC_WRAP - C_ACC_STEP then
            a_acc <= a_acc + C_ACC_STEP - C_ACC_WRAP;
         else
            a_acc <= a_acc + C_ACC_STEP;
         end if;

         if en_s(1) = '0' then
            a_active <= '0';
         elsif a_acc >= C_ACC_WRAP - C_ACC_STEP then
            -- The first sample after enable starts a packet and counts from 0
            idx   := a_idx;
            seq   := a_seq;
            store := a_store;
            if a_active = '0' then
               idx := 0;
               seq := (others => '0');
            end if;
            if idx = 0 then
               if a_fill_tog(a_slot) = a_sent_s(a_slot) then
                  store := '1';
               else
                  store := '0';
               end if;
               a_store <= store;
            end if;
            a_active <= '1';

            if store = '1' then
               aram(a_slot * C_SAMPLES + idx) <= std_logic_vector(audio_r_i) & std_logic_vector(audio_l_i);
            end if;

            if idx = C_SAMPLES - 1 then
               a_idx <= 0;
               if store = '1' then
                  a_tag_seq(a_slot)  <= seq;
                  a_fill_tog(a_slot) <= not a_fill_tog(a_slot);
                  a_slot             <= 1 - a_slot;
               else
                  a_drop_tog <= not a_drop_tog;
               end if;
               a_seq <= seq + 1;
            else
               a_idx <= idx + 1;
               a_seq <= seq;
            end if;
         end if;
      end if;
   end process audio_proc;

   -----------------------------------------------------------------------------
   -- Network side
   -----------------------------------------------------------------------------

   hdr_proc : process (clk_i)
   begin
      if rising_edge(clk_i) then
         if hdr_we_i = '1' then
            hram(to_integer(unsigned(hdr_addr_i))) <= hdr_data_i;
         end if;
      end if;
   end process hdr_proc;

   send_proc : process (clk_i)
      variable sb     : std_logic_vector(7 downto 0);
      variable word   : std_logic_vector(15 downto 0);
      variable aword  : std_logic_vector(31 downto 0);
      variable v_pend : std_logic;
      variable a_pend : std_logic;
   begin
      if rising_edge(clk_i) then
         v_fill_m <= v_fill_tog;
         v_fill_s <= v_fill_m;
         a_fill_m <= a_fill_tog;
         a_fill_s <= a_fill_m;
         drop_m   <= a_drop_tog & v_drop_tog;
         drop_s   <= drop_m;
         drop_d   <= drop_s;
         if drop_s(0) /= drop_d(0) then
            vdrop <= vdrop + 1;
         end if;
         if drop_s(1) /= drop_d(1) then
            adrop <= adrop + 1;
         end if;

         -- One step each time the output register is free: a byte, or the next packet
         if m_valid = '0' or m_ready_i = '1' then
            m_valid <= '0';
            m_last  <= '0';
            case state is
               when S_IDLE =>
                  v_pend := v_fill_s(tx_vslot) xor v_sent_tog(tx_vslot);
                  a_pend := a_fill_s(tx_aslot) xor a_sent_tog(tx_aslot);
                  cnt    <= 0;
                  -- Video first: its slot fills every 256 us, audio's every 4 ms
                  if v_pend = '1' and enable_i(0) = '1' then
                     cur_audio <= '0';
                     cur_slot  <= tx_vslot;
                     state     <= S_HDR;
                  elsif v_pend = '1' then                    -- switched off: drop what waits
                     v_sent_tog(tx_vslot) <= not v_sent_tog(tx_vslot);
                     tx_vslot <= 1 - tx_vslot;
                  elsif a_pend = '1' and enable_i(1) = '1' then
                     cur_audio <= '1';
                     cur_slot  <= tx_aslot;
                     state     <= S_HDR;
                  elsif a_pend = '1' then
                     a_sent_tog(tx_aslot) <= not a_sent_tog(tx_aslot);
                     tx_aslot <= 1 - tx_aslot;
                  end if;

               when S_HDR =>
                  if cur_audio = '1' then
                     word := hram(32 + cnt / 2);
                  else
                     word := hram(cnt / 2);
                  end if;
                  if cnt mod 2 = 1 then
                     m_data <= word(15 downto 8);
                  else
                     m_data <= word(7 downto 0);
                  end if;
                  m_valid <= '1';
                  if cnt = C_HDR_BYTES - 1 then
                     cnt   <= 0;
                     state <= S_SUB;
                  else
                     cnt <= cnt + 1;
                  end if;

               when S_SUB =>
                  if cur_audio = '1' then
                     if cnt = 0 then
                        sb := std_logic_vector(a_tag_seq(cur_slot)(7 downto 0));
                     else
                        sb := std_logic_vector(a_tag_seq(cur_slot)(15 downto 8));
                     end if;
                  else
                     case cnt is
                        when 0      => sb := std_logic_vector(v_tag_seq(cur_slot)(7 downto 0));
                        when 1      => sb := std_logic_vector(v_tag_seq(cur_slot)(15 downto 8));
                        when 2      => sb := std_logic_vector(v_tag_frm(cur_slot)(7 downto 0));
                        when 3      => sb := std_logic_vector(v_tag_frm(cur_slot)(15 downto 8));
                        when 4      => sb := std_logic_vector(v_tag_line(cur_slot)(7 downto 0));
                        when 5      => sb := v_tag_last(cur_slot) & "000000" & v_tag_line(cur_slot)(8);
                        when 6      => sb := std_logic_vector(to_unsigned(C_WIDTH mod 256, 8));
                        when 7      => sb := std_logic_vector(to_unsigned(C_WIDTH / 256, 8));
                        when 8      => sb := x"04";              -- lines per packet
                        when 9      => sb := x"04";              -- bits per pixel
                        when others => sb := x"00";              -- encoding 0
                     end case;
                  end if;
                  m_data  <= sb;
                  m_valid <= '1';
                  if (cur_audio = '1' and cnt = 1) or cnt = 11 then
                     cnt   <= 0;
                     state <= S_PAY;
                  else
                     cnt <= cnt + 1;
                  end if;

               when S_PAY =>
                  if cur_audio = '1' then
                     aword := aram(cur_slot * C_SAMPLES + cnt / 4);
                     case cnt mod 4 is
                        when 0      => m_data <= aword(7 downto 0);
                        when 1      => m_data <= aword(15 downto 8);
                        when 2      => m_data <= aword(23 downto 16);
                        when others => m_data <= aword(31 downto 24);
                     end case;
                  else
                     m_data <= vram(cur_slot * C_PAYLOAD + cnt);
                  end if;
                  m_valid <= '1';
                  if cnt = C_PAYLOAD - 1 then
                     -- The slot is free once its last byte is in the output register
                     m_last <= '1';
                     state  <= S_IDLE;
                     if cur_audio = '1' then
                        a_sent_tog(cur_slot) <= not a_sent_tog(cur_slot);
                        tx_aslot <= 1 - cur_slot;
                        asent    <= asent + 1;
                     else
                        v_sent_tog(cur_slot) <= not v_sent_tog(cur_slot);
                        tx_vslot <= 1 - cur_slot;
                        vsent    <= vsent + 1;
                     end if;
                  else
                     cnt <= cnt + 1;
                  end if;
            end case;
         end if;

         if rst_i = '1' then
            state   <= S_IDLE;
            m_valid <= '0';
            m_last  <= '0';
         end if;
      end if;
   end process send_proc;

   busy_o(0) <= '1' when state /= S_IDLE and cur_audio = '0' else '0';
   busy_o(1) <= '1' when state /= S_IDLE and cur_audio = '1' else '0';

   vsent_o   <= vsent;
   asent_o   <= asent;
   vdrop_o   <= vdrop;
   adrop_o   <= adrop;
   m_valid_o <= m_valid;
   m_data_o  <= m_data;
   m_last_o  <= m_last;

end architecture synthesis;
