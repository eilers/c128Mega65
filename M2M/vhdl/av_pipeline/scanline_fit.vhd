----------------------------------------------------------------------------------
-- MiSTer2MEGA65 Framework
--
-- CRT emulation on HDMI: scale the picture by a whole number of HDMI rows per
-- input line, so the polyphase scanline filter lands on the same spots of every
-- line. A stretch like 720/270 makes the scanlines beat into bands.
--
-- Lines that do not fit are cropped evenly from the top and bottom, and the
-- width keeps the picture's shape. If that is wider than the HDMI mode, the
-- picture fills the width and the sides are trimmed in proportion.
--
-- Runs on the input video clock: it measures the picture, crops it by blanking
-- and calculates ascal's output rectangle once per frame. Window and rectangle
-- change together at the start of a frame.
--
-- MiSTer2MEGA65 done by sy2002 and MJoergen in 2022 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity scanline_fit is
   port (
      video_clk_i       : in  std_logic;
      video_enable_i    : in  std_logic;                  -- CRT emulation is on

      -- The selected HDMI mode
      video_mode_i      : in  natural range 0 to 7;       -- index, passed through to video_mode_o
      video_hdisp_i     : in  natural range 0 to 4095;    -- active width
      video_vdisp_i     : in  natural range 0 to 4095;    -- active height
      video_scale_i     : in  natural range 1 to 15;      -- HDMI rows per input line
      video_lines_i     : in  natural range 0 to 4095;    -- most input lines shown: vdisp / scale
      video_hfactor_i   : in  natural range 0 to 65535;   -- output width per input line, 8.8 fixed point

      -- Input video stream
      video_ce_i        : in  std_logic;
      video_red_i       : in  std_logic_vector(7 downto 0);
      video_green_i     : in  std_logic_vector(7 downto 0);
      video_blue_i      : in  std_logic_vector(7 downto 0);
      video_hs_i        : in  std_logic;
      video_vs_i        : in  std_logic;
      video_hblank_i    : in  std_logic;
      video_vblank_i    : in  std_logic;

      -- Output video stream, one clock later and cropped
      video_ce_o        : out std_logic;
      video_red_o       : out std_logic_vector(7 downto 0);
      video_green_o     : out std_logic_vector(7 downto 0);
      video_blue_o      : out std_logic_vector(7 downto 0);
      video_hs_o        : out std_logic;
      video_vs_o        : out std_logic;
      video_hblank_o    : out std_logic;
      video_vblank_o    : out std_logic;

      -- ascal's output rectangle, for the mode in video_mode_o. Use it while video_valid_o is '1'.
      video_valid_o     : out std_logic;
      video_mode_o      : out natural range 0 to 7;
      video_hmin_o      : out natural range 0 to 4095;
      video_hmax_o      : out natural range 0 to 4095;
      video_vmin_o      : out natural range 0 to 4095;
      video_vmax_o      : out natural range 0 to 4095
   );
end entity scanline_fit;

architecture synthesis of scanline_fit is

   -- The picture as measured on the input
   signal de_d        : std_logic := '0';
   signal vs_d        : std_logic := '0';
   signal hcpt        : natural range 0 to 4095 := 0;
   signal vcpt        : natural range 0 to 4095 := 0;
   signal vcpt_last   : natural range 0 to 4095 := 0;
   signal width       : natural range 0 to 4095 := 0;
   signal lines       : natural range 0 to 4095 := 0;

   -- Crop window of the current frame
   signal crop_on     : std_logic := '0';
   signal top         : natural range 0 to 4095 := 0;
   signal bottom      : natural range 0 to 4095 := 0;   -- first line below the window
   signal left        : natural range 0 to 4095 := 0;
   signal right       : natural range 0 to 4095 := 0;   -- first pixel right of the window

   -- Calculation, run once per frame. Its results take effect at the next frame.
   type state_t is (IDLE_ST, SIZE_ST, WIDTH_ST, DIV_ST, TRIM_ST, RECT_ST);
   signal state       : state_t := IDLE_ST;
   signal mode        : natural range 0 to 7 := 0;
   signal cwidth      : natural range 0 to 4095 := 0;
   signal shown       : natural range 0 to 4095 := 0;
   signal hwant       : natural range 0 to 65535 := 0;  -- width that keeps the shape
   signal vsize       : natural range 0 to 65535 := 0;
   signal hsize       : natural range 0 to 65535 := 0;
   signal cols        : natural range 0 to 4095 := 0;
   signal div_num     : unsigned(27 downto 0) := (others => '0');
   signal div_den     : unsigned(27 downto 0) := (others => '0');
   signal div_bit     : natural range 0 to 11 := 0;

   signal next_on     : std_logic := '0';
   signal next_top    : natural range 0 to 4095 := 0;
   signal next_bottom : natural range 0 to 4095 := 0;
   signal next_left   : natural range 0 to 4095 := 0;
   signal next_right  : natural range 0 to 4095 := 0;
   signal next_mode   : natural range 0 to 7 := 0;
   signal next_hmin   : natural range 0 to 4095 := 0;
   signal next_hmax   : natural range 0 to 4095 := 0;
   signal next_vmin   : natural range 0 to 4095 := 0;
   signal next_vmax   : natural range 0 to 4095 := 0;

   -- Rectangle of the current frame
   signal valid       : std_logic := '0';
   signal valid_mode  : natural range 0 to 7 := 0;
   signal hmin        : natural range 0 to 4095 := 0;
   signal hmax        : natural range 0 to 4095 := 0;
   signal vmin        : natural range 0 to 4095 := 0;
   signal vmax        : natural range 0 to 4095 := 0;

begin

   p_fit : process (video_clk_i)
      variable de     : std_logic;
      variable in_win : boolean;
   begin
      if rising_edge(video_clk_i) then
         de := not (video_hblank_i or video_vblank_i);

         -- hcpt and vcpt are the position of the pixel on the input right now
         in_win := vcpt >= top and vcpt < bottom and hcpt >= left and hcpt < right;

         video_ce_o     <= video_ce_i;
         video_red_o    <= video_red_i;
         video_green_o  <= video_green_i;
         video_blue_o   <= video_blue_i;
         video_hs_o     <= video_hs_i;
         video_vs_o     <= video_vs_i;
         video_vblank_o <= video_vblank_i;
         video_hblank_o <= video_hblank_i;
         if crop_on = '1' and not in_win then
            video_hblank_o <= '1';
         end if;

         if video_ce_i = '1' then
            de_d <= de;
            vs_d <= video_vs_i;

            if de = '1' and hcpt < 4095 then
               hcpt <= hcpt + 1;
            end if;
            if de = '0' and de_d = '1' then
               width <= hcpt;
               hcpt  <= 0;
               if vcpt < 4095 then
                  vcpt <= vcpt + 1;
               end if;
            end if;

            if video_vs_i = '1' and vs_d = '0' then
               -- The larger of the last two frames, so alternating fields don't flip the size
               lines     <= maximum(vcpt, vcpt_last);
               vcpt_last <= vcpt;
               vcpt      <= 0;

               crop_on       <= next_on;
               top           <= next_top;
               bottom        <= next_bottom;
               left          <= next_left;
               right         <= next_right;
               valid         <= next_on;
               valid_mode    <= next_mode;
               hmin          <= next_hmin;
               hmax          <= next_hmax;
               vmin          <= next_vmin;
               vmax          <= next_vmax;

               state <= SIZE_ST;
            end if;
         end if;

         case state is
            when IDLE_ST =>
               null;

            when SIZE_ST =>
               mode   <= video_mode_i;
               cwidth <= width;
               shown  <= minimum(lines, video_lines_i);
               hwant  <= minimum((video_hfactor_i * lines + 128) / 256, 65535);
               state <= WIDTH_ST;

            when WIDTH_ST =>
               -- Centre the lines that are shown
               vsize       <= video_scale_i * shown;
               next_top    <= (lines - shown) / 2;
               next_bottom <= (lines - shown) / 2 + shown;
               if hwant <= video_hdisp_i then
                  hsize      <= hwant;
                  next_left  <= 0;
                  next_right <= cwidth;
                  state      <= RECT_ST;
               else
                  -- Too wide: fill the width and keep cols = width * hdisp / hwant pixels
                  hsize   <= video_hdisp_i;
                  div_num <= to_unsigned(cwidth * video_hdisp_i, 28);
                  div_den <= shift_left(to_unsigned(hwant, 28), 11);
                  cols    <= 0;
                  div_bit <= 11;
                  state   <= DIV_ST;
               end if;

            when DIV_ST =>
               -- One quotient bit per clock, most significant first
               if div_num >= div_den then
                  div_num <= div_num - div_den;
                  cols    <= cols * 2 + 1;
               else
                  cols    <= cols * 2;
               end if;
               div_den <= shift_right(div_den, 1);
               if div_bit = 0 then
                  state <= TRIM_ST;
               else
                  div_bit <= div_bit - 1;
               end if;

            when TRIM_ST =>
               next_left  <= (cwidth - cols) / 2;
               next_right <= (cwidth - cols) / 2 + cols;
               state      <= RECT_ST;

            when RECT_ST =>
               next_mode <= mode;
               if video_enable_i = '1' and lines >= 16 and cwidth >= 16 and
                  vsize >= 2 and vsize <= video_vdisp_i and hsize >= 2 and hsize <= video_hdisp_i then
                  next_on   <= '1';
                  next_hmin <= (video_hdisp_i - hsize) / 2;
                  next_hmax <= (video_hdisp_i - hsize) / 2 + hsize - 1;
                  next_vmin <= (video_vdisp_i - vsize) / 2;
                  next_vmax <= (video_vdisp_i - vsize) / 2 + vsize - 1;
               else
                  next_on <= '0';
               end if;
               state <= IDLE_ST;
         end case;
      end if;
   end process p_fit;

   video_valid_o <= valid;
   video_mode_o  <= valid_mode;
   video_hmin_o  <= hmin;
   video_hmax_o  <= hmax;
   video_vmin_o  <= vmin;
   video_vmax_o  <= vmax;

end architecture synthesis;
