----------------------------------------------------------------------------------
-- MiSTer2MEGA65 Framework
--
-- HDMI goes black while the core switches its video source (e.g. 40/80 columns).
--
-- ascal keeps the old picture's size for a while after a switch. The frame cut by the
-- switch gives it a wrong line count, so the next frame is stored at the wrong height:
-- the third frame of the new source is the first good one. scanline_fit crops by the
-- larger of the last two frames and applies that a frame later, so with it the sixth
-- frame is. HDMI shows black from the switch until then and comes back at the start of
-- an HDMI frame.
--
-- MiSTer2MEGA65 done by sy2002 and MJoergen in 2022 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity video_switch_blank is
   port (
      video_clk_i    : in  std_logic;     -- follows the source, like ascal's input
      video_vs_i     : in  std_logic;     -- the vsync ascal sees
      video_fit_i    : in  std_logic;     -- scanline_fit is on
      video_source_i : in  std_logic;     -- which source the core shows (any clock)

      hdmi_clk_i     : in  std_logic;
      hdmi_vs_i      : in  std_logic;     -- ascal's output vsync
      hdmi_blank_o   : out std_logic      -- '1': show black
   );
end entity video_switch_blank;

architecture synthesis of video_switch_blank is

   constant C_FRAMES     : natural := 3;
   constant C_FRAMES_FIT : natural := 6;

   signal source     : std_logic_vector(0 downto 0);
   signal source_d   : std_logic := '0';
   signal vs_d       : std_logic := '0';
   signal frames     : natural range 0 to C_FRAMES_FIT := 0;
   signal video_wait : std_logic := '0';

   signal hdmi_wait  : std_logic_vector(0 downto 0);
   signal hdmi_vs_d  : std_logic := '0';
   signal blank      : std_logic := '0';

begin

   i_cdc_source : entity work.cdc_stable
      generic map (
         G_DATA_SIZE => 1
      )
      port map (
         src_data_i(0) => video_source_i,
         dst_clk_i     => video_clk_i,
         dst_data_o    => source
      ); -- i_cdc_source

   -- Count the new source's frames from the switch
   p_video : process (video_clk_i)
   begin
      if rising_edge(video_clk_i) then
         source_d <= source(0);
         vs_d     <= video_vs_i;

         if source(0) /= source_d then
            if video_fit_i = '1' then
               frames <= C_FRAMES_FIT;
            else
               frames <= C_FRAMES;
            end if;
            video_wait <= '1';
         elsif video_vs_i = '1' and vs_d = '0' and frames /= 0 then
            frames <= frames - 1;
            if frames = 1 then
               video_wait <= '0';
            end if;
         end if;
      end if;
   end process p_video;

   i_cdc_wait : entity work.cdc_stable
      generic map (
         G_DATA_SIZE => 1
      )
      port map (
         src_clk_i     => video_clk_i,
         src_data_i(0) => video_wait,
         dst_clk_i     => hdmi_clk_i,
         dst_data_o    => hdmi_wait
      ); -- i_cdc_wait

   -- Black at once; picture back at an HDMI frame start
   p_hdmi : process (hdmi_clk_i)
   begin
      if rising_edge(hdmi_clk_i) then
         hdmi_vs_d <= hdmi_vs_i;

         if hdmi_wait(0) = '1' then
            blank <= '1';
         elsif hdmi_vs_i = '1' and hdmi_vs_d = '0' then
            blank <= '0';
         end if;
      end if;
   end process p_hdmi;

   hdmi_blank_o <= blank or hdmi_wait(0);

end architecture synthesis;
