-- HDMI CRT emulation: scanline_fit crops the picture to whole HDMI rows per line and
-- calculates ascal's output rectangle. Expected values come from the RTL's arithmetic.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

library work;
use work.video_modes_pkg.all;

entity tb_scanline_fit is
end entity;

architecture sim of tb_scanline_fit is
   signal clk        : std_logic := '0';
   signal done       : boolean := false;

   signal enable     : std_logic := '0';
   signal mode_index : natural range 0 to 7 := 0;
   signal mode       : hdmi_scanline_mode_t := (HDISP => 1280, VDISP => 720, SCALE => 3, LINES => 240, HFACTOR => 1024);

   signal ce_i       : std_logic := '0';
   signal red_i      : std_logic_vector(7 downto 0) := (others => '0');
   signal green_i    : std_logic_vector(7 downto 0) := (others => '0');
   signal hs_i       : std_logic := '0';
   signal vs_i       : std_logic := '0';
   signal hblank_i   : std_logic := '1';
   signal vblank_i   : std_logic := '1';

   signal ce_o       : std_logic;
   signal red_o      : std_logic_vector(7 downto 0);
   signal green_o    : std_logic_vector(7 downto 0);
   signal hs_o       : std_logic;
   signal vs_o       : std_logic;
   signal hblank_o   : std_logic;
   signal vblank_o   : std_logic;
   signal valid      : std_logic;
   signal valid_mode : natural range 0 to 7;
   signal hmin       : natural range 0 to 4095;
   signal hmax       : natural range 0 to 4095;
   signal vmin       : natural range 0 to 4095;
   signal vmax       : natural range 0 to 4095;

   -- What the last output frame looked like: lines with DE, width of the first one,
   -- and which input pixel (red) and line (green) it starts with
   signal res_lines  : natural := 0;
   signal res_cols   : natural := 0;
   signal res_left   : natural := 0;
   signal res_top    : natural := 0;

   pure function scanline_mode(video_mode : video_modes_t; id : video_mode_type) return hdmi_scanline_mode_t is
   begin
      return make_hdmi_scanline_mode(video_mode, make_hdmi_output_rect(video_mode, id, C_HDMI_FIT_LEGACY), 240);
   end function;
begin
   clk <= not clk after 5 ns when not done else '0';

   dut : entity work.scanline_fit
      port map (
         video_clk_i     => clk,
         video_enable_i  => enable,
         video_mode_i    => mode_index,
         video_hdisp_i   => mode.HDISP,
         video_vdisp_i   => mode.VDISP,
         video_scale_i   => mode.SCALE,
         video_lines_i   => mode.LINES,
         video_hfactor_i => mode.HFACTOR,
         video_ce_i      => ce_i,
         video_red_i     => red_i,
         video_green_i   => green_i,
         video_blue_i    => x"00",
         video_hs_i      => hs_i,
         video_vs_i      => vs_i,
         video_hblank_i  => hblank_i,
         video_vblank_i  => vblank_i,
         video_ce_o      => ce_o,
         video_red_o     => red_o,
         video_green_o   => green_o,
         video_blue_o    => open,
         video_hs_o      => hs_o,
         video_vs_o      => vs_o,
         video_hblank_o  => hblank_o,
         video_vblank_o  => vblank_o,
         video_valid_o   => valid,
         video_mode_o    => valid_mode,
         video_hmin_o    => hmin,
         video_hmax_o    => hmax,
         video_vmin_o    => vmin,
         video_vmax_o    => vmax
      );

   monitor : process (clk)
      variable de, de_d, vs_d : std_logic := '0';
      variable lines, cols, cols0, left, top : natural := 0;
   begin
      if rising_edge(clk) and ce_o = '1' then
         de := not (hblank_o or vblank_o);
         if de = '1' then
            if de_d = '0' and lines = 0 then
               left := to_integer(unsigned(red_o));
               top  := to_integer(unsigned(green_o));
            end if;
            cols := cols + 1;
         elsif de_d = '1' then
            if lines = 0 then
               cols0 := cols;
            end if;
            lines := lines + 1;
            cols  := 0;
         end if;
         if vs_o = '1' and vs_d = '0' then
            res_lines <= lines;
            res_cols  <= cols0;
            res_left  <= left;
            res_top   <= top;
            lines := 0;
         end if;
         de_d := de;
         vs_d := vs_o;
      end if;
   end process monitor;

   stimulus : process
      -- One frame: active lines first, then the vertical sync. A pixel lasts ce_div clocks.
      procedure frame(n_lines, width, ce_div : positive) is
         constant C_HT : positive := width + 120;
         constant C_VT : positive := n_lines + 42;
      begin
         for y in 0 to C_VT - 1 loop
            for x in 0 to C_HT - 1 loop
               for c in 0 to ce_div - 1 loop
                  wait until rising_edge(clk);
                  ce_i <= '0';
                  if c = 0 then
                     ce_i     <= '1';
                     hblank_i <= '1';
                     vblank_i <= '1';
                     hs_i     <= '0';
                     vs_i     <= '0';
                     if x >= 60 and x < 60 + width then
                        hblank_i <= '0';
                     end if;
                     if y >= 20 and y < 20 + n_lines then
                        vblank_i <= '0';
                     end if;
                     if x >= width + 80 and x < width + 100 then
                        hs_i <= '1';
                     end if;
                     if y >= n_lines + 30 and y < n_lines + 34 then
                        vs_i <= '1';
                     end if;
                     red_i   <= std_logic_vector(to_unsigned((x - 60) mod 256, 8));
                     green_i <= std_logic_vector(to_unsigned((y - 20) mod 256, 8));
                  end if;
               end loop;
            end loop;
         end loop;
      end procedure;

      procedure check(name : string; n_lines, width, ce_div : positive;
                      e_valid : std_logic; e_hmin, e_hmax, e_vmin, e_vmax : natural;
                      e_lines, e_top, e_cols, e_left : natural) is
      begin
         for f in 1 to 6 loop
            frame(n_lines, width, ce_div);
         end loop;
         wait until rising_edge(clk);
         assert valid = e_valid report "FAIL: " & name & ": valid" severity failure;
         if e_valid = '1' then
            assert valid_mode = mode_index report "FAIL: " & name & ": mode" severity failure;
            assert hmin = e_hmin and hmax = e_hmax and vmin = e_vmin and vmax = e_vmax
               report "FAIL: " & name & ": rectangle " & integer'image(hmin) & " " & integer'image(hmax) &
                      " " & integer'image(vmin) & " " & integer'image(vmax) severity failure;
         end if;
         assert res_lines = e_lines and res_top = e_top
            report "FAIL: " & name & ": lines " & integer'image(res_lines) & " from " & integer'image(res_top)
            severity failure;
         assert res_cols = e_cols and res_left = e_left
            report "FAIL: " & name & ": pixels " & integer'image(res_cols) & " from " & integer'image(res_left)
            severity failure;
         report "PASS: " & name;
      end procedure;
   begin
      enable <= '1';

      -- 720p: 3 rows per line, 240 of the VIC's 270 lines, 9/8 wider than the normal 960
      mode_index <= 0;
      mode       <= scanline_mode(C_HDMI_720p_50, C_VIDEO_HDMI_16_9_50);
      check("720p VIC PAL", 270, 384, 4, '1', 100, 1179, 0, 719, 240, 15, 384, 0);
      check("720p VDC PAL", 268, 760, 2, '1', 104, 1175, 0, 719, 240, 14, 760, 0);
      check("720p VIC NTSC", 250, 404, 1, '1', 140, 1139, 0, 719, 240, 5, 404, 0);

      -- 576p and 800x600: 2 rows per line, every line
      mode_index <= 2;
      mode       <= scanline_mode(C_HDMI_576p_50, C_VIDEO_HDMI_4_3_50);
      check("576p 4:3 VIC PAL", 270, 384, 1, '1', 22, 696, 18, 557, 270, 0, 384, 0);
      mode_index <= 3;
      mode       <= scanline_mode(C_HDMI_576p_50, C_VIDEO_HDMI_5_4_50);
      check("576p 5:4 VIC PAL", 270, 384, 1, '1', 0, 719, 18, 557, 270, 0, 384, 0);
      mode_index <= 6;
      mode       <= scanline_mode(C_SVGA_800_600_60, C_VIDEO_SVGA_800_60);
      check("800x600 VIC PAL", 270, 384, 1, '1', 40, 759, 30, 569, 270, 0, 384, 0);

      -- 480 lines: 240 lines at 2 rows, and the sides trimmed to keep the shape
      mode_index <= 4;
      mode       <= scanline_mode(C_HDMI_640x480p_60, C_VIDEO_HDMI_640_60);
      check("640x480 VIC PAL", 270, 384, 1, '1', 0, 639, 0, 479, 240, 15, 341, 21);
      mode_index <= 5;
      mode       <= scanline_mode(C_HDMI_720x480p_5994, C_VIDEO_HDMI_720_5994);
      check("720x480 VIC PAL", 270, 384, 1, '1', 0, 719, 0, 479, 240, 15, 341, 21);

      -- CRT emulation off: nothing cropped, no rectangle
      enable     <= '0';
      mode_index <= 0;
      mode       <= scanline_mode(C_HDMI_720p_50, C_VIDEO_HDMI_16_9_50);
      check("720p off", 270, 384, 1, '0', 0, 0, 0, 0, 270, 0, 384, 0);

      -- Fields of 269 and 268 lines keep one rectangle instead of switching every frame
      enable     <= '1';
      mode_index <= 2;
      mode       <= scanline_mode(C_HDMI_576p_50, C_VIDEO_HDMI_4_3_50);
      for f in 1 to 3 loop
         frame(269, 384, 1);
         frame(268, 384, 1);
      end loop;
      frame(269, 384, 1);
      wait until rising_edge(clk);
      assert valid = '1' and hmin = 23 and hmax = 695 and vmin = 19 and vmax = 556
         report "FAIL: fields: rectangle" severity failure;
      frame(268, 384, 1);
      wait until rising_edge(clk);
      assert valid = '1' and hmin = 23 and hmax = 695 and vmin = 19 and vmax = 556
         report "FAIL: fields: rectangle changed" severity failure;
      report "PASS: fields";

      report "ALL CHECKS PASSED: tb_scanline_fit";
      done <= true;
      wait;
   end process stimulus;
end architecture;
