----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Testbench for HDMI when the core switches between the 40- and 80-column pictures.
-- ascal (and scanline_fit, as with a scanline filter) get a narrow picture A and a
-- wide picture B with fewer lines, like the VIC and the VDC. The source flips at
-- different points of the frame. Every HDMI frame is checked: each pixel shows its
-- source's own pixel for that spot, or black. The raw picture shows what ascal does on
-- its own; the blanked one, after video_switch_blank, must never show anything else.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity tb_video_switch is
   generic (
      G_FIT    : boolean := true;    -- scanline_fit on, as with a scanline filter
      G_BW     : natural := 128      -- width of picture B (wider than 132 = downscaled)
   );
end entity tb_video_switch;

architecture sim of tb_video_switch is

   constant C_TAG_A : natural := 16#5A#;
   constant C_TAG_B : natural := 16#A5#;
   constant C_AW    : natural := 64;
   constant C_AH    : natural := 24;
   constant C_BH    : natural := 22;

   -- The source flips at these times (fractions of a frame apart from each other)
   type time_vector is array (natural range <>) of time;
   constant C_SWITCH : time_vector := (1000 us, 2237 us, 3411 us, 4659 us, 5830 us, 7013 us,
                                       8166 us, 9370 us);
   constant C_END    : time := 10500 us;

   function to_sl(b : boolean) return std_logic is
   begin
      if b then
         return '1';
      end if;
      return '0';
   end function to_sl;

   constant C_FIT_ON : std_logic := to_sl(G_FIT);

   signal i_clk  : std_logic := '0';
   signal o_clk  : std_logic := '0';
   signal hr_clk : std_logic := '0';
   signal done   : boolean := false;
   signal sel_b  : std_logic := '0';
   signal reset_na : std_logic := '0';

   -- Both pictures run all the time, like the VIC and the VDC
   signal a_div  : natural range 0 to 3 := 0;
   signal a_h    : natural range 0 to 79 := 0;
   signal a_v    : natural range 0 to 29 := 0;
   signal b_div  : natural range 0 to 1 := 0;
   signal b_h    : natural range 0 to G_BW + 31 := 0;
   signal b_v    : natural range 0 to 27 := 0;

   signal v_ce, v_hs, v_vs, v_hbl, v_vbl : std_logic := '0';
   signal v_r, v_g, v_b : std_logic_vector(7 downto 0) := (others => '0');

   signal f_ce, f_hs, f_vs, f_hbl, f_vbl, f_valid : std_logic;
   signal f_r, f_g, f_b : std_logic_vector(7 downto 0);
   signal f_mode : natural range 0 to 7;
   signal f_hmin, f_hmax, f_vmin, f_vmax : natural range 0 to 4095;
   signal hmin, hmax, vmin, vmax : natural range 0 to 4095;

   signal o_r, o_g, o_b : unsigned(7 downto 0);
   signal o_hs, o_vs, o_de : std_logic;
   signal blank : std_logic;

   signal w_write, w_read, w_rdv : std_logic;
   signal w_wait       : std_logic := '0';
   signal w_address    : std_logic_vector(18 downto 0);
   signal w_writedata  : std_logic_vector(127 downto 0);
   signal w_readdata   : std_logic_vector(127 downto 0) := (others => '0');
   signal w_byteenable : std_logic_vector(15 downto 0);
   signal w_burstcount : std_logic_vector(7 downto 0);

   type rect_t is record
      hmin, hmax, vmin, vmax : natural;
   end record;

   -- Where a picture of so many lines lands: scanline_fit with scale 2, 6 pixels per line
   function rect_of(lines : natural) return rect_t is
      variable w : natural;
   begin
      if not G_FIT then
         return (0, 159, 0, 47);
      end if;
      w := (1536 * lines + 128) / 256;
      return ((160 - w) / 2, (160 - w) / 2 + w - 1, (48 - 2 * lines) / 2, (48 - 2 * lines) / 2 + 2 * lines - 1);
   end function rect_of;

begin

   i_clk  <= not i_clk  after 5 ns when not done;   -- 100 MHz
   o_clk  <= not o_clk  after 4 ns when not done;   -- 125 MHz
   hr_clk <= not hr_clk after 5 ns when not done;

   reset_na <= '1' after 200 ns;

   p_switch : process
   begin
      for i in C_SWITCH'range loop
         wait for C_SWITCH(i) - now;
         wait until rising_edge(i_clk);
         sel_b <= not sel_b;
      end loop;
      wait;
   end process p_switch;

   p_sources : process (i_clk)
      variable ce : std_logic;
   begin
      if rising_edge(i_clk) then
         a_div <= (a_div + 1) mod 4;
         b_div <= (b_div + 1) mod 2;
         if a_div = 0 then
            a_h <= (a_h + 1) mod 80;
            if a_h = 79 then
               a_v <= (a_v + 1) mod 30;
            end if;
         end if;
         if b_div = 0 then
            b_h <= (b_h + 1) mod (G_BW + 32);
            if b_h = G_BW + 31 then
               b_v <= (b_v + 1) mod 28;
            end if;
         end if;

         if sel_b = '0' then
            ce := '1' when a_div = 0 else '0';
            v_ce <= ce;
            if ce = '1' then
               v_hbl <= '0' when a_h < C_AW else '1';
               v_vbl <= '0' when a_v < C_AH else '1';
               v_hs  <= '1' when a_h >= 68 and a_h < 72 else '0';
               v_vs  <= '1' when a_v >= 26 and a_v < 28 else '0';
               v_r   <= std_logic_vector(to_unsigned(C_TAG_A, 8));
               v_g   <= std_logic_vector(to_unsigned(a_h mod 256, 8));
               v_b   <= std_logic_vector(to_unsigned(a_v, 8));
            end if;
         else
            ce := '1' when b_div = 0 else '0';
            v_ce <= ce;
            if ce = '1' then
               v_hbl <= '0' when b_h < G_BW else '1';
               v_vbl <= '0' when b_v < C_BH else '1';
               v_hs  <= '1' when b_h >= G_BW + 8 and b_h < G_BW + 16 else '0';
               v_vs  <= '1' when b_v >= 24 and b_v < 26 else '0';
               v_r   <= std_logic_vector(to_unsigned(C_TAG_B, 8));
               v_g   <= std_logic_vector(to_unsigned(b_h mod 256, 8));
               v_b   <= std_logic_vector(to_unsigned(b_v, 8));
            end if;
         end if;
      end if;
   end process p_sources;

   i_fit : entity work.scanline_fit
      port map (
         video_clk_i     => i_clk,
         video_enable_i  => C_FIT_ON,
         video_mode_i    => 0,
         video_hdisp_i   => 160,
         video_vdisp_i   => 48,
         video_scale_i   => 2,
         video_lines_i   => 24,
         video_hfactor_i => 1536,
         video_ce_i      => v_ce,
         video_red_i     => v_r,
         video_green_i   => v_g,
         video_blue_i    => v_b,
         video_hs_i      => v_hs,
         video_vs_i      => v_vs,
         video_hblank_i  => v_hbl,
         video_vblank_i  => v_vbl,
         video_ce_o      => f_ce,
         video_red_o     => f_r,
         video_green_o   => f_g,
         video_blue_o    => f_b,
         video_hs_o      => f_hs,
         video_vs_o      => f_vs,
         video_hblank_o  => f_hbl,
         video_vblank_o  => f_vbl,
         video_valid_o   => f_valid,
         video_mode_o    => f_mode,
         video_hmin_o    => f_hmin,
         video_hmax_o    => f_hmax,
         video_vmin_o    => f_vmin,
         video_vmax_o    => f_vmax
      );

   hmin <= f_hmin when f_valid = '1' else 0;
   hmax <= f_hmax when f_valid = '1' else 159;
   vmin <= f_vmin when f_valid = '1' else 0;
   vmax <= f_vmax when f_valid = '1' else 47;

   i_ascal : entity work.ascal
      generic map (
         MASK => x"ff", RAMBASE => (others => '0'), RAMSIZE => to_unsigned(2**16, 32),
         INTER => false, HEADER => false, DOWNSCALE => true, DOWNSCALE_NN => false,
         BYTESWAP => true, ADAPTIVE => true, PALETTE => false, PALETTE2 => false,
         FRAC => 6, OHRES => 2048, IHRES => 1024, N_DW => 128, N_AW => 19, N_BURST => 128
      )
      port map (
         i_r => unsigned(f_r), i_g => unsigned(f_g), i_b => unsigned(f_b),
         i_hs => f_hs, i_vs => f_vs, i_fl => '0', i_de => not (f_hbl or f_vbl),
         i_ce => f_ce, i_clk => i_clk,
         o_r => o_r, o_g => o_g, o_b => o_b, o_hs => o_hs, o_vs => o_vs, o_de => o_de,
         o_vbl => open, o_ce => '1', o_clk => o_clk, o_border => x"000000",
         o_fb_ena => '0', o_fb_hsize => 0, o_fb_vsize => 0, o_fb_format => "000101",
         o_fb_base => x"0000_0000", o_fb_stride => (others => '0'),
         pal1_clk => '0', pal1_dw => x"000000000000", pal1_dr => open, pal1_a => "0000000",
         pal1_wr => '0', pal_n => '0',
         pal2_clk => '0', pal2_dw => x"000000", pal2_dr => open, pal2_a => "00000000", pal2_wr => '0',
         o_lltune => open,
         iauto => '1', himin => 0, himax => 0, vimin => 0, vimax => 0,
         i_hdmax => open, i_vdmax => open,
         run => '1', freeze => '0', mode => "00000",
         htotal => 200, hsstart => 170, hsend => 180, hdisp => 160,
         vtotal => 60, vsstart => 50, vsend => 52, vdisp => 48,
         hmin => hmin, hmax => hmax, vmin => vmin, vmax => vmax,
         format => "01",
         poly_clk => '0', poly_dw => (others => '0'), poly_a => (others => '0'), poly_wr => '0',
         avl_clk => hr_clk, avl_waitrequest => w_wait, avl_readdata => w_readdata,
         avl_readdatavalid => w_rdv, avl_burstcount => w_burstcount, avl_writedata => w_writedata,
         avl_address => w_address, avl_write => w_write, avl_read => w_read,
         avl_byteenable => w_byteenable, reset_na => reset_na
      );

   i_blank : entity work.video_switch_blank
      port map (
         video_clk_i    => i_clk,
         video_vs_i     => f_vs,
         video_fit_i    => C_FIT_ON,
         video_source_i => sel_b,
         hdmi_clk_i     => o_clk,
         hdmi_vs_i      => o_vs,
         hdmi_blank_o   => blank
      );

   -- Memory: one burst at a time, reads answered after a few clocks
   p_mem : process (hr_clk)
      type mem_t is array (0 to 4095) of std_logic_vector(127 downto 0);
      variable mem   : mem_t := (others => (others => '0'));
      variable waddr : natural := 0;
      variable wleft : natural := 0;
      variable raddr : natural := 0;
      variable rleft : natural := 0;
      variable rlat  : natural := 0;
   begin
      if rising_edge(hr_clk) then
         w_rdv <= '0';
         if w_write = '1' and w_wait = '0' then
            if wleft = 0 then
               waddr := to_integer(unsigned(w_address)) mod 4096;
               wleft := to_integer(unsigned(w_burstcount));
            end if;
            for i in 0 to 15 loop
               if w_byteenable(i) = '1' then
                  mem(waddr)(8 * i + 7 downto 8 * i) := w_writedata(8 * i + 7 downto 8 * i);
               end if;
            end loop;
            waddr := (waddr + 1) mod 4096;
            wleft := wleft - 1;
         elsif w_read = '1' and w_wait = '0' then
            raddr  := to_integer(unsigned(w_address)) mod 4096;
            rleft  := to_integer(unsigned(w_burstcount));
            rlat   := 8;
            w_wait <= '1';
         elsif rleft > 0 then
            if rlat > 0 then
               rlat := rlat - 1;
            else
               w_readdata <= mem(raddr);
               w_rdv      <= '1';
               raddr      := (raddr + 1) mod 4096;
               rleft      := rleft - 1;
               if rleft = 0 then
                  w_wait <= '0';
               end if;
            end if;
         end if;
      end if;
   end process p_mem;

   -- Every HDMI frame: '.' black, 'A'/'B' that picture whole, 'a'/'b' that picture with
   -- black parts, 'x' anything else. Upper case for the raw picture's letters too.
   p_check : process (o_clk)
      type flags_t is array (0 to 1) of boolean;   -- per picture
      type frame_t is record
         black : boolean;
         blk   : flags_t;   -- black pixels inside that picture's rectangle
         ok    : flags_t;   -- every other pixel is that picture's own
      end record;
      type frames_t is array (0 to 1) of frame_t;
      type line_t is record
         prev_x : integer;
         y      : integer;
      end record;
      type lines_t is array (0 to 1, 0 to 1) of line_t;   -- (stream, picture)
      type prevy_t is array (0 to 1, 0 to 1) of integer;
      variable fr      : frames_t;
      variable ln      : lines_t;
      variable prev_y  : prevy_t;
      variable ox, oy  : integer := 0;
      variable de_d    : std_logic := '0';
      variable vs_d    : std_logic := '0';
      variable seq     : string(1 to 2) := "  ";
      variable raw_l   : line;
      variable blk_l   : line;
      variable started : boolean := false;
      variable r, g, b : natural;
      variable rc      : rect_t;
      variable tag, w, h : natural;
      variable ok      : boolean;
      variable c       : character;
      variable cur     : character := 'A';     -- the picture the core shows
      variable phase   : natural := 0;         -- 0 old picture, 1 black, 2 new picture
      variable blacks  : natural := 0;
      variable errors  : natural := 0;
      variable settled : boolean := false;
      variable sel_d   : std_logic := '0';

      procedure new_frame is
      begin
         for s in 0 to 1 loop
            fr(s) := (true, (false, false), (true, true));
            for p in 0 to 1 loop
               prev_y(s, p) := -1;
            end loop;
         end loop;
      end procedure new_frame;

      function letter(f : frame_t) return character is
      begin
         if f.black then
            return '.';
         elsif f.ok(0) and not f.blk(0) then
            return 'A';
         elsif f.ok(0) then
            return 'a';
         elsif f.ok(1) and not f.blk(1) then
            return 'B';
         elsif f.ok(1) then
            return 'b';
         else
            return 'x';
         end if;
      end function letter;

      variable l : line;
   begin
      if rising_edge(o_clk) then
         if o_vs = '1' and vs_d = '0' then
            if started then
               seq(1) := letter(fr(0));
               seq(2) := letter(fr(1));
               write(raw_l, seq(1));
               write(blk_l, seq(2));
               -- The blanked picture: old picture (maybe cut short), black, then the new one whole
               c := seq(2);
               if not settled then
                  settled := c = 'A';
               elsif c = 'x' then
                  errors := errors + 1;
               elsif phase = 0 and c /= cur and c /= '.' then
                  -- still the old picture, possibly cut to black
                  null;
               elsif c = '.' then
                  phase  := 1;
                  blacks := blacks + 1;
               elsif c = cur then
                  if phase = 1 then
                     write(l, string'("switch to ") & cur & ": " & integer'image(blacks) & " black frames");
                     writeline(output, l);
                  end if;
                  phase := 2;
               else
                  errors := errors + 1;
               end if;
            end if;
            started := true;
            new_frame;
            oy := -1;
         end if;
         vs_d := o_vs;

         if sel_b /= sel_d then
            cur    := 'B' when sel_b = '1' else 'A';
            phase  := 0;
            blacks := 0;
         end if;
         sel_d := sel_b;

         if o_de = '1' and de_d = '0' then
            oy := oy + 1;
            ox := 0;
            for s in 0 to 1 loop
               for p in 0 to 1 loop
                  ln(s, p) := (-1, -1);
               end loop;
            end loop;
         end if;
         de_d := o_de;

         if o_de = '1' then
            for s in 0 to 1 loop
               if s = 0 or blank = '0' then
                  r := to_integer(o_r); g := to_integer(o_g); b := to_integer(o_b);
               else
                  r := 0; g := 0; b := 0;
               end if;
               if r /= 0 or g /= 0 or b /= 0 then
                  fr(s).black := false;
               end if;
               for p in 0 to 1 loop
                  if p = 0 then
                     tag := C_TAG_A; w := C_AW; h := C_AH;
                  else
                     tag := C_TAG_B; w := G_BW; h := C_BH;
                  end if;
                  rc := rect_of(h);
                  ok := true;
                  if r = 0 and g = 0 and b = 0 then
                     fr(s).blk(p) := fr(s).blk(p) or
                                     (ox >= rc.hmin and ox <= rc.hmax and oy >= rc.vmin and oy <= rc.vmax);
                  elsif ox < rc.hmin or ox > rc.hmax or oy < rc.vmin or oy > rc.vmax then
                     ok := false;
                  elsif r /= tag or g < ln(s, p).prev_x or b >= h or g >= w then
                     ok := false;
                  else
                     if ln(s, p).y = -1 then
                        ln(s, p).y := b;
                        ok := b >= prev_y(s, p) and (prev_y(s, p) /= -1 or oy > rc.vmin or b <= 1);
                        prev_y(s, p) := b;
                     elsif b /= ln(s, p).y then
                        ok := false;
                     end if;
                     if (ox = rc.hmin and g > 1) or (ox = rc.hmax and g + 3 < w) or
                        (oy = rc.vmax and b + 2 < h) then
                        ok := false;
                     end if;
                     ln(s, p).prev_x := g;
                  end if;
                  if not ok then
                     fr(s).ok(p) := false;
                  end if;
               end loop;
            end loop;
            ox := ox + 1;
         end if;

         if now >= C_END and not done then
            write(l, string'("raw:     ")); write(l, raw_l.all); writeline(output, l);
            write(l, string'("blanked: ")); write(l, blk_l.all); writeline(output, l);
            if errors = 0 and settled then
               report "ALL CHECKS PASSED" severity note;
            else
               report "FAIL: " & integer'image(errors) & " blanked frames show something else"
                  severity error;
            end if;
            done <= true;
         end if;
      end if;
   end process p_check;

end architecture sim;
