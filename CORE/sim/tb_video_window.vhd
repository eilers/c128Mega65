-- tb_video_window: where the 40- and 80-column displays sit in the window the core leaves
-- unblanked, which ascal and the VGA output show. The display must be centered in it.
-- VIC: the real VIC-II (PAL, $D011=$1B, border 14, background 6) into video_sync, the colour
-- registered once as main.vhd does. VDC: the real vdc_top with the KERNAL's PAL registers (as
-- a live R6 reads them), reverse screen without attributes, into video_sync_c128.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity tb_video_window is
end entity tb_video_window;

architecture sim of tb_video_window is

   component vdc_top is
      generic (
         RAM_ADDR_BITS : integer := 16
      );
      port (
         version   : in  std_logic;
         ram64k    : in  std_logic;
         initRam   : in  std_logic;
         debug     : in  std_logic;
         clk       : in  std_logic;
         enableBus : in  std_logic;
         reset     : in  std_logic;
         init      : in  std_logic;
         cs        : in  std_logic;
         rs        : in  std_logic;
         we        : in  std_logic;
         db_in     : in  std_logic_vector(7 downto 0);
         db_out    : out std_logic_vector(7 downto 0);
         lp_n      : in  std_logic;
         vsync     : out std_logic;
         hsync     : out std_logic;
         rgbi      : out std_logic_vector(3 downto 0);
         dbg_rd    : in  std_logic;
         dbg_addr  : in  std_logic_vector(15 downto 0);
         dbg_ram   : out std_logic_vector(7 downto 0);
         dbg_reg   : out std_logic_vector(7 downto 0)
      );
   end component;

   component video_sync_c128 is
      port (
         reset      : in  std_logic;
         clk32      : in  std_logic;
         pause      : in  std_logic;
         hshift_r60 : in  std_logic_vector(11 downto 0);
         hshift_l60 : in  std_logic_vector(11 downto 0);
         hshift_r50 : in  std_logic_vector(11 downto 0);
         hshift_l50 : in  std_logic_vector(11 downto 0);
         hsync      : in  std_logic;
         vsync      : in  std_logic;
         hsync_out  : out std_logic;
         vsync_out  : out std_logic;
         hblank     : out std_logic;
         vblank     : out std_logic;
         ilace      : out std_logic;
         field      : out std_logic;
         valid      : out std_logic;
         ce         : out std_logic
      );
   end component;

   -- fpga64_sid_iec's system cycle
   type sysCycleDef is (
      CYCLE_EXT0, CYCLE_EXT1, CYCLE_EXT2, CYCLE_EXT3,
      CYCLE_VIC0, CYCLE_VIC1, CYCLE_VIC2, CYCLE_VIC3,
      CYCLE_EXT4, CYCLE_EXT5, CYCLE_EXT6, CYCLE_EXT7,
      CYCLE_DMA0, CYCLE_DMA1, CYCLE_DMA2, CYCLE_DMA3,
      CYCLE_CPU0, CYCLE_CPU1, CYCLE_CPU2, CYCLE_CPU3,
      CYCLE_CPU4, CYCLE_CPU5, CYCLE_CPU6, CYCLE_CPU7,
      CYCLE_CPU8, CYCLE_CPU9, CYCLE_CPUA, CYCLE_CPUB,
      CYCLE_CPUC, CYCLE_CPUD, CYCLE_CPUE, CYCLE_CPUF);

   -- What a window measures: lines, border lines above/below, pixels, border pixels left/right
   type geometry_t is record
      lines, top, bottom, width, left, right : integer;
   end record;

   signal vic_done, vdc_done : boolean := false;
   signal vic_geo, vdc_geo   : geometry_t := (others => -1);

   -- VIC side
   signal clk          : std_logic := '0';
   signal sysCycle     : sysCycleDef := CYCLE_EXT0;
   signal vic_reset    : std_logic := '1';
   signal phi0         : std_logic;
   signal enableVic    : std_logic := '0';
   signal enablePixel  : std_logic := '0';
   signal vic_cs       : std_logic := '0';
   signal vic_we       : std_logic := '0';
   signal vic_areg     : unsigned(5 downto 0) := (others => '0');
   signal vic_dreg     : unsigned(7 downto 0) := (others => '0');
   signal vic_hs       : std_logic;
   signal vic_vs       : std_logic;
   signal vic_index    : unsigned(3 downto 0);
   signal vic_index_r  : unsigned(3 downto 0) := (others => '0');   -- main.vhd vic_r_reg
   signal vic_hblank   : std_logic;
   signal vic_vblank   : std_logic;
   signal vic_ready    : boolean := false;

   -- VDC side
   signal vclk         : std_logic := '0';
   signal vdc_rst      : std_logic := '1';
   signal en_bus       : std_logic := '0';
   signal vdc_cs       : std_logic := '0';
   signal vdc_rs       : std_logic := '0';
   signal vdc_we       : std_logic := '0';
   signal db_in        : std_logic_vector(7 downto 0) := x"00";
   signal db_out       : std_logic_vector(7 downto 0);
   signal vdc_hs       : std_logic;
   signal vdc_vs       : std_logic;
   signal rgbi         : std_logic_vector(3 downto 0);
   signal rgbi_r       : std_logic_vector(3 downto 0) := (others => '0');  -- main.vhd vdc_r_reg
   signal vdc_dot_ce   : std_logic := '0';
   signal vdc_hblank   : std_logic;
   signal vdc_vblank   : std_logic;
   signal vdc_ready    : boolean := false;

   type byte_list is array (natural range <>) of natural;
   -- R0-R36 as a live R6 reads them after the KERNAL's setup, but R24 reverse on and R25
   -- attributes off: the display is foreground F, everything else background 0
   constant C_VDC_REGS : byte_list(0 to 36) := (
      16#7F#, 16#50#, 16#66#, 16#49#, 16#26#, 16#E0#, 16#19#, 16#20#, 16#FC#, 16#E7#,
      16#A0#, 16#E7#, 16#00#, 16#00#, 16#00#, 16#00#, 0, 0, 0, 0,
      16#08#, 16#00#, 16#78#, 16#E8#, 16#60#, 16#07#, 16#F0#, 16#00#, 16#2F#, 16#E7#,
      0, 0, 0, 0, 16#7D#, 16#64#, 16#F5#);

   -- One window per frame from ce, blanking and "is display": the border around the display.
   -- Called on every clock; the state lives in the caller's variables.
   procedure measure(
      ce, hb, vb    : std_logic;
      is_display    : boolean;
      variable st   : inout geometry_t;     -- the frame being measured (top = first display line)
      variable line_vis, line_disp : inout boolean;
      variable px, lfirst, llast   : inout integer;
      variable hb_q : inout std_logic;
      variable last_disp : inout integer) is
   begin
      if ce = '1' then
         if hb = '1' and hb_q = '0' and line_vis then
            if line_disp then
               if st.top < 0 then
                  st.top := st.lines;
               end if;
               last_disp := st.lines;
               st.width  := px;
               st.left   := lfirst;
               st.right  := px - 1 - llast;
            end if;
            st.lines := st.lines + 1;
         end if;
         if hb = '0' and hb_q = '1' then
            line_vis  := false;
            line_disp := false;
            px        := 0;
         end if;
         hb_q := hb;
         if hb = '0' and vb = '0' then
            line_vis := true;
            if is_display then
               if not line_disp then
                  lfirst := px;
               end if;
               llast     := px;
               line_disp := true;
            end if;
            px := px + 1;
         end if;
      end if;
   end procedure measure;

begin

   ---------------------------------------------------------------------------
   -- VIC-II at clk32, enables as fpga64_sid_iec makes them
   ---------------------------------------------------------------------------

   clk <= not clk after 15.86 ns when not vic_done;

   p_sys : process (clk)
   begin
      if rising_edge(clk) then
         if sysCycle = sysCycleDef'high then
            sysCycle <= sysCycleDef'low;
         else
            sysCycle <= sysCycleDef'succ(sysCycle);
         end if;
         enableVic <= '0';
         if sysCycle = CYCLE_VIC2 or sysCycle = CYCLE_CPU6 then
            enableVic <= '1';
         end if;
         enablePixel <= '0';
         if sysCycle = CYCLE_VIC2 or sysCycle = CYCLE_EXT2 or sysCycle = CYCLE_DMA2
            or sysCycle = CYCLE_EXT6 or sysCycle = CYCLE_CPU2 or sysCycle = CYCLE_CPU6
            or sysCycle = CYCLE_CPUA or sysCycle = CYCLE_CPUE then
            enablePixel <= '1';
         end if;
         if enablePixel = '1' then
            vic_index_r <= vic_index;
         end if;
      end if;
   end process p_sys;

   phi0 <= '1' when sysCycle >= CYCLE_EXT4 and sysCycle <= CYCLE_CPU7 else '0';

   i_vic : entity work.video_vicii_656x
      generic map (
         registeredAddress => true,
         emulateRefresh    => true,
         emulateLightpen   => true,
         emulateGraphics   => true
      )
      port map (
         clk         => clk,
         phi         => phi0,
         enaData     => enableVic,
         enaPixel    => enablePixel,
         baSync      => '0',
         ba          => open,
         ba_dma      => open,
         mode6569    => '1',
         mode6567old => '0',
         mode6567R8  => '0',
         mode6572    => '0',
         turbo_en    => '1',
         turbo_state => open,
         vic2e       => '1',
         variant     => "00",
         reset       => vic_reset,
         cs          => vic_cs,
         we          => vic_we,
         lp_n        => '1',
         aRegisters  => vic_areg,
         diRegisters => vic_dreg,
         di          => x"20",
         diColor     => x"1",
         do          => open,
         vicAddr     => open,
         irq_n       => open,
         hSync       => vic_hs,
         vSync       => vic_vs,
         colorIndex  => vic_index,
         colorX      => open,
         colorY      => open,
         invertV     => open,
         ko          => open,
         debugX      => open,
         debugY      => open,
         vicRefresh  => open,
         addrValid   => open
      );

   i_vic_sync : entity work.video_sync
      port map (
         clk32     => clk,
         pause     => '0',
         hsync     => vic_hs,
         vsync     => vic_vs,
         ntsc      => '0',
         wide      => '0',
         hsync_out => open,
         vsync_out => open,
         hblank    => vic_hblank,
         vblank    => vic_vblank
      );

   p_vic_setup : process
      -- held for three C64 cycles: the VIC latches it on its own phase
      procedure wr(a : natural; d : natural) is
      begin
         wait until rising_edge(clk);
         vic_areg <= to_unsigned(a, 6);
         vic_dreg <= to_unsigned(d, 8);
         vic_cs   <= '1';
         vic_we   <= '1';
         for i in 1 to 96 loop
            wait until rising_edge(clk);
         end loop;
         vic_cs <= '0';
         vic_we <= '0';
         for i in 1 to 32 loop
            wait until rising_edge(clk);
         end loop;
      end procedure wr;
   begin
      for i in 1 to 200 loop
         wait until rising_edge(clk);
      end loop;
      vic_reset <= '0';
      for i in 1 to 2000 loop
         wait until rising_edge(clk);
      end loop;
      wr(16#11#, 16#1B#);   -- display on, 25 rows
      wr(16#16#, 16#08#);   -- 40 columns
      wr(16#20#, 14);       -- border
      wr(16#21#, 6);        -- background
      vic_ready <= true;
      wait;
   end process p_vic_setup;

   p_vic_measure : process
      variable st        : geometry_t;
      variable frame     : integer := -1;
      variable line_vis  : boolean := false;
      variable line_disp : boolean := false;
      variable px, lfirst, llast, last_disp : integer := 0;
      variable hb_q      : std_logic := '1';
      variable vb_q      : std_logic := '1';
   begin
      wait until vic_ready;
      loop
         wait until rising_edge(clk);
         if vic_vblank = '1' and vb_q = '0' then
            frame := frame + 1;
            if frame >= 2 then   -- frame 1 may have started before the registers
               st.bottom := st.lines - 1 - last_disp;
               vic_geo   <= st;
               vic_done  <= true;
               wait;
            end if;
            st := (lines => 0, top => -1, bottom => -1, width => -1, left => -1, right => -1);
         end if;
         vb_q := vic_vblank;
         measure(enablePixel, vic_hblank, vic_vblank, vic_index_r = 6,
                 st, line_vis, line_disp, px, lfirst, llast, hb_q, last_disp);
      end loop;
   end process p_vic_measure;

   ---------------------------------------------------------------------------
   -- VDC at 32 MHz, a dot every other clock
   ---------------------------------------------------------------------------

   vclk <= not vclk after 15.625 ns when not vdc_done;

   p_vdc_dot : process (vclk)
   begin
      if rising_edge(vclk) then
         vdc_dot_ce <= not vdc_dot_ce;
         rgbi_r     <= rgbi;
      end if;
   end process p_vdc_dot;

   i_vdc : vdc_top
      generic map (
         RAM_ADDR_BITS => 16
      )
      port map (
         version   => '0',
         ram64k    => '1',
         initRam   => '1',
         debug     => '0',
         clk       => vclk,
         enableBus => en_bus,
         reset     => vdc_rst,
         init      => '0',
         cs        => vdc_cs,
         rs        => vdc_rs,
         we        => vdc_we,
         db_in     => db_in,
         db_out    => db_out,
         lp_n      => '1',
         vsync     => vdc_vs,
         hsync     => vdc_hs,
         rgbi      => rgbi,
         dbg_rd    => '0',
         dbg_addr  => x"0000",
         dbg_ram   => open,
         dbg_reg   => open
      );

   -- The shifts main.vhd gives it
   i_vdc_sync : video_sync_c128
      port map (
         reset      => vdc_rst,
         clk32      => vclk,
         pause      => '0',
         hshift_r60 => std_logic_vector(to_unsigned(43, 12)),
         hshift_l60 => std_logic_vector(to_unsigned(236, 12)),
         hshift_r50 => std_logic_vector(to_unsigned(83, 12)),
         hshift_l50 => std_logic_vector(to_unsigned(294, 12)),
         hsync      => vdc_hs,
         vsync      => vdc_vs,
         hsync_out  => open,
         vsync_out  => open,
         hblank     => vdc_hblank,
         vblank     => vdc_vblank,
         ilace      => open,
         field      => open,
         valid      => open,
         ce         => open
      );

   p_vdc_setup : process
      -- $D600 (rs 0) or $D601 (rs 1), enableBus once as fpga64_sid_iec
      procedure cpu(r : std_logic; d : natural) is
      begin
         wait until rising_edge(vclk);
         vdc_cs <= '1';
         vdc_rs <= r;
         vdc_we <= '1';
         db_in  <= std_logic_vector(to_unsigned(d, 8));
         wait until rising_edge(vclk);
         en_bus <= '1';
         wait until rising_edge(vclk);
         en_bus <= '0';
         wait until rising_edge(vclk);
         vdc_cs <= '0';
         vdc_we <= '0';
         for i in 1 to 3 loop
            wait until rising_edge(vclk);
         end loop;
      end procedure cpu;
   begin
      for i in 1 to 64 loop
         wait until rising_edge(vclk);
      end loop;
      vdc_rst <= '0';
      for i in 1 to 64 loop
         wait until rising_edge(vclk);
      end loop;
      for r in C_VDC_REGS'range loop
         if r < 16 or (r >= 20 and r < 30) or r >= 34 then   -- not the address/data ones
            cpu('0', r);
            cpu('1', C_VDC_REGS(r));
         end if;
      end loop;
      vdc_ready <= true;
      wait;
   end process p_vdc_setup;

   p_vdc_measure : process
      variable st        : geometry_t;
      variable frame     : integer := -1;
      variable line_vis  : boolean := false;
      variable line_disp : boolean := false;
      variable px, lfirst, llast, last_disp : integer := 0;
      variable hb_q      : std_logic := '1';
      variable vb_q      : std_logic := '1';
   begin
      wait until vdc_ready;
      loop
         wait until rising_edge(vclk);
         if vdc_vblank = '1' and vb_q = '0' then
            frame := frame + 1;
            if frame >= 2 then
               st.bottom := st.lines - 1 - last_disp;
               vdc_geo   <= st;
               vdc_done  <= true;
               wait;
            end if;
            st := (lines => 0, top => -1, bottom => -1, width => -1, left => -1, right => -1);
         end if;
         vb_q := vdc_vblank;
         measure(vdc_dot_ce, vdc_hblank, vdc_vblank, rgbi_r /= "0000",
                 st, line_vis, line_disp, px, lfirst, llast, hb_q, last_disp);
      end loop;
   end process p_vdc_measure;

   ---------------------------------------------------------------------------
   -- Checks
   ---------------------------------------------------------------------------

   p_check : process
      variable fails : natural := 0;

      procedure show(name : string; g : geometry_t) is
         variable l : line;
      begin
         write(l, name & ": " & integer'image(g.lines) & " lines, border above " &
                  integer'image(g.top) & " below " & integer'image(g.bottom) & "; " &
                  integer'image(g.width) & " pixels, border left " & integer'image(g.left) &
                  " right " & integer'image(g.right));
         writeline(output, l);
      end procedure show;

      procedure check(ok : boolean; what : string) is
      begin
         if not ok then
            fails := fails + 1;
            report "FAIL: " & what severity error;
         end if;
      end procedure check;
   begin
      wait until vic_done and vdc_done;
      wait for 1 ns;
      show("VIC", vic_geo);
      show("VDC", vdc_geo);

      -- 384 x 270: 32 pixels and 35 lines around 320 x 200 (raster 16..285, as VICE)
      check(vic_geo.lines = 270 and vic_geo.width = 384, "VIC window size");
      check(vic_geo.top = 35 and vic_geo.bottom = 35, "VIC display not centered vertically");
      check(vic_geo.left = 32 and vic_geo.right = 32, "VIC display not centered horizontally");

      -- 763 x 269 around 640 x 200: odd sizes, the extra line below and the extra dot left
      check(vdc_geo.lines = 269 and vdc_geo.width = 763, "VDC window size");
      check(vdc_geo.top = 34 and vdc_geo.bottom = 35, "VDC display not centered vertically");
      check(vdc_geo.left = 62 and vdc_geo.right = 61, "VDC display not centered horizontally");

      if fails = 0 then
         report "ALL CHECKS PASSED" severity note;
      else
         report "FAILED: " & integer'image(fails) & " checks" severity error;
      end if;
      wait;
   end process p_check;

end architecture sim;
