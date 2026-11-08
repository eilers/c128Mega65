-- tb_vdc_peek: the system CPU's reads of the VDC (vdc_peek.vhd) against the real vdc_top.
-- A program's reads through $D600/$D601 are the reference: RAM in 16K and 64K mapping,
-- registers R0-R37, the status and the selected register. The peeks must not move the update
-- address or clear the light pen flag, writes to the device are ignored, and peeks during a
-- block fill neither disturb it nor see anything but old or new bytes.
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_vdc_peek is
end entity tb_vdc_peek;

architecture sim of tb_vdc_peek is

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

   signal vclk     : std_logic := '0';
   signal qclk     : std_logic := '0';
   signal vrst     : std_logic := '1';
   signal qrst     : std_logic := '1';

   -- The C128's side of the VDC
   signal en_bus   : std_logic := '0';
   signal cs       : std_logic := '0';
   signal rs       : std_logic := '0';
   signal we       : std_logic := '0';
   signal db_in    : std_logic_vector(7 downto 0) := x"00";
   signal db_out   : std_logic_vector(7 downto 0);
   signal lp_n     : std_logic := '1';

   -- vdc_peek
   signal dbg_rd   : std_logic;
   signal dbg_addr : unsigned(15 downto 0);
   signal dbg_ram  : std_logic_vector(7 downto 0);
   signal dbg_reg  : std_logic_vector(7 downto 0);
   signal q_addr   : std_logic_vector(27 downto 0) := (others => '0');
   signal q_ce     : std_logic := '0';
   signal q_we     : std_logic := '0';
   signal q_rdata  : std_logic_vector(15 downto 0);
   signal q_wait   : std_logic;

   signal done     : boolean := false;

   constant C_CSR  : natural := 16#FFFF000#;

   type byte_list is array (natural range <>) of natural;
   -- The C128 KERNAL's VDC setup (PAL); 16-19 and 30-33 are left alone
   constant C_INIT : byte_list(0 to 36) := (
      16#7E#, 16#50#, 16#66#, 16#49#, 16#27#, 16#E0#, 16#19#, 16#20#, 16#FC#, 16#E7#,
      16#A0#, 16#E7#, 16#00#, 16#00#, 16#00#, 16#00#, 0, 0, 0, 0,
      16#08#, 16#00#, 16#78#, 16#E8#, 16#20#, 16#47#, 16#F0#, 16#00#, 16#20#, 16#E7#,
      0, 0, 0, 0, 16#7D#, 16#64#, 16#F5#);

begin

   vclk <= not vclk after 15.625 ns when not done;   -- 32 MHz
   qclk <= not qclk after 10 ns when not done;       -- 50 MHz

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
         reset     => vrst,
         init      => '0',
         cs        => cs,
         rs        => rs,
         we        => we,
         db_in     => db_in,
         db_out    => db_out,
         lp_n      => lp_n,
         vsync     => open,
         hsync     => open,
         rgbi      => open,
         dbg_rd    => dbg_rd,
         dbg_addr  => std_logic_vector(dbg_addr),
         dbg_ram   => dbg_ram,
         dbg_reg   => dbg_reg
      );

   i_peek : entity work.vdc_peek
      port map (
         vdc_clk_i    => vclk,
         vdc_rd_o     => dbg_rd,
         vdc_addr_o   => dbg_addr,
         vdc_ram_i    => unsigned(dbg_ram),
         vdc_reg_i    => unsigned(dbg_reg),
         qnice_clk_i  => qclk,
         qnice_rst_i  => qrst,
         qnice_addr_i => q_addr,
         qnice_ce_i   => q_ce,
         qnice_we_i   => q_we,
         qnice_data_o => q_rdata,
         qnice_wait_o => q_wait
      );

   p_test : process
      variable fails  : natural := 0;
      variable checks : natural := 0;
      variable b, c   : natural;
      variable waits  : natural;

      procedure check(ok : boolean; what : string) is
      begin
         checks := checks + 1;
         if not ok then
            fails := fails + 1;
            report "FAIL: " & what severity error;
         end if;
      end procedure check;

      -- The C128's access to $D600 (rs 0) or $D601 (rs 1), enableBus once as fpga64_sid_iec
      procedure cpu(r : std_logic; w : std_logic; d : natural; q : out natural) is
      begin
         wait until rising_edge(vclk);
         cs    <= '1';
         rs    <= r;
         we    <= w;
         db_in <= std_logic_vector(to_unsigned(d, 8));
         wait until rising_edge(vclk);
         en_bus <= '1';
         wait until rising_edge(vclk);
         en_bus <= '0';
         wait until rising_edge(vclk);
         q  := to_integer(unsigned(db_out));
         cs <= '0';
         we <= '0';
         for i in 1 to 3 loop
            wait until rising_edge(vclk);
         end loop;
      end procedure cpu;

      procedure ready is
         variable s : natural;
      begin
         for i in 1 to 20000 loop
            cpu('0', '0', 0, s);
            if s >= 128 then
               return;
            end if;
         end loop;
         check(false, "the VDC never became ready, status " & to_string(db_out));
      end procedure ready;

      procedure reg_wr(r : natural; v : natural) is
         variable q : natural;
      begin
         cpu('0', '1', r, q);
         cpu('1', '1', v, q);
      end procedure reg_wr;

      procedure reg_rd(r : natural; v : out natural) is
         variable q : natural;
      begin
         cpu('0', '1', r, q);
         cpu('1', '0', 0, v);
      end procedure reg_rd;

      procedure seek(a : natural) is
      begin
         ready;
         reg_wr(18, a / 256);
         ready;
         reg_wr(19, a mod 256);
         ready;
      end procedure seek;

      -- A program's write and read of VDC RAM (R31 moves the update address on)
      procedure ram_wr(a : natural; v : natural) is
      begin
         seek(a);
         reg_wr(31, v);
         ready;
      end procedure ram_wr;

      procedure ram_rd(a : natural; v : out natural) is
      begin
         seek(a);
         reg_rd(31, v);
         ready;
      end procedure ram_rd;

      -- rv_soc: ce from a rising edge; the next rising edge without wait ends the access
      procedure dev(a : natural; w : std_logic; v : out natural; n_waits : out natural) is
         variable n : natural := 0;
      begin
         wait until rising_edge(qclk);
         q_ce   <= '1';
         q_we   <= w;
         q_addr <= std_logic_vector(to_unsigned(a, 28));
         loop
            wait until rising_edge(qclk);
            exit when q_wait = '0';
            n := n + 1;
            if n > 100 then
               check(false, "the device never ended its wait");
               exit;
            end if;
         end loop;
         v       := to_integer(unsigned(q_rdata(7 downto 0)));
         n_waits := n;
         q_ce    <= '0';
         q_we    <= '0';
      end procedure dev;

      procedure peek(a : natural; v : out natural) is
         variable n : natural;
      begin
         dev(a, '0', v, n);
      end procedure peek;

      procedure same_as_program(a : natural; tag : string) is
         variable p, r : natural;
      begin
         peek(a, p);
         ram_rd(a, r);
         check(p = r, tag & ": peek " & integer'image(a) & " = " & integer'image(p) &
               ", a program reads " & integer'image(r));
      end procedure same_as_program;

      constant C_SPOTS : byte_list := (16#0000#, 16#0001#, 16#00FF#, 16#0100#, 16#0101#,
         16#1000#, 16#1FFF#, 16#2000#, 16#3FFF#, 16#4000#, 16#4100#, 16#7FFF#, 16#8000#,
         16#C000#, 16#C101#, 16#FFFF#);

   begin
      wait for 200 ns;
      vrst <= '0';
      qrst <= '0';
      for i in C_INIT'range loop
         if not (i >= 16 and i <= 19) and not (i >= 30 and i <= 33) then
            reg_wr(i, C_INIT(i));
         end if;
      end loop;
      reg_rd(16, b);                             -- the light pen flag has no reset: a read clears it
      ready;                                     -- the reset's RAM erase is over
      report "VDC set up";

      -- The erase pattern (00 at even addresses, FF at odd ones), seen by a peek
      peek(16#2000#, b);
      peek(16#2001#, c);
      check(b = 16#00# and c = 16#FF#, "erased RAM: 00, FF, not " & integer'image(b) & ", " &
            integer'image(c));

      -- 16K mapping (R28 bit 4 clear): written bytes, and the aliasing a program sees
      for i in 0 to 15 loop
         ram_wr(16#1000# + i, (7 + 3 * i) mod 256);
      end loop;
      ram_wr(16#8000#, 16#A5#);
      for i in 0 to 15 loop
         peek(16#1000# + i, b);
         check(b = (7 + 3 * i) mod 256, "16K: peek of a written byte");
      end loop;
      peek(16#5000#, b);
      check(b = 7, "16K: $5000 is $1000, as for a program");
      peek(16#8000#, b);
      check(b = 16#A5#, "16K: $8000 apart from $0000");
      for i in C_SPOTS'range loop
         same_as_program(C_SPOTS(i), "16K");
      end loop;

      -- 64K mapping: four banks apart
      reg_wr(28, 16#30#);
      ready;
      ram_wr(16#0000#, 16#11#);
      ram_wr(16#4000#, 16#22#);
      ram_wr(16#8000#, 16#33#);
      ram_wr(16#C000#, 16#44#);
      ram_wr(16#0100#, 16#55#);
      peek(16#0000#, b);  check(b = 16#11#, "64K: $0000");
      peek(16#4000#, b);  check(b = 16#22#, "64K: $4000");
      peek(16#8000#, b);  check(b = 16#33#, "64K: $8000");
      peek(16#C000#, b);  check(b = 16#44#, "64K: $C000");
      peek(16#0100#, b);  check(b = 16#55#, "64K: $0100");
      for i in C_SPOTS'range loop
         same_as_program(C_SPOTS(i), "64K");
      end loop;
      reg_wr(28, 16#20#);
      ready;
      for i in C_SPOTS'range loop
         same_as_program(C_SPOTS(i), "16K again");
      end loop;

      -- Registers: what a program reads, without its side effects
      for r in 0 to 37 loop
         peek(C_CSR + r, b);
         reg_rd(r, c);
         check(b = c, "R" & integer'image(r) & ": peek " & integer'image(b) &
               ", a program reads " & integer'image(c));
      end loop;
      ready;
      cpu('0', '0', 0, c);
      peek(C_CSR + 38, b);
      -- bit 5 is the vertical blank, which may change between the two reads
      check(b / 64 = c / 64 and b mod 32 = c mod 32,
            "status: peek " & integer'image(b) & ", a program reads " & integer'image(c));
      cpu('0', '1', 26, c);                      -- select R26
      peek(C_CSR + 39, b);
      check(b = 26, "the selected register");

      seek(16#1234#);
      for i in 1 to 4 loop
         peek(C_CSR + 31, b);
      end loop;
      peek(C_CSR + 18, b);
      peek(C_CSR + 19, c);
      check(b = 16#12# and c = 16#34#, "peeks of R31 leave the update address");
      reg_rd(31, b);
      ready;
      peek(C_CSR + 19, c);
      check(c = 16#35#, "a program's read of R31 moves it");

      wait until rising_edge(vclk);
      lp_n <= '0';
      for i in 1 to 4 loop
         wait until rising_edge(vclk);
      end loop;
      lp_n <= '1';
      for i in 1 to 4 loop
         wait until rising_edge(vclk);
      end loop;
      peek(C_CSR + 38, b);
      check(b / 64 mod 2 = 1, "the light pen flag is set");
      peek(C_CSR + 16, c);
      peek(C_CSR + 17, c);
      peek(C_CSR + 38, b);
      check(b / 64 mod 2 = 1, "peeks of R16/R17 leave the light pen flag");
      reg_rd(16, c);
      peek(C_CSR + 38, b);
      check(b / 64 mod 2 = 0, "a program's read of R16 clears it");

      -- Writes to the device change nothing and don't wait
      peek(16#1000#, c);
      dev(16#1000#, '1', b, waits);
      check(waits = 0, "a write doesn't wait");
      peek(16#1000#, b);
      check(b = c, "a write changes nothing");
      dev(16#1001#, '0', b, waits);
      check(waits > 0, "a read waits for the VDC");

      -- Peeks while a block fill runs: old or new bytes only, and the fill is whole
      seek(16#2000#);
      reg_wr(31, 16#5A#);                        -- $2000, then 255 more by the fill
      ready;
      reg_wr(30, 16#FF#);
      b := 0;
      for i in 0 to 600 loop
         peek(16#2000# + (i * 37) mod 256, c);
         if c /= 16#5A# and c /= 16#FF# and c /= 16#00# then
            b := b + 1;
         end if;
      end loop;
      check(b = 0, "peeks during a fill see old or new bytes");
      ready;
      b := 0;
      for i in 0 to 255 loop
         peek(16#2000# + i, c);
         if c /= 16#5A# then
            b := b + 1;
         end if;
      end loop;
      check(b = 0, "the fill is whole: " & integer'image(b) & " bytes differ");
      peek(16#2100#, c);
      check(c = 16#00#, "and stops at $2100");
      peek(16#1000#, c);
      check(c = 7, "and leaves $1000");

      if fails = 0 then
         report "ALL CHECKS PASSED (" & integer'image(checks) & ")";
      else
         report integer'image(fails) & " of " & integer'image(checks) & " checks FAILED" severity error;
      end if;
      done <= true;
      wait;
   end process p_test;

end architecture sim;
