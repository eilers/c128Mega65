----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Testbench for the RISC-V CPU's HyperRAM bridge (M2M/vhdl/rv/rv_hr_bridge.vhd), driven
-- the way NEORV32's i-cache drives it: a line fill is one request, its answer, then the
-- other 15 on consecutive clocks. Checks:
--   1. a line fill straight after the reset, while the bridge still settles, is held and
--      answered (it used to get an error, and the rest of the line wedged the bridge)
--   2. a stray word from the middle of a line gets an error, and the bridge still works
--   3. a reset of the HyperRAM side (with the core's) in the middle of a line fill, in
--      its first burst and in its second: the rest of the line comes once it is over
--      (the burst used to be lost, and the CPU waited for it for ever)
--   4. a write that such a reset catches is written again, whole, and answered once
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity tb_rv_hr_bridge is
end entity tb_rv_hr_bridge;

architecture sim of tb_rv_hr_bridge is

   constant C_PERIOD     : time    := 10 ns;
   constant C_HR_LATENCY : natural := 16;
   constant C_ANSWER     : natural := 500;         -- clocks to wait for an answer

   signal clk  : std_logic := '0';
   signal rst  : std_logic := '1';
   signal done : boolean := false;

   -- The HyperRAM side's reset: after the n-th answer of a line fill (3), or now (4)
   signal hr_rst       : std_logic;
   signal hr_rst_fill  : std_logic := '0';
   signal hr_rst_now   : std_logic := '0';
   signal rst_after    : natural := 0;

   signal we   : std_logic := '0';
   signal sel  : std_logic_vector(3 downto 0) := "1111";
   signal wdat : std_logic_vector(31 downto 0) := (others => '0');

   signal stb  : std_logic := '0';
   signal adr  : std_logic_vector(23 downto 0) := (others => '0');
   signal cti  : std_logic_vector(2 downto 0) := "000";
   signal dat  : std_logic_vector(31 downto 0);
   signal ack  : std_logic;
   signal err  : std_logic;

   signal hr_read          : std_logic;
   signal hr_address       : std_logic_vector(31 downto 0);
   signal hr_burstcount    : std_logic_vector(7 downto 0);
   signal hr_readdata      : std_logic_vector(15 downto 0) := (others => '0');
   signal hr_readdatavalid : std_logic := '0';
   signal hr_waitrequest   : std_logic := '1';
   signal hr_write         : std_logic;
   signal hr_writedata     : std_logic_vector(15 downto 0);

   -- The writes HyperRAM took: how many, and the last two (address & data)
   type t_wlog is array (0 to 1) of std_logic_vector(47 downto 0);
   signal w_count : natural := 0;
   signal w_log   : t_wlog;

   -- HyperRAM word W holds W, so CPU byte address B reads words B/2 and B/2+1
   function expected(b : natural) return std_logic_vector is
   begin
      return std_logic_vector(to_unsigned((b / 2 + 1) mod 2 ** 16, 16)) &
             std_logic_vector(to_unsigned((b / 2) mod 2 ** 16, 16));
   end function expected;

begin

   clk <= not clk after C_PERIOD / 2 when not done;
   hr_rst <= hr_rst_fill or hr_rst_now;

   i_dut : entity work.rv_hr_bridge
      generic map (
         G_LINE_BYTES => 64
      )
      port map (
         clk_i               => clk,
         rst_i               => rst,
         hr_rst_i            => hr_rst,
         stb_i               => stb,
         adr_i               => adr,
         we_i                => we,
         sel_i               => sel,
         dat_i               => wdat,
         cti_i               => cti,
         dat_o               => dat,
         ack_o               => ack,
         err_o               => err,
         avm_write_o         => hr_write,
         avm_read_o          => hr_read,
         avm_address_o       => hr_address,
         avm_writedata_o     => hr_writedata,
         avm_byteenable_o    => open,
         avm_burstcount_o    => hr_burstcount,
         avm_readdata_i      => hr_readdata,
         avm_readdatavalid_i => hr_readdatavalid,
         avm_waitrequest_i   => hr_waitrequest
      ); -- i_dut

   -- HyperRAM: one read burst at a time, waitrequest now and then. Its reset loses the
   -- burst in flight, and it starts again a while after.
   p_hr : process
      variable lfsr  : std_logic_vector(7 downto 0) := x"5A";
      variable a     : natural;
      variable count : natural;
      variable init  : natural := 0;
      variable w_adr : unsigned(31 downto 0);   -- a write burst's next word
      variable w_left : natural := 0;
   begin
      wait until rising_edge(clk);
      hr_readdatavalid <= '0';
      lfsr := lfsr(6 downto 0) & (lfsr(7) xor lfsr(5) xor lfsr(4) xor lfsr(3));
      if hr_rst = '1' then
         init   := 20;
         w_left := 0;
      elsif init > 0 then
         init := init - 1;
      elsif hr_waitrequest = '0' and hr_read = '1' then
         a     := to_integer(unsigned(hr_address(23 downto 0)));
         count := to_integer(unsigned(hr_burstcount));
         assert count >= 1 and count <= 16 report "HyperRAM burst of " & integer'image(count) severity failure;
         hr_waitrequest <= '1';
         for i in 1 to C_HR_LATENCY loop
            wait until rising_edge(clk);
            exit when hr_rst = '1';
         end loop;
         for i in 0 to count - 1 loop
            exit when hr_rst = '1';
            hr_readdata      <= std_logic_vector(to_unsigned((a + i) mod 2 ** 16, 16));
            hr_readdatavalid <= '1';
            wait until rising_edge(clk);
         end loop;
         hr_readdatavalid <= '0';
         if hr_rst = '1' then
            init := 20;
         end if;
      elsif hr_waitrequest = '0' and hr_write = '1' then
         if w_left = 0 then
            w_adr  := unsigned(hr_address);
            w_left := to_integer(unsigned(hr_burstcount));
         end if;
         w_log(0) <= w_log(1);
         w_log(1) <= std_logic_vector(w_adr) & hr_writedata;
         w_count  <= w_count + 1;
         w_adr    := w_adr + 1;
         w_left   := w_left - 1;
      end if;
      if hr_rst = '1' or init > 0 then
         hr_waitrequest <= '1';
      else
         hr_waitrequest <= lfsr(0) and lfsr(2);
      end if;
   end process p_hr;

   -- 3.: the HyperRAM side's reset for 40 clocks, after the rst_after-th answer
   p_hr_rst : process
      variable n : natural;
   begin
      wait until rst_after > 0;
      n := 0;
      while n < rst_after loop
         wait until rising_edge(clk);
         if ack = '1' then
            n := n + 1;
         end if;
      end loop;
      hr_rst_fill <= '1';
      for i in 1 to 40 loop
         wait until rising_edge(clk);
      end loop;
      hr_rst_fill <= '0';
      wait until rst_after = 0;
   end process p_hr_rst;

   p_cpu : process

      -- One request; true when it was answered without an error and with the right data
      procedure single(b : natural; t : std_logic_vector(2 downto 0); ok : out boolean) is
      begin
         stb <= '1';
         adr <= std_logic_vector(to_unsigned(b, 24));
         cti <= t;
         wait until rising_edge(clk);
         stb <= '0';
         for i in 1 to C_ANSWER loop
            wait until rising_edge(clk);
            if ack = '1' then
               ok := err = '0' and dat = expected(b);
               return;
            end if;
         end loop;
         report "no answer at " & to_hstring(to_unsigned(b, 24)) severity error;
         ok := false;
      end procedure single;

      -- A line fill as the i-cache sends it
      procedure fill(base : natural; ok : out boolean) is
         variable sent, got : natural;
         variable good      : boolean;
         variable clocks    : natural := 0;
      begin
         single(base, "010", good);
         sent := 1;
         got  := 1;
         while got < 16 loop
            if sent < 16 then
               stb <= '1';
               adr <= std_logic_vector(to_unsigned(base + 4 * sent, 24));
               cti <= "010" when sent < 15 else "000";
               sent := sent + 1;
            else
               stb <= '0';
            end if;
            wait until rising_edge(clk);
            if ack = '1' then
               good := good and err = '0' and dat = expected(base + 4 * got);
               got  := got + 1;
            end if;
            clocks := clocks + 1;
            if clocks > C_ANSWER then
               report "line fill at " & to_hstring(to_unsigned(base, 24)) & " stuck after " &
                      integer'image(got) & " words" severity error;
               exit;
            end if;
         end loop;
         stb <= '0';
         ok  := good and got = 16;
      end procedure fill;

      variable ok     : boolean;
      variable acks   : natural;
      variable errs   : natural;
      variable before : natural;

   begin
      for i in 1 to 4 loop
         wait until rising_edge(clk);
      end loop;
      rst <= '0';

      fill(16#000040#, ok);
      assert ok report "1: line fill straight after the reset failed" severity error;

      for i in 1 to 100 loop                     -- past the settle time, whatever 1 did
         wait until rising_edge(clk);
      end loop;
      single(16#000084#, "010", ok);
      assert not ok and err = '1' report "2: stray line word not refused" severity error;
      single(16#000100#, "000", ok);
      assert ok report "2: single read after the stray word failed" severity error;
      fill(16#000200#, ok);
      assert ok report "2: line fill after the stray word failed" severity error;

      rst_after <= 5;                             -- in the first burst of the line
      fill(16#000400#, ok);
      assert ok report "3: line fill with a HyperRAM reset after 5 words failed" severity error;
      rst_after <= 0;
      wait until rising_edge(clk);
      rst_after <= 12;                            -- in the second
      fill(16#000800#, ok);
      assert ok report "3: line fill with a HyperRAM reset after 12 words failed" severity error;
      rst_after <= 0;
      single(16#000104#, "000", ok);
      assert ok report "3: single read after the resets failed" severity error;

      -- 4.
      before := w_count;
      stb  <= '1';
      we   <= '1';
      adr  <= x"000300";
      wdat <= x"BEEFCAFE";
      wait until rising_edge(clk);
      stb  <= '0';
      we   <= '0';
      hr_rst_now <= '1';
      acks := 0;
      errs := 0;
      for i in 1 to C_ANSWER loop
         wait until rising_edge(clk);
         if i = 30 then
            hr_rst_now <= '0';
         end if;
         if ack = '1' then
            acks := acks + 1;
            if err = '1' then
               errs := errs + 1;
            end if;
         end if;
      end loop;
      assert acks = 1 and errs = 0 report "4: the write was answered " & integer'image(acks) & " times" severity error;
      assert w_count > before and w_log(0) = x"00000180" & x"CAFE" and w_log(1) = x"00000181" & x"BEEF"
         report "4: the write did not reach HyperRAM whole" severity error;
      fill(16#000200#, ok);
      assert ok report "4: line fill after the write failed" severity error;

      report "ALL CHECKS PASSED";
      done <= true;
      wait;
   end process p_cpu;

end architecture sim;
