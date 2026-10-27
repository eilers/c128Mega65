----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- Testbench for the RISC-V CPU's HyperRAM bridge (M2M/vhdl/rv/rv_hr_bridge.vhd), driven
-- the way NEORV32's i-cache drives it: a line fill is one request, its answer, then the
-- other 15 on consecutive clocks. Checks:
--   1. a line fill straight after the reset, while the bridge still settles, is held and
--      answered (it used to get an error, and the rest of the line wedged the bridge)
--   2. a stray word from the middle of a line gets an error, and the bridge still works
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

   -- HyperRAM word W holds W, so CPU byte address B reads words B/2 and B/2+1
   function expected(b : natural) return std_logic_vector is
   begin
      return std_logic_vector(to_unsigned((b / 2 + 1) mod 2 ** 16, 16)) &
             std_logic_vector(to_unsigned((b / 2) mod 2 ** 16, 16));
   end function expected;

begin

   clk <= not clk after C_PERIOD / 2 when not done;

   i_dut : entity work.rv_hr_bridge
      generic map (
         G_LINE_BYTES => 64
      )
      port map (
         clk_i               => clk,
         rst_i               => rst,
         stb_i               => stb,
         adr_i               => adr,
         we_i                => '0',
         sel_i               => "1111",
         dat_i               => (others => '0'),
         cti_i               => cti,
         dat_o               => dat,
         ack_o               => ack,
         err_o               => err,
         avm_write_o         => open,
         avm_read_o          => hr_read,
         avm_address_o       => hr_address,
         avm_writedata_o     => open,
         avm_byteenable_o    => open,
         avm_burstcount_o    => hr_burstcount,
         avm_readdata_i      => hr_readdata,
         avm_readdatavalid_i => hr_readdatavalid,
         avm_waitrequest_i   => hr_waitrequest
      ); -- i_dut

   -- HyperRAM: one read burst at a time, waitrequest now and then
   p_hr : process
      variable lfsr  : std_logic_vector(7 downto 0) := x"5A";
      variable a     : natural;
      variable count : natural;
   begin
      wait until rising_edge(clk);
      hr_readdatavalid <= '0';
      lfsr := lfsr(6 downto 0) & (lfsr(7) xor lfsr(5) xor lfsr(4) xor lfsr(3));
      if hr_waitrequest = '0' and hr_read = '1' then
         a     := to_integer(unsigned(hr_address(23 downto 0)));
         count := to_integer(unsigned(hr_burstcount));
         assert count >= 1 and count <= 16 report "HyperRAM burst of " & integer'image(count) severity failure;
         hr_waitrequest <= '1';
         for i in 1 to C_HR_LATENCY loop
            wait until rising_edge(clk);
         end loop;
         for i in 0 to count - 1 loop
            hr_readdata      <= std_logic_vector(to_unsigned((a + i) mod 2 ** 16, 16));
            hr_readdatavalid <= '1';
            wait until rising_edge(clk);
         end loop;
         hr_readdatavalid <= '0';
      end if;
      hr_waitrequest <= lfsr(0) and lfsr(2);
   end process p_hr;

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

      variable ok : boolean;

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

      report "ALL CHECKS PASSED";
      done <= true;
      wait;
   end process p_cpu;

end architecture sim;
