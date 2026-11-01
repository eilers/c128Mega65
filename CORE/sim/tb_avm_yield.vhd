----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- tb_avm_yield: avm_yield.vhd, the system CPU giving HyperRAM turns to the HDMI scaler.
--
-- The scaler (P) reads and writes bursts with random gaps, in phases: normal, busy without
-- a break, and idle. The CPU (S) reads and writes bursts of one or two words, the way
-- rv_hr_bridge does, against an arbiter port that is busy at random. Checks: a request of S
-- reaches the arbiter only once P has been idle G_QUIET clocks with no read data to come,
-- or after G_LIMIT clocks of waiting; once there it stays until taken; a write burst is
-- never cut; S sees a request taken exactly when the arbiter takes it; every request of S
-- gets through, even while P is never idle.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity tb_avm_yield is
end entity tb_avm_yield;

architecture sim of tb_avm_yield is

   constant C_CLK   : time    := 10 ns;
   constant C_QUIET : natural := 8;
   constant C_LIMIT : natural := 300;
   constant C_TXNS  : natural := 800;
   constant C_SLACK : natural := 8;      -- the arbiter port's longest busy run, and then some

   signal clk       : std_logic := '0';
   signal rst       : std_logic := '1';
   signal running   : boolean := true;

   -- P, the master yielded to, and its memory
   signal p_write   : std_logic := '0';
   signal p_read    : std_logic := '0';
   signal p_bc      : std_logic_vector(7 downto 0) := x"10";
   signal p_wait    : std_logic := '1';
   signal p_rvalid  : std_logic := '0';

   -- S, the yielding master, and the arbiter's port for it
   signal s_write   : std_logic := '0';
   signal s_read    : std_logic := '0';
   signal s_bc      : std_logic_vector(7 downto 0) := x"01";
   signal s_wait    : std_logic;
   signal m_write   : std_logic;
   signal m_read    : std_logic;
   signal m_wait    : std_logic := '1';

   signal s_done    : natural := 0;
   signal s_errors  : natural := 0;
   signal c_errors  : natural := 0;
   signal by_quiet  : natural := 0;
   signal by_limit  : natural := 0;

   procedure rand(variable s1, s2 : inout positive; n : positive; variable v : out natural) is
      variable r : real;
   begin
      uniform(s1, s2, r);
      v := natural(floor(r * real(n)));
   end procedure rand;

begin

   clk <= not clk after C_CLK / 2 when running;

   i_dut : entity work.avm_yield
      generic map (
         G_QUIET => C_QUIET,
         G_LIMIT => C_LIMIT
      )
      port map (
         clk_i                 => clk,
         rst_i                 => rst,
         p_avm_write_i         => p_write,
         p_avm_read_i          => p_read,
         p_avm_burstcount_i    => p_bc,
         p_avm_waitrequest_i   => p_wait,
         p_avm_readdatavalid_i => p_rvalid,
         s_avm_write_i         => s_write,
         s_avm_read_i          => s_read,
         s_avm_burstcount_i    => s_bc,
         s_avm_waitrequest_o   => s_wait,
         m_avm_write_o         => m_write,
         m_avm_read_o          => m_read,
         m_avm_waitrequest_i   => m_wait
      ); -- i_dut

   -- The two arbiter ports: busy at random, never for more than four clocks in a row
   p_ports : process
      variable s1 : positive := 3;
      variable s2 : positive := 5;
      variable v  : natural;
      variable pw, mw : natural := 0;
   begin
      wait until rising_edge(clk);
      rand(s1, s2, 2, v);
      if v = 1 and pw < 4 then
         p_wait <= '1';
         pw := pw + 1;
      else
         p_wait <= '0';
         pw := 0;
      end if;
      rand(s1, s2, 2, v);
      if v = 1 and mw < 4 then
         m_wait <= '1';
         mw := mw + 1;
      else
         m_wait <= '0';
         mw := 0;
      end if;
   end process p_ports;

   -- P: bursts with gaps; 3000 clocks normal, 3000 never idle for G_QUIET, 500 idle
   p_master : process
      variable s1 : positive := 11;
      variable s2 : positive := 17;
      variable v, n, gap : natural;
      variable cycle : natural := 0;
      variable phase : natural;

      procedure tick is
      begin
         wait until rising_edge(clk);
         cycle := cycle + 1;
      end procedure tick;
   begin
      wait until rst = '0';
      while running loop
         phase := (cycle mod 6500) / 3000;   -- 0 normal, 1 busy, 2 idle
         if phase = 2 then
            tick;
         else
            rand(s1, s2, 24, n);
            n := n + 2;
            p_bc <= std_logic_vector(to_unsigned(n, 8));
            rand(s1, s2, 4, v);
            if v /= 0 then
               -- A read: taken, then its data after a latency, with gaps
               p_read <= '1';
               loop
                  tick;
                  exit when p_wait = '0';
               end loop;
               p_read <= '0';
               rand(s1, s2, 12, v);
               for i in 0 to v loop
                  tick;
               end loop;
               for i in 1 to n loop
                  p_rvalid <= '1';
                  tick;
                  rand(s1, s2, 6, v);
                  if v = 0 then
                     p_rvalid <= '0';
                     tick;
                  end if;
               end loop;
               p_rvalid <= '0';
            else
               -- A write burst
               p_write <= '1';
               v := 0;
               while v < n loop
                  tick;
                  if p_wait = '0' then
                     v := v + 1;
                  end if;
               end loop;
               p_write <= '0';
            end if;
            if phase = 1 then
               rand(s1, s2, C_QUIET - 1, gap);
            else
               rand(s1, s2, 3 * C_QUIET, gap);
            end if;
            for i in 1 to gap loop
               tick;
            end loop;
         end if;
      end loop;
      wait;
   end process p_master;

   -- S: reads of 16 or 2 words, writes of 1 or 2, as rv_hr_bridge
   s_master : process
      variable s1 : positive := 23;
      variable s2 : positive := 29;
      variable v, n, beats, waited : natural;
   begin
      wait for 5 * C_CLK;
      wait until rising_edge(clk);
      rst <= '0';
      for t in 1 to C_TXNS loop
         rand(s1, s2, 4, v);
         waited := 0;
         if v < 2 then
            s_bc <= x"10" when v = 0 else x"02";
            s_read <= '1';
            loop
               wait until rising_edge(clk);
               exit when s_wait = '0';
               waited := waited + 1;
            end loop;
            s_read <= '0';
         else
            n := v - 1;
            s_bc <= std_logic_vector(to_unsigned(n, 8));
            s_write <= '1';
            beats := 0;
            while beats < n loop
               wait until rising_edge(clk);
               if s_wait = '0' then
                  beats := beats + 1;
               elsif beats = 0 then
                  waited := waited + 1;
               end if;
            end loop;
            s_write <= '0';
         end if;
         if waited > C_LIMIT + C_SLACK then
            report "S waited " & integer'image(waited) & " clocks" severity error;
            s_errors <= s_errors + 1;
         end if;
         s_done <= t;
         rand(s1, s2, 4, v);
         for i in 1 to v loop
            wait until rising_edge(clk);
         end loop;
      end loop;
      wait;
   end process s_master;

   -- The rules, against a model of P's activity of its own
   checker : process (clk)
      variable pending   : integer := 0;
      variable quiet     : natural := 0;
      variable p_busy    : boolean;
      variable s_req     : std_logic;
      variable m_req     : std_logic;
      variable presented : boolean := false;   -- S's request is at the arbiter, not taken
      variable wr_left   : natural := 0;       -- a write burst's beats after its first
      variable held_back : natural := 0;
      variable errs      : natural := 0;

      procedure fail(msg : string) is
      begin
         report msg & " at " & time'image(now) severity error;
         errs := errs + 1;
      end procedure fail;
   begin
      if rising_edge(clk) and rst = '0' then
         p_busy := p_read = '1' or p_write = '1' or p_rvalid = '1' or pending /= 0;
         s_req  := s_read or s_write;
         m_req  := m_read or m_write;

         if presented and m_req = '0' then
            fail("a request was taken back from the arbiter");
         end if;
         if m_req = '1' and s_req = '0' then
            fail("a request out of nothing");
         end if;
         if (s_req = '1' and s_wait = '0') /= (m_req = '1' and m_wait = '0') then
            fail("S and the arbiter disagree on a request being taken");
         end if;
         if wr_left > 0 and s_write = '1' and m_write = '0' then
            fail("a write burst was cut");
         end if;
         if m_req = '1' and not presented and wr_left = 0 then
            if held_back >= C_LIMIT then
               by_limit <= by_limit + 1;
            elsif quiet >= C_QUIET and not p_busy then
               by_quiet <= by_quiet + 1;
            else
               fail("a request went on while P was busy");
            end if;
            presented := true;
         end if;

         if m_req = '1' and m_wait = '0' then
            presented := false;
            held_back := 0;
            if m_write = '1' then
               if wr_left = 0 then
                  wr_left := to_integer(unsigned(s_bc)) - 1;
               else
                  wr_left := wr_left - 1;
               end if;
            end if;
         elsif s_req = '1' and m_req = '0' then
            held_back := held_back + 1;
         end if;

         if p_read = '1' and p_wait = '0' then
            pending := pending + to_integer(unsigned(p_bc));
         end if;
         if p_rvalid = '1' then
            pending := pending - 1;
         end if;
         if p_busy then
            quiet := 0;
         elsif quiet < C_QUIET then
            quiet := quiet + 1;
         end if;
         c_errors <= errs;
      end if;
   end process checker;

   p_end : process
   begin
      wait until s_done = C_TXNS;
      wait for 10 * C_CLK;
      report integer'image(C_TXNS) & " requests: " & integer'image(by_quiet) & " after a quiet spell, " &
             integer'image(by_limit) & " after the longest wait";
      if by_quiet = 0 or by_limit = 0 then
         report "a way through was never taken" severity error;
      elsif s_errors + c_errors = 0 then
         report "ALL CHECKS PASSED";
      else
         report "FAILED with " & integer'image(s_errors + c_errors) & " errors" severity error;
      end if;
      running <= false;
      wait;
   end process p_end;

end architecture sim;
