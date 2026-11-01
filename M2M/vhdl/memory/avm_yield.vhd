----------------------------------------------------------------------------------
-- MiSTer2MEGA65 Framework
--
-- One Avalon master yields to another in front of their arbiter: the system CPU to the
-- HDMI scaler (framework.vhd). A request of the yielding master goes on only once the other
-- has been idle, with no read data to come, for G_QUIET clocks, so the other's bursts run
-- back to back. A request that has waited G_LIMIT clocks goes anyway. Once passed on, a
-- request stays until it is taken, and a write burst goes on to its end.
--
-- C128 core: licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity avm_yield is
   generic (
      G_QUIET : natural := 32;                              -- clocks the other must be idle
      G_LIMIT : natural := 16384                            -- longest wait for a request
   );
   port (
      clk_i                 : in  std_logic;
      rst_i                 : in  std_logic;

      -- The master to yield to, as its arbiter port sees it
      p_avm_write_i         : in  std_logic;
      p_avm_read_i          : in  std_logic;
      p_avm_burstcount_i    : in  std_logic_vector(7 downto 0);
      p_avm_waitrequest_i   : in  std_logic;
      p_avm_readdatavalid_i : in  std_logic;

      -- The yielding master; its address, data and burst count go to the arbiter directly
      s_avm_write_i         : in  std_logic;
      s_avm_read_i          : in  std_logic;
      s_avm_burstcount_i    : in  std_logic_vector(7 downto 0);
      s_avm_waitrequest_o   : out std_logic;
      m_avm_write_o         : out std_logic;
      m_avm_read_o          : out std_logic;
      m_avm_waitrequest_i   : in  std_logic
   );
end entity avm_yield;

architecture synthesis of avm_yield is

   signal pending : unsigned(8 downto 0) := (others => '0');   -- the other's read words to come
   signal quiet   : natural range 0 to G_QUIET := 0;
   signal waited  : natural range 0 to G_LIMIT := 0;
   signal held    : std_logic := '0';                          -- passed on: not taken yet, or
   signal wr_left : unsigned(7 downto 0) := (others => '0');  -- a write burst's beats to come
   signal busy    : std_logic;
   signal req     : std_logic;
   signal pass    : std_logic;

begin

   busy <= '1' when p_avm_read_i = '1' or p_avm_write_i = '1' or p_avm_readdatavalid_i = '1' or
                    pending /= 0 else '0';
   req  <= s_avm_read_i or s_avm_write_i;
   pass <= '1' when held = '1' or (quiet = G_QUIET and busy = '0') or waited = G_LIMIT else '0';

   m_avm_read_o        <= s_avm_read_i and pass;
   m_avm_write_o       <= s_avm_write_i and pass;
   s_avm_waitrequest_o <= m_avm_waitrequest_i or not pass;

   p_yield : process (clk_i)
      variable v_pending : unsigned(8 downto 0);
      variable v_left    : unsigned(7 downto 0);
   begin
      if rising_edge(clk_i) then
         -- The other master's read words still to come
         v_pending := pending;
         if p_avm_read_i = '1' and p_avm_waitrequest_i = '0' then
            v_pending := v_pending + unsigned(p_avm_burstcount_i);
         end if;
         if p_avm_readdatavalid_i = '1' then
            v_pending := v_pending - 1;
         end if;
         pending <= v_pending;

         if busy = '1' then
            quiet <= 0;
         elsif quiet < G_QUIET then
            quiet <= quiet + 1;
         end if;

         if req = '1' and pass = '1' then
            if m_avm_waitrequest_i = '0' then
               -- Taken: a read is done here, a write burst goes on to its last beat
               held   <= '0';
               waited <= 0;
               if s_avm_write_i = '1' then
                  if wr_left = 0 then
                     v_left := unsigned(s_avm_burstcount_i) - 1;
                  else
                     v_left := wr_left - 1;
                  end if;
                  wr_left <= v_left;
                  if v_left /= 0 then
                     held <= '1';
                  end if;
               end if;
            else
               held <= '1';
            end if;
         elsif req = '1' then
            if waited < G_LIMIT then
               waited <= waited + 1;
            end if;
         else
            waited <= 0;
            if wr_left = 0 then
               held <= '0';
            end if;
         end if;

         if rst_i = '1' then
            pending <= (others => '0');
            quiet   <= 0;
            waited  <= 0;
            held    <= '0';
            wr_left <= (others => '0');
         end if;
      end if;
   end process p_yield;

end architecture synthesis;
