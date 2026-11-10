----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- cmdhd_rtc: the CMD HD's RTC-72421, showing the MEGA65's own clock.
--
-- The time comes from the M2M RTC vector (BCD, bit 64 flips on each change). Between
-- changes, and on a board whose RTC never answers, the seconds tick here, so the boot ROM's
-- delay loops (they watch the seconds digit) always finish. Writes to the time digits are
-- ignored: the MEGA65's clock is the one clock. The control registers keep what is written;
-- CF bit 2 picks 24 or 12 hours, as on Epson's part. BUSY always reads 0.
--
-- Done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cmdhd_rtc is
generic (
   G_CLK_HZ : natural := 31_528_000
);
port (
   clk_i   : in  std_logic;
   rtc_i   : in  std_logic_vector(64 downto 0);   -- M2M's RTC vector, core clock
   we_i    : in  std_logic;
   addr_i  : in  std_logic_vector(3 downto 0);
   data_i  : in  std_logic_vector(3 downto 0);
   data_o  : out std_logic_vector(3 downto 0)
);
end entity cmdhd_rtc;

architecture synthesis of cmdhd_rtc is

   signal sec, min, hour : std_logic_vector(7 downto 0) := x"00";   -- BCD
   signal day            : std_logic_vector(7 downto 0) := x"01";
   signal month          : std_logic_vector(7 downto 0) := x"01";
   signal year           : std_logic_vector(7 downto 0) := x"26";
   signal wday           : std_logic_vector(2 downto 0) := "000";
   signal toggle_d       : std_logic := '0';
   signal tick           : natural range 0 to G_CLK_HZ - 1 := 0;
   signal cd, ce, cf     : std_logic_vector(3 downto 0) := "0000";
   signal h12            : std_logic_vector(7 downto 0);   -- hour in 12 hour BCD, bit 7 PM

   -- BCD + 1 that wraps to 0 after last
   function bcd_inc(v : std_logic_vector(7 downto 0); last : std_logic_vector(7 downto 0))
      return std_logic_vector is
   begin
      if v = last then
         return x"00";
      elsif v(3 downto 0) = x"9" then
         return std_logic_vector(unsigned(v(7 downto 4)) + 1) & x"0";
      else
         return v(7 downto 4) & std_logic_vector(unsigned(v(3 downto 0)) + 1);
      end if;
   end function;

begin

   -- 24 hours to 12: 00 -> 12 AM, 12 -> 12 PM, 13..23 -> 1..11 PM
   p_h12 : process (all)
      variable h : natural range 0 to 39;
   begin
      h := to_integer(unsigned(hour(7 downto 4))) * 10 + to_integer(unsigned(hour(3 downto 0)));
      if h = 0 then
         h12 <= x"12";
      elsif h < 12 then
         h12 <= hour;
      elsif h = 12 then
         h12 <= x"92";
      else
         h := h - 12;
         h12 <= '1' & std_logic_vector(to_unsigned(h / 10, 3)) &
                std_logic_vector(to_unsigned(h mod 10, 4));
      end if;
   end process p_h12;

   p_read : process (all)
   begin
      case addr_i is
         when x"0"   => data_o <= sec(3 downto 0);
         when x"1"   => data_o <= '0' & sec(6 downto 4);
         when x"2"   => data_o <= min(3 downto 0);
         when x"3"   => data_o <= '0' & min(6 downto 4);
         when x"4"   => data_o <= hour(3 downto 0) when cf(2) = '1' else h12(3 downto 0);
         when x"5"   => data_o <= "00" & hour(5 downto 4) when cf(2) = '1' else
                                  '0' & h12(7) & h12(5 downto 4);
         when x"6"   => data_o <= day(3 downto 0);
         when x"7"   => data_o <= "00" & day(5 downto 4);
         when x"8"   => data_o <= month(3 downto 0);
         when x"9"   => data_o <= "000" & month(4);
         when x"A"   => data_o <= year(3 downto 0);
         when x"B"   => data_o <= year(7 downto 4);
         when x"C"   => data_o <= '0' & wday;
         when x"D"   => data_o <= cd(3 downto 2) & '0' & cd(0);
         when x"E"   => data_o <= ce;
         when others => data_o <= cf;
      end case;
   end process p_read;

   p_clock : process (clk_i)
   begin
      if rising_edge(clk_i) then
         toggle_d <= rtc_i(64);
         if rtc_i(64) /= toggle_d then
            sec   <= rtc_i(7 downto 0);
            min   <= rtc_i(15 downto 8);
            hour  <= rtc_i(23 downto 16);
            day   <= rtc_i(31 downto 24);
            month <= rtc_i(39 downto 32);
            year  <= rtc_i(47 downto 40);
            wday  <= rtc_i(50 downto 48);
            tick  <= 0;
         elsif tick = G_CLK_HZ - 1 then
            tick <= 0;
            sec  <= bcd_inc(sec, x"59");
            if sec = x"59" then
               min <= bcd_inc(min, x"59");
               if min = x"59" then
                  hour <= bcd_inc(hour, x"23");
               end if;
            end if;
         else
            tick <= tick + 1;
         end if;

         if we_i = '1' then
            case addr_i is
               when x"D"   => cd <= data_i;
               when x"E"   => ce <= data_i;
               when x"F"   => cf <= data_i;
               when others => null;
            end case;
         end if;
      end if;
   end process p_clock;

end architecture synthesis;
