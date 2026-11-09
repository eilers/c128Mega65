----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- cmdhd_i8255: the CMD HD's 8255 (U11), modes 0 only, as the boot ROM and HD DOS use it.
--
-- A mode set clears the output latches, as on Intel's part: the boot ROM and the DOS run
-- their mode sets from RAM ("CMD HD" signature: STA $8803 / STX $8802) because port C bit 0
-- switches the boot ROM. A port that is an input shows its pull-ups (pa_pull_i etc.) on the
-- outputs, which is how port C enables the ROM after a reset. Bit set/reset is supported.
--
-- Done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cmdhd_i8255 is
port (
   clk_i     : in  std_logic;
   rst_i     : in  std_logic;
   we_i      : in  std_logic;                     -- one clock per CPU write
   addr_i    : in  std_logic_vector(1 downto 0);
   data_i    : in  std_logic_vector(7 downto 0);
   data_o    : out std_logic_vector(7 downto 0);  -- what a read of addr_i returns

   pa_i      : in  std_logic_vector(7 downto 0);  -- pins, when inputs
   pb_i      : in  std_logic_vector(7 downto 0);
   pc_i      : in  std_logic_vector(7 downto 0);
   pa_o      : out std_logic_vector(7 downto 0);  -- pins: the latch, or the pull-ups
   pb_o      : out std_logic_vector(7 downto 0);
   pc_o      : out std_logic_vector(7 downto 0);
   pa_out_o  : out std_logic                      -- port A drives its pins
);
end entity cmdhd_i8255;

architecture synthesis of cmdhd_i8255 is

   constant C_PA_PULL : std_logic_vector(7 downto 0) := x"FF";
   constant C_PB_PULL : std_logic_vector(7 downto 0) := x"7F";
   constant C_PC_PULL : std_logic_vector(7 downto 0) := x"E3";   -- ROM on, RAM window low

   signal ctrl           : std_logic_vector(7 downto 0) := x"9B";   -- every port an input
   signal la, lb, lc     : std_logic_vector(7 downto 0) := (others => '0');

   alias pa_in  : std_logic is ctrl(4);
   alias pch_in : std_logic is ctrl(3);
   alias pb_in  : std_logic is ctrl(1);
   alias pcl_in : std_logic is ctrl(0);

begin

   pa_o     <= C_PA_PULL when pa_in = '1' else la;
   pb_o     <= C_PB_PULL when pb_in = '1' else lb;
   pc_o(7 downto 4) <= C_PC_PULL(7 downto 4) when pch_in = '1' else lc(7 downto 4);
   pc_o(3 downto 0) <= C_PC_PULL(3 downto 0) when pcl_in = '1' else lc(3 downto 0);
   pa_out_o <= not pa_in;

   p_read : process (all)
   begin
      case addr_i is
         when "00" =>
            data_o <= la;
            if pa_in = '1' then
               data_o <= pa_i;
            end if;
         when "01" =>
            data_o <= lb;
            if pb_in = '1' then
               data_o <= pb_i;
            end if;
         when "10" =>
            data_o <= lc;
            if pch_in = '1' then
               data_o(7 downto 4) <= pc_i(7 downto 4);
            end if;
            if pcl_in = '1' then
               data_o(3 downto 0) <= pc_i(3 downto 0);
            end if;
         when others =>
            data_o <= x"FF";                     -- the control word cannot be read
      end case;
   end process p_read;

   p_regs : process (clk_i)
      variable bit_n : natural range 0 to 7;
   begin
      if rising_edge(clk_i) then
         if we_i = '1' then
            case addr_i is
               when "00"   => la <= data_i;
               when "01"   => lb <= data_i;
               when "10"   => lc <= data_i;
               when others =>
                  if data_i(7) = '1' then              -- mode set: the latches go to 0
                     ctrl <= data_i;
                     la   <= (others => '0');
                     lb   <= (others => '0');
                     lc   <= (others => '0');
                  else                                 -- port C bit set/reset
                     bit_n := to_integer(unsigned(data_i(3 downto 1)));
                     lc(bit_n) <= data_i(0);
                  end if;
            end case;
         end if;

         if rst_i = '1' then
            ctrl <= x"9B";
            la   <= (others => '0');
            lb   <= (others => '0');
            lc   <= (others => '0');
         end if;
      end if;
   end process p_regs;

end architecture synthesis;
