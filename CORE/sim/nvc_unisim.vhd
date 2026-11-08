----------------------------------------------------------------------------------
-- Behavioural models of the Xilinx primitives the network device uses (ODDR, DNA_PORT),
-- compiled as library unisim for nvc, which has no Xilinx libraries. Simulation only.
--
-- C128 core: licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

package vcomponents is

   component ODDR
      generic (
         DDR_CLK_EDGE : string := "OPPOSITE_EDGE";
         INIT         : bit    := '0';
         SRTYPE       : string := "SYNC"
      );
      port (
         Q  : out std_ulogic;
         C  : in  std_ulogic;
         CE : in  std_ulogic;
         D1 : in  std_ulogic;
         D2 : in  std_ulogic;
         R  : in  std_ulogic := 'L';
         S  : in  std_ulogic := 'L'
      );
   end component;

   component DNA_PORT
      generic (
         SIM_DNA_VALUE : bit_vector := X"000000000000000"
      );
      port (
         DOUT  : out std_ulogic;
         CLK   : in  std_ulogic;
         DIN   : in  std_ulogic;
         READ  : in  std_ulogic;
         SHIFT : in  std_ulogic
      );
   end component;

end package vcomponents;

library ieee;
use ieee.std_logic_1164.all;

entity ODDR is
   generic (
      DDR_CLK_EDGE : string := "OPPOSITE_EDGE";
      INIT         : bit    := '0';
      SRTYPE       : string := "SYNC"
   );
   port (
      Q  : out std_ulogic;
      C  : in  std_ulogic;
      CE : in  std_ulogic;
      D1 : in  std_ulogic;
      D2 : in  std_ulogic;
      R  : in  std_ulogic := 'L';
      S  : in  std_ulogic := 'L'
   );
end entity ODDR;

-- D1 from the rising edge, D2 from the falling one
architecture sim of ODDR is
   signal d2_r : std_ulogic := to_stdulogic(INIT);
begin
   process (C)
   begin
      if rising_edge(C) and CE = '1' then
         Q    <= D1;
         d2_r <= D2;
      elsif falling_edge(C) and CE = '1' then
         Q <= d2_r;
      end if;
   end process;
end architecture sim;

library ieee;
use ieee.std_logic_1164.all;

entity DNA_PORT is
   generic (
      SIM_DNA_VALUE : bit_vector := X"000000000000000"
   );
   port (
      DOUT  : out std_ulogic;
      CLK   : in  std_ulogic;
      DIN   : in  std_ulogic;
      READ  : in  std_ulogic;
      SHIFT : in  std_ulogic
   );
end entity DNA_PORT;

-- READ loads the 57 bits, SHIFT moves them towards DOUT (bit 56), DIN in at bit 0
architecture sim of DNA_PORT is
   constant C_DNA : bit_vector(SIM_DNA_VALUE'length - 1 downto 0) := SIM_DNA_VALUE;
   signal   reg   : std_ulogic_vector(56 downto 0) := (others => '0');
begin
   DOUT <= reg(56);
   process (CLK)
   begin
      if rising_edge(CLK) then
         if READ = '1' then
            reg <= to_stdulogicvector(C_DNA(56 downto 0));
         elsif SHIFT = '1' then
            reg <= reg(55 downto 0) & DIN;
         end if;
      end if;
   end process;
end architecture sim;
