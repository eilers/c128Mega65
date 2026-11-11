----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65 (c128mega65)
--
-- iecdrv_rom.sv line for line in VHDL, for tb_drive_rom in nvc, which has no Verilog. xsim
-- runs the same testbench against the real iecdrv_rom.sv (run_drive_rom_sim.tcl).
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity iecdrv_rom is
   port (
      clk_sys     : in  std_logic;
      clk         : in  std_logic;
      reset       : in  std_logic;
      rom_loading : in  std_logic;
      rom_reload  : in  std_logic;
      empty8k     : out std_logic;
      rom_valid   : out std_logic;
      rom_bank    : in  std_logic_vector(3 downto 0);
      mem_a       : in  std_logic_vector(14 downto 0);
      rom_do      : out std_logic_vector(7 downto 0);
      rom_req     : out std_logic;
      rom_addr    : out std_logic_vector(14 downto 0);
      rom_wr      : in  std_logic;
      rom_data    : in  std_logic_vector(7 downto 0)
   );
end entity iecdrv_rom;

architecture sim of iecdrv_rom is

   signal valid      : std_logic := '0';
   signal addr       : unsigned(14 downto 0) := (others => '0');
   signal bank_n     : std_logic_vector(3 downto 0) := (others => '0');
   signal req        : std_logic;
   signal e8k        : std_logic := '0';
   signal rom_wr_d   : std_logic := '0';
   signal rom_addr_d : std_logic_vector(14 downto 0) := (others => '0');
   signal rom_data_d : std_logic_vector(7 downto 0) := (others => '0');
   signal mem_a_d    : std_logic_vector(14 downto 0) := (others => '0');
   signal mem_a_r    : std_logic_vector(14 downto 0) := (others => '0');
   type t_ram is array (0 to 2**15 - 1) of std_logic_vector(7 downto 0);
   signal ram        : t_ram := (others => (others => '0'));

begin

   req       <= not (reset or rom_loading or valid);
   rom_req   <= req;
   rom_valid <= valid;
   rom_addr  <= std_logic_vector(addr);
   empty8k   <= e8k;

   p_addr : process (clk_sys)
      variable v_valid : std_logic;
   begin
      if rising_edge(clk_sys) then
         v_valid := valid;
         if rom_loading = '1' or rom_reload = '1' then
            v_valid := '0';
         end if;
         if reset = '1' then
            addr <= (others => '0');
         elsif rom_bank /= bank_n or rom_reload = '1' then
            v_valid := '0';
            addr    <= (others => '0');
            bank_n  <= rom_bank;
         elsif req = '1' and rom_wr = '1' then
            if addr = 2**15 - 1 then
               v_valid := '1';
            end if;
            addr <= addr + 1;
         end if;
         valid <= v_valid;
      end if;
   end process p_addr;

   p_empty8k : process (clk_sys)
   begin
      if rising_edge(clk_sys) then
         if rom_wr = '1' and addr = 0 then
            e8k <= '1';
         end if;
         if rom_wr = '1' and rom_data /= x"00" and rom_data /= x"FF" and addr(14 downto 8) /= "0000000" and
            addr(14 downto 13) = "00" then
            e8k <= '0';
         end if;
      end if;
   end process p_empty8k;

   p_delay : process (clk_sys)
   begin
      if rising_edge(clk_sys) then
         rom_wr_d   <= rom_wr and req;
         rom_addr_d <= std_logic_vector(addr);
         rom_data_d <= rom_data;
      end if;
   end process p_delay;

   p_mem_a : process (clk)
   begin
      if rising_edge(clk) then
         mem_a_d <= mem_a;
      end if;
   end process p_mem_a;

   -- The block RAM: the write a clock after the byte came, the read two clocks after mem_a.
   -- Not dualport_2clk_ram: its "clock xor" port expression is a delta late in nvc.
   p_ram : process (clk_sys)
   begin
      if rising_edge(clk_sys) then
         if rom_wr_d = '1' then
            ram(to_integer(unsigned(rom_addr_d))) <= rom_data_d;
         end if;
      end if;
   end process p_ram;

   p_ram_b : process (clk)
   begin
      if rising_edge(clk) then
         mem_a_r <= mem_a_d;
      end if;
   end process p_ram_b;

   rom_do <= ram(to_integer(unsigned(mem_a_r)));

end architecture sim;
