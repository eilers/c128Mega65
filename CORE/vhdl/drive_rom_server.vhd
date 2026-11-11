----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65 (c128mega65)
--
-- Hands each drive its DOS ROM bank over iec_drive's pull protocol. The library (boot1.rom's
-- six 32 KB banks) is in HyperRAM; the firmware writes the bytes a drive asks for (drvrom.c).
--
-- Device C_DEV_DRIVE_ROM, CSR window 0xFFFF:
--   0x000  R: bit i drive i has its ROM. W: bit i reloads drive i's bank (a drive in reset
--          waits until it is out)
--   0x001  R: the library's HyperRAM window (C_HMAP_DRVROM, in 8 KB)
--   0x002  R: bits 15:0 of the byte a drive asks for (bank * 32768 + offset); starts a feed
--          there, from its 4-byte boundary
--   0x003  R: bit 15 a drive was asking, bits 2:0 the address's bits 18:16 (as 0x002 read them)
--   0x004  W: the feed's next two bytes, low byte first; 0x005 the same (a 32-bit write)
--
-- A byte is taken only at the very address the drive asks for (each drive has banks of its
-- own), else dropped: a reload, a new model, a reset or the other drive's turn ends a feed.
--
-- MEGA65 port done by Stefan Eilers and Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity drive_rom_server is
   generic (
      G_WINDOW      : std_logic_vector(15 downto 0)   -- the library in HyperRAM, for the firmware
   );
   port (
      clk_i         : in  std_logic;                  -- QNICE clock, iec_drive's clk_sys
      rst_i         : in  std_logic;
      rom_loading_i : in  std_logic;                  -- the core's reset: no feed

      -- Device bus (C_DEV_DRIVE_ROM)
      qnice_addr_i  : in  std_logic_vector(27 downto 0);
      qnice_data_i  : in  std_logic_vector(15 downto 0);
      qnice_ce_i    : in  std_logic;
      qnice_we_i    : in  std_logic;
      qnice_data_o  : out std_logic_vector(15 downto 0);

      -- iec_drive
      rom_ready_i   : in  std_logic_vector(1 downto 0);
      rom_reload_o  : out std_logic_vector(1 downto 0);  -- a pulse per drive
      rom_req_i     : in  std_logic;
      rom_addr_i    : in  std_logic_vector(18 downto 0);
      rom_data_o    : out std_logic_vector(7 downto 0);
      rom_wr_o      : out std_logic
   );
end entity drive_rom_server;

architecture synthesis of drive_rom_server is

   type t_bytes is array (0 to 3) of std_logic_vector(7 downto 0);

   -- Device bus, falling edge: a pulse for the rising edge that follows
   signal f_ce_d    : std_logic := '0';
   signal f_reload  : std_logic_vector(1 downto 0) := "00";
   signal f_word    : std_logic_vector(15 downto 0) := (others => '0');
   signal f_word_v  : std_logic := '0';
   signal f_arm     : std_logic := '0';
   signal lat_req   : std_logic := '0';                         -- the request, as 0x002 read it
   signal lat_addr  : std_logic_vector(18 downto 0) := (others => '0');

   -- The feed: the bytes still to offer, the first one's address
   signal armed     : std_logic := '0';
   signal q_bytes   : t_bytes := (others => (others => '0'));
   signal q_n       : natural range 0 to 4 := 0;
   signal expect    : unsigned(18 downto 0) := (others => '0');

begin

   ---------------------------------------------------------------------------------------------
   -- Device bus: the first falling edge of each access counts
   ---------------------------------------------------------------------------------------------

   p_bus : process (clk_i)
   begin
      if falling_edge(clk_i) then
         f_ce_d   <= qnice_ce_i;
         f_reload <= "00";
         f_word_v <= '0';
         f_arm    <= '0';
         if qnice_ce_i = '1' and f_ce_d = '0' and qnice_addr_i(27 downto 12) = x"FFFF" then
            if qnice_we_i = '1' then
               case qnice_addr_i(11 downto 0) is
                  when x"000"          => f_reload <= qnice_data_i(1 downto 0);
                  when x"004" | x"005" => f_word   <= qnice_data_i;
                                          f_word_v <= '1';
                  when others          => null;
               end case;
            elsif qnice_addr_i(11 downto 0) = x"002" then
               lat_req  <= rom_req_i and not rom_loading_i;
               lat_addr <= rom_addr_i;
               f_arm    <= '1';
            end if;
         end if;
      end if;
   end process p_bus;

   p_read : process (all)
   begin
      qnice_data_o <= (others => '0');
      if qnice_addr_i(27 downto 12) = x"FFFF" then
         case qnice_addr_i(11 downto 0) is
            when x"000" => qnice_data_o(1 downto 0) <= rom_ready_i;
            when x"001" => qnice_data_o <= G_WINDOW;
            when x"002" => qnice_data_o <= lat_addr(15 downto 0);
            when x"003" => qnice_data_o(15)         <= lat_req;
                           qnice_data_o(2 downto 0) <= lat_addr(18 downto 16);
            when others => null;
         end case;
      end if;
   end process p_read;

   ---------------------------------------------------------------------------------------------
   -- The feed: the first byte is offered each clock, and leaves taken or dropped
   ---------------------------------------------------------------------------------------------

   p_feed : process (clk_i)
      variable v_bytes : t_bytes;
      variable v_n     : natural range 0 to 4;
      variable v_exp   : unsigned(18 downto 0);
   begin
      if rising_edge(clk_i) then
         rom_reload_o <= f_reload;
         v_bytes := q_bytes;
         v_n     := q_n;
         v_exp   := expect;

         if q_n /= 0 then
            v_bytes := q_bytes(1 to 3) & x"00";
            v_n     := q_n - 1;
            v_exp   := expect + 1;
         end if;

         if f_word_v = '1' and armed = '1' then
            if v_n <= 2 then
               v_bytes(v_n)     := f_word(7 downto 0);
               v_bytes(v_n + 1) := f_word(15 downto 8);
               v_n              := v_n + 2;
            else
               -- The bus can't bring bytes this fast; should it, no byte may land misplaced
               armed <= '0';
               v_n   := 0;
            end if;
         end if;

         if f_arm = '1' then
            armed <= lat_req;
            v_exp := unsigned(lat_addr(18 downto 2)) & "00";
            v_n   := 0;
         end if;

         if rst_i = '1' or rom_loading_i = '1' or f_reload /= "00" then
            armed <= '0';
            v_n   := 0;
         end if;

         q_bytes <= v_bytes;
         q_n     <= v_n;
         expect  <= v_exp;
      end if;
   end process p_feed;

   -- iecdrv_rom moves on with the edge that takes a byte, so this compares its next address
   rom_data_o <= q_bytes(0);
   rom_wr_o   <= '1' when q_n /= 0 and armed = '1' and rom_req_i = '1' and rom_loading_i = '0' and
                          unsigned(rom_addr_i) = expect else
                 '0';

end architecture synthesis;
