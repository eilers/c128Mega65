----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65 (c128mega65)
--
-- Serves the drive DOS ROM (boot1.rom) to iec_drive, one byte at a time.
--
-- iec_drive does not read boot1.rom directly. Each emulated drive keeps the 32 kB
-- bank it currently needs in its own block RAM and pulls it in over a tiny handshake:
-- rom_req_i stays high for as long as some drive still wants data, rom_addr_i carries
-- {bank, offset} of the byte it wants next, and every rom_wr_o pulse hands over one
-- byte and makes iecdrv_rom store it and advance its address.
--
-- The subtlety this entity exists for: iecdrv_rom advances rom_addr_i on the very
-- edge that ends the rom_wr_o pulse. Sampling rom_addr_i during that same cycle
-- yields the pre-increment value, which fetches the byte just written a second time
-- and shifts the whole bank by one. iecdrv_rom still reports rom_valid afterwards,
-- so the drive leaves reset and executes garbage - it answers nothing on the IEC bus
-- and the C128 reports "device not present". Hence the separate WRITE_ST.
--
-- done by Michael Jørgensen and sy2002 in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

entity drive_rom_server is
   generic (
      -- Width of the ROM address presented to the block RAM. iec_drive generates
      -- {bank[3:0], offset[14:0]}; only as many low bits as the ROM actually has are used.
      G_ROM_ADDR_WIDTH : natural := 18
   );
   port (
      clk_i          : in  std_logic;

      -- High while the ROM behind rom_data_i is not trustworthy yet, e.g. while the
      -- M2M Shell is still streaming boot1.rom in. No byte is served while this is high.
      rom_loading_i  : in  std_logic;

      -- Handshake towards iec_drive
      rom_req_i      : in  std_logic;
      rom_addr_i     : in  std_logic_vector(18 downto 0);
      rom_wr_o       : out std_logic;

      -- Read port of the block RAM holding boot1.rom. Registered address, so the byte
      -- appears two cycles after rom_ram_addr_o changes.
      rom_ram_addr_o : out std_logic_vector(G_ROM_ADDR_WIDTH - 1 downto 0)
   );
end entity drive_rom_server;

architecture synthesis of drive_rom_server is

   type state_t is (IDLE_ST, ADDR_ST, DATA_ST, WRITE_ST);
   signal state : state_t := IDLE_ST;

begin

   p_server : process (clk_i)
   begin
      if rising_edge(clk_i) then
         rom_wr_o <= '0';

         case state is
            when IDLE_ST =>
               if rom_req_i = '1' and rom_loading_i = '0' then
                  rom_ram_addr_o <= rom_addr_i(G_ROM_ADDR_WIDTH - 1 downto 0);
                  state          <= ADDR_ST;
               end if;

            -- one cycle for the block RAM to register the address
            when ADDR_ST =>
               state <= DATA_ST;

            -- the byte is valid now, so raise the strobe to hand it over
            when DATA_ST =>
               rom_wr_o <= '1';
               state    <= WRITE_ST;

            -- The strobe is high during this state, and iecdrv_rom advances rom_addr_i on
            -- the edge that ends it. Only now is it safe to sample the next address.
            when WRITE_ST =>
               state <= IDLE_ST;
         end case;

         if rom_loading_i = '1' then
            state    <= IDLE_ST;
            rom_wr_o <= '0';
         end if;
      end if;
   end process p_server;

end architecture synthesis;
