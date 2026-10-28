----------------------------------------------------------------------------------
-- Commodore 128 for MEGA65
--
-- tb_crt with the cartridge in SDRAM, as on R4 and later boards.
--
-- done by Jeff Flitton in 2026 and licensed under GPL v3
----------------------------------------------------------------------------------

entity tb_crt_sdram is
end entity tb_crt_sdram;

architecture sim of tb_crt_sdram is
begin

   u_tb : entity work.tb_crt
      generic map (
         G_SDRAM => true
      );

end architecture sim;
