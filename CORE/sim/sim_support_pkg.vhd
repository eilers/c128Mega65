----------------------------------------------------------------------------------
-- Shared helpers for C128 boot simulation (ROM preload + debug NDJSON log).
----------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

package sim_support_pkg is

   type byte_rom_t is array (natural range <>) of std_logic_vector(7 downto 0);

   impure function load_bin_rom(path : string; size : natural) return byte_rom_t;

   procedure log_boot_json(
      path          : string;
      session_id    : string;
      run_id        : string;
      hypothesis_id : string;
      location      : string;
      message       : string;
      data          : string
   );

end package sim_support_pkg;

package body sim_support_pkg is

   type rom_char_file_t is file of character;

   impure function load_bin_rom(path : string; size : natural) return byte_rom_t is
      file romfile : rom_char_file_t;
      variable ch  : character;
      variable mem     : byte_rom_t(0 to size - 1) := (others => (others => '0'));
      variable idx     : natural := 0;
   begin
      file_open(romfile, path, read_mode);
      while not endfile(romfile) and idx < size loop
         read(romfile, ch);
         mem(idx) := std_logic_vector(to_unsigned(character'pos(ch), 8));
         idx := idx + 1;
      end loop;
      file_close(romfile);
      return mem;
   end function;

   procedure log_boot_json(
      path          : string;
      session_id    : string;
      run_id        : string;
      hypothesis_id : string;
      location      : string;
      message       : string;
      data          : string
   ) is
      file logf : text;
      variable l : line;
   begin
      file_open(logf, path, append_mode);
      write(l, "{" &
         """sessionId"":""" & session_id & """," &
         """runId"":""" & run_id & """," &
         """hypothesisId"":""" & hypothesis_id & """," &
         """location"":""" & location & """," &
         """message"":""" & message & """," &
         """data"":" & data & "," &
         """timestamp"":" & integer'image(integer(now / 1 ns)) &
         "}");
      writeline(logf, l);
      file_close(logf);
   end procedure;

end package body sim_support_pkg;
