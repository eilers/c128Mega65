# VDC memory and registers

`vdc_screen.py HOST [--password PW]` prints the 80-column screen as text, read from the VDC's
memory by REST (`machine:vdcregs`, `machine:vdcmem`); `--monitor` reads it over the serial
monitor instead (`vdcreg:`, `vdcram:`). `--attrs` adds each row's attributes, `--raw` gives
screen codes, `--png FILE` draws the screen with the VDC's own character set and colours.
Nothing on the C128 changes, so a script can read it at any time, CP/M included (it writes
ASCII and loads an ASCII font into the second character set; the text follows the font);
pause first (`machine:pause`) for a snapshot that can't change halfway.

`hw_vdc.py HOST [--password PW]` checks the routes on the machine against a program's own
reads through $D600/$D601 (writemem and readmem, the C128 paused): the registers and the
status, VDC memory across the screen, the attributes, the character set and beyond; that
neither route moves the update address or the selected register; vdcmem's speed; and text
typed in 80 columns (machine:input) read back as text. It resets the C128 before and after.
Both need the menu's Network > REST API on (default off).
