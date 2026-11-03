# machine:input tests

`input-test.d64` holds `INPUTTEST` (C128 mode): with interrupts off it scans the keyboard
matrix both ways (C64 rows and K0-K2), both joystick ports, the POT lines, the 40/80 and Caps
Lock senses, counts NMIs (RESTORE) and mirrors `$D7FF`, into a table at `$1300` that it also
shows on the 40-column screen. The layout is in `inputtest.s`. Stop it with a reset.

`hw_input.py HOST [--password PW]` runs it on the core (runners:run_prg) and drives the REST
API's machine:input: every key name in both scan directions, the locking keys, RESTORE, both
joysticks with fire2/fire3, the mouse, the debug register; then typing at BASIC's READY,
100 keys at full speed, and the menu by keys. Each line ends in ok or FAIL.

`make_image.py` assembles it with cc65 and writes the disk image.
