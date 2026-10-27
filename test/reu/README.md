# C128 REU test

`reu-test.d64` holds `REUTEST`, a C128-mode program that tests the REU from the C128's side. It overwrites everything in the REU. Run it again with `RUN` or after a reset.

1. In the menu, set **REU** to 512 KB, 2 MB or 16 MB.
2. Mount `reu-test.d64` on drive 8.
3. `LOAD"REUTEST",8`, then `RUN`. 40 and 80 columns both work.

On a real C128, `reu-test-c128.crt` runs the same program from a cartridge loader such as Kung Fu Flash, whose menu would start a PRG in C64 mode. It is a C128 external function ROM: the C128 boots in C128 mode, the ROM copies the program to `$1C01` and starts it, and the machine waits for a reset after the test. The REU can be the loader's own (Kung Fu Flash 2, Ultimate II+) or a real one on a port expander.

All tests pass in VICE 3.9's `x128` with a 512 KB, a 2 MB and a 16 MB REU, from the disk and from the cartridge.

Each line ends in PASS or FAIL. A failure prints what it read and what it wanted (`GOT $xx NOT $yy`), the first byte that differs (`DIFF AT +$xx`), or the REU page that came back wrong (`BAD $xxxx00`).

| Line | What it checks |
|------|----------------|
| REGISTERS | The REU answers at `$DF00`. The address, length, interrupt and address-control registers read back, with their unused bits as 1. |
| SIZE | Writes each 64 KB bank's number into it and finds where the REU wraps, which has to be at 512 KB, 2 MB or 16 MB. It also shows `$DF06` after writing 0 to it: `$F8` on the 8726, VICE, the Ultimate and this core at any size; MiSTer's REU reads `$E0` at 2 MB. |
| STASH AND FETCH | 256 bytes across a 64 KB boundary, the status after it, and the registers ending one past the block with a length of 1. |
| SWAP | Both directions of a swap. |
| VERIFY | No fault when the bytes match, a fault when one differs. After a fault VICE leaves end-of-block clear and `reu.sv` sets it, so only the fault bit is checked. |
| AUTOLOAD | The registers return to their start values. |
| FIXED ADDRESSES | A fixed C128 address fills the REU from one byte; a fixed REU address fills the C128. |
| $FF00 TRIGGER | A command armed without bit 4 waits for a write to `$FF00`, as BASIC 7's `STASH`, `FETCH` and `SWAP` use it. |
| BANK 1 VIA $D506 | `FETCH` into and `STASH` from RAM bank 1 with the VIC bank bits in `$D506` set to 1. The CPU must find the block in bank 1, and bank 0 must not change. |
| $FF00 BANK BITS | A transfer started by writing a bank-1 configuration to `$FF00` still moves bank 0. On a C128 the REU's RAM bank comes from `$D506` (Mapping the Commodore 128, p. 419); `$FF00` only decides ROM, RAM or I/O. |
| DMA INTO VIC | `FETCH` into the VIC's border colour. |
| DMA FROM VIC | `STASH` from the border colour, 1 byte and then 4 bytes from the same address. The CPU reads the background colour first, so a DMA read that returns the VIC's previous read shows that colour. |
| DMA FROM CIA | `STASH` from CIA 1's DDRA, after the CPU read DDRB. |
| DMA FROM COLOUR RAM | `STASH` from colour RAM. |
| INTERRUPT | The end-of-block interrupt shows in `$DF00` and clears when read. |
| FAST MODE | A transfer in 2 MHz mode. |
| WHOLE REU | Every page of the size SIZE found or the bank register claims, whichever is larger, with a pattern and then its inverse. Bytes 0, 1 and 255 of each page hold its number, so pages that alias show up. Takes about 15 seconds at 2 MB and 2 minutes at 16 MB, with a dot for every 2 MB verified. |

The program leaves a mark with a run count in the REU's last page and reports the previous run's mark at the start. Run it, press reset, and run it again: `FOUND MARK` means the REU kept its contents through the reset.

It also writes its result to `$D7FF` (0 = pass, `$FF` = fail), the exit-code register of VICE's test harness. On the MEGA65 and a real C128 that write does nothing.

Rebuild with `python3 test/reu/make_image.py`. It needs `ca65` and `ld65` from cc65, and VICE's `cartconv` for the cartridge (set `CARTCONV` if it isn't in `/Applications/vice-arm64-gtk3-3.9/bin`).
