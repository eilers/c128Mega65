# C128 RAM bank test

`ram-test.d64` holds `RAMTEST`, a C128-mode program that tests RAM banks 0-3 with the menu's **256 KB RAM** on or off. It puts back every byte it changes.

1. Mount `ram-test.d64` on drive 8.
2. `LOAD"RAMTEST",8`, then `RUN`.
3. Check that `RAM SIZE` says what the menu says: 256 KB with **256 KB RAM** on, 128 KB with it off.

For the REU line, set **REU** to any size; without one it says `NO REU, SKIPPED`.

| Line | What it checks |
|------|----------------|
| MMU $D50B | The MMU's version register reads `$20` or `$40`. The core's MMU (from the MiSTer core) reads `$40` with 256 KB; VICE's `-c128fullbanks` leaves it at `$20`. |
| RAM SIZE | Writes a signature into each bank at `$1310`, `$8010` and `$F010` through `$FF00`, then reads bank 0: its own signature means 256 KB, bank 2's means 128 KB (banks 2 and 3 are banks 0 and 1). `$D50B = $40` with 128 KB fails. |
| CPU BANKS 0-3 VIA $FF00 | Every bank holds what the size says: its own signature with 256 KB, bank 2's or 3's with 128 KB. |
| REU DMA VIA $D506 | `STASH` from each bank with the VIC bank bits in `$D506` set to it, then `FETCH` into bank 3; the CPU must find the bytes where bank 3 is. |

It writes its result to `$D7FF` (0 = pass, `$FF` = fail) for VICE's test harness.

All four combinations pass in VICE 3.9's `x128`: plain, `-c128fullbanks`, and each with `-reu -reusize 512`. Two broken copies fail as they should: one that expects the wrong bank with 128 KB, and one whose DMA ignores `$D506`.

Rebuild with `python3 test/ram256/make_image.py` (needs `ca65` and `ld65` from cc65).
