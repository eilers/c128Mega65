# 1764 REU: where the first byte of a transfer goes

`reu-tests6.d64` holds `17-START` for issue #9, a C64-mode program for a real 1764 (or 1700/1750)
in the cartridge port, with the core's own REU off. It adds to C64MEGA65's round-5 disk
(`reu-tests5.d64`, issue MJoergen/C64MEGA65#259), whose numbering it continues and whose REU check
(`reucheck.bas`) it uses.

It runs thousands of 2-byte transfers, each a pseudo-random time after the last so that they
start all over the raster, once with the display off (no VIC stalls) and once with it on:
16384 verifies, then 2000 each of stash, fetch and swap, checked afterwards with the display off.
Each line gives the bad first and second bytes, and `BL`: how many bad runs started on a badline,
from `$D011`/`$D012` read four cycles before the command write. About 2.3 % of all runs start on a
badline. A second screen lists the start line of the first 32 bad runs of each mode, badlines
marked `*`.

- Bad runs only with the display on, nearly all of them `BL`: a transfer that starts as BA falls.
- Bad runs with the display off too: not the VIC at all.

VICE 3.9 `x64sc` with a 512 KB REU: `PREPARE: STATUS 80` and 0 in every line. Negative
controls in VICE: one wrong byte in the expected table gives 65+65 verify faults and 8+8 bad
fetch bytes; the swap command turned into a fetch or a stash gives 2000+2000 on the REU or the
C64 side. Build with `./build.sh` (VICE petcat and c1541, cc65).
