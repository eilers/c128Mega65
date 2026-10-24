# Virtual cartridge tests

Self-checking `.crt` files for the virtual cartridge. `tb_crt` runs them in simulation. They also run on hardware, as a virtual cartridge or from an Ultimate II+, so the two can be compared.

| File | Cartridge | Checks |
|------|-----------|--------|
| `crttest-c128.crt` | C128 function ROM: 8 KB at `$8000`, 16 KB at `$C000` | 16 KB ROML and ROMH windows; the 8 KB chip mirrored at `$A000` |
| `crttest-c128fast.crt` | The same, in FAST mode | The same reads at 2 MHz, where a cartridge has the least time to answer |
| `crttest-16k.crt` | C64, 16 KB at `$8000` | ROML and ROMH from one chip; writes to ROML go to the RAM underneath |
| `crttest-umax4k.crt` | C64 Ultimax, 4 KB at `$F000` | The reset vector at `$FFFC`; the chip mirrored at `$E000` |
| `crttest-ef.crt` | EasyFlash, 10 banks | Bank switching across more banks than the cache holds, 8 KB and 16 KB modes, the RAM at `$DF00`, switching the cartridge off |
| `crttest-badtype.crt` | Retro Replay | A type the core doesn't support: loading it must fail with "Unsupported type" |

Each program copies itself to `$0C00` and works through a table of probes: reads, which it checks, and writes, which switch banks or modes. It stores every byte it read at `$0500` and finishes by blanking the screen:

- **Green screen:** every probe read what it should. `$0400` holds `$10` ('P').
- **Red screen:** at least one didn't. `$0400` holds `$06` ('F') and `$0401` the number of bad reads; `$0500` onward shows what each read got, to compare with the `.exp` file.

The C128 cartridge starts at once, from the KERNAL's POLL (ID 1). The others start in C64 mode.

All but `crttest-umax4k` pass in VICE 3.9 (`x64sc`, `x128`). VICE leaves `$E000-$EFFF` empty for a 4 KB Ultimax chip, so the mirror probes there fail. The core mirrors it, as the Ultimate does and as a 4 KB ROM on a real cartridge would.

Rebuild with `python3 test/crt/make_crt_tests.py`. It needs `ca65` and `ld65` from cc65, and also writes the `.exp` files: the bytes each cartridge's reads should return, in order.
