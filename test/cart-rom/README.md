# Cartridge ROM test

Three C64 cartridges that check /ROML and /ROMH reads through the expansion port. Run them from an Ultimate II+'s file browser, for example.

| File | Cartridge | Lines |
|------|-----------|-------|
| `romtest-8k.crt` | 8 KB at `$8000` | /ROML |
| `romtest-16k.crt` | 16 KB at `$8000-$BFFF` | /ROML and /ROMH |
| `romtest-ultimax.crt` | 8 KB Ultimax at `$E000` | /ROMH |

The C128 starts each one in C64 mode. A short loader copies the test from the cartridge to RAM. The test then reads the cartridge's pattern area over and over at 1 MHz and checks each byte against a pattern it computes itself. The loader's own 2 KB isn't in the pattern area; it gets checked when it's copied.

- **Green border:** every pass matched. PASSES keeps counting.
- **Red border:** a pass failed. The screen shows the last failed pass: how many bytes were bad, the first bad address, what was read and what the pattern wanted.
- **Yellow border over the old screen:** the loader never got a matching copy into RAM, so cartridge reads are failing badly.
- **Anything else** (black, frozen, garbage): the CPU couldn't run the loader from the cartridge at all.

The test stays at 1 MHz. At 2 MHz the 8502 also reads the cartridge during PHI1, which a cartridge like the Ultimate may not serve.

All three pass in VICE 3.9's `x64sc` and `x128`, and from an Ultimate II+L on this core. With one bit flipped in the ROM, each turns red and shows that address.

Rebuild with `python3 test/cart-rom/make_carts.py`. It needs `ca65` and `ld65` from cc65.
