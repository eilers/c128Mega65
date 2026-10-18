# Dual-screen test disk

`dual-screen.d64` holds two BASIC 7.0 programs that use the VIC-II and the
VDC at the same time, for testing **Display Settings > Video** with a monitor
on HDMI and one on VGA.

- `DUAL TEXT` (first on the disk): a green 40-column text screen and a cyan
  80-column text screen, each with its own counter that keeps counting.
- `DUAL GFX`: a VIC-II bitmap with a bouncing box on the 40-column screen and
  a yellow scrolling log on the 80-column screen.

Mount the image on device 8 and run `RUN"DUAL TEXT"` or `RUN"DUAL GFX"`.
Any key stops either program.

What to check:

- **HDMI 40, VGA 80** and **HDMI 80, VGA 40**: each port shows its own
  screen and both keep updating.
- **Swap with 40/80**: pressing 40/80 while a program runs swaps the screens
  between the ports.
- **Follow 40/80**, **40 columns**, **80 columns**: both ports show the same
  screen.
- The on-screen menu appears on both ports in every mode.

Rebuild with VICE's tools:

```sh
petcat -w70 -o dual-text.prg -- dual-text.bas
petcat -w70 -o dual-gfx.prg -- dual-gfx.bas
c1541 -format "dual screen,ds" d64 dual-screen.d64 \
      -write dual-text.prg "dual text" -write dual-gfx.prg "dual gfx"
```
