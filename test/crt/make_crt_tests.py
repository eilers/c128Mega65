#!/usr/bin/env python3
"""Build the virtual cartridge tests (see README.md): crttest-*.crt and, for each, a .exp file
with the bytes its probes should read, one hex byte per line, which tb_crt compares.

Each cartridge is crttest.s plus a probe table, in chips filled with a pattern. Probes read the
pattern above the program (CODE_MAX) and fixed header bytes, so their values are known before
the program is assembled. Needs ca65 and ld65 from cc65.
"""

from __future__ import annotations

import struct
import subprocess
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
CODE_MAX = 0x400    # the program stays below this offset of its chip
READ, WRITE, POLL, STATUS, TOGGLE, POLLQ = 1, 2, 3, 4, 5, 6    # see crttest.s

LD_CFG = """
MEMORY {{
    ROM: file = %O, start = ${org:04X}, size = ${size:04X};
    RAM: file = "", start = $0C00, size = $0300;
}}
SEGMENTS {{
    HEADER: load = ROM, type = ro, optional = yes;
    CODE:   load = ROM, type = ro;
    ENGINE: load = ROM, run = RAM, type = rw, define = yes;
}}
"""


def pattern(chip: int, size: int) -> bytearray:
    """A chip's filler. 37 is odd, so each chip number gives a different byte at any offset."""
    return bytearray(((o * 7 + (o >> 8) * 13 + chip * 37 + 0x5A) & 0xFF) for o in range(size))


def program(mode: int, org: int, probes: list[tuple[int, int, int]]) -> bytes:
    """crttest.s assembled at org with these probes."""
    with tempfile.TemporaryDirectory() as tmp_name:
        tmp = Path(tmp_name)
        lines = [f"        .byte {op}, ${a & 0xFF:02X}, ${a >> 8:02X}, ${v:02X}\n" for op, a, v in probes]
        (tmp / "probes.inc").write_text("".join(lines) + "        .byte 0\n")
        (tmp / "crttest.cfg").write_text(LD_CFG.format(org=org, size=CODE_MAX))
        subprocess.run(["ca65", "-D", f"MODE={mode}", "-I", str(tmp), "-o", str(tmp / "crttest.o"),
                        str(HERE / "crttest.s")], check=True)
        subprocess.run(["ld65", "-C", str(tmp / "crttest.cfg"), "-o", str(tmp / "crttest.bin"),
                        str(tmp / "crttest.o")], check=True)
        return (tmp / "crttest.bin").read_bytes()


def with_program(chip: bytearray, code: bytes, vectors: int | None = None) -> bytes:
    """The chip with the program at its start and, for Ultimax, NMI/RESET/IRQ pointing at it."""
    assert len(code) <= CODE_MAX
    chip[:len(code)] = code
    if vectors is not None:
        chip[-6:] = struct.pack("<HHH", vectors, vectors, vectors)
    return bytes(chip)


def crt(path: Path, c128: bool, hw_type: int, exrom: int, game: int,
        chips: list[tuple[int, int, int, bytes]], title: str | None = None) -> None:
    """chips: (chip type, bank, load address, data); title defaults to the file name"""
    sig = b"C128 CARTRIDGE  " if c128 else b"C64 CARTRIDGE   "
    out = bytearray(sig + struct.pack(">IHHBBB", 0x40, 0x0100, hw_type, exrom, game, 0) + bytes(5))
    out += (title or path.stem.upper()).encode().ljust(32, b"\0")
    for chip_type, bank, load, data in chips:
        out += b"CHIP" + struct.pack(">IHHHH", 0x10 + len(data), chip_type, bank, load, len(data)) + data
    path.write_bytes(out)


def save(name: str, probes: list[tuple[int, int, int]], *args, **kwargs) -> None:
    crt(HERE / f"{name}.crt", *args, **kwargs)
    (HERE / f"{name}.exp").write_text("".join(f"{v:02X}\n" for op, _, v in probes if op != WRITE))
    print(f"{name}.crt: {sum(op != WRITE for op, _, _ in probes)} reads")


def c128_generic(name: str = "crttest-c128", mode: int = 1) -> None:
    """C128 function ROM: an 8 kB chip at $8000, mirrored at $A000, and 16 kB at $C000.
    Mode 3 runs it in FAST mode."""
    roml, romh = pattern(0, 0x2000), pattern(1, 0x4000)
    assert romh[7:10] != b"CBM"    # or the KERNAL finds a second function ROM at $C000
    probes = [(WRITE, 0xFF00, 0x2A),   # external function ROM at $8000 and $C000, I/O on
              (READ, 0x8006, 0x01), (READ, 0x8007, 0x43), (READ, 0x8009, 0x4D),
              (READ, 0x8800, roml[0x800]), (READ, 0x9FFF, roml[0x1FFF]),
              (READ, 0xA007, 0x43), (READ, 0xA800, roml[0x800]), (READ, 0xBFFF, roml[0x1FFF]),
              (READ, 0xC000, romh[0]), (READ, 0xC800, romh[0x800]), (READ, 0xCFFF, romh[0xFFF]),
              (READ, 0xE000, romh[0x2000]), (READ, 0xE123, romh[0x2123]),
              (READ, 0xFFF0, romh[0x3FF0]), (READ, 0xFFFF, romh[0x3FFF])]
    roml_img = with_program(roml, program(mode, 0x8000, probes))
    save(name, probes, True, 0, 0, 0, [(0, 0, 0x8000, roml_img), (0, 0, 0xC000, bytes(romh))])


def c64_16k() -> None:
    """C64 16 kB cartridge: one chip, ROML its first half and ROMH its second."""
    rom = pattern(0, 0x4000)
    probes = [(READ, 0x8004, 0xC3), (READ, 0x8008, 0x30),
              (READ, 0x8800, rom[0x800]), (READ, 0x9FFF, rom[0x1FFF]),
              (READ, 0xA000, rom[0x2000]), (READ, 0xA123, rom[0x2123]), (READ, 0xBFFF, rom[0x3FFF]),
              (WRITE, 0x8800, 0x77),         # goes to the RAM underneath
              (READ, 0x8800, rom[0x800])]
    img = with_program(rom, program(0, 0x8000, probes))
    save("crttest-16k", probes, False, 0, 0, 0, [(0, 0, 0x8000, img)])


def ultimax_4k() -> None:
    """Ultimax with a 4 kB chip at $F000, which ROMH mirrors at $E000."""
    rom = pattern(0, 0x1000)
    probes = [(READ, 0xF800, rom[0x800]), (READ, 0xFFFC, 0x00), (READ, 0xFFFD, 0xF0),
              (READ, 0xE000, 0x78),          # the program's first byte, SEI
              (READ, 0xE800, rom[0x800]), (READ, 0xEFFC, 0x00), (READ, 0xEFFD, 0xF0)]
    img = with_program(rom, program(2, 0xF000, probes), vectors=0xF000)
    save("crttest-umax4k", probes, False, 0, 1, 0, [(0, 0, 0xF000, img)])


def easyflash() -> None:
    """EasyFlash with 10 banks, more than the cache's 8 slots on each side."""
    banks = 10
    lo = [pattern(2 * b, 0x2000) for b in range(banks)]
    hi = [pattern(2 * b + 1, 0x2000) for b in range(banks)]
    hi[0][-6:] = struct.pack("<HHH", 0xE000, 0xE000, 0xE000)   # the program's vectors, known early
    probes = [(WRITE, 0xDE02, 0x07)]                           # 16 kB mode
    for b in [1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 9, 5]:
        probes += [(WRITE, 0xDE00, b),
                   (READ, 0x8400, lo[b][0x400]), (READ, 0x9FFF, lo[b][0x1FFF]),
                   (READ, 0xA800, hi[b][0x800]), (READ, 0xBFFF, hi[b][0x1FFF])]
    probes += [(WRITE, 0xDE02, 0x06), (READ, 0x8400, lo[5][0x400]),        # 8 kB mode
               (WRITE, 0xDF00, 0x5A), (WRITE, 0xDF80, 0xA5),               # its 256 bytes of RAM
               (READ, 0xDF00, 0x5A), (READ, 0xDF80, 0xA5),
               (WRITE, 0xDE02, 0x07), (WRITE, 0x8000, 0x77),               # RAM under ROML
               (WRITE, 0xDE02, 0x04), (READ, 0x8000, 0x77),                # cartridge off
               (WRITE, 0xDE02, 0x07), (WRITE, 0xDF10, 0x3C),               # $9Fxx is not $DFxx
               (WRITE, 0x9F10, 0xC3), (READ, 0xDF10, 0x3C),
               (WRITE, 0x8100, 0x11), (WRITE, 0xE100, 0x22),               # RAM under both
               (WRITE, 0xDE02, 0x05), (WRITE, 0x8100, 0xEE),               # Ultimax: these writes
               (WRITE, 0xE100, 0xDD), (READ, 0x8100, lo[5][0x100]),        # reach only the cart
               (WRITE, 0xDE02, 0x04), (READ, 0x8100, 0x11),
               (WRITE, 0x0001, 0x35), (READ, 0xE100, 0x22), (WRITE, 0x0001, 0x37)]
    hi[0] = bytearray(with_program(hi[0], program(2, 0xE000, probes)))
    chips = []
    for b in range(banks):
        chips += [(2, b, 0x8000, bytes(lo[b])), (2, b, 0xA000, bytes(hi[b]))]
    save("crttest-ef", probes, False, 32, 1, 0, chips, title="EasyFlash")


def easyflash_flash() -> None:
    """EasyFlash's flash chips, driven as EAPI does: commands in Ultimax mode, reads in 16 kB mode,
    a programmed byte back within EAPI's 20 reads. ROML has banks 0, 1, 8 and 56, ROMH 0 and 1;
    the others are missing from the file."""
    U, K16 = 0x05, 0x07                                        # $DE02: Ultimax, 16 kB
    lo = {b: pattern(2 * b, 0x2000) for b in (0, 1, 8, 56)}
    hi = {b: pattern(2 * b + 1, 0x2000) for b in (0, 1)}
    hi[0][-6:] = struct.pack("<HHH", 0xE000, 0xE000, 0xE000)

    def cmd(chip: int, *data: int) -> list[tuple[int, int, int]]:
        """Unlock and command bytes for ROML's chip ($8xxx) or ROMH's ($Exxx)"""
        out = []
        for d in data:
            out += [(WRITE, chip + 0x555, 0xAA), (WRITE, chip + 0x2AA, 0x55), (WRITE, chip + 0x555, d)]
        return out

    v = lo[1][0x123] & 0x5A                                    # clears bits, bit 7 among them
    w = 0x8F                                                   # would set bit 7 again
    vh = hi[1][0x456] & 0xA5
    assert v != lo[1][0x123] and vh != hi[1][0x456] and not v & 0x80

    probes = [(WRITE, 0xDE02, U)] + cmd(0xE000, 0x90) + cmd(0x8000, 0x90)       # autoselect
    probes += [(WRITE, 0xDE02, K16),
               (READ, 0xA000, 0x01), (READ, 0xA001, 0xA4),                     # AMD, AM29F040
               (READ, 0x8000, 0x01), (READ, 0x8001, 0xA4),
               (WRITE, 0xDE00, 9), (READ, 0x8002, 0x00), (READ, 0xA002, 0x00),  # not protected
               (WRITE, 0xDE02, U), (WRITE, 0xE000, 0x00),                      # not a reset
               (WRITE, 0xDE02, K16), (READ, 0xA000, 0x01),
               (WRITE, 0xDE02, U), (WRITE, 0xE000, 0xF0), (WRITE, 0x8000, 0xF0),  # reset
               (WRITE, 0xDE00, 0), (WRITE, 0xDE02, K16),
               (READ, 0xA800, hi[0][0x800]), (READ, 0x8800, lo[0][0x800])]
    # Program: unlock in bank 0, the byte in its own bank, as EAPI does
    probes += [(WRITE, 0xDE02, U)] + cmd(0x8000, 0xA0)
    probes += [(WRITE, 0xDE00, 1), (WRITE, 0x8123, v), (WRITE, 0xDE02, K16),
               (POLLQ, 0x8123, v), (READ, 0x8122, lo[1][0x122])]
    probes += [(WRITE, 0xDE02, U)] + cmd(0xE000, 0xA0)
    probes += [(WRITE, 0xE456, vh), (WRITE, 0xDE02, K16), (POLLQ, 0xA456, vh), (READ, 0xA457, hi[1][0x457])]
    # A 0 can't become 1 again: DQ5 status from any address of that chip, until a reset command
    probes += [(WRITE, 0xDE02, U)] + cmd(0x8000, 0xA0)
    probes += [(WRITE, 0x8123, w), (WRITE, 0xDE02, K16),
               (STATUS, 0x8123, 0x20), (STATUS, 0x8000, 0x20), (READ, 0xA456, vh),
               (WRITE, 0xDE02, U), (WRITE, 0x8000, 0xF0), (WRITE, 0xDE02, K16),
               (READ, 0x8123, v & w)]
    # Banks the file lacks read $FF and still take bytes
    probes += [(WRITE, 0xDE00, 9), (READ, 0x8456, 0xFF), (WRITE, 0xDE02, U)] + cmd(0x8000, 0xA0)
    probes += [(WRITE, 0x8456, 0x5A), (WRITE, 0xDE02, K16), (POLLQ, 0x8456, 0x5A),
               (READ, 0x8457, 0xFF), (READ, 0x9FFF, 0xFF)]
    probes += [(WRITE, 0xDE00, 8), (READ, 0xA777, 0xFF), (WRITE, 0xDE02, U)] + cmd(0xE000, 0xA0)
    probes += [(WRITE, 0xE777, 0x33), (WRITE, 0xDE02, K16), (POLLQ, 0xA777, 0x33)]
    # Erase ROML's sector of banks 8-15: status from any bank of that chip while it runs
    probes += [(WRITE, 0xDE02, U)] + cmd(0x8000, 0x80) + [(WRITE, 0x8555, 0xAA)]
    probes += [(WRITE, 0x82AA, 0x55), (WRITE, 0xDE00, 12), (WRITE, 0x8000, 0x30), (WRITE, 0xDE02, K16),
               (TOGGLE, 0x8000, 0x40), (STATUS, 0x8000, 0x08),
               (WRITE, 0xDE00, 1), (STATUS, 0x8123, 0x08), (READ, 0xA456, vh),
               (WRITE, 0xDE00, 8), (POLL, 0x8000, 0xFF), (READ, 0x9FFF, 0xFF),
               (WRITE, 0xDE00, 9), (READ, 0x8456, 0xFF),
               (WRITE, 0xDE00, 1), (READ, 0x8123, v & w),                      # other sector
               (WRITE, 0xDE00, 8), (READ, 0xA777, 0x33)]                       # other chip
    # Outside Ultimax mode the chips see no writes: this sequence programs nothing
    probes += [(WRITE, 0xDE00, 1)] + cmd(0x8000, 0xA0)
    probes += [(WRITE, 0x8800, 0x00), (READ, 0x8800, lo[1][0x800]),
               (WRITE, 0xDE02, U), (WRITE, 0x8800, 0x00), (WRITE, 0xDE02, K16),
               (READ, 0x8800, lo[1][0x800])]
    hi[0] = bytearray(with_program(hi[0], program(2, 0xE000, probes)))
    chips = [(2, b, 0x8000, bytes(d)) for b, d in lo.items()] + [(2, b, 0xA000, bytes(d)) for b, d in hi.items()]
    save("crttest-efflash", probes, False, 32, 1, 0, sorted(chips, key=lambda c: (c[1], c[2])))


def magic_desk_16k() -> None:
    """Magic Desk 16K at full size: 128 banks of 16 kB, a file just over 2 MiB (SDRAM only)."""
    banks = 128
    rom = [pattern(b, 0x4000) for b in range(banks)]
    probes = []
    for b in [127, 64, 1, 126, 0, 100]:
        probes += [(WRITE, 0xDE00, b),
                   (READ, 0x8400, rom[b][0x400]), (READ, 0x9FFF, rom[b][0x1FFF]),
                   (READ, 0xA800, rom[b][0x2800]), (READ, 0xBFFF, rom[b][0x3FFF])]
    probes += [(WRITE, 0x8800, 0x77),                      # goes to the RAM underneath
               (WRITE, 0xDE00, 0x80), (READ, 0x8800, 0x77)]  # cartridge off
    rom[0] = bytearray(with_program(rom[0], program(0, 0x8000, probes)))
    save("crttest-md16k", probes, False, 85, 0, 0, [(0, b, 0x8000, bytes(rom[b])) for b in range(banks)])


def unsupported() -> None:
    """Retro Replay, a type cartridge.vhd lacks: the parser must refuse it."""
    crt(HERE / "crttest-badtype.crt", False, 36, 0, 1, [(0, 0, 0x8000, bytes(pattern(0, 0x2000)))])
    print("crttest-badtype.crt")


if __name__ == "__main__":
    c128_generic()
    c128_generic("crttest-c128fast", 3)
    c64_16k()
    ultimax_4k()
    easyflash()
    easyflash_flash()
    magic_desk_16k()
    unsupported()
