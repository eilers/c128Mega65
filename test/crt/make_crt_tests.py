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
READ, WRITE = 1, 2

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
        chips: list[tuple[int, int, int, bytes]]) -> None:
    """chips: (chip type, bank, load address, data)"""
    sig = b"C128 CARTRIDGE  " if c128 else b"C64 CARTRIDGE   "
    out = bytearray(sig + struct.pack(">IHHBBB", 0x40, 0x0100, hw_type, exrom, game, 0) + bytes(5))
    out += path.stem.upper().encode().ljust(32, b"\0")
    for chip_type, bank, load, data in chips:
        out += b"CHIP" + struct.pack(">IHHHH", 0x10 + len(data), chip_type, bank, load, len(data)) + data
    path.write_bytes(out)


def save(name: str, probes: list[tuple[int, int, int]], *args) -> None:
    crt(HERE / f"{name}.crt", *args)
    (HERE / f"{name}.exp").write_text("".join(f"{v:02X}\n" for op, _, v in probes if op == READ))
    print(f"{name}.crt: {sum(op == READ for op, _, _ in probes)} reads")


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
               (WRITE, 0xDE02, 0x04), (READ, 0x8000, 0x77)]                # cartridge off
    hi[0] = bytearray(with_program(hi[0], program(2, 0xE000, probes)))
    chips = []
    for b in range(banks):
        chips += [(2, b, 0x8000, bytes(lo[b])), (2, b, 0xA000, bytes(hi[b]))]
    save("crttest-ef", probes, False, 32, 1, 0, chips)


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
    unsupported()
