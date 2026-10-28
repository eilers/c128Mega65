#!/usr/bin/env python3
"""Build the cartridge ROM tests: romtest-8k.crt, romtest-16k.crt and romtest-ultimax.crt.

Each is romtest.s's 2 KB code block plus a pattern the test computes itself. Needs ca65 and
ld65 from cc65.
"""

from __future__ import annotations

import hashlib
import struct
import subprocess
import tempfile
from pathlib import Path

HERE = Path(__file__).resolve().parent
SEED = 0xACE1

# name, MODE, CRT EXROM/GAME, ROM base, ROM size, code block, pattern area
CARTS = [
    ("romtest-8k", 0, (0, 1), 0x8000, 0x2000, 0x8000, (0x8800, 0xA000)),
    ("romtest-16k", 1, (0, 0), 0x8000, 0x4000, 0x8000, (0x8800, 0xC000)),
    ("romtest-ultimax", 2, (1, 0), 0xE000, 0x2000, 0xF800, (0xE000, 0xF800)),
]

# 5x7 glyphs, bolded to C64-style 2-pixel strokes. Anything not here is blank.
GLYPHS = {
    "A": ".###. #...# #...# ##### #...# #...# #...#",
    "B": "####. #...# #...# ####. #...# #...# ####.",
    "C": ".###. #...# #.... #.... #.... #...# .###.",
    "D": "####. #...# #...# #...# #...# #...# ####.",
    "E": "##### #.... #.... ####. #.... #.... #####",
    "F": "##### #.... #.... ####. #.... #.... #....",
    "G": ".###. #...# #.... #.### #...# #...# .####",
    "H": "#...# #...# #...# ##### #...# #...# #...#",
    "I": ".###. ..#.. ..#.. ..#.. ..#.. ..#.. .###.",
    "J": "..### ...#. ...#. ...#. ...#. #..#. .##..",
    "K": "#...# #..#. #.#.. ##... #.#.. #..#. #...#",
    "L": "#.... #.... #.... #.... #.... #.... #####",
    "M": "#...# ##.## #.#.# #.#.# #...# #...# #...#",
    "N": "#...# #...# ##..# #.#.# #..## #...# #...#",
    "O": ".###. #...# #...# #...# #...# #...# .###.",
    "P": "####. #...# #...# ####. #.... #.... #....",
    "Q": ".###. #...# #...# #...# #.#.# #..#. .##.#",
    "R": "####. #...# #...# ####. #.#.. #..#. #...#",
    "S": ".#### #.... #.... .###. ....# ....# ####.",
    "T": "##### ..#.. ..#.. ..#.. ..#.. ..#.. ..#..",
    "U": "#...# #...# #...# #...# #...# #...# .###.",
    "V": "#...# #...# #...# #...# #...# .#.#. ..#..",
    "W": "#...# #...# #...# #.#.# #.#.# #.#.# .#.#.",
    "X": "#...# #...# .#.#. ..#.. .#.#. #...# #...#",
    "Y": "#...# #...# .#.#. ..#.. ..#.. ..#.. ..#..",
    "Z": "##### ....# ...#. ..#.. .#... #.... #####",
    "0": ".###. #...# #..## #.#.# ##..# #...# .###.",
    "1": "..#.. .##.. ..#.. ..#.. ..#.. ..#.. .###.",
    "2": ".###. #...# ....# ...#. ..#.. .#... #####",
    "3": "##### ...#. ..#.. ...#. ....# #...# .###.",
    "4": "...#. ..##. .#.#. #..#. ##### ...#. ...#.",
    "5": "##### #.... ####. ....# ....# #...# .###.",
    "6": "..##. .#... #.... ####. #...# #...# .###.",
    "7": "##### ....# ...#. ..#.. .#... .#... .#...",
    "8": ".###. #...# #...# .###. #...# #...# .###.",
    "9": ".###. #...# #...# .#### ....# ...#. .##..",
    "$": "..#.. .#### #.#.. .###. ..#.# ####. ..#..",
    "+": "..... ..#.. ..#.. ##### ..#.. ..#.. .....",
    "-": "..... ..... ..... ##### ..... ..... .....",
    "/": "..... ....# ...#. ..#.. .#... #.... .....",
    ":": "..... .##.. .##.. ..... .##.. .##.. .....",
    ".": "..... ..... ..... ..... ..... .##.. .##..",
}


def screen_code(ch: str) -> int:
    return ord(ch) - 0x40 if "A" <= ch <= "Z" else ord(ch)


def font() -> bytes:
    """Screen codes 0-63, 8 bytes each."""
    data = bytearray(64 * 8)
    for ch, rows in GLYPHS.items():
        for y, row in enumerate(rows.split()):
            bits = int(row.replace("#", "1").replace(".", "0"), 2) << 2
            data[screen_code(ch) * 8 + y] = bits | bits >> 1
    return bytes(data)


def pattern(length: int) -> bytes:
    """The bytes romtest.s's check computes: step the LFSR, take its low byte."""
    out = bytearray()
    x = SEED
    for _ in range(length):
        lsb = x & 1
        x >>= 1
        if lsb:
            x ^= 0xB400
        out.append(x & 0xFF)
    return bytes(out)


def crt(name: str, exrom: int, game: int, load: int, rom: bytes) -> bytes:
    hdr = bytearray(64)
    hdr[0:16] = b"C64 CARTRIDGE   "
    hdr[16:20] = struct.pack(">I", 64)
    hdr[20:22] = struct.pack(">H", 0x0100)
    hdr[22:24] = struct.pack(">H", 0)       # normal cartridge
    hdr[24] = exrom
    hdr[25] = game
    hdr[32:32 + len(name)] = name.encode("ascii")
    return bytes(hdr) + b"CHIP" + struct.pack(">IHHHH", 16 + len(rom), 0, 0, load, len(rom)) + rom


def build(tmp: Path, name: str, mode: int, lines: tuple[int, int], base: int, size: int,
          block: int, area: tuple[int, int]) -> bytes:
    obj, out = tmp / f"{name}.o", tmp / f"{name}.bin"
    defines = {"MODE": mode, "PSTART": area[0] >> 8, "PEND": area[1] >> 8, "SEED": SEED}
    subprocess.run(["ca65", "--bin-include-dir", str(tmp), "-o", str(obj),
                    *[f"-D{k}={v}" for k, v in defines.items()], str(HERE / "romtest.s")],
                   check=True)
    subprocess.run(["ld65", "-C", str(HERE / "romtest.cfg"), "-D", f"__BLOCK__={block}",
                    "-o", str(out), str(obj)], check=True)
    code = out.read_bytes()
    assert len(code) == 0x800, len(code)

    rom = bytearray(b"\xFF" * size)
    rom[area[0] - base:area[1] - base] = pattern(area[1] - area[0])
    rom[block - base:block - base + len(code)] = code
    title = name.upper().replace("-", " ")
    return crt(title, *lines, base, bytes(rom))


def main() -> None:
    with tempfile.TemporaryDirectory() as t:
        tmp = Path(t)
        (tmp / "font.bin").write_bytes(font())
        for name, *rest in CARTS:
            data = build(tmp, name, *rest)
            path = HERE / f"{name}.crt"
            path.write_bytes(data)
            print(f"{path}: sha256={hashlib.sha256(data).hexdigest()}")


if __name__ == "__main__":
    main()
