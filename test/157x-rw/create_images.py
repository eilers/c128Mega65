#!/usr/bin/env python3
"""Build deterministic D64/D71 read/write qualification images."""

from __future__ import annotations

import argparse
import hashlib
from pathlib import Path

SECTORS = tuple([21] * 17 + [19] * 7 + [18] * 6 + [17] * 5)
TOKENS = {
    "END": 0x80, "FOR": 0x81, "NEXT": 0x82, "INPUT#": 0x84,
    "INPUT": 0x85, "GOTO": 0x89, "IF": 0x8B, "GOSUB": 0x8D,
    "REM": 0x8F, "PRINT#": 0x98, "PRINT": 0x99, "CLOSE": 0xA0,
    "OPEN": 0x9F, "TO": 0xA4, "THEN": 0xA7, "AND": 0xAF,
    "OR": 0xB0, "CHR$": 0xC7,
}


def sectors_on_track(track: int) -> int:
    return SECTORS[(track - 1) % 35]


def sector_index(track: int, sector: int) -> int:
    side = (track - 1) // 35
    physical = (track - 1) % 35
    return side * 683 + sum(SECTORS[:physical]) + sector


def tokenize_line(text: str) -> bytes:
    result = bytearray()
    pos = 0
    quoted = False
    rem = False
    names = sorted(TOKENS, key=len, reverse=True)
    while pos < len(text):
        char = text[pos]
        if char == '"':
            quoted = not quoted
            result.append(ord(char))
            pos += 1
            continue
        if not quoted and not rem:
            upper = text[pos:].upper()
            match = next((name for name in names if upper.startswith(name)), None)
            if match:
                result.append(TOKENS[match])
                pos += len(match)
                rem = match == "REM"
                continue
        result.append(ord(char))
        pos += 1
    return bytes(result)


def tokenize_basic(source: str, load_address: int = 0x1C01) -> bytes:
    records: list[tuple[int, bytes]] = []
    for raw in source.splitlines():
        raw = raw.strip()
        if not raw:
            continue
        number, body = raw.split(maxsplit=1)
        records.append((int(number), tokenize_line(body)))
    output = bytearray(load_address.to_bytes(2, "little"))
    address = load_address
    for number, body in records:
        next_address = address + 2 + 2 + len(body) + 1
        output += next_address.to_bytes(2, "little")
        output += number.to_bytes(2, "little")
        output += body + b"\0"
        address = next_address
    output += b"\0\0"
    return bytes(output)


class Disk:
    def __init__(self, tracks: int):
        self.tracks = tracks
        self.image = bytearray((683 if tracks == 35 else 1366) * 256)
        self.used: set[tuple[int, int]] = {(18, 0), (18, 1)}
        if tracks == 70:
            self.used.update((53, sector) for sector in range(sectors_on_track(53)))
        self.entries: list[tuple[int, int, int, str, int]] = []

    def offset(self, track: int, sector: int) -> int:
        return sector_index(track, sector) * 256

    def free_sectors(self, tracks: range | None = None) -> list[tuple[int, int]]:
        tracks = tracks or range(1, self.tracks + 1)
        return [(track, sector) for track in tracks
                for sector in range(sectors_on_track(track))
                if (track, sector) not in self.used and track not in (18, 53)]

    def add_file(self, name: str, data: bytes, file_type: int = 0x82,
                 candidates: list[tuple[int, int]] | None = None) -> None:
        needed = max(1, (len(data) + 253) // 254)
        available = candidates or self.free_sectors()
        chain = [item for item in available if item not in self.used][:needed]
        if len(chain) != needed:
            raise ValueError(f"not enough space for {name}")
        for index, (track, sector) in enumerate(chain):
            self.used.add((track, sector))
            chunk = data[index * 254:(index + 1) * 254]
            block = self.offset(track, sector)
            if index + 1 < len(chain):
                self.image[block:block + 2] = bytes(chain[index + 1])
            else:
                self.image[block] = 0
                self.image[block + 1] = len(chunk) + 1
            self.image[block + 2:block + 2 + len(chunk)] = chunk
        self.entries.append((file_type, chain[0][0], chain[0][1], name, len(chain)))

    def write_bam(self) -> None:
        bam = self.offset(18, 0)
        self.image[bam:bam + 4] = bytes((18, 1, 0x41, 0x80 if self.tracks == 70 else 0))
        for track in range(1, 36):
            free = [sector for sector in range(sectors_on_track(track))
                    if (track, sector) not in self.used]
            base = bam + 4 + (track - 1) * 4
            self.image[base] = len(free)
            for sector in free:
                self.image[base + 1 + sector // 8] |= 1 << (sector % 8)
        self.image[bam + 0x90:bam + 0xA0] = b"157X RW TEST".ljust(16, b"\xA0")
        self.image[bam + 0xA0:bam + 0xA2] = b"\xA0\xA0"
        self.image[bam + 0xA2:bam + 0xA4] = b"65"
        self.image[bam + 0xA4:bam + 0xAB] = b"\xA0" + b"2A" + b"\xA0" * 4
        if self.tracks == 70:
            bam2 = self.offset(53, 0)
            for track in range(36, 71):
                free = [sector for sector in range(sectors_on_track(track))
                        if (track, sector) not in self.used]
                self.image[bam + 0xDD + track - 36] = len(free)
                for sector in free:
                    self.image[bam2 + (track - 36) * 3 + sector // 8] |= 1 << (sector % 8)

    def write_directory(self) -> None:
        directory = self.offset(18, 1)
        self.image[directory:directory + 2] = b"\0\xFF"
        for index, (kind, track, sector, name, blocks) in enumerate(self.entries):
            base = directory + 2 + index * 32
            self.image[base] = kind
            self.image[base + 1:base + 3] = bytes((track, sector))
            self.image[base + 3:base + 19] = name.encode("ascii").ljust(16, b"\xA0")
            self.image[base + 28:base + 30] = blocks.to_bytes(2, "little")


def build(tracks: int, program: bytes) -> bytes:
    disk = Disk(tracks)
    side0 = disk.free_sectors(range(1, 36))
    disk.add_file("RWTEST", program, candidates=side0)
    side_data = b"SIDE1 OK\r" + bytes((index * 37 + 11) & 0xFF for index in range(245))
    if tracks == 70:
        disk.add_file("SIDE1DAT", side_data, file_type=0x81,
                      candidates=disk.free_sectors(range(36, 71)))
        # Consume usable side-0 data sectors so RWTEST's temporary file is
        # allocated on side 1 and therefore qualifies D71 write-back there.
        remaining = disk.free_sectors(range(1, 36))
        filler = bytes((index * 13 + 7) & 0xFF for index in range(len(remaining) * 254))
        disk.add_file("SIDE0FILL", filler, file_type=0xC1, candidates=remaining)
    else:
        disk.add_file("SIDE1DAT", side_data, file_type=0x81,
                      candidates=disk.free_sectors(range(1, 36)))
    disk.write_bam()
    disk.write_directory()
    return bytes(disk.image)


def validate(image: bytes, tracks: int) -> None:
    expected = 174848 if tracks == 35 else 349696
    assert len(image) == expected
    bam = sector_index(18, 0) * 256
    directory = sector_index(18, 1) * 256
    assert image[bam:bam + 3] == b"\x12\x01\x41"
    names = []
    starts = {}
    for entry in range(8):
        base = directory + 2 + entry * 32
        if image[base] & 0x80:
            name = bytes(value & 0x7F for value in image[base + 3:base + 19]
                         if value != 0xA0).decode("ascii")
            names.append(name)
            starts[name] = image[base + 1]
    assert "RWTEST" in names and "SIDE1DAT" in names
    assert starts["SIDE1DAT"] >= (36 if tracks == 70 else 1)
    if tracks == 70:
        assert "SIDE0FILL" in names
        assert sum(image[bam + 0xDD:bam + 0x100]) > 0


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--output-dir", type=Path, default=Path(__file__).parent)
    args = parser.parse_args()
    source = (Path(__file__).parent / "rwtest.bas").read_text(encoding="ascii")
    program = tokenize_basic(source)
    args.output_dir.mkdir(parents=True, exist_ok=True)
    (args.output_dir / "rwtest.prg").write_bytes(program)
    for tracks, suffix in ((35, "d64"), (70, "d71")):
        image = build(tracks, program)
        validate(image, tracks)
        path = args.output_dir / f"157x-rw-test.{suffix}"
        path.write_bytes(image)
        print(f"{path}: {len(image)} bytes sha256={hashlib.sha256(image).hexdigest()}")


if __name__ == "__main__":
    main()
