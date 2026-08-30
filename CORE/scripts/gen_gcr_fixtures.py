#!/usr/bin/env python3
"""Independent Commodore 1541 GCR reference and fixture generator."""

from __future__ import annotations

import argparse
import json
from pathlib import Path
import unittest


GCR_ENCODE = (
    0x0A, 0x0B, 0x12, 0x13, 0x0E, 0x0F, 0x16, 0x17,
    0x09, 0x19, 0x1A, 0x1B, 0x0D, 0x1D, 0x1E, 0x15,
)
GCR_DECODE = {code: nibble for nibble, code in enumerate(GCR_ENCODE)}

D64_TRACKS = 35
D71_TRACKS = 70
SECTOR_SIZE = 256


def encode_nibble(nibble: int) -> int:
    """Encode one 4-bit value as a Commodore 5-bit GCR code."""
    if not 0 <= nibble <= 0x0F:
        raise ValueError(f"nibble out of range: {nibble}")
    return GCR_ENCODE[nibble]


def decode_code(code: int) -> int:
    """Decode one 5-bit GCR code, rejecting all 16 unused code words."""
    if not 0 <= code <= 0x1F:
        raise ValueError(f"GCR code out of range: {code}")
    try:
        return GCR_DECODE[code]
    except KeyError as exc:
        raise ValueError(f"invalid GCR code: 0x{code:02x}") from exc


def encode_four(data: bytes) -> bytes:
    """Encode four bytes into the five-byte on-disk GCR representation."""
    if len(data) != 4:
        raise ValueError("encode_four requires exactly four bytes")
    bits = 0
    for byte in data:
        bits = (bits << 5) | encode_nibble(byte >> 4)
        bits = (bits << 5) | encode_nibble(byte & 0x0F)
    return bits.to_bytes(5, "big")


def decode_five(data: bytes) -> bytes:
    """Decode five on-disk GCR bytes, rejecting any invalid code word."""
    if len(data) != 5:
        raise ValueError("decode_five requires exactly five bytes")
    bits = int.from_bytes(data, "big")
    decoded = bytearray()
    for shift in range(35, -1, -10):
        hi = decode_code((bits >> shift) & 0x1F)
        lo = decode_code((bits >> (shift - 5)) & 0x1F)
        decoded.append((hi << 4) | lo)
    return bytes(decoded)


def encode_bytes(data: bytes) -> bytes:
    """Encode a byte string whose length is a multiple of four."""
    if len(data) % 4:
        raise ValueError("GCR input length must be a multiple of four")
    return b"".join(encode_four(data[pos:pos + 4])
                    for pos in range(0, len(data), 4))


def decode_bytes(data: bytes) -> bytes:
    """Decode a byte string whose length is a multiple of five."""
    if len(data) % 5:
        raise ValueError("GCR input length must be a multiple of five")
    return b"".join(decode_five(data[pos:pos + 5])
                    for pos in range(0, len(data), 5))


def xor_checksum(data: bytes) -> int:
    """Return the XOR checksum used by 1541 header and data blocks."""
    result = 0
    for byte in data:
        result ^= byte
    return result


def sectors_per_track(track: int, tracks: int = D64_TRACKS) -> int:
    """Return sector count for a one-based logical D64/D71 track."""
    if tracks not in (D64_TRACKS, D71_TRACKS):
        raise ValueError("supported geometries are D64 (35) and D71 (70) tracks")
    if not 1 <= track <= tracks:
        raise ValueError(f"track out of range for {tracks}-track image: {track}")
    physical_track = track if track <= D64_TRACKS else track - D64_TRACKS
    if physical_track <= 17:
        return 21
    if physical_track <= 24:
        return 19
    if physical_track <= 30:
        return 18
    return 17


def total_sectors(tracks: int) -> int:
    """Return total sectors for a standard D64 or D71 image."""
    return sum(sectors_per_track(track, tracks)
               for track in range(1, tracks + 1))


def image_size(tracks: int) -> int:
    """Return image byte size excluding optional error-information bytes."""
    return total_sectors(tracks) * SECTOR_SIZE


def sector_index(track: int, sector: int, tracks: int = D64_TRACKS) -> int:
    """Convert a one-based track and zero-based sector to a linear sector."""
    count = sectors_per_track(track, tracks)
    if not 0 <= sector < count:
        raise ValueError(f"sector out of range on track {track}: {sector}")
    return sum(sectors_per_track(item, tracks)
               for item in range(1, track)) + sector


def make_header(track: int, sector: int, disk_id: bytes = bytes((0x12, 0x34))) -> bytes:
    """Build the eight decoded bytes represented by a 1541 header block."""
    if len(disk_id) != 2:
        raise ValueError("disk ID must contain exactly two bytes")
    sectors_per_track(track, D64_TRACKS)
    if not 0 <= sector < sectors_per_track(track, D64_TRACKS):
        raise ValueError("sector out of range")
    checksum = sector ^ track ^ disk_id[0] ^ disk_id[1]
    return bytes((0x08, checksum, sector, track,
                  disk_id[0], disk_id[1], 0x0F, 0x0F))


def make_data_block(payload: bytes) -> bytes:
    """Build the 260 decoded bytes represented by a 1541 data block."""
    if len(payload) != SECTOR_SIZE:
        raise ValueError("1541 sector payload must contain exactly 256 bytes")
    return bytes((0x07,)) + payload + bytes((xor_checksum(payload), 0x00, 0x00))


def write_hex(path: Path, data: bytes) -> None:
    path.write_text("\n".join(f"{byte:02x}" for byte in data) + "\n",
                    encoding="ascii")


def generate_fixtures(output_dir: Path) -> None:
    """Generate deterministic, compact fixtures used by HDL and host tests."""
    output_dir.mkdir(parents=True, exist_ok=True)
    payload = bytes((index * 73 + 41) & 0xFF for index in range(SECTOR_SIZE))
    header = make_header(track=18, sector=0)
    data_block = make_data_block(payload)

    write_hex(output_dir / "gcr_nibble_codes.hex", bytes(GCR_ENCODE))
    write_hex(output_dir / "gcr_header_raw.hex", header)
    write_hex(output_dir / "gcr_header_encoded.hex", encode_bytes(header))
    write_hex(output_dir / "gcr_data_raw.hex", data_block)
    write_hex(output_dir / "gcr_data_encoded.hex", encode_bytes(data_block))

    geometry = {
        "d64": {
            "tracks": D64_TRACKS,
            "sectors": total_sectors(D64_TRACKS),
            "bytes": image_size(D64_TRACKS),
            "directory_sector_index": sector_index(18, 0, D64_TRACKS),
        },
        "d71": {
            "tracks": D71_TRACKS,
            "sectors": total_sectors(D71_TRACKS),
            "bytes": image_size(D71_TRACKS),
            "side_two_sector_index": sector_index(36, 0, D71_TRACKS),
        },
    }
    (output_dir / "gcr_geometry.json").write_text(
        json.dumps(geometry, indent=2, sort_keys=True) + "\n", encoding="ascii")


class ReferenceTests(unittest.TestCase):
    def test_all_nibbles_and_invalid_codes(self) -> None:
        for nibble in range(16):
            self.assertEqual(decode_code(encode_nibble(nibble)), nibble)
        invalid = set(range(32)) - set(GCR_ENCODE)
        self.assertEqual(len(invalid), 16)
        for code in invalid:
            with self.assertRaises(ValueError):
                decode_code(code)

    def test_exhaustive_byte_round_trip(self) -> None:
        for byte in range(256):
            raw = bytes((byte, byte ^ 0x55, byte ^ 0xAA, byte ^ 0xFF))
            self.assertEqual(decode_five(encode_four(raw)), raw)

    def test_checksums_and_blocks(self) -> None:
        payload = bytes(range(256))
        block = make_data_block(payload)
        self.assertEqual(len(block), 260)
        self.assertEqual(block[257], xor_checksum(payload))
        self.assertEqual(decode_bytes(encode_bytes(block)), block)
        header = make_header(18, 0)
        self.assertEqual(header[1], xor_checksum(header[2:6]))
        self.assertEqual(decode_bytes(encode_bytes(header)), header)

    def test_geometry(self) -> None:
        self.assertEqual([sectors_per_track(t) for t in (1, 17, 18, 24, 25, 30, 31, 35)],
                         [21, 21, 19, 19, 18, 18, 17, 17])
        self.assertEqual(total_sectors(D64_TRACKS), 683)
        self.assertEqual(image_size(D64_TRACKS), 174848)
        self.assertEqual(sector_index(18, 0), 357)
        self.assertEqual(total_sectors(D71_TRACKS), 1366)
        self.assertEqual(image_size(D71_TRACKS), 349696)
        self.assertEqual(sector_index(36, 0, D71_TRACKS), 683)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--output-dir",
        type=Path,
        default=Path(__file__).resolve().parents[1] / "sim" / "fixtures",
        help="fixture output directory",
    )
    parser.add_argument("--self-test", action="store_true",
                        help="run reference implementation unit tests")
    args = parser.parse_args()

    if args.self_test:
        suite = unittest.defaultTestLoader.loadTestsFromTestCase(ReferenceTests)
        result = unittest.TextTestRunner(verbosity=2).run(suite)
        if not result.wasSuccessful():
            return 1
    generate_fixtures(args.output_dir)
    print(f"generated GCR fixtures in {args.output_dir}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
