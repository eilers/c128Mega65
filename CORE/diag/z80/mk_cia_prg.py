#!/usr/bin/env python3
"""Emit a C128 8502 PRG that scans CIA1 columns (VICE C128-mode baseline)."""
from __future__ import annotations

import argparse
from pathlib import Path


def le16(x: int) -> bytes:
    return bytes((x & 0xFF, (x >> 8) & 0xFF))


def jsr_chrout(ch: int) -> bytes:
    return bytes((0xA9, ch & 0xFF, 0x20, 0xD2, 0xFF))


def build_prg() -> bytes:
    code_addr = 0x080D
    code = bytearray()

    # Clear screen + header
    code += jsr_chrout(147)
    for ch in b"CIA1-8502 .=idle *=down":
        code += jsr_chrout(ch)
    code += jsr_chrout(13)

    forever = code_addr + len(code)
    code += jsr_chrout(19)  # HOME
    code += bytes((0xA9, 0xFE))  # LDA #$FE
    loop = code_addr + len(code)
    code += bytes(
        (
            0x8D, 0x00, 0xDC,  # STA $DC00
            0x48,  # PHA
            0xAD, 0x01, 0xDC,  # LDA $DC01
            0xC9, 0xFF,  # CMP #$FF
            0xD0, 0x04,  # BNE +4
            0xA9, ord("."),  # LDA #'.'
            0xD0, 0x02,  # BNE +2
            0xA9, ord("*"),  # LDA #'*'
            0x20, 0xD2, 0xFF,  # JSR CHROUT
            0x68,  # PLA
            0x38,  # SEC
            0x2A,  # ROL A
        )
    )
    # BCS loop
    bcs_at = code_addr + len(code)
    rel = (loop - (bcs_at + 2)) & 0xFF
    code += bytes((0xB0, rel))

    for ch in b"  1=col7 Z=col1 Ret=col0":
        code += jsr_chrout(ch)

    code += bytes((0x4C,)) + le16(forever)

    # BASIC: 10 SYS 2061
    basic = le16(0x080D) + le16(10) + bytes((0x9E,)) + b"2061" + bytes((0x00,)) + le16(0)
    assert 0x0801 + len(basic) == 0x080D
    return le16(0x0801) + basic + code


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("-o", type=Path, required=True)
    args = ap.parse_args()
    data = build_prg()
    args.o.parent.mkdir(parents=True, exist_ok=True)
    args.o.write_bytes(data)
    print(f"Wrote {args.o} ({len(data)} bytes)")


if __name__ == "__main__":
    main()
