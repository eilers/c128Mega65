#!/usr/bin/env python3
"""Assemble the C128 RAM bank test with cc65 and put it on a D64."""

from __future__ import annotations

import hashlib
import importlib.util
import subprocess
import sys
from pathlib import Path

sys.dont_write_bytecode = True   # no __pycache__ next to 157x-rw/create_images.py

HERE = Path(__file__).resolve().parent


def load_d64_writer():
    path = HERE.parent / "157x-rw" / "create_images.py"
    spec = importlib.util.spec_from_file_location("create_images", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def main() -> None:
    obj = HERE / "ramtest.o"
    prg = HERE / "ramtest.prg"
    subprocess.run(["ca65", "-o", str(obj), str(HERE / "ramtest.s")], check=True)
    subprocess.run(["ld65", "-C", str(HERE / "ramtest.cfg"), "-o", str(prg), str(obj)], check=True)
    obj.unlink()

    images = load_d64_writer()
    disk = images.Disk(35)
    disk.add_file("RAMTEST", prg.read_bytes())
    disk.write_directory()
    disk.write_bam()
    bam = disk.offset(18, 0)
    disk.image[bam + 0x90:bam + 0xA0] = b"RAM BANK TEST".ljust(16, b"\xA0")
    disk.image[bam + 0xA2:bam + 0xA4] = b"RB"

    out = HERE / "ram-test.d64"
    out.write_bytes(disk.image)
    print(f"{prg}: {prg.stat().st_size} bytes")
    print(f"{out}: sha256={hashlib.sha256(disk.image).hexdigest()}")


if __name__ == "__main__":
    main()
