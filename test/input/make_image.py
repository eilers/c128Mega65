#!/usr/bin/env python3
"""Assemble INPUTTEST with cc65 and put it on input-test.d64."""

from __future__ import annotations

import hashlib
import importlib.util
import subprocess
import sys
import tempfile
from pathlib import Path

sys.dont_write_bytecode = True

HERE = Path(__file__).resolve().parent


def load_d64_writer():
    path = HERE.parent / "157x-rw" / "create_images.py"
    spec = importlib.util.spec_from_file_location("create_images", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def main() -> None:
    images = load_d64_writer()
    with tempfile.TemporaryDirectory() as tmp:
        obj = Path(tmp) / "inputtest.o"
        prg = HERE / "inputtest.prg"
        subprocess.run(["ca65", "-o", str(obj), str(HERE / "inputtest.s")], check=True)
        subprocess.run(["ld65", "-C", str(HERE / "inputtest.cfg"), "-S", "0x1C01", "-o", str(prg),
                        str(obj)], check=True)
    print(f"{prg}: {prg.stat().st_size} bytes")
    disk = images.Disk(35)
    disk.add_file("INPUTTEST", prg.read_bytes())
    disk.write_directory()
    disk.write_bam()
    bam = disk.offset(18, 0)
    disk.image[bam + 0x90:bam + 0xA0] = b"INPUT TEST".ljust(16, b"\xA0")
    disk.image[bam + 0xA2:bam + 0xA4] = b"IN"
    out = HERE / "input-test.d64"
    out.write_bytes(disk.image)
    print(f"{out}: sha256={hashlib.sha256(disk.image).hexdigest()}")


if __name__ == "__main__":
    main()
