#!/usr/bin/env python3
"""Assemble the C128 REU test with cc65 and put it on a D64 and in a C128 cartridge.

The cartridge needs VICE's cartconv; set CARTCONV if it isn't at the default path.
"""

from __future__ import annotations

import hashlib
import importlib.util
import os
import subprocess
import sys
from pathlib import Path

sys.dont_write_bytecode = True   # no __pycache__ next to 157x-rw/create_images.py

HERE = Path(__file__).resolve().parent
CARTCONV = os.environ.get("CARTCONV", "/Applications/vice-arm64-gtk3-3.9/bin/cartconv")


def load_d64_writer():
    path = HERE.parent / "157x-rw" / "create_images.py"
    spec = importlib.util.spec_from_file_location("create_images", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def main() -> None:
    obj = HERE / "reutest.o"
    prg = HERE / "reutest.prg"
    subprocess.run(["ca65", "-o", str(obj), str(HERE / "reutest.s")], check=True)
    subprocess.run(["ld65", "-C", str(HERE / "reutest.cfg"), "-o", str(prg), str(obj)], check=True)
    obj.unlink()

    images = load_d64_writer()
    disk = images.Disk(35)
    disk.add_file("REUTEST", prg.read_bytes())
    disk.write_directory()
    disk.write_bam()
    bam = disk.offset(18, 0)
    disk.image[bam + 0x90:bam + 0xA0] = b"REU TEST".ljust(16, b"\xA0")
    disk.image[bam + 0xA2:bam + 0xA4] = b"RT"

    out = HERE / "reu-test.d64"
    out.write_bytes(disk.image)
    print(f"{prg}: {prg.stat().st_size} bytes")
    print(f"{out}: sha256={hashlib.sha256(disk.image).hexdigest()}")

    # The same program as a C128 external function ROM, which starts it in C128 mode
    cart_obj = HERE / "reucart.o"
    cart_bin = HERE / "reucart.bin"
    crt = HERE / "reu-test-c128.crt"
    subprocess.run(["ca65", "--bin-include-dir", str(HERE), "-o", str(cart_obj),
                    str(HERE / "reucart.s")], check=True)
    subprocess.run(["ld65", "-C", str(HERE / "reucart.cfg"), "-o", str(cart_bin), str(cart_obj)],
                   check=True)
    cart_obj.unlink()
    if not Path(CARTCONV).exists():
        print(f"{CARTCONV} not found, so no {crt.name}")
        return
    subprocess.run([CARTCONV, "-t", "c128", "-n", "C128 REU TEST", "-l", "0x8000",
                    "-i", str(cart_bin), "-o", str(crt)], check=True, stdout=subprocess.DEVNULL)
    cart_bin.unlink()
    print(f"{crt}: sha256={hashlib.sha256(crt.read_bytes()).hexdigest()}")


if __name__ == "__main__":
    main()
