#!/usr/bin/env python3
"""Assemble the UCI tests with cc65, for C128 and C64 mode, and put them on a D64."""

from __future__ import annotations

import hashlib
import importlib.util
import subprocess
import sys
import tempfile
from pathlib import Path

sys.dont_write_bytecode = True   # no __pycache__ next to 157x-rw/create_images.py

HERE = Path(__file__).resolve().parent


def load_d64_writer():
    path = HERE.parent / "157x-rw" / "create_images.py"
    spec = importlib.util.spec_from_file_location("create_images", path)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def assemble(src: str, name: str, start: int, defines: list[str], tmp: Path) -> bytes:
    obj = tmp / f"{name}.o"
    prg = HERE / f"{name}.prg"
    subprocess.run(["ca65", "--bin-include-dir", str(tmp), *defines, "-o", str(obj),
                    str(HERE / src)], check=True)
    subprocess.run(["ld65", "-C", str(HERE / "ucitest.cfg"), "-S", hex(start), "-o", str(prg),
                    str(obj)], check=True)
    print(f"{prg}: {prg.stat().st_size} bytes")
    return prg.read_bytes()


def main() -> None:
    images = load_d64_writer()

    # UCIDOS writes an empty disk: zeros, with this BAM and directory sector on track 18
    empty = images.Disk(35)
    empty.write_directory()
    empty.write_bam()
    bam = empty.offset(18, 0)
    empty.image[bam + 0x90:bam + 0xA0] = b"UCI MOUNT".ljust(16, b"\xA0")
    empty.image[bam + 0xA2:bam + 0xA4] = b"UM"
    head = bytes(empty.image[bam:bam + 512])
    rest = bytes(empty.image[:bam]) + bytes(empty.image[bam + 512:])
    assert bam == 0x16500 and not any(rest), "an empty disk is zeros but for track 18"

    with tempfile.TemporaryDirectory() as tmp:
        (Path(tmp) / "d64head.bin").write_bytes(head)
        progs = [("UCITEST", assemble("ucitest.s", "ucitest", 0x1C01, [], Path(tmp))),
                 ("UCITEST64", assemble("ucitest.s", "ucitest64", 0x0801, ["-DC64"], Path(tmp))),
                 ("UCIDOS", assemble("ucidos.s", "ucidos", 0x1C01, [], Path(tmp))),
                 ("UCIDOS64", assemble("ucidos.s", "ucidos64", 0x0801, ["-DC64"], Path(tmp))),
                 ("UCICTRL", assemble("ucictrl.s", "ucictrl", 0x1C01, [], Path(tmp))),
                 ("UCICTRL64", assemble("ucictrl.s", "ucictrl64", 0x0801, ["-DC64"], Path(tmp))),
                 ("UCIREU", assemble("ucireu.s", "ucireu", 0x1C01, [], Path(tmp))),
                 ("UCIREU64", assemble("ucireu.s", "ucireu64", 0x0801, ["-DC64"], Path(tmp))),
                 ("UCICFG", assemble("ucicfg.s", "ucicfg", 0x1C01, [], Path(tmp))),
                 ("UCICFG64", assemble("ucicfg.s", "ucicfg64", 0x0801, ["-DC64"], Path(tmp))),
                 ("UCINET", assemble("ucinet.s", "ucinet", 0x1C01, [], Path(tmp))),
                 ("UCINET64", assemble("ucinet.s", "ucinet64", 0x0801, ["-DC64"], Path(tmp)))]

    disk = images.Disk(35)
    for name, prg in progs:
        disk.add_file(name, prg)
    disk.write_directory()                    # first: past 8 files it takes a second sector
    disk.write_bam()
    bam = disk.offset(18, 0)
    disk.image[bam + 0x90:bam + 0xA0] = b"UCI TEST".ljust(16, b"\xA0")
    disk.image[bam + 0xA2:bam + 0xA4] = b"UC"

    out = HERE / "uci-test.d64"
    out.write_bytes(disk.image)
    print(f"{out}: sha256={hashlib.sha256(disk.image).hexdigest()}")


if __name__ == "__main__":
    main()
