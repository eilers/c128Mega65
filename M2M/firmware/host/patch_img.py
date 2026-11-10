#!/usr/bin/env python3
"""Copies of a FAT32 card image that FatFs won't mount, for test_boot's diagnosis cases.

usage: patch_img.py fat32.img outdir
  nolabel.img  the boot sector without its "FAT32   " type text
  fewclus.img  64 sectors per cluster: too few clusters for FAT32
"""
import os
import struct
import sys

img = open(sys.argv[1], "rb").read()
lba = struct.unpack_from("<I", img, 446 + 8)[0]   # partition 1
vbr = lba * 512


def write(name, patches):
    b = bytearray(img)
    for ofs, data in patches:
        b[vbr + ofs:vbr + ofs + len(data)] = data
    open(os.path.join(sys.argv[2], name), "wb").write(b)


write("nolabel.img", [(82, b"        ")])
write("fewclus.img", [(13, b"\x40")])
