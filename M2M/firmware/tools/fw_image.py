#!/usr/bin/env python3
"""Wrap a linked firmware binary into the file the boot ROM loads (include/fw_image.h):
a 512-byte header block, then the binary.

usage: fw_image.py image.bin firmware.bin hw_version load_addr entry version
"""
import struct
import sys
import zlib

FW_MAGIC = 0x5652324D       # "M2RV"
FW_HEADER_VER = 1
FW_HEADER_SIZE = 512

src, dst, hw_ver, load_addr, entry, version = sys.argv[1:]
data = open(src, "rb").read()
v = version.encode("ascii")[:31]
header = struct.pack("<8I32s", FW_MAGIC, FW_HEADER_VER, int(hw_ver, 0), int(load_addr, 0),
                     int(entry, 0), len(data), zlib.crc32(data), 0, v)
with open(dst, "wb") as f:
    f.write(header.ljust(FW_HEADER_SIZE, b"\0"))
    f.write(data)
print("%s: %s, %d bytes, CRC %08X" % (dst, version, len(data), zlib.crc32(data)))
