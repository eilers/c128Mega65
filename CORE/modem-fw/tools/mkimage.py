#!/usr/bin/env python3
"""Put the boot ROM's 16-byte header in front of the firmware binary.

usage: mkimage.py firmware.bin modem.bin
Header: magic "MDM1", length, 32-bit sum of the (padded) image's words, version 1.
"""
import struct
import sys

MAGIC = 0x314D444D
MAX_LEN = 0x000FEFF0  # one byte per 16-bit HyperRAM word, 255 windows of 4 KW

data = open(sys.argv[1], "rb").read()
if len(data) > MAX_LEN:
    sys.exit("image too large: %d bytes" % len(data))
padded = data + b"\0" * (-len(data) % 4)
total = sum(struct.unpack("<%dI" % (len(padded) // 4), padded)) & 0xFFFFFFFF
header = struct.pack("<4I", MAGIC, len(data), total, 1)
open(sys.argv[2], "wb").write(header + padded)
print("%s: %d bytes" % (sys.argv[2], len(header) + len(padded)))
