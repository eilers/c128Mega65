#!/usr/bin/env python3
"""The firmware files test_boot.c loads: the real one, a bigger one, and broken ones.

usage: make_cases.py firmware.bin outdir
"""
import os
import random
import struct
import sys
import zlib

FMT = "<8I32s"   # include/fw_image.h
fw = open(sys.argv[1], "rb").read()
out = sys.argv[2]
hdr = list(struct.unpack_from(FMT, fw))   # magic, header_ver, hw_ver, load, entry, size, crc, 0, version


def write(name, data):
    open(os.path.join(out, name), "wb").write(data)


def with_header(fields, image):
    return struct.pack(FMT, *fields).ljust(512, b"\0") + image


image = fw[512:]
write("firmware.bin", fw)

# 1 MB and 3 bytes: many clusters, and a size that isn't whole words
big = random.Random(1).randbytes(1024 * 1024 + 3)
f = hdr[:]
f[5], f[6] = len(big), zlib.crc32(big)
write("fw1m.bin", with_header(f, big))

bad = bytearray(fw)
bad[512 + 100] ^= 0x01
write("crc.bin", bytes(bad))

f = hdr[:]
f[2] = 99
write("hwver.bin", with_header(f, image))
write("short.bin", fw[:-100])
write("hdrshort.bin", fw[:300])
write("magic.bin", b"Not a firmware file.\n" * 50)

f = hdr[:]
f[5] = 0x400000                           # more than the region holds
write("big.bin", with_header(f, image))

f = hdr[:]
f[4] = f[3] + f[5]                        # entry just past the image
write("entry.bin", with_header(f, image))

f = hdr[:]
f[3] += 2                                 # load address not on a word
f[4] += 2
write("align.bin", with_header(f, image))
