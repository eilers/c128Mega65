#!/usr/bin/env python3
"""Check a firmware.bin before it ships: its header (include/fw_image.h), the hardware
interface it was built for (M2M_RV_HW_VERSION in include/m2m_rv.h) and its CRC. Prints
the firmware's version, the commit it was built from.
usage: check_image.py firmware.bin m2m_rv.h
"""
import re
import struct
import sys
import zlib

FW_MAGIC = 0x5652324D       # "M2RV"
FW_HEADER_SIZE = 512

image, header_h = sys.argv[1:]
data = open(image, "rb").read()
hw = int(re.search(r"#define M2M_RV_HW_VERSION\s+(\d+)u", open(header_h).read()).group(1))

if len(data) < FW_HEADER_SIZE:
    sys.exit("%s: shorter than its header" % image)
magic, _, hw_ver, _, _, size, crc, _, version = struct.unpack("<8I32s", data[:64])
body = data[FW_HEADER_SIZE:]
version = version.rstrip(b"\0").decode("ascii", "replace")
if magic != FW_MAGIC:
    sys.exit("%s: not a firmware image" % image)
if hw_ver != hw:
    sys.exit("%s: built for hardware interface %d, the cores have %d" % (image, hw_ver, hw))
if size != len(body) or zlib.crc32(body) != crc:
    sys.exit("%s: size or CRC wrong" % image)
if version.endswith("-dirty"):
    sys.exit("%s: built from uncommitted sources (%s)" % (image, version))
print(version)
