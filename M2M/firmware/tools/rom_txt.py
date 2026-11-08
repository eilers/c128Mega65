#!/usr/bin/env python3
"""Turn a binary into the text rv_soc.vhd reads into its boot ROM: one 32-bit
little-endian word per line, written in binary.

usage: rom_txt.py image.bin image.rom max_words
"""
import struct
import sys

data = open(sys.argv[1], "rb").read()
data += b"\0" * (-len(data) % 4)
words = struct.unpack("<%dI" % (len(data) // 4), data)
if len(words) > int(sys.argv[3]):
    sys.exit("%s: %d words, the ROM holds %s" % (sys.argv[1], len(words), sys.argv[3]))
with open(sys.argv[2], "w") as f:
    for w in words:
        f.write("{:032b}\n".format(w))
print("%s: %d of %s words" % (sys.argv[2], len(words), sys.argv[3]))
