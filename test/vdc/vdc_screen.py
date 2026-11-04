#!/usr/bin/env python3
"""The 80-column screen from the VDC's memory, as text: by REST (machine:vdcregs and
machine:vdcmem) or by the serial monitor (vdcreg: and vdc:). The registers say where things
are: R12/R13 the screen, R20/R21 the attributes, R1 the columns, R6 the rows, R27 the bytes
skipped after each row, R28 the character set. Nothing on the C128 changes.

  vdc_screen.py HOST [--password PW]      by REST
  vdc_screen.py --monitor [-p PORT]       by the serial monitor (M2M/firmware/tools/mon.py)

  --attrs      each row's attributes in hex under it
  --raw        screen codes in hex instead of text
  --png FILE   the screen drawn from the VDC's own character set and colours (text mode)

Text: by the character set the cell's attribute picks (bit 7, the second set): the KERNAL's
screen codes, or ASCII where that set holds an ASCII font (CP/M loads one; told by its glyph
for 0x61 being an 'a'). Reverse cells read as their character, graphics as '#'.
"""
import argparse
import importlib.util
import struct
import sys
import urllib.request
import zlib
from pathlib import Path

HERE = Path(__file__).resolve().parent
MON_PY = HERE.parent.parent / "M2M" / "firmware" / "tools" / "mon.py"

LOWER_A = bytes([0x00, 0x00, 0x3C, 0x06, 0x3E, 0x66, 0x3E, 0x00])   # the C128 font's 'a'

# RGBI, as the core's default VDC palette
PALETTE = [(0, 0, 0), (85, 85, 85), (0, 0, 170), (85, 85, 255), (0, 170, 0), (85, 255, 85),
           (0, 170, 170), (85, 255, 255), (170, 0, 0), (255, 85, 85), (170, 0, 170),
           (255, 85, 255), (170, 85, 0), (255, 255, 85), (170, 170, 170), (255, 255, 255)]


class Rest:
    def __init__(self, host, password):
        self.base = "http://%s/v1/" % host
        self.password = password

    def get(self, path):
        req = urllib.request.Request(self.base + path)
        if self.password:
            req.add_header("X-Password", self.password)
        with urllib.request.urlopen(req, timeout=20) as r:
            return r.read()

    def regs(self):
        return self.get("machine:vdcregs")

    def mem(self, addr, n):
        return self.get("machine:vdcmem?address=%04X&length=%d" % (addr, n))


class Mon:
    def __init__(self, port):
        spec = importlib.util.spec_from_file_location("mon", MON_PY)
        self.m = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.m)
        self.mon = self.m.Monitor(port or self.m.find_port(), 2000000)
        self.mon.sync()

    def regs(self):
        return self.mon.read_bytes(self.m.Addr("vdcreg:0"), 40)

    def mem(self, addr, n):
        return self.mon.read_bytes(self.m.Addr("vdc:%X" % addr), n)


class Screen:
    """The text screen the registers describe, read from the VDC's memory"""

    def __init__(self, src):
        r = self.regs = src.regs()
        self.cols, self.rows = r[1], r[6]
        self.screen = r[12] << 8 | r[13]
        self.attr = r[20] << 8 | r[21]
        self.stride = self.cols + r[27]
        self.bitmap = bool(r[25] & 0x80)
        self.attrs_on = bool(r[25] & 0x40)
        n = self.stride * self.rows
        self.codes = self.read(src, self.screen, n)
        self.attrs = self.read(src, self.attr, n) if self.attrs_on else bytes(n)
        self.src = src
        base = (r[28] >> 5) * 0x2000
        self.ascii = tuple(src.mem(base + half + 0x61 * 16, 8) == LOWER_A for half in (0, 0x1000))

    @staticmethod
    def read(src, addr, n):
        # The VDC's addresses wrap at 64 KB
        first = min(n, 0x10000 - addr)
        data = src.mem(addr, first) if first else b""
        return data + (src.mem(0, n - first) if n > first else b"")

    def row(self, y):
        a = y * self.stride
        return self.codes[a:a + self.cols], self.attrs[a:a + self.cols]

    def text(self):
        return [decode(*self.row(y), self.ascii) for y in range(self.rows)]

    def png(self, path):
        r = self.regs
        height = (r[9] & 0x1F) + 1                       # scan lines a character row
        shown = min((r[23] & 0x1F) + 1, height)          # of which the glyph's
        base = (r[28] >> 5) * 0x2000
        charset = self.read(self.src, base, 0x2000)      # two sets of 256, 16 bytes each
        fg_all, bg = r[26] >> 4, r[26] & 15
        under = r[29] & 0x1F
        rvs_all = bool(r[24] & 0x40)
        w = self.cols * 8
        lines = []
        for y in range(self.rows):
            codes, attrs = self.row(y)
            for s in range(height):
                line = bytearray()
                for code, a in zip(codes, attrs):
                    fg = a & 15 if self.attrs_on else fg_all
                    glyph = charset[(0x1000 if a & 0x80 else 0) + code * 16 + s] if s < shown else 0
                    if a & 0x20 and s == under:
                        glyph = 0xFF
                    if bool(a & 0x40) != rvs_all:
                        glyph ^= 0xFF
                    line += bytes(fg if glyph & (0x80 >> b) else bg for b in range(8))
                lines.append(bytes(line))
        write_png(path, w, len(lines), lines)


def decode(codes, attrs, ascii=(False, False)):
    out = []
    for code, a in zip(codes, attrs):
        c, lower = code & 0x7F, bool(a & 0x80)
        if ascii[lower]:
            out.append(chr(c) if 0x20 <= c < 0x7F else "#")
        elif c == 0:
            out.append("@")
        elif c <= 26:
            out.append(chr((0x60 if lower else 0x40) + c))
        elif c < 32:
            out.append("[#]^<"[c - 27])
        elif c < 64:
            out.append(chr(c))
        elif lower and 0x41 <= c <= 0x5A:
            out.append(chr(c))
        else:
            out.append("#")
    return "".join(out)


def write_png(path, w, h, lines):
    def chunk(kind, data):
        return struct.pack(">I", len(data)) + kind + data + struct.pack(">I", zlib.crc32(kind + data))
    raw = b"".join(b"\x00" + line for line in lines)
    png = b"\x89PNG\r\n\x1a\n"
    png += chunk(b"IHDR", struct.pack(">IIBBBBB", w, h, 8, 3, 0, 0, 0))
    png += chunk(b"PLTE", bytes(c for rgb in PALETTE for c in rgb))
    png += chunk(b"IDAT", zlib.compress(raw, 9))
    png += chunk(b"IEND", b"")
    Path(path).write_bytes(png)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("host", nargs="?")
    ap.add_argument("--password")
    ap.add_argument("--monitor", action="store_true")
    ap.add_argument("-p", "--port")
    ap.add_argument("--attrs", action="store_true")
    ap.add_argument("--raw", action="store_true")
    ap.add_argument("--png")
    a = ap.parse_args()
    if not a.monitor and not a.host:
        ap.error("HOST, or --monitor")
    src = Mon(a.port) if a.monitor else Rest(a.host, a.password)
    s = Screen(src)
    if s.bitmap:
        sys.exit("vdc_screen.py: the VDC is in bitmap mode (R25 bit 7)")
    for y in range(s.rows):
        codes, attrs = s.row(y)
        print(" ".join("%02X" % c for c in codes) if a.raw else decode(codes, attrs, s.ascii).rstrip())
        if a.attrs:
            print(" ".join("%02X" % c for c in attrs))
    if a.png:
        s.png(a.png)


if __name__ == "__main__":
    main()
