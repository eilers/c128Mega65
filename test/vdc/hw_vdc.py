#!/usr/bin/env python3
"""machine:vdcmem and machine:vdcregs on the core, against a program's own reads through
$D600/$D601 (made with writemem and readmem while the C128 is paused): the registers, and VDC
memory across the screen, the attributes, the character set and the rest of the 64 KB. Then
that neither route moves the update address or the selected register, how fast vdcmem is,
and that text typed in 80 columns reads back through vdc_screen.py.

  hw_vdc.py HOST [--password PW]

It resets the C128 first and at the end.
"""
import argparse
import json
import sys
import time

from vdc_screen import Rest, Screen

fails = 0
SPOTS = [0x0000, 0x0001, 0x004F, 0x0050, 0x07CF, 0x0800, 0x0801, 0x0FCF, 0x1000, 0x1FFF,
         0x2000, 0x2001, 0x2208, 0x3000, 0x3FFF, 0x4000, 0x7FFF, 0x8000, 0xC000, 0xFFFF]


class Core(Rest):
    def call(self, method, path, body=None, ctype=None):
        import urllib.request
        req = urllib.request.Request(self.base + path, data=body, method=method)
        if self.password:
            req.add_header("X-Password", self.password)
        if ctype:
            req.add_header("Content-Type", ctype)
        try:
            with urllib.request.urlopen(req, timeout=20) as r:
                return r.status, r.read()
        except urllib.error.HTTPError as e:
            return e.code, e.read()

    def ok(self, method, path):
        status, body = self.call(method, path)
        assert status == 200, (path, status, body)

    def readmem(self, addr, n):
        status, body = self.call("GET", "machine:readmem?address=%04X&length=%d" % (addr, n))
        assert status == 200, body
        return body

    def writemem(self, addr, data):
        status, body = self.call("PUT", "machine:writemem?address=%04X&data=%s" % (addr, bytes(data).hex()))
        assert status == 200, body

    # A program's reads: $D600 selects, $D601 reads (mirrored up to $D6FF)
    def prog_reg(self, r):
        self.writemem(0xD600, [r])
        return self.readmem(0xD601, 1)[0]

    def prog_mem(self, a):
        self.writemem(0xD600, [18, a >> 8, 19, a & 0xFF, 31])   # R18, R19, then select R31
        return self.readmem(0xD601, 1)[0]                        # DA, loaded by the R19 write

    def events(self, *events):
        status, body = self.call("POST", "machine:input", json.dumps({"events": list(events)}).encode(),
                                 "application/json")
        assert status == 200, body


def check(name, ok, got=""):
    global fails
    print("%-56s %s %s" % (name, "ok" if ok else "FAIL", "" if ok else got))
    fails += not ok


def ready(core):
    t0 = time.monotonic()
    while time.monotonic() - t0 < 15:
        status, body = core.call("GET", "machine:readmem?address=0A00&length=1")
        if status == 200 and body[:1] == b"\x03":
            return
        time.sleep(0.2)
    raise RuntimeError("no READY")


def tap(*names):
    return {"kind": "keyboard", "transition": "tap", "inputs": list(names)}


def type_text(core, text):
    """Lower-case letters, digits, space, quotes and Return, a tap each"""
    names = {" ": "space", "\n": "return", '"': ("left_shift", "2")}
    ev = []
    for c in text:
        n = names.get(c, c)
        ev.append(tap(*n) if isinstance(n, tuple) else tap(n))
    for i in range(0, len(ev), 64):
        core.events(*ev[i:i + 64])
    time.sleep(len(ev) * 0.101 + 0.5)


def registers(core):
    core.ok("PUT", "machine:pause")
    regs = core.regs()
    check("vdcregs: 40 bytes", len(regs) == 40, len(regs))
    check("vdcregs: R1 80 columns, R6 25 rows", regs[1] == 80 and regs[6] == 25, regs[:8].hex())
    differ = []
    for r in range(38):
        if r in (16, 17, 31):                 # a program's read changes these: below
            continue
        p = core.prog_reg(r)
        if p != regs[r]:
            differ.append("R%d %02X/%02X" % (r, regs[r], p))
    check("vdcregs: R0-R37 as a program reads them", not differ, " ".join(differ))
    status = core.readmem(0xD600, 1)[0]
    check("vdcregs: the status as a program reads it", (status ^ core.regs()[38]) & 0xDF == 0,
          "%02X/%02X" % (core.regs()[38], status))
    core.writemem(0xD600, [26])
    check("vdcregs: the selected register", core.regs()[39] == 26, core.regs()[39])
    core.ok("PUT", "machine:resume")


def memory(core):
    core.ok("PUT", "machine:pause")
    before = core.regs()
    whole = core.mem(0, 65536)
    after = core.regs()
    check("vdcmem 64 KB: update address, data, selection kept",
          before[18:20] == after[18:20] and before[31] == after[31] and before[39] == after[39],
          "%s -> %s" % (before[18:20].hex(), after[18:20].hex()))
    s = Screen(core)
    spots = SPOTS + [s.screen + i for i in range(0, 2000, 97)] + [s.attr + i for i in range(0, 2000, 89)] \
        + [0x2000 + i for i in range(0, 0x2000, 251)]
    differ = []
    for a in spots:
        p = core.prog_mem(a & 0xFFFF)
        if p != whole[a & 0xFFFF]:
            differ.append("%04X %02X/%02X" % (a & 0xFFFF, whole[a & 0xFFFF], p))
    check("vdcmem: %d bytes as a program reads them" % len(spots), not differ, " ".join(differ[:8]))
    check("vdcmem: the same twice while paused", core.mem(0, 65536) == whole)
    # Put back what the program reads moved
    core.writemem(0xD600, [18, before[18], 19, before[19], before[39]])
    core.ok("PUT", "machine:resume")


def speed(core):
    t0 = time.monotonic()
    core.mem(0, 65536)
    t = time.monotonic() - t0
    check("vdcmem 64 KB in %.0f ms (%.2f us a byte)" % (t * 1000, t * 1e6 / 65536), t < 1.0)


def typed(core):
    type_text(core, "\n")
    core.events(tap("esc"), tap("x"))         # the screen editor moves to 80 columns
    time.sleep(0.5)
    type_text(core, 'print "vdc peek ok"\n')
    time.sleep(1)
    s = Screen(core)
    text = s.text()
    check("80 columns: the typed line", any('PRINT "VDC PEEK OK"' in t for t in text), "\n".join(text[:25]))
    check("80 columns: its output", any(t.startswith("VDC PEEK OK") for t in text))
    core.events(tap("esc"), tap("x"))


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("host")
    ap.add_argument("--password")
    a = ap.parse_args()
    core = Core(a.host, a.password)
    core.events({"kind": "release_all"})
    core.ok("PUT", "machine:reset")
    time.sleep(1)
    ready(core)
    time.sleep(1)
    registers(core)
    memory(core)
    speed(core)
    typed(core)
    core.ok("PUT", "machine:reset")
    print("%d failed" % fails)
    sys.exit(fails != 0)


if __name__ == "__main__":
    main()
