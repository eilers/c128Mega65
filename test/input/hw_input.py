#!/usr/bin/env python3
"""machine:input on the core, checked by what the C128 itself sees (INPUTTEST's table, read
with readmem): every key name in both scan directions, the locking keys, RESTORE, both
joysticks with fire2/fire3, the mouse, the debug register; then typing at BASIC's READY in
40 and 80 columns, a long line at full speed, and the menu driven by keys.

  hw_input.py HOST [--password PW] [--only NAME,...]

HOST is the core's address. It runs inputtest.prg (runners:run_prg) and leaves the C128
reset at the end.
"""
import argparse
import json
import sys
import time
import urllib.request
from pathlib import Path

HERE = Path(__file__).resolve().parent
TABLE = 0x1300
SIZE = 0x1F

# His names and ours, by (row, bit): rows 0-7 the C64 matrix (PA), 8-10 K0-K2
KEYS = [
    "inst_del", "return", "cursor_left_right", "f7", "f1", "f3", "f5", "cursor_up_down",
    "3", "w", "a", "4", "z", "s", "e", "left_shift",
    "5", "r", "d", "6", "c", "f", "t", "x",
    "7", "y", "g", "8", "b", "h", "u", "v",
    "9", "i", "j", "0", "m", "k", "o", "n",
    "plus", "p", "l", "minus", "period", "colon", "at", "comma",
    "pound", "star", "semicolon", "clr_home", "right_shift", "equals", "arrow_up", "slash",
    "1", "arrow_left", "ctrl", "2", "space", "commodore", "q", "run_stop",
    "help", "kp_8", "kp_5", "tab", "kp_2", "kp_4", "kp_7", "kp_1",
    "esc", "kp_plus", "kp_minus", "line_feed", "kp_enter", "kp_6", "kp_9", "kp_3",
    "alt", "kp_0", "kp_period", "up", "down", "left", "right", "no_scroll",
]

fails = 0


class Core:
    def __init__(self, host, password):
        self.base = "http://%s/v1/" % host
        self.password = password

    def call(self, method, path, body=None, ctype=None):
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

    def events(self, *events):
        status, body = self.call("POST", "machine:input", json.dumps({"events": list(events)}).encode(),
                                 "application/json")
        if status != 200:
            raise RuntimeError("input: %d %s" % (status, body))
        return json.loads(body)

    def readmem(self, addr, n):
        status, body = self.call("GET", "machine:readmem?address=%04X&length=%d" % (addr, n))
        assert status == 200, body
        return body

    def writemem(self, addr, data):
        status, body = self.call("PUT", "machine:writemem?address=%04X&data=%s" % (addr, data.hex()))
        assert status == 200, body

    def table(self):
        return self.readmem(TABLE, SIZE)


def key(transition, *names):
    return {"kind": "keyboard", "transition": transition, "inputs": list(names)}


def joy(port, transition, *names):
    return {"kind": "joystick", "port": port, "transition": transition, "inputs": list(names)}


def check(name, ok, got=""):
    global fails
    print("%-50s %s %s" % (name, "ok" if ok else "FAIL", "" if ok else got))
    fails += not ok


def settle(core):
    """Two of INPUTTEST's rounds after now"""
    r0 = core.table()[0x1E]
    t0 = time.monotonic()
    while time.monotonic() - t0 < 2:
        if (core.table()[0x1E] - r0) & 0xFF >= 2:
            return
    raise RuntimeError("INPUTTEST doesn't run")


def start(core):
    prg = (HERE / "inputtest.prg").read_bytes()
    status, body = core.call("POST", "runners:run_prg", prg, "application/octet-stream")
    assert status == 200, body
    t0 = time.monotonic()
    while time.monotonic() - t0 < 15:
        if core.readmem(0x0400, 9) == bytes([9, 14, 16, 21, 20, 20, 5, 19, 20]):
            settle(core)
            return
        time.sleep(0.2)
    raise RuntimeError("INPUTTEST didn't start")


def keys(core):
    for i, name in enumerate(KEYS):
        row, bit = divmod(i, 8)
        core.events(key("press", name))
        settle(core)
        t = core.table()
        want = bytearray(19)
        want[row] = 1 << bit
        if row < 8:
            want[11 + bit] = 1 << row          # the other way: PB bit low, PA row read
        check("key %s" % name, t[:19] == bytes(want), t[:19].hex())
        core.events(key("release", name))
    settle(core)
    check("keys: all released", core.table()[:19] == bytes(19), core.table()[:19].hex())
    # A chord, as RUN/STOP + C= or shifted keys need
    core.events(key("press", "left_shift", "commodore", "a"))
    settle(core)
    t = core.table()
    check("chord left_shift + commodore + a", t[1] == 0x84 and t[7] == 0x20, t[:8].hex())
    core.events({"kind": "release_all"})


def locking(core):
    base = core.table()
    core.events(key("press", "40_80"))
    settle(core)
    check("40_80 engaged: $D505 bit 7 low", not core.table()[0x19] & 0x80, "%02X" % core.table()[0x19])
    core.events(key("release", "40_80"))
    settle(core)
    check("40_80 let go", core.table()[0x19] & 0x80 == base[0x19] & 0x80, "%02X" % core.table()[0x19])
    core.events(key("press", "caps_lock"))
    settle(core)
    check("caps_lock engaged: $01 bit 6 low", not core.table()[0x1A] & 0x40, "%02X" % core.table()[0x1A])
    core.events(key("release", "caps_lock"))
    settle(core)
    check("caps_lock let go", core.table()[0x1A] & 0x40 == base[0x1A] & 0x40, "%02X" % core.table()[0x1A])


def restore(core):
    n0 = core.table()[0x1B]
    core.events(key("tap", "restore"))
    time.sleep(0.2)
    n1 = core.table()[0x1B]
    check("restore tap: one NMI", (n1 - n0) & 0xFF == 1, "%d -> %d" % (n0, n1))
    core.events(key("tap", "restore"), key("tap", "restore"), key("tap", "restore"))
    time.sleep(0.4)
    n2 = core.table()[0x1B]
    check("three restore taps: three NMIs", (n2 - n1) & 0xFF == 3, "%d -> %d" % (n1, n2))


def joysticks(core):
    for port, at in ((1, 0x13), (2, 0x14)):
        for bit, name in enumerate(("up", "down", "left", "right", "fire")):
            core.events(joy(port, "press", name))
            settle(core)
            t = core.table()
            check("port %d %s" % (port, name), t[at] == 1 << bit, "%02X" % t[at])
            core.events(joy(port, "release", name))
        # fire2 and fire3: the POT lines, $00 pressed (his JOYSTICK_POT_PRESSED)
        core.events(joy(port, "press", "fire2", "fire3"))
        settle(core)
        t = core.table()
        p = 0x15 + 2 * (port - 1)
        check("port %d fire2+fire3: POTX, POTY 00" % port, t[p] < 0x08 and t[p + 1] < 0x08, t[p:p + 2].hex())
        core.events(joy(port, "release", "fire2"))
        settle(core)
        t = core.table()
        check("port %d fire3 alone: POTX 80, POTY 00" % port, 0x78 <= t[p] <= 0x88 and t[p + 1] < 0x08,
              t[p:p + 2].hex())
        core.events(joy(port, "release", "fire3"))
    settle(core)
    t = core.table()
    check("joysticks let go", t[0x13] == 0 and t[0x14] == 0, t[0x13:0x15].hex())
    # Taps: one 20 ms tick; INPUTTEST sees it if a round falls inside
    core.events(joy(2, "tap", "fire"))
    settle(core)
    check("port 2 tap: over", core.table()[0x14] == 0)


def mouse(core):
    core.events({"kind": "mouse", "move": {"x": 0, "y": 0}})
    time.sleep(0.1)
    x0, y0 = core.table()[0x15:0x17]
    core.events({"kind": "mouse", "move": {"x": 10, "y": -5}})
    time.sleep(0.1)
    x1, y1 = core.table()[0x15:0x17]
    # 1351: two POT counts a step, 7 bits; y up the screen is a smaller value
    dx, dy = (x1 - x0) & 0x7F, (y1 - y0) & 0x7F
    check("mouse move 10, -5: POTX +20, POTY +10", dx == 20 and dy == 10, "%02X %02X -> %02X %02X" % (x0, y0, x1, y1))
    core.events({"kind": "mouse", "inputs": ["left"], "transition": "press"})
    time.sleep(0.1)
    check("mouse left = fire", core.table()[0x13] == 0x10, "%02X" % core.table()[0x13])
    core.events({"kind": "mouse", "inputs": ["left"], "transition": "release"},
                {"kind": "mouse", "inputs": ["right"], "transition": "press"})
    time.sleep(0.1)
    check("mouse right = up", core.table()[0x13] == 0x01, "%02X" % core.table()[0x13])
    s = core.events({"kind": "release_all"})
    check("release_all: detached", s["mouse"]["attached"] is False)
    time.sleep(0.1)
    check("port 1 let go", core.table()[0x13] == 0)


def debugreg(core):
    status, body = core.call("PUT", "machine:debugreg?value=5A")
    check("PUT debugreg 5A", status == 200 and json.loads(body)["value"] == "5A", body)
    settle(core)
    check("the C128 reads 5A at $D7FF", core.table()[0x1C] == 0x5A, "%02X" % core.table()[0x1C])
    core.writemem(TABLE + 0x1D, b"\xa5")
    settle(core)
    status, body = core.call("GET", "machine:debugreg")
    check("the C128 wrote A5: GET debugreg", json.loads(body)["value"] == "A5", body)
    core.writemem(TABLE + 0x1D, b"\x00")


def ready(core):
    t0 = time.monotonic()
    while time.monotonic() - t0 < 15:
        if core.readmem(0x0A00, 1)[0] == 3:
            return
        time.sleep(0.2)
    raise RuntimeError("no READY")


def type_line(core, text):
    """Lower-case letters, digits, space, + and Return, a tap each"""
    names = {" ": "space", "+": "plus", "\n": "return"}
    ev = [key("tap", names.get(c, c)) for c in text]
    for i in range(0, len(ev), 64):
        core.events(*ev[i:i + 64])


def basic(core):
    core.call("PUT", "machine:reset")
    time.sleep(1)
    ready(core)
    time.sleep(1)
    type_line(core, "print 1+1\n")
    time.sleep(1.5)
    screen = core.readmem(0x0400, 1000)
    rows = [screen[i:i + 40] for i in range(0, 1000, 40)]
    ok = any(r[:2] == bytes([0x20, 0x32]) for r in rows)   # " 2"
    check("BASIC 40 columns: PRINT 1+1 gives 2", ok)
    # 100 characters at full speed: a REM line, then its length from the screen
    line = "rem " + "abcdefghij" * 9 + "abcdef"
    t0 = time.monotonic()
    type_line(core, line)
    time.sleep(len(line) * 0.101 + 0.5)
    screen = core.readmem(0x0400, 1000)
    text = bytes(screen)
    want = bytes((ord(c) - 0x60) if c.isalpha() else ord(c) for c in line)
    check("100 keys typed, none lost (%.1f s)" % (time.monotonic() - t0), want[:40] in text and want[-20:] in text)
    type_line(core, "\n")


def menu(core):
    status, _ = core.call("PUT", "machine:menu_button")
    time.sleep(0.5)
    status, before = core.call("GET", "machine:menu_screen")
    check("menu open", status == 200)
    core.events(key("tap", "cursor_up_down"))
    time.sleep(0.3)
    status, after = core.call("GET", "machine:menu_screen")
    check("menu: cursor_up_down moves the cursor", status == 200 and after != before)
    core.events(key("tap", "left_shift", "cursor_up_down"))
    time.sleep(0.3)
    status, back = core.call("GET", "machine:menu_screen")
    check("menu: shifted, back up", back == before)
    core.events(key("tap", "help"))
    time.sleep(0.6)
    status, _ = core.call("GET", "machine:menu_screen")
    check("menu: help closes it", status == 404)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("host")
    ap.add_argument("--password")
    ap.add_argument("--only")
    args = ap.parse_args()
    core = Core(args.host, args.password)
    parts = [("keys", keys), ("locking", locking), ("restore", restore), ("joysticks", joysticks),
             ("mouse", mouse), ("debugreg", debugreg)]
    only = set(args.only.split(",")) if args.only else None
    core.events({"kind": "release_all"})
    if any(not only or n in only for n, _ in parts):
        start(core)
        for name, fn in parts:
            if not only or name in only:
                fn(core)
    if not only or "basic" in only:
        basic(core)
    if not only or "menu" in only:
        menu(core)
    core.events({"kind": "release_all"})
    print("%d failed" % fails)
    sys.exit(fails != 0)


if __name__ == "__main__":
    main()
