#!/usr/bin/env python3
"""Run z80diag.bin under VICE x128 via the remote monitor (no MEGA65/Vivado).

VICE 3.10's monitor does not reliably show Z80 data writes in `m` dumps, so the
smoke test checks the final register snapshot at halt_loop:
  A='Z', B=T2, C=T3, D=T4, E='D'
"""
from __future__ import annotations

import argparse
import re
import socket
import subprocess
import sys
import time
from pathlib import Path

STUB = bytes(
    [
        0xAD, 0x00, 0x12,
        0xC9, 0xA5,
        0xD0, 0xF9,
        0xA9, 0x5A,
        0x8D, 0x01, 0x12,
        0xAD, 0x05, 0xD5,
        0x29, 0xFE,
        0x8D, 0x05, 0xD5,
        0x4C, 0x80, 0x12,
    ]
)

HALT_FALLBACK = 0x3095


class ViceMonitor:
    def __init__(self, host: str = "127.0.0.1", port: int = 6510) -> None:
        self.s = socket.create_connection((host, port), timeout=5.0)
        self.s.settimeout(10.0)
        self.cmd("")

    def _read_until_prompt(self) -> str:
        data = ""
        while True:
            chunk = self.s.recv(4096).decode("utf-8", errors="replace")
            if not chunk:
                break
            data += chunk
            if re.search(r"\((?:C|Z80|z80):\$[0-9a-fA-F]+\)", data):
                self.s.settimeout(0.15)
                try:
                    while True:
                        more = self.s.recv(4096).decode("utf-8", errors="replace")
                        if not more:
                            break
                        data += more
                except socket.timeout:
                    pass
                self.s.settimeout(10.0)
                break
        return data

    def cmd(self, line: str) -> str:
        self.s.sendall((line + "\n").encode("utf-8"))
        return self._read_until_prompt()


def wait_port(port: int, timeout: float = 20.0) -> None:
    deadline = time.time() + timeout
    while time.time() < deadline:
        try:
            with socket.create_connection(("127.0.0.1", port), timeout=0.2):
                return
        except OSError:
            time.sleep(0.1)
    raise RuntimeError(f"VICE remote monitor not on port {port}")


def load_bin(mon: ViceMonitor, data: bytes, addr: int) -> None:
    i = 0
    while i < len(data):
        chunk = data[i : i + 12]
        hexbytes = " ".join(f"{b:02x}" for b in chunk)
        mon.cmd(f">{addr + i:x} {hexbytes}")
        i += len(chunk)


def read_lab_halt(lab: Path) -> int:
    if not lab.exists():
        return HALT_FALLBACK
    for line in lab.read_text().splitlines():
        if line.startswith("halt_loop:"):
            m = re.search(r"\$([0-9a-fA-F]+)", line)
            if m:
                return int(m.group(1), 16)
    return HALT_FALLBACK


def parse_z80_regs(text: str) -> dict[str, int]:
    """Parse VICE Z80 `r` dump. Line like: .;3000 b168 d505 ..."""
    regs: dict[str, int] = {}
    for line in text.splitlines():
        line = line.strip()
        if not line.startswith(".;"):
            continue
        # .;PC AF BC DE HL IX IY SP ...
        parts = line[2:].split()
        if len(parts) < 5:
            continue
        try:
            regs["pc"] = int(parts[0], 16)
            regs["af"] = int(parts[1], 16)
            regs["bc"] = int(parts[2], 16)
            regs["de"] = int(parts[3], 16)
            regs["hl"] = int(parts[4], 16)
            if len(parts) > 7:
                regs["sp"] = int(parts[7], 16)
        except ValueError:
            continue
        break
    return regs


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--bin", type=Path, required=True)
    ap.add_argument("--lab", type=Path, default=None)
    ap.add_argument("--port", type=int, default=6510)
    ap.add_argument("--x128", default="x128")
    ap.add_argument("--keep-open", action="store_true")
    ap.add_argument("--no-mailbox", action="store_true", help="Skip 8502 mailbox stub")
    args = ap.parse_args()

    data = args.bin.read_bytes()
    lab = args.lab or args.bin.with_suffix(".lab")
    halt = read_lab_halt(lab)

    proc = subprocess.Popen(
        [
            args.x128,
            "-default",
            "-warp",
            "+sound",
            "-remotemonitor",
            "-remotemonitoraddress",
            f"ip4://127.0.0.1:{args.port}",
        ],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )

    try:
        wait_port(args.port)
        time.sleep(3.0)
        mon = ViceMonitor(port=args.port)

        load_bin(mon, data, 0x3000)
        if not args.no_mailbox:
            mon.cmd("cpu 6502")
            mon.cmd(">1200 00 00")
            mon.cmd(">127f 01")
            load_bin(mon, STUB, 0x1280)
            mon.cmd("r pc=1280")

        mon.cmd(f"break {halt:x}")
        mon.cmd("cpu z80")
        mon.cmd("r pc=3000")
        mon.cmd("r sp=31ff")
        mon.cmd(">d505 00")

        mon.s.settimeout(30.0)
        out = mon.cmd("g")
        mon.s.settimeout(10.0)

        mon.cmd("cpu z80")
        rtext = mon.cmd("r")
        regs = parse_z80_regs(rtext)

        print("Breakpoint (tail):")
        print("\n".join(out.splitlines()[-6:]))
        print("Registers:")
        print(rtext.strip().splitlines()[0] if rtext.strip() else rtext)
        if ".;" in rtext:
            for line in rtext.splitlines():
                if line.strip().startswith(".;"):
                    print(line.strip())
                    break

        ok = True
        if "pc" not in regs:
            print("FAIL: could not parse Z80 registers")
            return 1

        a = (regs["af"] >> 8) & 0xFF
        b = (regs["bc"] >> 8) & 0xFF
        c = regs["bc"] & 0xFF
        d = (regs["de"] >> 8) & 0xFF
        e = regs["de"] & 0xFF

        print(f"SNAPSHOT A={a:02X} B={b:02X} C={c:02X} D={d:02X} E={e:02X} PC={regs['pc']:04X}")

        if a != ord("Z"):
            print("FAIL: A magic != 'Z' (diag did not finish)")
            ok = False
        else:
            print("PASS: A magic 'Z'")

        if e != ord("D"):
            print("FAIL: E end magic != 'D'")
            ok = False
        else:
            print("PASS: E end magic 'D'")

        print(f"INFO: T2 idle XOR in B={b:02X} (expect 00)")
        print(f"INFO: T3 key bits in C={c:02X} (b0=1 b1=Z b2=Ret)")
        print(f"INFO: T4 mailbox in D={d:02X} (00=ok 01=fail FF=no stub)")

        if b != 0:
            print("WARN: T2 non-zero — idle CIA matrix not all $FF")
        else:
            print("PASS: T2 idle matrix")

        if args.no_mailbox:
            if d != 0xFF:
                print(f"WARN: expected T4=$FF without stub, got {d:02X}")
        else:
            if d == 0x00:
                print("PASS: T4 mailbox round-trip")
            elif d == 0x01:
                print("FAIL: T4 mailbox stub did not ack")
                ok = False
            elif d == 0xFF:
                print("FAIL: T4 stub flag not seen")
                ok = False
            else:
                print(f"WARN: T4 unexpected {d:02X}")

        if abs(regs["pc"] - halt) > 2:
            print(f"WARN: PC {regs['pc']:04X} not at halt_loop {halt:04X}")

        if not args.keep_open:
            try:
                mon.cmd("quit")
            except OSError:
                pass
        return 0 if ok else 1
    finally:
        if not args.keep_open:
            proc.terminate()
            try:
                proc.wait(timeout=3)
            except subprocess.TimeoutExpired:
                proc.kill()


if __name__ == "__main__":
    sys.exit(main())
