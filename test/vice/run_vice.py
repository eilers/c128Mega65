#!/usr/bin/env python3
"""Run VICE testbench exitcode programs on the C128 core over the Ultimate REST API.

The core is a machine on the network (doc/rest.md). A program that loads at $0801 is
started in C64 mode (the firmware types GO64). Any other load address stays in C128
mode. The program leaves its result in the debug register at $D7FF, which
machine:debugreg reads in both modes.

  run_vice.py --mode go64 --test testbench/selftest/c64-pass.prg
  run_vice.py --mode c128 --test testbench/selftest/c128-pass.prg
  run_vice.py --all-exitcode

Results go to test/vice-results/<CORE_VERSION>.json. CORE_VERSION is read from
CORE/vhdl/config.vhd. The programs are test/vice-exitcode, the plain exitcode
rows from https://github.com/libsidplayfp/VICE-testprogs (commit 21b802f).
Rows that mount a disk, a cartridge, or a RAM Expansion Unit are skipped.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
VICE_ROOT = ROOT / "test" / "vice-exitcode"
CONFIG_VHD = ROOT / "CORE" / "vhdl" / "config.vhd"
RESULTS_DIR = ROOT / "test" / "vice-results"

LISTS = {
    "go64": VICE_ROOT / "testbench" / "x128c64-testlist.txt",
    "c128": VICE_ROOT / "testbench" / "x128-testlist.txt",
}
COLLECTIONS = {
    "go64": "87BE2595-0FC7-421F-B0B0-E2893B16C458",
    "c128": "21143D5B-7142-4503-92BE-8879C5F6840B",
}

# A value the test is not expected to leave. Polling stops when the register changes.
SENTINEL = 0x2A
PAL_CYCLES_PER_SEC = 985248.0
MEDIA_TOKENS = (
    "mountcrt",
    "mountd64",
    "mountd71",
    "mountd81",
    "mountg64",
    "mountreu",
    "reu:",
)

if hasattr(sys.stdout, "reconfigure"):
    sys.stdout.reconfigure(line_buffering=True)


def core_version() -> str:
    text = CONFIG_VHD.read_text()
    match = re.search(r'constant\s+CORE_VERSION\s*:\s*string\s*:=\s*"([^"]*)"', text)
    if not match:
        raise SystemExit(f"CORE_VERSION not found in {CONFIG_VHD}")
    return match.group(1)


def results_path(version: str) -> Path:
    return RESULTS_DIR / f"{version}.json"


def craft_column(version: str) -> str:
    return version.lower()


class Core:
    def __init__(self, host: str, password: str):
        self.base = f"http://{host}/v1/"
        self.password = password

    def call(self, method: str, path: str, body: bytes | None = None,
             ctype: str | None = None, timeout: float = 20) -> tuple[int, bytes]:
        req = urllib.request.Request(self.base + path, data=body, method=method)
        if self.password:
            req.add_header("X-Password", self.password)
        if ctype:
            req.add_header("Content-Type", ctype)
        try:
            with urllib.request.urlopen(req, timeout=timeout) as response:
                return response.status, response.read()
        except urllib.error.HTTPError as error:
            return error.code, error.read()

    def info(self) -> tuple[int, bytes]:
        return self.call("GET", "info", timeout=10)

    def debugreg(self) -> int:
        status, body = self.call("GET", "machine:debugreg", timeout=10)
        if status != 200:
            raise RuntimeError(f"GET debugreg {status} {body[:200]!r}")
        return int(json.loads(body)["value"], 16)

    def set_debugreg(self, value: int) -> int:
        status, body = self.call("PUT", f"machine:debugreg?value={value:02X}", timeout=10)
        if status != 200:
            raise RuntimeError(f"PUT debugreg {status} {body[:200]!r}")
        return int(json.loads(body)["value"], 16)

    def reset(self) -> None:
        status, body = self.call("PUT", "machine:reset", timeout=20)
        if status != 200:
            raise RuntimeError(f"PUT machine:reset {status} {body[:200]!r}")

    def run_prg(self, prg: bytes) -> tuple[int, bytes]:
        return self.call(
            "POST",
            "runners:run_prg",
            prg,
            "application/octet-stream",
            timeout=60,
        )


def resolve_testlist_prg(directory: str, name: str) -> tuple[Path, str]:
    if directory.startswith("./"):
        rel = f"testbench/{directory[2:]}{name}"
    elif directory.startswith("../"):
        rel = f"{directory[3:]}{name}"
    else:
        rel = f"{directory}{name}"
    rel = rel.replace("//", "/")
    return VICE_ROOT / rel, rel


def load_exitcode_tests(mode: str) -> list[dict]:
    """Plain .prg exitcode rows. Disk, cartridge, and REU rows are left out."""
    path = LISTS[mode]
    if not path.exists():
        raise SystemExit(f"missing {path}")
    tests: list[dict] = []
    seen: set[str] = set()
    for raw in path.read_text().splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        parts = line.split(",")
        if len(parts) < 3 or parts[2] != "exitcode":
            continue
        directory, name = parts[0], parts[1]
        extra = ",".join(parts[3:]).lower()
        if any(token in extra for token in MEDIA_TOKENS) or not name.endswith(".prg"):
            continue
        prg, rel = resolve_testlist_prg(directory, name)
        if rel in seen:
            continue
        seen.add(rel)
        cycles = 0
        if len(parts) >= 4:
            try:
                cycles = int(parts[3])
            except ValueError:
                cycles = 0
        tests.append(
            {
                "prg": prg,
                "test": rel,
                "id": None,
                "collection": COLLECTIONS[mode],
                "mode": mode,
                "expect_error": "expect:error" in extra,
                "cycles": cycles,
            }
        )
    return tests


def timeout_from_cycles(cycles: int) -> float:
    if cycles <= 0:
        return 15.0
    return max(3.0, min(90.0, cycles / PAL_CYCLES_PER_SEC + 2.0))


def score(exitcode: int | None, expect_error: bool) -> str:
    if exitcode is None:
        return "timeout"
    failed = exitcode != 0
    if expect_error:
        return "ok" if failed else "fail"
    return "ok" if exitcode == 0 else "fail"


def find_spec(mode: str, test: str) -> dict:
    rel = test.replace("\\", "/").lstrip("/")
    for spec in load_exitcode_tests(mode):
        if spec["test"] == rel:
            return spec
    prg = VICE_ROOT / rel
    return {
        "prg": prg,
        "test": rel,
        "id": None,
        "collection": COLLECTIONS[mode],
        "mode": mode,
        "expect_error": False,
        "cycles": 0,
    }


def run_one(core: Core, spec: dict) -> dict:
    prg_path: Path = spec["prg"]
    if not prg_path.is_file():
        raise RuntimeError(f"missing {prg_path}")
    prg = prg_path.read_bytes()
    if len(prg) < 3:
        raise RuntimeError(f"{prg_path} is not a Commodore program")
    load = prg[0] | (prg[1] << 8)
    limit = timeout_from_cycles(spec["cycles"])
    print(
        f"mode={spec['mode']} {spec['test']} load=${load:04X} "
        f"bytes={len(prg) - 2} timeout={limit:.1f}s"
        f"{' expect:error' if spec['expect_error'] else ''}"
    )
    if spec["mode"] == "go64" and load != 0x0801:
        print("warning: run_prg enters C64 mode only for a program at $0801")
    elif spec["mode"] == "c128" and load == 0x0801:
        print("warning: run_prg will type GO64 for a program at $0801")

    seen = core.set_debugreg(SENTINEL)
    if seen != SENTINEL:
        raise RuntimeError(f"debug register read back ${seen:02X}, wanted ${SENTINEL:02X}")

    status, body = core.run_prg(prg)
    if status == 423:
        print(f"run_prg 423 {body[:180]!r}; reset and try once more")
        core.reset()
        time.sleep(2)
        core.set_debugreg(SENTINEL)
        status, body = core.run_prg(prg)
    if status != 200:
        print(f"run_prg failed {status} {body[:300]!r}")
        return {"result": "timeout", "exitcode": None, "hardware_go64": spec["mode"] == "go64"}

    deadline = time.monotonic() + limit
    exitcode: int | None = None
    next_note = time.monotonic() + 2
    while time.monotonic() < deadline:
        value = core.debugreg()
        if value != SENTINEL:
            exitcode = value
            break
        if time.monotonic() >= next_note:
            print(f"  still ${value:02X}")
            next_note = time.monotonic() + 2
        time.sleep(0.1)

    result = score(exitcode, spec["expect_error"])
    shown = "none" if exitcode is None else f"${exitcode:02X}"
    print(f"result={result} exitcode={shown}")
    return {
        "result": result,
        "exitcode": exitcode,
        "hardware_go64": spec["mode"] == "go64",
    }


def record_of(spec: dict, outcome: dict) -> dict:
    return {
        "id": spec.get("id"),
        "collection": spec["collection"],
        "mode": spec["mode"],
        "test": spec["test"],
        "check": "exitcode",
        "result": outcome["result"],
        "exitcode": outcome["exitcode"],
        "hardware_go64": outcome["hardware_go64"],
    }


def write_json(path: Path, version: str, record: dict) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    tests: list[dict] = []
    if path.exists():
        try:
            existing = json.loads(path.read_text())
            tests = list(existing.get("tests") or [])
        except json.JSONDecodeError:
            tests = []

    def same(item: dict) -> bool:
        return item.get("test") == record.get("test") and item.get("collection") == record.get("collection")

    tests = [item for item in tests if not same(item)]
    tests.append(record)
    payload = {
        "core_version": version,
        "craft_column": craft_column(version),
        "tests": tests,
    }
    path.write_text(json.dumps(payload, indent=2) + "\n")
    print(f"wrote {path} ({len(tests)} tests)")


def already_scored(path: Path) -> set[tuple[str, str]]:
    """Tests that already left an exit code. A timeout is run again."""
    if not path.exists():
        return set()
    try:
        tests = json.loads(path.read_text()).get("tests") or []
    except json.JSONDecodeError:
        return set()
    done = set()
    for item in tests:
        if item.get("exitcode") is None:
            continue
        name = item.get("test")
        collection = item.get("collection")
        if name and collection:
            done.add((name, collection))
    return done


def drives_off(core: Core) -> None:
    """A powered drive with no disk keeps C128 BASIC from READY, so run_prg answers 423."""
    for name in ("a", "b"):
        status, body = core.call("PUT", f"drives/{name}:off", timeout=20)
        print(f"drive {name} off: {status} {body[:80]!r}")


def probe(core: Core) -> None:
    try:
        status, body = core.info()
    except urllib.error.URLError as error:
        raise SystemExit(f"cannot reach {core.base}: {error}") from error
    if status != 200:
        raise SystemExit(f"GET /v1/info returned {status}: {body[:300]!r}")
    try:
        info = json.loads(body)
    except json.JSONDecodeError:
        info = {}
    product = info.get("product", "?")
    firmware = info.get("firmware_version", info.get("version", "?"))
    print(f"core {product} firmware {firmware}")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Run VICE exitcode tests over the Ultimate REST API")
    parser.add_argument("--host", default="192.168.178.57", help="core address (default 192.168.178.57)")
    parser.add_argument("--password", default="", help="X-Password, if the core has one set")
    parser.add_argument("--mode", choices=sorted(LISTS), help="go64 (C64 mode) or c128")
    parser.add_argument("--test", help="program path as stored in the JSON, e.g. testbench/selftest/c64-pass.prg")
    parser.add_argument("--all-exitcode", action="store_true", help="every plain exitcode program in the list")
    parser.add_argument("--resume", action="store_true", help="skip a program that already has an exit code in the results file")
    args = parser.parse_args()
    if args.all_exitcode and args.test:
        parser.error("--test and --all-exitcode are separate")
    if not args.all_exitcode and not args.test:
        parser.error("give --test or --all-exitcode")
    if args.test and not args.mode:
        parser.error("--test needs --mode")
    return args


def main() -> int:
    args = parse_args()
    version = core_version()
    out = results_path(version)
    core = Core(args.host, args.password)
    probe(core)
    if args.all_exitcode:
        drives_off(core)

    if args.all_exitcode:
        modes = [args.mode] if args.mode else ["go64", "c128"]
        specs = []
        for mode in modes:
            specs.extend(load_exitcode_tests(mode))
    else:
        specs = [find_spec(args.mode, args.test)]

    if args.resume:
        done = already_scored(out)
        before = len(specs)
        specs = [spec for spec in specs if (spec["test"], spec["collection"]) not in done]
        print(f"resume: {before - len(specs)} already scored, {len(specs)} left")

    print(f"CORE_VERSION {version} -> {out.name} ({len(specs)} program(s))")
    failed = 0
    for spec in specs:
        if not spec["prg"].is_file():
            print(f"missing {spec['prg']}")
            failed += 1
            continue
        try:
            outcome = run_one(core, spec)
        except (RuntimeError, urllib.error.URLError, TimeoutError, json.JSONDecodeError) as error:
            print(f"error: {error}")
            outcome = {
                "result": "timeout",
                "exitcode": None,
                "hardware_go64": spec["mode"] == "go64",
            }
        write_json(out, version, record_of(spec, outcome))
        if outcome["result"] != "ok":
            failed += 1
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
