#!/usr/bin/env python3
"""Read 157x work RAM through QNICE's JTAG UART monitor.

The script asks QNICE for its monitor by sending CTRL+E, selects
C_DEV_VDRIVE_DIAG window 1, reads the requested bytes and resumes the Shell,
all without touching the MEGA65 keyboard.

On cores built before the Shell learned that trigger, enter debug mode by
holding Run/Stop + Cursor Up and pressing Help while still holding them. The
prompt only ever appears on the JTAG UART, so the MEGA65 screen gives no
indication that it worked. If the Shell seems unresponsive, hold the reset
button for more than 1.5 seconds: a short press resets only the core, while a
long press restarts QNICE and makes it print its banner over this port.
"""

from __future__ import annotations

import argparse
import glob
import os
import re
import select
import sys
import termios
import time


PROMPT = b"QMON> "
RETURN_ADDRESS_RE = re.compile(rb"C R ([0-9A-Fa-f]{4}) to return")


def hex_address(value: str) -> int:
    try:
        result = int(value.removeprefix("$").removeprefix("0x"), 16)
    except ValueError as exc:
        raise argparse.ArgumentTypeError(f"invalid hexadecimal address: {value}") from exc
    if not 0 <= result <= 0x7FF:
        raise argparse.ArgumentTypeError("drive RAM address must be between 0000 and 07FF")
    return result


def default_port() -> str:
    ports = sorted(glob.glob("/dev/ttyUSB*"), key=lambda path: int(re.search(r"\d+$", path)[0]))
    if not ports:
        raise SystemExit("No /dev/ttyUSB* port found.")
    return ports[-1]


class SerialPort:
    def __init__(self, path: str) -> None:
        self.fd = os.open(path, os.O_RDWR | os.O_NOCTTY | os.O_NONBLOCK)
        attrs = termios.tcgetattr(self.fd)
        attrs[0] = 0
        attrs[1] = 0
        attrs[2] = termios.B115200 | termios.CS8 | termios.CREAD | termios.CLOCAL
        attrs[3] = 0
        attrs[4] = termios.B115200
        attrs[5] = termios.B115200
        attrs[6][termios.VMIN] = 0
        attrs[6][termios.VTIME] = 0
        termios.tcsetattr(self.fd, termios.TCSANOW, attrs)
        termios.tcflush(self.fd, termios.TCIOFLUSH)

    def close(self) -> None:
        os.close(self.fd)

    # QMON echoes every character in its polling loop and has no receive FIFO, so
    # a back-to-back burst at 115200 silently loses bytes. Type at keyboard speed.
    CHAR_INTERVAL = 0.01

    def write(self, data: bytes) -> None:
        for index in range(len(data)):
            _, writable, _ = select.select([], [self.fd], [], 1.0)
            if not writable:
                raise TimeoutError("timed out writing to JTAG UART")
            os.write(self.fd, data[index:index + 1])
            time.sleep(self.CHAR_INTERVAL)

    def drain(self, duration: float) -> bytes:
        deadline = time.monotonic() + duration
        data = bytearray()
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                return bytes(data)
            readable, _, _ = select.select([self.fd], [], [], remaining)
            if readable:
                data.extend(os.read(self.fd, 4096))

    def read_until(self, marker: bytes, timeout: float) -> bytes:
        deadline = time.monotonic() + timeout
        data = bytearray()
        while marker not in data:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                preview = bytes(data[-200:])
                raise TimeoutError(
                    f"timed out waiting for {marker!r}; received {len(data)} bytes: {preview!r}"
                )
            readable, _, _ = select.select([self.fd], [], [], min(remaining, 0.25))
            if readable:
                chunk = os.read(self.fd, 4096)
                if chunk:
                    data.extend(chunk)
        return bytes(data)


VERBOSE = False


def qmon_command(port: SerialPort, command: str, timeout: float = 5.0) -> bytes:
    port.write(command.encode("ascii"))
    response = port.read_until(PROMPT, timeout)
    if VERBOSE:
        print(f"  >> {command}\n  << {response!r}", file=sys.stderr, flush=True)
    return response


CTRL_E = b"\x05"


def enter_monitor(port: SerialPort, timeout: float = 600.0) -> bytes:
    """Get QNICE into its monitor and return everything it printed.

    The Shell's main loop treats CTRL+E on this port as a request to enter the
    monitor, so no keyboard interaction is needed. It is re-sent periodically
    because the Shell can be busy in a blocking prompt when the first one
    arrives. Cores built before that trigger existed ignore it entirely, which
    is why the key combination is still offered as a fallback.
    """
    deadline = time.monotonic() + timeout
    data = bytearray()
    announced = False
    while PROMPT not in data:
        if time.monotonic() > deadline:
            raise TimeoutError(f"QNICE did not enter the monitor within {timeout:.0f} s")
        port.write(CTRL_E)
        data.extend(port.drain(3.0))
        if not announced and PROMPT not in data:
            print("Still waiting. On an older core, hold Run/Stop + Cursor Up and press Help.",
                  flush=True)
            announced = True
    return bytes(data)


def resync(port: SerialPort) -> None:
    """Return QMON to its prompt from an unknown input state.

    A previous run may have died while QMON was still collecting the hex operand
    of a command. Zeros complete any such operand (the worst case is examining
    address 0000) and any leftover digit is rejected as an unknown command, so
    either way the monitor ends up back at the prompt.
    """
    port.drain(0.3)
    port.write(b"0000")
    port.drain(0.5)
    qmon_command(port, "ME0000")


def change_word(port: SerialPort, address: int, value: int) -> None:
    qmon_command(port, f"MC{address:04X}{value:04X}")


def examine_word(port: SerialPort, address: int) -> int:
    response = qmon_command(port, f"ME{address:04X}")
    match = re.search(rb"ADDRESS=[0-9A-Fa-f]{4}\s+([0-9A-Fa-f]{4})", response)
    if not match:
        raise RuntimeError(f"could not parse QMON response: {response!r}")
    return int(match.group(1), 16)


def qnice_address(drive: int, drive_address: int) -> int:
    return 0x7000 + (drive - 8) * 0x800 + drive_address


# QMON prints with CR after LF ("\n\r7000: ..."), so a start-of-line match
# would miss every row if it required the address in column 0.
DUMP_ROW_RE = re.compile(rb"([0-9A-Fa-f]{4}):((?: [0-9A-Fa-f]{4})+)")


def examine_range(port: SerialPort, first: int, last: int) -> list[int]:
    """Read a contiguous word range with QMON's block dump.

    One MD covers the whole range, which is far quicker than an ME per word: a
    full 2 KiB drive RAM takes seconds instead of minutes.
    """
    response = qmon_command(port, f"MD{first:04X}{last:04X}", timeout=120.0)
    values: dict[int, int] = {}
    for row in DUMP_ROW_RE.finditer(response):
        base = int(row.group(1), 16)
        for index, word in enumerate(row.group(2).split()):
            values[base + index] = int(word, 16)

    missing = [address for address in range(first, last + 1) if address not in values]
    if missing:
        preview = response[-400:]
        raise RuntimeError(
            f"QMON dump did not report {len(missing)} of the requested words; "
            f"parsed {len(values)} words from {len(response)} bytes: {preview!r}"
        )
    return [values[address] for address in range(first, last + 1)]


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--port", help="JTAG UART device; defaults to highest /dev/ttyUSB*")
    parser.add_argument("--no-resume", action="store_true", help="leave QNICE at the monitor prompt")
    parser.add_argument("--verbose", action="store_true", help="echo the raw QMON dialogue")
    parser.add_argument("--attached", metavar="RETADDR",
                        help="QMON is already at its prompt; RETADDR is the hex 'C R' return address")
    subparsers = parser.add_subparsers(dest="operation", required=True)

    peek = subparsers.add_parser("peek", help="read one or more drive RAM bytes")
    peek.add_argument("drive", type=int, choices=(8, 9))
    peek.add_argument("addresses", nargs="+", type=hex_address)

    dump = subparsers.add_parser("dump", help="dump an inclusive drive RAM range")
    dump.add_argument("drive", type=int, choices=(8, 9))
    dump.add_argument("start", type=hex_address)
    dump.add_argument("end", type=hex_address)

    cmd = subparsers.add_parser("cmd", help="send raw QMON commands and print the replies")
    cmd.add_argument("commands", nargs="+")

    raw = subparsers.add_parser("raw", help="read words from any 4k window of the diag device")
    raw.add_argument("window", type=lambda v: int(v, 16))
    raw.add_argument("offset", type=lambda v: int(v, 16))
    raw.add_argument("count", type=int)

    watch = subparsers.add_parser(
        "watch",
        help="sample the host request words of window 0 and report every change")
    watch.add_argument("drive", type=int, choices=(8, 9))
    watch.add_argument("seconds", type=float, nargs="?", default=30.0)

    args = parser.parse_args()
    global VERBOSE
    VERBOSE = args.verbose
    if args.operation == "dump" and args.end < args.start:
        parser.error("dump end address must not precede start address")

    path = args.port or default_port()
    print(f"Listening on {path} at 115200 baud.", flush=True)
    if not args.attached:
        print("Requesting the QNICE monitor with CTRL+E.", flush=True)

    port = SerialPort(path)
    saved_csr: int | None = None
    try:
        if args.attached:
            # The banner was already consumed by another listener, so the return
            # address has to come from the caller instead of being parsed here.
            return_address = args.attached
            resync(port)
        else:
            banner = enter_monitor(port)
            match = RETURN_ADDRESS_RE.search(banner)
            return_address = match.group(1).decode("ascii") if match else None

        if args.operation == "cmd":
            for command in args.commands:
                print(qmon_command(port, command).decode("ascii", "replace"))
            if not args.no_resume and return_address:
                port.write(f"CR{return_address}".encode("ascii"))
            return 0

        if args.operation == "watch":
            # Deliberately leaves the core running: the point is to catch a request
            # that only exists while the DOS is working. Word 4/5 hold the last
            # linear LBA the drive asked the host for and keep it until the next
            # request, so sampling a few times a second is enough to see a track
            # fetch that lasts milliseconds of host time.
            change_word(port, 0xFFF4, 0x0106)
            change_word(port, 0xFFF5, 0x0000)
            base = 0x7000 + (args.drive - 8) * 0x80
            deadline = time.monotonic() + args.seconds
            previous = None
            print(f"Watching drive {args.drive} for {args.seconds:.0f} s. "
                  f"Run the command on the MEGA65 now.", flush=True)
            while time.monotonic() < deadline:
                counts = examine_word(port, base + 3)
                lba = examine_word(port, base + 4) | (examine_word(port, base + 5) << 16)
                blocks = examine_word(port, base + 6)
                sample = (counts, lba, blocks)
                if sample != previous:
                    print(f"  +{args.seconds - (deadline - time.monotonic()):6.2f}s "
                          f"reads={counts >> 8:3d} writes={counts & 0xFF:3d} "
                          f"lba={lba:5d} blocks={blocks >> 8:2d}", flush=True)
                    previous = sample
            if not args.no_resume and return_address:
                port.write(f"CR{return_address}".encode("ascii"))
                print("QNICE Shell resumed.")
            return 0

        # Freeze the core (including both drive CPUs) for a coherent snapshot. QNICE
        # itself keeps running, so its monitor and RAMROM bridge remain accessible.
        saved_csr = examine_word(port, 0xFFE0)
        change_word(port, 0xFFE0, saved_csr | 0x0002)

        # RAMROM device 0x0106 is VDRIVE_DIAG; 4 KiB window 1 is drive work RAM.
        change_word(port, 0xFFF4, 0x0106)
        change_word(port, 0xFFF5, args.window if args.operation == "raw" else 0x0001)

        if args.operation == "raw":
            for index in range(args.count):
                offset = args.offset + index
                value = examine_word(port, 0x7000 + offset)
                print(f"window {args.window:04X} +{offset:03X} = {value:04X}")
            change_word(port, 0xFFE0, saved_csr)
            saved_csr = None
            if not args.no_resume and return_address:
                port.write(f"CR{return_address}".encode("ascii"))
            return 0

        if args.operation == "peek":
            for address in args.addresses:
                value = examine_word(port, qnice_address(args.drive, address)) & 0xFF
                print(f"drive {args.drive} ${address:04X} = ${value:02X} ({value})")
        else:
            words = examine_range(port,
                                  qnice_address(args.drive, args.start),
                                  qnice_address(args.drive, args.end))
            for row in range(0, len(words), 16):
                chunk = words[row:row + 16]
                print(f"{args.start + row:04X}: " + " ".join(f"{word & 0xFF:02X}" for word in chunk))

        change_word(port, 0xFFE0, saved_csr)
        saved_csr = None

        if not args.no_resume:
            if not return_address:
                print("Could not find the Shell return address; leaving QMON active.", file=sys.stderr)
            else:
                # C/R jumps out of QMON, so there is deliberately no following QMON prompt.
                port.write(f"CR{return_address}".encode("ascii"))
                print("QNICE Shell resumed.")
    finally:
        if saved_csr is not None:
            try:
                change_word(port, 0xFFE0, saved_csr)
            except (OSError, TimeoutError):
                print("warning: could not restore the core pause state", file=sys.stderr)
        port.close()

    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (OSError, TimeoutError, RuntimeError) as exc:
        raise SystemExit(f"error: {exc}") from exc
