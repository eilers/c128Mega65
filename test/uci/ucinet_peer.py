#!/usr/bin/env python3
"""The far end for test/uci's UCINET. Run it on a computer on the same network as the C128,
then type one of the addresses it shows into UCINET. Ctrl-C ends it.

TCP port 6464 takes lines that end in CR or LF:
  P n   n bytes of the pattern (byte i is i mod 251)
  E n   the next n bytes must be the pattern; the answer is "OK n" or "BAD k" (k: the first
        byte that differs), then CR. The peer waits 1.5 s before it reads them, and its
        receive buffer is small, so the C128's WRITEs have to wait for room.
  C     the peer closes the connection
UDP port 6464: "P n [seed]" gets a datagram of n bytes of the pattern from seed on, "D n k"
k datagrams of n bytes (seeds 0 to k-1); anything else comes back as it is.
TCP port 6465 must stay closed: UCINET checks that a connection there is refused.

M2M/firmware/host/host_net.c is the same peer for the host tests.
"""

import socket
import sys
import threading
import time

PORT = 6464
HOLD_SECONDS = 1.5
RCVBUF = 4096


def log(*args) -> None:
    print(time.strftime("%H:%M:%S"), *args, flush=True)


def pattern(n: int, seed: int = 0) -> bytes:
    return bytes((i + seed) % 251 for i in range(n))


def numbers(text: bytes, command: bytes) -> list[int] | None:
    """The numbers after a one-letter command, or None if it isn't that command."""
    parts = text.split()
    if not parts or parts[0].upper() != command:
        return None
    try:
        return [int(p) for p in parts[1:]]
    except ValueError:
        return None


def read_exactly(conn: socket.socket, n: int, pending: bytearray) -> bytes:
    while len(pending) < n:
        chunk = conn.recv(65536)
        if not chunk:
            break
        pending += chunk
    data = bytes(pending[:n])
    del pending[:n]
    return data


def serve_tcp(conn: socket.socket, who: str) -> None:
    log(f"tcp {who}: connected")
    pending = bytearray()
    try:
        while True:
            while b"\r" not in pending and b"\n" not in pending:
                chunk = conn.recv(65536)
                if not chunk:
                    log(f"tcp {who}: closed by the C128")
                    return
                pending += chunk
            cut = min(i for i in (pending.find(b"\r"), pending.find(b"\n")) if i >= 0)
            line = bytes(pending[:cut]).strip()
            del pending[:cut + 1]
            if not line:
                continue
            if (n := numbers(line, b"P")) is not None and len(n) == 1:
                conn.sendall(pattern(n[0]))
                log(f"tcp {who}: P {n[0]}: sent")
            elif (n := numbers(line, b"E")) is not None and len(n) == 1:
                time.sleep(HOLD_SECONDS)
                start = time.time()
                data = read_exactly(conn, n[0], pending)
                want = pattern(n[0])
                bad = next((i for i, (a, b) in enumerate(zip(data, want)) if a != b), None)
                if bad is None and len(data) < n[0]:
                    bad = len(data)
                answer = f"OK {len(data)}" if bad is None else f"BAD {bad}"
                conn.sendall(answer.encode() + b"\r")
                log(f"tcp {who}: E {n[0]}: {answer} ({time.time() - start:.2f} s after the hold)")
            elif line.upper() == b"C":
                log(f"tcp {who}: C: closing")
                conn.shutdown(socket.SHUT_WR)
                conn.close()
                return
            else:
                conn.sendall(b"?\r")
                log(f"tcp {who}: unknown {line!r}")
    except OSError as e:
        log(f"tcp {who}: {e}")
    finally:
        conn.close()


def tcp_server() -> None:
    srv = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    srv.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    srv.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, RCVBUF)   # the accepted ones inherit it
    srv.bind(("", PORT))
    srv.listen(8)
    while True:
        conn, addr = srv.accept()
        threading.Thread(target=serve_tcp, args=(conn, f"{addr[0]}:{addr[1]}"), daemon=True).start()


def udp_server() -> None:
    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    sock.bind(("", PORT))
    while True:
        data, addr = sock.recvfrom(65536)
        who = f"{addr[0]}:{addr[1]}"
        if (n := numbers(data, b"P")) is not None and len(n) in (1, 2):
            sock.sendto(pattern(n[0], n[1] if len(n) == 2 else 0), addr)
            log(f"udp {who}: P {' '.join(map(str, n))}: sent")
        elif (n := numbers(data, b"D")) is not None and len(n) == 2:
            for k in range(n[1]):
                sock.sendto(pattern(n[0], k), addr)
            log(f"udp {who}: D {n[0]} {n[1]}: sent")
        else:
            sock.sendto(data, addr)
            log(f"udp {who}: {len(data)} bytes echoed")


def addresses() -> list[str]:
    found = set()
    try:
        for info in socket.getaddrinfo(socket.gethostname(), None, socket.AF_INET):
            found.add(info[4][0])
    except OSError:
        pass
    probe = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    try:
        probe.connect(("192.0.2.1", 9))     # no packet goes out; it picks the outgoing address
        found.add(probe.getsockname()[0])
    except OSError:
        pass
    finally:
        probe.close()
    return sorted(a for a in found if not a.startswith("127."))


def main() -> int:
    threading.Thread(target=tcp_server, daemon=True).start()
    log(f"ucinet peer on TCP and UDP port {PORT}; this computer is {', '.join(addresses()) or '?'}")
    try:
        udp_server()
    except KeyboardInterrupt:
        return 0
    return 0


if __name__ == "__main__":
    sys.exit(main())
