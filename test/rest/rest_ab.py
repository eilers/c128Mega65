#!/usr/bin/env python3
"""The REST API, A/B: the same raw requests to the core and to an Ultimate, answers compared
byte for byte. Raw sockets, so every byte sent and received is the script's own.

  rest_ab.py HOST                 print each answer of HOST
  rest_ab.py HOST_A HOST_B        compare: A is ours, B the Ultimate
  --slow                          also the 15 s cases (idle reaper, a header too big)
  --contract ULTIMATE_REPO        check the first host's answers to the documented requests
                                  against Gideon's U2 OpenAPI document, with his
                                  tests/lib/openapi_contract.py (needs PyYAML and
                                  openapi-schema-validator)

A HOST is NAME[:PORT]; `M2M/firmware/host/build/test_http -serve 18080` puts the host
build of the server on 127.0.0.1:18080.
"""
import argparse
import json
import socket
import sys
import time

# (name, request bytes, how to compare): "exact", "info" (the same keys in the same order),
# or "ours" (only ours is checked: the Ultimate has a web UI there)
CASES = [
    ("version", b"GET /v1/version HTTP/1.1\r\nHost: x\r\n\r\n", "exact"),
    ("info", b"GET /v1/info HTTP/1.1\r\n\r\n", "info"),
    ("help without its argument", b"GET /v1/help HTTP/1.1\r\n\r\n", "exact"),
    ("help", b"GET /v1/help?command=readmem HTTP/1.1\r\n\r\n", "exact"),
    ("help, the argument decoded", b"GET /v1/help?comm%61nd HTTP/1.1\r\n\r\n", "exact"),
    ("help, a stranger first", b"GET /v1/help?x=1 HTTP/1.1\r\n\r\n", "exact"),
    ("an unknown argument", b"GET /v1/version?foo=1 HTTP/1.1\r\n\r\n", "exact"),
    ("two", b"GET /v1/version?a=1&b HTTP/1.1\r\n\r\n", "exact"),
    ("an empty query", b"GET /v1/version? HTTP/1.1\r\n\r\n", "exact"),
    ("&", b"GET /v1/version?& HTTP/1.1\r\n\r\n", "exact"),
    ("&& then one", b"GET /v1/version?&&y HTTP/1.1\r\n\r\n", "exact"),
    ("a trailing &", b"GET /v1/version?x=1& HTTP/1.1\r\n\r\n", "exact"),
    ("a nameless argument", b"GET /v1/version?=x HTTP/1.1\r\n\r\n", "exact"),
    ("a path", b"GET /v1/version/a/b%20c HTTP/1.1\r\n\r\n", "exact"),
    (":none", b"GET /v1/version:none HTTP/1.1\r\n\r\n", "exact"),
    ("no leading /", b"GET v1/version HTTP/1.1\r\n\r\n", "exact"),
    ("no HTTP version", b"GET /v1/version\r\n\r\n", "exact"),
    ("HEAD", b"HEAD /v1/version HTTP/1.1\r\n\r\n", "exact"),
    ("FOO", b"FOO /v1/version HTTP/1.1\r\n\r\n", "exact"),
    ("/v1", b"GET /v1 HTTP/1.1\r\n\r\n", "exact"),
    ("/v1/", b"GET /v1/ HTTP/1.1\r\n\r\n", "exact"),
    ("/v2", b"GET /v2/version HTTP/1.1\r\n\r\n", "exact"),
    ("//", b"GET /v1//version HTTP/1.1\r\n\r\n", "exact"),
    ("upper case", b"GET /v1/Version HTTP/1.1\r\n\r\n", "exact"),
    ("a command it hasn't", b"GET /v1/version:foo HTTP/1.1\r\n\r\n", "exact"),
    ("a method it hasn't", b"DELETE /v1/version HTTP/1.1\r\n\r\n", "exact"),
    ("POST with a body", b"POST /v1/version HTTP/1.1\r\nContent-Length: 5\r\n\r\nhello", "exact"),
    ("chunked", b"POST /v1/nothing HTTP/1.1\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nhello\r\n0\r\n\r\n",
     "exact"),
    ("no framing, some body", b"POST /v1/info HTTP/1.1\r\n\r\nxyz", "exact"),
    ("PUT: no body", b"PUT /v1/version HTTP/1.1\r\nContent-Length: 3\r\n\r\nabc", "exact"),
    ("GET: no body", b"GET /v1/version HTTP/1.1\r\nContent-Length: 3\r\n\r\nabc", "exact"),
    ("Content-Length 0", b"POST /v1/version HTTP/1.1\r\nContent-Length: 0\r\n\r\n", "exact"),
    ("40 lines", b"GET /v1/version HTTP/1.1\r\n" + b"A: b\r\n" * 39 + b"\r\n", "exact"),
    ("a 2048-byte header", None, "exact"),
    ("/", b"GET / HTTP/1.1\r\n\r\n", "ours"),
    ("index.html", b"GET /index.html HTTP/1.1\r\n\r\n", "ours"),
]

NOT_FOUND = b"HTTP/1.1 404 Not Found\r\nConnection: close\r\n\r\n"

# What his OpenAPI document describes; the other cases are odd requests it says nothing
# about (and for help without its argument it says HTML, where his code answers JSON)
DOCUMENTED = {"version", "info", "help", "help, the argument decoded"}


def header(n):
    """A request whose header, with its empty line, is n bytes"""
    start = b"GET /v1/version HTTP/1.1\r\nX-Pad: "
    return start + b"a" * (n - len(start) - 4) + b"\r\n\r\n"


def address(host):
    name, _, port = host.partition(":")
    return name, int(port or 80)


def ask(host, req, wait=20.0):
    """The answer, how the connection ended ("fin", "reset" or "open") and when, in s"""
    s = socket.create_connection(address(host), timeout=5)
    s.settimeout(wait)
    t0 = time.monotonic()
    s.sendall(req)
    got, end = b"", "open"
    try:
        while True:
            d = s.recv(65536)
            if not d:
                end = "fin"
                break
            got += d
    except ConnectionResetError:
        end = "reset"
    except socket.timeout:
        pass
    s.close()
    return got, end, time.monotonic() - t0


def split(answer):
    head, _, body = answer.partition(b"\r\n\r\n")
    lines = head.decode("latin-1").split("\r\n")
    status = int(lines[0].split()[1]) if lines and len(lines[0].split()) > 1 else 0
    headers = dict(line.split(": ", 1) for line in lines[1:] if ": " in line)
    return status, headers, body


def same_info(a, b):
    """The same keys in the same order (but wifi_mac: the MEGA65 has no Wi-Fi)"""
    try:
        ka = list(json.loads(split(a)[2]))
        kb = [k for k in json.loads(split(b)[2]) if k != "wifi_mac"]
    except ValueError:
        return False, "not JSON"
    return ka == kb, "keys %s vs %s" % (ka, kb)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("hosts", nargs="+")
    ap.add_argument("--slow", action="store_true")
    ap.add_argument("--contract")
    args = ap.parse_args()

    contract = None
    if args.contract:
        sys.path.insert(0, args.contract + "/tests/lib")
        import openapi_contract
        from report import Failure
        contract = openapi_contract.Contract.load("u2")

    cases = [(n, header(2048) if r is None else r, how) for n, r, how in CASES]
    failed = 0
    for name, req, how in cases:
        answers = [ask(h, req) for h in args.hosts]
        a = answers[0]
        note = ""
        if len(answers) == 1:
            ok = a[1] == "fin"
            print("%-28s %s, closed %s\n%s\n" % (name, ok and "ok" or "FAIL", a[1], a[0].decode("latin-1")))
        elif how == "ours":
            ok = a[0] == NOT_FOUND and a[1] == "fin"
            note = "(B has a web UI here)"
        elif how == "info":
            ok, note = same_info(a[0], answers[1][0])
            ok = ok and split(a[0])[0] == split(answers[1][0])[0] == 200
        else:
            ok = a[0] == answers[1][0]
        if len(answers) > 1:
            print("%-28s %s %s ends %s/%s" % (name, "same" if ok else "DIFFERENT", note, a[1], answers[1][1]))
            if not ok:
                print("  A: %r\n  B: %r" % (a[0], answers[1][0]))
        if contract and name in DOCUMENTED:
            first = req.split(b"\r\n", 1)[0].decode("latin-1").split(" ")
            method, path = first[0], first[1] if len(first) > 1 else ""
            path = "/" + path.lstrip("/")
            status, headers, body = split(a[0])
            if method in ("GET", "PUT", "POST", "DELETE") and status:
                try:
                    contract.check(method, path, status, headers, body)
                except Failure as e:
                    ok = False
                    print("  contract: %s" % e)
        failed += not ok

    if args.slow:
        # A header one byte too big: no answer, closed by the 15 s reaper; one of 4 KB: no
        # answer, closed at once
        for name, req, lo, hi in (("a 2049-byte header", header(2049), 14.5, 16.0),
                                  ("a 4 KB header", header(4000), 0.0, 1.0),
                                  ("silent", b"", 14.5, 16.0)):
            for h in args.hosts:
                got, end, t = ask(h, req, 25.0)
                ok = not got and end != "open" and lo <= t <= hi
                failed += not ok
                print("%-28s %s %s: %s after %.1f s" % (name, h, ok and "ok" or "FAIL", end, t))

    print("%d failed" % failed)
    return failed != 0


if __name__ == "__main__":
    sys.exit(main())
