#!/usr/bin/env python3
"""Receive the core's video and audio streams (his U64 Data Streams) and check them.

  stream_rx.py HOST [--password PW] [--seconds N] [--out DIR] [--no-start]

Starts both streams to this machine over REST (PUT /v1/streams/...:start?ip=ME:PORT),
listens on 11000 (video) and 11001 (audio) for N seconds, then stops them. Reports per
stream the packets, sequence gaps and malformed packets; for video the whole frames and
their rate (PAL 50.12/s), for audio the sample rate (his 47983 Hz). Writes the last whole
frame as frame.png (VIC colours) and the audio as audio.wav into DIR.

Use a wired machine for loss figures: Wi-Fi drops bursts of a 25 Mbit/s stream.
"""
import argparse
import select
import socket
import struct
import sys
import time
import urllib.request
import wave
import zlib
from pathlib import Path

VIDEO_PORT, AUDIO_PORT = 11000, 11001
WIDTH, LINES_PER_PACKET, LINE_BYTES = 384, 4, 192
PAL_LINES, NTSC_LINES = 272, 240

# The VIC's colours (Pepto's PAL palette)
PALETTE = [
    (0x00, 0x00, 0x00), (0xFF, 0xFF, 0xFF), (0x68, 0x37, 0x2B), (0x70, 0xA4, 0xB2),
    (0x6F, 0x3D, 0x86), (0x58, 0x8D, 0x43), (0x35, 0x28, 0x79), (0xB8, 0xC7, 0x6F),
    (0x6F, 0x4F, 0x25), (0x43, 0x39, 0x00), (0x9A, 0x67, 0x59), (0x44, 0x44, 0x44),
    (0x6C, 0x6C, 0x6C), (0x9A, 0xD2, 0x84), (0x6C, 0x5E, 0xB5), (0x95, 0x95, 0x95),
]


def rest(host, path, password):
    req = urllib.request.Request(f"http://{host}{path}", method="PUT", data=b"")
    if password:
        req.add_header("X-Password", password)
    with urllib.request.urlopen(req, timeout=10) as r:
        return r.status, r.read().decode(errors="replace")


def my_address(host):
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    s.connect((host, 80))
    ip = s.getsockname()[0]
    s.close()
    return ip


def listen(port, rcvbuf):
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    if rcvbuf:
        s.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, rcvbuf)
    s.bind(("0.0.0.0", port))
    s.setblocking(False)
    return s


def write_png(path, frame, height):
    rows = bytearray()
    for y in range(height):
        rows.append(0)                          # filter: none
        for x in range(WIDTH // 2):
            b = frame[y * LINE_BYTES + x]
            rows.append(b & 0x0F)               # left pixel: the low nibble
            rows.append(b >> 4)
    # 8-bit palette image
    def chunk(kind, data):
        c = kind + data
        return struct.pack(">I", len(data)) + c + struct.pack(">I", zlib.crc32(c))
    png = b"\x89PNG\r\n\x1a\n"
    png += chunk(b"IHDR", struct.pack(">IIBBBBB", WIDTH, height, 8, 3, 0, 0, 0))
    png += chunk(b"PLTE", bytes(c for rgb in PALETTE for c in rgb))
    png += chunk(b"IDAT", zlib.compress(bytes(rows), 9))
    png += chunk(b"IEND", b"")
    Path(path).write_bytes(png)


class Seq:
    """A 16-bit sequence: forward gaps are loss, a step back is reordering."""

    def __init__(self):
        self.last = None
        self.lost = 0
        self.reordered = 0

    def see(self, seq):
        if self.last is not None:
            gap = (seq - self.last) & 0xFFFF
            if gap == 0 or gap >= 0x8000:
                self.reordered += 1
                return
            self.lost += gap - 1
        self.last = seq


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("host")
    ap.add_argument("--password", default="")
    ap.add_argument("--seconds", type=float, default=10.0)
    ap.add_argument("--out", default=".")
    ap.add_argument("--no-start", action="store_true", help="the streams are already running")
    ap.add_argument("--rcvbuf", type=int, default=8 << 20,
                    help="socket receive buffer in bytes, 0 for the system's default (as most viewers)")
    ap.add_argument("--watch", action="store_true",
                    help="a still screen: log each whole frame that differs from the first by more "
                         "than an 8x8 cell (a blinking cursor), and save the first ten")
    a = ap.parse_args()

    vs, aus = listen(VIDEO_PORT, a.rcvbuf), listen(AUDIO_PORT, a.rcvbuf)
    me = my_address(a.host)
    core = socket.gethostbyname(a.host)
    if not a.no_start:
        for name, port in (("video", VIDEO_PORT), ("audio", AUDIO_PORT)):
            st, body = rest(a.host, f"/v1/streams/{name}:start?ip={me}:{port}", a.password)
            print(f"{name}:start -> {st} {body.strip()}")

    out = Path(a.out)
    out.mkdir(parents=True, exist_ok=True)
    ref = {}
    glitches = []
    t_start = time.monotonic()

    def watch(frame, frm, now):
        buf, height = frame
        if "buf" not in ref:
            ref["buf"] = buf
            return
        diff = [i for i in range(height * LINE_BYTES) if buf[i] != ref["buf"][i]]
        if not diff:
            return
        ys = [i // LINE_BYTES for i in diff]
        xs = [2 * (i % LINE_BYTES) for i in diff]
        box = (min(xs), min(ys), max(xs) + 1, max(ys))
        if box[2] - box[0] < 8 and box[3] - box[1] < 8:
            return                               # the cursor
        glitches.append(box)
        print(f"{now - t_start:7.2f} s frame {frm}: {len(diff)} bytes differ in x {box[0]}..{box[2]}, "
              f"y {box[1]}..{box[3]}", flush=True)
        if len(glitches) <= 10:
            write_png(out / f"glitch{len(glitches):02d}-f{frm}.png", buf, height)

    vseq, aseq = Seq(), Seq()
    vpackets = apackets = vbad = abad = 0
    frames = {}                                  # frame number -> (buffer, lines seen)
    whole = []                                   # (time, frame number, height)
    last_frame = None
    audio = bytearray()
    a_first = a_last = None
    a_count = 0
    v_last = None
    gaps = []                                    # video arrival pauses over 20 ms
    warm = time.monotonic() + 1.0                # rates from here: the socket held earlier packets
    end = time.monotonic() + a.seconds
    try:
        while time.monotonic() < end:
            ready, _, _ = select.select([vs, aus], [], [], 0.2)
            now = time.monotonic()
            for s in ready:
                while True:
                    try:
                        data, src = s.recvfrom(2048)
                    except BlockingIOError:
                        break
                    if src[0] != core:
                        continue
                    if s is vs:
                        if len(data) != 780:
                            vbad += 1
                            continue
                        seq, frm, line, width, lpp, bpp, enc = struct.unpack_from("<HHHHBBH", data)
                        last = bool(line & 0x8000)
                        line &= 0x7FFF
                        if width != WIDTH or lpp != LINES_PER_PACKET or bpp != 4 or enc != 0 \
                                or line + LINES_PER_PACKET > PAL_LINES:
                            vbad += 1
                            continue
                        vpackets += 1
                        vseq.see(seq)
                        if v_last is not None and now - v_last > 0.02 and now > warm:
                            gaps.append(now - v_last)
                        v_last = now
                        buf, seen = frames.setdefault(frm, (bytearray(PAL_LINES * LINE_BYTES), set()))
                        buf[line * LINE_BYTES:(line + 4) * LINE_BYTES] = data[12:]
                        seen.add(line)
                        if last:
                            height = line + LINES_PER_PACKET
                            if all(l in seen for l in range(0, height, LINES_PER_PACKET)):
                                whole.append((now, frm, height))
                                last_frame = (bytes(buf), height)
                                if a.watch:
                                    watch(last_frame, frm, now)
                            frames.pop(frm, None)
                            for k in [k for k in frames if (frm - k) & 0xFFFF < 0x8000 and k != frm]:
                                frames.pop(k)    # older frames that can't finish now
                    else:
                        if len(data) != 770:
                            abad += 1
                            continue
                        apackets += 1
                        aseq.see(struct.unpack_from("<H", data)[0])
                        audio += data[2:]
                        a_count += 1
                        a_first = a_first or now
                        a_last = now
    finally:
        if not a.no_start:
            for name in ("video", "audio"):
                try:
                    rest(a.host, f"/v1/streams/{name}:stop", a.password)
                except OSError as e:
                    print(f"{name}:stop failed: {e}")

    ok = True
    if a.watch:
        print(f"watch: {len(glitches)} frames differed from the first beyond a cursor cell")
    print(f"video: {vpackets} packets, {vseq.lost} lost, {vseq.reordered} reordered, {vbad} malformed")
    steady = [w for w in whole if w[0] > warm]
    if len(steady) >= 2:
        rate = ((steady[-1][1] - steady[0][1]) & 0xFFFF) / (steady[-1][0] - steady[0][0])
        heights = sorted({h for _, _, h in whole})
        print(f"video: {len(whole)} whole frames, {rate:.2f} frames/s, height {heights}")
        print(f"video: {len(gaps)} arrival pauses over 20 ms, {sum(g > 0.1 for g in gaps)} over 100 ms, "
              f"longest {max(gaps, default=0) * 1000:.0f} ms (viewers that time a frame out at 100 ms "
              f"split frames there)")
    else:
        print("video: fewer than 2 whole frames")
        ok = False
    if last_frame:
        write_png(out / "frame.png", *last_frame)
        print(f"video: last whole frame in {out / 'frame.png'}")
    if a_count >= 2:
        rate = (a_count - 1) * 192 / (a_last - a_first)
        print(f"audio: {apackets} packets, {aseq.lost} lost, {aseq.reordered} reordered, {abad} malformed, "
              f"{rate:.0f} samples/s")
        with wave.open(str(out / "audio.wav"), "wb") as w:
            w.setnchannels(2)
            w.setsampwidth(2)
            w.setframerate(47983)
            w.writeframes(bytes(audio))
        print(f"audio: {len(audio) // 4} frames in {out / 'audio.wav'}")
    else:
        print("audio: fewer than 2 packets")
        ok = False
    ok = ok and vbad == 0 and abad == 0
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()
