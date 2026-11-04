#!/usr/bin/env python3
"""Compare a streamed VIC frame with a VICE screenshot of the same screen, pixel for pixel.

  compare_frame.py STREAM.png VICE.png [--ignore X,Y,W,H ...]

Both 384x272: stream_rx.py's frame.png, and x128 -VICIIborders 0 -exitscreenshotvicii. The
palettes differ, so it checks that both split the picture into the same colour regions: each
colour of one maps to exactly one colour of the other, both ways. A blinking cursor can be
left out with --ignore (its 8x8 cell). Prints the mismatches' count and bounding box.
"""
import argparse
import sys
from collections import Counter

from PIL import Image


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("stream")
    ap.add_argument("vice")
    ap.add_argument("--ignore", action="append", default=[], help="X,Y,W,H")
    a = ap.parse_args()

    s = Image.open(a.stream).convert("RGB")
    v = Image.open(a.vice).convert("RGB")
    if s.size != v.size:
        sys.exit(f"sizes differ: {s.size} and {v.size}")
    w, h = s.size
    skip = [tuple(int(n) for n in r.split(",")) for r in a.ignore]

    def kept(x, y):
        return not any(x0 <= x < x0 + rw and y0 <= y < y0 + rh for x0, y0, rw, rh in skip)

    sp, vp = s.load(), v.load()
    pairs = Counter((sp[x, y], vp[x, y]) for y in range(h) for x in range(w) if kept(x, y))
    # Each colour's partner is the one it meets most; any other pairing is a mismatch
    s2v, v2s = {}, {}
    for (sc, vc), n in pairs.most_common():
        if sc not in s2v and vc not in v2s:
            s2v[sc], v2s[vc] = vc, sc
    bad = [(x, y) for y in range(h) for x in range(w)
           if kept(x, y) and s2v.get(sp[x, y]) != vp[x, y]]
    print(f"{len(s2v)} colours matched: " + ", ".join(f"{k}->{v}" for k, v in s2v.items()))
    if bad:
        xs, ys = [p[0] for p in bad], [p[1] for p in bad]
        print(f"MISMATCH: {len(bad)} pixels in x {min(xs)}..{max(xs)}, y {min(ys)}..{max(ys)}")
        sys.exit(1)
    print("IDENTICAL: every pixel in the same place")


if __name__ == "__main__":
    main()
