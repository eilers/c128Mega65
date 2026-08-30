#!/usr/bin/env python3
"""Decode the VDRIVE_DIAG lines the QNICE shell prints on the JTAG UART.

Words 0-7 are the host-side handshake, 8-15 the DOS snapshot, 16-63 the
serial-bus trace and 64-111 the DOS bit counter ($98) paired with the last
value the DOS actually read out of the VIA1 interrupt flag register ($180D),
which is what it branches on when it chooses between "take the next bit" and
"acknowledge EOI".

Usage:
  CORE/scripts/decode_vdrive_trace.py jtag.log            # last distinct trace
  CORE/scripts/decode_vdrive_trace.py jtag.log --all      # every distinct trace
"""

import argparse
import re
import sys

LINE_RE = re.compile(r"VDRIVE_DIAG drive=([0-9A-Fa-f]+)((?:\s+[0-9A-Fa-f]{4})+)")

ATN_ACC_BITS = [
    "drive pulled DATA (ATN ack)",
    "drive pulled CLK",
    "DATA low on merged bus",
    "CLK low on merged bus",
    "VIA1 interrupt",
    "VIA2 interrupt",
    "CIA interrupt",
    "CPU saw IRQ",
    "DOS wrote VIA1 ORB",
    "DOS read VIA1 ORB",
    "CPU fetched IRQ vector",
    "fast serial direction set",
    "CPU ran at 2 MHz",
    "CPU halted by clock switch",
    "window outlasted tick counter",
    "a window was observed",
]


def parse(path):
    """Yield the 16-bit word list of every VDRIVE_DIAG line in the log."""
    with open(path, "r", errors="replace") as handle:
        for line in handle:
            match = LINE_RE.search(line)
            if match:
                yield [int(w, 16) for w in match.group(2).split()]


def format_entry(index, entry):
    state, age = entry >> 8, entry & 0xFF
    bits = {
        "ATN": (state >> 7) & 1,
        "CLKi": (state >> 6) & 1,
        "DATAi": (state >> 5) & 1,
        "DATAo": (state >> 4) & 1,
        "CLKo": (state >> 3) & 1,
        "ATNA": (state >> 2) & 1,
        "PB1": (state >> 1) & 1,
        "PB3": state & 1,
    }
    flags = " ".join(f"{name}={value}" for name, value in bits.items())
    gap = "  +---" if age == 0xFF else f"  +{age:3d}"
    return f"  {index:2d} {gap} us   {flags}"


IFR_BITS = ["CA2", "CA1/ATN", "SR", "CB2", "CB1", "T2", "T1", "IRQ"]


def format_ifr(value):
    names = [name for bit, name in enumerate(IFR_BITS) if value >> bit & 1]
    return f"${value:02X}" + (f" ({','.join(names)})" if names else "")


def format_dos(entry):
    bit_count, ifr = entry >> 8, entry & 0xFF
    return f"$98=${bit_count:02X} last_IFR={format_ifr(ifr)}"


def show(words):
    print(f"host: signature={words[0]:04X} reads/writes={words[3]:04X} "
          f"lba={(words[5] << 16) | words[4]} blkcnt/ack={words[6]:04X}")

    atn_acc = words[9]
    active = [name for bit, name in enumerate(ATN_ACC_BITS) if atn_acc >> bit & 1]
    print(f"dos:  signature={words[8]:04X} atn_acc={atn_acc:04X} "
          f"orb_writes={words[13] >> 8:02X} atn_edges={words[13] & 0xFF:02X} "
          f"jobs={words[12] & 0xFF:02X} state/job0={words[11]:04X}")
    print("      " + ", ".join(active))

    print("trace (entry 0 is the ATN falling edge):")
    for index, entry in enumerate(words[16:64]):
        print(format_entry(index, entry))
        print("                 " + format_dos(words[64 + index]))
        if index and entry == 0:
            break

    orb_pins = words[116]
    print(f"live CPU bus address=${words[112]:04X} "
          f"{format_dos(words[115])}")
    print(f"last $1800 read=${orb_pins >> 8:02X} raw port B pins=${orb_pins & 0xFF:02X}")

    # ph2_r[0] is meant to be 1 MHz and the core clock is 31.5 MHz, so a healthy drive
    # reports 32768/31.5 = 1040 ticks per window. A different number means the drive's
    # whole sense of time, including the trace timestamps above, is scaled.
    rate = words[117] & 0x0FFF
    verdict = "ok" if 1020 <= rate <= 1060 else f"UNEXPECTED, implies {rate / 1040:.2f} MHz"
    print(f"drive ticks per 32768 core clocks={rate} (expect ~1040: {verdict})")
    print(f"T1 high-byte writes={words[118] & 0xFF} "
          f"last value=${words[118] >> 8:02X}; "
          f"EOI entries={words[119] >> 8} byte acknowledgements={words[119] & 0xFF}")

    frate = words[122] & 0x0FFF
    # atn_acc bit 12 records that the 1571 DOS selected its native 2 MHz mode.
    # ena_f follows that selector, so its healthy count doubles while ph2_r[0]
    # (and therefore the trace timestamp reference above) remains at 1 MHz.
    expected_frate = 2080 if atn_acc & (1 << 12) else 1040
    tolerance = 40 if expected_frate == 2080 else 20
    fverdict = ("ok" if abs(frate - expected_frate) <= tolerance
                else "UNEXPECTED, drive CPU/timer enable has the wrong rate")
    print(f"timer enable ticks per 32768 core clocks={frate} "
          f"(expect ~{expected_frate}: {fverdict})")

    # Writing $1805 must clear the T1 flag, so the first $180D read after arming should
    # come back with bit 6 low. Seeing it set a few microseconds later means the write
    # did not clear it; only an age near the 256 us load is a genuine timeout.
    age, first = words[120] & 0xFF, words[120] >> 8
    if first & 0x40:
        note = "flag ALREADY SET" if age < 32 else "genuine timeout"
        print(f"first $180D after arming T1 = {format_ifr(first)} "
              f"after {age} us -> {note}")
    else:
        print(f"first $180D after arming T1 = {format_ifr(first)} after {age} us "
              f"-> write cleared the flag")
    acr = words[121] >> 8
    print(f"last ACR written=${acr:02X} "
          f"(T1 {'free-run' if acr & 0x40 else 'one-shot'}"
          f"{', PB7 output' if acr & 0x80 else ''}) "
          f"last IER written=${words[121] & 0xFF:02X} "
          f"$180D reads={words[123] >> 8}")
    # Everything below is counted into a ~66 ms window and then latched, so it describes
    # the drive right now rather than since reset. 255 means the counter saturated.
    gcr = words[127]

    # Word order note: the last five words are, in transmission order,
    # 123=$180F pin/CPU low counts, 124=busy/track-change, 125=raw/gated bytes,
    # 126=VIA2 ORA reads + live track, 127=sync + live pin states.
    pa7_pin, pa7_cpu = words[123] >> 8, words[123] & 0xFF
    pa_rds, trk_chg = (words[124] >> 8) * 64, words[124] & 0xFF
    raw, gated = words[125] >> 8, words[125] & 0xFF
    ora_win, track = words[126] >> 8, words[126] & 0x7F
    sync = words[127] >> 8
    busy, wgate = (gcr >> 5) & 1, (gcr >> 4) & 1

    print(f"per 66ms window: GCR bytes raw={raw} gated={gated} sync={sync} "
          f"VIA2 ORA reads={ora_win}")
    print(f"per 66ms window: port A reads={pa_rds}+ head moves={trk_chg} "
          f"(live half-track={track} busy={busy})")
    print(f"per 66ms window: port A reads seeing byte-ready low: "
          f"pin={pa7_pin} CPU={pa7_cpu}")

    if trk_chg:
        print("  -> the head is still moving, which holds the GCR bit clock in "
              "reset: the seek never converges")
    elif not pa_rds:
        print("  -> the DOS is not polling port A at all")
    elif not raw:
        print("  -> the GCR engine is not producing bytes at all right now")
    elif not gated:
        print("  -> bytes are produced but SOE is gating them away")
    elif not pa7_pin:
        print("  -> byte-ready pulses exist but never coincide with a DOS read: "
              "the pulse is too short for the polling loop to catch")
    elif not pa7_cpu:
        print("  -> the pin goes low but the VIA never returns it: port A input path")
    else:
        print("  -> the DOS does see byte-ready; the hang is downstream of PA7")

    print(f"live raw_byte_n={(gcr >> 3) & 1} byte_n={(gcr >> 2) & 1} "
          f"SOE={(gcr >> 1) & 1} motor={gcr & 1} wgate={wgate}")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("log")
    parser.add_argument("--all", action="store_true",
                        help="print every distinct trace, not just the last")
    args = parser.parse_args()

    seen, distinct = set(), []
    for words in parse(args.log):
        if len(words) < 128:
            continue
        key = tuple(words[8:112])
        if key not in seen:
            seen.add(key)
            distinct.append(words)

    if not distinct:
        print("No 128-word VDRIVE_DIAG lines found. Is this a PC-trace bitstream?",
              file=sys.stderr)
        return 1

    for words in distinct if args.all else distinct[-1:]:
        show(words)
        print()
    return 0


if __name__ == "__main__":
    sys.exit(main())
