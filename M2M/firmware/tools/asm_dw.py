#!/usr/bin/env python3
"""Turns the .DW tables of QNICE assembly files (the ascal filters in M2M/video_filters)
into a C header: one uint16_t array per label.

Usage: asm_dw.py out.h file.asm..."""
import re
import sys

out = ["// Made by tools/asm_dw.py from " + ", ".join(sys.argv[2:]) + "; do not edit", "#pragma once",
       "#include <stdint.h>", ""]
for path in sys.argv[2:]:
    label, words = None, []
    for line in open(path):
        line = line.split(";")[0].strip()
        m = re.match(r"^([A-Za-z_][A-Za-z0-9_]*)$", line)
        if m:
            label = m.group(1)
        elif line.upper().startswith(".DW"):
            words += [w.strip() for w in line[3:].split(",") if w.strip()]
    if not label or not words:
        sys.exit(f"{path}: no label or no .DW data")
    out.append(f"static const uint16_t {label}[{len(words)}] = {{")
    for i in range(0, len(words), 8):
        out.append("   " + ", ".join(words[i:i + 8]) + ",")
    out.append("};")
    out.append("")
open(sys.argv[1], "w").write("\n".join(out))
