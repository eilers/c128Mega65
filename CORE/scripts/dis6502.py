#!/usr/bin/env python3
"""Disassemble a window of one bank of sdcard/c128/boot1.rom.

The drive ROM bundle holds six 32 KB banks (1541, 1571 and 1581, two drives
each) and each bank is mapped at $8000 in the drive's address space. When a
simulation or a JTAG trace reports that the drive CPU is spinning at some
address, this turns that address back into DOS code.

    CORE/scripts/dis6502.py 2 D300 D360
"""

import sys
from pathlib import Path

BANK_SIZE = 32768
BANK_BASE = 0x8000

# addressing modes: operand length and formatting
IMP, ACC, IMM, ZP, ZPX, ZPY, ABS, ABX, ABY, IND, IZX, IZY, REL = range(13)
SIZE = {IMP: 0, ACC: 0, IMM: 1, ZP: 1, ZPX: 1, ZPY: 1, ABS: 2,
        ABX: 2, ABY: 2, IND: 2, IZX: 1, IZY: 1, REL: 1}

OPS = {}


def _add(code, name, mode):
    OPS[code] = (name, mode)


for code, name, mode in [
    (0x00, "BRK", IMP), (0x01, "ORA", IZX), (0x05, "ORA", ZP), (0x06, "ASL", ZP),
    (0x08, "PHP", IMP), (0x09, "ORA", IMM), (0x0A, "ASL", ACC), (0x0D, "ORA", ABS),
    (0x0E, "ASL", ABS), (0x10, "BPL", REL), (0x11, "ORA", IZY), (0x15, "ORA", ZPX),
    (0x16, "ASL", ZPX), (0x18, "CLC", IMP), (0x19, "ORA", ABY), (0x1D, "ORA", ABX),
    (0x1E, "ASL", ABX), (0x20, "JSR", ABS), (0x21, "AND", IZX), (0x24, "BIT", ZP),
    (0x25, "AND", ZP), (0x26, "ROL", ZP), (0x28, "PLP", IMP), (0x29, "AND", IMM),
    (0x2A, "ROL", ACC), (0x2C, "BIT", ABS), (0x2D, "AND", ABS), (0x2E, "ROL", ABS),
    (0x30, "BMI", REL), (0x31, "AND", IZY), (0x35, "AND", ZPX), (0x36, "ROL", ZPX),
    (0x38, "SEC", IMP), (0x39, "AND", ABY), (0x3D, "AND", ABX), (0x3E, "ROL", ABX),
    (0x40, "RTI", IMP), (0x41, "EOR", IZX), (0x45, "EOR", ZP), (0x46, "LSR", ZP),
    (0x48, "PHA", IMP), (0x49, "EOR", IMM), (0x4A, "LSR", ACC), (0x4C, "JMP", ABS),
    (0x4D, "EOR", ABS), (0x4E, "LSR", ABS), (0x50, "BVC", REL), (0x51, "EOR", IZY),
    (0x55, "EOR", ZPX), (0x56, "LSR", ZPX), (0x58, "CLI", IMP), (0x59, "EOR", ABY),
    (0x5D, "EOR", ABX), (0x5E, "LSR", ABX), (0x60, "RTS", IMP), (0x61, "ADC", IZX),
    (0x65, "ADC", ZP), (0x66, "ROR", ZP), (0x68, "PLA", IMP), (0x69, "ADC", IMM),
    (0x6A, "ROR", ACC), (0x6C, "JMP", IND), (0x6D, "ADC", ABS), (0x6E, "ROR", ABS),
    (0x70, "BVS", REL), (0x71, "ADC", IZY), (0x75, "ADC", ZPX), (0x76, "ROR", ZPX),
    (0x78, "SEI", IMP), (0x79, "ADC", ABY), (0x7D, "ADC", ABX), (0x7E, "ROR", ABX),
    (0x81, "STA", IZX), (0x84, "STY", ZP), (0x85, "STA", ZP), (0x86, "STX", ZP),
    (0x88, "DEY", IMP), (0x8A, "TXA", IMP), (0x8C, "STY", ABS), (0x8D, "STA", ABS),
    (0x8E, "STX", ABS), (0x90, "BCC", REL), (0x91, "STA", IZY), (0x94, "STY", ZPX),
    (0x95, "STA", ZPX), (0x96, "STX", ZPY), (0x98, "TYA", IMP), (0x99, "STA", ABY),
    (0x9A, "TXS", IMP), (0x9D, "STA", ABX), (0xA0, "LDY", IMM), (0xA1, "LDA", IZX),
    (0xA2, "LDX", IMM), (0xA4, "LDY", ZP), (0xA5, "LDA", ZP), (0xA6, "LDX", ZP),
    (0xA8, "TAY", IMP), (0xA9, "LDA", IMM), (0xAA, "TAX", IMP), (0xAC, "LDY", ABS),
    (0xAD, "LDA", ABS), (0xAE, "LDX", ABS), (0xB0, "BCS", REL), (0xB1, "LDA", IZY),
    (0xB4, "LDY", ZPX), (0xB5, "LDA", ZPX), (0xB6, "LDX", ZPY), (0xB8, "CLV", IMP),
    (0xB9, "LDA", ABY), (0xBA, "TSX", IMP), (0xBC, "LDY", ABX), (0xBD, "LDA", ABX),
    (0xBE, "LDX", ABY), (0xC0, "CPY", IMM), (0xC1, "CMP", IZX), (0xC4, "CPY", ZP),
    (0xC5, "CMP", ZP), (0xC6, "DEC", ZP), (0xC8, "INY", IMP), (0xC9, "CMP", IMM),
    (0xCA, "DEX", IMP), (0xCC, "CPY", ABS), (0xCD, "CMP", ABS), (0xCE, "DEC", ABS),
    (0xD0, "BNE", REL), (0xD1, "CMP", IZY), (0xD5, "CMP", ZPX), (0xD6, "DEC", ZPX),
    (0xD8, "CLD", IMP), (0xD9, "CMP", ABY), (0xDD, "CMP", ABX), (0xDE, "DEC", ABX),
    (0xE0, "CPX", IMM), (0xE1, "SBC", IZX), (0xE4, "CPX", ZP), (0xE5, "SBC", ZP),
    (0xE6, "INC", ZP), (0xE8, "INX", IMP), (0xE9, "SBC", IMM), (0xEA, "NOP", IMP),
    (0xEC, "CPX", ABS), (0xED, "SBC", ABS), (0xEE, "INC", ABS), (0xF0, "BEQ", REL),
    (0xF1, "SBC", IZY), (0xF5, "SBC", ZPX), (0xF6, "SBC", ZPX), (0xF8, "SED", IMP),
    (0xF9, "SBC", ABY), (0xFD, "SBC", ABX), (0xFE, "INC", ABX),
]:
    _add(code, name, mode)


def operand(mode, lo, hi, pc):
    if mode in (IMP,):
        return ""
    if mode == ACC:
        return "A"
    if mode == IMM:
        return f"#${lo:02X}"
    if mode == ZP:
        return f"${lo:02X}"
    if mode == ZPX:
        return f"${lo:02X},X"
    if mode == ZPY:
        return f"${lo:02X},Y"
    if mode == ABS:
        return f"${hi:02X}{lo:02X}"
    if mode == ABX:
        return f"${hi:02X}{lo:02X},X"
    if mode == ABY:
        return f"${hi:02X}{lo:02X},Y"
    if mode == IND:
        return f"(${hi:02X}{lo:02X})"
    if mode == IZX:
        return f"(${lo:02X},X)"
    if mode == IZY:
        return f"(${lo:02X}),Y"
    if mode == REL:
        return f"${(pc + 2 + (lo - 256 if lo > 127 else lo)) & 0xFFFF:04X}"
    return ""


def main():
    if len(sys.argv) < 4:
        print(__doc__)
        return 1
    bank = int(sys.argv[1])
    start = int(sys.argv[2], 16)
    end = int(sys.argv[3], 16)

    rom = Path(__file__).resolve().parents[2] / "sdcard" / "c128" / "boot1.rom"
    data = rom.read_bytes()
    base = bank * BANK_SIZE

    pc = start
    while pc < end:
        off = base + (pc - BANK_BASE)
        op = data[off]
        name, mode = OPS.get(op, ("???", IMP))
        n = SIZE[mode]
        lo = data[off + 1] if n >= 1 else 0
        hi = data[off + 2] if n >= 2 else 0
        raw = " ".join(f"{data[off + i]:02X}" for i in range(1 + n))
        print(f"${pc:04X}  {raw:<8}  {name} {operand(mode, lo, hi, pc)}")
        pc += 1 + n
    return 0


if __name__ == "__main__":
    sys.exit(main())
