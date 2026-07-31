# Z80 / CP/M integration diagnostic

Offline (VICE) golden tests for MEGA65 CP/M debugging. No Vivado or MEGA65 required.

See also:

- [`doc/HANDOVER-z80-cpm.md`](../../../doc/HANDOVER-z80-cpm.md) — start here on another machine
- [`doc/plans/z80-cpm-test-env.md`](../../../doc/plans/z80-cpm-test-env.md)
- [`doc/z80-cpm-debug.md`](../../../doc/z80-cpm-debug.md)
- [`doc/c128-mister-patch-audit.md`](../../../doc/c128-mister-patch-audit.md)

## Prerequisites

- `z80asm` (`brew install z80asm`)
- VICE `x128` (`brew install vice`)
- Python 3

## Build

```bash
make -C CORE/diag/z80
```

Outputs:

| File | Role |
|------|------|
| `build/z80diag.bin` | Z80 tests T1–T4, ORG `$3000` |
| `build/cia_matrix8502.prg` | 8502 CIA column scanner (C128 mode baseline) |

## VICE smoke (Z80 path)

```bash
make -C CORE/diag/z80 vice-smoke
```

Starts `x128` with the remote monitor, loads the Z80 binary, forces Z80 at `$3000`,
breaks at `halt_loop`, and checks the **register snapshot** (`A='Z' … E='D'`).

VICE 3.10 does not show Z80 data writes in monitor `m` dumps, so the smoke test
does not rely on `$1300` memory. Optional mailbox variant:

```bash
make -C CORE/diag/z80 vice-smoke-mailbox
```

Note: with default MMU `CR`, Z80 maps `$0000–$0FFF` to BIOS ROM. Result/mailbox
addresses stay in `$12xx`/`$13xx` to avoid that overlay.

### Register snapshot (VICE smoke)

At `halt_loop`:

| Reg | Meaning |
|-----|---------|
| A | `'Z'` magic |
| B | T2 idle-matrix XOR (`0` = all columns `$FF`) |
| C | T3 keys: bit0=`1`, bit1=`Z`, bit2=Return |
| D | T4 mailbox: `0`=ok, `1`=fail, `$FF`=no stub |
| E | `'D'` end magic |

### Result page (`$1300`)

Same fields as A/B/C/D/E when Z80 RAM stores are visible (MEGA65). VICE 3.10 monitor `m` dumps do not show those writes.

Mailbox: flag `$127F=1`, stub `$1280`, request/ack `$1200`/`$1201`.

## VICE CIA baseline (8502 / C128 mode)

```bash
make -C CORE/diag/z80 vice-cia
x128 -autostart CORE/diag/z80/build/cia_matrix8502.prg
```

Shows `.` (idle) / `*` (key down) per CIA column. Use this to confirm expected matrix bitmaps before comparing MEGA65 Z80 T2/T3.

## MEGA65 (later)

Same `z80diag.bin` should be hosted via Function ROM / ROM load path / future mount.
Compare `$1300` / register snapshot to VICE. Do **not** patch `C128_MiSTer` keyboard
RTL based on failures — investigate MEGA65 glue first.
