# Handover: Z80 / CP/M MEGA65 debug

Pass this file to a session on another machine. It carries the context that is
**not** obvious from a cold read of the tree.

## Where you are

| Item | Value |
|------|--------|
| Branch | `DEBUG_Z80` (work may be staged but not yet committed — check `git status`) |
| Target HW | MEGA65 R6 |
| Submodule pin | `CORE/C128_MiSTer` @ `1840dd1` (fork `eilers/C128_MiSTer`) |
| Upstream reference | `upstream/master` on that submodule (`MiSTer-devel/C128_MiSTer`) |

## Hard rules (do not rediscover the hard way)

1. **Treat `C128_MiSTer` as stable.** MiSTer hardware is reported to run CP/M. Do **not** patch CIA / `WAIT_n` / `alt_crsr` / `$D02F` `k_reg` inside the submodule to chase MEGA65 CP/M symptoms.
2. **Old keyboard “Fix 1–5” text in `KNOWN_BUGS.md` was removed on purpose.** Those notes pointed at the wrong layer. Do not reinstate submodule keyboard experiments from memory or chat logs.
3. **Fixes belong in MEGA65 glue first:** [`CORE/vhdl/main.vhd`](../CORE/vhdl/main.vhd) (keyboard→`ps2_key` bridge, BRAM/ROM mux, IEC pins, reset), then re-audit local submodule diffs.

## Symptoms to explain

Both CP/M boot disk images work in **VICE**. On MEGA65:

| Disk | MEGA65 behaviour | VICE |
|------|------------------|------|
| A | Reaches `A>` but keyboard wrong/dead (`1`, `Z`, Return among others) | Interactive OK |
| B | Stops at `Insert Disk L in Drive A`; Enter does nothing | **Boots without needing any second disk** |

Disk L on MEGA65 is a **spurious failure**, not a normal multi-volume layout. Possible causes: wrong IEC/mailbox status, and/or Z80 never sees Return.

C128 mode on MEGA65 is stable. That only proves the 8502 + matrix path under non-Z80 timing.

## Architecture (missing if you only look for “mailbox” RTL)

There is **no mailbox block** in the FPGA. CP/M software uses:

- **Z80 direct** CIA1 / VDC for console keyboard
- **Shared RAM + MMU `$D505` bit0** to hand the bus to the **8502**, which runs BIOS85 / Kernal IEC for disk

So CP/M stress is: Z80 IORQ + BUSRQ/BUSAK handoff + IEC under that handoff.

## What is already done (offline, no Vivado/MEGA65)

Read these in order:

1. **This handover** — context
2. [`plans/z80-cpm-test-env.md`](plans/z80-cpm-test-env.md) — full plan + remaining phases
3. [`z80-cpm-debug.md`](z80-cpm-debug.md) — hypothesis matrix + ILA probe list
4. [`c128-mister-patch-audit.md`](c128-mister-patch-audit.md) — every local submodule diff classified
5. [`../CORE/diag/z80/README.md`](../CORE/diag/z80/README.md) — VICE diagnostic

### Diagnostic

```bash
# Prerequisites: z80asm, VICE x128 (e.g. brew install z80asm vice)
make -C CORE/diag/z80
make -C CORE/diag/z80 vice-smoke    # expect EXIT 0; A='Z', E='D', T2 B=00
```

- Smoke test forces Z80 at `$3000` via VICE remote monitor and checks **registers** at `halt_loop` (`A B C D E`).
- VICE 3.10 does **not** show Z80 data writes in monitor `m` dumps — do not chase that as a MEGA65 bug.
- `$1300` result page is for MEGA65 when RAM stores are visible.
- `vice-smoke` uses `--no-mailbox` by default; `vice-smoke-mailbox` is experimental.
- 8502 CIA baseline: `make -C CORE/diag/z80 vice-cia` then `x128 -autostart CORE/diag/z80/build/cia_matrix8502.prg`

### Patch audit highlights

- Z80 `$0000` prime + `reset_t80` full-pulse: MEGA65 BRAM first-fetch / warm reset — keep until proven movable to glue.
- `vicRamDin` port exists in core but **`main.vhd` still wires `vicRamDin => ram_data` (TEMP)** — snow fix incomplete on wrapper side.
- VDC RAM rewritten for Vivado Block RAM; SID/`cpu_6510` diffs are mostly Vivado/line-ending noise.
- ILA `dbg_*` / `z80_we_o` exist on the core but are tied to `open` in `main.vhd`.

### Docs cleanup

[`KNOWN_BUGS.md`](KNOWN_BUGS.md) now points at `z80-cpm-debug.md` for CP/M; no submodule keyboard fix trail.

## What the other machine should do next

Depends on tools available:

### Still no Vivado / no MEGA65

- Keep improving the diag (key injection for T3 in VICE; harden mailbox smoke).
- Draft virtual-IEC wiring from C64MEGA65 + M2M `vdrives` (do not need bitstream to design).
- Optionally commit/push `DEBUG_Z80` if not already.

### Vivado available, no board

- Extend xsim: `tb_z80_cia_iorq`, `tb_z80_mailbox` beside `run_boot_sim.sh`.
- Wire ILA taps in `main.vhd` from existing ports (see probe list in `z80-cpm-debug.md`).

### MEGA65 available

1. Deliver `z80diag.bin` somehow (Function ROM / ROM path / future mount — **not designed yet**).
2. Compare register/`$1300` results to VICE golden.
3. Only then change MEGA65 glue; one hypothesis at a time.
4. Later: virtual IEC (`C_VDNUM`) so CP/M disks are mountable without physical media; prefer D71.

## Important file map

| Path | Role |
|------|------|
| `CORE/vhdl/main.vhd` | MEGA65 glue: mem, `keyboard_ps2_bridge`, IEC, core instance |
| `CORE/vhdl/globals.vhd` | `C_VDNUM = 0` (virtual drives off) |
| `CORE/C128_MiSTer/rtl/cpu_z80.vhd` | Local first-fetch prime |
| `CORE/C128_MiSTer/rtl/fpga64_sid_iec.vhd` | Local reset_t80 / vicRamDin / dbg ports |
| `CORE/sim/tb_c128_boot.vhd` | Boot handoff gate only |
| `CORE/diag/z80/` | Offline Z80 diag |

## One-line mission

Find why MEGA65 CP/M breaks under Z80-active I/O and mailbox IEC **in the wrapper**, prove it with the diag against VICE, and do not fork keyboard RTL inside the stable MiSTer core.
