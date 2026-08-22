# Debugging virtual IEC drives on MEGA65

This is a reusable playbook. The 1581 / `.D81` work in 2026 is the worked
example; the same layers apply to 1541 / `.D64` and 1571 / `.D71` later.

The short version: **do not guess from HDL**. Put a probe on both sides of
every hop, capture a real run, then change one thing.

> **Instrumentation status:** The temporary UART/MMIO probes described below
> were removed after the fix was verified on hardware. The LED overlay remains
> in `main.vhd` behind `C_DRIVE_LED_DEBUG := false`. This document records the
> probe design and word map so they can be restored for a future investigation.

## 1. What you are debugging

A virtual drive is a chain of four machines that do not share a clock:

```
C128 Kernal / BASIC
        │  IEC bus (CIA, ATN / CLK / DATA / SRQ)
        ▼
1581 DOS (6502 + CIA inside iec_drive)
        │  WD1772 commands (seek, read sector, force interrupt)
        ▼
c1581_fdc1772  ──sd_rd / sd_lba──►  vdrives  ──MMIO──►  QNICE Shell
        │                               │
        │ clk_main_i (~32 MHz)          │ clk_sd_i (50 MHz)
        └───────── CDC ─────────────────┘
```

QNICE then copies 256-byte blocks out of HyperRAM into the FDC's sector
buffer. If any hop is wrong, the C128 symptom is almost always the same:

- `00, OK, 00, 00` after `I` (DOS answered the command channel)
- `?FILE NOT FOUND` on `LOAD"$"`
- `74, DRIVE NOT READY, 00, 00` afterwards

That status means "the drive talked, then it could not trust the disk".
It does **not** tell you whether the motor ran, whether the FDC sought,
whether a sector was requested, or whether the host filled the right
block. Those are four different bugs.

## 2. Rules that keep the investigation cheap

1. Write 3–5 hypotheses that name a **hop** and a **wrong value**.
   "The drive is broken" is not a hypothesis.
2. Add probes that can confirm **and** reject all of them in one run.
   A probe that can only confirm is a trap.
3. Capture one serial log per bitstream. Flash, mount, `I`, `LOAD"$"`,
   stop. Do not mix two bitstreams in one `jtag.log`.
4. Do not "fix" from HDL until a log line names the hop. Speculative
   geometry / timeout / sector-base edits cost days and hide the real
   bug.
5. After a proven fix, **leave the probes in** for one verification
   run. Compare the same fields before and after.
6. When a hypothesis dies, revert the code that existed only for that
   hypothesis. Do not accumulate guards.

## 3. Observability layers, coarsest first

Use the cheapest layer that can still distinguish your hypotheses.
Each layer is a bitstream (or ROM) change.

| Layer | What it can prove | What it cannot |
|---|---|---|
| Drive LED colour | motor on/off, "did we ever seek T40", "did `sd_rd` fire" | LBA, sector, which drive |
| `DRV_RD LBA256=` on JTAG | the block QNICE actually fetched | why that LBA was chosen |
| `C_DEV_V1581_DIAG` snapshot | FDC command, track, sector, geometry, LBA at several hops | byte contents of the sector |
| ILA / simulation | cycle-accurate one hop | a 20-second user session |

The 1581 case spent many rounds on the LED and concluded "wrong track".
The first serial snapshot showed the opposite: DOS was reading Commodore
track 40 correctly. **The LED was looking at the wrong drive's track
register.** Coarse probes lie when the wiring between modules is the
bug.

### 3.1 Drive LED (`C_DRIVE_LED_DEBUG` in `CORE/vhdl/main.vhd`)

Enabled while the constant is `true`. Current colour map:

| Colour | Meaning |
|---|---|
| Magenta flash (~400 ms) | rising edge of `sd_rd(0)` |
| White | FDC track 39 (Commodore T40 / BAM) latched |
| Red | FDC track 1 (old 1-bit seek leftover) |
| Green | FDC track 0 |
| Yellow | some other track, motor on |
| Cyan | motor off, never saw track 39 or 1 |
| Blue | spin-up (`floppy_ready` not yet true) |

Use this only as a "did anything happen" channel. Treat a colour as
evidence about **the signal the LED is wired to**, not about the drive
the user mounted.

### 3.2 JTAG serial console

The M2M / QNICE Shell prints at **115200 8N1**. The MEGA65 factory core
prints at **2 000 000**. Mixing them produces garbage, not silence.

```
sudo CORE/scripts/jtag_console.sh -l jtag.log          # this core
sudo CORE/scripts/jtag_console.sh -b 2000000           # factory core
```

Do **not** press Run/Stop + Cursor Up + Help. That combination enters
the QNICE monitor and stops the Shell, so `DRV_RD` lines stop appearing
even if the drive is requesting sectors.

`/dev/ttyUSB1` needs `dialout` or `sudo`.

The instrumented Shell printed:

- `DRV_RD LBA256=....` from `HANDLE_DRV_RD` in `M2M/rom/shell.asm`.
  This is the 256-byte LBA QNICE is about to copy. `vdrives` is
  instantiated with `BLKSZ => 1` (256 bytes). The FDC thinks in
  512-byte blocks; `iec_drive.sv` left-shifts by one for D81.
- `V1581 SNAP[00..1F]=` whenever the WD command count **or** the
  `sd_lba` latch count changes, and again after each sector fill.

After changing `shell.asm` / `strings.asm` / `shell_vars.asm`:

```
CORE/m2m-rom/make_rom.sh
```

The ROM is baked into the bitstream. A Shell change without a rebuild
does nothing on the MEGA65.

### 3.3 Diagnostic MMIO bank

The temporary build reserved device id `C_DEV_V1581_DIAG` = `0x0106` in
`CORE/vhdl/globals.vhd`. Re-add that constant when restoring the bank.
QNICE maps it the same way as `vdrives`: pick the device, 4K window 0,
then sequential 16-bit words at `M2M$RAMROM_DATA`.

The FDC snapshot (`dbg_ext`) was built on `clk_main_i` and crossed with
`xpm_cdc_array_single` into `clk_sd_i` before QNICE reads it. Fields
are **latched / cumulative** (command counters, last command, LBA at
the last `sd_rd` latch) so a torn sample during a write is rare. Live
combinational fields (`sd_lba_comb`, current track/sector) can still
tear; prefer the latched copies when they disagree.

Map version is word 1 (`0200` at the time of writing). Bump it when
you change the layout so an old Shell dump is obvious.

#### Word map (version `0200`, 32 words)

| Word | Contents |
|---|---|
| `00` | signature `1581` (bank present) |
| `01` | map version |
| `02` | WD command count |
| `03` | Type I count (restore / seek / step) |
| `04` | Type II count (read / write sector) |
| `05` | Type III count (read address / read track / write track) |
| `06` | last command byte / track register at that write |
| `07` | last sector / last data register at that write |
| `08` | geometry + FDC FSM flags (see below) |
| `09` | number of times `sd_lba` was latched |
| `0A`/`0B` | live `sd_lba_comb` (FDC, 512-byte) |
| `0C`/`0D` | latched `sd_lba` (FDC, 512-byte) |
| `0E` | track / sector captured at that latch |
| `0F` | geometry at that latch `{0, doubleside, side, spt}` |
| `10` | live track / sector |
| `11` | disk-change / CIA port A |
| `12`/`13` | `iec_sd_lba(0)` in `main.vhd` (256-byte, after `<< 1`) |
| `14`/`15` | `img_size` |
| `16` | legacy sticky `iec_dbg` |
| `17`/`18` | `sd_lba[0]` inside `iec_drive.sv` (256-byte) |
| `19`/`1A` | `c1581_sd_lba[0]` (512-byte, before the mux and shift) |
| `1B` | `img_hd` (bit 0 set → 1581 path selected) |
| `1C`/`1D` | `iec_sd_lba(1)` — **the other drive** |
| `1E`/`1F` | `vd_sd_lba(0)` — what `vdrives` is actually handed |

Word `06` command nibble: `8x` = read sector, `Ax` = write sector,
`Cx` = read address, `Dx` = force interrupt, `1x`/`2x`/`3x` = seek /
step. Track `27` hex = 39 decimal = Commodore track 40.

Expected D81 BAM numbers, track 39, 10 sectors/track, double sided,
sector *n* (1-based):

```
fdc_lba_512  = (10 * 39) << 1  + (side ? 0 : 10) + n - 1
             = 780 + n - 1          (side 1, which the 1581 uses for T40)
qnice_lba_256 = fdc_lba_512 << 1    = 1560 + 2*(n-1)
```

Worked values from a healthy BAM sweep (sectors 7,8,9,10,1,…,6):

| Sector | FDC `sd_lba` | QNICE `LBA256` |
|---|---|---|
| 7 | `0312` (786) | `0624` (1572) |
| 8 | `0313` | `0626` |
| 10 | `0315` | `062A` |
| 1 | `030C` (780) | `0618` (1560) |
| 6 | `0311` | `0622` |

If those two columns do not match, the defect is **after** the FDC.
If they match the formula but `DRV_RD` prints something else, the
defect is **at or after** the `vdrives` / QNICE binding.

## 4. Standard reproduction

Always the same sequence, so logs compare:

1. Flash `CORE/CORE-R6-vivado2022.runs/impl_1/mega65_r6.bit`.
2. Start `sudo CORE/scripts/jtag_console.sh -l jtag.log` **before**
   touching the C128.
3. Mount `Empty.d81` (or the image under test) on device 8.
4. `OPEN1,8,15,"I":CLOSE1` then `PRINT DS$`.
5. `LOAD"$",8` then `PRINT DS$`. Note the LED colour.
6. Ctrl+C the console. Keep the whole `jtag.log`.

Optional extras that were useful:

- `PRINT DS$` with no `I` after a fresh mount (DOS power-on message).
- Repeat `LOAD"$"` without remounting (cache / RNF leftover).
- Mount on device 9 as well, to see whether both unpacked-array
  indices move.

## 5. How to place a new probe

The pattern that survived this investigation:

1. Latch the value **in the clock domain that owns it**.
2. Add or widen `dbg_ext` (the final debug build used 352 bits out of
   `iec_drive`) or add a
   word in `v1581_diag_read` that samples a VHDL signal **on the QNICE
   clock** (`iec_sd_lba`, `vd_sd_lba`, `img_size`). Signals that
   already live on `clk_sd_i` do not need the CDC.
3. Give the new word a stable index. Dump N consecutive words from
   address 0 in `LOG_V1581_DIAG` / `HANDLE_DRV_RD` so you do not have
   to remember offsets.
4. Run `CORE/scripts/rtl_elab_check.tcl` before a full bitstream.
   Implicit-net mistakes (`identifier already declared on line …`)
   show up here in ~2 minutes instead of ~25.

Do **not** print from the FDC in Verilog. There is no UART there.
The QNICE Shell is the only cheap runtime printer.

## 6. Mixed-language array reversal

This is the class of bug the 1581 hunt actually found. Write it down
because it will hit every other virtual drive, and any other Verilog
module with an unpacked array port.

### What goes wrong

SystemVerilog unpacked arrays are declared `[NDR]`, i.e. indices
`0 to NDR-1`. VHDL unconstrained arrays in this tree are usually
declared `(VDNUM - 1 downto 0)`, i.e. indices `1 downto 0` for two
drives.

When Vivado associates a **whole array port** with a **whole array
signal** of the opposite direction, it pairs **positionally**: first
element to first element. It does **not** match by index name.

```
Verilog sd_lba[0], sd_lba[1]     listed  0 then 1
VHDL    sd_lba(1 downto 0)       listed  1 then 0
                 ↓ bind whole
VHDL(1) ← Verilog[0]     VHDL(0) ← Verilog[1]
```

Packed vectors (`sd_rd`, `sd_wr`, `sd_ack`, `img_mounted`) are just
bit vectors. Both languages write them the same way, so request
**timing** stays on the right drive while the **payload** (`sd_lba`,
`sd_buff_din`, `out_track`) comes from the other drive.

Symptom in the log: FDC words `0C`/`0D` step through the BAM, QNICE
prints one constant LBA, and that constant equals the **idle**
drive's leftover (`0778` in the 1581 case).

### How to bind it correctly

One declaration cannot satisfy both neighbours.

- On the `iec_drive` (Verilog) side, declare the VHDL signals
  **ascending**: `vd_vec_array(0 to G_VDNUM - 1)`.
- On the `vdrives` (VHDL, `downto`) side, do **not** associate the
  ascending signal. Copy by index:

```vhdl
iec_drives_reset_gen : for i in 0 to G_VDNUM - 1 generate
  vd_sd_lba(i)          <= iec_sd_lba(i);
  vd_sd_blk_cnt(i)      <= iec_sd_blk_cnt(i);
  vd_sd_buf_data_out(i) <= iec_sd_buf_data_out(i);
end generate;
```

Index-valued assignment is direction-independent. Always probe
**both** indices (`iec_sd_lba(0)` and `(1)`) plus the copy that
`vdrives` actually sees (`vd_sd_lba(0)`). If 0 is right and 1 holds
the stale constant, the Verilog boundary is fixed. If `DRV_RD` still
prints the stale constant, the `vdrives` boundary is still reversed.

`out_track` is unpacked too. The debug LED reads `iec_out_track(0)`;
if that array is reversed, the LED describes the empty drive.

## 7. Worked example: 1581 `74, DRIVE NOT READY`

Chronology of *evidence*, not of guesses.

1. LED: magenta on mount, then cyan. `I` went blue → yellow → cyan.
   `LOAD"$"` did not even flash. **Hypothesis at the time:** DOS
   never issued a disk command. **Later rejected.**
2. JTAG `DRV_RD LBA256=0778` ten times on mount, never on `LOAD"$"`.
   Decimal 1912 is not the BAM (1560). **New hypothesis:** FDC
   geometry / LBA formula is wrong.
3. Snapshot words `06`/`07`: last command `88`, track `27`, sectors
   `07`…`0A` then `01`…`06`. Type II count rose by ten. **FDC and
   DOS were doing the right thing.** Geometry `spt=10`, double sided,
   side 1. Formula on those inputs is 786, 787, … — not 1912.
   **Geometry hypothesis rejected.**
4. Latched FDC LBA (`0C`) matched the formula and stepped every
   request. Latch count (`09`) stepped 1…10. **CDC / stale-latch
   hypothesis rejected.**
5. `iec_sd_lba(0)` was the constant `0778`; `iec_sd_lba(1)` held the
   correct `0624`, `0626`, … **Unpacked-array reversal at the
   Verilog boundary, confirmed.**
6. Signals flipped to `0 to 1`. Next run: `iec_sd_lba(0)` became
   `0624` (correct) and the LED went white (track 39, now looking
   at the right drive). `DRV_RD` still printed `0778`, which now
   sat in `iec_sd_lba(1)`. **Same reversal, second hop, into
   `vdrives`.**

That is the whole method: each run moved the "last correct value"
one hop further along the chain until the remaining hop was the
module boundary.

## 8. Build / flash loop

```
# Shell / strings changed?
CORE/m2m-rom/make_rom.sh

# HDL sanity (~2 min)
CORE/scripts/vivado.sh -mode batch \
  -source CORE/scripts/rtl_elab_check.tcl \
  -tclargs CORE/CORE-R6-vivado2022.xpr

# Bitstream (~25 min)
CORE/scripts/vivado.sh -mode batch \
  -source CORE/scripts/build_bitstream.tcl \
  -tclargs CORE/CORE-R6-vivado2022.xpr
```

Output: `CORE/CORE-R6-vivado2022.runs/impl_1/mega65_r6.bit`.

Simulations that already cover *other* drive bugs, and should be
re-run if you touch those files:

| Testbench | What it caught |
|---|---|
| `CORE/sim/tb_drive_rom.vhd` | DOS ROM off-by-one on the QNICE write |
| `CORE/sim/tb_iecdrv_mem.vhd` | drive RAM address vs data skew |
| `CORE/sim/tb_vdrive_mount.vhd` | `img_type` decode; did **not** catch the Shell `R7` clobber |

None of them instantiate the mixed-language `sd_lba` array the way
`main.vhd` does. A unit test of `c1581_fdc1772` alone will never see
the reversal.

## 9. Next time you debug a drive

Start here, in order:

1. Is the image type actually D81 / D64 / D71? (`img_size` words
   `14`/`15`, `img_hd` word `1B`, DOS banner in `DS$`).
2. Does the FDC issue Type II commands with the track you expect?
   (words `02`–`07`).
3. Does `sd_lba` at the FDC match the formula for that track/sector?
   (words `0C`–`0F`).
4. Do `iec_sd_lba(0)`, `iec_drive sd_lba[0]`, and `vd_sd_lba(0)` all
   equal `FDC << 1` (D81) or the FDC value (1541/1571)? If any one
   of them is a constant that belongs to the other index, stop and
   fix the array binding. Do not touch the FDC.
5. Only then look at sector payload (HyperRAM contents, `sd_buff`
   writes, RNF).

When the bug is fixed and a verification log shows `DRV_RD LBA256=`
walking `0618`…`062A` on a BAM read **and** `LOAD"$"` produces a
directory, strip the LED override (`C_DRIVE_LED_DEBUG := false`) and
the `#region agent log` blocks, but keep this document.
