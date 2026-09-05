# Debugging virtual IEC drives on MEGA65

This is a reusable playbook. The 1581 / `.D81` work in 2026 is the worked
example; the same layers apply to 1541 / `.D64` and 1571 / `.D71` later.

The short version: **do not guess from HDL**. Put a probe on both sides of
every hop, capture a real run, then change one thing.

> **Instrumentation status:** All temporary drive probes have been removed from the
> release implementation, including `C_DEV_VDRIVE_DIAG`, the drive-LED overlay and
> the automated QMON helper. Sections 3.1–3.3 describe the historical instruments
> used during this investigation; they are not commands for the current tree. See
> [jtag-debug-interface.md](jtag-debug-interface.md) for a precise reconstruction
> blueprint.

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
| `C_DEV_VDRIVE_DIAG` snapshot | 157x image/model flags, track, LBA, block count, host-request counters | byte contents of the sector |
| `C_DEV_VDRIVE_DIAG` bus trace | the microsecond-timed IEC handshake of the last ATN window | anything outside an ATN window |
| ILA / simulation | cycle-accurate one hop | a 20-second user session |

The 1581 case spent many rounds on the LED and concluded "wrong track".
The first serial snapshot showed the opposite: DOS was reading Commodore
track 40 correctly. **The LED was looking at the wrong drive's track
register.** Coarse probes lie when the wiring between modules is the
bug.

### 3.1 Historical drive LED (`C_DRIVE_LED_DEBUG`, removed)

The temporary overlay used this colour map:

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

The historical instrumented Shell printed:

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

### 3.3 Historical diagnostic MMIO bank (removed)

`C_DEV_VDRIVE_DIAG` = `0x0106` was read-only. In 4 KiB window 0, addresses
`0..127` select drive 8 and addresses `128..255` select drive 9. Words `0..7`
describe the host side of the drive and are assembled in the QNICE/host clock
domain; track-side state is synchronized before it is used by the request
controller, and event counters preserve short `sd_rd`, `sd_wr`, and `sd_ack`
pulses.

Window 1 exposes each 157x drive's complete 2 KiB DOS work RAM through the
RAM's independent read-only port. QNICE address offsets `0x000..0x7FF` are
drive 8 and `0x800..0xFFF` are drive 9; each byte is returned zero-extended in
a 16-bit QNICE word. This does not halt or otherwise alter the drive CPU.

Unlike window 0, window 1 reads a block RAM with a registered output, so the
device asserts `qnice_dev_wait_o` for one clock. Without that stall QNICE
samples one cycle too early and every location returns the byte belonging to
whatever address the CPU drove just before — a constant that looks like real
data but tracks the monitor's instruction stream instead of the address.

The removed host-side helper entered QMON, read selected locations, and returned to the
Shell automatically. It temporarily asserts the core pause bit while reading,
so the dump is a coherent snapshot rather than RAM changing underneath it:

```
sudo python3 CORE/scripts/qnice_drive_monitor.py peek 8 01af 02ac 01b1
sudo python3 CORE/scripts/qnice_drive_monitor.py dump 8 0200 02ff
```

No keyboard interaction is needed: the Shell's `CHECK_DEBUG` also enters the
monitor when it sees CTRL+E on this port, and the helper sends it. Stop any
`jtag_console.sh` process beforehand because only one process may own the UART.

The `Run/Stop + Cursor Up + Help` combination still works, but it is awkward
enough to be worth avoiding. All three keys have to land in one keyboard scan,
and on the MEGA65 `Cursor Up` also asserts shift towards the core, so the C128
sees `Shift + Run/Stop` and starts a LOAD while QNICE sees nothing.

Two further things make the combination look dead when it is not. The prompt
appears only on the JTAG UART, never on HDMI, so the screen reacting to the
keys is expected. And a short press of the reset button restarts only the core:
QNICE keeps running, so the Shell banner that proves the serial path works only
appears after holding reset for more than 1.5 seconds.

If a previous session died mid-command, `--attached <retaddr>` re-uses a prompt
that is already open instead of waiting for the key combination, and `cmd`
sends raw QMON commands:

```
sudo python3 CORE/scripts/qnice_drive_monitor.py --attached 2092 peek 8 02ac
sudo python3 CORE/scripts/qnice_drive_monitor.py --attached 2092 cmd MD70007017
```

The helper types one character at a time. QMON echoes in a polling loop with no
receive FIFO, so a back-to-back burst at 115200 silently loses characters and
the monitor is left waiting for the rest of an operand.

### 3.4 D71 side-detection failure

The 1571 DOS does not trust the image type supplied by the host. In native
mode it reads track 53 and compares the GCR header's disk ID with the ID it
already learned from track 18. Only a successful job and matching ID leave
`$01AF=$80` and `$02AC=$47`; failure leaves `$01AF=$00` and `$02AC=$24`, which
makes a valid D71 appear as a full 35-track disk.

The original mount delay announced insertion through write-protect sense
before sector data became available:

```
wps_n        = ~readonly ^ ch_timeout[23]
disk_present = present only when ch_timeout == 0
GCR busy     = sd_busy | ~disk_present
```

`ch_timeout[23]` makes its final transition when the top two timeout bits
become `00`, one quarter of the delay before zero. DOS reacted immediately,
but `~disk_present` forced the track-53 GCR job busy, so the no-retry probe
failed. No change indication occurred after `disk_present` eventually rose.

`c157x_drv.sv` now raises `disk_present` at that final write-protect transition
and retains the remaining quarter as settling time in `disk_ready`. The D71
boot regression sends `U0>M1`, observes the track-53 request at linear LBA
1040, and requires `$01AF=$80`, `$02AC=$47`, PA5 high and the expected disk ID.

That mount-timing fix was necessary but not sufficient: the probe still failed
on hardware, where `$02AC` briefly reached `$47` and fell back to `$24`. The
`watch` mode of `qnice_drive_monitor.py` caught the drive requesting LBA 1040,
which proved the probe runs and fails rather than being skipped.

The remaining cause is host transfer latency. `HANDLE_DRV_RD` in `shell.asm`
copies a track byte by byte through a 4k window, roughly 55 QNICE instructions
plus a HyperRAM read per byte, so one 19-sector track costs 15-25 ms. The
sector GCR engine holds its bit clock in reset for the whole transfer, leaving
the DOS blind to sync marks for that long. After a head step this is harmless,
because the DOS expects garbage while the head settles and retries. A side
change has no such grace period: on a real 1571 the second head is already over
the disk, so the single-shot probe at `$A708` fails and `$A726` records 36.

Stopping the drive CPU during `host_busy` made the probe pass, but was not a
safe solution. DOS prefetches sectors while it is still sending the previous
sector over IEC; freezing it in the middle of that handshake can exceed the
computer's 200 us end-of-input deadline. Hardware RWTEST consequently failed
on record 1 intermittently.

The final design instead keeps one FPGA sector-buffer bank per 1571 head.
Whenever a physical track is loaded, `c157x_track.sv` requests the selected
side first and then prefetches the corresponding track under the other head.
Changing VIA1 PA2 then selects an already resident bank immediately, matching
the real two-head mechanism without stopping the drive CPU. Each bank has its
own cached logical-track tag, and writeback carries the bank belonging to the
request so dirty side-0 and side-1 tracks remain independent.

The first dual-bank hardware build still reported 0 blocks free even though
diagnostics showed a completed 19-sector LBA 1040 fetch. The cache was present;
the failure was the bank transition itself. `c1541_gcr` uses registered block
RAM output and previously reset only its bit-clock divider when the logical
track changed. One old-bank byte could therefore be spliced into the block in
progress. It now starts a fresh sync/header on every head or track change. The
GCR regression poisons side 0, switches to side 1 at an arbitrary bit phase,
and requires the first complete header/data pair to come solely from side 1.

Any monitor session still parks the shell and therefore starves the drive;
`?FILE NOT FOUND` during monitoring is expected rather than a drive fault.

The boot simulation models this: the fourth argument of
`run_c157x_boot_sim.tcl` is the host cost per transferred byte in nanoseconds.
At `4000` the D71 initialization reproduces the hardware failure with a
single track buffer and passes with the dual-head cache, while the default `0`
keeps the instant host model.

| Word | Contents |
|---|---|
| `0` | signature/version `157D` |
| `1` | image/model, motor, VIA activity, host handshake, side and reset flags |
| `2` | raw track / low byte of host buffer address |
| `3` | read-request count / write-request count |
| `4`/`5` | 32-bit linear 256-byte LBA |
| `6` | block count / acknowledge count |
| `7` | high buffer address / FDC busy / write / buffer-update flags |

Words `8..15` describe the DOS running inside the drive. They are sampled in
the drive clock domain and crossed without a synchronizer, so treat each field
on its own: they are mirrors and trend counters, never an atomic snapshot.

| Word | Contents |
|---|---|
| `8` | signature `D05A` |
| `9` | sticky state of the last ATN-low window (see below) |
| `10` | VIA1 port B output / direction at the end of that window |
| `11` | drive state `$20` / job slot `$00` |
| `12` | IRQ vector fetches / number of jobs posted |
| `13` | VIA1 ORB writes / ATN falling edges seen |
| `14` | VIA1 port B output / direction, live |
| `15` | IEC inputs (ATN, CLK, DATA, SRQ) / outputs, motor, host busy |

Word `9` exists because the whole ATN handshake lasts about a millisecond,
far below the rate at which the shell samples this bank, so a plain mirror of
the bus always reads back idle. The bits are set while ATN is low and cleared
only by the next ATN falling edge, so a slow reader still sees the last window.

| Bit | Meaning while ATN was low |
|---|---|
| `0` | drive pulled DATA, i.e. it acknowledged ATN |
| `1` | drive pulled CLK |
| `2`/`3` | DATA / CLK low on the merged bus |
| `4`/`5` | VIA1 / VIA2 raised an interrupt |
| `6`/`7` | CIA raised an interrupt / the CPU saw IRQ asserted |
| `8`/`9` | DOS wrote / read VIA1 ORB (`$1800`) |
| `10` | CPU fetched the IRQ vector |
| `11` | fast serial direction was set |
| `12`/`13` | CPU ran at 2 MHz / was halted by the clock switch |
| `14` | window outlasted the 16-bit tick counter |
| `15` | a window was observed at all |

Bit `0` separates the two failure modes that look alike from the computer:
cleared means the drive never acknowledged ATN and the computer reports
DEVICE NOT PRESENT, while set with bits `7`/`8` cleared means the hardware
acknowledged but the DOS never ran its ATN service routine.

Read all sixteen words after a failure. Counter deltas identify which drive
made the request even if the request pulse has already ended. A D64 or
D71 request is linear-sector addressed: word 6 reports `sectors-1`, matching
the `vdrives` block-count contract.

#### Serial-bus trace, words `16..63`

The sticky bits in word `9` prove whether the drive answered ATN at all, but
they collapse the entire window into one value and so cannot show *where*
inside a command byte a handshake stalls. Words `16..63` are 48 trace entries
that record one line per change of the serial bus. Capture restarts on every
ATN falling edge, so the buffer always holds the beginning of the most recent
attempt and never needs to be armed. Entries past the end of a short window
keep the value they had during the previous, longer one.

| Bits | Contents |
|---|---|
| `15` | ATN in |
| `14`/`13` | CLK / DATA in, after the wired-AND merge |
| `12`/`11` | DATA / CLK the drive itself drives, `1` = released |
| `10` | VIA1 PB4, the ATNA latch |
| `9`/`8` | VIA1 PB1 / PB3, the DATA / CLK output registers |
| `7:0` | microseconds since the previous entry, saturating at 255 |

The timestamp counts `ph2_r[0]`, the drive's own 1 MHz phase, so it is a true
microsecond in drive time regardless of the model or clock speed selected. A
saturated age still writes an entry, which keeps a long wait visible as a run
of 255 us gaps instead of silently folding into the next transition.

Entry `0` is the state at the ATN falling edge itself and always carries a zero
age. Reading the high byte of successive entries as a column shows the byte
being clocked in: for a healthy receive, CLK in (bit `14`) toggles eight times
while DATA in (bit `13`) carries the data, and the drive answers each byte by
pulling DATA (bit `12` going to `0`).

Words `64..111` contain, for each bus-trace entry, the DOS bit counter `$98`
(high byte) and the last value the DOS actually read out of the VIA1 interrupt
flag register `$180D` (low byte). Those are the two inputs the receive loop
branches on: `$98` counts the eight bits of the frame, and bit `6` of `$180D`
is the T1 flag that sends the DOS to the EOI acknowledge at `$E9F2`. A DATA
pulse in the bus trace is therefore attributable to a bit count that ran out
early or to a timer flag that was set when it should not have been.

Both fields are sampled off the drive CPU bus rather than out of the VIA, so
the vendored `iecdrv_via6522.vhd` stays untouched.

Words `112..114` retain the live CPU bus address, data and interrupt control
flags, word `115` the live `$98`/`$180D` pair, and word `116` the last value
`LDA $1800` returned alongside the raw port B pin vector, which separates "the
VIA cannot see the bus" from "the DOS read the bus correctly and still decided
wrongly".

Word `117` is a sanity check on the drive's sense of time. Every timestamp in
the bus trace is counted in the drive's own 1 MHz phase, so a drive whose time
base is wrong produces a trace that is internally consistent and still
disagrees with the C128, which is the real-time master. The word counts those
ticks against the main core clock instead: at 31.5 MHz a correct 1 MHz phase
gives about `1040` ticks per 32768 clocks, and any other value scales every
other timestamp in the snapshot.

The shell prints this snapshot roughly three times a second from its main loop
(`LOG_DIAG_TICK`). Logging only after a mount is not enough: a drive that stops
answering does so later, while the computer sits in a KERNAL wait loop that
never times out.

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

## 7a. Worked example: 1541 `?FILE NOT FOUND` on `LOAD"$",8`

The drive answered on the IEC bus (otherwise the C128 would say
`DEVICE NOT PRESENT`), the image mounted, and the host request
contract simulated clean. The disk *surface* was the broken hop.

Two defects, both in the ported sector-GCR path, both invisible to
the tests that existed at the time:

1. **Mirrored GCR codes.** `c1541_gcr` serialises a five-bit code
   with `gcr_nibble[gcr_bit_cnt]`, and `gcr_bit_cnt` counts up.
   The reference compensates by storing every code in its
   `gcr_lut` bit-reversed. Our port moved the table into
   `c1541_gcr_codec` and made it canonical without changing the
   serialiser, so every code reached the head mirrored. The
   round-trip codec test passed because encoder and decoder shared
   the same wrong convention.
2. **Missing SOE gate.** `c157x_h156` masks byte-ready with SOE
   internally; the sector-GCR module does not. `c157x_logic` fed
   its `byte_n` straight to the CPU, so DOS saw byte-ready pulses
   while it had byte-ready switched off. The reference computes
   `cpu_so_n = byte_n | ~soe`.

The lesson for the next port: a self-consistent round trip proves
nothing about what lands on the disk surface. `tb_c157x_gcr_path`
now decodes the emitted surface with a canonical table written out
literally in the bench, checks header and data blocks against the
sector buffer, and sweeps all four density values, because DOS
picks a different density per zone.

## 7b. Worked example: 1571 `LOAD"$",8` hangs, LED never lights

`PRINT DS$` answered `73,CBM DOS V3.0 1571`, so native 1571 DOS had
booted and the serial bus worked. 1541 mode listed directories.
`LOAD"$"` in 1571 mode blocked with the CPU at `$9459` polling VIA1
PA7 (BYTE READY).

Simulation retired the same job. Hardware windowed counters showed
the GCR engine still emitting bytes at full rate, SOE open, the
seek already on track 18 — and **zero** port A reads catching the pin
low. The 1541 DOS catches byte-ready on the CPU SO pin (edge). The
1571 DOS polls PA7. At 2 MHz the poll loop is an integer multiple of
the bit cell, so the sample lands on the same point of every cell
forever, outside `c1541_gcr`'s short pulse. `c157x_h156` already
holds byte-ready until `ted` (CPU touch of VIA2) and forces `ted`
true at 1 MHz; the sector path did not. The latch in `c157x_logic`
mirrors that: transparent at 1 MHz (1541 unchanged), sticky at
2 MHz until VIA2 is touched.

A second hardware-only defect sits next to it: Vivado's synthesised
`iecdrv_via6522` can leave IFR bit 6 set after a T1-high write that
XSim clears. The 1541 receive path then takes EOI immediately. The
T1 guard in `c157x_logic` masks only the polled bit for the programmed
one-shot; the vendored VIA is left alone.

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
