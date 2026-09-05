# Reusable JTAG/QMON debug interface

This document records the virtual-drive diagnostic interface that was used to
qualify the C128 core. The implementation was intentionally removed after the
drive work was complete; this is a blueprint for rebuilding the same pattern
for another subsystem.

The important architectural point is that this is not a custom JTAG register
protocol. JTAG exposes QNICE's 115200-baud UART as `/dev/ttyUSB*`. A host tool
uses that serial connection to enter the resident QNICE Monitor (QMON), and
QMON reads ordinary QNICE MMIO. The core-specific diagnostic hardware appears
behind the existing M2M RAM/ROM window bridge.

## End-to-end path

```text
host Python tool
  -> /dev/ttyUSBn, 115200 8N1
  -> QNICE Shell receives CTRL+E
  -> QNICE Monitor command parser
  -> QNICE MMIO at FFF4/FFF5 and 7000..7FFF
  -> M2M RAM/ROM device decoder
  -> core-specific device 0106
  -> snapshot registers, or a registered read-only RAM port
```

Each layer has one responsibility:

1. The host tool handles UART framing, command pacing, parsing and cleanup.
2. The Shell provides a remotely triggerable, resumable transition into QMON.
3. QMON provides generic memory examine/change commands.
4. The RAM/ROM bridge converts a device plus window selection into a 4 Kiword
   MMIO aperture.
5. The core device decodes addresses, returns diagnostic words and asserts a
   wait state when its data is registered.
6. The observed subsystem supplies either stable snapshots or a non-intrusive
   secondary RAM read port.

## QNICE-side addresses

The generic M2M bridge is defined in `M2M/rom/sysdef.asm`:

| QNICE address | Purpose |
|---|---|
| `FFE0` | M2M control/status register; bit 1 pauses the emulated core |
| `FFF4` | RAM/ROM device selector |
| `FFF5` | 4 Kiword window selector |
| `7000..7FFF` | selected device/window data aperture |

The drive diagnostic device used ID `0106`, declared as
`C_DEV_VDRIVE_DIAG` in `CORE/vhdl/globals.vhd`. Device IDs `0100..FFFF`
are available to core-specific RAM-like devices.

To read offset `N` from window `W` manually in QMON:

```text
MCFFF40106   select device 0106
MCFFF5WWWW   select window W
ME7000+N     examine one 16-bit word
```

QMON does not evaluate `7000+N`; the final command must contain the calculated
four-digit hexadecimal address. `MD700077FF`, for example, dumps 2048 words.

Relevant QMON commands:

| Command | Meaning |
|---|---|
| `MEaaaa` | examine one word at address `aaaa` |
| `MDaaaabbbb` | dump inclusive range `aaaa..bbbb` |
| `MCaaaavvvv` | write word `vvvv` to address `aaaa` |
| `CRaaaa` | continue execution at return address `aaaa` |

QMON prints lines with LF followed by CR, rather than conventional CR/LF.
Host parsers must tolerate `"\n\r7000: ..."`.

## Entering and leaving QMON remotely

The normal Shell checks the JTAG UART status register in `CHECK_DEBUG`.
If a character is available, it consumes it; CTRL+E (`05`) branches to
`START_MONITOR`. The original keyboard chord remains an optional fallback.

`START_MONITOR` prints:

```text
C R aaaa to return
QMON>
```

The host extracts `aaaa`, performs its monitor commands, then sends `CRaaaa`.
This resumes the Shell at the point selected by its release-mode monitor
wrapper. If the return address cannot be parsed, the safe behavior is to leave
QMON active rather than jumping to a guessed address.

The UART has no receive FIFO usable by QMON's polling loop. Sending a complete
command at 115200 baud can lose characters even though the OS write succeeds.
The proven host implementation sends one character every 10 ms and waits for
the next `QMON> ` prompt after every command.

Only one process may own `/dev/ttyUSBn`. Stop `jtag_console.sh` before starting
an automated session. Device permissions may need to be restored after USB or
FPGA re-enumeration:

```sh
sudo setfacl -m u:$USER:rw /dev/ttyUSB0
```

## Core-specific RAM/ROM device

`mega65.vhd` adds the device to `core_specific_devices`. The decoder defaults
all device enables and wait outputs inactive, then for device `0106`:

```vhdl
qnice_vd_diag_ce  <= qnice_dev_ce_i;
qnice_dev_data_o  <= qnice_vd_diag_data;
qnice_dev_wait_o  <= qnice_vd_diag_wait;
```

The defaults are essential. This is combinational device selection; retaining
an enable or wait value would infer state and can stall unrelated devices.

The implementation used two windows:

| Window | Address map | Behavior |
|---|---|---|
| `0` | `000..07F` drive 8, `080..0FF` drive 9 | combinational 16-bit snapshots |
| `1` | `000..7FF` drive 8, `800..FFF` drive 9 | zero-extended bytes from 2 KiB DOS RAM |

Unused addresses returned zero. The signatures `157D` and `D05A` in window 0
made it possible to distinguish a valid snapshot from an all-zero or stale
bus.

## Snapshot design and clock domains

Short request pulses cannot be observed reliably by polling. Window 0
therefore included monotonically increasing counters for rising edges of
`sd_rd`, `sd_wr` and `sd_ack`, plus the most recent LBA and block count.

The host-side fields were collected in `clk_sys`, the same domain as the
vdrives request interface. DOS/VIA fields were collected in the drive clock
domain and exposed as a packed vector. Crossing a large live vector without a
handshake is acceptable only for diagnostic trends: each word may belong to a
slightly different instant. It must not be interpreted as an atomic snapshot.

Use one of these patterns when recreating the interface:

- event counter for a pulse;
- sticky bit cleared by a well-defined new event;
- slowly changing live mirror for qualitative inspection;
- explicit request/acknowledge snapshot if words must be mutually consistent.

Do not synchronize a one-cycle pulse with a plain two-flop level synchronizer;
it may disappear entirely.

## Non-intrusive work-RAM access

The 157x DOS RAM uses `iecdrv_mem`, a true dual-port RAM. Port A remains the
drive CPU's normal read/write port. The diagnostic path repurposed port B as:

```systemverilog
.clock_b(dbg_clk),
.address_b(dbg_ram_addr),
.data_b(8'h00),
.wren_b(1'b0),
.q_b(dbg_ram_data)
```

`WRITE_B=0` is significant. It selects the read-only-port RAM template before
synthesis; merely tying a write-enable low may still infer a dual-write-port
RAM with undefined same-address collision behavior.

The debug address and RAM clock came directly from the QNICE/device domain.
No request crosses into the drive CPU clock domain, and the CPU does not stop.
The only shared resource is the memory array's independent second port.

### Registered-read wait state

Block RAM output is registered. Address `A` is applied on one `dbg_clk` edge
and its byte becomes valid after that edge. Returning data combinationally
without waiting gives QNICE the previous address's value.

The device therefore delays only window-1 accesses:

```vhdl
if rising_edge(clk_sd_i) then
   ce_delayed <= qnice_ce and window_is_ram;
end if;

qnice_wait <= qnice_ce and window_is_ram and not ce_delayed;
```

Cycle sequence:

1. QNICE asserts chip enable and address.
2. Device asserts wait; block RAM registers the address.
3. RAM output becomes valid and `ce_delayed` rises.
4. Device releases wait; QNICE samples the zero-extended byte.

Window 0 remains combinational and must never inherit this wait.

## Coherent dumps

The secondary RAM port is non-intrusive, but the CPU may update different
locations while QMON walks a large range. For a coherent postmortem dump, the
host tool:

1. reads and saves `FFE0`;
2. writes back `FFE0 | 0002` to pause the emulated core;
3. selects device/window and dumps RAM;
4. restores the exact saved `FFE0` value in normal and exception paths.

Pausing the core is optional for individual live fields. It is required when
relationships between multiple RAM bytes matter.

Do not confuse pausing the emulated core with entering QMON. Entering QMON
parks the QNICE Shell itself. While parked, the Shell cannot service virtual
drive `sd_rd`/`sd_wr` requests, so running a disk command concurrently with a
monitor session causes expected read failures. Capture after the operation, or
build a hardware trace/counter that can be read afterward.

## Host helper structure

The removed `qnice_drive_monitor.py` implemented these reusable pieces:

- choose the highest numbered `/dev/ttyUSB*` by default;
- configure raw 115200 8N1 with nonblocking reads;
- send CTRL+E periodically until `QMON> ` appears;
- parse the `C R aaaa to return` banner;
- recover a monitor left in a partial command by completing/rejecting input;
- pace every transmitted character;
- wait for a prompt after each command;
- parse `ME` and `MD` output;
- save/pause/restore the core around coherent dumps;
- resume the Shell in a `finally`-style cleanup path where possible.

Useful operations were:

```text
peek DRIVE ADDRESS...      read selected DOS RAM bytes
dump DRIVE FIRST LAST      dump an inclusive DOS RAM range
raw WINDOW OFFSET COUNT    inspect arbitrary diagnostic words
watch DRIVE SECONDS        sample retained host request counters
cmd COMMAND...             send raw QMON commands
```

The `watch` operation could not truly observe a drive while QMON was active,
because QMON parks the Shell. It was useful only for values retained in
hardware across the operation. A future live monitor should use a standalone
QNICE task or hardware FIFO instead of QMON.

## Reimplementation checklist

1. Allocate an unused core device ID in `globals.vhd`.
2. Define a compact, versioned address map with a signature.
3. Capture pulse events in their source clock domains.
4. Add explicit CDC or document intentionally non-atomic mirrors.
5. Prefer an unused dual-port RAM port over CPU bus arbitration.
6. Return zero for unused addresses.
7. Assert `qnice_dev_wait_o` for registered or asynchronous backends.
8. Add the device case with safe combinational defaults.
9. Add a serial monitor trigger only if unattended entry is required.
10. Pace QMON characters and parse LF/CR robustly.
11. Save and restore pause state; never assume it was initially clear.
12. Test one-word reads, boundary addresses, full dumps and exception cleanup.
13. Never run a Shell-serviced peripheral operation while QMON owns the Shell.

