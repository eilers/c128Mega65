# SwiftLink Modem

The core can emulate a SwiftLink RS-232 cartridge with an internet modem behind it, on
the MEGA65's Ethernet port. Terminal programs such as CCGMS see a SwiftLink and a Hayes
modem: they dial a BBS with `ATDT host:port`, and XMODEM and Punter transfers run over
the connection. Callers can also reach the C128 on TCP port 6400, so it can run a BBS.

## Using it

1. Put `modem.bin` (from `sdcard/c128/`) on the SD card as `/c128/modem.bin`.
2. Connect the MEGA65 to a network with DHCP.
3. In the menu, set **Modem** to the address your program expects: `$DE00` is
   SwiftLink's usual one. The modem uses NMI, as a SwiftLink does.
4. In the terminal program, pick SwiftLink at that address, at up to 38400 baud.

| Command            | What it does                                                                     |
|--------------------|----------------------------------------------------------------------------------|
| `ATDT host:port`   | Dial. A host name or IP address; the port defaults to 23. Quotes are fine (`ATD"host:port"`). |
| `ATA`              | Answer a caller (the modem says `RING` every 2 seconds)                          |
| `ATS0=n`           | Answer by itself after n rings; 0 = never (the default)                          |
| `+++`              | Back to commands while staying connected (one second of quiet before and after)  |
| `ATO`              | Back online                                                                      |
| `ATH`              | Hang up. Dropping DTR does the same, unless `AT&D0`.                             |
| `ATI`, `ATI1`, `ATI2` | Version; network status (MAC, IP, gateway, DNS); the modem's debug log       |
| `ATZ`, `AT&F`      | Back to the defaults                                                             |
| `ATE`, `ATV`, `ATQ`, `AT&C` | Echo, word or number results, quiet, DCD always on (`&C0`) or with the carrier |
| `ATS50=n`          | Telnet: 0 = use it when the far end does or the port is 23 (default), 1 = never, 2 = always |

Settings survive a C128 reset (a short press of the reset button) and last until the
MEGA65 is switched off or reset with a long press; the modem has nothing to save them to.

**Addresses.** `$DE00` and `$DF00` are cartridge I/O, and a cartridge that uses the same
address collides with the modem, as a real SwiftLink would. At `$DF00` the core's REU is
off, because its registers are there too. `$D700` is the C128's spare I/O select, free
in C128 and C64 mode; CCGMS knows it as `D7`.

**Speeds.** All of SwiftLink's rates up to 38400, and 57600 to 230400 through the
Turbo232 speed register at base+7. Bytes reach the C128 at the rate it set, so a
terminal program sees the timing of a real line.

## How it is built

```
C128 bus ── 6551 (acia6551) ── 64-byte FIFOs ── NEORV32 ── RMII MAC ── Ethernet PHY
              main_clk              │            modem_clk (50 MHz)
                                    └── firmware from HyperRAM (hr_clk)
```

- **The 6551** (`CORE/vhdl/modem/acia6551.vhd`) follows the Ultimate's `acia6551.vhd`,
  whose author measured a real 6551: DTR gates the receiver and the IRQs, the transmit
  IRQ is a level, a status read clears the IRQ, a status write is the programmed reset.
  Unlike a 6551 it never overruns: the next byte waits until the C128 has read the last
  one. RTS (command bits 2-3) pauses the incoming data, which is how terminal programs
  hold the modem off. With DTR off the receiver is off and incoming bytes are lost, as on
  a 6551. A byte that arrives while the receive IRQ is off interrupts when the IRQ comes
  on; a 6551 would interrupt on the overrun instead. CCGMS needs both: it turns DTR on
  before it installs its NMI handler, and keeps the receive IRQ off while it starts up.
  A byte still unread a character time later interrupts again, as the next byte would on
  a 6551. CCGMS's send routine reads the status, which clears the IRQ; when that read
  lands just before the CPU samples NMI, the NMI is lost, and without the repeat the
  receiver would stay full for good. `ATI1` counts these as `MISSED NMIS`.
- **A C128 reset** resets the 6551 and makes the modem hang up and drop anything it was
  still sending. Its settings stay, as on an external modem.
- **The modem** is a NEORV32 RISC-V (`CORE/vhdl/neorv32`, v1.13.6) running lwIP 2.2.1
  and a Hayes command set (`CORE/modem-fw`). The command set follows the Ultimate's
  `modem.cc`; telnet negotiation is new. The Ultimate relays raw TCP, which corrupts
  XMODEM blocks containing `$FF` on BBSes that speak the telnet protocol.
- **The Ethernet MAC** is C64MEGA65's `eth_rmii.vhd` (`M2M/vhdl/controllers/ethernet`),
  with a transmit frame buffer added to its `eth_wrapper.vhd`.

### Block RAM

The modem was laid out to use as little block RAM as possible: 6.5 RAMB36 tiles (R6
build: 292 of 365, from 285.5 without the modem), and about 6,100 LUTs.

| What                         | Tiles | Why                                                              |
|------------------------------|-------|------------------------------------------------------------------|
| CPU data RAM, 16 KB          | 4     | lwIP's buffers and the stack (13.5 KB used)                      |
| Instruction cache, 8 KB      | 2.5   | four byte lanes of a RAMB18 each, and the tags; the firmware itself, 52 KB, stays in HyperRAM |
| No data cache                | 0     | NEORV32's would take 2 tiles at any size up to 8 KB; constants come from HyperRAM a word at a time |
| Register file, boot code     | 0     | flip-flops and LUTs; NEORV32's RAM styles took 1.5 tiles          |
| MAC frame buffers, 4 + 2 KB  | 0     | LUTRAM (Vivado can't put `axis_dropper`'s buffer in block RAM)   |
| 6551 FIFOs, 64 + 64 bytes    | 0     | LUTRAM                                                           |

Running the firmware from block RAM instead would have cost 13 more tiles. From
HyperRAM it also updates without a new core: copy a new `modem.bin` and reset.

**No big receive buffer.** The FIFOs only cover the CPU's gaps (64 bytes is 11 ms at
57600 baud). When the C128 falls behind, for example while it writes an XMODEM block to
disk, it drops RTS. The FIFO fills, the firmware stops taking data from lwIP, the TCP
window closes, and the sender waits. The data waits on the server, so nothing needs to
buffer it here. XMODEM itself never has more than one block in flight. A 2-segment TCP
window covers a round trip of up to about 500 ms at 57600 baud.

### Booting

The Shell loads `/c128/modem.bin` into HyperRAM at startup (`0x300`–`0x3FE`, see
`hyperram-map.md`), one byte per 16-bit word. The CPU is held in reset while the menu
has the modem off. Once released, it starts in its boot code (`CORE/modem-fw/boot/boot.S`,
104 bytes, served by `modem.vhd` at `0xF0003000`), which checks the image's header and
checksum every 100 ms and jumps to it once both are right. Without the file the modem stays silent, and the 6551 still answers.

A trap (a bus error, say) or a main loop stuck for a second restarts the firmware. The
C128 sees `MODEM RESTARTED` with the cause, the call is dropped, and the settings go back
to the defaults. `ATI2`'s log keeps what came before, and `ATI1` counts the restarts.

### Addresses

| CPU address    | What                                             |
|----------------|--------------------------------------------------|
| `0x40000000`   | firmware in HyperRAM, cached                     |
| `0x80000000`   | 16 KB data RAM                                   |
| `0xF0000000`   | 6551 FIFOs and lines (`CORE/modem-fw/src/hw.h`)  |
| `0xF0001000`   | Ethernet MAC                                     |
| `0xF0002000`   | FPGA device DNA, for the MAC address             |
| `0xF0003000`   | boot code, where the CPU starts                  |

The MAC address is `02:C1:28:xx:xx:xx`, with the last three bytes from the FPGA's DNA,
so every MEGA65 gets its own.

## Testing

- `CORE/sim/tb_modem.vhd` (`CORE/scripts/run_modem_sim.tcl`): the real firmware boots
  from a HyperRAM model; the testbench programs the 6551, types AT commands and checks
  the echo, `OK`, `ATI`, `ERROR`, `NO DIALTONE` and the 6551's IRQ. It also plays
  CCGMS's start (DTR on with the receive IRQ off, then on), a status read that clears
  the IRQ before the NMI is taken, DTR off, a C128 reset in the middle of an answer, and
  restarts after a trap and after a HyperRAM read that takes too long.
- CCGMS Future (github.com/mist64/ccgmsterm) has an automated transfer test: `test/xfer.c`
  behind socat on a PC drives the terminal through every XMODEM variant and Punter. Its
  regular build supports SwiftLink at `$DE00`, `$DF00` and `$D700`. The EasyFlash build
  leaves SwiftLink out.
