# SwiftLink Modem

The core can emulate a SwiftLink RS-232 cartridge with an internet modem behind it, on
the MEGA65's Ethernet port. Terminal programs such as CCGMS see a SwiftLink and a Hayes
modem: they dial a BBS with `ATDT host:port`, and XMODEM and Punter transfers run over
the connection. Callers can also reach the C128 on TCP port 6400, so it can run a BBS.

## Using it

1. Connect the MEGA65 to a network with DHCP. The modem is part of the system firmware
   (`/c128/firmware.bin`); it needs no file of its own.
2. In the menu, set **Modem** to the address your program expects: `$DE00` is
   SwiftLink's usual one. The modem uses NMI, as a SwiftLink does.
3. In the terminal program, pick SwiftLink at that address, at up to 38400 baud.

| Command            | What it does                                                                     |
|--------------------|----------------------------------------------------------------------------------|
| `ATDT host:port`   | Dial. A host name or IP address; the port defaults to 23. Quotes are fine (`ATD"host:port"`). |
| `ATA`              | Answer a caller (the modem says `RING` every 2 seconds)                          |
| `ATS0=n`           | Answer by itself after n rings; 0 = never (the default)                          |
| `+++`              | Back to commands while staying connected (one second of quiet before and after)  |
| `ATO`              | Back online                                                                      |
| `ATH`              | Hang up. Dropping DTR does the same, unless `AT&D0`.                             |
| `ATI`, `ATI1`, `ATI2` | Version; network status (MAC, IP, gateway, DNS, counters); the network's debug log |
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
C128 bus ── 6551 (acia6551) ── 64-byte FIFOs ──┐
              main_clk                          ├── net.vhd, device 0x010B ── system CPU:
Ethernet PHY ── RMII MAC (eth_wrapper) ─────────┘   modem_clk, 50 MHz           lwIP, the modem
```

- **The 6551** (`CORE/vhdl/net/acia6551.vhd`) follows the Ultimate's `acia6551.vhd`,
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
- **The modem** runs in the RISC-V system firmware (`CORE/firmware/net`): lwIP 2.2.1 and
  a Hayes command set. The command set follows the Ultimate's `modem.cc`; telnet
  negotiation is new. The Ultimate relays raw TCP, which corrupts XMODEM blocks containing
  `$FF` on BBSes that speak the telnet protocol.
- **The Ethernet MAC** is C64MEGA65's `eth_rmii.vhd` (`M2M/vhdl/controllers/ethernet`),
  with a transmit frame buffer added to its `eth_wrapper.vhd`. It stays in reset until the
  firmware starts the network.

### Turns

The network gets a turn in every round of the firmware's main loop (`core_net`): up to
four frames from the MAC into lwIP, lwIP's timers, then the modem moves bytes between the
FIFOs and TCP. Long disk work, a big UCI copy or a folder the menu draws, also gives it a
turn between sectors (`core_idle`, at most every millisecond), so the FIFOs and the MAC's
buffer don't wait for the round to end. lwIP's callbacks only change the network's own
state, so a turn in the middle of a FatFs call is safe; a turn never runs inside another.
`ATI1`'s `LONGEST WAIT` is the longest time without a turn since the last `ATI1`.

**No big receive buffer.** The FIFOs only cover the gaps between turns (64 bytes is 16.7 ms
at 38400 baud). When the C128 falls behind, for example while it writes an XMODEM block to
disk, it drops RTS. The FIFO fills, the firmware stops passing bytes on, the TCP window
closes, and the sender waits. The data waits on the server, so nothing needs to buffer it
here. XMODEM itself never has more than one block in flight. A 2-segment TCP window covers
a round trip of up to about 500 ms at 57600 baud, and fits in the MAC's 4 KB. What has
arrived waits in a ring of one window (in HyperRAM), not in lwIP's buffers: a far end
sending small segments would otherwise empty the pool that every other frame needs.

**Failures.** A trap (a bus error, say) stops the whole firmware on its trap screen, and so
does a main loop stuck for a second (the watchdog, marked `stuck`, with the address it was
stuck at). The core needs a reset then (a long press). `AT*T` and `AT*W` test the two.

### Resources

lwIP, sized for the modem, UCI's network target and the REST server, takes about 28 KB of
the system CPU's 64 KB data RAM. The MAC's frame buffers (4 + 2 KB) and the 6551's FIFOs
are LUTRAM.

**Short HyperRAM bursts.** HDMI's scaler keeps its frame buffer in HyperRAM and takes
turns with the core, and it only has room for two bursts of the incoming picture. So the
system CPU's bridge (`rv_hr_bridge.vhd`) reads a 64-byte cache line in two 16-word bursts.
A 64-word burst made the scaler miss its deadlines on the 80-column screen, and HDMI
showed sparkles (VGA doesn't go through the scaler).

### Registers

16-bit words of device `0x010B` (`C_DEV_NET`) on the system CPU's device bus;
`CORE/firmware/net/net_hw.h` and the header of `CORE/vhdl/net/net.vhd` list them: the
6551's FIFOs, lines and events, the MAC's receive and transmit bytes and status, and the
FPGA's device DNA.

The MAC address is `02:C1:28:xx:xx:xx`, with the last three bytes from the FPGA's DNA,
so every MEGA65 gets its own.

## Testing

- `CORE/sim/tb_net.vhd` (`CORE/scripts/run_net_nvc.sh`, nvc, a few seconds): a firmware
  model on the device bus reads the DNA, sends two frames through an RMII loopback and
  reads them back, then echoes the C128's bytes. A C128 bus model programs the 6551 and
  plays CCGMS's start (DTR on with the receive IRQ off, then on), a status read that clears
  the IRQ before the NMI is taken, DTR off, a C128 reset in the middle of a line, and the
  modem switched off.
- `M2M/firmware/host/test_modem.c` (`make -C M2M/firmware host-test`): the real `modem.c`
  and lwIP on the Mac, against a C128 that sends and reads at 38400 baud through 64-byte
  FIFOs and a server behind a 4 KB MAC buffer: AT commands, uploads (HTTP, bulk, telnet),
  pkg-style downloads with disk pauses, and both with the firmware stalling 66 ms out of
  every 250.
- CCGMS Future (github.com/mist64/ccgmsterm) has an automated transfer test: `test/xfer.c`
  behind socat on a PC drives the terminal through every XMODEM variant and Punter. Its
  regular build supports SwiftLink at `$DE00`, `$DF00` and `$D700`. The EasyFlash build
  leaves SwiftLink out. See `test/modem/README.md`.
