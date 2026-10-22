# Roadmap

The current release line is 1.0. This is the order of work after it.
Defects stay in [doc/KNOWN_BUGS.md](doc/KNOWN_BUGS.md).

## 1.1

### Virtual cartridges

A `.crt` file from the SD card mounts the way a disk image does and appears to the C128 as a cartridge. A cartridge plugged into the expansion port keeps working as it does today.

### REU

An REU (RAM Expansion Unit) of 512 KB or 2 MB. At 512 KB it matches the Commodore 1750, so C128 and C64 software that expects a 1750 finds it; 2 MB serves newer software. It is MiSTer's REU, in HyperRAM; `test/reu/` has a C128 program that tests it.

The REU is off by default. When switched on it works together with a cartridge in the slot or a virtual cartridge. [doc/hyperram-map.md](doc/hyperram-map.md) has the memory layout and the reasons.

## After 1.1

No version is assigned to this yet.

Planned:

### Real disk drives

A physical 1541, 1571, or 1581 on the Commodore serial port, tested on its own and together with the virtual drives. The Help menu can already enable that port.

### Larger REUs

REUs larger than 2 MB, up to 16 MB, in the SDRAM of R4, R5 and R6 boards. They don't fit in HyperRAM beside a virtual cartridge.
