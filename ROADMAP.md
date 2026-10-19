# Roadmap

The current release line is 1.0. This is the order of work after it.
Defects stay in [doc/KNOWN_BUGS.md](doc/KNOWN_BUGS.md).

## 1.1

### Virtual cartridges

A `.crt` file from the SD card mounts the way a disk image does and appears to the C128 as a cartridge. A cartridge plugged into the expansion port keeps working as it does today.

### REU

An REU (RAM Expansion Unit) compatible with the Commodore 1750, 512 KB. C128 and C64 software that expects a 1750 finds it. The MiSTer core already contains this device; the MEGA65 port still has to connect it.

## After 1.1

No version is assigned to this yet.

Planned:

### Real disk drives

A physical 1541, 1571, or 1581 on the Commodore serial port, tested on its own and together with the virtual drives. The Help menu can already enable that port.
