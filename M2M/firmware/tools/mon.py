#!/usr/bin/env python3
"""Host side of the firmware's serial monitor (fw/monitor.c), through the serial port of
the MEGA65 JTAG adapter. Reads and writes the C128's memory, the core's devices and the
system CPU's address space while the core runs.

  mon.py peek ADDR [N]       hex dump of N bytes (16 if no N)
  mon.py poke ADDR BYTE...   write bytes
  mon.py save ADDR N FILE    N bytes into FILE
  mon.py load FILE [ADDR]    FILE into memory; a .prg without ADDR goes to its own
                             load address in bank 0
  mon.py pause | resume      stop or restart the C128
  mon.py cmd LINE            one monitor command as typed (h lists them); ADDR
                             can be a name here too
  mon.py term                type monitor commands (Ctrl-D ends)

ADDR (hex): ram:1C01 (the C128's 256 KB, bank N at N0000), sysrom:, drvrom:, osm: (the
menu's characters), hr: (HyperRAM), DEV:WORD (word WORD of core device DEV), or a CPU
address such as 40000000. A byte of a device is the low byte of its 16-bit word.
"""
import argparse
import glob
import os
import select
import struct
import sys
import termios
import time

SPACES = {'ram': 0x0100, 'sysrom': 0x0101, 'drvrom': 0x0102, 'osm': 0x0000, 'hr': None}
HYPERRAM = 0x40000000
CSR = 0xE0000000
CSR_PAUSE = 0x0002
CHUNK = 32  # bytes per write command


class MonError(Exception):
    pass


class Addr:
    """A byte address: word WORD of a device, or a CPU address (dev None)."""

    def __init__(self, text):
        dev, sep, off = text.rpartition(':')
        try:
            if not sep:
                self.dev, self.off = None, int(off, 16)
            elif dev.lower() == 'hr':
                self.dev, self.off = None, HYPERRAM + int(off or '0', 16)
            elif dev.lower() in SPACES:
                self.dev, self.off = SPACES[dev.lower()], int(off or '0', 16)
            else:
                self.dev, self.off = int(dev, 16), int(off or '0', 16)
        except ValueError:
            raise MonError(f'address {text}: hex, DEV:WORD or one of {", ".join(SPACES)}')

    def plus(self, n):
        a = Addr.__new__(Addr)
        a.dev, a.off = self.dev, self.off + n
        return a

    def __str__(self):
        return f'{self.off:X}' if self.dev is None else f'{self.dev:X}:{self.off:X}'


class Monitor:
    def __init__(self, path, baud):
        self.path, self.baud = path, baud
        self.fd = os.open(path, os.O_RDWR | os.O_NOCTTY | os.O_NONBLOCK)
        iflag, oflag, cflag, lflag, ispeed, ospeed, cc = termios.tcgetattr(self.fd)
        cflag = termios.CS8 | termios.CREAD | termios.CLOCAL
        cc[termios.VMIN] = cc[termios.VTIME] = 0
        speed = getattr(termios, f'B{baud}', None)   # Linux has B2000000, macOS doesn't
        termios.tcsetattr(self.fd, termios.TCSANOW,
                          [0, 0, cflag, 0, speed or termios.B9600, speed or termios.B9600, cc])
        if speed is None:
            import fcntl
            fcntl.ioctl(self.fd, 0x80085402, struct.pack('L', baud))   # IOSSIOSPEED
        termios.tcflush(self.fd, termios.TCIOFLUSH)
        self.buf = b''

    def write(self, data):
        while data:
            select.select([], [self.fd], [])
            data = data[os.write(self.fd, data):]

    def read_until(self, end, idle=3.0):
        """Everything up to and including end; idle: seconds without a byte that fail."""
        while end not in self.buf:
            if not select.select([self.fd], [], [], idle)[0]:
                raise MonError(f'no answer from the monitor on {self.path} at {self.baud} baud'
                               ' (is a core with the RISC-V firmware running?)')
            self.buf += os.read(self.fd, 65536)
        i = self.buf.index(end) + len(end)
        data, self.buf = self.buf[:i], self.buf[i:]
        return data

    def sync(self):
        """Stops a read and drops a half-typed line, then waits for a prompt."""
        self.write(b'\x03')
        time.sleep(0.1)
        termios.tcflush(self.fd, termios.TCIFLUSH)
        self.buf = b''
        self.command('')

    def command(self, line):
        """The lines a command prints; MonError if it fails or isn't echoed as sent."""
        self.write(line.encode('ascii') + b'\r')
        lines = self.read_until(b'\r\n> ').decode('latin-1').split('\r\n')
        echo, body, status = lines[0], lines[1:-2], lines[-2]
        if echo != line:
            raise MonError(f'the monitor got {echo!r}, not {line!r}')
        if status != 'ok':
            raise MonError(f'{line}: {status.lstrip("? ")}')
        return body

    def read_bytes(self, addr, n):
        data = bytearray()
        for line in self.command(f'rb {addr} {n:X}'):
            data += bytes(int(v, 16) for v in line.split()[1:])
        if len(data) != n:
            raise MonError(f'{len(data)} bytes, not {n}')
        return bytes(data)

    def write_bytes(self, addr, data):
        for i in range(0, len(data), CHUNK):
            values = ' '.join(f'{b:X}' for b in data[i:i + CHUNK])
            self.command(f'wb {addr.plus(i)} {values}')

    def csr(self, set_bits=0, clear_bits=0):
        """Sets and clears bits of the CSR; returns what it was."""
        v = int(self.command(f'rw {CSR:X}')[0].split()[1], 16)
        self.command(f'ww {CSR:X} {(v | set_bits) & ~clear_bits:X}')
        return v


def named(line):
    """A command as typed, with a named ADDR made hex: rb ram:1C01 is rb 100:1C01."""
    t = line.split()
    if len(t) > 1 and t[1].partition(':')[0].lower() in SPACES:
        t[1] = str(Addr(t[1]))
    return ' '.join(t)


def hexdump(addr, data):
    for i in range(0, len(data), 16):
        row = data[i:i + 16]
        text = ''.join(chr(b) if 32 <= b < 127 else '.' for b in row)
        print(f'{str(addr.plus(i)):>12}  {" ".join(f"{b:02X}" for b in row):<47}  {text}')


def find_port():
    # The adapter's second channel is the serial port: ...1 on macOS, ttyUSB1 on Linux
    ports = sorted(glob.glob('/dev/cu.usbserial-*1')) or sorted(glob.glob('/dev/ttyUSB*'))[1:]
    if not ports:
        raise MonError('no JTAG adapter serial port found; name it with -p')
    return ports[0]


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('-p', '--port', help='serial port (default: the adapter\'s second channel)')
    ap.add_argument('-b', '--baud', type=int, default=2000000)
    ap.add_argument('what', choices=['peek', 'poke', 'save', 'load', 'pause', 'resume', 'cmd', 'term'])
    ap.add_argument('args', nargs='*')
    a = ap.parse_args()

    try:
        mon = Monitor(a.port or find_port(), a.baud)
        mon.sync()
        args = a.args
        if a.what == 'peek' and len(args) in (1, 2):
            addr = Addr(args[0])
            hexdump(addr, mon.read_bytes(addr, int(args[1], 16) if len(args) > 1 else 16))
        elif a.what == 'poke' and len(args) >= 2:
            mon.write_bytes(Addr(args[0]), bytes(int(v, 16) for v in args[1:]))
        elif a.what == 'save' and len(args) == 3:
            data = mon.read_bytes(Addr(args[0]), int(args[1], 16))
            with open(args[2], 'wb') as f:
                f.write(data)
            print(f'{len(data)} bytes from {args[0]} into {args[2]}')
        elif a.what == 'load' and len(args) in (1, 2):
            with open(args[0], 'rb') as f:
                data = f.read()
            if len(args) == 2:
                addr = Addr(args[1])
            elif args[0].lower().endswith('.prg') and len(data) >= 2:
                addr, data = Addr(f'ram:{data[0] | data[1] << 8:X}'), data[2:]
            else:
                raise MonError('load: an address, or a .prg file')
            mon.write_bytes(addr, data)
            print(f'{len(data)} bytes from {args[0]} at {addr}')
        elif a.what in ('pause', 'resume') and not args:
            on = a.what == 'pause'
            v = mon.csr(CSR_PAUSE if on else 0, 0 if on else CSR_PAUSE)
            print(f'C128 {"paused" if on else "running"} (CSR was {v:04X})')
        elif a.what == 'cmd' and args:
            for line in mon.command(named(' '.join(args))):
                print(line)
        elif a.what == 'term' and not args:
            while True:
                try:
                    line = input('mon> ')
                except EOFError:
                    print()
                    break
                try:
                    for out in mon.command(named(line)):
                        print(out)
                except MonError as e:
                    print(f'? {e}')
                except KeyboardInterrupt:   # stops a long read
                    print()
                    mon.sync()
        else:
            ap.error(f'{a.what}: wrong arguments')
    except (MonError, OSError) as e:
        sys.exit(f'mon.py: {e}')


if __name__ == '__main__':
    main()
