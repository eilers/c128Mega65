# Video and audio streams

The core sends the VIC-II's picture and the sound over the Ethernet port as UDP packets, in
the format of Gideon's Ultimate 64 data streams, so viewers made for a U64 show the C128's
40-column screen and play its sound: the C64 Stream plugin for OBS, U64 Streamer, u64view,
the C64 Ultimate Toolbox. The 80-column (VDC) screen isn't streamed. The Toolbox's keyboard
types only in C64 mode (`doc/rest.md`, Clients).

## Starting a stream

- **Menu:** Network submenu, Video stream or Audio stream. Each goes to the target on its
  `to:` line, which you type as the host name is. The tick shows a stream that runs. Streams
  are never on after power-up.
- **REST:** `PUT /v1/streams/video:start?ip=TARGET`, `audio:start`, and `:stop`, as on a U64
  (`doc/rest.md`). An empty `ip=` uses the menu's target. REST is off by default: turn it
  on in the Network submenu first. Most viewers start the streams themselves this way.

A target is `host[:port]`: an address or a name, and a port (default 11000 for video, 11001
for audio). His defaults are multicast groups, `239.0.1.64:11000` and `239.0.1.65:11001`:
any number of viewers on the network can join them. A single address sends to that machine
only.

A start of a stream that runs sends it to the new target without a break, as his: its
sequence and frame numbers go on. A stop and a start count from 0 again.

## Format

The same as his U64 (his Data Streams page):

- Video: 780-byte packets, a 12-byte header (sequence, frame, line with bit 15 on the frame's
  last packet, 384 pixels, 4 lines, 4 bits a pixel, encoding 0) and 4 lines of VIC colour
  numbers, two pixels a byte. A PAL frame is 384x272 in 68 packets, 50 a second.
- Audio: 770-byte packets, a 2-byte sequence number and 192 stereo 16-bit samples,
  about 48 kHz (the C128's clock x 15/308 on PAL).

## Network

Video takes about 23 Mbit/s and audio about 1.7, a quarter of the 100 Mbit port. Use a
wired network: multicast over Wi-Fi goes at its slowest rate and floods it. Without IGMP
snooping a switch sends multicast to every port; send to one address instead if that is a
problem. The C64 Ultimate Toolbox draws a frame early when its packets pause for more than
100 ms, which Wi-Fi does often: the picture jumps down and lines stretch.

## How it works

The FPGA makes the packets (`CORE/vhdl/net`); the firmware (`CORE/firmware/net/stream.c`,
his `data_streamer.cc`) finds the target's MAC (a group's or the broadcast address's from
the address itself, else by ARP, the gateway's for an address off the subnet), writes each
stream's Ethernet, IP and UDP header into the network device and turns the stream on.
