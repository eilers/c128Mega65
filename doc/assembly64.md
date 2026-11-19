# Assembly 64

The menu's **Assembly 64** line searches the [Assembly 64](https://hackerswithstyle.se)
database, as the Ultimate firmware's Assembly 64 search does: pick a title, pick one of its
files, and it is downloaded to the SD card and used. It needs the Ethernet port with an
address (the menu's Network line shows it); nothing else has to be on.

- `CORE/firmware/net/a64.c`: the server's four requests, on lwIP's raw API. One request at
  a time, moved on from the main loop.
- `CORE/firmware/a64_search.c`: the three screens, on the Shell's screen (`core_app_open`
  and `core_app_key` in `M2M/firmware/fw/core.h`).
- `CORE/firmware/net/rest_runners.c`: REST's `run_prg`, `load_prg` and `run_crt` without a
  request (`runners.h`), for a file's Run.
- `M2M/firmware/host/test_shell.c` walks the screens against a model of the server in
  `host_net.c`'s web server; `test_http.c` has the runner. `test_shell config_dump.txt
  fat32.img -live NAME` (not part of `make host-test`) runs the same screens against the
  real server through this Mac's network: a search for NAME, its first title, that title's
  first file into drive A.

## Using it

| Screen | Keys |
|---|---|
| The form: Name, Group, Handle, Event are typed; the lines below are picked | Up, Down. In a typed line: type, Del, Clr empties it, **Return searches**. On a pick line: Return lists its values, Left and Right (or - and +) step through them, Space is Any again. Return on Search. Run/Stop: the menu |
| The titles: `Name (Group, Year)` | Up, Down, Left and Right a page (as the file browser). Return: the title's files. Run/Stop: the form |
| A title's files, with their sizes | Return: what to do with the file. Run/Stop: the titles |
| A file's choices | D64, D71, D81: Mount in drive A, or B. PRG, P00: Run, or Load without RUN. CRT: Start as cartridge. Any file: Download to the SD card only |

Run/Stop also cancels a search or a download. The screens stay as they were left: after a
mount the menu comes back with the drive's line showing the image, and the Assembly 64 line
opens the title's files again, so the next disk of a title is two Returns.

A file goes to `/c128/a64/TITLE (ID-CATEGORY)/NAME` on the card and stays there. One that
is there already, with the size the server gives, is used without a download; so is one a
drive holds. A disk image is mounted as the menu's Disk line mounts it (the drive turns on,
its model follows the image). A program runs as REST's `run_prg` runs it: the C128 is reset,
a program for `$0801` starts in C64 mode, any other in C128 mode (`doc/rest.md`). A `.crt`
becomes the virtual cartridge, as the Cartridge submenu's CRT line.

## The server

- `GET /leet/search/aql/presets`, `/leet/search/aql/OFFSET/50?query=`,
  `/leet/search/entries/ID/CATEGORY` and `/leet/search/bin/ID/CATEGORY/N`, plain HTTP on
  port 80, with the headers `User-Agent: Assembly Query` and `Client-Id: Ultimate`.
- The server answers only clients it knows (error 464 otherwise). "Ultimate" is the one
  with the whole database, the C128 category included. With `Client-Id: Commodore` (the
  Commodore 64 Ultimate's CommoServe) it gives fewer pick lists and a small, licensed part
  of the titles.
- The query is `(name:"jumpman") & (category:games)`: typed values in quotes, rating as `>=`.
- A download cut short is discarded.

## Compared with the Ultimate

The requests and the form's fields are the Ultimate firmware's. The differences:

- The pick lists start with Category, Type and Date, then the others in the server's order,
  labelled with the server's descriptions ("Sort by").
- Return in a typed field searches.
- Titles come 50 at a time, and the next 50 follow when the cursor reaches the list's end,
  up to 500. The Ultimate shows the first 20.
- Downloads are files on the card and stay. The Ultimate keeps them in a RAM disk until
  power-off.
- No Run Disk (issue #31), no SID player, no tape or G64 images, no view of a text file or
  into a disk image: such files can be downloaded only.
- Left and Right page through a list, as in the file browser. On the Ultimate, Left goes
  back.

## Limits

- A JSON answer is at most 40 KB (a page of 50 titles is under 15 KB).
- A program is at most 64 KB, as REST's.

## Download speed

The server is far from most users (200 to 280 ms a round trip from the US), and TCP moves
one window a round trip. The server sends that window in one burst, faster than the
firmware takes frames from the MAC, so the burst has to fit the MAC's receive buffer.

- The MAC's receive buffer is 32 KB (`C_ETH_RX_BITS` in `CORE/vhdl/net/net.vhd`), and the
  core says so in `ETH_RXKB` (word 13 of the network device).
- The client's connection gets a window of the full frames that buffer holds, less one:
  20 segments (29,200 bytes). Every other connection keeps lwIP's two segments
  (`TCP_WND`): on a LAN the card is their limit, and the modem's is its baud rate. On a core
  with the old 4 KB buffer, `ETH_RXKB` reads 0 and the client uses two segments too.
- lwIP's window was one size for all connections; `tcp_rcv_wnd_max` (marked `c128Mega65`
  in `lwip/include/lwip/tcp.h` and `lwip/core/tcp.c`) gives one connection its own.
- What has arrived waits in a ring of one window in HyperRAM and goes to the card 4 KB at a
  time, so the window opens again while the card is written.
