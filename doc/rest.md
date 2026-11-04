# REST API

The HTTP API of Gideon's Ultimate firmware, on port 80 of the MEGA65's Ethernet port, so
tools written for an Ultimate II+ or an Ultimate 64 can drive the C128. It answers as his
Ultimate 64 firmware does (the core is a whole machine, as a U64, not a cartridge), byte for
byte, because tools parse his answers. Where it differs, "Not his" below says so.

- `CORE/firmware/net/http.c`: the server, a port of MicroHttpServer (Gideon's
  `software/httpd` at `ad73d32`) to lwIP's raw API. Its rules: four connections and two
  more waiting, a header of up to 2048 bytes and 20 fields, a body only on POST
  (Content-Length or chunked; with neither it is taken as empty, as his),
  `Connection: close`, and a connection reaped after 15 s without traffic. Requests run from
  the main loop, so a route may use the card; a route's answer can take several rounds
  (readmem's DMA). Its buffers are in HyperRAM. `net/multipart.c` splits a POST's body as
  his `multipart.c` does.
- `CORE/firmware/net/rest.c`: the API, from Gideon's `software/api` at `4b00732` (his 3.15a,
  on Jeff's II+L, has the same core): the URL split, the arguments and their checks,
  X-Password, his JSON layout (no escaping, `{ \n`, `[  ]`) and status texts (any code but
  200, 203, 400, 403 and 404 goes out as `Unknown`), and a bare 404 for anything that isn't a
  route. A JSON answer starts in a 64 KB buffer that doubles as it needs (buffers left behind
  are reused, the heap never frees), as long as the heap keeps 512 KB.

## Routes

| Route | Answer |
|---|---|
| `GET /v1/version` | `0.1`, as his |
| `GET /v1/info` | product `C128MEGA65` (a name, as his `Ultimate II+L`), firmware version = config.vhd's `CORE_VERSION`, the firmware's git hash, `fpga_version` "1" and the hardware interface's number in two hex digits (`108`), `core_version` "1." and the same number (`1.08`; his U64's form), host name, `unique_id` (24 bits from the FPGA's DNA, as his from his flash's serial), the Ethernet MAC. With `core_version` his tests and clients take it for a U64; no `wifi_mac` (no Wi-Fi) |
| `GET /v1/help?command=` | his fixed page |
| `PUT /v1/machine:reset`, `:reboot` | the reset button, as UCI's REBOOT; a pause ends with it |
| `PUT /v1/machine:pause`, `:resume` | the C128's CPU held by bus DMA, as his `stop()`; answered at once |
| `GET /v1/machine:readmem?address=&length=` | memory as the C128's CPU sees it then (the MMU's banks, ROMs, I/O), read by bus DMA with the CPU held for the whole request; 1 µs a byte, 64 KB in about 70 ms |
| `PUT /v1/machine:writemem?address=&data=`, `POST` | the same, written: up to 128 bytes in the URL, or a POST's body (raw, or a multipart file part: the first part, up to 64 KB) |
| `PUT /v1/machine:menu_button` | opens the On-Screen-Menu, or closes it (not while a file is being picked) |
| `GET /v1/machine:vdcmem?address=&length=` | readmem's form and answers, for the VDC's 64 KB: the bytes a program reads through R18/R19 and R31 (16K or 64K, as R28 bit 4 has it), without moving the update address. No bus DMA: the C128 keeps running, also while the Z80 runs (CP/M's screen reads; readmem answers 423 then). 0.7 µs a byte, 64 KB in about 50 ms; sending 64 KB takes about 0.4 s, as readmem's. Pause first for a still snapshot |
| `GET /v1/machine:vdcregs` | 40 bytes: R0-R37 as a program reads them, then the status byte ($D600) and the selected register, without a read's side effects (R31 moves the update address, R16/R17 clear the light pen flag) |
| `GET /v1/machine:menu_screen` | 404 while the menu is closed; else 2000 bytes: the OSM window from its top left as 40 × 25 C64 screen codes (lower-case set), then their colours |
| `PUT /v1/machine:poweroff` | 501, his answer where the hardware can't: the MEGA65 can't switch itself off |
| `GET /v1/machine:measure` | 501, as his U2: no bus measurement |
| `GET /v1/machine:debugreg`, `PUT ?value=` | his U64's debug register at $D7FF, two hex digits: here a plain byte the C128 and REST both read and write (a test program can leave its result there). In C64 mode the C128 writes it but reads the SID mirror there; REST reads it in both modes |
| `GET /v1/machine:input`, `POST` | his U64's keyboard, joystick and mouse input, below |
| `PUT /v1/streams/S:start?ip=`, `:stop` | his video and audio streams (`doc/streams.md`): `ip` is `host[:port]`, empty for the Data Streams store's target; answered once the target is resolved, with his texts and codes (404 "Network Host Resolve Error" for a name or MAC not found, 400 "Invalid Parameter" for port 0). `debug`: start 501 (no debug stream), stop 200. An unknown name his 404 |
| `GET /v1/machine:heap` | the firmware's HyperRAM heap: it never frees, so `min_ever_free` is `free` |
| `GET /v1/configs[/CATEGORY[/ITEM]]` | his stores and items that the core has (below), with his names and patterns |
| `PUT /v1/configs/CATEGORY/ITEM?value=` or `/VALUE` | one item, in effect at once, in memory |
| `POST /v1/configs` | a JSON body of categories and items, each tried |
| `PUT /v1/configs[/CATEGORY]:save_to_flash`, `:load_from_flash`, `:reset_to_default` | once a matching category has changes, `/c128/settings.cfg`, written whole (`written` names the changed categories); back from it (the defaults for what it doesn't name); the defaults |
| `GET /v1/drives` | `a` and `b`: power, bus id (8-11, its submenu's), what it runs as, `rom` the ROM picked for its model (his default name, `1571.rom`, for the stock one; `Unable to load!` for a file that won't), the image as his: a REST mount shows the path it was given as `image_file` with an empty `image_path`, a menu or UCI mount its folder and name. The internal floppy, when it is a drive's model, is that drive: a 1581 with no image. No `softiec` |
| `PUT /v1/drives/D:mount?image=&type=&mode=`, `POST` | the image into the drive, answered once a 1541/1571 has it (as UCI's MOUNT_DISK). `type` (default: the extension) only picks his command and Ftype; the core takes D64, D71 and D81 by their size, G64/G71 answer 501. `readonly` write protects the disk. `unlinked` mounts a copy in `/c128/temp`: writes go to the copy, the file stays as it was. On the internal floppy's drive the floppy is turned off first, as the menu does. POST: the body goes to `/c128/temp/NAME` (NAME from the part's file name, `temp0000` and up without one; `_1`, `_2` while that name is in a drive) |
| `PUT /v1/drives/D:reset`, `:on`, `:off`, `:remove` | the drive's ROM reloaded (a reset), its power, the image out |
| `PUT /v1/drives/D:unlink` | the drive moves onto a copy of its image in `/c128/temp` without a disk change: its requests wait (vd_sd's hold) while its changes go to the card and the copy is made, then its table points at the copy. GET drives still shows the file it came from |
| `PUT /v1/drives/D:load_rom?file=`, `POST` | 16 or 32 KB into the drive's bank of `boot1.rom` for its model (a 16 KB image twice), then the bank reloaded into the drive, which resets it; another size is written anyway and answers his 412. Lasts until the drive runs another model (then the bank gets its configured ROM again, as his set_drive_type loads it) or a long reset loads `boot1.rom` again |
| `PUT /v1/drives/D:set_mode?mode=` | 1541, 1571 or 1581: the drive's model, as his set_drive_type and its config; the internal floppy's drive stays it as a 1581. A model its image can't run ejects the image, as the menu does. A mount moves the model to one its image runs: a D81 a 1581, a D71 a 1571, a D64 in a 1581 a 1571 |
| `GET /v1/files/PATH:info` | his name, size and extension of a file on the card (`SD/...`) |
| `PUT /v1/files/PATH:create_d64?tracks=&diskname=`, `:create_d71`, `:create_d81` | zeros, then his `format()`: the BAM and the first directory block (byte for byte his, `$20` in a D64's byte 3 included). `create_dnp` checks his arguments, then 501 |
| `PUT /v1/runners:load_prg?file=`, `:run_prg`, `POST` | a program for `$0801` runs in C64 mode, any other in C128 mode: the C128 reset, BASIC's READY awaited, GO64 typed for C64 mode, the program and his loader's pointers by DMA with the CPU held, the lines relinked (his runner's LINKPRG), RUN typed for run_prg. A `.cfg` (or `.usr`) next to the file is loaded first, as his: a line with an error is skipped, the rest take effect until the next start (`settings.cfg` keeps what it had for them unless they change again). A P00's header is skipped. 423 while the Z80 runs, when C64 BASIC comes up for a C128 program (a C64 cartridge), or when READY doesn't come within 10 s (a cartridge, a boot or CP/M disk) |
| `PUT /v1/runners:run_crt?file=`, `POST` | the `.crt` as the menu's CRT: line: the cartridge's source becomes the virtual cartridge, main.vhd resets the C128 into it; an upload goes straight into the cartridge's memory. Too large: 412, not a `.crt` the parser takes: 415 |
| `PUT /v1/runners:sidplay`, `:modplay`, `POST` | 501: modplay as his U2 without a sampler ("Sampler module not available"); sidplay with his "not supported on this architecture": no player here, where his U2 has one |

A request that takes the bus waits
for another one's; the Z80 takes no DMA, so readmem and writemem answer 423 (his "Could not
obtain lock of subsystem") while it runs. The bus DMA is `CORE/vhdl/c128_dma.vhd`: jobs of
up to 2 KB through its own buffer, after a cartridge and the REU on the core's DMA port.

### The stores

The same table LOAD_CONFIG uses (`CORE/firmware/stores.c`): his names and types; the
choices are the core's, under his names where he has the same.

| Category | Items |
|---|---|
| U64 Specific Settings | his U64's, for the menu's lines: System Mode (PAL, the core's only), HDMI Scan Resolution (the menu's HDMI modes), Joystick Swapper (Normal, Swapped), Digital Video Mode (HDMI, DVI: the menu's HDMI audio on or off), HDMI Scan lines (Enabled while the HDMI filter has scan lines: Scanlines or a CRT one; set Enabled, it picks Scanlines for a filter without them, set Disabled the default filter for one with them), Serial Bus Mode (All Connected: the IEC port on; C64 <-> Internal: off); and his for machine:input's mouse, with no menu lines: Mouse Mode (Cursor, Mouse, Mouse + Cursor, Mouse + Wheel), Mouse Sensitivity (1-16), Mouse Acceleration (Off, Adaptive), Mouse Wheel Sensitivity (1-16), Mouse Wheel Direction (Normal, Reversed) |
| C64 and Cartridge Settings | Cartridge (a `.crt` in `/c128/carts`, or a card path `/SD/...`, loaded at every start), Cartridge Preference (External: the slot; Internal: a `.crt` file), Alternate Kernal (8 KB, or 16 KB with C64 BASIC), RAM Expansion Unit, REU Size (512 KB, 2 MB, 16 MB; 512 KB on R3), REU Preload Image (a card path, `/SD/...`, into the REU at every start), Command Interface |
| Drive A Settings, Drive B Settings | Drive (its power), Drive Type (1541, 1571, 1581, Internal 1581: its model, as set_mode; the internal floppy one drive at a time), Drive Bus ID (8-11; two drives may share one, as his; the menu swaps them instead), ROM for 1541 mode (8, 16 or 32 KB; 8 KB is its $E000 half, on the stock $C000 half), 1571 mode, 1581 mode (16 or 32 KB) |
| Modem Settings | Modem Interface, ACIA (6551) Mapping (Off, DE00/NMI, DF00/NMI, D700/NMI), Hardware Mode |
| Ethernet Settings | Use DHCP, Static IP, Static Netmask, Static Gateway, Static DNS |
| Network Settings | Host Name, Network Password, FTP File Service (`doc/ftp.md`), Web Remote Control Service |
| Data Streams | Stream VIC to, Stream Audio to (`host[:port]`, 36 characters; his defaults `239.0.1.64:11000`, `239.0.1.65:11001`). A change takes effect at the stream's next start, as his |
| C128 ROM Settings (ours) | C128 Kernal ROM (16 KB: editor, Z80 BIOS, KERNAL), C128 Basic ROM (32 KB), C64 Basic ROM (8 KB), Char ROM (8 KB), Internal Function ROM (16 or 32 KB) |
| MEGA65 Settings (ours) | the menu's settings he has nothing like, its words for the choices: Video Output, VIC-II Jailbars, HDMI Flicker-free, HDMI Filter, VGA Display Mode, Caps Lock Key, Help Menu on F13 |

The ROM items are his CFG_TYPE_STRFUNC: a file name in `/c128/roms` (his `/flash/roms`),
or ours only, a card path (`/SD/...`) for a file the menu picked elsewhere; GET with an item
in full lists his `presets` (the files of the right size in `/c128/roms`). Empty, or a
drive's default name with no such file, is the stock ROM. A system ROM changes with a C128
reset (it isn't written under a running CPU), a drive's with a reload of the drive if it
runs that model. The cartridge loads at the next start.

Every item is kept in `/c128/settings.cfg`, his settings format; drive power only once it
was set here or by LOAD_CONFIG.

A change made here stays in memory until `save_to_flash`, unless the menu saves first: it
writes `settings.cfg` whole when it closes after a change, when a ROM or the REU's file is
picked or cleared, or when a network setting is typed. Unsaved REST changes go with it.

The routes are in `net/rest_machine.c`, `rest_configs.c`, `rest_drives.c`, `rest_files.c`,
`rest_runners.c` and `rest_input.c`.

### machine:input

His U64's input API (`route_input.cc`, `input_api.h` at `4b00732`), at the hardware level, so
it works in every mode and program: BASIC, games that scan the matrix, GEOS, CP/M. A POST is
a batch of up to 64 events, checked whole before any is applied (his error texts); the answer
is what is held then. `net/rest_input.c` has the routes, `input.c` the keys and joysticks
(his `Keyboard_USB` and `JoystickOutput`), `mouse.c` the mouse (his report queue and USB mouse
handling); the hardware is `CORE/vhdl/input_inject.vhd`.

- Keys: his names, by their place in the C64 matrix, joined to the same lines as the MEGA65
  keyboard's (which stays live). `press` holds, `release` lets go, `tap` holds for 80 ms and
  leaves 20 ms before the next tap (4 + 1 of his 20 ms ticks: a key every 100 ms), or 80 ms
  before a tap with a key of the one before (the same letter twice, Shift in both). `restore`
  only taps, an NMI edge, with RUN/STOP or C= in the same event for their combinations.
- The C128's own keys (ours): `help`, `esc`, `alt`, `tab`, `line_feed`, `no_scroll`,
  `kp_0`-`kp_9`, `kp_plus`, `kp_minus`, `kp_period`, `kp_enter`, the top-row `up`, `down`,
  `left`, `right` (K0-K2), and `40_80` and `caps_lock`, which lock: `press` engages, `release`
  lets go. They come after his names in the answer's list.
- Joysticks: `port` 1 and 2 are the C128's control ports (after Swap joystick ports). `fire2`
  and `fire3` are POTX and POTY, as his.
- Mouse: a 1351 on port 1 (POT lines, left button = fire, right = up, middle = down), from
  the first mouse event until `release_all` or a reset. Reports go out at most every 20 ms,
  paced so a driver never sees the pointer jump back (his MousePotPacer); 1024 reports queue,
  a batch that needs more is a 429. The U64 Specific Settings' Mouse items apply, as his.
- While the menu is open, keys go to it instead: cursor keys (Shift for up and left, or the
  top-row ones), Return, Space, RUN/STOP, Help, F1, F3, and typing in text lines. The C128 sees
  none of them, as with the MEGA65 keyboard. What the menu held is dropped when it closes.
- `release_all`, `machine:reset` and `machine:reboot` let go of everything and detach the
  mouse.

### How run_prg finds READY

The firmware resets the C128 with `$0A00` and `$2D` cleared by DMA first (the CPU held until
the reset has happened, so old values can't be taken for new ones). C128 BASIC is ready once
`$0A00` (the system vector) is 3, the end of its cold start; C64 BASIC once `$2B`-`$2E` are
`01 08 03 08` (its NEW). Then a NUL goes into the keyboard buffer: the editor takes it, and
prints nothing, only once it waits at READY, after the disk boot. Checked in VICE with the
core's own ROMs, 40 and 80 columns.

### Bitstream

Hardware interface 4: `C_DEV_DRIVE_ROM`'s CSR (register 0: R bit i drive i has its ROM, W
bit i reloads drive i's bank, its `iecdrv_rom` and the ROM server starting over) and vd_sd's
hold (CSR `0x035`). iec_drive's per-drive `rom_reload` is in the C128_MiSTer submodule.

Hardware interface 5: the internal function ROM in `C_DEV_SYSTEM_ROM` from `0x18000`
(boot0.rom's BRAM has 128 KB), its size in that device's CSR register 0 (0 none, 1 16 KB,
2 32 KB; a missing half floats, as on MiSTer). The firmware writes a 16 KB function ROM twice,
as VICE and a 16 KB EPROM in the C128's 32 KB socket have it, so it uses 0 and 2.

Hardware interface 11: `C_DEV_INPUT` (`input_inject.vhd`, register map in its header): the
keys by matrix position (C64 rows, K0-K2), RESTORE, 40/80, Caps Lock, both ports' lines and
POT values, sent to the C128 together by a commit; and the debug register at $D7FF.

Hardware interface 12: his video and audio streams in `C_DEV_NET` (words `0x20`-`0x7F`,
`net/net_hw.h`), and the Network submenu's stream lines.

Hardware interface 13: `C_DEV_VDC` (`vdc_peek.vhd`), the VDC's RAM (words `0x0000`-`0xFFFF`)
and registers (its CSR window), read through a second port of the VDC's own RAM and a copy
of its register read, on its clock. The VDC is in the C128_MiSTer submodule.

## Settings

The menu's Network submenu: REST API, FTP Server and DHCP are menu items, saved when the
menu closes; host name, password and the static address are typed in their lines (Return
keeps, Run/Stop leaves, Del, Clr), saved at once. Both go into `/c128/settings.cfg`. While
DHCP is on the address lines show what it gave; typing one turns DHCP off, the others then as
DHCP gave them. The main menu's line shows the address. Defaults: REST and FTP off, no
password, host `c128-mega65`, DHCP.

Its stream lines are his F5 menu's: Video stream and Audio stream start and stop a stream
(never saved: none at power-up, as his), each to the target on its `to:` line, typed as the
host name is and kept in the Data Streams store. A tick shows a stream that runs, started
here or by REST. A start that fails shows his popup's text until Space.

## Clients

- **C64 Ultimate Toolbox** (macOS): its keyboard types only in C64 mode (`GO64`) for now. It
  doesn't use `machine:input`; it writes each character into the C64 KERNAL's keyboard
  buffer with `writemem` (`$0277` on, its count at `$C6`). In C128 mode those addresses are
  BASIC's input buffer and `BA`, the LOAD/SAVE bank: nothing is typed and `BA` counts up to
  10 (BASIC sets it again before every LOAD and SAVE). Its video needs a wired network
  (`doc/streams.md`).

## Not his

- Off by default, as the FTP server is: with no password set, anyone on the network could
  drive the machine. His is on. `:reset_to_default` turns it off too.
- No web pages: his firmware serves its web UI from flash for any other path; this one
  answers 404.
- A connection closes with a FIN after any bytes that followed the request. His sockets
  would reset it then.
- run_prg/load_prg load by DMA in the program's own mode, and run_prg then types RUN; his boot
  cartridge always starts C64 mode (on a C128 too) and prints LOAD/SEARCHING/LOADING lines first.
- A D64 mounted in a 1581 makes the drive a 1571 (a C128's drive; Jeff, 2026-11-02); his
  makes it a 1541.
- A model a drive's image can't run (set_mode, Drive Type) ejects the image; his keeps it
  mounted, unread. main.vhd runs an image as its own type, so keeping it would run another
  model than the one set.
- No debug stream: its start answers 501, and the Data Streams store has no Stream Debug to
  or Debug Stream Mode (Jeff deferred it, 2026-11-04).
- The streams' start and stop are lines in the Network submenu, ticked while a stream runs; his
  are F5 actions that ask "Send to..." each time. A failed start's text is a message screen,
  his a popup.
- machine:input's C128 keys (above) are ours (Jeff, 2026-11-03); his U64 has a C64 keyboard.
- machine:debugreg is a plain byte; his U64's register drives FPGA debug features.
- machine:vdcmem and machine:vdcregs are ours: the U64 has no VDC. vdcmem takes readmem's
  arguments and gives its answers.
- A batch whose tap waits for room in the tap queue holds its answer until the tap is in (his
  REST task waits); another input request waits for it, as for his mutex.
- A tap holds a key 80 ms, his 40 (a key every 100 ms, his 60): the C128's CP/M BIOS loses
  keys held under ~45 ms (measured on the R6). And before a tap that repeats a key of the one
  before, 80 ms up, his 20: CP/M takes a key again only once it was up ~70 ms. Jeff,
  2026-11-03: in every mode.
- Keys tapped while the menu is open go to it through the same tap queue as the C128's (100 ms
  a key); his push key codes into his menu's buffer at once. The mouse doesn't drive the menu
  (his Menu Mouse Navigation).
- Right after the menu closes the C128's keyboard is off for a third of a second; taps wait
  for it.
- Uploads are files on the card (`/c128/temp`), where his are in a RAM disk; a .crt upload
  goes into the cartridge's memory as it comes.
- Never his 403 "Disk has been modified, save first": the drives write to the card as they go.
- A JSON answer that would leave the heap less than 512 KB (his has no limit but his RAM) is a
  500 with "The answer is longer than N KB."

## Tests

- `make -C M2M/firmware host-test` runs `test_http`: the server and lwIP with clients on a
  loopback, every answer to the byte, the limits, the reaper, bodies, the backlog; the
  routes on `host/host_rest.c`'s fakes (a C128's memory behind a DMA model, a C128 that
  resets, boots to BASIC and takes typed keys, the drives, the drive ROM and cartridge
  devices, the menu's bits) with the real `stores.c` and FatFs on a card image. `test_shell` types in the Network
  submenu; `test_uci` and `test_prg` keep LOAD_CONFIG's answers.
- Streams: `test_http` starts and stops them against a DNS server and ARP entries on its
  loopback and checks each header against `test/streams/udp_headers.py` (his
  calculate_udp_headers in Python), his ARP probes and their timing, his error texts, a failed
  restart leaving the old target streaming, two starts in turn, a stop while a name
  resolves; `test_shell` the menu's lines, his popup and a typed target.
- `CORE/scripts/run_c128_dma_nvc.sh`: the bus DMA against a model of the core's DMA port.
- VDC: `test_http` checks vdcmem's arguments and answers (readmem's), that it needs no DMA
  and works while the Z80 runs, and vdcregs. `CORE/scripts/run_vdc_peek_sim.tcl`
  (tb_vdc_peek) reads the real VDC both ways, by the device and as a program does through
  $D600/$D601, in 16K and 64K mapping; registers without side effects; during a block fill.
  `test/vdc/hw_vdc.py` does the same on the machine.
- machine:input: `test_http` checks his validation texts, the answers, tap timing on its clock,
  the C128 keys, RESTORE, joysticks, the mouse and its 429, the menu, release_all and reset;
  `test_shell` drives the menu by API taps. `CORE/scripts/run_input_inject_nvc.sh` (the device:
  commits, never a mix of two, the debug register) and `run_keyboard_nvc.sh` (every key
  position, both scan directions, with the Z80).
- `M2M/firmware/host/build/test_http -serve 18080 IMG` puts that build on `127.0.0.1:18080`,
  with the FAT image IMG as its card.
- `CORE/scripts/run_drive_rom_sim.tcl` (tb_drive_rom: reloads, also in the middle of a load
  and in reset) and `run_vd_sd_sim.tcl` (tb_vd_sd: hold).
- `test/rest/rest_ab.py`: the same raw requests to the core and to an Ultimate, compared
  byte for byte (`--slow` adds the 15 s cases). `--contract ~/Code/1541ultimate` checks the
  answers against his OpenAPI document for what `/v1/info` says (`core_version`: U64) with his
  own checker. Against his U64 document only poweroff's 501 fails (above). The cases change nothing on either
  machine (no such file, or refused first).
