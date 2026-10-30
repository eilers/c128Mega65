# REST API

The HTTP API of Gideon's Ultimate firmware, on port 80 of the MEGA65's Ethernet port, so
tools written for an Ultimate II+ or an Ultimate 64 can drive the C128. It answers as his
firmware does, byte for byte, because tools parse his answers.

- `CORE/firmware/net/http.c`: the server, a port of MicroHttpServer (Gideon's
  `software/httpd` at `ad73d32`) to lwIP's raw API. Its rules: four connections and two
  more waiting, a header of up to 2048 bytes and 20 fields, a body only on POST
  (Content-Length, chunked, or until the client closes), `Connection: close`, and a
  connection reaped after 15 s without traffic. Requests run from the main loop, so a route
  may use the card; a route's answer can take several rounds (readmem's DMA). Its buffers
  are in HyperRAM. `net/multipart.c` splits a POST's body as his `multipart.c` does.
- `CORE/firmware/net/rest.c`: the API, from Gideon's `software/api` at `4b00732` (his 3.15a,
  on Jeff's II+L, has the same core): the URL split, the arguments and their checks,
  X-Password, his JSON layout (no escaping, `{ \n`, `[  ]`) and status texts (412 and 500
  go out as `Unknown`), and a bare 404 for anything that isn't a route.

## Routes

| Route | Answer |
|---|---|
| `GET /v1/version` | `0.1`, as his |
| `GET /v1/info` | product `C128MEGA65` (a name, as his `Ultimate II+L`), firmware version = config.vhd's `CORE_VERSION`, the firmware's git hash, `fpga_version` 1 + the hardware interface's number, host name, `unique_id` (24 bits from the FPGA's DNA, as his from his flash's serial), the Ethernet MAC. No `core_version`, so his tests take it for a U2 (a cartridge) and check it against his U2 document; no `wifi_mac` |
| `GET /v1/help?command=` | his fixed page |
| `PUT /v1/machine:reset`, `:reboot` | the reset button, as UCI's REBOOT; a pause ends with it |
| `PUT /v1/machine:pause`, `:resume` | the C128's CPU held by bus DMA, as his `stop()`; answered at once |
| `GET /v1/machine:readmem?address=&length=` | memory as the C128's CPU sees it then (the MMU's banks, ROMs, I/O), read by bus DMA with the CPU held for the whole request; 1 µs a byte, 64 KB in about 70 ms |
| `PUT /v1/machine:writemem?address=&data=`, `POST` | the same, written: up to 128 bytes in the URL, or a POST's body (raw, or a multipart file part: the first part, up to 64 KB) |
| `PUT /v1/machine:menu_button` | opens the On-Screen-Menu, or closes it (not while a file is being picked) |
| `GET /v1/machine:menu_screen` | 404 while the menu is closed; else 2000 bytes: the OSM window from its top left as 40 × 25 C64 screen codes (lower-case set), then their colours |
| `PUT /v1/machine:poweroff`, `GET :measure` | 501, as his U2 |
| `GET /v1/machine:heap` | the firmware's HyperRAM heap: it never frees, so `min_ever_free` is `free` |
| `GET /v1/configs[/CATEGORY[/ITEM]]` | his stores and items that the core has (below), with his names and patterns |
| `PUT /v1/configs/CATEGORY/ITEM?value=` or `/VALUE` | one item, in effect at once, in memory |
| `POST /v1/configs` | a JSON body of categories and items, each tried |
| `PUT /v1/configs[/CATEGORY]:save_to_flash`, `:load_from_flash`, `:reset_to_default` | the changed categories into the menu's settings file and `network.cfg`; back from them; the defaults |
| `GET /v1/drives` | `a` (drive 8) and `b` (drive 9): power, bus id, model, `rom` his default name for the model (`1571.rom`), the image as his: a REST mount shows the path it was given as `image_file` with an empty `image_path`, a menu or UCI mount its folder and name. The internal floppy, when it's drive 8 or 9, is that drive: a 1581 with no image. No `softiec` |
| `PUT /v1/drives/D:mount?image=&type=&mode=`, `POST` | the image into the drive, answered once a 1541/1571 has it (as UCI's MOUNT_DISK). `type` (default: the extension) only picks his command and Ftype; the core takes D64, D71 and D81 by their size, G64/G71 answer 501. `readonly` write protects the disk. `unlinked` mounts a copy in `/c128/temp`: writes go to the copy, the file stays as it was. On the internal floppy's drive the floppy is turned off first, as the menu does. POST: the body goes to `/c128/temp/NAME` (NAME from the part's file name, `temp0000` and up without one; `_1`, `_2` while that name is in a drive) |
| `PUT /v1/drives/D:reset`, `:on`, `:off`, `:remove` | the drive's ROM reloaded (a reset), its power, the image out |
| `PUT /v1/drives/D:unlink` | the drive moves onto a copy of its image in `/c128/temp` without a disk change: its requests wait (vd_sd's hold) while its changes go to the card and the copy is made, then its table points at the copy. GET drives still shows the file it came from |
| `PUT /v1/drives/D:load_rom?file=`, `POST` | 16 or 32 KB into the drive's bank of `boot1.rom` for its model (a 16 KB image twice), then the bank reloaded into the drive, which resets it; another size is written anyway and answers his 412. Lasts until a long reset loads `boot1.rom` again |
| `PUT /v1/drives/D:set_mode?mode=` | 1541 or 1571: the menu's model, which both 5.25" drives share; 1581 only for a drive that is one (a D81, the internal floppy), else his 415 |
| `GET /v1/files/PATH:info` | his name, size and extension of a file on the card (`SD/...`) |
| `PUT /v1/files/PATH:create_d64?tracks=&diskname=`, `:create_d71`, `:create_d81` | zeros, then his `format()`: the BAM and the first directory block (byte for byte his, `$20` in a D64's byte 3 included). `create_dnp` checks his arguments, then 501 |
| `PUT /v1/runners:load_prg?file=`, `:run_prg`, `POST` | a program for `$0801` runs in C64 mode, any other in C128 mode: the C128 reset, BASIC's READY awaited, GO64 typed for C64 mode, the program and his loader's pointers by DMA with the CPU held, the lines relinked (his runner's LINKPRG), RUN typed for run_prg. A `.cfg` (or `.usr`) next to the file is loaded first, as his. A P00's header is skipped. 423 while the Z80 runs, or when READY doesn't come within 10 s (a C128 cartridge, a boot or CP/M disk, a C128 program with a C64 cartridge) |
| `PUT /v1/runners:run_crt?file=`, `POST` | the `.crt` as the menu's CRT: line: the cartridge's source becomes the virtual cartridge, main.vhd resets the C128 into it; an upload goes straight into the cartridge's memory. Too large: 412, not a `.crt` the parser takes: 415 |
| `PUT /v1/runners:sidplay`, `:modplay`, `POST` | 501: his U2's "Sampler module not available" for modplay, "not supported on this architecture" for sidplay |

`machine:debugreg` is a U64's only: 404, as on his U2. A request that takes the bus waits
for another one's; the Z80 takes no DMA, so readmem and writemem answer 423 (his "Could not
obtain lock of subsystem") while it runs. The bus DMA is `CORE/vhdl/c128_dma.vhd`: jobs of
up to 2 KB through its own buffer, after a cartridge and the REU on the core's DMA port.

### The stores

The same table LOAD_CONFIG uses (`CORE/firmware/stores.c`): his names and types; the
choices are the core's, under his names where he has the same.

| Category | Items |
|---|---|
| C64 and Cartridge Settings | RAM Expansion Unit, REU Size (512 KB, 2 MB, 16 MB; 512 KB on R3), REU Preload Image (a card path, `/SD/...`), Command Interface |
| Drive A Settings, Drive B Settings | Drive (its power), Drive Type (1541/1571 for both 5.25" drives, 1581 only for a drive that is one), Drive Bus ID (8, or 9) |
| Modem Settings | Modem Interface, ACIA (6551) Mapping (Off, DE00/NMI, DF00/NMI, D700/NMI), Hardware Mode |
| Ethernet Settings | Use DHCP, Static IP, Static Netmask, Static Gateway, Static DNS |
| Network Settings | Host Name, Network Password, Web Remote Control Service |

A drive's power and the REU's image have no home on the card: save_to_flash keeps
neither, load_from_flash leaves them.

The routes are in `net/rest_machine.c`, `rest_configs.c`, `rest_drives.c`, `rest_files.c`
and `rest_runners.c`.

### How run_prg finds READY

The firmware resets the C128 with `$0A00` and `$2D` cleared by DMA first (the CPU held until
the reset has happened, so old values can't be taken for new ones). C128 BASIC is ready once
`$0A00` (the system vector) is 3, the end of its cold start; C64 BASIC once `$2B`-`$2E` are
`01 08 03 08` (its NEW). Then a NUL goes into the keyboard buffer: the editor takes it, and
prints nothing, only once it waits at READY, after the disk boot. Checked in VICE with the
core's own ROMs, 40 and 80 columns.

### Bitstream

Hardware interface 4: `C_DEV_DRIVE_ROM`'s CSR (register 0: R bit i drive i has its ROM, W bit i
reloads drive i's bank, its `iecdrv_rom` and the ROM server starting over) and vd_sd's hold
(CSR `0x035`). iec_drive's per-drive `rom_reload` is in the C128_MiSTer submodule.

## Settings

The menu's Network submenu: REST API and DHCP are menu items, saved with the menu; host
name, password and the static address are typed in their lines (Return keeps, Run/Stop
leaves, Del, Clr), kept in `/c128/network.cfg` (his settings format) at once. While DHCP is
on the address lines show what it gave; typing one turns DHCP off, the others then as DHCP
gave them. The main menu's line shows the address. Defaults: REST on, no password, host
`c128-mega65`, DHCP.

## Not his

- No web pages: his firmware serves its web UI from flash for any other path; this one
  answers 404.
- A connection closes with a FIN after any bytes that followed the request. His sockets
  would reset it then.
- run_prg/load_prg type RUN after a DMA load in the program's own mode; his boot cartridge
  always starts C64 mode (on a C128 too) and prints LOAD/SEARCHING/LOADING lines first.
- A load_rom'd ROM stays in its bank through a drive model change; his reloads the configured
  ROM file then.
- Uploads are files on the card (`/c128/temp`), where his are in a RAM disk; a .crt upload
  goes into the cartridge's memory as it comes.
- Never his 403 "Disk has been modified, save first": the drives write to the card as they go.

## Tests

- `make -C M2M/firmware host-test` runs `test_http`: the server and lwIP with clients on a
  loopback, every answer to the byte, the limits, the reaper, bodies, the backlog; the
  routes on `host/host_rest.c`'s fakes (a C128's memory behind a DMA model, a C128 that
  resets, boots to BASIC and takes typed keys, the drives, the drive ROM and cartridge
  devices, the menu's bits) with the real `stores.c` and FatFs on a card image. `test_shell` types in the Network
  submenu; `test_uci` and `test_prg` keep LOAD_CONFIG's answers.
- `CORE/scripts/run_c128_dma_nvc.sh`: the bus DMA against a model of the core's DMA port.
- `M2M/firmware/host/build/test_http -serve 18080 IMG` puts that build on `127.0.0.1:18080`,
  with the FAT image IMG as its card.
- `CORE/scripts/run_drive_rom_sim.tcl` (tb_drive_rom: reloads, also in the middle of a load
  and in reset) and `run_vd_sd_sim.tcl` (tb_vd_sd: hold).
- `test/rest/rest_ab.py`: the same raw requests to the core and to an Ultimate, compared
  byte for byte (`--slow` adds the 15 s cases). `--contract ~/Code/1541ultimate` checks the
  answers against his U2 OpenAPI document with his own checker; his document says an HTML
  400 for `/v1/help` without its argument, where his code (and the II+L) answers JSON, and
  leaves out the 400 for an argument a call hasn't. The cases change nothing on either
  machine (no such file, or refused first).
