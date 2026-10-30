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

Machine and configs routes are in `net/rest_machine.c` and `net/rest_configs.c`; drives,
files and runners come next (8g).

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

## Tests

- `make -C M2M/firmware host-test` runs `test_http`: the server and lwIP with clients on a
  loopback, every answer to the byte, the limits, the reaper, bodies, the backlog; the
  machine and configs routes on `host/host_rest.c`'s fakes (a C128's memory behind a DMA
  model, the menu's bits) with the real `stores.c`. `test_shell` types in the Network
  submenu; `test_uci` and `test_prg` keep LOAD_CONFIG's answers.
- `CORE/scripts/run_c128_dma_nvc.sh`: the bus DMA against a model of the core's DMA port.
- `M2M/firmware/host/build/test_http -serve 18080` puts that build on `127.0.0.1:18080`.
- `test/rest/rest_ab.py`: the same raw requests to the core and to an Ultimate, compared
  byte for byte (`--slow` adds the 15 s cases). `--contract ~/Code/1541ultimate` checks the
  answers against his U2 OpenAPI document with his own checker; his document says an HTML
  400 for `/v1/help` without its argument, where his code (and the II+L) answers JSON, and
  leaves out the 400 for an argument a call hasn't. The machine and configs cases change
  nothing on either machine.
