# REST API

The HTTP API of Gideon's Ultimate firmware, on port 80 of the MEGA65's Ethernet port, so
tools written for an Ultimate II+ or an Ultimate 64 can drive the C128. It answers as his
firmware does, byte for byte, because tools parse his answers.

- `CORE/firmware/net/http.c`: the server, a port of MicroHttpServer (Gideon's
  `software/httpd` at `ad73d32`) to lwIP's raw API. Its rules: four connections and two
  more waiting, a header of up to 2048 bytes and 20 fields, a body only on POST
  (Content-Length, chunked, or until the client closes), `Connection: close`, and a
  connection reaped after 15 s without traffic. Requests run from the main loop, so a route
  may use the card. Its buffers are in HyperRAM.
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

Machine and configs routes come next (8f), then drives, files and runners (8g).

## Settings

REST is on, with no password and the host name `c128-mega65`, until the menu gets the
network settings (REST on/off, password, host name, static IP), with text entry in the OSM.

## Not his

- No web pages: his firmware serves its web UI from flash for any other path; this one
  answers 404.
- A connection closes with a FIN after any bytes that followed the request. His sockets
  would reset it then.

## Tests

- `make -C M2M/firmware host-test` runs `test_http`: the server and lwIP with clients on a
  loopback, every answer to the byte, the limits, the reaper, bodies, the backlog. 10 of 11
  mutations caught: the 11th (bytes after a request dropped, not kept) needs a long answer,
  which comes with 8f's readmem.
- `M2M/firmware/host/build/test_http -serve 18080` puts that build on `127.0.0.1:18080`.
- `test/rest/rest_ab.py`: the same raw requests to the core and to an Ultimate, compared
  byte for byte (`--slow` adds the 15 s cases). `--contract ~/Code/1541ultimate` checks the
  answers against his U2 OpenAPI document with his own checker; his document says an HTML
  400 for `/v1/help` without its argument, where his code (and the II+L) answers JSON.
