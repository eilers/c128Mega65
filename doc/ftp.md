# FTP server

The FTP server of Gideon's Ultimate firmware, on port 21 of the MEGA65's Ethernet port, so
files go onto the SD card from Finder, FileZilla, `curl` or `lftp` while the core runs. It
answers as his `software/network/ftpd.cc` (at `4b00732`) does, reply for reply.

- `CORE/firmware/net/ftp.c`: his server on lwIP's raw API, as `http.c` is his web server.
  lwIP's callbacks only move bytes; the commands and the card work run from the main loop, a
  step a round. Each session's transfer buffer (4 KB) is in the data RAM.
- `M2M/firmware/host/test_ftp.c`: the host test, with lwIP clients on a loopback and a FAT
  image for the card. The network takes turns inside FatFs's disk work there too, as on the
  hardware.

## As his

- Four sessions; a fifth gets `421 Too many FTP connections, try again later.` A session ends
  after 300 s without a command.
- Any user name. The password is the Network Password (menu, REST, LOAD_CONFIG); with none
  set, any password logs in. A wrong one is answered after a second.
- Passive mode (PASV, ports from 51000) and active mode (PORT). A data connection gets 5 s to
  come up, and a transfer 5 s without progress, or the reply is 425 or 426. No EPSV:
  clients fall back to PASV.
- Commands: USER, PASS, QUIT, PORT, PASV, CWD, CDUP, PWD, XPWD, LIST, NLST, MLSD, MLST,
  RETR, STOR, SIZE, DELE, RNFR, RNTO, MKD, XMKD, RMD, XRMD, TYPE (binary always), MODE
  (502), NOOP, SYST (`214 UNIX system type.`), FEAT, ABOR. Others: 502.
- Commands that arrive during a transfer wait for it to end.
- Paths as UCI DOS has them: `/` holds one folder, `SD`, the active card (the back slot if it
  holds one, else the bottom tray). A session starts in `/`. LIST of a name or a pattern
  (`*`, `?`) lists the matching files of its folder.

## Not his

- A mounted disk image can't be overwritten, deleted or renamed (the drives read and write it
  on the card, where his firmware has a copy in memory): `450 File in use.`
- An empty upload makes an empty file; his makes none.
- ABOR is answered with 226; his doesn't answer.
- MKD's reply names the new folder once (`257 "/SD/new" created.`); his puts a second `/`
  after the folder it was made in.
- Uploads are dated 1 January 2026, as everything the firmware writes (FatFs without a clock).

## Speed

On an R6 (2026-10-31, 1 MB and 4 MB files): about 135 KB/s up and 218 KB/s down. Neither
limit is measured apart yet; timers around the card reads and writes in `ftp.c` would split
card time from network time.

- Up: about 3.8 ms per 512-byte block, roughly what a card takes for a single-block write.
  FatFs reaches the card through the SD controller's register interface (`lib/sd.c`): one
  block per command, each byte moved through the controller's cursor. The controller's
  engine port already streams multi-block transfers (CMD18/CMD25) for the core's hardware;
  `reu_sd.vhd` writes REU memory to the card that way. A write path from FatFs's
  multi-block `disk_write` calls into that port, from a buffer in HyperRAM, is the likely
  way to faster uploads. A bigger TCP window alone wouldn't help, and lwIP's `TCP_WND` is
  global: UCI's socket buffers are sized by it, in the data RAM.
- Down: probably lwIP's send buffer, two segments (`TCP_SND_BUF`) a main-loop round. A bigger
  one (about 24 KB more of lwIP's heap, `MEM_SIZE`, for four downloads at once) is the cheap
  experiment.

## Settings

The menu's Network submenu has an `FTP Server` line (default on), saved with the menu; REST
and LOAD_CONFIG call it `FTP File Service` in Network Settings, as his. Turning it off closes
port 21; sessions already open go on.
