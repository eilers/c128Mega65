// The FTP server: Gideon's ftpd.cc (software/network at 4b00732) on lwIP's raw API. Port 21,
// four sessions, the card under /SD. Its work runs from the main loop, so it may use the card.
#pragma once

void ftp_start(void);     // from the network's start
void ftp_listen(int on);  // port 21 open or closed (the menu's FTP Server item), in a turn or under net_lock
void ftp_poll(void);      // from the main loop
