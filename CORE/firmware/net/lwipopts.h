// lwIP for the system firmware: raw API, no OS, IPv4 only. Sized for the modem's call,
// UCI's network target (4 sockets) and the REST server, in the 128 KB data RAM; the TCP
// window only has to cover modem speed x RTT.
#pragma once

#define NO_SYS                      1
#define SYS_LIGHTWEIGHT_PROT        0
#define LWIP_SOCKET                 0
#define LWIP_NETCONN                0
#define LWIP_TIMERS                 1

#define MEM_ALIGNMENT               4
#define MEM_SIZE                    (12 * 1024)
#define MEMP_NUM_PBUF               8
#define MEMP_NUM_RAW_PCB            0
#define MEMP_NUM_UDP_PCB            6      // DHCP, DNS, UCI's sockets
#ifndef MEMP_NUM_TCP_PCB                   // test_http adds its clients' share
#define MEMP_NUM_TCP_PCB            12     // the call, UCI's sockets, HTTP clients, closing ones
#endif
#define MEMP_NUM_TCP_PCB_LISTEN     3      // the modem, HTTP, one spare
#define TCP_LISTEN_BACKLOG          1      // HTTP queues 2, as his listen()
#define MEMP_NUM_TCP_SEG            32
#define MEMP_NUM_ARP_QUEUE          4
#define PBUF_POOL_SIZE              8      // one pool pbuf holds a whole frame

#define LWIP_IPV4                   1
#define LWIP_IPV6                   0
#define LWIP_ARP                    1
#define ARP_TABLE_SIZE              8
#define ETHARP_SUPPORT_STATIC_ENTRIES 0
#define IP_REASSEMBLY               0
#define IP_FRAG                     0
#define LWIP_ICMP                   1
#define LWIP_RAW                    0
#define LWIP_DHCP                   1
#define LWIP_DHCP_DOES_ACD_CHECK    0
#define LWIP_ACD                    0
#define LWIP_AUTOIP                 0
#define LWIP_IGMP                   0
#define LWIP_DNS                    1
#define DNS_TABLE_SIZE              4
#define DNS_MAX_NAME_LENGTH         64
#define LWIP_UDP                    1
#define LWIP_TCP                    1

#define TCP_MSS                     1460
#define TCP_WND                     (2 * TCP_MSS)
#define TCP_SND_BUF                 (2 * TCP_MSS)
#define TCP_SND_QUEUELEN            (4 * TCP_SND_BUF / TCP_MSS)
#define TCP_QUEUE_OOSEQ             0      // out-of-order segments get resent; saves pbufs
#define LWIP_TCP_KEEPALIVE          1
#define SO_REUSE                    1      // a listener comes back while its old connections wait out TIME_WAIT
#define LWIP_CALLBACK_API           1

#define LWIP_NETIF_HOSTNAME         1
#define LWIP_NETIF_STATUS_CALLBACK  1
#define LWIP_NETIF_LINK_CALLBACK    0
#define LWIP_SINGLE_NETIF           1

#define LWIP_STATS                  0
#define LWIP_STATS_DISPLAY          0
#define LWIP_CHECKSUM_ON_COPY       0
#define LWIP_PROVIDE_ERRNO          1
