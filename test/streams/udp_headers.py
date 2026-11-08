#!/usr/bin/env python3
"""The 42-byte headers test_http expects of stream.c, worked out apart from it: Gideon's
calculate_udp_headers (software/io/network/data_streamer.cc at 4b00732) line by line, with
the IP and UDP lengths of the whole packet and the IP checksum over them, as the packets
on the wire must have them. Prints a C string per case for test_http.c."""
import ipaddress

PAYLOAD = {0: 780, 1: 770}                      # video, audio: what follows the UDP header
SPORT = {0: (0xD0, 0x00), 1: (0xD4, 0x00)}


def his_ip(text):
    """His uint32: the address in network order, read as a little-endian word."""
    return int.from_bytes(ipaddress.IPv4Address(text).packed, "little")


def header(stream, my_mac, my_ip, dest_ip, dest_port, dest_mac=None):
    h = [0x01, 0x00, 0x5E, 0x00, 0x01, 0x40, 0x00, 0x15, 0x41, 0xAA, 0xAA, 0x01, 0x08, 0x00,
         0x45, 0x00, 0x00, 0x1C, 0xBB, 0xBB, 0x40, 0x00, 0x40, 0x11, 0x00, 0x00,
         0xEF, 0x00, 0x01, 0x40, 0xC0, 0xA8, 0x01, 0x40,
         0xD0, 0x00, 0x2A, 0xF8, 0x00, 0x08, 0x00, 0x00]
    me, dest = his_ip(my_ip), his_ip(dest_ip)
    h[34], h[35] = SPORT[stream]
    h[6:12] = my_mac
    for i in range(4):
        h[26 + i] = (me >> (8 * i)) & 0xFF
        h[30 + i] = (dest >> (8 * i)) & 0xFF
    if (dest & 0xF8) == 0xE8:
        h[3], h[4], h[5] = h[31] & 0x7F, h[32], h[33]
    elif dest == 0xFFFFFFFF:
        h[0:6] = [0xFF] * 6
    else:
        h[0:6] = dest_mac
    h[36], h[37] = (dest_port >> 8) & 0xFF, dest_port & 0xFF
    # The whole packet's lengths, then the IP header's checksum (RFC 791)
    ip_len, udp_len = 20 + 8 + PAYLOAD[stream], 8 + PAYLOAD[stream]
    h[16], h[17] = ip_len >> 8, ip_len & 0xFF
    h[38], h[39] = udp_len >> 8, udp_len & 0xFF
    h[24] = h[25] = 0
    s = sum(h[14 + 2 * i] << 8 | h[15 + 2 * i] for i in range(10))
    while s >> 16:
        s = (s & 0xFFFF) + (s >> 16)
    h[24], h[25] = (~s >> 8) & 0xFF, ~s & 0xFF
    return bytes(h)


MAC = [0x02, 0xC1, 0x28, 0x12, 0x34, 0x56]      # test_http's netif, 10.0.0.1/24, gateway .254
CASES = [
    ("multicast", 0, "239.0.1.64", 11000, None),
    ("broadcast", 1, "255.255.255.255", 6000, None),
    ("unicast", 0, "10.0.0.77", 11000, [0x00, 0x11, 0x22, 0x33, 0x44, 0x55]),
    ("gateway", 1, "192.168.5.5", 11001, [0x66, 0x77, 0x88, 0x99, 0xAA, 0xBB]),
]
for name, stream, ip, port, mac in CASES:
    h = header(stream, MAC, "10.0.0.1", ip, port, mac)
    print(f'#define HDR_{name.upper():9} "{"".join(f"{b:02X}" for b in h)}"')
