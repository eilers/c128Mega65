# Video and audio stream tests

`stream_rx.py HOST [--password PW] [--seconds N] [--watch] [--out DIR]` starts both streams
to the machine it runs on (REST, his `/v1/streams`), listens on 11000 and 11001 and reports
lost, reordered and malformed packets (the one malformed per start is his 2-byte ARP probe),
whole frames and their rate (50.12/s on PAL), arrival pauses, and the audio's sample rate
(his 47983 Hz). `--watch` on a still screen logs each frame that differs from the first by
more than the cursor's cell. It saves the last whole frame as `frame.png` and the audio as
`audio.wav`. Run it on a wired machine: Wi-Fi pauses for 100 ms and more. It needs the menu's
Network > REST API on (default off).

`compare_frame.py STREAM.png VICE.png` checks a frame pixel for pixel against x128's picture
of the same screen, taken with
`x128 -default -VICIIborders 0 -limitcycles 9000000 -exitscreenshotvicii vice.png` (the
C128's start-up screen with READY). Leave the cursor out with `--ignore X,Y,8,8`.

`udp_headers.py` is his `calculate_udp_headers` in Python; `test_http` checks the
firmware's headers against it.
