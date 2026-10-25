#!/bin/sh
# build.sh <program.prg> <out.crt> <cart name>: a C64 program as a Magic Desk cartridge.
# Needs cc65 (ca65, ld65) and VICE's cartconv on PATH, or CARTCONV=<path>.
set -e
out=$(cd "$(dirname "$2")" && pwd)/$(basename "$2")
prg=$(cd "$(dirname "$1")" && pwd)/$(basename "$1")
cd "$(dirname "$0")" >/dev/null
cp "$prg" payload.prg
ca65 cart.s -o cart.o
ld65 -C cart.cfg cart.o -o cart.bin
"${CARTCONV:-cartconv}" -q -t md -i cart.bin -o "$out" -n "$3"
rm -f payload.prg cart.o cart.bin
