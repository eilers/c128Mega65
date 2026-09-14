#!/usr/bin/env bash
# Print the MEGA65 JTAG UART (QNICE Shell log, including DRV_RD lines).
# Our core runs 115200 8N1 (UART_DIVISOR=27 at 50 MHz in qnice_globals.vhd).
# The MEGA65 factory core's serial monitor runs at 2000000 instead, so reading
# it at 115200 produces garbage. Do not press Run/Stop + Cursor Up + Help.
#
# Usage:
#   CORE/scripts/jtag_console.sh              # auto-pick port, print to stdout
#   CORE/scripts/jtag_console.sh /dev/ttyUSB1
#   CORE/scripts/jtag_console.sh -l jtag.log  # also append to a file
#   CORE/scripts/jtag_console.sh -b 2000000   # MEGA65 factory core
# Exit with Ctrl+C.

set -euo pipefail

log_file=""
port=""
baud=115200
while [[ $# -gt 0 ]]; do
  case "$1" in
    -l|--log) log_file="${2:-}"; shift 2 ;;
    -b|--baud) baud="${2:-}"; shift 2 ;;
    -h|--help)
      sed -n '2,13p' "$0"
      exit 0
      ;;
    *) port="$1"; shift ;;
  esac
done

if [[ -z "$port" ]]; then
  # The TE-0790 exposes two serial interfaces and the QNICE console is the second one.
  # Reprogramming the FPGA makes the adapter re-enumerate, so the numbering is not
  # stable across sessions: after a few programming cycles ttyUSB1 can become ttyUSB2.
  # Take the highest-numbered port that exists rather than a hardcoded pair.
  mapfile -t ports < <(ls -1 /dev/ttyUSB* 2>/dev/null | sort -V)
  if [[ ${#ports[@]} -eq 0 ]]; then
    echo "No /dev/ttyUSB* found. Is the TE-0790 JTAG adapter plugged in?" >&2
    ls -l /dev/ttyACM* 2>/dev/null || true
    exit 1
  fi
  port="${ports[-1]}"
fi

if [[ ! -e "$port" ]]; then
  echo "Port $port does not exist." >&2
  exit 1
fi

if [[ ! -r "$port" || ! -w "$port" ]]; then
  echo "Cannot open $port (need read+write)." >&2
  echo "This adapter is usually in group dialout. Either:" >&2
  echo "  sudo usermod -aG dialout \"\$USER\"   # then log out and back in" >&2
  echo "  sudo $0 $port" >&2
  ls -l "$port" >&2
  exit 1
fi

# MEGA65 JTAG UART: 8N1, no XON/XOFF, no RTS/CTS, no DTR/DSR.
stty -F "$port" "$baud" cs8 -cstopb -parenb -ixon -ixoff -crtscts clocal raw \
  -echo -echoe -echok -echoctl -echoke

echo "Listening on $port at $baud 8N1 (Ctrl+C to stop)." >&2
echo "Do not press Run/Stop + Cursor Up + Help; this is the live Shell log." >&2
if [[ -n "$log_file" ]]; then
  echo "Also appending to $log_file" >&2
  exec cat "$port" | tee -a "$log_file"
else
  exec cat "$port"
fi
