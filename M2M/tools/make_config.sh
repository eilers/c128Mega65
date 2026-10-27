#!/usr/bin/env bash

if [ $# -ne 2 ]; then
  echo "MiSTer2MEGA65 - Create empty config file. Usage: make_config.sh <filename> <OPTM_SIZE|auto|auto-r3>"
  exit
fi

if [ "$2" = "auto" ] || [ "$2" = "auto-r3" ]; then
  OPTM_SIZE=$(grep "constant OPTM_SIZE_ALL" "../../CORE/vhdl/config.vhd" | sed -E 's/.*:= ([0-9]+).*/\1/')
  if [ -z "$OPTM_SIZE" ]; then
    echo "Error: Could not find OPTM_SIZE_ALL in ../../CORE/vhdl/config.vhd"
    exit 1
  fi
  # R3-memory builds leave the 2 MB and 16 MB REU items out of the menu
  if [ "$2" = "auto-r3" ]; then
    R3_DROP=$(grep "constant C_MENU_R3_DROP" "../../CORE/vhdl/globals.vhd" | sed -E 's/.*:= ([0-9]+).*/\1/')
    if [ -z "$R3_DROP" ]; then
      echo "Error: Could not find C_MENU_R3_DROP in ../../CORE/vhdl/globals.vhd"
      exit 1
    fi
    OPTM_SIZE=$((OPTM_SIZE - R3_DROP))
  fi
elif [[ "$2" =~ ^[0-9]+$ ]] && [[ $2 -ge 1 ]]; then
  OPTM_SIZE=$2
else
  echo "MiSTer2MEGA65 - Create empty config file. Usage: make_config.sh <filename> <OPTM_SIZE>"
  echo "Error: <OPTM_SIZE> needs to be an integer which is >= 1 or 'auto'"
  exit 1
fi

configfile=""

for (( i=1; i<= $OPTM_SIZE; i++ ))
do
    configfile="${configfile}\xff"
done

echo -ne "$configfile" > "$1"
