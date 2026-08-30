#!/usr/bin/env python3
"""Add the virtual-drive sources to a Vivado .xpr project file.

The .xpr files are generated artefacts that Vivado rewrites wholesale, so this
inserts the <File> blocks idempotently rather than expecting a hand edit to
survive. Run it on CORE-R6-vivado2022.xpr (and CORE-R6.xpr) and let
gen_board_xpr.sh derive R3/R4/R5 from the R6 template afterwards.
"""

import re
import sys

# (path relative to $PPRDIR, Vivado source type). The .v files are listed as
# SVerilog because they use SystemVerilog constructs despite their extension.
SOURCES = [
    ("vhdl/mount_buf_wrapper.vhd", "VHDL2008"),
    ("vhdl/drive_rom_server.vhd", "VHDL2008"),
    ("vhdl/drive_led_policy.vhd", "VHDL2008"),
    ("C128_MiSTer/rtl/iec_drive/iecdrv_via6522.vhd", "VHDL2008"),
    ("C128_MiSTer/rtl/iec_drive/iec_drive.sv", "SVerilog"),
    ("C128_MiSTer/rtl/iec_drive/c1541_gcr_codec.sv", "SVerilog"),
    ("C128_MiSTer/rtl/iec_drive/c1541_gcr.sv", "SVerilog"),
    ("C128_MiSTer/rtl/iec_drive/iecdrv_misc.sv", "SVerilog"),
    ("C128_MiSTer/rtl/iec_drive/iecdrv_rom.sv", "SVerilog"),
    ("C128_MiSTer/rtl/iec_drive/c157x_multi.sv", "SVerilog"),
    ("C128_MiSTer/rtl/iec_drive/c157x_drv.sv", "SVerilog"),
    ("C128_MiSTer/rtl/iec_drive/c157x_logic.sv", "SVerilog"),
    ("C128_MiSTer/rtl/iec_drive/c157x_h156.sv", "SVerilog"),
    ("C128_MiSTer/rtl/iec_drive/c157x_heads.sv", "SVerilog"),
    ("C128_MiSTer/rtl/iec_drive/c157x_track.sv", "SVerilog"),
    ("C128_MiSTer/rtl/iec_drive/c157x_fdc1772.sv", "SVerilog"),
    ("C128_MiSTer/rtl/iec_drive/c1581_multi.sv", "SVerilog"),
    ("C128_MiSTer/rtl/iec_drive/c1581_drv.sv", "SVerilog"),
    ("C128_MiSTer/rtl/iec_drive/c1581_fdc1772.v", "SVerilog"),
    ("C128_MiSTer/rtl/iec_drive/floppy.v", "SVerilog"),
]

# Insert in front of the top level, which is always present in every project.
ANCHOR = '      <File Path="$PPRDIR/vhdl/mega65.vhd">\n'


def file_block(path, sftype):
    used_in = ['        <FileInfo SFType="%s">\n' % sftype,
               '          <Attr Name="UsedIn" Val="synthesis"/>\n']
    if sftype == "SVerilog":
        used_in.append('          <Attr Name="UsedIn" Val="implementation"/>\n')
    used_in.append('          <Attr Name="UsedIn" Val="simulation"/>\n')
    return ('      <File Path="$PPRDIR/%s">\n' % path
            + "".join(used_in)
            + "        </FileInfo>\n"
            + "      </File>\n")


def main(paths):
    for xpr in paths:
        with open(xpr, encoding="utf-8") as handle:
            text = handle.read()

        if ANCHOR not in text:
            sys.exit("ERROR: anchor not found in %s" % xpr)

        added = []
        repaired = []
        blocks = ""
        for path, sftype in SOURCES:
            # Vivado drops SFType whenever it considers the type to be the default for the
            # extension, which for .sv means plain Verilog. Put it back, otherwise opening
            # the project in the GUI compiles SystemVerilog as Verilog-2001.
            existing = re.search(
                r'(Path="\$PPRDIR/%s">\n\s*<FileInfo)( SFType="[^"]*")?'
                % re.escape(path), text)
            if existing:
                if existing.group(2) != ' SFType="%s"' % sftype:
                    text = (text[:existing.start()]
                            + existing.group(1) + ' SFType="%s"' % sftype
                            + text[existing.end():])
                    repaired.append(path)
                continue
            blocks += file_block(path, sftype)
            added.append(path)

        if not added and not repaired:
            print("%s: already up to date" % xpr)
            continue

        if blocks:
            text = text.replace(ANCHOR, blocks + ANCHOR, 1)
        with open(xpr, "w", encoding="utf-8") as handle:
            handle.write(text)
        print("%s: added %d source(s), repaired %d file type(s)"
              % (xpr, len(added), len(repaired)))


if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit("Usage: add_iec_drive_sources.py <project.xpr> [...]")
    main(sys.argv[1:])
