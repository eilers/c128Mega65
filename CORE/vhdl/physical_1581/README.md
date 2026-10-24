These files are copied unchanged from C64MEGA65 (MJoergen/C64MEGA65 `develop`,
commit 61fdbb2, by sy2002, GPL v3). They drive the MEGA65's internal 3.5" drive
as a 1581: `doc/internal-floppy.md` explains how they are wired into this core.

To update them, copy the newer versions over these and port any changes to
C64MEGA65's `fdc1772.v` physical-mode branch (in its C64_MiSTerMEGA65 submodule)
into `CORE/C128_MiSTer/rtl/iec_drive/c1581_fdc1772.v` by hand.
