## NAME-OF-YOUR-PROJECT for MEGA65 (NAME-OF-THE-GITHUB-REPO)
##
## Core specific constraints
##
## This machine is based on EXACT GITHUB REPO NAME OF THE MiSTer REPO
## Powered by MiSTer2MEGA65
## MEGA65 port done by YOURNAME in YEAR and licensed under GPL v3


## NOTE: the auto-derived core clocks are main_clk_mmcm_orig / main_clk_mmcm_slow
## (BUFGMUX-selected onto the core clock) and vdc runs on main_clk after H-V33. The old
## create_generated_clock renames to "main_clk"/"vdc_clk" matched nothing (you cannot rename
## a propagated BUFG clock that way) and no constraint references those names, so they were
## removed -- they only produced [Constraints 18-851] critical warnings.

## NOTE (investigation): the VDC<->main crossing is intentionally left UNCONSTRAINED.
## Constraining it (set_clock_groups -asynchronous OR bounded set_max_delay) was tried on HW
## and BROKE the boot, so the unconstrained crossing is kept.

# VDC MMCM uses a fractional multiply value that Vivado rounds internally.
# Treat this as warning to avoid blocking bitstream generation.
set_property SEVERITY Warning [get_drc_checks AVAL-139]

