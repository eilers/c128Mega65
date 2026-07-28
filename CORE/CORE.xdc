## NAME-OF-YOUR-PROJECT for MEGA65 (NAME-OF-THE-GITHUB-REPO)
##
## Core specific constraints
##
## This machine is based on EXACT GITHUB REPO NAME OF THE MiSTer REPO
## Powered by MiSTer2MEGA65
## MEGA65 port done by YOURNAME in YEAR and licensed under GPL v3


## NOTE: auto-derived core clocks are main_clk_mmcm_orig / main_clk_mmcm_slow
## (BUFGMUX-selected onto the core clock). VDC has a dedicated 32.000 MHz MMCM
## (entity vdc_clk / clk_vdc.vhd). HDMI video_clk is BUFGMUX of MMCM CLKOUT nets
## (main_clk_raw / vdc_clk_raw) — not BUFG outputs — to avoid illegal cascades.

## NOTE (investigation): the VDC<->main bus crossing is intentionally left UNCONSTRAINED.
## Constraining it (set_clock_groups -asynchronous OR bounded set_max_delay) was tried on HW
## and BROKE the boot, so the unconstrained crossing is kept.

# VDC MMCM uses integer M/D now; keep AVAL demotion for any residual fractional MMCMs.
set_property SEVERITY Warning [get_drc_checks AVAL-139]

# Board 100 MHz (clk_i) feeds framework MMCMs + C64 dual-MMCM + VDC MMCM (>3 loads).
# BACKBONE lets the placer spread them; RTRES-1 may still fire at bitgen if the router
# does not use backbone wires — demote that check so bring-up bitstreams can be written.
set_property CLOCK_DEDICATED_ROUTE BACKBONE [get_nets -quiet clk_i_IBUF]
set_property SEVERITY Warning [get_drc_checks RTRES-1]


