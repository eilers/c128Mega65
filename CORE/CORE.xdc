## NAME-OF-YOUR-PROJECT for MEGA65 (NAME-OF-THE-GITHUB-REPO)
##
## Core specific constraints
##
## This machine is based on EXACT GITHUB REPO NAME OF THE MiSTer REPO
## Powered by MiSTer2MEGA65
## MEGA65 port done by YOURNAME in YEAR and licensed under GPL v3


## NOTE: auto-derived core clocks are main_clk_mmcm_orig / main_clk_mmcm_slow
## (BUFGMUX-selected onto the core clock). VDC has a dedicated 32.000 MHz MMCM
## (entity vdc_clk / clk_vdc.vhd). HDMI video_clk is BUFGMUX of the core clock net
## (main_clk_raw, i.e. behind the flicker-free mux) and vdc_clk_raw.

## "HDMI: Flicker-free" switches the core between main_clk_mmcm_orig (31.527778 MHz) and
## main_clk_mmcm_slow (31.448993 MHz) through a glitch-free BUFGMUX_CTRL. Without the
## case analysis below, both clocks propagate through that mux and STA has to analyse every
## core path twice plus all the bogus crossings between the two. Pinning the select to 0
## leaves only the faster of the two clocks, which is the pessimistic (and therefore safe)
## one to close timing on.
## Watch the implementation log for a "no pins matched" warning on the next line: if
## synthesis ever renames hr_core_speed_reg[0], the case analysis silently stops applying
## and STA quietly starts analysing both core clocks again.
set_case_analysis 0 [get_pins CORE/hr_core_speed_reg[0]/Q]
create_generated_clock -name main_clk [get_pins CORE/clk_gen/i_clk_c64_orig/CLKOUT0]

## video_clk is a BUFGMUX of main_clk_raw (which is itself a BUFGMUX_CTRL output) and
## vdc_clk_raw, so it sits one buffer cascade deep - the same depth the main_clk BUFG
## already had. If the router objects to that cascade, relax the routing here instead of
## restructuring the mux; taking main_clk_raw in front of the flicker-free mux again would
## silently desynchronise the VIC video path from the core.
# set_property CLOCK_DEDICATED_ROUTE ANY_CMT_COLUMN [get_nets -quiet CORE/main_clk_raw]

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


