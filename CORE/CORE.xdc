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


## ---------------------------------------------------------------------------
## qnice_clk <-> main_clk: asynchronous, every crossing handled in the RTL
## ---------------------------------------------------------------------------
## These two clocks come from different MMCMs and have no phase relationship, so STA
## computes a meaningless requirement for the crossings between them (0.088 ns for the
## pair as generated here) and reports them as violations no matter how the design is
## placed. Every crossing is synchronized in the source, so cut the pair in both
## directions. This is what the C64 MEGA65 core does for the identical drive hierarchy.
##
## The crossings this covers, and why each is safe:
##
##   M2M framework   xpm_cdc_* instances (including i_cdc_main2qnice_rst in mega65.vhd,
##                   which is what makes the drive-ROM server wait for the core reset)
##                   and ascal's quasi-static mode register.
##
##   vdrives.vhd     Its own xpm_cdc chains towards the core, plus the MiSTer SD
##                   handshake: sd_lba/sd_blk_cnt are stable long before sd_rd/sd_wr
##                   rise and sd_ack stays high for the whole transfer.
##
##   iec_drive.sv    img_ds/img_gcr/img_mfm/img_hd and rom_bank change only on a
##                   (re-)mount, hundreds of milliseconds before anything reads them.
##
##   c157x_track     Four iecdrv_sync 2-FF chains inbound (track, change, save, reset)
##                   and busy_sync outbound. A 2-FF synchronizer is by construction a
##                   path that cannot meet setup.
##
##   c157x_heads     track_len and the bit rate derived from VIA2's PCR are shared
##                   between both domains without a synchronizer, but they only move
##                   while sd_busy gates the head machine, i.e. during a track load.
##
##   c1581_fdc1772   SD FSM and sd_lba latch on clk_sys (qnice_clk); request/done
##                   cross via iecdrv_sync. FIFO is dual-clock (clk_sys / main_clk).
##
## A clock-to-clock false path can never exempt a same-clock path, so the intra-domain
## logic of all of the above is still fully analysed.
##
## This deliberately replaces the earlier per-instance exceptions. Those matched cells by
## hierarchical name and went silently dead the moment iec_drive moved into a generate
## block, which is precisely the failure mode a name-based exception invites.
set_false_path -from [get_clocks qnice_clk] -to [get_clocks main_clk]
set_false_path -from [get_clocks main_clk]  -to [get_clocks qnice_clk]


