## NAME-OF-YOUR-PROJECT for MEGA65 (NAME-OF-THE-GITHUB-REPO)
##
## Core specific constraints
##
## This machine is based on EXACT GITHUB REPO NAME OF THE MiSTer REPO
## Powered by MiSTer2MEGA65
## MEGA65 port done by YOURNAME in YEAR and licensed under GPL v3


## NOTE: auto-derived core clocks are main_clk_mmcm_orig / main_clk_mmcm_slow
## (BUFGMUX-selected onto the core clock). VDC has a dedicated 32.000 MHz MMCM
## (entity vdc_clk / clk_vdc.vhd). HDMI video_clk and VGA vga_video_clk are each a BUFGMUX
## of the core clock net (main_clk_raw, i.e. behind the flicker-free mux) and vdc_clk_raw.

## "HDMI: Flicker-free" switches the core between main_clk_mmcm_orig (31.527778 MHz) and
## main_clk_mmcm_slow (31.448993 MHz) through a glitch-free BUFGMUX_CTRL. Without the
## case analysis below, both clocks propagate through that mux and STA has to analyse every
## core path twice plus all the bogus crossings between the two. Pinning the select to 0
## leaves only the faster of the two clocks, which is the pessimistic (and therefore safe)
## one to close timing on.
## Watch the implementation log for a "no pins matched" warning on the next line.
## A renamed hr_core_speed_reg[0] makes the case analysis stop applying, and static
## timing quietly starts analysing both core clocks again.
set_case_analysis 0 [get_pins CORE/hr_core_speed_reg[0]/Q]
create_generated_clock -name main_clk [get_pins CORE/clk_gen/i_clk_c64_orig/CLKOUT0]
create_generated_clock -name vdc_clk  [get_pins CORE/clk_vdc_gen/mmcm_adv_inst/CLKOUT0]

## video_clk is a BUFGMUX of main_clk_raw (which is itself a BUFGMUX_CTRL output) and
## vdc_clk_raw, so it sits one buffer cascade deep - the same depth the main_clk BUFG
## already had. If the router objects to that cascade, relax the routing here instead of
## restructuring the mux; taking main_clk_raw in front of the flicker-free mux again would
## silently desynchronise the VIC video path from the core.
# set_property CLOCK_DEDICATED_ROUTE ANY_CMT_COLUMN [get_nets -quiet CORE/main_clk_raw]

## The mux above outputs one clock at a time, but static timing follows both inputs unless
## the select is pinned. main_clk is 31.528 MHz and vdc_clk is 32.000 MHz, both derived
## from the 100 MHz board clock, so Vivado treats them as related and the worst alignment
## it has to close is 0.027 ns. That requirement landed on every flip-flop and line-buffer
## port downstream of the mux (ascal, the overlay, the HQ2x mixer): about 15,000 endpoints,
## worst slack -13 ns, total violation -64 us. route_design cannot meet it and spent
## 1 h 49 min on the R3 bitstream (CPU time 9 h 55 min) before giving up. The same stall
## is where the R6 route sat until it was killed.
##
## Pin the select to I0 (main_clk_raw). That is the VIC/HDMI clock and the slower of the
## two only by 0.47 ns/period; the intra-clock slack on both clocks is about 10 ns, so
## timing closed here still covers 80-column mode. The main_clk <-> vdc_clk max delay
## further down does not replace this: without the pin, every path inside the video
## pipeline would become a main/VDC crossing too.
##
## The global clock multiplexer BUFGMUX_CTRL lowers to the primitive clock-control
## buffer BUFGCTRL. Both select pins sit on the same net (main_video_sel_vdc); S0 is
## inverted inside the cell and S1 is not. A logical 0 on both pins is "select I0".
## Pinning only one pin leaves the other select free, and static timing still follows
## vdc_clk. The constraint parser in this file rejects Tcl control flow, so the
## pins are pinned directly. A "no pins matched" warning on the next line means the
## select was renamed and the phantom crossing is back.
set_case_analysis 0 [get_pins {CORE/i_video_clk_mux/S0 CORE/i_video_clk_mux/S1}]

## The VGA port has its own copy of that mux (i_vga_clk_mux), pinned the same way for the
## same reasons. With both pinned, STA sees the HDMI and VGA video clocks as one clock, so a
## path between the two streams would go unchecked: keep them separate in the RTL.
set_case_analysis 0 [get_pins {CORE/i_vga_clk_mux/S0 CORE/i_vga_clk_mux/S1}]

## The C64 orig/slow and VDC MMCMs all feed BUFGs that cascade into both video muxes, so they
## must share a half of the device, and the bottom half of the clk_i column has exactly three
## MMCM sites. With two video muxes the clock placer no longer finds that on its own.
set_property LOC MMCME2_ADV_X0Y0 [get_cells CORE/clk_gen/i_clk_c64_orig]
set_property LOC MMCME2_ADV_X0Y2 [get_cells CORE/clk_gen/i_clk_c64_slow]
set_property LOC MMCME2_ADV_X0Y1 [get_cells CORE/clk_vdc_gen/mmcm_adv_inst]

## ---------------------------------------------------------------------------
## main_clk <-> vdc_clk: bounded, every crossing safe by design
## ---------------------------------------------------------------------------
## Both clocks come from the board clock, so STA asks these paths for 0.027 ns, the closest
## the two clocks' edges ever get. Nothing can meet that. What crosses:
##
##   VDC register bus  Address, data and R/W are stable from before CYCLE_CPU0 to the end
##                     of the CPU cycle. The VDC takes them 1-2 vdc_clk after enableVdc
##                     crosses, and the CPUs latch db_out at CPU3 at the earliest.
##                     Both directions have over 100 ns to spare.
##   Resets, light pen Two-flop synchronizers (fpga64_sid_iec.vhd, main.vhd).
##   VDC video out     Same-clock in 80-column mode, where the video mux selects vdc_clk.
##                     The case analysis above makes STA time it against main_clk instead.
##
## 15 ns is far inside the bus margin and leaves the video paths room for clock skew.
## vdc_clk -> main_clk keeps clock skew and the hold check: the video paths need hold, and
## main_clk reaches the video mux later than vdc_clk, so this hold check is the stricter one.
set_max_delay -datapath_only 15 -from [get_clocks main_clk] -to [get_clocks vdc_clk]
set_max_delay 15 -from [get_clocks vdc_clk] -to [get_clocks main_clk]

# VDC MMCM uses integer M/D now; keep AVAL demotion for any residual fractional MMCMs.
set_property SEVERITY Warning [get_drc_checks AVAL-139]



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




## ---------------------------------------------------------------------------
## main_clk <-> hr_clk: the virtual cartridge in SDRAM (R4 and later; R3 has none)
## ---------------------------------------------------------------------------
## A request crosses as a key and a Gray count, the count through two flip-flops. The reader
## takes the key once the count has arrived, two hr_clk or more after both were set. The byte
## comes back unsynchronized: the reader has it about 140 ns after the request, 80 ns before
## the C128 latches it in FAST mode, and it holds until the next request.
set_max_delay -datapath_only 8  -from [get_cells -quiet -hierarchical -filter {NAME =~ "*i_sw_cartridge_wrapper/sdram_gen.main_*_r_reg*"}] -to [get_clocks hr_clk]
set_max_delay -datapath_only 30 -from [get_cells -quiet -hierarchical -filter {NAME =~ "*i_sw_cartridge_wrapper/sdram_gen.i_crt_reader/*_data_o_reg*"}] -to [get_clocks main_clk]
