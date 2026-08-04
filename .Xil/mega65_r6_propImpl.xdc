set_property SRC_FILE_INFO {cfile:/var/home/bazzite/Dokumente/Developer/c128Mega65/M2M/MEGA65-R6.xdc rfile:../M2M/MEGA65-R6.xdc id:1} [current_design]
set_property SRC_FILE_INFO {cfile:/var/home/bazzite/Dokumente/Developer/c128Mega65/M2M/common.xdc rfile:../M2M/common.xdc id:2} [current_design]
set_property SRC_FILE_INFO {cfile:/var/home/bazzite/Dokumente/Developer/c128Mega65/CORE/CORE.xdc rfile:../CORE/CORE.xdc id:3} [current_design]
set_property src_info {type:XDC file:1 line:369 export:INPUT save:INPUT read:READ} [current_design]
create_pblock pblock_i_hyperram
add_cells_to_pblock [get_pblocks pblock_i_hyperram] [get_cells -quiet [list i_framework/i_hyperram]]
resize_pblock [get_pblocks pblock_i_hyperram] -add {SLICE_X0Y200:SLICE_X7Y224}
set_property src_info {type:XDC file:1 line:374 export:INPUT save:INPUT read:READ} [current_design]
create_pblock pblock_m65driver
add_cells_to_pblock [get_pblocks pblock_m65driver] [get_cells -quiet [list i_framework/i_m2m_keyb/m65driver]]
resize_pblock [get_pblocks pblock_m65driver] -add {SLICE_X0Y225:SLICE_X7Y243}
set_property src_info {type:XDC file:1 line:379 export:INPUT save:INPUT read:READ} [current_design]
create_pblock pblock_sdcard
add_cells_to_pblock [get_pblocks pblock_sdcard] [get_cells -quiet [list i_framework/i_qnice_wrapper/QNICE_SOC/sd_card]]
resize_pblock [get_pblocks pblock_sdcard] -add {SLICE_X66Y178:SLICE_X99Y193}
set_property src_info {type:XDC file:1 line:384 export:INPUT save:INPUT read:READ} [current_design]
create_pblock pblock_vga
add_cells_to_pblock [get_pblocks pblock_vga] [get_cells i_framework/i_av_pipeline/i_analog_pipeline/VGA_OUT_PHASE_SHIFTED.*]
resize_pblock [get_pblocks pblock_vga] -add {SLICE_X0Y75:SLICE_X5Y99}
set_property src_info {type:XDC file:2 line:25 export:INPUT save:INPUT read:READ} [current_design]
set_case_analysis 1 [get_pins i_framework/i_video_out_clock/clk_mux_reg/Q]
set_property src_info {type:XDC file:2 line:32 export:INPUT save:INPUT read:READ} [current_design]
set_max_delay 8 -datapath_only -from [get_generated_clocks] -to [get_pins -hierarchical "*cdc_stable_gen.dst_*_d_reg[*]/D"]
set_property src_info {type:XDC file:2 line:33 export:INPUT save:INPUT read:READ} [current_design]
set_max_delay 8 -datapath_only -from [get_clocks clk] -to [get_pins -hierarchical "*cdc_stable_gen.dst_*_d_reg[*]/D"]
set_property src_info {type:XDC file:2 line:76 export:INPUT save:INPUT read:READ} [current_design]
set_max_delay 2 -datapath_only -from [get_cells i_framework/i_hyperram/hyperram_ctrl_inst/hb_read_o_reg]
set_property src_info {type:XDC file:2 line:77 export:INPUT save:INPUT read:READ} [current_design]
set_max_delay 2 -datapath_only -from [get_cells i_framework/i_hyperram/hyperram_rx_inst/iddr_dq_gen[*].iddr_dq_inst]
set_property src_info {type:XDC file:2 line:82 export:INPUT save:INPUT read:READ} [current_design]
set_max_delay 2 -datapath_only -from [get_clocks hr_rwds] -to [get_clocks hr_clk]
set_property src_info {type:XDC file:2 line:97 export:INPUT save:INPUT read:READ} [current_design]
set_multicycle_path -from [get_cells -include_replicated {i_framework/i_qnice_wrapper/QNICE_SOC/eae_inst/op*_reg[*]}] -to [get_cells -include_replicated {i_framework/i_qnice_wrapper/QNICE_SOC/eae_inst/res_reg[*]}] -hold 2
set_property src_info {type:XDC file:3 line:24 export:INPUT save:INPUT read:READ} [current_design]
set_case_analysis 0 [get_pins {CORE/hr_core_speed_reg[0]/Q}]
