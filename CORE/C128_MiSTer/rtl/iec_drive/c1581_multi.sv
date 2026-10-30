//-------------------------------------------------------------------------------
//
// C1541 multi-drive implementation with shared ROM
// (C) 2021 Alexey Melnikov
//
// Input clock/ce 16MHz
//
//-------------------------------------------------------------------------------


module c1581_multi #(parameter PARPORT=1,DRIVES=2)
(
	//clk ports
	input         clk,
	input   [N:0] reset,
	input   [1:0] drv_id[NDR],      // MEGA65: bus ID - 8
	input         ce,

	input         pause,

	input   [N:0] img_mounted,
	input         img_readonly,
	input  [31:0] img_size,

	output  [N:0] act_led,
	output  [N:0] pwr_led,

	input         iec_atn_i,
	input         iec_data_i,
	input         iec_clk_i,
	input         iec_fclk_i,
	output        iec_data_o,
	output        iec_clk_o,
	output        iec_fclk_o,

	// parallel bus
	input   [7:0] par_data_i,
	input         par_stb_i,
	output logic [7:0] par_data_o,
	output        par_stb_o,

	//clk_sys ports
	input         clk_sys,

	output [31:0] sd_lba[NDR],
	output  [N:0] sd_rd,
	output  [N:0] sd_wr,
	input   [N:0] sd_ack,
	input   [8:0] sd_buff_addr,
	input   [7:0] sd_buff_dout,
	output  [7:0] sd_buff_din[NDR],
	input         sd_buff_wr,
	
	output  [7:0] out_track[NDR],
	output  [N:0] out_we,

	// input  [14:0] rom_addr,
	// input   [7:0] rom_data,
	// input         rom_wr,
	// input         rom_std

	output [14:0] mem_a[NDR],
	input   [7:0] rom_do[NDR],

	// MEGA65 internal floppy (ported from C64MEGA65 #90/#93/#244). There is
	// one mechanism and one controller: the drive whose phys_mode bit is set
	// drives the outputs below (at most one bit is set, and main.vhd passes
	// every change through an all-zero gap). The controller's outputs go to
	// all drives; a drive ignores them while its own phys_mode is 0.
	input   [N:0] phys_mode,
	output        phys_active,
	output        phys_cia_motor_on,
	output        phys_cia_side,
	output        phys_step_req_tgl,
	output        phys_step_outward,
	output        phys_rd_req_tgl,
	output  [2:0] phys_rd_op,
	output  [7:0] phys_rd_track,
	output        phys_rd_side,
	output  [7:0] phys_rd_sector,
	output        phys_rd_cancel_tgl,
	output  [1:0] phys_rd_seq,
	input         phys_step_ack_tgl,
	input         phys_rd_done_tgl,
	input   [1:0] phys_rd_done_seq,
	input   [4:0] phys_rd_result,
	input         phys_rd_crc_err,
	input         phys_rd_rnf,
	input         phys_rd_deleted,
	input   [7:0] phys_rd_c,
	output        phys_byte_rd_en,
	input   [7:0] phys_byte_data,
	input         phys_byte_empty,
	input         phys_media_ready,
	input         phys_index,
	input         phys_track0,
	input         phys_wprot,
	input         phys_change,
	input         phys_motor_on,
	input         phys_head_settled,
	output        phys_wr_deleted,
	output        phys_wr_precomp,
	output        phys_wr_byte_wr,
	output  [7:0] phys_wr_byte_data,
	input         phys_wr_fifo_full,
	input         phys_wr_feed,
	input         phys_wr_lost,
	input         phys_wr_busy,
	output        phys_dbg_lost_tgl,
	output        phys_dbg_drain_tgl,
	output        phys_dbg_staledone_tgl,
	output        phys_dbg_busycmd_tgl,
	output        phys_dbg_fin_tgl,
	output [10:0] phys_dbg_pres_cnt,
	output        phys_dbg_wr_fin_tgl,
	output        phys_dbg_wr_wp_tgl,
	output        phys_dbg_wr_drop_tgl
);

localparam NDR = (DRIVES < 1) ? 1 : (DRIVES > 4) ? 4 : DRIVES;
localparam N   = NDR - 1;

wire iec_atn, iec_data, iec_clk, iec_fclk;
iecdrv_sync atn_sync(clk,  iec_atn_i,  iec_atn);
iecdrv_sync dat_sync(clk,  iec_data_i, iec_data);
iecdrv_sync clk_sync(clk,  iec_clk_i,  iec_clk);
iecdrv_sync fclk_sync(clk, iec_fclk_i, iec_fclk);

wire [N:0] reset_drv;
iecdrv_sync #(NDR) rst_sync(clk, reset, reset_drv);

reg ph2_r;
reg ph2_f;
reg wd_ce;
always @(posedge clk) begin
	reg [2:0] div;
	reg       ena, ena1;

	ena1 <= ~pause;
	if(div[1:0]) ena <= ena1;

	ph2_r <= 0;
	ph2_f <= 0;
	wd_ce  <= 0;
	if(ce) begin
		div <= div + 1'd1;
		ph2_r <= ena && !div[2] && !div[1:0];
		ph2_f <= ena &&  div[2] && !div[1:0];
		wd_ce  <= ena && !div[0];
	end
end

// wire [7:0] rom_do;
// iecdrv_mem #(8,15,"rtl/iec_drive/c1581_rom.mif") rom
// (
// 	// .clock_a(clk_sys),
// 	// .address_a(rom_addr),
// 	// .data_a(rom_data),
// 	// .wren_a(rom_wr),

// 	.clock_b(clk),
// 	.address_b(mem_a),
// 	.q_b(rom_do)
// );

// reg  [14:0] mem_a;
// wire [14:0] drv_addr[NDR];
// reg   [7:0] drv_data[4];
// always @(posedge clk) begin
// 	reg [2:0] state;
// 	reg [14:0] mem_d;
	
// 	if(~&state) state <= state + 1'd1;
// 	if(ph2_f)   state <= 0;

// 	case(state)
// 		0,1,2,3: mem_a <= drv_addr[state[1:0]];
// 	endcase
	
// 	case(state)
// 		3,4,5,6: drv_data[state[1:0] - 2'd3] <= rom_do;
// 	endcase
// end

wire [N:0] iec_data_d, iec_clk_d, iec_fclk_d;
iecdrv_reset_filter #(NDR) rst_flt_clk  (clk, reset_drv, iec_clk_d, iec_clk_o);
iecdrv_reset_filter #(NDR) rst_flt_data (clk, reset_drv, iec_data_d, iec_data_o);
iecdrv_reset_filter #(NDR) rst_flt_fclk (clk, reset_drv, iec_fclk_d, iec_fclk_o);

wire [7:0] par_data_d[NDR];
wire [N:0] par_stb_d;
assign     par_stb_o = &{par_stb_d | reset_drv};
always_comb begin
	par_data_o = 8'hFF;
	for(int i=0; i<NDR; i=i+1) if(~reset_drv[i]) par_data_o = par_data_o & par_data_d[i];
end

wire [N:0] act_led_drv, pwr_led_drv;
assign     act_led = act_led_drv & ~reset_drv;
assign     pwr_led = pwr_led_drv & ~reset_drv;

// MEGA65 internal floppy: each drive's outputs, and the one drive picked by
// phys_mode (drive 0 when no bit is set; its outputs are inert then).
//
// The request toggles are never reset, so two drives generally park them at
// different levels, and a plain mux would show the controller a phantom
// request when the internal drive moves between 8 and 9. Each toggle output is
// therefore re-encoded: it flips only on a real event of the drive that is in
// physical mode. The levels and quasi-static data are muxed directly; they are
// only read while the controller is enabled, and the select never moves then.
wire [N:0] phys_active_d, phys_cia_motor_on_d, phys_cia_side_d;
wire [N:0] phys_step_req_tgl_d, phys_step_outward_d, phys_rd_req_tgl_d;
wire [N:0] phys_rd_side_d, phys_rd_cancel_tgl_d, phys_byte_rd_en_d;
wire [N:0] phys_dbg_lost_tgl_d, phys_dbg_drain_tgl_d, phys_dbg_staledone_tgl_d;
wire [N:0] phys_dbg_busycmd_tgl_d, phys_dbg_fin_tgl_d;
wire [N:0] phys_wr_deleted_d, phys_wr_precomp_d, phys_wr_byte_wr_d;
wire [N:0] phys_dbg_wr_fin_tgl_d, phys_dbg_wr_wp_tgl_d, phys_dbg_wr_drop_tgl_d;
wire [7:0] phys_wr_byte_data_d[NDR];
wire [2:0] phys_rd_op_d[NDR];
wire [7:0] phys_rd_track_d[NDR];
wire [7:0] phys_rd_sector_d[NDR];
wire [1:0] phys_rd_seq_d[NDR];
wire [10:0] phys_dbg_pres_cnt_d[NDR];

integer phys_sel;
always_comb begin
	phys_sel = 0;
	for(int k=1; k<NDR; k=k+1) if(phys_mode[k]) phys_sel = k;
end

reg [N:0] p_step_req_prev = '0, p_rd_req_prev = '0, p_rd_cancel_prev = '0;
reg [N:0] p_dbg_lost_prev = '0, p_dbg_drain_prev = '0, p_dbg_staledone_prev = '0;
reg [N:0] p_dbg_busycmd_prev = '0, p_dbg_fin_prev = '0;
reg [N:0] p_dbg_wr_fin_prev = '0, p_dbg_wr_wp_prev = '0, p_dbg_wr_drop_prev = '0;
reg p_step_req_out = 1'b0, p_rd_req_out = 1'b0, p_rd_cancel_out = 1'b0;
reg p_dbg_lost_out = 1'b0, p_dbg_drain_out = 1'b0, p_dbg_staledone_out = 1'b0;
reg p_dbg_busycmd_out = 1'b0, p_dbg_fin_out = 1'b0;
reg p_dbg_wr_fin_out = 1'b0, p_dbg_wr_wp_out = 1'b0, p_dbg_wr_drop_out = 1'b0;
always @(posedge clk) begin
	for(int k=0; k<NDR; k=k+1) begin
		if (phys_mode[k] && (phys_step_req_tgl_d[k]      != p_step_req_prev[k]))      p_step_req_out      <= ~p_step_req_out;
		if (phys_mode[k] && (phys_rd_req_tgl_d[k]        != p_rd_req_prev[k]))        p_rd_req_out        <= ~p_rd_req_out;
		if (phys_mode[k] && (phys_rd_cancel_tgl_d[k]     != p_rd_cancel_prev[k]))     p_rd_cancel_out     <= ~p_rd_cancel_out;
		if (phys_mode[k] && (phys_dbg_lost_tgl_d[k]      != p_dbg_lost_prev[k]))      p_dbg_lost_out      <= ~p_dbg_lost_out;
		if (phys_mode[k] && (phys_dbg_drain_tgl_d[k]     != p_dbg_drain_prev[k]))     p_dbg_drain_out     <= ~p_dbg_drain_out;
		if (phys_mode[k] && (phys_dbg_staledone_tgl_d[k] != p_dbg_staledone_prev[k])) p_dbg_staledone_out <= ~p_dbg_staledone_out;
		if (phys_mode[k] && (phys_dbg_busycmd_tgl_d[k]   != p_dbg_busycmd_prev[k]))   p_dbg_busycmd_out   <= ~p_dbg_busycmd_out;
		if (phys_mode[k] && (phys_dbg_fin_tgl_d[k]       != p_dbg_fin_prev[k]))       p_dbg_fin_out       <= ~p_dbg_fin_out;
		if (phys_mode[k] && (phys_dbg_wr_fin_tgl_d[k]    != p_dbg_wr_fin_prev[k]))    p_dbg_wr_fin_out    <= ~p_dbg_wr_fin_out;
		if (phys_mode[k] && (phys_dbg_wr_wp_tgl_d[k]     != p_dbg_wr_wp_prev[k]))     p_dbg_wr_wp_out     <= ~p_dbg_wr_wp_out;
		if (phys_mode[k] && (phys_dbg_wr_drop_tgl_d[k]   != p_dbg_wr_drop_prev[k]))   p_dbg_wr_drop_out   <= ~p_dbg_wr_drop_out;
		p_step_req_prev[k]      <= phys_step_req_tgl_d[k];
		p_rd_req_prev[k]        <= phys_rd_req_tgl_d[k];
		p_rd_cancel_prev[k]     <= phys_rd_cancel_tgl_d[k];
		p_dbg_lost_prev[k]      <= phys_dbg_lost_tgl_d[k];
		p_dbg_drain_prev[k]     <= phys_dbg_drain_tgl_d[k];
		p_dbg_staledone_prev[k] <= phys_dbg_staledone_tgl_d[k];
		p_dbg_busycmd_prev[k]   <= phys_dbg_busycmd_tgl_d[k];
		p_dbg_fin_prev[k]       <= phys_dbg_fin_tgl_d[k];
		p_dbg_wr_fin_prev[k]    <= phys_dbg_wr_fin_tgl_d[k];
		p_dbg_wr_wp_prev[k]     <= phys_dbg_wr_wp_tgl_d[k];
		p_dbg_wr_drop_prev[k]   <= phys_dbg_wr_drop_tgl_d[k];
	end
end

assign phys_active        = phys_active_d[phys_sel];
assign phys_cia_motor_on  = phys_cia_motor_on_d[phys_sel];
assign phys_cia_side      = phys_cia_side_d[phys_sel];
assign phys_step_req_tgl  = p_step_req_out;
assign phys_step_outward  = phys_step_outward_d[phys_sel];
assign phys_rd_req_tgl    = p_rd_req_out;
assign phys_rd_op         = phys_rd_op_d[phys_sel];
assign phys_rd_track      = phys_rd_track_d[phys_sel];
assign phys_rd_side       = phys_rd_side_d[phys_sel];
assign phys_rd_sector     = phys_rd_sector_d[phys_sel];
assign phys_rd_cancel_tgl = p_rd_cancel_out;
assign phys_rd_seq        = phys_rd_seq_d[phys_sel];
assign phys_byte_rd_en    = phys_byte_rd_en_d[phys_sel];
assign phys_wr_deleted    = phys_wr_deleted_d[phys_sel];
assign phys_wr_precomp    = phys_wr_precomp_d[phys_sel];
assign phys_wr_byte_wr    = phys_wr_byte_wr_d[phys_sel] & phys_mode[phys_sel];
assign phys_wr_byte_data  = phys_wr_byte_data_d[phys_sel];
assign phys_dbg_lost_tgl      = p_dbg_lost_out;
assign phys_dbg_drain_tgl     = p_dbg_drain_out;
assign phys_dbg_staledone_tgl = p_dbg_staledone_out;
assign phys_dbg_busycmd_tgl   = p_dbg_busycmd_out;
assign phys_dbg_fin_tgl       = p_dbg_fin_out;
assign phys_dbg_pres_cnt      = phys_dbg_pres_cnt_d[phys_sel];
assign phys_dbg_wr_fin_tgl    = p_dbg_wr_fin_out;
assign phys_dbg_wr_wp_tgl     = p_dbg_wr_wp_out;
assign phys_dbg_wr_drop_tgl   = p_dbg_wr_drop_out;

generate
	genvar i;
	for(i=0; i<NDR; i=i+1) begin :drives
		c1581_drv c1581_drv
		(
			.clk(clk),
			.reset(reset_drv[i]),

			.ce(ce),
			.wd_ce(wd_ce),
			.ph2_r(ph2_r),
			.ph2_f(ph2_f),

			.img_mounted(img_mounted[i]),
			.img_readonly(img_readonly),
			.img_size(img_size),

			.drive_num(drv_id[i]),
			.act_led(act_led_drv[i]),
			.pwr_led(pwr_led_drv[i]),

			.iec_atn_i(iec_atn),
			.iec_data_i(iec_data & iec_data_o),
			.iec_clk_i(iec_clk & iec_clk_o),
			.iec_fclk_i(iec_fclk & iec_fclk_o),
			.iec_data_o(iec_data_d[i]),
			.iec_clk_o(iec_clk_d[i]),
			.iec_fclk_o(iec_fclk_d[i]),

			.par_data_i(par_data_i),
			.par_stb_i(par_stb_i),
			.par_data_o(par_data_d[i]),
			.par_stb_o(par_stb_d[i]),

			.rom_addr(mem_a[i]),
			.rom_data(rom_do[i]),

			.clk_sys(clk_sys),

			.sd_lba(sd_lba[i]),
			.sd_rd(sd_rd[i]),
			.sd_wr(sd_wr[i]),
			.sd_ack(sd_ack[i]),
			.sd_buff_addr(sd_buff_addr),
			.sd_buff_dout(sd_buff_dout),
			.sd_buff_din(sd_buff_din[i]),
			.sd_buff_wr(sd_buff_wr),
			.out_track(out_track[i]),
			.out_we(out_we[i]),

			.phys_mode             ( phys_mode[i]               ),
			.phys_active           ( phys_active_d[i]           ),
			.phys_cia_motor_on     ( phys_cia_motor_on_d[i]     ),
			.phys_cia_side         ( phys_cia_side_d[i]         ),
			.phys_step_req_tgl     ( phys_step_req_tgl_d[i]     ),
			.phys_step_outward     ( phys_step_outward_d[i]     ),
			.phys_rd_req_tgl       ( phys_rd_req_tgl_d[i]       ),
			.phys_rd_op            ( phys_rd_op_d[i]            ),
			.phys_rd_track         ( phys_rd_track_d[i]         ),
			.phys_rd_side          ( phys_rd_side_d[i]          ),
			.phys_rd_sector        ( phys_rd_sector_d[i]        ),
			.phys_rd_cancel_tgl    ( phys_rd_cancel_tgl_d[i]    ),
			.phys_rd_seq           ( phys_rd_seq_d[i]           ),
			.phys_step_ack_tgl     ( phys_step_ack_tgl          ),
			.phys_rd_done_tgl      ( phys_rd_done_tgl           ),
			.phys_rd_done_seq      ( phys_rd_done_seq           ),
			.phys_rd_result        ( phys_rd_result             ),
			.phys_rd_crc_err       ( phys_rd_crc_err            ),
			.phys_rd_rnf           ( phys_rd_rnf                ),
			.phys_rd_deleted       ( phys_rd_deleted            ),
			.phys_rd_c             ( phys_rd_c                  ),
			.phys_byte_rd_en       ( phys_byte_rd_en_d[i]       ),
			.phys_byte_data        ( phys_byte_data             ),
			.phys_byte_empty       ( phys_byte_empty            ),
			.phys_media_ready      ( phys_media_ready           ),
			.phys_index            ( phys_index                 ),
			.phys_track0           ( phys_track0                ),
			.phys_wprot            ( phys_wprot                 ),
			.phys_change           ( phys_change                ),
			.phys_motor_on         ( phys_motor_on              ),
			.phys_head_settled     ( phys_head_settled          ),
			.phys_wr_deleted       ( phys_wr_deleted_d[i]       ),
			.phys_wr_precomp       ( phys_wr_precomp_d[i]       ),
			.phys_wr_byte_wr       ( phys_wr_byte_wr_d[i]       ),
			.phys_wr_byte_data     ( phys_wr_byte_data_d[i]     ),
			.phys_wr_fifo_full     ( phys_wr_fifo_full          ),
			.phys_wr_feed          ( phys_wr_feed               ),
			.phys_wr_lost          ( phys_wr_lost               ),
			.phys_wr_busy          ( phys_wr_busy               ),
			.phys_dbg_lost_tgl     ( phys_dbg_lost_tgl_d[i]     ),
			.phys_dbg_drain_tgl    ( phys_dbg_drain_tgl_d[i]    ),
			.phys_dbg_staledone_tgl( phys_dbg_staledone_tgl_d[i]),
			.phys_dbg_busycmd_tgl  ( phys_dbg_busycmd_tgl_d[i]  ),
			.phys_dbg_fin_tgl      ( phys_dbg_fin_tgl_d[i]      ),
			.phys_dbg_pres_cnt     ( phys_dbg_pres_cnt_d[i]     ),
			.phys_dbg_wr_fin_tgl   ( phys_dbg_wr_fin_tgl_d[i]   ),
			.phys_dbg_wr_wp_tgl    ( phys_dbg_wr_wp_tgl_d[i]    ),
			.phys_dbg_wr_drop_tgl  ( phys_dbg_wr_drop_tgl_d[i]  )
		);
	end
endgenerate

endmodule
