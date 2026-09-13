//-------------------------------------------------------------------------------
//
// Reworked and adapted to MiSTer by Sorgelig@MiSTer (07.09.2018)
//
// Commodore 1541/157x to SD card by Dar (darfpga@aol.fr)
// http://darfpga.blogspot.fr
//
// c1541_logic    from : Mark McDougall
// via6522        from : Gideon Zweijtzer  <gideon.zweijtzer@gmail.com>
// c1541_track    from : Sorgelig@MiSTer
//
// c1541_logic    modified for : slow down CPU (EOI ack missed by real c64)
//                             : remove iec internal OR wired
//                             : synched atn_in (sometime no IRQ with real c64)
//
// Input clk 16MHz
//
// Extended with support for 157x models by Erik Scheffers
//
//-------------------------------------------------------------------------------
//
// Adjusted for the MEGA65 port (Vivado instead of Quartus):
//
//   reset_drv was declared as a wire and then assigned inside an always block.
//   Quartus accepts that, Vivado rejects it outright, so it is a reg now.
//
//-------------------------------------------------------------------------------

module c157x_drv #(parameter DRIVE)
(
	//clk ports
	input         clk,
	input         reset,

	input   [1:0] drv_mode,

	input         ce,
	input         wd_ce,
	input   [1:0] ph2_r,
	input   [1:0] ph2_f,

	input         img_mounted,
	input         img_readonly,
	input  [31:0] img_size,
	output    reg disk_ready,
	output  [7:0] out_track,
	output        out_we,
	
	input         img_ds,
	input         img_gcr,
	input         img_mfm,

	output        led,

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
	output  [7:0] par_data_o,
	output        par_stb_o,

	input         ext_en,
	output [14:0] rom_addr,
	input   [7:0] rom_data,

	//clk_sys ports
	input         clk_sys,

	output [31:0] sd_lba,
	output  [5:0] sd_blk_cnt,
	output        sd_rd,
	output        sd_wr,
	input         sd_ack,
	input  [15:0] sd_buff_addr,
	input   [7:0] sd_buff_dout,
	output  [7:0] sd_buff_din,
	input         sd_buff_wr
);

localparam SD_BLK_CNT_1541 = 31;
localparam SD_BLK_CNT_157X = 52;

// The physical drive LED is controlled by VIA2 PB3, just like the real drive.
// sd_busy is only a host-buffer handshake and must not fill DOS error-blink gaps.
assign led = act;

// MEGA65 port: old_mounted and present lived inside the always block. Vivado infers
// registers for those, xsim re-initialises them on every invocation, so the mount edge
// is never detected and the drive never sees a disk. See iecdrv_rom.sv for the same
// trap. ch_timeout also needs its initialiser, or an X there swallows the timeout.
reg        readonly = 0;
reg        disk_present = 0;
reg [24:0] ch_timeout = 0;
reg        old_mounted = 0;
reg        present = 0;
always @(posedge clk) begin

	if(ce && ch_timeout > 0) ch_timeout <= ch_timeout - 1'd1;
	// ch_timeout[23] drives the write-protect transitions that tell DOS a disk
	// changed. Make the image readable at the final transition (01 -> 00 in the
	// top two counter bits), not only when the remaining quarter of the timeout
	// reaches zero. Otherwise DOS starts its D71 side-1 probe while the GCR path
	// is still forced busy by ~disk_present, records a single-sided disk, and
	// receives no later change indication after data finally becomes available.
	if(ch_timeout[24:23] == 2'b00) disk_present <= present;
	disk_ready <= !ch_timeout;

	old_mounted <= img_mounted;
	if (~old_mounted & img_mounted) begin
		ch_timeout <= '1;
		readonly <= img_readonly;
		present <= |img_size;
		disk_present <= 0;
	end
end

// reset drive when drive mode changes. The drive powers up held in reset, which is
// also what the FPGA does: reset is high until the M2M Shell reports a mounted image.
reg reset_drv = 1;
reg [1:0] last_drv_mode = 0;
reg [3:0] reset_hold = 0;
always @(posedge clk) begin

	if (reset) begin
		last_drv_mode <= drv_mode;
		reset_hold    <= 0;
		reset_drv	  <= 1;
	end
	else if (ph2_r[0]) begin
		last_drv_mode <= drv_mode;
		if (last_drv_mode != drv_mode) begin
			reset_hold <= '1;
			reset_drv  <= 1;
		end
		else if (reset_hold)
			reset_hold <= reset_hold - 1'd1;
		else
			reset_drv  <= 0;
	end
end

wire       mode, wgate;
wire [1:0] stp;
wire       mtr;
wire       act;
wire       fdc_busy;
wire [1:0] freq;

// MEGA65 port: everything from here down to save_track is declared before the three
// instances that follow, because a port connection naming an identifier ahead of its
// declaration makes Vivado create a 1-bit implicit net instead. track is the one that
// mattered: as a single bit it left the head permanently reporting track 0.
wire       hinit, hclk, hf, ht, index, we, write, sd_update;
wire       side;
wire       busy;
reg  [7:0] track;
reg        save_track = 0;
reg        track_modified = 0;
reg  [6:0] track_num = 36;
reg  [1:0] move = 0, stp_old = 0;
reg        side_old = 0;
wire       drive_enable = disk_present & mtr;
// Declared ahead of c157x_logic for the same reason as track above: a port connection
// that names an identifier before its declaration silently becomes an implicit net.
wire       sd_busy;
iecdrv_sync busy_sync(clk, busy, sd_busy);
wire       sector_mode = img_gcr & ~img_mfm;
// A head bump can step past track 35. The linear sector table only covers a real disk,
// so clamp before adding the side offset instead of addressing past the end of a D64/D71.
wire [6:0] sector_track_raw = {1'b0, track_num[6:1]} + 7'd1;
wire [6:0] sector_track = (sector_track_raw > 7'd35 ? 7'd35 : sector_track_raw) +
                          ((img_ds & side) ? 7'd35 : 7'd0);
wire [5:0] raw_blk_cnt = 6'(|drv_mode ? SD_BLK_CNT_157X : SD_BLK_CNT_1541);

wire [7:0] sector_gcr_do, sector_gcr_di, sector_sd_buff_din;
wire       sector_gcr_sync_n, sector_gcr_byte_n, sector_gcr_we;
wire       sector_sd_bank;
wire [7:0] heads_sd_buff_din;

c157x_logic #(.DRIVE(DRIVE)) c157x_logic
(
	.clk(clk),
	.reset(reset_drv),
	.drv_mode(drv_mode),

	.wd_ce(wd_ce),
	.ph2_r(ph2_r),
	.ph2_f(ph2_f),

	// serial bus
	.iec_clk_in(iec_clk_i),
	.iec_data_in(iec_data_i),
	.iec_atn_in(iec_atn_i),
	.iec_fclk_in(iec_fclk_i),
	.iec_clk_out(iec_clk_o),
	.iec_data_out(iec_data_o),
	.iec_fclk_out(iec_fclk_o),

	.ext_en(ext_en),
	.rom_addr(rom_addr),
	.rom_data(rom_data),

	// parallel bus
	.par_data_in(par_data_i),
	.par_stb_in(par_stb_i),
	.par_data_out(par_data_o),
	.par_stb_out(par_stb_o),

	// drive signals
	.wps_n(~readonly ^ ch_timeout[23]),
	.act(act),
	.side(side),
	.mode(mode),
	.wgate(wgate),
	.fdc_busy(fdc_busy),

	// .din(dgcr_do),
	// .dout(gcr_di),
	// .mode(mode),
	.stp(stp),
	.mtr(mtr),
	.freq(freq),
	// .soe(soe),
	// .ted(ted),
	// .sync_n(dgcr_sync_n),
	// .byte_n(dgcr_byte_n),

	.hinit(hinit),
	.hclk(hclk),
	.hf(hf),
	.ht(ht),
	.tr00_sense(~|track),
	.index_sense(index),
	.drive_enable(drive_enable),
	.disk_present(disk_present),

	.img_mfm(img_mfm),
	.sector_gcr_enable(sector_mode),
	.sector_gcr_dout(sector_gcr_do),
	.sector_gcr_sync_n(sector_gcr_sync_n),
	.sector_gcr_byte_n(sector_gcr_byte_n),
	.sector_gcr_din(sector_gcr_di)
);

// wire  [7:0] gcr_di;
// assign      sd_buff_din = /*gcr_mode ? dgcr_sd_buff_dout : gcr_sd_buff_dout*/ dgcr_sd_buff_dout;

// wire [7:0]  gcr_do, gcr_sd_buff_dout;
// wire        gcr_sync_n, gcr_byte_n, gcr_we;

// c1541_gcr c1541_gcr
// (
// 	.clk(clk),
// 	.ce(ce & ~gcr_mode),

// 	.dout(gcr_do),
// 	.din(gcr_di),
// 	.mode(mode),
// 	.mtr(mtr),
// 	.freq(freq),
// 	.sync_n(gcr_sync_n),
// 	.byte_n(gcr_byte_n),

// 	.track(track[6:1]+1'd1),
// 	.busy(sd_busy | ~disk_present),
// 	.we(gcr_we),

// 	.sd_clk(clk_sys),
// 	.sd_lba(sd_lba),
// 	.sd_buff_addr(sd_buff_addr[12:0]),
// 	.sd_buff_dout(sd_buff_dout),
// 	.sd_buff_din(gcr_sd_buff_dout),
// 	.sd_buff_wr(sd_ack & sd_buff_wr & ~gcr_mode)
// );

// wire [7:0] dgcr_do, dgcr_sd_buff_dout;
// wire       dgcr_sync_n, dgcr_byte_n, dgcr_we, dgcr_index_n;

// c1541_direct_gcr c1541_direct_gcr
// (
// 	.clk(clk),
// 	.ce(ce /*& gcr_mode*/),
// 	.reset(reset_drv),

// 	// .dout(dgcr_do),
// 	// .din(gcr_di),
// 	.mode(mode),
// 	.mtr(mtr),
// 	.freq(freq),
// 	// .soe(soe),
// 	// .ted(ted),
// 	// .sync_n(dgcr_sync_n),
// 	// .byte_n(dgcr_byte_n),
// 	// .index_n(dgcr_index_n),

// 	.busy(sd_busy | ~disk_present),
// 	.we(dgcr_we),

// 	.sd_clk(clk_sys),
// 	.sd_buff_addr(sd_buff_addr),
// 	.sd_buff_dout(sd_buff_dout),
// 	.sd_buff_din(dgcr_sd_buff_dout),
// 	.sd_buff_wr(sd_ack & sd_buff_wr /*& gcr_mode*/)
// );

c1541_gcr sector_gcr
(
	.clk(clk),
	.ce(ce & sector_mode),
	.dout(sector_gcr_do),
	.din(sector_gcr_di),
	.mode(mode),
	.mtr(mtr),
	.freq(freq),
	.sync_n(sector_gcr_sync_n),
	.byte_n(sector_gcr_byte_n),
	.track(sector_track),
	.busy(sd_busy | ~disk_present),
	.we(sector_gcr_we),
	.sd_clk(clk_sys),
	.sd_lba(sd_lba),
	.sd_bank(sector_sd_bank),
	.sd_buff_addr(sd_buff_addr[12:0]),
	.sd_buff_dout(sd_buff_dout),
	.sd_buff_din(sector_sd_buff_din),
	.sd_buff_wr(sd_ack & sd_buff_wr & sector_mode)
);

assign sd_buff_din = sector_mode ? sector_sd_buff_din : heads_sd_buff_din;

c157x_heads #(.DRIVE(DRIVE), .TRACK_BUF_LEN(SD_BLK_CNT_157X*256)) c157x_heads
(
	.clk(clk),
	.ce(ce),
	.reset(reset_drv),
	.enable(drive_enable & ~sector_mode),
	.img_ds(img_ds),
	.img_gcr(img_gcr),
	.img_mfm(img_mfm),

	.freq(freq),
	.side(side),
	.mode(mode),
	.wgate(wgate),
	.write(write),

	.hinit(hinit),
	.hclk(hclk),
	.hf(hf),
	.ht(ht),

	.index(index),

	.sd_busy(sd_busy),
	.sd_clk(clk_sys),
	.sd_buff_addr(sd_buff_addr),
	.sd_buff_dout(sd_buff_dout),
	.sd_buff_din(heads_sd_buff_din),
	.sd_buff_wr(sd_ack & sd_buff_wr & ~sector_mode),
	.sd_update(sd_update)
);

c157x_track c157x_track
(
	.clk(clk_sys),
	.reset(reset_drv),

	.sd_lba(sd_lba),
	.sd_blk_cnt(sd_blk_cnt),
	.sd_rd(sd_rd),
	.sd_wr(sd_wr),
	.sd_ack(sd_ack),

	.freq(freq),
	.sector_mode(sector_mode),
	.dual_side(img_ds),
	.sector_track(sector_track),
	.raw_blk_cnt(raw_blk_cnt),

	.save_track(save_track),
	.change(img_mounted),
	.track(track),
	.busy(busy),
	.sd_bank(sector_sd_bank)
);

always @(posedge clk) begin
	track <= track_num + (side ? 8'd84 : 8'd0);

	side_old <= side;
	stp_old <= stp;
	move <= stp - stp_old;

	if (sector_mode ? sector_gcr_we : sd_update) track_modified <= 1;
	if (img_mounted) track_modified <= 0;

	if (reset_drv) begin
		track_num <= 36;
		side_old <= 0;
		track_modified <= 0;
	end else begin
		if (mtr) begin
			if (move[0] && !move[1] && track_num < 84) track_num <= track_num + 1'b1;
			if (move[0] &&  move[1] && track_num > 0 ) track_num <= track_num - 1'b1;
			if ((move[0] || side != side_old) && track_modified) begin
				save_track <= ~save_track;
				track_modified <= 0;
			end
		end

		if (track_modified && !write && !act && !fdc_busy && !sd_busy) begin	// stopping activity
			save_track <= ~save_track;
			track_modified <= 0;
		end
	end
end

assign out_track = track;
assign out_we = track_modified | sd_wr;

endmodule
