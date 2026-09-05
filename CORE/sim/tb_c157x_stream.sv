// Does the read head produce a byte stream once a .D64 track is in the buffer?
//
// tb_c1541_gcr_codec and tb_c157x_gcr_path prove the GCR encoder itself against
// golden fixtures, and tb_c157x_boot proves the DOS boots and answers the serial
// bus. Neither covers the join between them: c1541_gcr as it is actually wired
// into c157x_drv, with the track number, density and busy gating that the real
// instance sees.
//
// That join is what a stuck drive looks like from outside. The DOS turns the
// motor on, waits for a sector header that never arrives, and so never steps the
// head, never lights the LED and never asks the host for another block.
//
// Rather than wait out the DOS power-up self test and the disk-change timeout,
// this drives the head directly: mount an image, let the track load, then hold
// the VIA2 outputs at motor-on / read / byte-ready-enabled and watch the stream.
// A few milliseconds of simulated time is enough to see many sectors go by.

`timescale 1ns/1ps

module tb_c157x_stream;

	import sim_boot1_path_pkg::*;

	localparam NDR = 2;

	logic clk = 0;
	always #15.625 clk = ~clk;

	logic ce = 0;
	always @(posedge clk) ce <= ~ce;

	logic clk_sys = 0;
	always #10 clk_sys = ~clk_sys;

	logic  [NDR-1:0] reset = '1;
	logic            pause = 0;
	logic      [1:0] drv_mode[NDR];
	logic  [NDR-1:0] img_mounted = 0;
	logic            img_readonly = 0;
	logic     [31:0] img_size = 0;
	logic      [3:0] img_type = 0;

	wire   [NDR-1:0] led;
	wire             disk_ready;
	wire       [7:0] out_track[NDR];
	wire   [NDR-1:0] out_we;

	wire             iec_data_o, iec_clk_o, iec_fclk_o;

	wire      [31:0] sd_lba[NDR];
	wire       [5:0] sd_blk_cnt[NDR];
	wire   [NDR-1:0] sd_rd, sd_wr;
	logic  [NDR-1:0] sd_ack = 0;
	logic     [15:0] sd_buff_addr = 0;
	logic      [7:0] sd_buff_dout = 0;
	wire       [7:0] sd_buff_din[NDR];
	logic            sd_buff_wr = 0;

	logic            rom_loading = 1;
	wire             rom_req;
	wire      [18:0] rom_addr;
	logic      [7:0] rom_data = 0;
	logic            rom_wr = 0;

	int errors = 0;

	initial begin
		drv_mode[0] = 2'b10;   // menu default: 1571
		drv_mode[1] = 2'b10;
	end

	iec_drive #(.PARPORT(0), .DRIVES(NDR)) dut
	(
		.clk(clk),
		.reset(reset),
		.ce(ce),
		.pause(pause),

		.drv_mode(drv_mode),
		.img_mounted(img_mounted),
		.img_readonly(img_readonly),
		.img_size(img_size),
		.img_type(img_type),

		.led(led),
		.disk_ready(disk_ready),
		.out_track(out_track),
		.out_we(out_we),

		.iec_atn_i(1'b1),
		.iec_data_i(1'b1),
		.iec_clk_i(1'b1),
		.iec_fclk_i(1'b1),
		.iec_data_o(iec_data_o),
		.iec_clk_o(iec_clk_o),
		.iec_fclk_o(iec_fclk_o),

		.par_data_i(8'hFF),
		.par_stb_i(1'b1),
		.par_data_o(),
		.par_stb_o(),

		.clk_sys(clk_sys),

		.sd_lba(sd_lba),
		.sd_blk_cnt(sd_blk_cnt),
		.sd_rd(sd_rd),
		.sd_wr(sd_wr),
		.sd_ack(sd_ack),
		.sd_buff_addr(sd_buff_addr),
		.sd_buff_dout(sd_buff_dout),
		.sd_buff_din(sd_buff_din),
		.sd_buff_wr(sd_buff_wr),

		.rom_loading(rom_loading),
		.rom_req(rom_req),
		.rom_addr(rom_addr),
		.rom_data(rom_data),
		.rom_wr(rom_wr)
	);

	// ---------------------------------------------------------------------------
	// ROM and SD hosts, same as tb_c157x_boot
	// ---------------------------------------------------------------------------

	localparam ROM_BYTES = 6*32768;
	logic [7:0] boot1[0:ROM_BYTES-1];

	initial begin
		int fd, c, n;
		fd = $fopen(BOOT1_ROM_PATH, "rb");
		if (!fd) begin
			$display("FAIL: cannot open %s", BOOT1_ROM_PATH);
			$finish;
		end
		for (n = 0; n < ROM_BYTES; n = n + 1) begin
			c = $fgetc(fd);
			if (c < 0) break;
			boot1[n] = c[7:0];
		end
		$fclose(fd);
	end

	logic [1:0] srv_phase = 0;
	always @(posedge clk_sys) begin
		rom_wr <= 0;
		if (!rom_req) srv_phase <= 0;
		else begin
			srv_phase <= srv_phase + 1'd1;
			if (srv_phase == 0) rom_data <= boot1[rom_addr];
			if (srv_phase == 2) rom_wr <= 1;
		end
	end

	localparam D64_BYTES = 174848;
	logic [7:0] d64[0:D64_BYTES-1];
	logic       d64_loaded = 0;

	initial begin
		int fd, c, n;
		fd = $fopen(D64_IMAGE_PATH, "rb");
		if (!fd) $display("NOTE: no .D64 at %s, serving blank sectors", D64_IMAGE_PATH);
		else begin
			for (n = 0; n < D64_BYTES; n = n + 1) begin
				c = $fgetc(fd);
				if (c < 0) break;
				d64[n] = c[7:0];
			end
			$fclose(fd);
			d64_loaded = 1;
			$display("INFO: serving %0d bytes of %s", n, D64_IMAGE_PATH);
		end
	end

	int   host_reads = 0;
	int   last_lba   = -1;

	always @(posedge clk_sys) begin
		int unsigned blocks, i, off;
		logic        is_read;
		if (sd_rd[0] || sd_wr[0]) begin
			is_read = sd_rd[0];
			if (is_read) begin
				host_reads++;
				last_lba = sd_lba[0];
				$display("INFO: host serves LBA %0d, %0d blocks at %0t",
				         sd_lba[0], sd_blk_cnt[0] + 1, $time);
			end
			blocks = sd_blk_cnt[0] + 1;
			repeat (20) @(posedge clk_sys);
			sd_ack[0] <= 1;
			for (i = 0; i < blocks*256; i++) begin
				@(posedge clk_sys);
				off = sd_lba[0]*256 + i;
				sd_buff_addr <= i[15:0];
				sd_buff_dout <= (d64_loaded && off < D64_BYTES) ? d64[off] : 8'h00;
				sd_buff_wr   <= is_read;
			end
			@(posedge clk_sys);
			sd_buff_wr <= 0;
			sd_ack[0]  <= 0;
		end
	end

	// ---------------------------------------------------------------------------
	// The head under observation
	// ---------------------------------------------------------------------------

	wire       rom_valid0 = dut.rom_valid[0];
	wire       gcr_sync_n = dut.c157x.drives[0].c157x_drv.sector_gcr_sync_n;
	wire       gcr_byte_n = dut.c157x.drives[0].c157x_drv.sector_gcr_byte_n;
	wire [7:0] gcr_do     = dut.c157x.drives[0].c157x_drv.sector_gcr_do;
	wire       cpu_byte_n = dut.c157x.drives[0].c157x_drv.c157x_logic.byte_n;
	wire       soe        = dut.c157x.drives[0].c157x_drv.c157x_logic.soe;
	wire       gcr_busy   = dut.c157x.drives[0].c157x_drv.sector_gcr.busy;
	wire       gcr_mtr    = dut.c157x.drives[0].c157x_drv.sector_gcr.mtr;
	wire [1:0] gcr_freq   = dut.c157x.drives[0].c157x_drv.sector_gcr.freq;
	wire [6:0] gcr_track  = dut.c157x.drives[0].c157x_drv.sector_gcr.track;
	wire       gcr_mode   = dut.c157x.drives[0].c157x_drv.sector_gcr.mode;
	wire       gcr_ce     = dut.c157x.drives[0].c157x_drv.sector_gcr.ce;
	wire       via_sync_n = dut.c157x.drives[0].c157x_drv.c157x_logic.cpu_di[7];

	int   sync_falls  = 0;
	int   byte_pulses = 0;
	int   cpu_pulses  = 0;
	int   via_sync_low_cycles = 0;
	logic sync_d = 1, byte_d = 1, cpu_d = 1;

	// The first bytes to come off the surface after a sync mark are the sector
	// header. $08 encodes to the GCR codes 01010 01001, so a healthy 1541 head
	// always presents $52 as the first byte of a header.
	logic [7:0] first_after_sync[0:7];
	int         after_sync_cnt = 99;
	logic       captured = 0;

	always @(posedge clk) begin
		sync_d <= gcr_sync_n;
		byte_d <= gcr_byte_n;
		cpu_d  <= cpu_byte_n;

		if (sync_d && !gcr_sync_n) sync_falls <= sync_falls + 1;
		if (byte_d && !gcr_byte_n) byte_pulses <= byte_pulses + 1;
		if (cpu_d  && !cpu_byte_n) cpu_pulses  <= cpu_pulses + 1;
		if (!gcr_sync_n && !via_sync_n)
			via_sync_low_cycles <= via_sync_low_cycles + 1;

		// A sync mark ends when sync_n releases; the header follows immediately.
		if (!sync_d && gcr_sync_n && !captured) after_sync_cnt <= 0;

		if (byte_d && !gcr_byte_n && after_sync_cnt < 8) begin
			first_after_sync[after_sync_cnt] <= gcr_do;
			after_sync_cnt <= after_sync_cnt + 1;
			if (after_sync_cnt == 7) captured <= 1;
		end
	end

	task automatic check(input logic cond, input string what);
		if (cond) $display("PASS: %s", what);
		else begin
			$display("FAIL: %s", what);
			errors++;
		end
	endtask

	task automatic snapshot(input string tag);
		$display("INFO: %s mtr=%0b mode=%0b busy=%0b freq=%0d track=%0d soe=%0b ce=%0b sync_n=%0b",
		         tag, gcr_mtr, gcr_mode, gcr_busy, gcr_freq, gcr_track, soe, gcr_ce, gcr_sync_n);
		$display("INFO: %s syncs=%0d head bytes=%0d cpu bytes=%0d",
		         tag, sync_falls, byte_pulses, cpu_pulses);
	endtask

	initial begin
		time t0;
		repeat (100) @(posedge clk_sys);
		rom_loading <= 0;
		repeat (100) @(posedge clk_sys);

		img_size    <= 32'd174848;
		img_type    <= 4'b0010;
		img_mounted <= 2'b01;
		reset[0]    <= 0;
		repeat (50) @(posedge clk_sys);
		img_mounted <= 2'b00;

		t0 = $time;
		while (!rom_valid0 && ($time - t0) < 6_000_000) @(posedge clk_sys);
		check(rom_valid0, "DOS ROM streamed into the drive");

		// Let the track load land in the GCR buffer.
		#500_000;
		check(host_reads > 0, $sformatf("host served the initial track (LBA %0d)", last_lba));

		// Stand in for a DOS that has started a job: disk in place, motor running,
		// head in read mode, byte-ready enabled, density for the current zone.
		force dut.c157x.drives[0].c157x_drv.disk_present = 1'b1;
		force dut.c157x.drives[0].c157x_drv.c157x_logic.via2_pb_o  = 8'b0100_0100;
		force dut.c157x.drives[0].c157x_drv.c157x_logic.via2_pb_oe = 8'b0110_1111;
		force dut.c157x.drives[0].c157x_drv.c157x_logic.via2_ca2_o  = 1'b1;
		force dut.c157x.drives[0].c157x_drv.c157x_logic.via2_ca2_oe = 1'b1;
		force dut.c157x.drives[0].c157x_drv.c157x_logic.via2_cb2_o  = 1'b1;
		force dut.c157x.drives[0].c157x_drv.c157x_logic.via2_cb2_oe = 1'b1;
		// Hold the CPU bus on VIA2 ORB exactly as DOS does in its BIT $1C00
		// sync-wait loop. This proves the generated low pulse survives the VIA
		// input pipeline and reaches CPU data bit 7.
		force dut.c157x.drives[0].c157x_drv.c157x_logic.cpu_a  = 24'h001c00;
		force dut.c157x.drives[0].c157x_drv.c157x_logic.cpu_rw = 1'b1;

		#100_000;
		snapshot("head forced on");

		// One sector takes about 20 ms of head time at 300 rpm, and this rate is
		// set by the bit clock rather than the DOS, so a few ms already covers
		// several sectors' worth of bytes.
		#20_000_000;
		snapshot("after 20 ms of spin");

		check(sync_falls  > 0, $sformatf("head found sync marks (%0d)", sync_falls));
		check(byte_pulses > 0, $sformatf("head produced byte-ready pulses (%0d)", byte_pulses));
		check(cpu_pulses  > 0, $sformatf("byte-ready reached the CPU through SOE (%0d)", cpu_pulses));
		check(via_sync_low_cycles > 0,
		      $sformatf("sync reached CPU through VIA2 PB7 (%0d low cycles)",
		                via_sync_low_cycles));

		if (captured) begin
			$display("INFO: first bytes after a sync: %02h %02h %02h %02h %02h %02h %02h %02h",
			         first_after_sync[0], first_after_sync[1], first_after_sync[2],
			         first_after_sync[3], first_after_sync[4], first_after_sync[5],
			         first_after_sync[6], first_after_sync[7]);
			check(first_after_sync[0] == 8'h52,
			      $sformatf("header starts with the GCR mark $52 (saw $%02h)", first_after_sync[0]));
		end
		else check(0, "captured a header after a sync mark");

		if (errors == 0) $display("ALL CHECKS PASSED");
		else             $display("%0d CHECK(S) FAILED", errors);
		$finish;
	end

endmodule
