`timescale 1ns/1ps

module tb_c157x_track;
	reg clk = 0;
	always #5 clk = ~clk;

	reg reset = 1;
	reg [1:0] freq = 0;
	reg sector_mode = 1;
	reg [6:0] sector_track = 18;
	reg [5:0] raw_blk_cnt = 31;
	reg save_track = 0;
	reg change = 0;
	reg [7:0] track = 36;
	reg sd_ack = 0;
	wire [31:0] sd_lba;
	wire [5:0] sd_blk_cnt;
	wire sd_rd, sd_wr, busy;
	integer errors = 0;

	c157x_track dut (
		.clk, .reset, .sd_lba, .sd_blk_cnt, .sd_rd, .sd_wr, .sd_ack,
		.freq, .sector_mode, .sector_track, .raw_blk_cnt,
		.save_track, .change, .track, .busy
	);

	task automatic finish_request;
	begin
		sd_ack = 1;
		repeat (4) @(posedge clk);
		sd_ack = 0;
		wait (!busy);
		repeat (2) @(posedge clk);
	end
	endtask

	task automatic expect_write(input [31:0] lba, input [5:0] blocks);
	begin
		wait (sd_wr === 1'b0);
		save_track = ~save_track;
		wait (sd_wr === 1'b1);
		repeat (2) @(posedge clk);
		if (sd_lba !== lba || sd_blk_cnt !== blocks) begin
			$display("FAIL write: lba=%0d/%0d blocks=%0d/%0d",
			         sd_lba, lba, sd_blk_cnt, blocks);
			errors = errors + 1;
		end
		finish_request();
	end
	endtask

	task automatic expect_track(input [6:0] trk, input [31:0] lba, input [5:0] blocks);
	begin
		wait (sd_rd === 1'b0);
		sector_track = trk;
		wait (sd_rd === 1'b1);
		repeat (2) @(posedge clk);
		if (sd_lba !== lba || sd_blk_cnt !== blocks) begin
			$display("FAIL track %0d: lba=%0d/%0d blocks=%0d/%0d",
			         trk, sd_lba, lba, sd_blk_cnt, blocks);
			errors = errors + 1;
		end
		finish_request();
	end
	endtask

	initial begin
		repeat (6) @(posedge clk);
		reset = 0;
		// The first request fetches one BAM sector so the GCR encoder learns the
		// disk ID. c157x_track deliberately keeps busy asserted and chains the
		// actual initial-track request after that acknowledgement.
		wait (sd_rd);
		@(negedge clk);
		if (sd_lba !== 357 || sd_blk_cnt !== 0) errors = errors + 1;
		sd_ack = 1;
		repeat (4) @(posedge clk);
		sd_ack = 0;
		wait (sd_rd);
		repeat (2) @(posedge clk);
		if (sd_lba !== 357 || sd_blk_cnt !== 18) errors = errors + 1;
		finish_request();

		expect_track(1, 0, 20);
		expect_track(18, 357, 18);
		expect_track(35, 666, 16);
		expect_track(36, 683, 20);
		expect_track(53, 1040, 18);
		expect_write(1040, 18);
		expect_track(70, 1349, 16);

		if (errors == 0)
			$display("ALL CHECKS PASSED: D64/D71 linear track geometry");
		else
			$display("FAIL: %0d c157x_track checks", errors);
		$finish;
	end
endmodule
