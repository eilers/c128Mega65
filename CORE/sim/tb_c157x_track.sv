`timescale 1ns/1ps

module tb_c157x_track;
	reg clk = 0;
	always #5 clk = ~clk;

	reg reset = 1;
	reg [1:0] freq = 0;
	reg sector_mode = 1;
	reg dual_side = 1;
	reg [6:0] sector_track = 18;
	reg [5:0] raw_blk_cnt = 31;
	reg save_track = 0;
	reg change = 0;
	reg [7:0] track = 36;
	reg sd_ack = 0;
	wire [31:0] sd_lba;
	wire [5:0] sd_blk_cnt;
	wire sd_rd, sd_wr, busy, sd_bank;
	integer errors = 0;

	c157x_track dut (
		.clk, .reset, .sd_lba, .sd_blk_cnt, .sd_rd, .sd_wr, .sd_ack,
		.freq, .sector_mode, .dual_side, .sector_track, .raw_blk_cnt,
		.save_track, .change, .track, .busy, .sd_bank
	);

	task automatic acknowledge;
	begin
		sd_ack = 1;
		repeat (4) @(posedge clk);
		sd_ack = 0;
		repeat (2) @(posedge clk);
	end
	endtask

	task automatic expect_request(input write_req, input [31:0] lba,
	                              input [5:0] blocks, input bank);
	begin
		if (write_req) wait (sd_wr === 1'b1);
		else           wait (sd_rd === 1'b1);
		repeat (2) @(posedge clk);
		if (sd_lba !== lba || sd_blk_cnt !== blocks || sd_bank !== bank) begin
			$display("FAIL %s: lba=%0d/%0d blocks=%0d/%0d bank=%0d/%0d",
			         write_req ? "write" : "read", sd_lba, lba,
			         sd_blk_cnt, blocks, sd_bank, bank);
			errors = errors + 1;
		end
		acknowledge();
	end
	endtask

	task automatic expect_cylinder(input [6:0] trk, input [31:0] lba,
	                               input [5:0] blocks, input [31:0] other_lba);
	begin
		wait (sd_rd === 1'b0);
		sector_track = trk;
		expect_request(0, lba, blocks, trk > 35);
		expect_request(0, other_lba, blocks, trk <= 35);
		wait (!busy);
		repeat (2) @(posedge clk);
	end
	endtask

	initial begin
		repeat (6) @(posedge clk);
		reset = 0;
		// Learn the disk ID, then fetch both heads at physical track 18.
		expect_request(0, 357, 0, 0);
		expect_request(0, 357, 18, 0);
		expect_request(0, 1040, 18, 1);
		wait (!busy);

		// Switching heads on the cached cylinder must not ask the host anything.
		sector_track = 53;
		repeat (20) @(posedge clk);
		if (sd_rd || busy) begin
			$display("FAIL: cached side change caused a host request");
			errors = errors + 1;
		end

		// A step fetches the selected head first and then its opposite head.
		expect_cylinder(36, 683, 20, 0);
		expect_cylinder(70, 1349, 16, 666);

		// Dirty side 1 is written back through bank 1.
		save_track = ~save_track;
		expect_request(1, 1349, 16, 1);
		wait (!busy);

		// Its already-resident opposite head remains an instant cache hit.
		sector_track = 35;
		repeat (20) @(posedge clk);
		if (sd_rd || busy) begin
			$display("FAIL: cached side 0 caused a host request after writeback");
			errors = errors + 1;
		end

		if (errors == 0)
			$display("ALL CHECKS PASSED: D64/D71 linear track geometry");
		else
			$display("FAIL: %0d c157x_track checks", errors);
		$finish;
	end
endmodule
