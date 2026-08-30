`timescale 1ns/1ps

module tb_via_timer;
	logic clk = 0;
	always #5 clk = ~clk;

	logic rising = 0, falling = 0, reset = 1;
	logic [3:0] addr = 0;
	logic wen = 0, ren = 0;
	logic [7:0] data_in = 0;
	wire [7:0] data_out;
	wire [7:0] pa_o, pa_t, pb_o, pb_t;
	wire ca2_o, ca2_t, cb1_o, cb1_t, cb2_o, cb2_t, irq, phi2_ref;

	iecdrv_via6522 dut (
		.clock(clk), .rising(rising), .falling(falling), .reset(reset),
		.addr(addr), .wen(wen), .ren(ren), .data_in(data_in), .data_out(data_out),
		.phi2_ref(phi2_ref),
		.port_a_o(pa_o), .port_a_t(pa_t), .port_a_i(8'hff),
		.port_b_o(pb_o), .port_b_t(pb_t), .port_b_i(8'hff),
		.ca1_i(1'b1), .ca2_o(ca2_o), .ca2_i(1'b1), .ca2_t(ca2_t),
		.cb1_o(cb1_o), .cb1_i(1'b1), .cb1_t(cb1_t),
		.cb2_o(cb2_o), .cb2_i(1'b1), .cb2_t(cb2_t), .irq(irq)
	);

	task automatic fall_cycle;
		begin
			@(negedge clk);
			falling = 1;
			@(negedge clk);
			falling = 0;
		end
	endtask

	initial begin
		repeat (4) @(negedge clk);
		reset = 0;

		// Exact operation used by 1571 DOS at $F556: write $D0 to T1CH.
		addr = 4'h5;
		data_in = 8'hd0;
		wen = 1;
		fall_cycle();
		wen = 0;
		ren = 1;
		repeat (2) @(posedge clk);

		if (data_out[7] !== 1'b1) begin
			$display("FAIL: T1 high byte after $D0 load is $%02h", data_out);
			$finish;
		end
		$display("PASS: T1 high byte after $D0 load is $%02h", data_out);

		// One VIA cycle later it must still be negative; the DOS uses BPL as its
		// timeout test before waiting for a disk sync mark.
		fall_cycle();
		repeat (2) @(posedge clk);
		if (data_out[7] !== 1'b1)
			$display("FAIL: T1 high byte lost bit 7 after one count: $%02h", data_out);
		else
			$display("PASS: T1 high byte after one count is $%02h", data_out);
		$finish;
	end
endmodule
