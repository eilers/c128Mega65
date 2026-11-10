`timescale 1ns/1ps

// CIA serial port, C128 to drive: JiffyDOS 128's fast send ($FD15), cycle by cycle at 1 and
// 2 MHz. It stops the shift (CRA $08) right after it sees the SP flag, so the last bit must
// still be on SP when CNT rises. Checked with a receiving CIA (as in the 1571/1581) and with an
// edge-sampling receiver (as a real chip, e.g. a CMD HD), and the hold time after the 8th CNT
// rise must be 1 us or more (VICE: the flag comes 2 cycles after the 8th fall).
module tb_cia_sp;
	logic clk = 0;
	always #5 clk = ~clk;                       // 100 clocks = one 1 MHz phi2 cycle

	int cnt = 0;
	always @(posedge clk) cnt <= cnt + 1;

	int rxph = 0;                                // the drive's phi2 phase, in clocks
	wire tx_p = (cnt % 100) == 0;
	wire tx_n = (cnt % 100) == 50;
	wire rx_p = ((cnt + 1000 - rxph) % 100) == 0;
	wire rx_n = ((cnt + 1000 - rxph) % 100) == 50;

	logic res_n = 0;
	logic tx_cs_n = 1, tx_rw = 1, rx_cs_n = 1, rx_rw = 1;
	logic [3:0] tx_rs = 0, rx_rs = 0;
	logic [7:0] tx_di = 0, rx_di = 0;
	wire  [7:0] tx_do, rx_do;
	logic fsdir_out = 0;                         // MMU $D505 bit 3

	wire tx_sp, tx_cnt, rx_sp, rx_cnt;
	wire srq  = tx_cnt | ~fsdir_out;
	wire data = tx_sp  | ~fsdir_out;

	wire [7:0] unused_pa, unused_pb, unused_pao, unused_pbo, rx_pao, rx_pbo, rx_pa, rx_pb;
	wire tx_irq, rx_irq, tx_pc, rx_pc;

	mos6526_8520 tx (
		.mode(2'd0), .clk(clk), .phi2_p(tx_p), .phi2_n(tx_n), .res_n(res_n),
		.cs_n(tx_cs_n), .rw(tx_rw), .rs(tx_rs), .db_in(tx_di), .db_out(tx_do),
		.pa_in(8'hff), .pa_out(unused_pa), .pa_oe(unused_pao),
		.pb_in(8'hff), .pb_out(unused_pb), .pb_oe(unused_pbo),
		.flag_n(1'b1), .pc_n(tx_pc), .tod(1'b0),
		.sp_in(1'b1), .sp_out(tx_sp), .cnt_in(1'b1), .cnt_out(tx_cnt), .irq_n(tx_irq)
	);

	mos6526_8520 rx (
		.mode(2'd0), .clk(clk), .phi2_p(rx_p), .phi2_n(rx_n), .res_n(res_n),
		.cs_n(rx_cs_n), .rw(rx_rw), .rs(rx_rs), .db_in(rx_di), .db_out(rx_do),
		.pa_in(8'hff), .pa_out(rx_pa), .pa_oe(rx_pao),
		.pb_in(8'hff), .pb_out(rx_pb), .pb_oe(rx_pbo),
		.flag_n(1'b1), .pc_n(rx_pc), .tod(1'b0),
		.sp_in(data), .sp_out(rx_sp), .cnt_in(srq), .cnt_out(rx_cnt), .irq_n(rx_irq)
	);

	// Edge-sampling receiver: SP at each SRQ rise; how long SP holds after the 8th
	logic [7:0] edge_byte;
	int edge_bits = 0, last_rise = 0, hold = 0;
	logic last_bit;
	logic srq_q = 1, data_q = 1;
	always @(posedge clk) begin
		srq_q <= srq;
		data_q <= data;
		if (!srq_q && srq && edge_bits < 8) begin
			edge_byte <= {edge_byte[6:0], data};
			edge_bits <= edge_bits + 1;
			last_bit  <= data;
			last_rise <= cnt;
			hold      <= 100000;
		end
		if (edge_bits == 8 && hold == 100000 && data != last_bit)
			hold <= cnt - last_rise;
	end

	// 8502 timing: a cycle is 50 clocks at 2 MHz, 100 at 1 MHz; I/O ends on a phi2 fall
	int cyc = 50, cur = 0;
	task automatic cpu(input int n); cur += n * cyc; endtask

	function automatic int io_time();            // the phi2 fall that ends this access
		int t;
		cur += 3 * cyc;                          // opcode and address bytes
		t = cur > cnt ? cur : cnt + 1;
		while (t % 100 != 50) t++;
		return t;
	endfunction

	task automatic io(input [3:0] r, input bit w, input [7:0] d, output [7:0] q);
		int t = io_time();
		while (cnt < t) @(negedge clk);
		tx_cs_n = 0; tx_rw = !w; tx_rs = r; tx_di = d;
		@(posedge clk); #1 q = tx_do;
		@(negedge clk); tx_cs_n = 1; tx_rw = 1;
		cur = t + 50;
	endtask

	task automatic mmu(input bit out);          // STA $D505: stretched like I/O
		int t = io_time();
		while (cnt < t) @(negedge clk);
		fsdir_out = out;
		cur = t + 50;
	endtask

	task automatic other_io();                   // an access to another chip, only its timing
		int t = io_time();
		cur = t + 50;
	endtask

	task automatic rx_io(input [3:0] r, input bit w, input [7:0] d, output [7:0] q);
		@(negedge clk);
		while (!rx_n) @(negedge clk);
		rx_cs_n = 0; rx_rw = !w; rx_rs = r; rx_di = d;
		@(posedge clk); #1 q = rx_do;
		@(negedge clk); rx_cs_n = 1; rx_rw = 1;
	endtask

	// One byte as JiffyDOS sends it ($FD15), PRE cycles later into its poll
	task automatic send(input [7:0] b, input int pre, output [7:0] got_cia, output [7:0] got_edge,
	                    output int hold_ns);
		logic [7:0] q;
		int polls;
		rx_io(4'hd, 1, 8'h7f, q);                // receiver: no IRQs, flags clear
		rx_io(4'hd, 0, 8'h00, q);
		while (cnt < cur) @(negedge clk);
		cur = (cnt / 100 + 1) * 100;             // a cycle starts with phi2
		edge_bits = 0;
		other_io(); cpu(2); mmu(1);              // LDA $D505, ORA, STA $D505: FSDIR out
		cpu(2); io(4'hd, 1, 8'h7f, q);
		cpu(2); io(4'h5, 1, 8'h00, q);
		cpu(2); io(4'h4, 1, 8'h03, q);
		io(4'he, 0, 8'h00, q); cpu(4);           // LDA $DC0E, AND, ORA
		io(4'hd, 0, 8'h00, q);                   // BIT $DC0D
		other_io(); cpu(2); other_io();          // BIT $DD00, BPL, STX $DD00
		io(4'he, 1, 8'h55, q);                   // start TA, SP out
		cpu(3); io(4'hc, 1, b, q);               // LDA $95, STA $DC0C
		cpu(pre);                                // where the poll meets the flag (refresh stalls)
		io(4'he, 0, 8'h00, q); cpu(6);           // LDA $DC0E, AND, ORA, TAX
		other_io(); cpu(2 + 3 + 2);              // LDA $D505, AND, PHA, LDA #$08
		polls = 0;
		do begin
			io(4'hd, 0, 8'h00, q);               // BIT $DC0D
			if (!q[3]) cpu(3);                   // BEQ taken
			polls++;
		end while (!q[3] && polls < 400);
		cpu(2);                                  // BEQ not taken
		io(4'he, 1, 8'h08, q);                   // STX $DC0E: stop, SP in
		cpu(4 + 2 + 2); mmu(0);                  // PLA, LDX, CLC, STA $D505: FSDIR in
		cur += 20000;
		while (cnt < cur) @(negedge clk);
		rx_io(4'hc, 0, 8'h00, got_cia);
		got_edge = edge_byte;
		hold_ns  = hold * 10;
	endtask

	int fails = 0, runs = 0, min_hold;
	logic [7:0] pats [7] = '{8'h00, 8'h30, 8'h4c, 8'h4e, 8'hfe, 8'h55, 8'hff};
	logic [7:0] gc, ge;
	int h;

	initial begin
		logic [7:0] q;
		repeat (300) @(negedge clk);
		res_n = 1;
		repeat (300) @(negedge clk);
		rx_io(4'he, 1, 8'h00, q);                // receiver CRA: SP input
		for (int speed = 2; speed >= 1; speed--) begin
			cyc = speed == 2 ? 50 : 100;
			min_hold = 100000;
			for (int ph = 0; ph < 100; ph += 25) begin
				rxph = ph;
				for (int pre = 0; pre < 8; pre++)
					foreach (pats[i]) begin
						send(pats[i], pre, gc, ge, h);
						runs++;
						if (h < min_hold) min_hold = h;
						if (gc !== pats[i] || ge !== pats[i] || h < 1000) begin
							fails++;
							if (fails <= 12)
								$display("FAIL: %0d MHz rx phase %0d pre %0d sent %02x: CIA %02x, edge %02x, hold %0d ns",
								         speed, ph, pre, pats[i], gc, ge, h);
						end
					end
			end
			$display("INFO: %0d MHz, shortest hold after the 8th CNT rise: %0d ns", speed, min_hold);
		end
		if (fails == 0) $display("PASS: CIA SP fast send at 1 and 2 MHz (%0d bytes)", runs);
		else            $display("FAIL: %0d of %0d bytes", fails, runs);
		$finish;
	end
endmodule
