// Control experiment for tb_c157x_boot.
//
// tb_c157x_boot drives a model of the C128 side of the serial bus and finds that the
// 1541/1571 never acknowledges a command byte. That result is only worth anything if
// the bus model itself is right, so this testbench points exactly the same model at
// the 1581, which is known to work on real hardware.
//
// 1581 acknowledges and 157x does not  -> the bus model is sound, the 157x has a bug.
// Neither acknowledges                 -> the bus model is wrong, fix it first.

`timescale 1ns/1ps

module tb_c1581_iec;

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
	wire       [9:0] dbg;
	wire     [2047:0] diag[NDR];
	wire       [7:0] out_track[NDR];
	wire   [NDR-1:0] out_we;

	wire             iec_data_o, iec_clk_o, iec_fclk_o;

	logic            host_atn  = 1;
	logic            host_clk  = 1;
	logic            host_data = 1;

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

	initial begin
		drv_mode[0] = 2'b10;
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
		.dbg(dbg),
		.diag(diag),
		.out_track(out_track),
		.out_we(out_we),

		.iec_atn_i(host_atn),
		.iec_data_i(host_data),
		.iec_clk_i(host_clk),
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
			if (c < 0) begin
				$display("FAIL: %s is only %0d bytes", BOOT1_ROM_PATH, n);
				$fclose(fd);
				$finish;
			end
			boot1[n] = c[7:0];
		end
		$fclose(fd);
		$display("INFO: loaded %0d bytes of drive ROM", ROM_BYTES);
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

	always @(posedge clk_sys) begin
		int unsigned blocks, i;
		logic        is_read;
		if (sd_rd[0] || sd_wr[0]) begin
			is_read = sd_rd[0];
			blocks  = sd_blk_cnt[0] + 1;
			repeat (20) @(posedge clk_sys);
			sd_ack[0] <= 1;
			for (i = 0; i < blocks*256; i++) begin
				@(posedge clk_sys);
				sd_buff_addr <= i[15:0];
				sd_buff_dout <= 8'h00;
				sd_buff_wr   <= is_read;
			end
			@(posedge clk_sys);
			sd_buff_wr <= 0;
			sd_ack[0]  <= 0;
		end
	end

	// ---------------------------------------------------------------------------
	// The same C128 bus model as tb_c157x_boot
	// ---------------------------------------------------------------------------

	task automatic wait_data(input logic want, input time limit, output logic ok);
		time deadline;
		begin
			deadline = $time + limit;
			ok = 0;
			while ($time < deadline) begin
				if (iec_data_o === want) begin
					ok = 1;
					return;
				end
				#1000;
			end
		end
	endtask

	task automatic send_byte(input logic [7:0] value, output logic acked);
		begin
			for (int b = 0; b < 8; b++) begin
				host_clk  = 0;
				host_data = value[b];
				#40_000;
				host_clk  = 1;
				#40_000;
			end
			host_clk  = 0;
			host_data = 1;
			wait_data(1'b0, 1ms, acked);
		end
	endtask

	task automatic try_listen(input string tag, input logic [7:0] cmd, output logic acked);
		logic present_ok, ready_ok;
		begin
			host_atn  = 1;
			host_clk  = 1;
			host_data = 1;
			#100_000;

			host_atn = 0;
			host_clk = 0;
			wait_data(1'b0, 1ms, present_ok);

			ready_ok = 0;
			acked    = 0;
			if (present_ok) begin
				host_clk = 1;
				wait_data(1'b1, 1ms, ready_ok);
				if (ready_ok) send_byte(cmd, acked);
			end

			$display("INFO: %s listen $%02h: atn_answer=%0b ready=%0b ack=%0b",
			         tag, cmd, present_ok, ready_ok, acked);

			host_atn  = 1;
			host_clk  = 1;
			host_data = 1;
			#100_000;
		end
	endtask

	int errors = 0;
	task automatic check(input logic cond, input string what);
		if (cond) $display("PASS: %s", what);
		else begin
			$display("FAIL: %s", what);
			errors++;
		end
	endtask

	logic bus_ready = 0;

	initial begin
		time t0;
		repeat (100) @(posedge clk_sys);
		rom_loading <= 0;
		repeat (100) @(posedge clk_sys);

		// Mount a .D81 on drive 8, which turns this drive into a 1581.
		img_size    <= 32'd819200;
		img_type    <= 4'b1000;
		img_mounted <= 2'b01;
		reset[0]    <= 0;
		repeat (50) @(posedge clk_sys);
		img_mounted <= 2'b00;

		t0 = $time;
		while (!dut.rom_valid[0] && ($time - t0) < 6_000_000) @(posedge clk_sys);
		check(dut.rom_valid[0], "1581 DOS ROM streamed into the drive");

		for (int ms = 0; ms < 2000 && !bus_ready; ms++) begin
			if (ms % 100 == 99)
				try_listen($sformatf("t=%0d ms", ms + 1), 8'h28, bus_ready);
			else
				#1_000_000;
		end

		check(bus_ready, "1581 acknowledged a LISTEN $28 from the C128 bus model");

		if (errors == 0) $display("ALL CHECKS PASSED");
		else             $display("%0d CHECK(S) FAILED", errors);
		$finish;
	end

endmodule
