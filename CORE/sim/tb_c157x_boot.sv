// Bring-up test for the 1541/1571 half of iec_drive.
//
// Everything else in CORE/sim tests what the drive does once its CPU runs. This one
// tests the step before that: mount a .D64, hand over the real DOS ROM through the
// same pull handshake the hardware uses, and prove that the drive CPU leaves reset,
// fetches its reset vector and starts talking to its own VIAs.
//
// A drive that fails here is silent on the IEC bus and the C128 answers
// "?DEVICE NOT PRESENT", no matter how correct the GCR path below it is.
//
// The DOS image comes from the real boot1.rom. Its absolute path arrives through
// sim_boot1_path_pkg, which run_c157x_boot_sim.tcl regenerates for the current
// checkout, in the same spirit as boot_paths_pkg.vhd for the C128 boot simulation.

`timescale 1ns/1ps

module tb_c157x_boot;

	import sim_boot1_path_pkg::*;

	localparam NDR = 2;

	// 32 MHz core clock with a 16 MHz chip enable, as main.vhd generates it.
	logic clk = 0;
	always #15.625 clk = ~clk;

	logic ce = 0;
	always @(posedge clk) ce <= ~ce;

	// 50 MHz QNICE/SD clock.
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

	// The C128 side of the serial bus. All three lines are wired-AND and active low,
	// so 1 means released and 0 means pulled down by whoever is driving.
	logic            host_atn  = 1;
	logic            eoi_acked = 1;
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

	// ---------------------------------------------------------------------------
	// boot1.rom, byte-served exactly like drive_rom_server.vhd does it
	// ---------------------------------------------------------------------------

	localparam ROM_BYTES = 6*32768;
	logic [7:0] boot1[0:ROM_BYTES-1];
	string      boot1_path;
	int         rom_bytes_served = 0;

	initial begin
		int fd, c, n;
		boot1_path = BOOT1_ROM_PATH;
		fd = $fopen(boot1_path, "rb");
		if (!fd) begin
			$display("FAIL: cannot open %s", boot1_path);
			$finish;
		end
		for (n = 0; n < ROM_BYTES; n = n + 1) begin
			c = $fgetc(fd);
			if (c < 0) begin
				$display("FAIL: %s is only %0d bytes, expected %0d", boot1_path, n, ROM_BYTES);
				$fclose(fd);
				$finish;
			end
			boot1[n] = c[7:0];
		end
		$fclose(fd);
		$display("INFO: loaded %0d bytes from %s", ROM_BYTES, boot1_path);
	end

	// The real server needs several cycles per byte because its block RAM read is
	// registered. Latching the address and pulsing the write in different phases
	// reproduces that, and keeps rom_data aligned with the address iecdrv_rom holds.
	logic [1:0] srv_phase = 0;
	always @(posedge clk_sys) begin
		rom_wr <= 0;
		if (!rom_req) begin
			srv_phase <= 0;
		end
		else begin
			srv_phase <= srv_phase + 1'd1;
			if (srv_phase == 0) rom_data <= boot1[rom_addr];
			if (srv_phase == 2) begin
				rom_wr <= 1;
				rom_bytes_served <= rom_bytes_served + 1;
			end
		end
	end

	// ---------------------------------------------------------------------------
	// Minimal SD host: acknowledge whatever the drive asks for with zeroes. The
	// power-up sequence of the DOS does not touch the disk, so the content only has
	// to keep the handshake from stalling.
	// ---------------------------------------------------------------------------

	// A real .D64/.D71 so the DOS finds a genuine BAM and directory rather than an
	// empty surface it would reject before ever exercising the read path.
	logic [7:0] drive_image[0:DRIVE_IMAGE_BYTES-1];
	logic       drive_image_loaded = 0;

	initial begin
		int fd, c, n;
		fd = $fopen(DRIVE_IMAGE_PATH, "rb");
		if (!fd) begin
			$display("NOTE: no drive image at %s, serving blank sectors", DRIVE_IMAGE_PATH);
		end
		else begin
			for (n = 0; n < DRIVE_IMAGE_BYTES; n = n + 1) begin
				c = $fgetc(fd);
				if (c < 0) break;
				drive_image[n] = c[7:0];
			end
			$fclose(fd);
			drive_image_loaded = 1;
			$display("INFO: serving %0d bytes of %s", n, DRIVE_IMAGE_PATH);
		end
	end

	int   host_reads   = 0;
	logic saw_dir_read = 0;

	always @(posedge clk_sys) begin
		int unsigned blocks, i, off;
		logic        is_read;
		if (sd_rd[0] || sd_wr[0]) begin
			is_read = sd_rd[0];
			if (is_read) begin
				host_reads++;
				$display("INFO: host serves LBA %0d, %0d blocks at %0t",
				         sd_lba[0], sd_blk_cnt[0] + 1, $time);
				// The mount-time ID prefetch also reads LBA 357, but only one
				// block. A real directory-track request transfers all 19 sectors.
				if (sd_lba[0] == 357 && sd_blk_cnt[0] > 0) saw_dir_read <= 1;
			end
			blocks = sd_blk_cnt[0] + 1;
			repeat (20) @(posedge clk_sys);
			// The shell copies the image byte by byte through a 4k window, which
			// costs microseconds per byte rather than the single clock this model
			// needs. The GCR engine is held in reset for the whole transfer, so
			// the drive is blind to sync marks for as long as it lasts.
			if (HOST_BYTE_NS > 0) #(blocks * 256 * HOST_BYTE_NS);
			sd_ack[0] <= 1;
			for (i = 0; i < blocks*256; i++) begin
				@(posedge clk_sys);
				off = sd_lba[0]*256 + i;
				sd_buff_addr <= i[15:0];
				sd_buff_dout <= (drive_image_loaded && off < DRIVE_IMAGE_BYTES)
				                ? drive_image[off] : 8'h00;
				sd_buff_wr   <= is_read;
			end
			@(posedge clk_sys);
			sd_buff_wr <= 0;
			sd_ack[0]  <= 0;
		end
	end

	// ---------------------------------------------------------------------------
	// Observation points inside the drive
	// ---------------------------------------------------------------------------

	wire        rom_valid0  = dut.rom_valid[0];
	wire        reset_drv0  = dut.c157x.drives[0].c157x_drv.reset_drv;
	wire [23:0] cpu_a0      = dut.c157x.drives[0].c157x_drv.c157x_logic.cpu_a;
	wire        cpu_rw0     = dut.c157x.drives[0].c157x_drv.c157x_logic.cpu_rw;
	wire        via1_cs0    = dut.c157x.drives[0].c157x_drv.c157x_logic.via1_cs;
	wire        via2_cs0    = dut.c157x.drives[0].c157x_drv.c157x_logic.via2_cs;
	wire  [7:0] via1_pb_oe0 = dut.c157x.drives[0].c157x_drv.c157x_logic.via1_pb_oe;
	wire        iec_data_d0 = dut.c157x.iec_data_d[0];
	wire        ram_wr0     = dut.c157x.drives[0].c157x_drv.c157x_logic.ena_r &&
	                         !cpu_rw0 &&
	                         dut.c157x.drives[0].c157x_drv.c157x_logic.ram_cs;
	wire  [7:0] cpu_do0     = dut.c157x.drives[0].c157x_drv.c157x_logic.cpu_do;
	integer native_limit_writes = 0;

	always @(negedge clk) begin
		if (ram_wr0 && cpu_a0[15:0] == 16'h02AC && cpu_do0 == 8'h47)
			native_limit_writes <= native_limit_writes + 1;
		if (ram_wr0 && (cpu_a0[15:0] == 16'h01AF || cpu_a0[15:0] == 16'h02AC))
			$display("INITWRITE addr=%04h data=%02h pa5=%0b job0=%02h id=%02h/%02h at %0t",
			         cpu_a0[15:0], cpu_do0,
			         dut.c157x.drives[0].c157x_drv.c157x_logic.via1_pa_o[5],
			         dram(0), dram('h16), dram('h17), $time);
	end

	// The D71 variant is a focused initialization test. Stopping here keeps it
	// under a few minutes of wall time instead of running the full directory
	// transfer, while still covering mount ID prefetch and the track-53 probe.
	initial begin
		if (DRIVE_IMAGE_TYPE == 3) begin
			// Safety timeout. The main sequence finishes the focused D71 run as
			// soon as U0>M1 followed by I0 has settled.
			#900_000_000;
			$display("FAIL: D71 initialization test timed out");
			$finish;
		end
	end

	// Native 1571 initialization at $A6E5 first runs a controller job. It only
	// raises the legal-track limit to 71 if that job returns < 2 and VIA1 PA5
	// says the CPU is in 2 MHz mode. Trace those branch points directly.
	logic init_a6e5_seen = 0;
	always @(posedge clk) begin
		if (ce && cpu_rw0) begin
			case (cpu_a0[15:0])
				16'hA6E5: begin
					init_a6e5_seen <= 1;
					$display("INITTRACE A6E5 enter job0=%02h pa5=%0b limit=%02h at %0t",
					         dram(0),
					         dut.c157x.drives[0].c157x_drv.c157x_logic.via1_pa_o[5],
					         dram('h2AC), $time);
				end
				16'hA6E8:
					$display("INITTRACE A6E8 job returned job0=%02h limit=%02h at %0t",
					         dram(0), dram('h2AC), $time);
				16'hA6F4:
					$display("INITTRACE A6F4 job OK and PA5 high, limit=%02h at %0t",
					         dram('h2AC), $time);
				16'hA708:
					$display("INITTRACE A708 posting track-53 probe, sidecap=%02h limit=%02h at %0t",
					         dram('h1AF), dram('h2AC), $time);
				16'hA711:
					$display("INITTRACE A711 track-53 probe returned job0=%02h at %0t",
					         dram(0), $time);
				16'hA724:
					// $A724 is both the failure-path LDA opcode and the high
					// operand byte of the success-path BIT instruction. The
					// following write to $02AC distinguishes them.
					$display("INITTRACE A724 probe decision, job0=%02h sidecap=%02h limit=%02h at %0t",
					         dram(0), dram('h1AF), dram('h2AC), $time);
			endcase
		end
	end

	// Receive-path monitor for the first command byte.
	//
	// Under ATN the DOS loads $98 with 8, then for each bit waits at $EA0B for CLK to
	// rise, shifts the bit into $85 and decrements $98. On the way in it arms VIA1 T1
	// as a 200 us EOI timeout and branches to $E9F2 whenever it reads the T1 flag out
	// of $180D. Hardware shows a single bit followed by a DATA pulse, which is either
	// that EOI branch or a $98 that ran out early; logging both distinguishes them.
	wire        ena_r0    = dut.c157x.drives[0].c157x_drv.c157x_logic.ena_r;
	wire  [7:0] cpu_do0   = dut.c157x.drives[0].c157x_drv.c157x_logic.cpu_do;
	wire  [7:0] via1_do0  = dut.c157x.drives[0].c157x_drv.c157x_logic.via1_do;
	wire        ram_cs0   = dut.c157x.drives[0].c157x_drv.c157x_logic.ram_cs;

	logic rx_watch = 0;

	always @(posedge clk) if (rx_watch && ena_r0) begin
		if (!cpu_rw0 && ram_cs0 && cpu_a0[15:0] == 16'h0098)
			$display("NOTE: %8t  $98 <= $%02h", $time, cpu_do0);
		if (cpu_rw0 && via1_cs0 && cpu_a0[3:0] == 4'hD && via1_do0[6])
			$display("NOTE: %8t  DOS read $180D = $%02h (T1 flag set -> EOI at $E9F2)",
			         $time, via1_do0);
		if (!cpu_rw0 && via1_cs0 && cpu_a0[3:0] == 4'h5)
			$display("NOTE: %8t  T1 armed, $1805 <= $%02h", $time, cpu_do0);
		if (cpu_a0[15:0] == 16'hE9F2)
			$display("NOTE: %8t  DOS entered the EOI acknowledge at $E9F2", $time);
		if (cpu_a0[15:0] == 16'hEA28)
			$display("NOTE: %8t  DOS finished a byte and acknowledged at $EA28", $time);
	end

	// Stepping div by four keeps the exact phase pattern the divider produces, so
	// the CPU still sees alternating rising and falling phi2, only sooner.
	logic [3:0] fast_div  = 0;
	logic       fast_mode = 0;
	always @(posedge clk) if (ce && fast_mode) fast_div <= fast_div + 4;

	// The one question this test exists to answer: does the command the C128 sent
	// actually reach the DOS? A DOS that understood LOAD"$" puts a job in its
	// queue within a few milliseconds, long before the motor is up to speed.
	function automatic logic [7:0] dram(input int unsigned a);
		dram = dut.c157x.drives[0].c157x_drv.c157x_logic.ram.ram[a];
	endfunction

	logic saw_job  = 0;
	logic watching = 0;
	always @(posedge clk_sys) begin
		logic [7:0] slot;
		if (watching)
			for (int b = 0; b < 6; b++) begin
				slot = dram(b);
				if (slot[7]) saw_job <= 1;
			end
	end

	// If the DOS really received "$" on the command channel it will be sitting in
	// the command buffer at $0200. Seeing it there separates a DOS that listened
	// from the drive's hardware ATN acknowledge, which answers with no CPU at all.
	task automatic mem_dump(input int unsigned base, input int unsigned rows,
	                        input string tag);
		string line;
		for (int r = 0; r < rows; r++) begin
			line = $sformatf("INFO: %s $%04h:", tag, base + r*16);
			for (int i = 0; i < 16; i++) line = {line, $sformatf(" %02h", dram(base + r*16 + i))};
			$display("%s", line);
		end
	endtask

	logic saw_reset_vector = 0;
	logic saw_dos_entry    = 0;
	logic saw_via_write    = 0;
	logic saw_data_release = 0;
	logic saw_led          = 0;
	int   distinct_pc      = 0;

	int ph2_ticks = 0;
	always @(posedge clk) if (dut.c157x.ph2_r[0] === 1'b1) ph2_ticks <= ph2_ticks + 1;

	// Where the DOS spends its time, and whether it blinks the LED. A 1541 that fails
	// its power-up self test never reaches the serial bus loop and blinks instead.
	int   led_edges = 0;
	logic led_d = 0;
	always @(posedge clk) begin
		led_d <= led[0];
		if (led[0] !== led_d) led_edges <= led_edges + 1;
	end

	// Did the CPU actually vector through its IRQ, and does it move DATA while the
	// C128 clocks bits in? Those two separate "the DOS never noticed" from "the DOS
	// noticed but the byte never arrives".
	logic irq_seen = 0;
	always @(posedge clk) if (!reset_drv0 && cpu_a0[15:0] == 16'hFFFE) irq_seen <= 1;

	int   drv_data_edges = 0;
	logic drv_data_d = 1;
	always @(posedge clk) begin
		drv_data_d <= iec_data_o;
		if (iec_data_o !== drv_data_d) drv_data_edges <= drv_data_edges + 1;
	end

	// What the drive core actually sees on the clock line, after synchronisation and
	// after the merge with its own output. If this never moves, the DOS is deaf.
	wire  drv_clk_in = dut.c157x.iec_clk & dut.c157x.iec_clk_o;
	int   drv_clk_edges = 0;
	logic drv_clk_d = 1;
	always @(posedge clk) begin
		drv_clk_d <= drv_clk_in;
		if (drv_clk_in !== drv_clk_d) drv_clk_edges <= drv_clk_edges + 1;
	end

	logic [15:0] pc_lo = 16'hFFFF;
	logic [15:0] pc_hi = 16'h0000;
	always @(posedge clk) if (!reset_drv0) begin
		if (cpu_a0[15:0] < pc_lo) pc_lo <= cpu_a0[15:0];
		if (cpu_a0[15:0] > pc_hi) pc_hi <= cpu_a0[15:0];
	end

	task automatic pc_window(input string tag);
		pc_lo = 16'hFFFF;
		pc_hi = 16'h0000;
		#1_000_000;
		$display("INFO: %s addresses $%04h..$%04h led=%0b edges=%0d",
		         tag, pc_lo, pc_hi, led[0], led_edges);
	endtask

	logic [23:0] last_a = 24'hFFFFFF;
	always @(posedge clk) begin
		if (!reset_drv0) begin
			if (cpu_a0 !== last_a) begin
				last_a <= cpu_a0;
				if (distinct_pc < 100000) distinct_pc <= distinct_pc + 1;
			end
			if (cpu_a0[15:0] == 16'hFFFC) saw_reset_vector <= 1;
			if (cpu_a0[15:0] == 16'hEAA0) saw_dos_entry <= 1;
			if ((via1_cs0 || via2_cs0) && !cpu_rw0) saw_via_write <= 1;
			if (|via1_pb_oe0 && iec_data_d0) saw_data_release <= 1;
			if (led[0]) saw_led <= 1;
		end
	end

	// ---------------------------------------------------------------------------
	// Stimulus
	// ---------------------------------------------------------------------------

	int errors = 0;

	// ---------------------------------------------------------------------------
	// C128 side of the serial bus
	//
	// Only as much of the protocol as it takes to decide whether the drive is on
	// the bus at all: the KERNAL declares "?DEVICE NOT PRESENT" when nobody pulls
	// DATA down within 1 ms of ATN going active, and that is precisely the step
	// the hardware is failing.
	// ---------------------------------------------------------------------------

	// Wait until the merged DATA line reaches `want`, or give up after `limit`.
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

	task automatic wait_clk(input logic want, input time limit, output logic ok);
		time deadline;
		begin
			deadline = $time + limit;
			ok = 0;
			while ($time < deadline) begin
				if (iec_clk_o === want) begin
					ok = 1;
					return;
				end
				#1000;
			end
		end
	endtask

	// The DOS samples the bus in a loop that costs about 45 us per pass (JSR $EA59
	// plus the debounced double read at $E9C0), so each half bit has to stay put
	// well beyond that or the drive polls straight past a transition.
	//
	// The default is deliberately generous. +bit_phase_ns tightens it so a run can be
	// compared against the half-bit times actually measured on the IEC bus in hardware,
	// which is the only way to tell "the drive is too slow" from "the host is too fast".
	// Baked into the generated package rather than passed as a plusarg, for the same
	// reason as the image paths: xsim does not pick up simulator options that are set
	// after the simulation set exists.
	localparam time BIT_PHASE = sim_boot1_path_pkg::BIT_PHASE_NS;

	initial $display("NOTE: IEC half-bit time %0d ns", BIT_PHASE);

	// One complete frame to a listener, starting and ending with CLK asserted.
	//
	// After the listener releases DATA to say "ready", the DOS waits at $FF20 for
	// the DATA line to actually read high before it starts timing bits. The talker
	// therefore has to leave DATA alone for a moment; driving the first bit low
	// straight away parks the drive in that loop forever.
	task automatic send_frame(input logic [7:0] value, input logic eoi,
	                          output logic acked);
		logic ready_ok, eoi_lo, eoi_hi;
		begin
			acked    = 0;
			host_clk = 1;                       // ready to send
			wait_data(1'b1, 5ms, ready_ok);     // listener releases DATA
			if (!ready_ok) begin
				$display("INFO: send $%02h: listener never released DATA (clk_o=%0b data_o=%0b)",
				         value, iec_clk_o, iec_data_o);
				return;
			end

			if (eoi) begin
				// Holding off longer than 200 us is how the talker says "last byte".
				// The listener answers with a short pulse on DATA. EOI is only ever
				// used for data bytes, never for commands under ATN, so a listener
				// that skips this pulse still takes every command and then silently
				// loses the payload. A real KERNAL aborts here; so do we.
				#300_000;
				wait_data(1'b0, 1ms, eoi_lo);
				wait_data(1'b1, 1ms, eoi_hi);
				eoi_acked = eoi_lo & eoi_hi;
				if (!eoi_acked) begin
					$display("INFO: no EOI acknowledge (data pulled=%0b, released=%0b)",
					         eoi_lo, eoi_hi);
					return;
				end
			end

			#60_000;                            // let the DOS observe DATA released

			for (int b = 0; b < 8; b++) begin
				host_clk  = 0;
				host_data = value[b];
				#BIT_PHASE;
				host_clk  = 1;
				#BIT_PHASE;
			end

			host_clk  = 0;
			host_data = 1;
			wait_data(1'b0, 5ms, acked);        // listener pulls DATA for frame ack
			if (!acked)
				$display("INFO: send $%02h: no frame acknowledge (clk_o=%0b data_o=%0b)",
				         value, iec_clk_o, iec_data_o);
		end
	endtask

	// A full "LISTEN device 8" attempt, reported stage by stage. Retrying this over
	// and over is what separates "the drive is still busy with its power-up self
	// test" from "the drive never accepts commands at all".
	// One byte from the drive while it is the talker. Mirror image of send_frame:
	// the drive releases CLK when it has a bit ready and we sample DATA on the
	// release, then pull DATA down to acknowledge the frame.
	task automatic recv_byte(output logic [7:0] value, output logic ok,
	                         output logic eoi);
		logic tmp;
		time  t0;
		begin
			value = 0;
			eoi   = 0;
			ok    = 0;

			// The listener must release DATA first. The talker waits for that
			// ready indication before releasing CLK for the first byte.
			host_data = 1;
			wait_clk(1'b1, 2ms, tmp);        // talker ready to send
			if (!tmp) return;

			// A talker that leaves CLK alone for more than 200 us is saying "last
			// byte". The listener answers with a short pulse on DATA.
			t0 = $time;
			while (iec_clk_o === 1'b1 && ($time - t0) < 250_000) #1000;
			if (iec_clk_o === 1'b1) begin
				eoi = 1;
				host_data = 0;
				#80_000;
				host_data = 1;
				wait_clk(1'b0, 2ms, tmp);
				if (!tmp) return;
			end

			for (int b = 0; b < 8; b++) begin
				if (b > 0) begin
					wait_clk(1'b0, 2ms, tmp);
					if (!tmp) return;
				end
				wait_clk(1'b1, 2ms, tmp);
				if (!tmp) return;
				value[b] = iec_data_o;
			end

			host_data = 0;                   // frame acknowledge
			#100_000;
			ok = 1;
		end
	endtask

	// Ask the drive for its status the way PRINT DS$ does. Whatever went wrong
	// with the directory, the DOS names it here.
	task automatic read_error_channel(output string msg, output logic ok);
		logic a1, a2, tmp, eoi;
		logic [7:0] b;
		begin
			msg = "";
			ok  = 0;

			host_atn  = 0;
			host_clk  = 0;
			host_data = 1;
			#200_000;
			wait_data(1'b0, 1ms, tmp);
			$display("INFO: status channel ATN acknowledge=%0b", tmp);
			// Hardware ATN holds DATA until DOS copies ATN into ATNA (VIA1 PB4).
			// After a 1 MHz disk job that can take several milliseconds, so wait
			// for ATNA before releasing CLK for the command byte.
			for (int ms = 0; ms < 20 &&
			     !dut.c157x.drives[0].c157x_drv.c157x_logic.via1_pb_o[4]; ms++)
				#1_000_000;
			$display("INFO: before TALK soe=%0b irq_n=%0b atna=%0b state=$%02h pc=$%04h",
			         dut.c157x.drives[0].c157x_drv.c157x_logic.soe,
			         dut.c157x.drives[0].c157x_drv.c157x_logic.cpu_irq_n,
			         dut.c157x.drives[0].c157x_drv.c157x_logic.via1_pb_o[4],
			         dram(8'h20), cpu_a0[15:0]);
			$display("INFO: VIA1 pcr=%02h ifr=%02h ier=%02h jobs %02h %02h %02h %02h %02h %02h",
			         dut.c157x.drives[0].c157x_drv.c157x_logic.via1.pcr,
			         {dut.c157x.drives[0].c157x_drv.c157x_logic.via1.irq_out,
			          dut.c157x.drives[0].c157x_drv.c157x_logic.via1.irq_flags},
			         {1'b1, dut.c157x.drives[0].c157x_drv.c157x_logic.via1.irq_mask},
			         dram(0), dram(1), dram(2), dram(3), dram(4), dram(5));
			if (!dut.c157x.drives[0].c157x_drv.c157x_logic.via1_pb_o[4]) begin
				host_atn = 1;
				#50_000;
				host_atn = 0;
				#200_000;
				wait_data(1'b0, 1ms, tmp);
				$display("INFO: ATN re-asserted, present=%0b atna=%0b irq_n=%0b",
				         tmp,
				         dut.c157x.drives[0].c157x_drv.c157x_logic.via1_pb_o[4],
				         dut.c157x.drives[0].c157x_drv.c157x_logic.cpu_irq_n);
			end
			send_frame(8'h48, 1'b0, a1);          // TALK device 8
			$display("INFO: status channel TALK acknowledge=%0b", a1);
			if (!a1) return;
			send_frame(8'h6F, 1'b0, a2);          // secondary 15: error channel
			$display("INFO: status channel secondary acknowledge=%0b", a2);
			if (!a2) return;

			// Turn the bus around: the controller becomes the listener.
			host_data = 0;
			host_clk  = 1;
			host_atn  = 1;
			#200_000;

			ok = 1;
			for (int i = 0; i < 48; i++) begin
				recv_byte(b, tmp, eoi);
				if (!tmp) begin
					$display("INFO: status receive stopped before byte %0d (clk=%0b data=%0b)",
					         i, iec_clk_o, iec_data_o);
					ok = 0;
					break;
				end
				if (b >= 8'h20 && b < 8'h7F) msg = {msg, string'(b)};
				if (eoi) break;
			end

			host_atn  = 0;
			host_clk  = 0;
			host_data = 1;
			#200_000;
			send_frame(8'h5F, 1'b0, tmp);         // UNTALK
			host_atn  = 1;
			host_clk  = 1;
			host_data = 1;
		end
	endtask

	// LOAD"$",8 as the C128 puts it on the bus: address the drive, open the
	// directory channel, hand over the filename, then let go. If this works the
	// drive seeks to track 18 and reads the directory, which shows up as a host
	// block request and is what the whole chain has to produce.
	task automatic open_directory(input string tag, output logic ok);
		logic present_ok, a1, a2, a3, a4;
		begin
			host_atn  = 1;
			host_clk  = 1;
			host_data = 1;
			#100_000;

			irq_seen = 0;
			host_atn = 0;
			host_clk = 0;
			wait_data(1'b0, 1ms, present_ok);

			a1 = 0; a2 = 0; a3 = 0; a4 = 0;
			if (present_ok) begin
				rx_watch = 1;
				send_frame(8'h28, 1'b0, a1);        // LISTEN device 8
				rx_watch = 0;
				if (a1) send_frame(8'hF0, 1'b0, a2); // OPEN channel 0

				if (a2) begin
					host_atn  = 1;                   // filename is data, not command
					eoi_acked = 0;
					#200_000;
					send_frame("$", 1'b1, a3);       // last (and only) filename byte
				end

				if (a3) begin
					host_atn = 0;                    // UNLISTEN is a command again
					#200_000;
					send_frame(8'h3F, 1'b0, a4);
				end
			end

			host_atn  = 1;
			host_clk  = 1;
			host_data = 1;

			ok = a1 & a2 & a3 & a4 & eoi_acked;
			$display("INFO: %s open \"$\": present=%0b listen=%0b open=%0b name=%0b eoi=%0b unlisten=%0b irq=%0b",
			         tag, present_ok, a1, a2, a3, eoi_acked, a4, irq_seen);
		end
	endtask

	task automatic send_dos_command(input string command, output logic ok);
		logic present_ok, a1, a2, data_ack, a4;
		begin
			host_atn  = 0;
			host_clk  = 0;
			host_data = 1;
			wait_data(1'b0, 1ms, present_ok);

			a1 = 0; a2 = 0; data_ack = 0; a4 = 0;
			eoi_acked = 0;
			if (present_ok) begin
				send_frame(8'h28, 1'b0, a1);          // LISTEN device 8
				if (a1) send_frame(8'hFF, 1'b0, a2); // OPEN channel 15
				if (a2) begin
					host_atn = 1;
					#200_000;
					for (int i = 0; i < command.len(); i++)
						send_frame(command[i], i == command.len()-1, data_ack);
				end
				if (data_ack) begin
					host_atn = 0;
					#200_000;
					send_frame(8'h3F, 1'b0, a4);     // UNLISTEN
				end
			end
			host_atn  = 1;
			host_clk  = 1;
			host_data = 1;
			ok = present_ok & a1 & a2 & data_ack & a4 & eoi_acked;
			$display("INFO: DOS command \"%s\": present=%0b listen=%0b open15=%0b data=%0b eoi=%0b unlisten=%0b",
			         command, present_ok, a1, a2, data_ack, eoi_acked, a4);
		end
	endtask

	task automatic check(input logic cond, input string what);
		if (cond) $display("PASS: %s", what);
		else begin
			$display("FAIL: %s", what);
			errors++;
		end
	endtask

	logic bus_ready = 0;

	initial begin
		time   t0;
		string status;
		logic  status_ok;
		logic  mode_ok;
		logic  saw_sector_job;
		logic  sector_job_done;
		integer quiet_ms;
		repeat (100) @(posedge clk_sys);
		rom_loading <= 0;
		repeat (100) @(posedge clk_sys);

		// Mount the selected GCR image on drive 8.
		// main.vhd releases the drive from reset exactly while an image is mounted.
		img_size    <= DRIVE_IMAGE_BYTES;
		img_type    <= DRIVE_IMAGE_TYPE;
		img_mounted <= 2'b01;
		reset[0]    <= 0;
		repeat (50) @(posedge clk_sys);
		img_mounted <= 2'b00;

		t0 = $time;
		while (!rom_valid0 && ($time - t0) < 6_000_000) @(posedge clk_sys);
		check(rom_valid0, "DOS ROM fully streamed into the drive (rom_valid)");
		if (rom_valid0)
			$display("INFO: %0d ROM bytes served, drive left reset at %0t", rom_bytes_served, $time);

		// From here the drive CPU has everything it needs. Its power-up self test
		// walks the whole 2 kB of RAM and checksums 16 kB of ROM before it ever
		// touches the serial bus, which is tens of milliseconds of drive time.
		// The zero-page test at $EAB2 runs 256x256 iterations before the DOS ever
		// looks at the serial bus, so a real drive needs about a second to become
		// ready. Anything shorter here would only prove that it is still testing.
		// Poll the bus while the DOS works through its self test. The drive answers
		// ATN in hardware from the very first cycle, so only the command byte
		// acknowledge tells us that the DOS itself is on the bus.
		// Race the CPU through the self test. It is pure RAM and ROM work with no
		// relationship to head or bus timing, and normal speed is restored before
		// anything touches the serial bus.
		fast_mode = 1;
		force dut.c157x.div = fast_div;
		#250_000_000;
		release dut.c157x.div;
		fast_mode = 0;
		#5_000_000;

		// Retire the 2.1 s disk-change timeout the same way. Dropping it in two
		// steps keeps the write-protect flip that tells the DOS a disk arrived.
		dut.c157x.drives[0].c157x_drv.ch_timeout = 25'h0800100;
		#2_000_000;
		dut.c157x.drives[0].c157x_drv.ch_timeout = 25'd200;
		#20_000_000;

		for (int ms = 0; ms < 400 && !bus_ready; ms++) begin
			// Two gates have to pass before a directory read can possibly work:
			// the DOS power-up self test (about 700 ms) and the disk-change
			// timeout in c157x_drv, which is 25 bits at the 16 MHz enable and so
			// holds disk_present low for 2.1 s after the image is mounted. The
			// hardware is always probed well past both, so the model must be too.
			if (DRIVE_IMAGE_TYPE == 3 && ms == 0) begin
				send_dos_command("U0>M1", mode_ok);
				check(mode_ok, "DOS accepted U0>M1 native-mode command");
				for (int wait_ms = 0; wait_ms < 300; wait_ms++) begin
					#1_000_000;
					if (dram(8'h48) > 8'd2)
						dut.c157x.drives[0].c157x_drv.c157x_logic.ram.ram[8'h48] = 8'd2;
					if (native_limit_writes >= 2)
						break;
				end
				check(native_limit_writes >= 2,
				      "U0>M1 completed the track-53 probe and retained 71 tracks");
				// The disk-change path follows asynchronously after the native
				// probe. Wait for it to publish side capability rather than
				// injecting another IEC command while U0 still owns the drive.
				for (int wait_ms = 0; wait_ms < 250; wait_ms++) begin
					#1_000_000;
					if (dram(8'h48) > 8'd2)
						dut.c157x.drives[0].c157x_drv.c157x_logic.ram.ram[8'h48] = 8'd2;
					if (!((dram(0) | dram(1) | dram(2) |
					       dram(3) | dram(4) | dram(5)) & 8'h80) &&
					    dram('h01AF) == 8'h80)
						break;
				end
				$display("D71INIT final sidecap=%02h limit=%02h pa5=%0b id=%02h/%02h",
				         dram('h01AF), dram('h02AC),
				         dut.c157x.drives[0].c157x_drv.c157x_logic.via1_pa_o[5],
				         dram('h16), dram('h17));
				check(dram('h01AF) == 8'h80, "D71 initialization detected side 1");
				check(dram('h02AC) == 8'h47, "D71 initialization retained 71-track limit");
				if (errors == 0) $display("D71 INIT CHECKS PASSED");
				else             $display("%0d D71 INIT CHECK(S) FAILED", errors);
				$finish;
			end
			else if (ms % 50 == 49)
				open_directory($sformatf("t=%0d ms", ms + 1), bus_ready);
			else
				#1_000_000;
		end

		check(!reset_drv0,          "drive released its internal reset");
		check(saw_reset_vector,     "CPU fetched the reset vector at $FFFC");
		check(saw_dos_entry,        "CPU jumped to the DOS entry point $EAA0");
		check(distinct_pc > 500,    $sformatf("CPU is executing (%0d address changes)", distinct_pc));
		check(saw_via_write,        "DOS wrote to its VIA registers");
		check(saw_data_release,     "DOS drove the IEC DATA line through VIA1 port B");

		check(bus_ready, "drive accepted the whole LOAD\"$\",8 command sequence");

		// Opening the directory makes the DOS queue a job, then step to track 18
		// and pull it in. The queue entry appears at once; the read itself waits
		// out the motor spin-up, which is most of a second of drive time.
		pc_window("after open");
		watching = 1;
		for (int ms = 0; ms < 60 && !saw_job; ms++) #1_000_000;
		check(saw_job, "DOS queued a disk job in response to the command");
		$display("INFO: job queue %02h %02h %02h %02h %02h %02h, buffer 0 wants track %0d sector %0d",
		         dram(0), dram(1), dram(2), dram(3), dram(4), dram(5), dram(6), dram(7));
		mem_dump('h0200, 3, "cmdbuf");
		mem_dump('h0220, 2, "chan");
		mem_dump('h0070, 2, "zp");

		// Once the controller takes a job it waits 50 more interrupts, about 700 ms
		// of drive time, for the motor to reach speed. That delay is real and
		// correct, but simulating it costs half an hour, so run the counter down
		// to its last ticks and let the rest of the sequence play out normally.
		for (int ms = 0; ms < 100; ms++) begin
			#1_000_000;
			if (dram(8'h48) > 8'd2)
				dut.c157x.drives[0].c157x_drv.c157x_logic.ram.ram[8'h48] = 8'd2;
		end
		$display("INFO: drive state $%02h, spin-up counter $%02h, job queue %02h %02h %02h %02h %02h %02h",
		         dram(8'h20), dram(8'h48),
		         dram(0), dram(1), dram(2), dram(3), dram(4), dram(5));

		for (int ms = 0; ms < 400 && !saw_dir_read; ms++) #1_000_000;
		check(saw_dir_read, "drive read directory track 18 (LBA 357) from the host");

		// The host request starts before the controller has decoded the supplied
		// track. DOS briefly clears one entry before posting the next directory
		// sector in another slot, so a single idle sample does not mean LOAD is
		// complete. Require the whole queue to remain idle for 20 ms.
		saw_sector_job = 0;
		sector_job_done = 0;
		quiet_ms = 0;
		for (int ms = 0; ms < 900 && !sector_job_done; ms++) begin
			#1_000_000;
			if (dram(0) == 8'h80 || dram(1) == 8'h80 ||
			    dram(2) == 8'h80 || dram(3) == 8'h80 ||
			    dram(4) == 8'h80 || dram(5) == 8'h80)
				saw_sector_job = 1;
			if ((dram(0) | dram(1) | dram(2) |
			     dram(3) | dram(4) | dram(5)) & 8'h80)
				quiet_ms = 0;
			else if (saw_sector_job) begin
				quiet_ms = quiet_ms + 1;
				if (quiet_ms >= 20) sector_job_done = 1;
			end
		end

		// Mechanics: a DOS that started a job turns the motor on and steps the
		// head. If these never move the job never left the queue.
		$display("INFO: mechanics mtr=%0b act=%0b stp=%02b track_num=%0d sd_busy=%0b",
		         dut.c157x.drives[0].c157x_drv.mtr,
		         dut.c157x.drives[0].c157x_drv.act,
		         dut.c157x.drives[0].c157x_drv.stp,
		         dut.c157x.drives[0].c157x_drv.track_num,
		         dut.c157x.drives[0].c157x_drv.sd_busy);
		pc_window("while waiting for the directory");

		// Serving the sectors to the drive is not the same as the drive using them.
		// A queue entry keeps bit 7 set until the disk controller retires it, so an
		// entry still reading $b0 here means the seek never converged. That is
		// invisible from the host side and leaves the computer waiting forever.
		check(!((dram(0) | dram(1) | dram(2) |
		         dram(3) | dram(4) | dram(5)) & 8'h80),
		      "disk controller retired the queued job");

		// $00-$05 are the job codes, $06-$11 the per-buffer track/sector the job
		// wants, and $12/$13 the disk ID the DOS insists every header carries.
		// A seek that never retires is normally one of those three disagreeing
		// with what the GCR path actually writes into the headers.
		mem_dump('h0000, 2, "zp");
		$display("INFO: GCR header carries id1=%02h id2=%02h, DOS expects %02h %02h",
		         dut.c157x.drives[0].c157x_drv.sector_gcr.id1,
		         dut.c157x.drives[0].c157x_drv.sector_gcr.id2,
		         dram('h12), dram('h13));

		// Let the DOS name the fault itself, exactly as PRINT DS$ would.
		// After the disk job the 1 MHz DOS can still be leaving the controller
		// loop; a real KERNAL also waits before it talks channel 15.
		if (bus_ready) begin
			#20_000_000;
			read_error_channel(status, status_ok);
			if (status_ok) $display("INFO: drive status channel: \"%s\"", status);
			else           $display("INFO: drive status channel unreadable, partial \"%s\"", status);
			check(status_ok, "drive still answers the status channel after the directory attempt");
		end

		$display("INFO: VIA1 pb_o=%02h pb_oe=%02h atna=%0b data_out=%0b",
		         dut.c157x.drives[0].c157x_drv.c157x_logic.via1_pb_o,
		         dut.c157x.drives[0].c157x_drv.c157x_logic.via1_pb_oe,
		         dut.c157x.drives[0].c157x_drv.c157x_logic.via1_pb_o[4],
		         iec_data_o);

		$display("INFO: led seen=%0b, host block reads=%0d, track=%0d",
		         saw_led, host_reads, out_track[0]);
		$display("INFO: reset=%b img_hd=%b rom_valid=%b multi_reset=%b drv_reset=%b",
		         reset, dut.img_hd, dut.rom_valid, dut.c157x.reset_drv,
		         dut.c157x.drives[0].c157x_drv.reset);
		$display("INFO: ce=%b ph2_r=%b ph2_f=%b ph2_ticks=%0d disk_present=%b",
		         ce, dut.c157x.ph2_r, dut.c157x.ph2_f, ph2_ticks,
		         dut.c157x.drives[0].c157x_drv.disk_present);

		if (errors == 0) $display("ALL CHECKS PASSED");
		else             $display("%0d CHECK(S) FAILED", errors);
		$finish;
	end

endmodule
