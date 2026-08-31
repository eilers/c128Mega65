// Can the DOS disk controller actually complete a read job?
//
// tb_c157x_stream proves the head produces a correct GCR stream, and
// tb_c157x_boot proves the DOS boots and answers the serial bus, yet the drive
// still never reads a directory. The step in between is the disk controller:
// the interrupt routine that turns the motor on, hunts for a sector header,
// steps the head to the wanted track and copies the sector into a buffer.
//
// The 1541/1571 DOS exposes that routine through its job queue in zero page, so
// this test skips the serial bus entirely: post a "read track 18 sector 1" job
// straight into drive RAM and read back the status the controller writes there.
// That status is the DOS naming its own failure, in one byte:
//
//     $01 ok            $02 header not found   $03 no sync
//     $04 data not found $05 data checksum     $09 header checksum
//     $0B disk id mismatch                     $0F drive not ready
//
// The DOS power-up self test walks 2 kB of RAM before it services any job, which
// is about 700 ms of drive time and far too slow to iterate on. The drive CPU is
// therefore clocked four times over during that phase only; it is pure CPU work
// with no relationship to head timing, and normal speed is restored well before
// the job is posted.

`timescale 1ns/1ps

module tb_c157x_job;

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
		drv_mode[0] = sim_boot1_path_pkg::DRIVE_MODE;
		drv_mode[1] = sim_boot1_path_pkg::DRIVE_MODE;
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
				if (sd_lba[0] == 357) saw_dir_read <= 1;
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
	// Run the drive CPU four times over while it self tests
	// ---------------------------------------------------------------------------

	// Stepping div by four keeps the exact phase pattern the divider produces, so
	// the CPU still sees alternating rising and falling phi2, only sooner.
	logic [3:0] fast_div  = 0;
	logic       fast_mode = 0;
	always @(posedge clk) if (ce && fast_mode) fast_div <= fast_div + 4;

	// ---------------------------------------------------------------------------
	// Observation points
	// ---------------------------------------------------------------------------

	wire        rom_valid0 = dut.rom_valid[0];
	wire        gcr_sync_n = dut.c157x.drives[0].c157x_drv.sector_gcr_sync_n;
	wire        cpu_byte_n = dut.c157x.drives[0].c157x_drv.c157x_logic.byte_n;
	wire        soe        = dut.c157x.drives[0].c157x_drv.c157x_logic.soe;
	wire  [7:0] via2_pb_o  = dut.c157x.drives[0].c157x_drv.c157x_logic.via2_pb_o;
	wire  [7:0] via2_pb_oe = dut.c157x.drives[0].c157x_drv.c157x_logic.via2_pb_oe;
	wire        mtr        = dut.c157x.drives[0].c157x_drv.mtr;
	wire        act        = dut.c157x.drives[0].c157x_drv.act;
	wire  [1:0] stp        = dut.c157x.drives[0].c157x_drv.stp;
	wire  [6:0] track_num  = dut.c157x.drives[0].c157x_drv.track_num;
	wire        disk_pres  = dut.c157x.drives[0].c157x_drv.disk_present;
	wire        cpu_irq_n  = dut.c157x.drives[0].c157x_drv.c157x_logic.cpu_irq_n;
	wire [23:0] cpu_a      = dut.c157x.drives[0].c157x_drv.c157x_logic.cpu_a;
	wire        cpu_rw     = dut.c157x.drives[0].c157x_drv.c157x_logic.cpu_rw;
	wire  [7:0] cpu_do     = dut.c157x.drives[0].c157x_drv.c157x_logic.cpu_do;
	wire        cpu_step   = dut.c157x.drives[0].c157x_drv.c157x_logic.ena_f;
	wire  [2:0] accl       = dut.c157x.drives[0].c157x_drv.c157x_logic.accl;

	int   cpu_pulses = 0;
	int   sync_falls = 0;
	int   stp_moves  = 0;
	logic cpu_d = 1, sync_d = 1;
	logic [1:0] stp_d = 0;

	int   irq_count = 0;
	logic irq_d = 1;

	// SOE is how the DOS says "I am reading bytes now". Logging where the CPU is
	// each time it flips shows which routine gives up on the byte stream.
	int         soe_events = 0;
	logic       soe_d = 0;
	logic [1:0] soe_dir[0:31];
	logic [15:0] soe_where[0:31];

	// A write job can complete its mechanics yet fail verification. Count the bytes the
	// DOS sends to VIA2 and the bytes c1541_gcr accepts into its sector buffer so 1 MHz
	// and 2 MHz failures can be separated without a waveform dump.
	int         via2_ora_writes = 0;
	int         gcr_buffer_writes = 0;
	int         gcr_decode_errors = 0;
	logic       gcr_decode_error_d = 0;
	logic [15:0] via2_ora_sum = 0;
	logic [15:0] gcr_buffer_sum = 0;

	always @(posedge clk) begin
		cpu_d  <= cpu_byte_n;
		sync_d <= gcr_sync_n;
		stp_d  <= stp;
		irq_d  <= cpu_irq_n;
		soe_d  <= soe;
		if (cpu_d  && !cpu_byte_n) cpu_pulses <= cpu_pulses + 1;
		if (sync_d && !gcr_sync_n) sync_falls <= sync_falls + 1;
		if (stp_d  != stp)         stp_moves  <= stp_moves  + 1;
		if (irq_d  && !cpu_irq_n)  irq_count  <= irq_count  + 1;

		if (sampling && (soe_d !== soe) && soe_events < 32) begin
			soe_dir[soe_events]   <= {1'b0, soe};
			soe_where[soe_events] <= cpu_a[15:0];
			soe_events            <= soe_events + 1;
		end

		gcr_decode_error_d <=
			dut.c157x.drives[0].c157x_drv.sector_gcr.decode_error;
		if (sampling && cpu_step && !cpu_rw && cpu_a[15:0] == 16'h1C01) begin
			via2_ora_writes <= via2_ora_writes + 1;
			via2_ora_sum <= via2_ora_sum + cpu_do;
		end
		if (sampling && dut.c157x.drives[0].c157x_drv.sector_gcr.we) begin
			gcr_buffer_writes <= gcr_buffer_writes + 1;
			gcr_buffer_sum <= gcr_buffer_sum
			                + dut.c157x.drives[0].c157x_drv.sector_gcr.buff_di;
		end
		if (sampling && !gcr_decode_error_d
		    && dut.c157x.drives[0].c157x_drv.sector_gcr.decode_error)
			gcr_decode_errors <= gcr_decode_errors + 1;
	end

	function automatic logic [7:0] dram(input int unsigned a);
		dram = dut.c157x.drives[0].c157x_drv.c157x_logic.ram.ram[a];
	endfunction

	// The bytes the read head actually presents after each sync mark. The DOS compares
	// exactly these against the GCR header image it builds at $24, so printing both
	// sides separates "the head emits the wrong track" from "the DOS is looking for the
	// wrong thing". A header block starts $52, a data block $55.
	// Keep more than one complete track rotation so the exact requested sector
	// can be compared even when the head moves after logging starts.
	localparam int HDR_LOG = 80;
	logic [7:0] hdr_bytes[0:HDR_LOG-1][0:7];
	int         hdr_n = 0;
	int         hdr_i = 8;

	wire [7:0] head_do   = dut.c157x.drives[0].c157x_drv.c157x_logic.gcr_do;
	wire       head_bn   = dut.c157x.drives[0].c157x_drv.sector_gcr_byte_n;
	logic      head_bn_d = 1;

	always @(posedge clk) begin
		head_bn_d <= head_bn;
		if (sampling && sync_d && !gcr_sync_n)
			hdr_i <= 0;
		else if (sampling && head_bn_d && !head_bn && hdr_i < 8 && hdr_n < HDR_LOG) begin
			hdr_bytes[hdr_n][hdr_i] <= head_do;
			hdr_i                   <= hdr_i + 1;
			if (hdr_i == 7) hdr_n <= hdr_n + 1;
		end
	end

	task automatic header_log();
		string line;
		line = "INFO: DOS expects  ";
		for (int i = 0; i < 8; i++) line = {line, $sformatf(" %02h", dram(8'h24 + i))};
		$display("%s", line);
		for (int k = 0; k < hdr_n; k++) begin
			line = $sformatf("INFO: after sync %2d", k);
			for (int i = 0; i < 8; i++) line = {line, $sformatf(" %02h", hdr_bytes[k][i])};
			$display("%s", line);
		end
	endtask

	// Where is the drive CPU spending its time? Counting bus addresses while the
	// job is outstanding turns "it hangs" into a ROM address that can be looked up
	// in the DOS listing.
	int   addr_hits[65536];
	logic sampling = 0;
	int   read_flow[11];
	int   sync_poll_overlap = 0;
	int   sync_poll_seen = 0;
	logic data_sync_wait = 0;
	int   data_wait_syncs = 0;
	int   data_wait_polls = 0;

	always @(posedge clk) begin
		if (sampling && sync_d && !gcr_sync_n && data_sync_wait)
			data_wait_syncs <= data_wait_syncs + 1;
		if (sampling && cpu_step) begin
			addr_hits[cpu_a[15:0]] <= addr_hits[cpu_a[15:0]] + 1;
			// Milestones through the DOS read path: header wait, accepted header,
			// data wait, completed payload, timeout and final status write.
			case (cpu_a[15:0])
				16'hF3BE: read_flow[0] <= read_flow[0] + 1;
				16'hF3C8: read_flow[1] <= read_flow[1] + 1;
				16'hF423: read_flow[2] <= read_flow[2] + 1;
				16'hF4D1: begin
					read_flow[3] <= read_flow[3] + 1;
					data_sync_wait <= 1;
				end
				16'hF4D4: begin
					read_flow[4] <= read_flow[4] + 1;
					data_sync_wait <= 0;
				end
				16'hF4ED: read_flow[5] <= read_flow[5] + 1;
				16'hF553: read_flow[6] <= read_flow[6] + 1;
				16'hF969: read_flow[7] <= read_flow[7] + 1;
				16'hF54D: read_flow[8] <= read_flow[8] + 1;
				16'hF54E: read_flow[9] <= read_flow[9] + 1;
				16'hF556: read_flow[10] <= read_flow[10] + 1;
			endcase
			if (cpu_rw && cpu_a[15:0] == 16'h1C00 && !gcr_sync_n) begin
				sync_poll_overlap <= sync_poll_overlap + 1;
				if (!dut.c157x.drives[0].c157x_drv.c157x_logic.cpu_di[7])
					sync_poll_seen <= sync_poll_seen + 1;
				if (data_sync_wait &&
				    !dut.c157x.drives[0].c157x_drv.c157x_logic.cpu_di[7])
					data_wait_polls <= data_wait_polls + 1;
			end
		end
	end

	task automatic hot_addresses(input int unsigned n, input int unsigned lo,
	                             input int unsigned hi, input string tag);
		int best, best_a, total;
		total = 0;
		for (int a = lo; a <= hi; a++) total += addr_hits[a];
		$display("INFO: %s: %0d sampled bus cycles", tag, total);
		for (int k = 0; k < n; k++) begin
			best = 0; best_a = 0;
			for (int a = lo; a <= hi; a++)
				if (addr_hits[a] > best) begin best = addr_hits[a]; best_a = a; end
			if (!best) break;
			$display("INFO: %s hot $%04h  %0d cycles", tag, best_a, best);
			addr_hits[best_a] = 0;
		end
	endtask

	task automatic page_profile();
		int page_total;
		for (int p = 0; p < 16; p++) begin
			page_total = 0;
			for (int a = p*4096; a < (p+1)*4096; a++) page_total += addr_hits[a];
			if (page_total)
				$display("INFO: page $%01h000  %0d cycles", p, page_total);
		end
	endtask

	task automatic zero_page();
		string line;
		for (int row = 0; row < 16; row++) begin
			line = $sformatf("INFO: zp $%02h:", row*8);
			for (int i = 0; i < 8; i++) line = {line, $sformatf(" %02h", dram(row*8 + i))};
			$display("%s", line);
		end
	endtask

	task automatic snapshot(input string tag);
		$display("INFO: %s pb_o=%02h pb_oe=%02h mtr=%0b act=%0b stp=%02b track=%0d present=%0b soe=%0b irq_n=%0b",
		         tag, via2_pb_o, via2_pb_oe, mtr, act, stp, track_num, disk_pres, soe, cpu_irq_n);
		$display("INFO: %s job=%02h trk=%0d sec=%0d | cpu bytes=%0d syncs=%0d steps=%0d reads=%0d",
		         tag, dram(0), dram(6), dram(7), cpu_pulses, sync_falls, stp_moves, host_reads);
		// $20 is the drive state and $62/$63 the vector for the controller state
		// that runs on the next interrupt. Together they name exactly which part
		// of the disk controller the drive is living in.
		$display("INFO: %s state=$%02h next=$%02h%02h steps_left=$%02h retries=$%02h",
		         tag, dram(8'h20), dram(8'h63), dram(8'h62),
		         dram(8'h4A), dram(8'h43));
		$display("INFO: %s acceleration=%03b", tag, accl);
	endtask

	task automatic check(input logic cond, input string what);
		if (cond) $display("PASS: %s", what);
		else begin
			$display("FAIL: %s", what);
			errors++;
		end
	endtask

	initial begin
		time        t0;
		logic [7:0] job;
		logic [7:0] post_job;
		logic       done;

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

		// Race through the power-up self test.
		fast_mode = 1;
		force dut.c157x.div = fast_div;
		#250_000_000;
		release dut.c157x.div;
		fast_mode = 0;
		#5_000_000;
		snapshot("self test done");

		// Let the disk-change timeout expire. Dropping it in two steps keeps the
		// write-protect flip that tells the DOS a disk arrived.
		dut.c157x.drives[0].c157x_drv.ch_timeout = 25'h0800100;
		#2_000_000;
		dut.c157x.drives[0].c157x_drv.ch_timeout = 25'd200;
		#20_000_000;
		snapshot("disk present");
		check(disk_pres, "drive reports a disk in place");
		if (!sim_boot1_path_pkg::FORCE_FAST)
			check(accl == 3'b000,
			      "drive remains at the authentic 1 MHz compatibility clock");
		if (sim_boot1_path_pkg::FORCE_FAST) begin
			force dut.c157x.drives[0].c157x_drv.c157x_logic.accl = 3'b111;
			$display("INFO: forcing the 1571 CPU to its 2 MHz state");
		end

		// A raw job goes straight to the controller and skips the INITIALIZE that
		// normally teaches the DOS which disk is in the drive, so the header ID
		// check at the end of the read would fail against a master ID of zero.
		// Seed it from the BAM, where the DOS would have read it.
		dut.c157x.drives[0].c157x_drv.c157x_logic.ram.ram[8'h12] = d64[357*256 + 'hA2];
		dut.c157x.drives[0].c157x_drv.c157x_logic.ram.ram[8'h13] = d64[357*256 + 'hA3];

		// Post a job for track 18, sector 1 into buffer 0 of the job queue. The
		// default is $80 (read), but the first thing the DOS actually issues after
		// a mount is $b0 (seek), so +JOB=b0 exercises the path the real LOAD takes.
		post_job = sim_boot1_path_pkg::JOB_CODE;
		dut.c157x.drives[0].c157x_drv.c157x_logic.ram.ram[6] = 8'd18;
		dut.c157x.drives[0].c157x_drv.c157x_logic.ram.ram[7] = 8'd1;
		dut.c157x.drives[0].c157x_drv.c157x_logic.ram.ram[0] = post_job;
		$display("INFO: posted job $%02h for track 18 sector 1 at %0t", post_job, $time);

		// The controller picks the job up on its next interrupt, puts the drive in
		// state $A0 and waits 50 more interrupts for the motor to reach speed. At
		// one tick per 14 ms that is most of a second of dead time, and this test
		// is about the read rather than the delay, so let the counter run down to
		// its last few ticks once the job has been taken.
		#40_000_000;
		$display("INFO: job taken: state $%02h, spin-up counter $%02h",
		         dram(8'h20), dram(8'h48));
		if (dram(8'h48) > 8'd2)
			dut.c157x.drives[0].c157x_drv.c157x_logic.ram.ram[8'h48] = 8'd2;

		job = dram(8'h20);
		for (int ms = 0; ms < 60 && job[7]; ms++) begin
			#1_000_000;
			job = dram(8'h20);
		end
		$display("INFO: motor up to speed, drive state $%02h", dram(8'h20));

		sampling = 1;

		done = 0;
		// At the authentic 1 MHz clock, DOS can finish its setup just after the
		// requested sector passes. Cover enough time for that sector to return on
		// the following rotation instead of treating normal rotational latency as
		// a controller hang.
		for (int ms = 0; ms < 600 && !done; ms++) begin
			#1_000_000;
			job = dram(0);
			if (!job[7]) begin
				done = 1;
				$display("INFO: controller finished the job after %0d ms with status $%02h",
				         ms + 1, job);
			end
			if ((ms % 50) == 49) snapshot($sformatf("t+%0d ms", ms + 1));
		end
		sampling = 0;

		snapshot("final");
		$display("INFO: %0d CPU interrupts while the job was outstanding", irq_count);
		zero_page();
		header_log();
		page_profile();
		$display("INFO: %0d SOE transitions logged", soe_events);
		for (int i = 0; i < soe_events; i++)
			$display("INFO: SOE -> %0d at $%04h", soe_dir[i][0], soe_where[i]);
		$display("INFO: write path VIA2_ORA=%0d sum=%04h GCR_buffer=%0d sum=%04h decode_errors=%0d",
		         via2_ora_writes, via2_ora_sum, gcr_buffer_writes, gcr_buffer_sum,
		         gcr_decode_errors);
		$display("INFO: read flow header_wait=%0d header_bytes=%0d header_ok=%0d data_start=%0d data_wait=%0d data_done=%0d timeout=%0d status=%0d",
		         read_flow[0], read_flow[1], read_flow[2], read_flow[3],
		         read_flow[4], read_flow[5], read_flow[6], read_flow[7]);
		$display("INFO: target-header match=%0d mismatch=%0d sync-wait entries=%0d",
		         read_flow[8], read_flow[9], read_flow[10]);
		$display("INFO: sync polling overlap=%0d CPU-low=%0d",
		         sync_poll_overlap, sync_poll_seen);
		$display("INFO: while waiting for data sync: syncs=%0d CPU-low polls=%0d",
		         data_wait_syncs, data_wait_polls);
		hot_addresses(20, 16'hF000, 16'hFFFF, "F000-FFFF");
		hot_addresses(10, 16'h1C00, 16'h1CFF, "VIA2");
		hot_addresses(20, 16'h0000, 16'hEFFF, "below F000");
		check(done, "disk controller ran the job at all");
		if (done) check(dram(0) == 8'h01,
		                $sformatf("job completed without error (status $%02h)", dram(0)));
		check(saw_dir_read, "controller asked the host for track 18 (LBA 357)");

		if (errors == 0) $display("ALL CHECKS PASSED");
		else             $display("%0d CHECK(S) FAILED", errors);
		$finish;
	end

endmodule
