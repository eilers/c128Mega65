`timescale 1ns/1ps

// Golden-model test for the sector-image GCR path.
//
// The point of this bench is that it never reuses the DUT's own encoding tables.
// It decodes the emitted surface with a canonical 4-to-5 GCR table written out
// literally, so an encoder that is self-consistent but wrong (mirrored codes, for
// example) fails here instead of on real hardware.
//
// It also sweeps the density input, because byte-ready is derived from the bit
// clock counter and DOS selects a different density for every zone of the disk.

module c157x_gcr_path_test #(parameter integer TRACK=18);

	localparam integer ZONE       = (TRACK > 35) ? TRACK - 35 : TRACK;
	localparam integer SECTOR_MAX = ZONE < 18 ? 20 : ZONE < 25 ? 18 : ZONE < 31 ? 17 : 16;
	localparam integer NSECT      = SECTOR_MAX + 1;
	localparam [31:0]  FILL_LBA   = (TRACK == 18) ? 32'd357 : 32'd1040;

	reg clk = 0, sd_clk = 0;
	always #1 clk = ~clk;
	always #3 sd_clk = ~sd_clk;

	reg         ce = 1;
	reg  [7:0]  din = 0;
	reg         mode = 1;
	reg         mtr = 1;
	reg  [1:0]  freq = 0;
	reg  [6:0]  active_track = TRACK;
	reg         busy = 1;
	reg  [31:0] sd_lba = FILL_LBA;
	reg         sd_bank = TRACK > 35;
	reg  [12:0] sd_buff_addr = 0;
	reg  [7:0]  sd_buff_dout = 0;
	reg         sd_buff_wr = 0;
	wire [7:0]  dout, sd_buff_din;
	wire        sync_n, byte_n, we;

	integer errors = 0;
	integer i, j, k;

	c1541_gcr dut (
		.clk(clk), .ce(ce), .dout(dout), .din(din), .mode(mode), .mtr(mtr),
		.freq(freq), .sync_n(sync_n), .byte_n(byte_n), .track(active_track),
		.busy(busy), .we(we), .sd_clk(sd_clk), .sd_lba(sd_lba),
		.sd_bank(sd_bank),
		.sd_buff_addr(sd_buff_addr), .sd_buff_dout(sd_buff_dout),
		.sd_buff_din(sd_buff_din), .sd_buff_wr(sd_buff_wr)
	);

	// ---------------------------------------------------------------- fixtures

	function automatic [7:0] pattern(input integer sector, input integer offset);
		pattern = (sector * 7 + offset * 73 + TRACK) & 8'hFF;
	endfunction

	reg [7:0] id1_exp = 0, id2_exp = 0;

	// -------------------------------------------------------- canonical tables

	function automatic [4:0] canon_code(input [3:0] n);
		case (n)
			4'h0: canon_code = 5'b01010;
			4'h1: canon_code = 5'b01011;
			4'h2: canon_code = 5'b10010;
			4'h3: canon_code = 5'b10011;
			4'h4: canon_code = 5'b01110;
			4'h5: canon_code = 5'b01111;
			4'h6: canon_code = 5'b10110;
			4'h7: canon_code = 5'b10111;
			4'h8: canon_code = 5'b01001;
			4'h9: canon_code = 5'b11001;
			4'hA: canon_code = 5'b11010;
			4'hB: canon_code = 5'b11011;
			4'hC: canon_code = 5'b01101;
			4'hD: canon_code = 5'b11101;
			4'hE: canon_code = 5'b11110;
			default: canon_code = 5'b10101;
		endcase
	endfunction

	// Returns {valid, nibble}.
	function automatic [4:0] canon_decode(input [4:0] code);
		integer n;
		begin
			canon_decode = 5'b0_0000;
			for (n = 0; n < 16; n = n + 1)
				if (canon_code(n[3:0]) == code) canon_decode = {1'b1, n[3:0]};
		end
	endfunction

	// ------------------------------------------------------- surface capturing

	localparam integer MAX_BLK  = 12;
	localparam integer MAX_BLKB = 512;

	reg  [7:0] blk [0:MAX_BLK-1][0:MAX_BLKB-1];
	integer    blk_len [0:MAX_BLK-1];
	integer    nblocks = 0;
	integer    curlen = 0;
	reg        capture = 0;
	reg        sync_d = 1;

	always @(posedge clk) begin
		sync_d <= sync_n;
		if (capture && sync_d && !sync_n) begin
			if (curlen > 0 && nblocks < MAX_BLK) begin
				blk_len[nblocks] = curlen;
				nblocks = nblocks + 1;
			end
			curlen = 0;
		end
	end

	always @(negedge byte_n) begin
		if (capture && mode && nblocks < MAX_BLK && curlen < MAX_BLKB) begin
			blk[nblocks][curlen] = dout;
			curlen = curlen + 1;
		end
	end

	task automatic reset_capture;
		begin
			capture = 0;
			nblocks = 0;
			curlen  = 0;
			capture = 1;
		end
	endtask

	// --------------------------------------------------------------- decoding

	reg [7:0] decoded [0:299];

	function automatic bit_at(input integer bidx, input integer pos);
		bit_at = blk[bidx][pos/8][7 - (pos % 8)];
	endfunction

	// Decodes want_bytes GCR bytes out of a captured block, high nibble first.
	// Returns 0 when the surface contained a code that is not valid GCR.
	function automatic integer decode_block(input integer bidx, input integer want_bytes);
		integer b, half, bit_i, pos;
		reg [4:0] code, dec;
		reg [7:0] value;
		begin
			decode_block = 1;
			if (blk_len[bidx] * 8 < want_bytes * 10) decode_block = 0;
			else begin
				for (b = 0; b < want_bytes; b = b + 1) begin
					value = 0;
					for (half = 0; half < 2; half = half + 1) begin
						code = 0;
						for (bit_i = 0; bit_i < 5; bit_i = bit_i + 1) begin
							pos  = (b * 2 + half) * 5 + bit_i;
							code = {code[3:0], bit_at(bidx, pos)};
						end
						dec = canon_decode(code);
						if (!dec[4]) decode_block = 0;
						value = half == 0 ? {dec[3:0], 4'h0} : {value[7:4], dec[3:0]};
					end
					decoded[b] = value;
				end
			end
		end
	endfunction

	// ------------------------------------------------------------ verification

	integer first_sector = -1;

	task automatic check_header(input integer bidx, input integer expect_sector);
		reg [7:0] cks;
		begin
			if (!decode_block(bidx, 6)) begin
				$display("FAIL track %0d freq %0d: header block is not valid GCR", TRACK, freq);
				errors = errors + 1;
			end
			else begin
				cks = TRACK ^ expect_sector ^ id1_exp ^ id2_exp;
				if (decoded[0] !== 8'h08 || decoded[1] !== cks ||
				    decoded[2] !== expect_sector[7:0] || decoded[3] !== TRACK[7:0] ||
				    decoded[4] !== id2_exp || decoded[5] !== id1_exp) begin
					$display("FAIL track %0d freq %0d header: got %02h %02h %02h %02h %02h %02h, want 08 %02h %02h %02h %02h %02h",
					         TRACK, freq, decoded[0], decoded[1], decoded[2], decoded[3], decoded[4], decoded[5],
					         cks, expect_sector[7:0], TRACK[7:0], id2_exp, id1_exp);
					errors = errors + 1;
				end
			end
		end
	endtask

	task automatic check_data(input integer bidx, input integer expect_sector);
		reg [7:0] cks;
		integer n, bad;
		begin
			if (!decode_block(bidx, 258)) begin
				$display("FAIL track %0d freq %0d: data block is not valid GCR", TRACK, freq);
				errors = errors + 1;
			end
			else begin
				bad = 0;
				cks = 0;
				for (n = 0; n < 256; n = n + 1) begin
					cks = cks ^ pattern(expect_sector, n);
					if (decoded[n+1] !== pattern(expect_sector, n) && bad < 3) begin
						$display("FAIL track %0d freq %0d sector %0d byte %0d: got %02h want %02h",
						         TRACK, freq, expect_sector, n, decoded[n+1], pattern(expect_sector, n));
						bad = bad + 1;
					end
				end
				if (decoded[0] !== 8'h07) begin
					$display("FAIL track %0d freq %0d: data marker %02h", TRACK, freq, decoded[0]);
					bad = bad + 1;
				end
				if (decoded[257] !== cks) begin
					$display("FAIL track %0d freq %0d sector %0d: checksum %02h want %02h",
					         TRACK, freq, expect_sector, decoded[257], cks);
					bad = bad + 1;
				end
				if (bad) errors = errors + 1;
			end
		end
	endtask

	// Reads a captured header block's sector number without judging it.
	function automatic integer header_sector(input integer bidx);
		begin
			header_sector = -1;
			if (decode_block(bidx, 6) && decoded[0] === 8'h08) header_sector = decoded[2];
		end
	endfunction

	task automatic verify_freq(input [1:0] f, input integer sectors);
		integer b, sect, checked;
		begin
			freq = f;
			busy = 1;
			repeat (200) @(posedge clk);
			reset_capture();
			busy = 0;

			fork
				begin wait (nblocks >= 2*sectors + 1); end
				begin #3ms; end
			join_any
			disable fork;
			busy = 1;
			capture = 0;

			if (nblocks < 2*sectors + 1) begin
				$display("FAIL track %0d freq %0d: only %0d blocks reached the surface",
				         TRACK, freq, nblocks);
				errors = errors + 1;
			end
			else begin
				// Block 0 may be a partial capture, so start on the first header.
				checked = 0;
				b = 0;
				while (b + 1 < nblocks && checked < sectors) begin
					sect = header_sector(b);
					if (sect >= 0) begin
						if (first_sector < 0) first_sector = sect;
						check_header(b, sect);
						check_data(b+1, sect);
						if (sect > SECTOR_MAX) begin
							$display("FAIL track %0d freq %0d: sector %0d exceeds %0d",
							         TRACK, freq, sect, SECTOR_MAX);
							errors = errors + 1;
						end
						checked = checked + 1;
						b = b + 2;
					end
					else b = b + 1;
				end
				if (checked < sectors) begin
					$display("FAIL track %0d freq %0d: found %0d headers, wanted %0d",
					         TRACK, freq, checked, sectors);
					errors = errors + 1;
				end
			end
		end
	endtask

	task automatic verify_head_switch;
		begin
			if (TRACK > 35) begin
				// Run side 0, then select side 1 at a deliberately arbitrary bit
				// phase. The very first complete blocks after the transition must
				// be a clean side-1 header/data pair, not a splice of both banks.
				freq = 0;
				active_track = TRACK - 35;
				busy = 0;
				#12345;
				reset_capture();
				active_track = TRACK;
				fork
					begin wait (nblocks >= 2); end
					begin #3ms; end
				join_any
				disable fork;
				busy = 1;
				capture = 0;
				if (nblocks < 2) begin
					$display("FAIL track %0d: no complete blocks after head switch", TRACK);
					errors = errors + 1;
				end
				else begin
					check_header(0, 0);
					check_data(1, 0);
				end
			end
		end
	endtask

	// ------------------------------------------------------------- write path

	integer replay_len = 0;
	reg [7:0] replay [0:MAX_BLKB-1];
	integer replay_idx = 0;
	reg replaying = 0;

	// The drive samples one surface byte per eight bit clocks. Advance in lockstep
	// with that and stop at the end of the recording instead of wrapping, so the
	// decoder never sees a second, misframed copy of the block.
	always @(posedge clk) begin
		if (replaying && dut.bit_clk_en && dut.bit_cnt == 3'd7) begin
			if (replay_idx + 1 < replay_len) begin
				replay_idx <= replay_idx + 1;
				din        <= replay[replay_idx + 1];
			end
			else din <= 8'hFF;
		end
	end

	task automatic host_write(input integer addr, input [7:0] value);
		begin
			@(negedge sd_clk);
			sd_buff_addr = addr[12:0];
			sd_buff_dout = value;
			sd_buff_wr   = 1;
			@(negedge sd_clk);
			sd_buff_wr   = 0;
		end
	endtask

	task automatic host_read(input integer addr, output [7:0] value);
		begin
			@(negedge sd_clk);
			sd_buff_addr = addr[12:0];
			@(posedge sd_clk);
			@(negedge sd_clk);
			value = sd_buff_din;
		end
	endtask

	// Replays a recorded data block into the write decoder and proves that the
	// decoded payload lands in one sector of the track buffer.
	task automatic verify_write_path(input integer data_block);
		integer n, s, n_match, hit;
		reg [7:0] value;
		reg ok;
		begin
			if (data_block < 0) begin
				$display("FAIL track %0d: no data block captured for the write test", TRACK);
				errors = errors + 1;
			end
			else begin
				if (!decode_block(data_block, 258)) begin
					$display("FAIL track %0d: write source block is not valid GCR", TRACK);
					errors = errors + 1;
				end
				// decoded[1..256] is the payload the drive must write back.
				// DOS precedes a data block with five sync bytes. The first of those
				// five is still in the shift register when write mode starts, so four
				// go into the recording. Together they are 40 bits, which realigns the
				// five-bit nibble framing with the block boundary.
				replay_len = blk_len[data_block] + 4;
				for (n = 0; n < 4; n = n + 1) replay[n] = 8'hFF;
				for (n = 0; n < blk_len[data_block]; n = n + 1)
					replay[n+4] = blk[data_block][n];

				busy   = 1;
				sd_lba = 0;                   // do not disturb the latched disk ID
				repeat (200) @(posedge clk);
				for (n = 0; n < NSECT * 256; n = n + 1) host_write(n, 8'hE5);

				freq       = 0;
				replay_idx = 0;
				din        = replay[0];
				busy       = 0;
				// Enter write mode on a sync mark, which is where DOS takes over too.
				@(negedge sync_n);
				@(posedge clk);
				mode       = 0;
				replaying  = 1;
				#1500us;
				replaying  = 0;
				mode       = 1;
				busy       = 1;
				repeat (200) @(posedge clk);

				n_match = 0;
				hit     = -1;
				for (s = 0; s < NSECT; s = s + 1) begin
					ok = 1;
					for (n = 0; n < 256; n = n + 1) begin
						host_read(s * 256 + n, value);
						if (value !== decoded[n+1]) ok = 0;
					end
					if (ok) begin
						n_match = n_match + 1;
						hit = s;
					end
				end

				if (n_match == 0) begin
					$display("FAIL track %0d: the write decoder stored no complete sector", TRACK);
					errors = errors + 1;
				end
				else
					$display("track %0d: write decoder restored sector %0d (%0d sector(s) matched)",
					         TRACK, hit, n_match);
			end
		end
	endtask

	// ------------------------------------------------------------------- main

	integer first_data_block;

	initial begin
		// Load every linear sector of the track, exactly as the host would.
		if (TRACK > 35) begin
			// Poison the corresponding side-0 bank so a mixed-head block is
			// observable instead of accidentally containing matching bytes.
			sd_bank = 0;
			for (i = 0; i < NSECT; i = i + 1)
				for (j = 0; j < 256; j = j + 1)
					host_write(i * 256 + j, pattern(i, j) ^ 8'hFF);
		end
		sd_bank = TRACK > 35;
		for (i = 0; i < NSECT; i = i + 1)
			for (j = 0; j < 256; j = j + 1)
				host_write(i * 256 + j, pattern(i, j));
		@(negedge sd_clk);
		sd_buff_wr = 0;

		// The drive latches the disk ID out of the BAM sector of track 18.
		if (TRACK == 18) begin
			id1_exp = pattern(0, 'hA2);
			id2_exp = pattern(0, 'hA3);
		end

		verify_head_switch();

		// DOS picks a different density per zone, so every value has to work.
		verify_freq(2'd0, 2);
		verify_freq(2'd1, 2);
		verify_freq(2'd2, 2);
		verify_freq(2'd3, 2);

		// Re-capture one clean sector at the slowest density for the write test.
		freq = 0;
		busy = 1;
		repeat (200) @(posedge clk);
		reset_capture();
		busy = 0;
		fork
			begin wait (nblocks >= 3); end
			begin #3ms; end
		join_any
		disable fork;
		busy = 1;
		capture = 0;

		first_data_block = -1;
		for (k = 0; k + 1 < nblocks; k = k + 1)
			if (first_data_block < 0 && header_sector(k) >= 0) first_data_block = k + 1;

		verify_write_path(first_data_block);

		if (errors == 0)
			$display("ALL CHECKS PASSED: track %0d GCR read/write path", TRACK);
		else
			$display("FAIL: track %0d GCR path had %0d errors", TRACK, errors);
		$finish;
	end
endmodule

module tb_c1541_read_write;
	c157x_gcr_path_test #(.TRACK(18)) test();
endmodule

module tb_c1571_read_write;
	c157x_gcr_path_test #(.TRACK(53)) test();
endmodule
