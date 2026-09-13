// Commodore 1541 sector-image GCR (read/write).
// Original by Dar; MiSTer adaptation by Alexey Melnikov; QNICE RAM adaptation
// by sy2002. Ported from C64MEGA65 commit 5afb90e for the C128 157x drive.
module c1541_gcr
(
	input             clk,
	input             ce,
	output reg  [7:0] dout,
	input       [7:0] din,
	input             mode,
	input             mtr,
	input       [1:0] freq,
	output            sync_n,
	output reg        byte_n,
	input       [6:0] track,
	input             busy,
	output reg        we,
	input             sd_clk,
	input      [31:0] sd_lba,
	input             sd_bank,
	input      [12:0] sd_buff_addr,
	input       [7:0] sd_buff_dout,
	output      [7:0] sd_buff_din,
	input             sd_buff_wr
);

reg sync_in_n = 0;
assign sync_n = ~mtr | busy | sync_in_n;

// D71 side 1 uses logical track numbers 36..70 but repeats the same four
// physical density zones as tracks 1..35.
wire [5:0] zone_track = track > 35 ? track - 35 : track;
wire [4:0] sector_max = zone_track < 18 ? 5'd20 :
                        zone_track < 25 ? 5'd18 :
                        zone_track < 31 ? 5'd17 : 5'd16;

reg [7:0] id1=0, id2=0;
reg [12:0] buff_addr = 0;
wire [7:0] buff_do;
reg [7:0] buff_di = 0;
reg [4:0] sector = 0;
reg byte_in = 0;
reg [8:0] byte_cnt = 0;
reg nibble = 0;
reg state = 0;
reg [7:0] data_cks = 0;
reg [7:0] gcr_byte_out = 0;
reg [4:0] gcr_nibble_out = 0;
reg [7:0] hdr_cks = 0;
reg decode_error = 0;
reg [5:0] bit_clk_cnt = 0;
reg mode_r1 = 0;
reg mode_r2 = 0;
reg autorise_write = 0;
reg autorise_count = 0;
reg [5:0] sync_cnt = 0;
reg [7:0] gcr_byte = 0;
reg [2:0] bit_cnt = 0;
reg [3:0] gcr_bit_cnt = 0;
reg [5:0] write_ones = 0;
reg write_aligned = 0;

// Preserve the original sector generator's header-to-data layout.
localparam [8:0] HEADER_TO_DATA_BYTES = 9'd16;

wire [7:0] data_header = byte_cnt == 0 ? 8'h08 :
                         byte_cnt == 1 ? hdr_cks :
                         byte_cnt == 2 ? sector :
                         byte_cnt == 3 ? track :
                         byte_cnt == 4 ? id2 :
                         byte_cnt == 5 ? id1 : 8'h0F;

wire [7:0] data_body = byte_cnt == 0   ? 8'h07 :
                       byte_cnt == 257 ? data_cks :
                       byte_cnt == 258 ? 8'h00 :
                       byte_cnt == 259 ? 8'h00 :
                       byte_cnt >= 260 ? 8'h0F : buff_do;

wire [7:0] data = state ? data_body : data_header;
wire [2:0] gcr_bit_index = 3'd4 - gcr_bit_cnt[2:0];
wire [4:0] gcr_nibble;
wire [3:0] nibble_out;
wire decode_valid;
c1541_gcr_codec nibble_codec
(
	.encode_nibble(nibble ? data[3:0] : data[7:4]),
	.encoded_code(gcr_nibble),
	.decode_code(gcr_nibble_out),
	.decoded_nibble(nibble_out),
	.decode_valid(decode_valid)
);

reg bit_clk_en = 0;
reg [6:0] old_track = 0;
always @(posedge clk) begin
	bit_clk_en <= 0;
	if(ce) begin
		old_track <= track;
		mode_r1 <= mode;
		byte_n <= 1;
		if ((old_track != track) | (mode_r1 ^ mode) | busy | ~mtr)
			bit_clk_cnt <= {freq,2'b00};
		else begin
			bit_clk_cnt <= bit_clk_cnt + 1'b1;
			if(byte_in && bit_clk_cnt[5:4] == 1) byte_n <= 0;
			if (&bit_clk_cnt) begin
				bit_clk_en <= 1;
				bit_clk_cnt <= {freq,2'b00};
			end
		end
	end
end

always @(posedge sd_clk) begin
	if(sd_lba == 357 && sd_buff_wr) begin
		if(sd_buff_addr == 'hA2) id1 <= sd_buff_dout;
		if(sd_buff_addr == 'hA3) id2 <= sd_buff_dout;
	end
end

// Keep the sectors under both 1571 heads resident. The generator reads the bank
// selected by the logical track while the host independently fills or drains the bank
// belonging to its current request. A side change therefore changes only this bank bit;
// it never has to wait for QNICE to copy another track into the FPGA.
wire active_bank = track > 35;

iecdrv_mem #(8,14) buffer
(
	.clock_a(sd_clk),
	.address_a({sd_bank,sd_buff_addr}),
	.data_a(sd_buff_dout),
	.wren_a(sd_buff_wr),
	.q_a(sd_buff_din),
	.clock_b(clk),
	.address_b({active_bank,buff_addr}),
	.data_b(buff_di),
	.wren_b(we),
	.q_b(buff_do)
);

always @(posedge clk) begin
	hdr_cks <= track ^ sector ^ id1 ^ id2;
	we <= 0;
	// Port B is a registered RAM output. Changing heads changes its bank address,
	// but the old bank's byte remains on buff_do for one clock and the encoder may
	// otherwise splice it into a block already in progress. Start a fresh sync and
	// header on every logical-track change so DOS never sees a mixed-head block.
	if (old_track != track) begin
		sync_in_n <= 0;
		sync_cnt <= 0;
		state <= 0;
		sector <= 0;
		byte_cnt <= 0;
		nibble <= 0;
		gcr_bit_cnt <= 0;
		bit_cnt <= 0;
		byte_in <= 0;
		gcr_byte <= 0;
		data_cks <= 0;
		mode_r2 <= mode;
	end
	else if (sector > sector_max)
		sector <= 0;
	else if (bit_clk_en) begin
		mode_r2 <= mode;
		if (mode) autorise_write <= 0;
		if (mode ^ mode_r2) begin
			if (mode) begin
				sync_in_n <= 0;
				sync_cnt <= 0;
				state <= 0;
			end else begin
				byte_cnt <= 0;
				nibble <= 0;
				gcr_bit_cnt <= 0;
				write_ones <= 0;
				write_aligned <= 0;
				bit_cnt <= 0;
				gcr_byte <= 0;
				data_cks <= 0;
				decode_error <= 0;
			end
		end

		byte_in <= 0;
		if (~sync_in_n & mode) begin
			byte_cnt <= 0;
			nibble <= 0;
			gcr_bit_cnt <= 0;
			bit_cnt <= 0;
			dout <= 8'hFF;
			gcr_byte <= 0;
			data_cks <= 0;
			sync_cnt <= sync_cnt + 1'd1;
			if (sync_cnt == 39) begin
				sync_cnt <= 0;
				sync_in_n <= 1;
			end
		end else begin
			// The write gate can open at any phase of the byte already in the VIA
			// shift register. At 1 MHz the discarded prefix happened to align the
			// following GCR stream; at 2 MHz it did not, so the $07 data marker was
			// never decoded. A real drive establishes framing from the run of sync
			// one-bits. Do the same: after at least four $FF bytes, make the first
			// zero bit the first bit of a five-bit GCR code.
			if (~mode && ~write_aligned) begin
				if (gcr_byte_out[~bit_cnt]) begin
					if (~&write_ones) write_ones <= write_ones + 1'b1;
				end else if (write_ones >= 32) begin
					write_aligned <= 1;
					write_ones <= 0;
					gcr_bit_cnt <= 1;
					gcr_nibble_out <= 0;
					nibble <= 0;
					byte_cnt <= 0;
					data_cks <= 0;
					decode_error <= 0;
				end else begin
					write_ones <= 0;
				end
			end else begin
				gcr_bit_cnt <= gcr_bit_cnt + 1'b1;
				if (gcr_bit_cnt == 4) begin
					gcr_bit_cnt <= 0;
					if (nibble) begin
						nibble <= 0;
						buff_addr <= {sector,byte_cnt[7:0]};
						if (!byte_cnt) data_cks <= 0;
						else data_cks <= data_cks ^ data;
						if (mode | autorise_count) byte_cnt <= byte_cnt + 1'b1;
					end else begin
						nibble <= 1;
						if (~mode && buff_di == 'h07) begin
							autorise_write <= 1;
							autorise_count <= 1;
							decode_error <= 0;
						end
						if (byte_cnt[8]) begin
							autorise_write <= 0;
							autorise_count <= 0;
						end
					end
				end
			end

			bit_cnt <= bit_cnt + 1'b1;
			if (bit_cnt == 7) begin
				byte_in <= 1;
				gcr_byte_out <= din;
			end

			if (~state) begin
				if (byte_cnt == HEADER_TO_DATA_BYTES) begin
					sync_in_n <= 0;
					state <= 1;
				end
			end else if (byte_cnt == 273) begin
				sync_in_n <= 0;
				state <= 0;
				if (sector == sector_max) sector <= 0;
				else sector <= sector + 1'b1;
			end

			// GCR goes onto the surface most significant bit first, and the write
			// decoder below reassembles gcr_nibble_out in that same order. Emitting
			// bit 0 first would mirror every five-bit code, which no DOS can decode.
			gcr_byte <= {gcr_byte[6:0], gcr_nibble[gcr_bit_index]};
			if (bit_cnt == 7) dout <= {gcr_byte[6:0], gcr_nibble[gcr_bit_index]};
			if (mode | write_aligned) begin
				gcr_nibble_out <= {gcr_nibble_out[3:0], gcr_byte_out[~bit_cnt]};
				if (!gcr_bit_cnt) begin
					if (~mode && autorise_write && ~decode_valid) decode_error <= 1;
					if (nibble) buff_di[7:4] <= nibble_out;
					else buff_di[3:0] <= nibble_out;
				end
				if (gcr_bit_cnt == 1 && ~nibble && autorise_write && ~decode_error)
					we <= 1;
			end
		end
	end
end

endmodule
