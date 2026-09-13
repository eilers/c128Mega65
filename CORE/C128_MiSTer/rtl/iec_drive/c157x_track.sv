// 
// c1541_track
// Copyright (c) 2016 Sorgelig
//
// This source file is free software: you can redistribute it and/or modify
// it under the terms of the Lesser GNU General Public License as published
// by the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This source file is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
//
/////////////////////////////////////////////////////////////////////////

// Extended with support for 157x models by Erik Scheffers

module c157x_track
(
	input         clk,
	input         reset,
	
	output [31:0] sd_lba,
	output  [5:0] sd_blk_cnt,
	output reg    sd_rd,
	output reg    sd_wr,
	input         sd_ack,

	input   [1:0] freq,
	input         sector_mode,
	input         dual_side,
	input   [6:0] sector_track,
	input   [5:0] raw_blk_cnt,
	input         save_track,
	input         change,
	input   [7:0] track,
	output reg    busy,
	output reg    sd_bank
);

reg [31:0] lba;
reg [5:0] blk_cnt;
assign sd_lba = lba;
assign sd_blk_cnt = blk_cnt;

wire [7:0] track_s;
wire [6:0] sector_track_s;
wire [1:0] freq_s;
wire       sector_mode_s, dual_side_s, change_s, save_track_s, reset_s;

iecdrv_sync #(8) track_sync  (clk, track,      track_s);
iecdrv_sync #(7) sector_track_sync(clk, sector_track, sector_track_s);
iecdrv_sync #(2) freq_sync   (clk, freq,       freq_s);
iecdrv_sync #(1) sector_sync (clk, sector_mode,sector_mode_s);
iecdrv_sync #(1) ds_sync     (clk, dual_side,  dual_side_s);
iecdrv_sync #(1) change_sync (clk, change,     change_s);
iecdrv_sync #(1) save_sync   (clk, save_track, save_track_s);
iecdrv_sync #(1) reset_sync  (clk, reset,      reset_s);

localparam [9:0] START_SECTORS[0:35] =
'{  0, 21, 42, 63, 84,105,126,147,168,189,210,231,252,273,294,315,336,357,
  376,395,414,433,452,471,490,508,526,544,562,580,598,615,632,649,666,683 };

// Tracks 1..35 are side 0, tracks 36..70 repeat the same geometry on side 1. Anything
// outside that range would index past START_SECTORS, so it saturates on the last track.
function automatic [5:0] table_index(input [6:0] logical_track);
	reg [6:0] relative;
	begin
		relative = logical_track > 35 ? logical_track - 36 : logical_track - 1;
		table_index = relative > 34 ? 6'd34 : relative[5:0];
	end
endfunction

function automatic [31:0] linear_lba(input [6:0] logical_track);
	begin
		linear_lba = (logical_track > 35 ? 32'd683 : 32'd0) + START_SECTORS[table_index(logical_track)];
	end
endfunction

function automatic [5:0] linear_blocks(input [6:0] logical_track);
	reg [5:0] index;
	begin
		index = table_index(logical_track);
		linear_blocks = START_SECTORS[index+1] - START_SECTORS[index] - 1'd1;
	end
endfunction

// Sector 0 of track 18 holds the BAM, and bytes $A2/$A3 of it are the disk ID that
// c1541_gcr stamps into every sector header it generates.
localparam [31:0] BAM_LBA = 32'd357;

reg [7:0] cur_track = 0;
reg [7:0] track_new = 0;
reg [7:0] request_track = 0;
reg [7:0] cached_track[0:1];
reg old_change = 0;
reg update = 0;
reg saving = 0;
reg loading = 0;
reg prefetch = 0;
reg old_save_track = 0;
reg old_ack = 0;
reg id_fetch = 0;

function automatic track_bank(input [7:0] logical_track);
	begin
		track_bank = logical_track > 35;
	end
endfunction

function automatic [7:0] opposite_track(input [7:0] logical_track);
	begin
		opposite_track = logical_track > 35 ?
		                 logical_track - 8'd35 : logical_track + 8'd35;
	end
endfunction

always @(posedge clk) begin
	track_new <= sector_mode_s ? {1'b0, sector_track_s} : track_s;

	old_change <= change_s;
	if(~old_change & change_s) begin
		update <= 1;
		cached_track[0] <= '1;
		cached_track[1] <= '1;
	end
	
	old_ack <= sd_ack;
	if(sd_ack) {sd_rd,sd_wr} <= 0;

	if(reset_s) begin
		cur_track <= '1;
		busy      <= 0;
		sd_rd     <= 0;
		sd_wr     <= 0;
		saving    <= 0;
		loading   <= 0;
		prefetch  <= 0;
		update    <= 1;
		id_fetch  <= 0;
		sd_bank   <= 0;
		cached_track[0] <= '1;
		cached_track[1] <= '1;
	end
	else if(busy) begin
		if(old_ack && ~sd_ack) begin
			if(id_fetch) begin
				id_fetch  <= 0;
				loading   <= 1;
				request_track <= track_new;
				sd_bank   <= track_bank(track_new);
				lba       <= linear_lba(track_new[6:0]);
				blk_cnt   <= linear_blocks(track_new[6:0]);
				sd_rd     <= 1;
				prefetch  <= dual_side_s &&
				             cached_track[~track_bank(track_new)] !=
				             opposite_track(track_new);
			end
			else if(loading) begin
				cached_track[track_bank(request_track)] <= request_track;
				if(prefetch) begin
					request_track <= opposite_track(request_track);
					sd_bank   <= ~track_bank(request_track);
					lba       <= linear_lba(opposite_track(request_track));
					blk_cnt   <= linear_blocks(opposite_track(request_track));
					sd_rd     <= 1;
					prefetch  <= 0;
				end
				else begin
					loading   <= 0;
					// request_track is the bank that actually completed. If the
					// mechanics moved during the transfer, leaving this tag rather
					// than claiming the live track makes the idle path fetch the
					// newly requested cylinder next.
					cur_track <= request_track;
					busy      <= 0;
				end
			end
			else if(saving && (cur_track != track_new)) begin
				saving    <= 0;
				if(sector_mode_s &&
				   cached_track[track_bank(track_new)] == track_new) begin
					cur_track <= track_new;
					busy      <= 0;
				end
				else begin
					loading   <= sector_mode_s;
					request_track <= track_new;
					if(!sector_mode_s) cur_track <= track_new;
					sd_bank   <= sector_mode_s ? track_bank(track_new) : 1'b0;
					lba       <= sector_mode_s ? linear_lba(track_new[6:0]) :
					             {20'h00000, 2'b01, freq_s, track_new};
					blk_cnt   <= sector_mode_s ? linear_blocks(track_new[6:0]) : raw_blk_cnt;
					sd_rd     <= 1;
					prefetch  <= sector_mode_s && dual_side_s &&
					             cached_track[~track_bank(track_new)] !=
					             opposite_track(track_new);
				end
			end
			else begin
				saving    <= 0;
				busy      <= 0;
			end
		end
	end
	else begin
		old_save_track <= save_track_s;
		if((old_save_track ^ save_track_s) && ~&cur_track[7:1]) begin
			saving    <= 1;
			sd_bank   <= sector_mode_s ? track_bank(cur_track) : 1'b0;
			lba       <= sector_mode_s ? linear_lba(cur_track[6:0]) :
			             {20'h00000, 2'b01, freq_s, cur_track};
			blk_cnt   <= sector_mode_s ? linear_blocks(cur_track[6:0]) : raw_blk_cnt;
			sd_wr     <= 1;
			busy      <= 1;
		end
		else if(update && sector_mode_s) begin
			// First request after a mount or reset: pull the BAM so that the disk ID
			// is known before the DOS reads its first sector header. Without this the
			// headers carry ID $00,$00 until the head happens to reach track 18, and
			// the DOS rejects the very first access with error 29 while still on the
			// power-up track, so it never seeks there and never recovers.
			id_fetch  <= 1;
			sd_bank   <= 0;
			lba       <= BAM_LBA;
			blk_cnt   <= 0;
			sd_rd     <= 1;
			busy      <= 1;
			update    <= 0;
		end
		else if(cur_track != track_new || update) begin
			if(sector_mode_s &&
			   cached_track[track_bank(track_new)] == track_new) begin
				cur_track <= track_new;
			end
			else begin
				loading   <= sector_mode_s;
				request_track <= track_new;
				if(!sector_mode_s) cur_track <= track_new;
				sd_bank   <= sector_mode_s ? track_bank(track_new) : 1'b0;
				lba       <= sector_mode_s ? linear_lba(track_new[6:0]) :
				             {20'h00000, 2'b01, freq_s, track_new};
				blk_cnt   <= sector_mode_s ? linear_blocks(track_new[6:0]) : raw_blk_cnt;
				sd_rd     <= 1;
				busy      <= 1;
				prefetch  <= sector_mode_s && dual_side_s &&
				             cached_track[~track_bank(track_new)] !=
				             opposite_track(track_new);
			end
			update <= 0;
		end
	end
end

endmodule
