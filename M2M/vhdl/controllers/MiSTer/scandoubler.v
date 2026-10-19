//
// scandoubler.v
// 
// Copyright (c) 2015 Till Harbaum <till@harbaum.org> 
// Copyright (c) 2017-2021 Alexey Melnikov
// 
// This source file is free software: you can redistribute it and/or modify 
// it under the terms of the GNU General Public License as published 
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

// TODO: Delay vsync one line

module scandoubler #(
    parameter LENGTH     = 768,
    parameter HALF_DEPTH = 0,
    localparam DWIDTH = HALF_DEPTH ? 3 : 7
)
(
	// system interface
	input             clk_vid,
	input             hq2x,

	// shifter video interface
	input             ce_pix,
	input             hs_in,
	input             vs_in,
	input             hb_in,
	input             vb_in,
	input  [DWIDTH:0] r_in,
	input  [DWIDTH:0] g_in,
	input  [DWIDTH:0] b_in,

	// output interface
	output            ce_pix_out,
	output reg        hs_out,
	output            vs_out,
	output            hb_out,
	output            vb_out,
	output [DWIDTH:0] r_out,
	output [DWIDTH:0] g_out,
	output [DWIDTH:0] b_out
);

reg  [7:0] pix_len = 0;
wire [7:0] pl = pix_len + 1'b1;

reg  [7:0] pix_in_cnt = 0;
wire [7:0] pc_in = pix_in_cnt + 1'b1;
reg  [7:0] pixsz, pixsz2, pixsz4 = 0;

reg ce_x4i, ce_x1i;
always @(posedge clk_vid) begin
	reg old_ce, valid, hs;

	if(~&pix_len) pix_len <= pl;
	if(~&pix_in_cnt) pix_in_cnt <= pc_in;

	ce_x4i <= 0;
	ce_x1i <= 0;

	// use such odd comparison to place ce_x4 evenly if master clock isn't multiple of 4.
	if((pc_in == pixsz4) || (pc_in == pixsz2) || (pc_in == (pixsz2+pixsz4))) ce_x4i <= 1;

	old_ce <= ce_pix;
	if(~old_ce & ce_pix) begin
		if(valid & ~hb_in & ~vb_in) begin
			pixsz  <= pl;
			pixsz2 <= {1'b0,  pl[7:1]};
			pixsz4 <= {2'b00, pl[7:2]};
		end
		pix_len <= 0;
		valid <= 1;
	end

	hs <= hs_in;
	if((~hs & hs_in) || (pc_in >= pixsz)) begin
		ce_x4i <= 1;
		ce_x1i <= 1;
		pix_in_cnt <= 0;
	end

	if(hb_in | vb_in) valid <= 0;
end

reg req_line_reset;
reg [DWIDTH:0] r_d, g_d, b_d;
always @(posedge clk_vid) begin
	if(ce_x1i) begin
		req_line_reset <= hb_in;
		r_d <= r_in;
		g_d <= g_in;
		b_d <= b_in;
	end
end

reg ce_x4o, ce_x2o;
reg [1:0] sd_line;
wire [DWIDTH*3+2:0] hq_pixel;
Hq2x #(.LENGTH(LENGTH), .HALF_DEPTH(HALF_DEPTH)) Hq2x
(
	.clk(clk_vid),

	.ce_in(ce_x4i),
	.inputpixel({b_d,g_d,r_d}),
	.disable_hq2x(~hq2x),
	.reset_frame(vb_in),
	.reset_line(req_line_reset),

	.ce_out(ce_x4o),
	.read_y(sd_line),
	.hblank(hbo[0]&hbo[8]),
	.outpixel(hq_pixel)
);

reg  [7:0] pix_out_cnt = 0;
wire [7:0] pc_out = pix_out_cnt + 1'b1;

always @(posedge clk_vid) begin
	reg hs;

	if(~&pix_out_cnt) pix_out_cnt <= pc_out;

	ce_x4o <= 0;
	ce_x2o <= 0;

	// use such odd comparison to place ce_x4 evenly if master clock isn't multiple of 4.
	if((pc_out == pixsz4) || (pc_out == pixsz2) || (pc_out == (pixsz2+pixsz4))) ce_x4o <= 1;
	if( pc_out == pixsz2) ce_x2o <= 1;

	hs <= hs_out;
	if((~hs & hs_out) || (pc_out >= pixsz)) begin
		ce_x2o <= 1;
		ce_x4o <= 1;
		pix_out_cnt <= 0;
	end
end

reg [3:0] vbo;
reg [3:0] vso;
reg [8:0] hbo;
always @(posedge clk_vid) begin

	reg [31:0] hcnt;
	reg [30:0] sd_hcnt;
	reg [30:0] hs_start, hs_end;
	reg [30:0] hde_start, hde_end;

	reg hs, hb;

	if(ce_x4o) begin
		hbo[8:1] <= hbo[7:0];
	end

	// output counter synchronous to input and at twice the rate
	sd_hcnt <= sd_hcnt + 1'd1;
	if(sd_hcnt == hde_start) begin
		sd_hcnt <= 0;
		vbo[3:1] <= vbo[2:0];
	end

	if(sd_hcnt == hs_end) begin
		sd_line <= sd_line + 1'd1;
		if(&vbo[3:2]) sd_line <= 1;
		vso[3:1] <= vso[2:0];
	end

	if(sd_hcnt == hde_start)hbo[0] <= 0;
	if(sd_hcnt == hde_end)  hbo[0] <= 1;

	// replicate horizontal sync at twice the speed
	if(sd_hcnt == hs_end)   hs_out <= 0;
	if(sd_hcnt == hs_start) hs_out <= 1;

	hs <= hs_in;
	hb <= hb_in;

	hcnt <= hcnt + 1'd1;
	if(hb && !hb_in) begin
		hde_start <= hcnt[31:1];
		hbo[0] <= 0;
		hcnt <= 0;
		sd_hcnt <= 0;
		vbo <= {vbo[2:0],vb_in};
	end

	if(!hb && hb_in) hde_end <= hcnt[31:1];

	// falling edge of hsync indicates start of line
	if(hs && !hs_in) begin
		hs_end <= hcnt[31:1];
		vso[0] <= vs_in;
	end

	// save position of rising edge
	if(!hs && hs_in) hs_start <= hcnt[31:1];
end

// MEGA65: plain line doubler for hq2x = 0. Hq2x needs four clocks per input pixel even
// with the filter off; the C128's VDC delivers one every two. Each input line goes into
// one of two buffers and is read back for both output lines of the next input line.
localparam LB_AW = $clog2(LENGTH);

reg  [DWIDTH*3+2:0] lb_ram[0:(2<<LB_AW)-1];
reg  [DWIDTH*3+2:0] lb_q;
reg     [LB_AW-1:0] lb_wx, lb_rx;
reg                 lb_hb, lb_wbuf = 0, lb_rbuf = 0;
reg           [2:0] lb_hbd;
wire                lb_line_start = lb_hb & ~hb_in;

always @(posedge clk_vid) begin
	lb_hb <= hb_in;
	if(lb_line_start) begin
		lb_wbuf <= ~lb_wbuf;
		lb_rbuf <= lb_wbuf;
	end

	if(hb_in) lb_wx <= 0;
	else if(ce_x1i && ~&lb_wx) lb_wx <= lb_wx + 1'd1;
end

// The first pixel can arrive on the clock the buffers swap.
always @(posedge clk_vid) begin
	if(ce_x1i && ~hb_in) lb_ram[{lb_wbuf ^ lb_line_start, lb_wx}] <= {b_in, g_in, r_in};
end

// Three output pixels behind hbo[0], the position of hbo[6] at four clocks per pixel.
always @(posedge clk_vid) begin
	if(ce_x2o) begin
		lb_hbd <= {lb_hbd[1:0], hbo[0]};
		if(lb_hbd[1]) lb_rx <= 0;
		else if(~&lb_rx) lb_rx <= lb_rx + 1'd1;
		lb_q <= lb_ram[{lb_rbuf, lb_rx}];
	end
end

// One output line earlier than Hq2x, which waits for the next input line; sync moves too.
assign vs_out = hq2x ? vso[3] : vso[2];
assign ce_pix_out = hq2x ? ce_x4o : ce_x2o;

//Compensate picture shift after HQ2x
assign vb_out = hq2x ? vbo[3] : vbo[2];
assign hb_out = hq2x ? hbo[6] : lb_hbd[2];
assign {b_out, g_out, r_out} = hq2x ? hq_pixel : lb_q;

endmodule
