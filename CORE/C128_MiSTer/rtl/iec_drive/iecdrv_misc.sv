/*
   Adjusted for the MEGA65 port (Vivado instead of Quartus):

   1. iecdrv_mem lost its "ram_init_file" attribute. It is Quartus-only and every
      call site that passed an INITFILE is commented out upstream.

   2. iecdrv_trackmem was an altsyncram instance, which Vivado cannot synthesize.
      It is now inferred block RAM using the same shape as iecdrv_mem above.
      Both take address, data and wren on one edge, the way altsyncram registers
      them: every caller strobes wren for a single cycle, so a pipeline stage that
      does not carry the data along stores the wrong byte. See CORE/sim/tb_iecdrv_mem.vhd.

   3. iecdrv_sync needs a set_false_path entry in CORE.xdc for every instantiation.
*/

module iecdrv_sync #(parameter WIDTH = 1) 
(
	input                  clk,
	input      [WIDTH-1:0] in,
	output reg [WIDTH-1:0] out
);

reg [WIDTH-1:0] s1,s2;
always @(posedge clk) begin
	s1 <= in;
	s2 <= s1;
	if(s1 == s2) out <= s2;
end

endmodule

module iecdrv_reset_filter #(parameter WIDTH = 1) 
(
	input              clk,
	input  [WIDTH-1:0] reset,
	input  [WIDTH-1:0] in,
	output             out
);

reg [WIDTH-1:0] active;

assign out = &{in | reset | ~active};

generate
	genvar i;
	for(i=0; i<WIDTH; i=i+1) begin :reset_filter_active
		always @(posedge clk)
			if(reset[i] || !active[i])
				active[i] <= in[i];
	end
endgenerate

endmodule

// -------------------------------------------------------------------------------

module iecdrv_mem #(parameter DATAWIDTH, ADDRWIDTH, INITFILE=" ")
(
	input	                     clock_a,
	input	     [ADDRWIDTH-1:0] address_a,
	input	     [DATAWIDTH-1:0] data_a,
	input	                     wren_a,
	output reg [DATAWIDTH-1:0] q_a,

	input	                     clock_b,
	input	     [ADDRWIDTH-1:0] address_b,
	input	     [DATAWIDTH-1:0] data_b,
	input	                     wren_b,
	output reg [DATAWIDTH-1:0] q_b
);

// Every caller drives wren_a from a single-cycle strobe (ena_r or ph2_f) while the CPU
// holds address and data for the whole ph2 cycle, so address, data and wren have to be
// consumed on the same edge. Registering the address ahead of the data writes the byte
// that follows the strobe, which corrupts the drive's work RAM: the DOS then still runs
// from ROM and answers ATN, but has no status message, no directory and no buffers.
reg [DATAWIDTH-1:0] ram[1<<ADDRWIDTH];

always @(posedge clock_a) begin
	if(wren_a) begin
		ram[address_a] <= data_a;
		q_a <= data_a;
	end else begin
		q_a <= ram[address_a];
	end
end

always @(posedge clock_b) begin
	if(wren_b) begin
		ram[address_b] <= data_b;
		q_b <= data_b;
	end else begin
		q_b <= ram[address_b];
	end
end

endmodule

module iecdrv_trackmem #(parameter ADDRWIDTH, parameter WORDS, parameter DATAWIDTH=8)
(
	input	                     clock_a,
	input	     [ADDRWIDTH-1:0] address_a,
	input	     [DATAWIDTH-1:0] data_a,
	input	                     wren_a,
	output reg [DATAWIDTH-1:0] q_a,

	input	                     clock_b,
	input	     [ADDRWIDTH-1:0] address_b,
	input	     [DATAWIDTH-1:0] data_b,
	input	                     wren_b,
	output reg [DATAWIDTH-1:0] q_b
);

// The array covers the whole address space rather than just WORDS entries, because
// callers read past the end of the track and mask the result off afterwards. On a
// Xilinx BRAM both depths cost the same number of tiles.
//
// Same single-edge timing as iecdrv_mem above: the head side writes one byte per
// bit-cell with a single-cycle buff_we, so its data must not lag its address either.
reg [DATAWIDTH-1:0] ram[1<<ADDRWIDTH];

always @(posedge clock_a) begin
	if(wren_a) begin
		ram[address_a] <= data_a;
		q_a <= data_a;
	end else begin
		q_a <= ram[address_a];
	end
end

always @(posedge clock_b) begin
	if(wren_b) begin
		ram[address_b] <= data_b;
		q_b <= data_b;
	end else begin
		q_b <= ram[address_b];
	end
end

endmodule
