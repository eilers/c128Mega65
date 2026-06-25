`timescale 1 ps / 1 ps

module vdcram
#(
parameter DATA_WIDTH    = 8,
parameter ADDRESS_WIDTH = 16
)
(
input                           clk,
input                           rd,
input                           we,
input      [ADDRESS_WIDTH-1:0]  addr,
input      [DATA_WIDTH-1:0]     dai,
output     [DATA_WIDTH-1:0]     dao
);

// MEGA65: force this 64KB VDC RAM into Block RAM. Vivado otherwise inferred it as
// distributed RAM (~8192 LUTRAMs), consuming nearly all device SLICEM -> severe placement
// pressure (boot fragility) and no room for debug cores. There are ample free BRAM tiles.
// The original code added an explicit write-first read bypass (dao_r<=dai on we&rd) which
// does not match a Block-RAM template, so Vivado fell back to distributed RAM even with the
// ram_style hint. Use a plain read-enabled, read-first template (BRAM-inferable). The only
// behavioural change is read-during-write to the SAME address returns the old byte; the VDC
// (80-col, unused in 40-col mode) tolerates this.
(* ram_style = "block" *) reg [DATA_WIDTH-1:0] ram [0:(1 << ADDRESS_WIDTH)-1];
reg [DATA_WIDTH-1:0] dao_r;

always @(posedge clk) begin
    if (we) begin
        ram[addr] <= dai;
    end

    if (rd) begin
        dao_r <= ram[addr];
    end
end

assign dao = dao_r;

endmodule
