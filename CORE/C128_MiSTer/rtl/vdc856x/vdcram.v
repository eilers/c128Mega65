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

reg [DATA_WIDTH-1:0] ram [0:(1 << ADDRESS_WIDTH)-1];
reg [DATA_WIDTH-1:0] dao_r;

always @(posedge clk) begin
    if (we) begin
        ram[addr] <= dai;
    end

    if (rd) begin
        if (we) begin
            dao_r <= dai;
        end else begin
            dao_r <= ram[addr];
        end
    end
end

assign dao = dao_r;

endmodule
