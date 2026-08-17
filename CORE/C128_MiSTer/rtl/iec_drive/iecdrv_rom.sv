module iecdrv_rom
(
   input             clk_sys,
   input             clk,
   input             reset,
   input             rom_loading,

   output reg        empty8k,
   output reg        rom_valid = 0,

   input       [3:0] rom_bank,
   input      [14:0] mem_a,
   output      [7:0] rom_do,

   output            rom_req,
	output reg [14:0] rom_addr,
	input             rom_wr,
	input       [7:0] rom_data
);

assign rom_req = ~(reset | rom_loading | rom_valid);

// MEGA65 port: rom_bank_n was declared inside the always block below. Vivado synthesis
// gives such a declaration static lifetime and infers a register (rom_bank_n_reg), but
// xsim re-initialises it on every invocation, so the "rom_bank != rom_bank_n" compare
// below never settles and rom_addr is held at 0 forever for any bank other than 0.
// Hoisting it to module scope makes both agree and costs nothing in hardware.
reg [3:0] rom_bank_n = 0;

always @(posedge clk_sys) begin
   if (rom_loading)
      rom_valid <= 0;

   if (reset) begin
      rom_addr <= 0;
   end
   else if (rom_bank != rom_bank_n) begin
      rom_valid  <= 0;
      rom_addr   <= 0;
      rom_bank_n <= rom_bank;
   end
   else if (rom_req && rom_wr) begin
      if (&rom_addr)
         rom_valid <= 1;
      rom_addr <= rom_addr + 1'd1;
   end
end

always @(posedge clk_sys) begin
   if (rom_wr && !rom_addr) 
      empty8k <= 1;

   if (rom_wr && |rom_data && ~&rom_data && rom_addr[14:8] && !rom_addr[14:13]) 
      empty8k <= 0;
end

// MEGA65 port: the Quartus altsyncram instance is replaced by MiSTer2MEGA65's
// dualport_2clk_ram, which Vivado infers as a true dual port block RAM.
// altsyncram registered both its inputs and, via outdata_reg, its outputs, so the
// extra register stage on each port here preserves the original two-cycle latency.

reg        rom_wr_d;
reg [14:0] rom_addr_d;
reg  [7:0] rom_data_d;
always @(posedge clk_sys) begin
   rom_wr_d   <= rom_wr & rom_req;
   rom_addr_d <= rom_addr;
   rom_data_d <= rom_data;
end

reg [14:0] mem_a_d;
always @(posedge clk) mem_a_d <= mem_a;

dualport_2clk_ram #(
   .ADDR_WIDTH(15),
   .DATA_WIDTH(8)
) rom (
   .clock_a(clk_sys),
   .address_a(rom_addr_d),
   .do_latch_addr_a(1'b0),
   .data_a(rom_data_d),
   .wren_a(rom_wr_d),
   .q_a(),

   .clock_b(clk),
   .address_b(mem_a_d),
   .do_latch_addr_b(1'b0),
   .data_b(8'h00),
   .wren_b(1'b0),
   .q_b(rom_do)
);

endmodule
