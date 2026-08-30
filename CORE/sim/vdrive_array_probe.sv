// Mixed-language array-direction probe used by tb_vdrive_index.
module vdrive_array_probe #(parameter DRIVES=2)
(
	input  logic [31:0] from_vhdl[DRIVES],
	output logic [31:0] to_vhdl[DRIVES]
);

always_comb begin
	for (int i = 0; i < DRIVES; i = i + 1)
		to_vhdl[i] = from_vhdl[i] ^ (32'h1111_0000 + i);
end

endmodule
