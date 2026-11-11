//
// Commodore 1541 combinational GCR nibble codec.
//
// This module deliberately contains no drive state or bit-serial logic. It is a
// reusable, synthesis-safe primitive for converting one binary nibble to its
// five-bit on-disk representation and validating the reverse conversion.
//
// Licensed under GPL v3.
//

module c1541_gcr_codec (
   input  logic [3:0] encode_nibble,
   output logic [4:0] encoded_code,
   input  logic [4:0] decode_code,
   output logic [3:0] decoded_nibble,
   output logic       decode_valid
);

   always_comb begin
      case (encode_nibble)
         4'h0: encoded_code = 5'h0A;
         4'h1: encoded_code = 5'h0B;
         4'h2: encoded_code = 5'h12;
         4'h3: encoded_code = 5'h13;
         4'h4: encoded_code = 5'h0E;
         4'h5: encoded_code = 5'h0F;
         4'h6: encoded_code = 5'h16;
         4'h7: encoded_code = 5'h17;
         4'h8: encoded_code = 5'h09;
         4'h9: encoded_code = 5'h19;
         4'hA: encoded_code = 5'h1A;
         4'hB: encoded_code = 5'h1B;
         4'hC: encoded_code = 5'h0D;
         4'hD: encoded_code = 5'h1D;
         4'hE: encoded_code = 5'h1E;
         4'hF: encoded_code = 5'h15;
         default: encoded_code = 5'h00;
      endcase
   end

   always_comb begin
      decoded_nibble = 4'h0;
      decode_valid   = 1'b1;
      case (decode_code)
         5'h0A: decoded_nibble = 4'h0;
         5'h0B: decoded_nibble = 4'h1;
         5'h12: decoded_nibble = 4'h2;
         5'h13: decoded_nibble = 4'h3;
         5'h0E: decoded_nibble = 4'h4;
         5'h0F: decoded_nibble = 4'h5;
         5'h16: decoded_nibble = 4'h6;
         5'h17: decoded_nibble = 4'h7;
         5'h09: decoded_nibble = 4'h8;
         5'h19: decoded_nibble = 4'h9;
         5'h1A: decoded_nibble = 4'hA;
         5'h1B: decoded_nibble = 4'hB;
         5'h0D: decoded_nibble = 4'hC;
         5'h1D: decoded_nibble = 4'hD;
         5'h1E: decoded_nibble = 4'hE;
         5'h15: decoded_nibble = 4'hF;
         default: decode_valid = 1'b0;
      endcase
   end

endmodule
