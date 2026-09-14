//
// Independent unit tests for the Commodore 1541 GCR nibble codec.
//
// Licensed under GPL v3.
//

`timescale 1ns / 1ps

module tb_c1541_gcr_codec;

   logic [3:0] encode_nibble;
   logic [4:0] encoded_code;
   logic [4:0] decode_code;
   logic [3:0] decoded_nibble;
   logic       decode_valid;

   reg [7:0] nibble_codes [0:15];
   reg [7:0] header_raw   [0:7];
   reg [7:0] data_raw     [0:259];

   integer errors = 0;
   integer i;
   integer valid_count;
   reg [7:0] round_trip;
   reg [7:0] checksum;

   c1541_gcr_codec dut (
      .encode_nibble (encode_nibble),
      .encoded_code  (encoded_code),
      .decode_code   (decode_code),
      .decoded_nibble(decoded_nibble),
      .decode_valid  (decode_valid)
   );

   function automatic integer sectors_per_track(input integer track);
      integer physical_track;
      begin
         physical_track = (track > 35) ? track - 35 : track;
         if (physical_track <= 17)
            sectors_per_track = 21;
         else if (physical_track <= 24)
            sectors_per_track = 19;
         else if (physical_track <= 30)
            sectors_per_track = 18;
         else
            sectors_per_track = 17;
      end
   endfunction

   function automatic integer sectors_before_track(input integer track);
      integer item;
      begin
         sectors_before_track = 0;
         for (item = 1; item < track; item = item + 1)
            sectors_before_track = sectors_before_track + sectors_per_track(item);
      end
   endfunction

   task automatic mismatch(input string message);
      begin
         $display("tb_c1541_gcr_codec: MISMATCH %s", message);
         errors = errors + 1;
      end
   endtask

   initial begin
      $display("tb_c1541_gcr_codec: start");
      $readmemh("../../../../sim/fixtures/gcr_nibble_codes.hex", nibble_codes);
      $readmemh("../../../../sim/fixtures/gcr_header_raw.hex", header_raw);
      $readmemh("../../../../sim/fixtures/gcr_data_raw.hex", data_raw);

      // Verify all 16 canonical mappings against independently generated data.
      for (i = 0; i < 16; i = i + 1) begin
         encode_nibble = i[3:0];
         #1;
         if (encoded_code !== nibble_codes[i][4:0])
            mismatch($sformatf("encode %x produced %02x, expected %02x",
                               i[3:0], encoded_code, nibble_codes[i][4:0]));
         decode_code = nibble_codes[i][4:0];
         #1;
         if (!decode_valid || decoded_nibble !== i[3:0])
            mismatch($sformatf("decode %02x produced valid=%b nibble=%x",
                               nibble_codes[i][4:0], decode_valid, decoded_nibble));
      end

      // Exactly half of the 32 possible code words are legal.
      valid_count = 0;
      for (i = 0; i < 32; i = i + 1) begin
         decode_code = i[4:0];
         #1;
         if (decode_valid)
            valid_count = valid_count + 1;
         else if (decoded_nibble !== 4'h0)
            mismatch($sformatf("invalid code %02x did not return deterministic zero", i));
      end
      if (valid_count != 16)
         mismatch($sformatf("valid-code count is %0d, expected 16", valid_count));

      // Exhaustively round-trip both nibbles of every possible byte.
      for (i = 0; i < 256; i = i + 1) begin
         encode_nibble = i[7:4];
         #1;
         decode_code = encoded_code;
         #1;
         round_trip[7:4] = decoded_nibble;
         if (!decode_valid)
            mismatch($sformatf("high nibble of byte %02x encoded invalidly", i));

         encode_nibble = i[3:0];
         #1;
         decode_code = encoded_code;
         #1;
         round_trip[3:0] = decoded_nibble;
         if (!decode_valid || round_trip !== i[7:0])
            mismatch($sformatf("byte %02x round-tripped as %02x", i, round_trip));
      end

      // Fixture-level header and data XOR checksum checks.
      checksum = 8'h00;
      for (i = 2; i <= 5; i = i + 1)
         checksum = checksum ^ header_raw[i];
      if (header_raw[0] !== 8'h08 || header_raw[1] !== checksum)
         mismatch($sformatf("header marker/checksum is %02x/%02x, expected 08/%02x",
                            header_raw[0], header_raw[1], checksum));
      if (header_raw[2] !== 8'd0 || header_raw[3] !== 8'd18)
         mismatch("fixture header is not track 18 sector 0");

      checksum = 8'h00;
      for (i = 1; i <= 256; i = i + 1)
         checksum = checksum ^ data_raw[i];
      if (data_raw[0] !== 8'h07 || data_raw[257] !== checksum)
         mismatch($sformatf("data marker/checksum is %02x/%02x, expected 07/%02x",
                            data_raw[0], data_raw[257], checksum));
      if (data_raw[258] !== 8'h00 || data_raw[259] !== 8'h00)
         mismatch("data block padding is not zero");

      // Standard D64/D71 geometry and selected offsets.
      if (sectors_per_track(1) != 21 || sectors_per_track(17) != 21 ||
          sectors_per_track(18) != 19 || sectors_per_track(24) != 19 ||
          sectors_per_track(25) != 18 || sectors_per_track(30) != 18 ||
          sectors_per_track(31) != 17 || sectors_per_track(35) != 17)
         mismatch("D64 sectors-per-track zones are incorrect");
      if (sectors_before_track(36) != 683)
         mismatch($sformatf("D64 total is %0d sectors, expected 683",
                            sectors_before_track(36)));
      if (sectors_before_track(71) != 1366)
         mismatch($sformatf("D71 total is %0d sectors, expected 1366",
                            sectors_before_track(71)));
      if (sectors_before_track(18) != 357)
         mismatch($sformatf("D64 directory track starts at sector %0d, expected 357",
                            sectors_before_track(18)));
      if (sectors_before_track(36) * 256 != 174848 ||
          sectors_before_track(71) * 256 != 349696)
         mismatch("D64/D71 byte sizes are incorrect");

      $display("tb_c1541_gcr_codec: simulation finished {\"pass\":%0s}",
               (errors == 0) ? "true" : "false");
      $finish;
   end

endmodule
