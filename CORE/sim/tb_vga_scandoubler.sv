`timescale 1ns/1ps

// The M2M analog VGA path (MiSTer video_mixer as analog_pipeline.vhd uses it) at the core's
// two dot rates on a ~32 MHz clock: VIC-II one dot per 4 clocks, VDC one per 2. Each dot
// encodes its position, so every output line must show dots 0..H_ACTIVE-1 of one input line,
// and the scandoubler must show each input line twice. Interlaced cases alternate fields of
// V_TOTAL and V_TOTAL+1 lines; as in video_sync_c128, one field's vsync starts with hsync
// and the other's half a line away.
// Beta 2's VDC enable is a negative control.
// State lives at module scope: xsim re-initialises variables declared in always blocks.

module vga_sd_case #(
   parameter string NAME        = "",
   parameter int    P           = 4,    // video clocks per dot
   parameter int    CE_SQUARE   = 0,    // 1 = Beta 2 VDC enable: high 2 clocks out of 4
   parameter int    SCANDOUBLER = 1,
   parameter int    DATA_PHASE  = 0,    // clock within the dot where data/sync/blank change
   parameter int    CE_PHASE    = 0,    // clock within the dot where the enable pulses
   parameter int    H_ACTIVE    = 403,
   parameter int    H_TOTAL     = 504,
   parameter int    HS_START    = 420,
   parameter int    HS_LEN      = 37,
   parameter int    V_ACTIVE    = 12,
   parameter int    V_TOTAL     = 20,
   parameter int    VS_START    = 15,
   parameter int    VS_LEN      = 2,
   parameter int    VS_DOT      = 0,    // dot where vsync starts
   parameter int    INTERLACE   = 0,
   parameter int    QUIET       = 0     // negative control: count errors, do not print them
) (
   input  logic clk,
   output int   errors,
   output int   frames_checked,
   output int   first_px,       // output pixels from the HSync rising edge to the first DE pixel
   output int   first_line      // output HSyncs from the VSync rising edge to the first DE line
);

   // ------------------------------------------------------------------ source
   logic [1:0] c4 = 0;
   int         dot = 0;
   int         line = 0;
   logic [7:0] r = 0, g = 0, b = 0;
   logic       hs = 0, vs = 0, hb = 1, vb = 1;
   logic       ce;

   // Field 1 follows field 0 within the same line count; pixels carry the field in bit 7 of B.
   localparam int FRAME_LINES = INTERLACE ? 2 * V_TOTAL + 1 : V_TOTAL;
   wire       fld  = (INTERLACE != 0) && (line >= V_TOTAL);
   wire [9:0] lrel = fld ? line - V_TOTAL : line;
   wire [19:0] pos  = lrel * H_TOTAL + dot;
   wire [19:0] vs_a = VS_START * H_TOTAL + VS_DOT + (fld ? H_TOTAL / 2 : 0);
   wire        vs_x = (pos >= vs_a) && (pos < vs_a + VS_LEN * H_TOTAL);

   always @(posedge clk) begin
      c4 <= c4 + 2'd1;
      if (int'(c4) % P == DATA_PHASE) begin
         r  <= dot[7:0];
         g  <= {lrel[5:0], dot[9:8]};
         b  <= {fld, lrel[6:0]};
         hb <= (dot >= H_ACTIVE);
         vb <= (lrel >= V_ACTIVE);
         hs <= (dot >= HS_START) && (dot < HS_START + HS_LEN);
         vs <= vs_x;
         if (dot == H_TOTAL - 1) begin
            dot  <= 0;
            line <= (line == FRAME_LINES - 1) ? 0 : line + 1;
         end
         else
            dot <= dot + 1;
      end
   end

   assign ce = CE_SQUARE ? c4[1] : (int'(c4) % P == CE_PHASE);

   // --------------------------------------------------------------------- DUT
   logic       ce_pixel;
   logic [7:0] vga_r, vga_g, vga_b;
   logic       vga_hs, vga_vs, vga_de;
   wire [21:0] gamma_bus;

   video_mixer dut (
      .CLK_VIDEO   (clk),
      .CE_PIXEL    (ce_pixel),
      .ce_pix      (ce),
      .scandoubler (SCANDOUBLER != 0),
      .hq2x        (1'b0),
      .gamma_bus   (gamma_bus),
      .R           (r),
      .G           (g),
      .B           (b),
      .HSync       (hs),
      .VSync       (vs),
      .HBlank      (hb),
      .VBlank      (vb),
      .HDMI_FREEZE (1'b0),
      .freeze_sync (),
      .VGA_R       (vga_r),
      .VGA_G       (vga_g),
      .VGA_B       (vga_b),
      .VGA_VS      (vga_vs),
      .VGA_HS      (vga_hs),
      .VGA_DE      (vga_de)
   );

   // ----------------------------------------------------------------- checker
   // Outputs change on CE_PIXEL; sampling there sees each pixel once (one pixel late).
   localparam int LINES_PER_IN = SCANDOUBLER ? 2 : 1;

   logic de_q = 0, hs_q = 0, vs_q = 0;
   int   frame = 0;
   int   out_line = 0;
   int   run_px = 0;
   int   run_line = 0;
   int   px_since_hs = 0;
   int   hs_since_vs = 0;
   int   x, l;
   bit   f, frame_fld, prev_fld;
   bit   have_prev = 0;
   bit   checking;

   initial begin
      errors = 0;
      frames_checked = 0;
      first_px = -1;
      first_line = -1;
   end

   // Skip the frames in which the scandoubler measures line and pixel length.
   assign checking = (frame >= 3);

   task automatic fail(input string msg);
      if (!QUIET && errors < 5)
         $display("  %s: %s (frame %0d, output line %0d)", NAME, msg, frame, out_line);
      errors = errors + 1;
   endtask

   always @(posedge clk) begin
      if (ce_pixel) begin
         x = {vga_g[1:0], vga_r};
         l = vga_b[6:0];
         f = vga_b[7];

         px_since_hs = (vga_hs && !hs_q) ? 0 : px_since_hs + 1;

         if (vga_vs && !vs_q) begin
            if (checking) begin
               if (out_line != LINES_PER_IN * V_ACTIVE)
                  fail($sformatf("frame had %0d visible lines, expected %0d",
                                 out_line, LINES_PER_IN * V_ACTIVE));
               frames_checked = frames_checked + 1;
            end
            frame = frame + 1;
            out_line = 0;
            hs_since_vs = 0;
         end
         else if (vga_hs && !hs_q)
            hs_since_vs = hs_since_vs + 1;

         if (vga_de) begin
            if (!de_q) begin
               run_px = 0;
               run_line = out_line / LINES_PER_IN;
               if (out_line == 0) begin
                  if (checking && INTERLACE && have_prev && f == prev_fld)
                     fail("field did not alternate");
                  frame_fld = f;
                  prev_fld  = f;
                  have_prev = 1;
               end
               if (checking) begin
                  if (out_line == 0) first_line = hs_since_vs;
                  first_px = px_since_hs;
               end
            end
            if (checking && f != frame_fld)
               fail("line from the other field");
            if (checking && $isunknown({vga_r, vga_g, vga_b}))
               fail($sformatf("pixel %0d is unknown (X)", run_px));
            else if (checking && (x != run_px || l != run_line))
               fail($sformatf("pixel %0d shows dot %0d of line %0d, expected dot %0d of line %0d",
                              run_px, x, l, run_px, run_line));
            run_px = run_px + 1;
         end
         else if (de_q) begin
            if (checking && run_px != H_ACTIVE)
               fail($sformatf("line showed %0d dots, expected %0d", run_px, H_ACTIVE));
            out_line = out_line + 1;
         end

         de_q = vga_de;
         hs_q = vga_hs;
         vs_q = vga_vs;
      end
   end

endmodule


module tb_vga_scandoubler;

   logic clk = 0;
   always #15.625 clk = ~clk;   // 32 MHz

   // PAL line geometry; only a few lines per frame to keep the run short.
   localparam int VIC_HA = 403, VIC_HT = 504, VIC_HS = 420, VIC_HL = 37;
   localparam int VDC_HA = 760, VDC_HT = 1024, VDC_HS = 800, VDC_HL = 72;

   localparam int N = 9;
   int    errors[N], frames[N], fpx[N], fline[N];
   string names[N];
   bit    must_fail[N];

   // 40 columns: data changes the clock after enablePixel, as vic_r_reg does.
   vga_sd_case #(.NAME("VIC  Standard"), .P(4), .SCANDOUBLER(1), .CE_PHASE(1), .DATA_PHASE(2),
                 .H_ACTIVE(VIC_HA), .H_TOTAL(VIC_HT), .HS_START(VIC_HS), .HS_LEN(VIC_HL))
      c0 (clk, errors[0], frames[0], fpx[0], fline[0]);
   vga_sd_case #(.NAME("VIC  15 kHz"), .P(4), .SCANDOUBLER(0), .CE_PHASE(1), .DATA_PHASE(2),
                 .H_ACTIVE(VIC_HA), .H_TOTAL(VIC_HT), .HS_START(VIC_HS), .HS_LEN(VIC_HL))
      c1 (clk, errors[1], frames[1], fpx[1], fline[1]);

   // 80 columns: the VDC's dot phase is unrelated to the enable, so test both.
   vga_sd_case #(.NAME("VDC  Standard, phase 0"), .P(2), .SCANDOUBLER(1), .CE_PHASE(0), .DATA_PHASE(0),
                 .H_ACTIVE(VDC_HA), .H_TOTAL(VDC_HT), .HS_START(VDC_HS), .HS_LEN(VDC_HL))
      c2 (clk, errors[2], frames[2], fpx[2], fline[2]);
   vga_sd_case #(.NAME("VDC  Standard, phase 1"), .P(2), .SCANDOUBLER(1), .CE_PHASE(0), .DATA_PHASE(1),
                 .H_ACTIVE(VDC_HA), .H_TOTAL(VDC_HT), .HS_START(VDC_HS), .HS_LEN(VDC_HL))
      c3 (clk, errors[3], frames[3], fpx[3], fline[3]);
   vga_sd_case #(.NAME("VDC  15 kHz, phase 0"), .P(2), .SCANDOUBLER(0), .CE_PHASE(0), .DATA_PHASE(0),
                 .H_ACTIVE(VDC_HA), .H_TOTAL(VDC_HT), .HS_START(VDC_HS), .HS_LEN(VDC_HL))
      c4 (clk, errors[4], frames[4], fpx[4], fline[4]);
   vga_sd_case #(.NAME("VDC  15 kHz, phase 1"), .P(2), .SCANDOUBLER(0), .CE_PHASE(0), .DATA_PHASE(1),
                 .H_ACTIVE(VDC_HA), .H_TOTAL(VDC_HT), .HS_START(VDC_HS), .HS_LEN(VDC_HL))
      c5 (clk, errors[5], frames[5], fpx[5], fline[5]);

   // VDC interlace (R8 = 1 or 3).
   vga_sd_case #(.NAME("VDC  Standard, interlaced"), .P(2), .SCANDOUBLER(1), .INTERLACE(1), .VS_DOT(VDC_HS),
                 .H_ACTIVE(VDC_HA), .H_TOTAL(VDC_HT), .HS_START(VDC_HS), .HS_LEN(VDC_HL))
      c6 (clk, errors[6], frames[6], fpx[6], fline[6]);
   vga_sd_case #(.NAME("VDC  15 kHz, interlaced"), .P(2), .SCANDOUBLER(0), .INTERLACE(1), .VS_DOT(VDC_HS),
                 .H_ACTIVE(VDC_HA), .H_TOTAL(VDC_HT), .HS_START(VDC_HS), .HS_LEN(VDC_HL))
      c7 (clk, errors[7], frames[7], fpx[7], fline[7]);

   // Negative control: Beta 2's 2-on/2-off VDC enable.
   vga_sd_case #(.NAME("VDC  15 kHz, Beta 2 enable"), .P(2), .CE_SQUARE(1), .SCANDOUBLER(0), .QUIET(1),
                 .H_ACTIVE(VDC_HA), .H_TOTAL(VDC_HT), .HS_START(VDC_HS), .HS_LEN(VDC_HL))
      c8 (clk, errors[8], frames[8], fpx[8], fline[8]);

   initial begin
      bit ok;

      names = '{"VIC  Standard", "VIC  15 kHz", "VDC  Standard, phase 0", "VDC  Standard, phase 1",
                "VDC  15 kHz, phase 0", "VDC  15 kHz, phase 1", "VDC  Standard, interlaced",
                "VDC  15 kHz, interlaced", "VDC  15 kHz, Beta 2 enable"};
      must_fail = '{0, 0, 0, 0, 0, 0, 0, 0, 1};

      // 11 frames of the longer (VDC) line: 3 to settle, then at least 5 checked.
      repeat (11 * 20 * VDC_HT * 2) @(posedge clk);

      ok = 1;
      for (int i = 0; i < N; i++) begin
         if (must_fail[i]) begin
            if (errors[i] == 0) begin
               $display("FAIL: %s was expected to lose dots but the checker saw none", names[i]);
               ok = 0;
            end
            else
               $display("PASS: %s loses dots as expected (%0d errors)", names[i], errors[i]);
         end
         else if (errors[i] != 0 || frames[i] < 5) begin
            $display("FAIL: %s: %0d errors in %0d checked frames", names[i], errors[i], frames[i]);
            ok = 0;
         end
         else
            $display("PASS: %s: %0d frames, first pixel %0d after HSync, first line %0d after VSync",
                     names[i], frames[i], fpx[i], fline[i]);
      end

      if (ok) $display("PASS: tb_vga_scandoubler");
      else    $display("FAIL: tb_vga_scandoubler");
      $finish;
   end

endmodule
