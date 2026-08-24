//
// Commodore 128 for MEGA65 (C128MEGA65)
//
// Testbench for the 1581 "drive ready" path.
//
// On hardware a mounted .D81 identifies correctly as a 1581 ("73,COPYRIGHT CBM V10
// 1581,00,00"), but DIRECTORY returns nothing and DS$ then reports "74, DRIVE NOT
// READY" with no drive activity. floppy_ready is the signal the DOS polls through
// CIA port A, and it is
//
//    assign floppy_ready = fd_ready && fd_present;
//
// fd_present is latched from |img_size on the rising edge of img_mounted, and
// fd_ready comes from the floppy model, which only reports ready once the spin-up
// ramp has counted rate up to RATEDD. That ramp is clocked by clk8m_en, so it
// depends on the clock-enable chain this port generates rather than on MiSTer's.
//
// This testbench reproduces that chain exactly (the 16 MHz DDA from main.vhd feeding
// the divider from c1581_multi.sv), mounts a D81-sized image, turns the motor on and
// measures how long floppy_ready takes to assert. It then issues a READ SECTOR and
// checks that sd_rd is raised with a sensible LBA.
//
// MEGA65 port done by Stefan Eilers in 2026 and licensed under GPL v3
//

`timescale 1ns / 1ps

module tb_c1581_ready;

   // The core clock: CORE_CLK_SPEED_PAL = 31,527,778 Hz, i.e. 31.7180 ns.
   localparam real CLK_PERIOD_NS = 1000.0 / 31.527778;

   // A D81 is 80 tracks * 2 sides * 10 sectors * 512 bytes.
   localparam int  D81_SIZE      = 819200;

   localparam int  RATEDD      = 250000;

   // CLK_EN tells the FDC how many kHz clk8m_en runs at, and every modelled floppy
   // time (spin-up, one revolution, step rate) is derived from it. Hardware uses
   // 8000; simulating a real 0.47 s spin-up plus a 200 ms revolution of a bit-level
   // floppy model takes over half an hour in xsim. Telling the model it is clocked
   // ten times slower than it really is compresses every one of those times by 10
   // without touching a single line of logic, so the same states are exercised in a
   // tenth of the simulated time.
   localparam int  CLK_EN_SIM  = 800;
   localparam int  TIME_SCALE  = 8000 / CLK_EN_SIM;

   // Spin-up needs RATEDD increments of "rate"; with the compressed timebase that is
   // one increment per clk8m_en, i.e. about 31 ms.
   localparam time READY_TIMEOUT = 120ms;
   localparam time RAMP_WINDOW   = 2ms;

   reg clk = 1'b0;
   always #(CLK_PERIOD_NS/2.0) clk = ~clk;

   // ------------------------------------------------------------------------
   // Clock enable chain, identical to the hardware
   // ------------------------------------------------------------------------

   // main.vhd: drift-compensated 16 MHz chip enable out of the 31.527778 MHz core clock
   integer dce_sum = 0;
   reg     ce      = 1'b0;
   always @(posedge clk) begin
      integer nextsum;
      nextsum = dce_sum + 16000000;
      ce <= 1'b0;
      if (nextsum >= 31527778) begin
         dce_sum <= nextsum - 31527778;
         ce      <= 1'b1;
      end else begin
         dce_sum <= nextsum;
      end
   end

   // c1581_multi.sv: ph2 at 2 MHz and wd_ce at 8 MHz out of that 16 MHz ce
   reg       ph2_r = 1'b0;
   reg       ph2_f = 1'b0;
   reg       wd_ce = 1'b0;
   reg [2:0] div   = 3'd0;
   reg       ena   = 1'b0;
   reg       ena1  = 1'b0;
   always @(posedge clk) begin
      ena1 <= 1'b1;                 // ~pause, and pause is tied low here
      if (div[1:0]) ena <= ena1;

      ph2_r <= 1'b0;
      ph2_f <= 1'b0;
      wd_ce <= 1'b0;
      if (ce) begin
         div   <= div + 1'd1;
         ph2_r <= ena && !div[2] && !div[1:0];
         ph2_f <= ena &&  div[2] && !div[1:0];
         wd_ce <= ena && !div[0];
      end
   end

   // Measure the actual ce / wd_ce rates over the first millisecond, so that a wrong
   // divider shows up as a number rather than as a mysterious timeout.
   integer ce_count = 0;
   integer wd_count = 0;
   always @(posedge clk) begin
      if (ce)    ce_count <= ce_count + 1;
      if (wd_ce) wd_count <= wd_count + 1;
   end

   // ------------------------------------------------------------------------
   // Device under test
   // ------------------------------------------------------------------------

   reg         floppy_reset = 1'b0;   // active low, driven from ~reset in c1581_drv
   reg         floppy_motor = 1'b0;
   reg         floppy_side  = 1'b1;  // FDC: high = side 0 (c1581_drv drives ~cia_pa[0])
   wire        floppy_step;
   wire        floppy_ready;

   wire        irq, drq;

   reg  [1:0]  cpu_addr = 2'd0;
   reg         cpu_sel  = 1'b0;
   reg         cpu_rw   = 1'b1;
   reg  [7:0]  cpu_din  = 8'd0;
   wire [7:0]  cpu_dout;

   reg         img_mounted = 1'b0;
   reg  [31:0] img_size    = 32'd0;

   wire [31:0] sd_lba;
   wire        sd_rd, sd_wr;
   reg         sd_ack         = 1'b0;
   reg  [8:0]  sd_buff_addr   = 9'd0;
   reg  [7:0]  sd_dout        = 8'd0;
   reg         sd_dout_strobe = 1'b0;
   wire [7:0]  sd_din;
   wire [7:0]  out_track;

   // QNICE / vdrives domain (50 MHz). The FDC SD FSM lives here after the
   // C64-style clk_sys latch, so the responder and sd_rd share a clock.
   localparam real CLK_SD_PERIOD_NS = 1000.0 / 50.0;
   reg clk_sd = 1'b0;
   always #(CLK_SD_PERIOD_NS/2.0) clk_sd = ~clk_sd;

   c1581_fdc1772 #(.IMG_TYPE(1), .EXT_MOTOR(1), .FD_NUM(1), .CLK_EN(CLK_EN_SIM)) dut (
      .clkcpu        (clk),
      .clk_sys       (clk_sd),
      .clk8m_en      (wd_ce),

      .floppy_drive  (1'b0),
      .floppy_side   (floppy_side),
      .floppy_reset  (floppy_reset),
      .floppy_step   (floppy_step),
      .floppy_motor  (floppy_motor),
      .floppy_ready  (floppy_ready),

      .irq           (irq),
      .drq           (drq),

      .cpu_addr      (cpu_addr),
      .cpu_sel       (cpu_sel),
      .cpu_rw        (cpu_rw),
      .cpu_din       (cpu_din),
      .cpu_dout      (cpu_dout),

      .img_mounted   (img_mounted),
      .img_wp        (1'b0),
      .img_ds        (1'b0),
      .img_size      (img_size),

      .sd_lba        (sd_lba),
      .sd_rd         (sd_rd),
      .sd_wr         (sd_wr),
      .sd_ack        (sd_ack),
      .sd_buff_addr  (sd_buff_addr),
      .sd_dout       (sd_dout),
      .sd_din        (sd_din),
      .sd_dout_strobe(sd_dout_strobe),
      .out_track     (out_track),
      .out_we        ()
   );

   // ------------------------------------------------------------------------
   // Minimal SD responder, modelled on what vdrives + the QNICE Shell do
   // ------------------------------------------------------------------------

   // Each of QNICE's register stores is a VD_CAD_WRITE subroutine call, so it costs far
   // more than one cycle. The exact figure does not matter, it only has to be long
   // enough that a store is never a single-cycle pulse.
   localparam int SD_STORE = 20;

   // The expected sector content, so that a byte which never landed is visible as a
   // wrong byte rather than as a plausible one.
   function [7:0] expected(input integer idx);
      expected = idx[7:0] ^ 8'h5A;
   endfunction

   integer  sd_reads  = 0;
   integer  sd_blocks = 0;
   integer  i;
   initial begin
      forever begin
         wait (sd_rd === 1'b1);
         sd_reads = sd_reads + 1;

         // HANDLE_DRV_RD acknowledges first, then walks the block storing address, data
         // and write-enable through separate CPU writes. Note what it does NOT do: the
         // "XOR 0, R9" meant to clear the write enable again is a no-op, so the strobe
         // goes high on the first byte and then stays high for the whole block. Model
         // that faithfully, because it is what the hardware sees.
         repeat (SD_STORE) @(posedge clk_sd);
         sd_ack = 1'b1;
         for (i = 0; i < 512; i = i + 1) begin
            repeat (SD_STORE) @(posedge clk_sd);
            sd_buff_addr   = i[8:0];
            repeat (SD_STORE) @(posedge clk_sd);
            sd_dout        = expected(i);
            repeat (SD_STORE) @(posedge clk_sd);
            sd_dout_strobe = 1'b1;
         end
         repeat (SD_STORE) @(posedge clk_sd);
         sd_ack    = 1'b0;
         sd_blocks = sd_blocks + 1;
         wait (sd_rd === 1'b0);
      end
   end

   // ------------------------------------------------------------------------
   // WD1772 register access through the CPU port
   // ------------------------------------------------------------------------

   task wd_write(input [1:0] addr, input [7:0] data);
      begin
         @(posedge ph2_f);
         cpu_addr <= addr;
         cpu_din  <= data;
         cpu_rw   <= 1'b0;
         cpu_sel  <= 1'b1;
         @(posedge clk);
         @(posedge clk);
         cpu_sel  <= 1'b0;
         cpu_rw   <= 1'b1;
      end
   endtask

   task wd_read(input [1:0] addr, output [7:0] data);
      begin
         @(posedge ph2_f);
         cpu_addr <= addr;
         cpu_rw   <= 1'b1;
         cpu_sel  <= 1'b1;
         @(posedge clk);
         data = cpu_dout;
         @(posedge clk);
         cpu_sel  <= 1'b0;
      end
   endtask

   // ------------------------------------------------------------------------
   // CPU side of a sector read: drain the data register on every DRQ, which is
   // what the drive's 6502 does in its read loop.
   // ------------------------------------------------------------------------

   reg        draining = 1'b0;
   reg  [7:0] got [0:1023];
   integer    got_n = 0;

   initial begin
      reg [7:0] d;
      forever begin
         wait (draining && drq === 1'b1);
         wd_read(2'd3, d);
         if (got_n < 1024) got[got_n] = d;
         got_n = got_n + 1;
         // Reading the data register is what clears drq, but only a cycle later, so
         // without this the same byte would be picked up twice.
         wait (drq === 1'b0);
      end
   end

   // ------------------------------------------------------------------------
   // Stimulus
   // ------------------------------------------------------------------------

   // Continuous monitors: the sector header is only under the head for a few bytes, so
   // sampling it from the stimulus block would almost always miss it.
   integer hdr_matches = 0;
   integer xfer_starts = 0;
   integer hdr_seen [0:31];      // headers that passed the head, per sector number
   integer hdrs     = 0;
   reg     hdr_d    = 1'b0;
   integer j;
   initial for (j = 0; j < 32; j = j + 1) hdr_seen[j] = 0;
   always @(posedge clk) begin
      if (dut.fd_sector_hdr && dut.fd_sector == dut.sector) hdr_matches <= hdr_matches + 1;
      if (dut.data_transfer_start)                          xfer_starts <= xfer_starts + 1;
      hdr_d <= dut.fd_sector_hdr;
      if (dut.fd_sector_hdr && !hdr_d) begin
         hdr_seen[dut.fd_sector] = hdr_seen[dut.fd_sector] + 1;
         hdrs = hdrs + 1;
      end
   end

   // The SD state machine only returns to idle on the falling edge of sd_ack, and the
   // handover to the drive CPU is gated on it being idle, so log every transition.
   reg [1:0] sd_state_d = 2'd0;
   always @(posedge clk) begin
      if (dut.sd_state !== sd_state_d) begin
         $display("tb_c1581_ready: %0t sd_state %0d -> %0d (sd_rd=%b sd_ack=%b s_odd=%b size_code=%0d)",
                  $time, sd_state_d, dut.sd_state, sd_rd, sd_ack, dut.s_odd, dut.sector_size_code);
         sd_state_d <= dut.sd_state;
      end
   end

   time    t_motor_on;
   integer errors    = 0;
   integer rate_a;
   integer rate_b;
   integer bad;
   integer first_bad;

   initial begin
      $display("tb_c1581_ready: start");
      $fflush;

      // Hold the drive in reset for a moment, exactly like ~reset does on hardware.
      floppy_reset = 1'b0;
      repeat (100) @(posedge clk);
      floppy_reset = 1'b1;

      // Let the clock-enable chain settle and report the measured rates.
      #1ms;
      $display("tb_c1581_ready: ce pulses in 1 ms = %0d (expect ~16000)", ce_count);
      $display("tb_c1581_ready: wd_ce pulses in 1 ms = %0d (expect ~8000)", wd_count);
      if (ce_count < 15000 || ce_count > 17000) begin
         $display("tb_c1581_ready: MISMATCH ce rate is not 16 MHz");
         errors = errors + 1;
      end
      if (wd_count < 7500 || wd_count > 8500) begin
         $display("tb_c1581_ready: MISMATCH wd_ce rate is not 8 MHz");
         errors = errors + 1;
      end

      // Mount a D81: size first, then a single strobe, as VD_STROBE_IM does.
      img_size = D81_SIZE;
      repeat (10) @(posedge clk);
      img_mounted = 1'b1;
      repeat (4)  @(posedge clk);
      img_mounted = 1'b0;
      repeat (10) @(posedge clk);

      $display("tb_c1581_ready: mounted a %0d byte image, fd_present=%b floppy_ready=%b",
               D81_SIZE, dut.fd_present, floppy_ready);
      $fflush;

      // fd_present is latched from |img_size on the rising edge of img_mounted. If this
      // is 0 the DOS reports "74, DRIVE NOT READY" without ever spinning the motor.
      if (dut.fd_present !== 1'b1) begin
         $display("tb_c1581_ready: MISMATCH fd_present is not set after mounting");
         errors = errors + 1;
      end
      $display("tb_c1581_ready: geometry spt=%0d doubleside=%b hd=%b sector_len=%0d",
               dut.fd_spt, dut.fd_doubleside, dut.fdn_hd[0], dut.fdn_sector_len[0]);
      $fflush;

      // Turn the motor on, which is what the DOS does through CIA port A bit 2.
      t_motor_on   = $time;
      floppy_motor = 1'b1;

      // Confirm the spin-up ramp is actually climbing before waiting it out, so that a
      // dead clk8m_en is reported as such instead of as a bare timeout.
      repeat (200) @(posedge clk);
      $display("tb_c1581_ready: motor path: floppy_motor=%b fd_motor=%b fd_any=%b fdn=%0d select=%b motor_on=%b motor_on_sel=%b",
               floppy_motor, dut.fd_motor, dut.fd_any, dut.fdn,
               dut.fdd[0].floppy.select, dut.fdd[0].floppy.motor_on,
               dut.fdd[0].floppy.motor_on_sel);
      $display("tb_c1581_ready: spin_up_counter=%0d SPIN_UP_CLKS=%0d rate=%0d hd=%b fm=%b",
               dut.fdd[0].floppy.spin_up_counter, dut.fdd[0].floppy.SPIN_UP_CLKS,
               dut.fdd[0].floppy.rate, dut.fdd[0].floppy.hd, dut.fdd[0].floppy.fm);
      $fflush;

      #RAMP_WINDOW;
      rate_a = dut.fdd[0].floppy.rate;
      #RAMP_WINDOW;
      rate_b = dut.fdd[0].floppy.rate;
      $display("tb_c1581_ready: spin-up rate at %0t = %0d, at %0t = %0d (target %0d)",
               RAMP_WINDOW, rate_a, 2*RAMP_WINDOW, rate_b, RATEDD);
      $fflush;
      if (rate_b <= rate_a) begin
         $display("tb_c1581_ready: MISMATCH spin-up ramp is not advancing, the drive can never report ready");
         errors = errors + 1;
      end

      fork : wait_ready
         begin
            wait (floppy_ready === 1'b1);
            $display("tb_c1581_ready: floppy_ready asserted %0t after motor on (= %0t on hardware)",
                     $time - t_motor_on, ($time - t_motor_on) * TIME_SCALE);
            disable wait_ready;
         end
         begin
            #READY_TIMEOUT;
            $display("tb_c1581_ready: MISMATCH floppy_ready never asserted within %0t", READY_TIMEOUT);
            errors = errors + 1;
            disable wait_ready;
         end
      join
      $fflush;

      if (floppy_ready === 1'b1) begin
         // Ask for track 0, sector 1, i.e. the first sector of the image.
         wd_write(2'd1, 8'd0);      // track register
         wd_write(2'd2, 8'd1);      // sector register
         wd_write(2'd0, 8'h88);     // READ SECTOR
         draining = 1'b1;

         fork : wait_rd
            begin
               wait (sd_reads > 0);
               $display("tb_c1581_ready: sd_rd raised, sd_lba=%0d", sd_lba);
               disable wait_rd;
            end
            begin
               // A full revolution at 300 RPM is 200 ms, compressed to 20 ms here, so
               // the sector can take that long to come under the head.
               #40ms;
               $display("tb_c1581_ready: MISMATCH no sd_rd after READ SECTOR");
               // Which of the three steps stalled: did the command reach the FDC, did
               // the sector ever pass under the head, did the transfer start.
               $display("tb_c1581_ready: busy=%b cmd=%02h hdr_matches=%0d xfer_starts=%0d sd_state=%0d",
                        dut.busy, dut.cmd, hdr_matches, xfer_starts, dut.sd_state);
               errors = errors + 1;
               disable wait_rd;
            end
         join
      end

      // The sector has been requested; now follow it all the way to the CPU. This is the
      // part the drive LED reports as magenta on hardware.
      if (sd_reads > 0) begin
         wait (sd_blocks > 0);
         $display("tb_c1581_ready: block delivered, sd_buff_addr reached %0d, strobe still high = %b",
                  sd_buff_addr, sd_dout_strobe);

         // What actually landed in the sector buffer.
         bad = 0;
         first_bad = -1;
         for (i = 0; i < 512; i = i + 1) begin
            if (dut.fifo.ram[i] !== expected(i)) begin
               bad = bad + 1;
               if (first_bad < 0) first_bad = i;
            end
         end
         $display("tb_c1581_ready: sector buffer: %0d of 512 bytes wrong, first at %0d", bad, first_bad);
         if (bad != 0) begin
            $display("tb_c1581_ready: MISMATCH the block did not land correctly in the sector buffer");
            errors = errors + 1;
         end

         // And what the CPU got out of it.
         fork : wait_xfer
            begin
               wait (got_n >= 512);
               #100us;
               disable wait_xfer;
            end
            begin
               #40ms;
               $display("tb_c1581_ready: MISMATCH the CPU only received %0d of 512 bytes", got_n);
               errors = errors + 1;
               disable wait_xfer;
            end
         join

         bad = 0;
         first_bad = -1;
         for (i = 0; i < 512 && i < got_n; i = i + 1) begin
            if (got[i] !== expected(i)) begin
               bad = bad + 1;
               if (first_bad < 0) first_bad = i;
            end
         end
         $display("tb_c1581_ready: CPU read %0d bytes, %0d wrong, first at %0d, RNF=%b data_lost=%b",
                  got_n, bad, first_bad, dut.RNF, dut.data_lost);

         // The handover from the SD side to the drive CPU waits for the requested sector
         // header to pass under the head:
         //    if (fd_ready && fd_sector_hdr && fd_sector == sector) data_transfer_start <= 1
         // so report every term of it.
         $display("tb_c1581_ready: handover: busy=%b sd_state=%0d fifo_cpuptr=%0d cmd=%02h sd_reads=%0d sd_blocks=%0d sd_rd=%b",
                  dut.busy, dut.sd_state, dut.fifo_cpuptr, dut.cmd, sd_reads, sd_blocks, sd_rd);
         $display("tb_c1581_ready: handover: fd_ready=%b requested sector=%0d sector_base=%0d spt=%0d",
                  dut.fd_ready, dut.sector, dut.sector_base, dut.fd_spt);
         $display("tb_c1581_ready: %0d sector headers passed the head, %0d of them matched, %0d transfers started",
                  hdrs, hdr_matches, xfer_starts);
         for (j = 0; j < 32; j = j + 1)
            if (hdr_seen[j] != 0)
               $display("tb_c1581_ready:    sector %0d seen %0d times", j, hdr_seen[j]);
         if (bad != 0) begin
            $display("tb_c1581_ready: MISMATCH the CPU did not receive the sector contents");
            errors = errors + 1;
         end
         if (dut.RNF !== 1'b0) begin
            $display("tb_c1581_ready: MISMATCH the FDC reported RECORD NOT FOUND");
            errors = errors + 1;
         end

         // ----------------------------------------------------------------
         // Hypothesis H1: SEEK to D81 directory cylinder (track 39 / T40).
         // If data_in truncation (or any seek bug) remains, step_to stays 0/1
         // and out_track never reaches 39. Directory LBA must be 780.
         // ----------------------------------------------------------------
         begin : seek_dir
            integer seek_lba_before;
            integer dbg_fd;
            reg [7:0] st;

            seek_lba_before = sd_reads;
            draining = 1'b0;

            // #region agent log
            dbg_fd = $fopen("/home/bazzite/Dokumente/Developer/c128Mega65/.cursor/debug-36b09c.log", "a");
            if (dbg_fd) begin
               $fdisplay(dbg_fd, "{\"sessionId\":\"36b09c\",\"runId\":\"track40-sim\",\"hypothesisId\":\"H1\",\"location\":\"tb_c1581_ready.sv:seek\",\"message\":\"before SEEK to track 39\",\"data\":{\"out_track\":%0d,\"track_reg\":%0d,\"step_to\":%0d,\"RNF\":%0d},\"timestamp\":%0t}",
                         out_track, dut.track, dut.step_to, dut.RNF, $time);
               $fclose(dbg_fd);
            end
            // #endregion

            wd_write(2'd3, 8'd39);     // data register = seek target
            wd_write(2'd0, 8'h14);     // SEEK (type-1, verify off, 30ms step)

            fork : wait_seek
               begin
                  // busy rises then falls
                  wait (dut.busy === 1'b1);
                  wait (dut.busy === 1'b0);
                  disable wait_seek;
               end
               begin
                  #80ms;
                  $display("tb_c1581_ready: MISMATCH SEEK to track 39 did not complete");
                  errors = errors + 1;
                  disable wait_seek;
               end
            join

            $display("tb_c1581_ready: after SEEK out_track=%0d track_reg=%0d step_to=%0d fd_track=%0d",
                     out_track, dut.track, dut.step_to, dut.fd_track);

            // #region agent log
            dbg_fd = $fopen("/home/bazzite/Dokumente/Developer/c128Mega65/.cursor/debug-36b09c.log", "a");
            if (dbg_fd) begin
               $fdisplay(dbg_fd, "{\"sessionId\":\"36b09c\",\"runId\":\"track40-sim\",\"hypothesisId\":\"H1\",\"location\":\"tb_c1581_ready.sv:after_seek\",\"message\":\"after SEEK to track 39\",\"data\":{\"out_track\":%0d,\"track_reg\":%0d,\"step_to\":%0d,\"fd_track\":%0d,\"busy\":%0d},\"timestamp\":%0t}",
                         out_track, dut.track, dut.step_to, dut.fd_track, dut.busy, $time);
               $fclose(dbg_fd);
            end
            // #endregion

            if (dut.track !== 8'd39 || dut.step_to !== 8'd39) begin
               $display("tb_c1581_ready: MISMATCH SEEK target not latched (track=%0d step_to=%0d) — data_in/seek path broken",
                        dut.track, dut.step_to);
               errors = errors + 1;
            end
            if (out_track !== 8'd39 && dut.fd_track !== 7'd39) begin
               $display("tb_c1581_ready: MISMATCH head did not reach track 39 (out_track=%0d fd_track=%0d)",
                        out_track, dut.fd_track);
               errors = errors + 1;
            end

            // READ SECTOR 1 on track 39, side 0 → stock ST LBA
            //   ((10*39)<<1) + 0 + 1 - 1 = 780  (D81 T40 S0 header)
            draining = 1'b1;
            got_n = 0;
            wd_write(2'd1, 8'd39);     // track register
            wd_write(2'd2, 8'd1);      // sector 1 (1-based, as on MiSTer)
            wd_write(2'd0, 8'h88);     // READ SECTOR

            fork : wait_rd40
               begin
                  wait (sd_reads > seek_lba_before);
                  $display("tb_c1581_ready: dir-cylinder sd_rd, sd_lba=%0d (expect 780)", sd_lba);
                  disable wait_rd40;
               end
               begin
                  #60ms;
                  $display("tb_c1581_ready: MISMATCH no sd_rd for track-40 READ SECTOR");
                  errors = errors + 1;
                  disable wait_rd40;
               end
            join

            // #region agent log
            dbg_fd = $fopen("/home/bazzite/Dokumente/Developer/c128Mega65/.cursor/debug-36b09c.log", "a");
            if (dbg_fd) begin
               $fdisplay(dbg_fd, "{\"sessionId\":\"36b09c\",\"runId\":\"stock-st\",\"hypothesisId\":\"G0\",\"location\":\"tb_c1581_ready.sv:dir_lba\",\"message\":\"directory cylinder sector request\",\"data\":{\"sd_lba\":%0d,\"expect\":780,\"out_track\":%0d,\"RNF\":%0d,\"sd_reads\":%0d},\"timestamp\":%0t}",
                         sd_lba, out_track, dut.RNF, sd_reads, $time);
               $fclose(dbg_fd);
            end
            // #endregion

            if (sd_reads > seek_lba_before && sd_lba !== 32'd780) begin
               $display("tb_c1581_ready: MISMATCH directory LBA %0d != 780 — stock ST math (sector 1 → first 512B of T40 S0)", sd_lba);
               errors = errors + 1;
            end

            if (sd_reads > seek_lba_before) begin
               wait (sd_blocks > 1 || got_n >= 512);
               #200us;
               wd_read(2'd0, st);
               $display("tb_c1581_ready: dir read status=%02h RNF=%b got_n=%0d", st, dut.RNF, got_n);
               // #region agent log
               dbg_fd = $fopen("/home/bazzite/Dokumente/Developer/c128Mega65/.cursor/debug-36b09c.log", "a");
               if (dbg_fd) begin
                  $fdisplay(dbg_fd, "{\"sessionId\":\"36b09c\",\"runId\":\"track40-sim\",\"hypothesisId\":\"H2\",\"location\":\"tb_c1581_ready.sv:dir_status\",\"message\":\"directory read status\",\"data\":{\"status\":%0d,\"RNF\":%0d,\"got_n\":%0d,\"floppy_ready\":%0d},\"timestamp\":%0t}",
                            st, dut.RNF, got_n, floppy_ready, $time);
                  $fclose(dbg_fd);
               end
               // #endregion
               if (dut.RNF !== 1'b0) begin
                  $display("tb_c1581_ready: MISMATCH RNF on directory cylinder");
                  errors = errors + 1;
               end
            end
         end

         // Stock MiSTer: sector 0 is below sector_base=1 → RNF, no host request.
         begin : stock_sector0
            integer sd_reads_before0;
            integer dbg_fd0;
            draining = 1'b1;
            got_n = 0;
            #1ms;
            sd_reads_before0 = sd_reads;
            wd_write(2'd1, 8'd39);
            wd_write(2'd2, 8'd0);
            wd_write(2'd0, 8'h88);
            fork : wait_rd0
               begin
                  wait (sd_reads > sd_reads_before0);
                  disable wait_rd0;
               end
               begin
                  #60ms;
                  disable wait_rd0;
               end
            join
            $display("tb_c1581_ready: stock sector0 sd_rd_delta=%0d sd_lba=%0d RNF=%b snf=%b",
                     sd_reads - sd_reads_before0, sd_lba, dut.RNF, dut.sector_not_found);
            // #region agent log
            dbg_fd0 = $fopen("/home/bazzite/Dokumente/Developer/c128Mega65/.cursor/debug-36b09c.log", "a");
            if (dbg_fd0) begin
               $fdisplay(dbg_fd0, "{\"sessionId\":\"36b09c\",\"runId\":\"stock-st\",\"hypothesisId\":\"G0\",\"location\":\"tb_c1581_ready.sv:sector0\",\"message\":\"stock MiSTer sector 0 is RNF\",\"data\":{\"sd_rd_delta\":%0d,\"sd_lba\":%0d,\"RNF\":%0d,\"sector_not_found\":%0d,\"sd_reads\":%0d},\"timestamp\":%0t}",
                         sd_reads - sd_reads_before0, sd_lba, dut.RNF, dut.sector_not_found, sd_reads, $time);
               $fclose(dbg_fd0);
            end
            // #endregion
            if (sd_reads > sd_reads_before0) begin
               $display("tb_c1581_ready: MISMATCH stock sector 0 must not raise sd_rd");
               errors = errors + 1;
            end
         end
      end

      $display("tb_c1581_ready: simulation finished {\"pass\":%0s}", (errors == 0) ? "true" : "false");
      $fflush;
      $finish;
   end

endmodule
