//
// fdc1772.v
//
// Copyright (c) 2015 Till Harbaum <till@harbaum.org>
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
//

// TODO: 
// - 30ms settle time after step before data can be read
// - implement sector size 0

module c1581_fdc1772 (
	input            clkcpu, // system cpu clock.
	input            clk_sys, // MEGA65: QNICE clock for the SD/vdrives interface
	input            clk8m_en,

	// external set signals
	input      [W:0] floppy_drive,
	input            floppy_side, 
	input            floppy_reset,
	output           floppy_step,
	input            floppy_motor,
	output           floppy_ready,

	// interrupts
	output reg       irq,
	output reg       drq, // data request

	input      [1:0] cpu_addr,
	input            cpu_sel,
	input            cpu_rw,
	input      [7:0] cpu_din,
	output reg [7:0] cpu_dout,

	// place any signals that need to be passed up to the top after here.
	input      [W:0] img_mounted, // signaling that new image has been mounted
	input      [W:0] img_wp,      // write protect
	input            img_ds,      // double-sided image (for BBC Micro only)
	input     [31:0] img_size,    // size of image in bytes
	output reg[31:0] sd_lba,
	output reg [W:0] sd_rd = 0,
	output reg [W:0] sd_wr = 0,
	input            sd_ack,
	input      [8:0] sd_buff_addr,
	input      [7:0] sd_dout,
	output     [7:0] sd_din,
	input            sd_dout_strobe,
	output     [7:0] out_track,
	output           out_we,

	// MEGA65 port: disk_present is also the CIA /RDY sense (a disk is inserted).
	output           disk_present,

	// ---------------------------------------------------------------------
	// MEGA65 internal floppy as a 1581, ported from C64MEGA65 (issues #90 and
	// #244). A toggle/level interface to physical_1581_controller (50 MHz)
	// and its read and write FIFOs. Everything below is gated on phys_mode;
	// with phys_mode=0 the image path above is unchanged.
	// ---------------------------------------------------------------------
	input            phys_mode,         // 1 = this drive is the internal floppy

	// to the controller (raw toggles/levels; the controller syncs them)
	output           phys_active,       // enable for the controller
	output           phys_cia_motor_on, // motor-on request (PA2 sense)
	output           phys_cia_side,     // side select (PA0 sense)
	output reg       phys_step_req_tgl, // Type-I one-step request (toggle)
	output reg       phys_step_outward, // 1 = step toward track 0
	output reg       phys_rd_req_tgl,   // op request (toggle), also writes
	output reg [2:0] phys_rd_op,        // RDOP_* / WROP_*
	output reg [7:0] phys_rd_track,     // WD track register at request time
	output reg       phys_rd_side,      // current head/side
	output reg [7:0] phys_rd_sector,    // WD sector register at request time
	output reg       phys_rd_cancel_tgl,// force interrupt / cancel (toggle)
	output reg [1:0] phys_rd_seq,       // op sequence tag, stable before rd_req_tgl

	// from the controller (synced here into clkcpu)
	input            phys_step_ack_tgl, // step complete (toggle)
	input            phys_rd_done_tgl,  // op complete (toggle)
	input      [1:0] phys_rd_done_seq,  // seq of the op being completed
	input      [4:0] phys_rd_result,    // RES_* code
	input            phys_rd_crc_err,   // data/id CRC error
	input            phys_rd_rnf,       // record not found / not ready
	input            phys_rd_deleted,   // deleted data mark seen
	input      [7:0] phys_rd_c,         // ID field C (Read Address -> sector reg)

	// read FIFO (read side is native clkcpu)
	output reg       phys_byte_rd_en,   // pop the FIFO head
	input      [7:0] phys_byte_data,    // FIFO head byte (first-word-fall-through)
	input            phys_byte_empty,

	// live drive state (levels, synced here)
	input            phys_media_ready,
	input            phys_index,
	input            phys_track0,
	input            phys_wprot,
	input            phys_change,
	input            phys_motor_on,
	input            phys_head_settled,

	// write path: Write Sector / Write Track ride on the request channel above
	// (phys_rd_op 100 / 101); the bytes go through the write FIFO, whose write
	// side is native clkcpu. feed/lost/busy are 50 MHz levels, synced here.
	output reg       phys_wr_deleted,   // with phys_rd_req_tgl: DAM F8 instead of FB
	output reg       phys_wr_precomp,   // with phys_rd_req_tgl: precomp requested (P = 0)
	output reg       phys_wr_byte_wr,   // one-clkcpu push into the write FIFO
	output reg [7:0] phys_wr_byte_data,
	input            phys_wr_fifo_full, // write FIFO full: DRQ is FIFO room
	input            phys_wr_feed,      // controller wants bytes (ID found / track armed)
	input            phys_wr_lost,      // a slot was served from an empty FIFO (sticky per op)
	input            phys_wr_busy,      // writer not idle (gate open or tail draining)

	// diagnostic event toggles for physical_1581_diag (never reset: each edge
	// is one event) and the bytes presented by the last read op
	output reg        phys_dbg_lost_tgl,      // a presentation overwrote an unconsumed byte
	output reg        phys_dbg_drain_tgl,     // a between-ops drain episode started
	output reg        phys_dbg_staledone_tgl, // a done was ignored (seq mismatch / no op pending)
	output reg        phys_dbg_busycmd_tgl,   // a command write while busy was ignored
	output reg        phys_dbg_fin_tgl,       // a physical read op finished
	output reg [10:0] phys_dbg_pres_cnt,      // bytes presented by that op
	output reg        phys_dbg_wr_fin_tgl,    // a physical write op finished
	output reg        phys_dbg_wr_wp_tgl,     // a write was refused by the write-protect check
	output reg        phys_dbg_wr_drop_tgl    // a data-register write during a write op was dropped
	);

parameter CLK_EN           = 16'd8000; // in kHz
parameter FD_NUM           = 2;    // number of supported floppies
parameter MODEL            = 2;    // 0 - wd1770, 1 - fd1771, 2 - wd1772, 3 = wd1773/fd1793
parameter EXT_MOTOR        = 1'b0; // != 0 if motor is controlled externally by floppy_motor
parameter INVERT_HEAD_RA   = 1'b0; // != 0 - invert head in READ_ADDRESS reply

localparam IMG_ARCHIE      = 0;
localparam IMG_ST          = 1;
localparam IMG_BBC         = 2; // SSD, DSD formats
localparam IMG_TI99        = 3; // V9T9 format

parameter  IMG_TYPE        = IMG_ARCHIE;

localparam W    = FD_NUM - 1;
localparam WIDX = $clog2(FD_NUM);

// -------------------------------------------------------------------------
// MEGA65 internal floppy (C64MEGA65 #90/#244): clock crossing and state
// -------------------------------------------------------------------------
wire       phys_media_ready_c, phys_index_c, phys_track0_c, phys_wprot_c;
wire       phys_change_c, phys_motor_on_c, phys_head_settled_c;
wire [4:0] phys_rd_result_c;
wire       phys_rd_crc_err_c, phys_rd_rnf_c, phys_rd_deleted_c;
wire [7:0] phys_rd_c_c;
wire [1:0] phys_rd_done_seq_c;
wire       phys_step_ack_c, phys_rd_done_c;
wire       phys_wr_feed_c, phys_wr_lost_c, phys_wr_busy_c;

iecdrv_sync      phys_mrdy_sync    (clkcpu, phys_media_ready,  phys_media_ready_c);
iecdrv_sync      phys_index_sync   (clkcpu, phys_index,        phys_index_c);
iecdrv_sync      phys_trk0_sync    (clkcpu, phys_track0,       phys_track0_c);
iecdrv_sync      phys_wp_sync      (clkcpu, phys_wprot,        phys_wprot_c);
iecdrv_sync      phys_chg_sync     (clkcpu, phys_change,       phys_change_c);
iecdrv_sync      phys_mot_sync     (clkcpu, phys_motor_on,     phys_motor_on_c);
iecdrv_sync      phys_hs_sync      (clkcpu, phys_head_settled, phys_head_settled_c);
iecdrv_sync #(5) phys_res_sync     (clkcpu, phys_rd_result,    phys_rd_result_c);
iecdrv_sync      phys_crc_sync     (clkcpu, phys_rd_crc_err,   phys_rd_crc_err_c);
iecdrv_sync      phys_rnf_sync     (clkcpu, phys_rd_rnf,       phys_rd_rnf_c);
iecdrv_sync      phys_del_sync     (clkcpu, phys_rd_deleted,   phys_rd_deleted_c);
iecdrv_sync #(8) phys_c_sync       (clkcpu, phys_rd_c,         phys_rd_c_c);
iecdrv_sync #(2) phys_dseq_sync    (clkcpu, phys_rd_done_seq,  phys_rd_done_seq_c);
iecdrv_sync      phys_stepack_sync (clkcpu, phys_step_ack_tgl, phys_step_ack_c);
iecdrv_sync      phys_rddone_sync  (clkcpu, phys_rd_done_tgl,  phys_rd_done_c);
iecdrv_sync      phys_wfeed_sync   (clkcpu, phys_wr_feed,      phys_wr_feed_c);
iecdrv_sync      phys_wlost_sync   (clkcpu, phys_wr_lost,      phys_wr_lost_c);
iecdrv_sync      phys_wbusy_sync   (clkcpu, phys_wr_busy,      phys_wr_busy_c);

// Act on the done toggle two clkcpu after it arrives. The controller sets the
// result flags in the same cycle it flips the toggle, and each crosses through
// its own synchronizer, which may resolve a cycle apart from the others.
reg phys_rd_done_c_d1 = 1'b0;
reg phys_rd_done_c_d2 = 1'b0;
always @(posedge clkcpu) begin
	phys_rd_done_c_d1 <= phys_rd_done_c;
	phys_rd_done_c_d2 <= phys_rd_done_c_d1;
end

// physical read/step engine state (clkcpu)
reg         phys_step_busy = 1'b0;     // waiting for a step ack
reg         phys_rd_pending = 1'b0;    // an op was requested and is not done yet
reg         phys_reading = 1'b0;       // presenting bytes from the read FIFO
reg         phys_done_latched = 1'b0;  // the controller reported done
reg         phys_verify = 1'b0;        // the op in flight is a Type-I verify
reg         phys_reissue = 1'b0;       // next sector of a multiple-sector read
reg         phys_rnf_l = 1'b0, phys_crc_l = 1'b0, phys_del_l = 1'b0;   // latched result flags
reg  [7:0]  phys_c_l = 8'd0;           // latched ID C (Read Address -> sector reg)
reg         phys_step_ack_serviced = 1'b0;
reg         phys_rd_done_serviced = 1'b0;
reg         phys_rd_start = 1'b0;      // new op: re-arm pace, clear counters
reg         phys_cmd_clear = 1'b0;     // a command was accepted: clear DRQ and LOST DATA
reg         phys_set_sector = 1'b0;    // Read Address: copy phys_c_l into the sector reg
reg  [7:0]  phys_step_tally = 8'd0;    // steps issued by the current command (RESTORE bound)

// Bytes are presented at disk pace, like the real WD1772 moves them from its
// shift register: one DD MFM byte-time is 32 us = 252 clk8m_en ticks.
localparam [7:0] PHYS_PACE_TICKS = 8'd252;
reg  [7:0]  phys_pace_cnt = 8'd0;   // ticks until the next presentation (0 = expired)
reg  [10:0] phys_pres_cnt = 11'd0;  // bytes presented this op (diagnostics only)
reg         phys_lost_l   = 1'b0;   // LOST DATA (status bit 2, Type II/III)
reg         phys_draining = 1'b0;   // drain episode tracker for phys_dbg_drain_tgl

// The real WD1772 stays busy for about a millisecond even when a Type-I command
// needs no steps, and the 1581 ROM relies on that: after writing a command it
// waits for BUSY to rise ($CBF4). A sub-microsecond pulse hangs the DOS. Every
// Type-I command and every write termination stays busy at least 1.5 ms.
localparam [13:0] PHYS_T1_MIN_TICKS = 14'd12000;
reg  [13:0] phys_t1_min_cnt = 14'd0;

// Physical Write Sector / Write Track. The op is issued on the request channel
// (phys_rd_op 100 / 101) with the same seq tag, pending latch and done handling
// as a read. Every data-register write while the op is active goes into the
// write FIFO, and DRQ means FIFO room: it rises once the controller raises
// feed (ID found / track op armed), so the CPU runs at most three bytes ahead
// of the disk. The op completes on its done once the writer is idle. A
// write-protected disk is refused here before any request.
localparam [4:0] PHYS_RES_WRITE_PROTECTED = 5'b01001;  // physical_1581_pkg RES_WRITE_PROTECTED
localparam [4:0] PHYS_RES_LOST_DATA       = 5'b01011;  // physical_1581_pkg RES_LOST_DATA
localparam [4:0] PHYS_RES_OK              = 5'b00000;  // physical_1581_pkg RES_OK
reg         phys_wr_op       = 1'b0; // the op in flight is a write
reg         phys_wr_active   = 1'b0; // a write op takes bytes (until its done / abort)
reg         phys_wr_active_d = 1'b0;
reg         phys_wr_track_op = 1'b0; // Write Track (no 512-byte limit)
reg         phys_wr_wpterm   = 1'b0; // write-protect refusal in progress
reg         phys_wr_wp_l     = 1'b0; // status bit 6 of a write command
reg         phys_wr_lost_l   = 1'b0; // status bit 2 of a write command
reg         phys_wr_clean_l  = 1'b0; // done was RES_OK without lost (m = 1 may continue)
reg         phys_wr_reissue  = 1'b0; // next sector of a multiple-sector write
reg  [10:0] phys_wr_cnt      = 11'd0;// bytes pushed by the current op
reg         phys_wr_fed      = 1'b0; // feed was seen high during this op

// The request toggles are never reset (that would be a phantom edge for the
// controller), so give them a defined power-up value.
initial begin
	phys_step_req_tgl      = 1'b0;
	phys_rd_req_tgl        = 1'b0;
	phys_rd_cancel_tgl     = 1'b0;
	phys_rd_seq            = 2'd0;
	phys_step_outward      = 1'b0;
	phys_rd_op             = 3'd0;
	phys_rd_track          = 8'd0;
	phys_rd_side           = 1'b0;
	phys_rd_sector         = 8'd0;
	phys_byte_rd_en        = 1'b0;
	phys_dbg_lost_tgl      = 1'b0;
	phys_dbg_drain_tgl     = 1'b0;
	phys_dbg_staledone_tgl = 1'b0;
	phys_dbg_busycmd_tgl   = 1'b0;
	phys_dbg_fin_tgl       = 1'b0;
	phys_dbg_pres_cnt      = 11'd0;
	phys_wr_deleted        = 1'b0;
	phys_wr_precomp        = 1'b0;
	phys_wr_byte_wr        = 1'b0;
	phys_wr_byte_data      = 8'd0;
	phys_dbg_wr_fin_tgl    = 1'b0;
	phys_dbg_wr_wp_tgl     = 1'b0;
	phys_dbg_wr_drop_tgl   = 1'b0;
end

// EXT_MOTOR=1 for the 1581, so floppy_motor is the CIA's motor line
assign phys_active       = phys_mode & floppy_reset;
assign phys_cia_motor_on = floppy_motor;
assign phys_cia_side     = floppy_side;

// -------------------------------------------------------------------------
// --------------------- IO controller image handling ----------------------
// -------------------------------------------------------------------------

reg  [10:0] fdn_sector_len[FD_NUM];
reg   [4:0] fdn_spt[FD_NUM];     // sectors/track
reg   [9:0] fdn_gap_len[FD_NUM]; // gap len/sector
reg         fdn_doubleside[FD_NUM];
reg         fdn_hd[FD_NUM];
reg         fdn_fm[FD_NUM];
reg         fdn_present[FD_NUM];

reg  [11:0] image_sectors = 0;
reg  [11:0] image_sps; // sectors/side
reg   [4:0] image_spt; // sectors/track
reg   [9:0] image_gap_len = 0;
reg         image_doubleside = 0;
wire        image_hd = img_size[20];
reg         image_fm = 0;

reg   [1:0] sector_size_code; // sec size 0=128, 1=256, 2=512, 3=1024
reg  [10:0] sector_size = 0;
reg         sector_base; // number of first sector on track (archie 0, dos 1)

// MEGA65 (D81 enable): combinational LBA from clkcpu-domain track/sector;
// the clk_sys SD FSM latches it onto sd_lba at the start of each transfer.
reg [31:0] sd_lba_comb;
reg        s_odd = 0; // odd half-sector (IMG_ARCHIE)

always @(*) begin
	case (IMG_TYPE)
	IMG_ARCHIE: begin
		// archie, 1024 bytes/sector
		sector_size_code = 2'd3;
		sector_base = 0;
		sd_lba_comb = {(16'd0 + (fd_spt*track[6:0]) << fd_doubleside) + (floppy_side ? 5'd0 : fd_spt) + sector[4:0], s_odd };

		image_fm = 0;
		image_sectors = img_size[21:10];
		image_doubleside = 1'b1;
		image_spt = image_hd ? 5'd10 : 5'd5;
		image_gap_len = 10'd220;

	end
	IMG_ST: begin
		// this block is valid for the .st format (or similar arrangement), 512 bytes/sector
		sector_size_code = 2'd2;
		sector_base = 1;
		sd_lba_comb = ((fd_spt*track[6:0]) << fd_doubleside) + (floppy_side ? 5'd0 : fd_spt) + sector[4:0] - 1'd1;

		image_fm = 0;
		image_sectors = img_size[20:9];
		image_doubleside = 1'b0;
		image_sps = image_sectors;
		if (image_sectors > (85*12)) begin
			image_doubleside = 1'b1;
			image_sps = image_sectors >> 1'b1;
		end
		if (image_hd) image_sps = image_sps >> 1'b1;

		// spt : 9-12, tracks: 79-85
		case (image_sps)
			711,720,729,738,747,756,765   : image_spt = 5'd9;
			790,800,810,820,830,840,850   : image_spt = 5'd10;
			948,960,972,984,996,1008,1020 : image_spt = 5'd12;
			default : image_spt = 5'd11;
		endcase;

		if (image_hd) image_spt = image_spt << 1'b1;

		// SECTOR_GAP_LEN = BPT/SPT - (SECTOR_LEN + SECTOR_HDR_LEN) = 6250/SPT - (512+6)
		case (image_spt)
			5'd9, 5'd18: image_gap_len = 10'd176;
			5'd10,5'd20: image_gap_len = 10'd107;
			5'd11,5'd22: image_gap_len = 10'd50;
			default : image_gap_len = 10'd2;
		endcase;
	end
	IMG_BBC, IMG_TI99: begin
		// 256 bytes/sector single density (BBC SSD/DSD, TI99/4A)
		sector_size_code = 2'd1;
		sector_base = 0;
		if (IMG_TYPE == IMG_BBC) begin
			sd_lba_comb = (((fd_spt*track[6:0]) << fd_doubleside) + (floppy_side ? 5'd0 : fd_spt) + sector[4:0]) >> 1;
			image_spt = 10;
		end else begin
			sd_lba_comb = (fd_spt*(floppy_side ? track[5:0] : 79-track[5:0]) + sector[4:0]) >> 1;
			image_spt = 9;
		end

		image_fm = 1;
		image_sectors = img_size[19:8];
		image_doubleside = img_ds;
		if (img_ds)
			image_sps = image_sectors >> 1'b1;
		else
			image_sps = image_sectors;
		image_gap_len = 10'd50;
	end
	default: begin
		sector_size_code = 2'd0;
		sector_base = 0;
		sd_lba_comb = 0;
		image_fm = 0;
		image_sectors = 0;
		image_doubleside = 0;
		image_spt = 0;
		image_gap_len = 0;
	end

	endcase

	sector_size = 11'd128 << sector_size_code;
end

reg [W:0] img_mountedD = 0;   // MEGA65 port: see the note at sd_ackD

always @(posedge clkcpu) begin
	integer i;
	img_mountedD <= img_mounted;
	
	for(i = 0; i < FD_NUM; i = i+1'd1) begin
		if (~img_mountedD[i] && img_mounted[i]) begin
			fdn_present[i] <= |img_size;
			fdn_sector_len[i] <= sector_size;
			fdn_spt[i] <= image_spt;
			fdn_gap_len[i] <= image_gap_len;
			fdn_doubleside[i] <= image_doubleside;
			fdn_hd[i] <= image_hd;
			fdn_fm[i] <= image_fm;
		end
	end
end

// -------------------------------------------------------------------------
// ---------------------------- IRQ/DRQ handling ---------------------------
// -------------------------------------------------------------------------
reg cpu_selD = 0;
reg cpu_rwD = 0;
always @(posedge clkcpu) begin
	cpu_rwD <= cpu_sel & ~cpu_rw;
	cpu_selD <= cpu_sel;
end

wire cpu_we = cpu_sel & ~cpu_rw & ~cpu_rwD;

reg irq_set = 0;

// floppy_reset and read of status register/write of command register clears irq
reg cpu_rw_cmdstatus = 0;
always @(posedge clkcpu)
  cpu_rw_cmdstatus <= ~cpu_selD && cpu_sel && cpu_addr == FDC_REG_CMDSTATUS;

wire irq_clr = !floppy_reset || cpu_rw_cmdstatus;

always @(posedge clkcpu) begin
	if(irq_clr) irq <= 1'b0;
	else if(irq_set) irq <= 1'b1;
end

reg drq_set = 0;

reg cpu_rw_data = 0;
always @(posedge clkcpu)
	cpu_rw_data <= ~cpu_selD && cpu_sel && cpu_addr == FDC_REG_DATA;

// MEGA65 internal floppy: bytes are accepted while a write op is active, for
// Write Sector up to the 512th, and once feed has been up only while it still
// is (bytes the DOS writes after a Write Track ended are dropped, like the real
// chip does). DRQ also needs FIFO room, no push in flight and no open
// data-register access: the T65 holds the select for the whole bus cycle and
// strobes the write at its end.
wire phys_wr_accept = phys_wr_active && (phys_wr_track_op || phys_wr_cnt < sector_size)
                      && !(phys_wr_fed && !phys_wr_feed_c);
wire phys_wr_drq_ok = phys_mode && phys_wr_accept && phys_wr_feed_c && !phys_wr_fifo_full
                      && !phys_wr_byte_wr && !(cpu_sel && cpu_addr == FDC_REG_DATA)
                      && !cpu_rw_data;

// Never present a byte into an open CPU read of the data register: the CPU
// latches cpu_dout at the end of its bus cycle, so a byte landing inside it
// would be read twice, or lose its DRQ, or count as LOST DATA.
wire phys_cpu_rd_data_open = cpu_sel && cpu_rw && (cpu_addr == FDC_REG_DATA);

// Bytes are presented as they come off the disk, as the real WD1772 does, so
// busy drops right after the sector and the DOS catches the next one in the
// same revolution. (C64MEGA65 holds each sector until its CRC is known, which
// costs a revolution per sector.) An error done drops phys_reading and the
// drain below discards what is left; the status then shows the CRC error and
// the DOS retries, as on a real 1581.
wire phys_present_now = phys_mode && phys_reading
                        && (phys_pace_cnt == 8'd0) && !phys_byte_empty
                        && !phys_cpu_rd_data_open && !cpu_rw_data;

// MEGA65 internal floppy: accepting a command clears DRQ (real-WD1772
// semantics), and during a write op DRQ follows phys_wr_drq_ok one clkcpu late.
wire drq_clr = !floppy_reset || cpu_rw_data || phys_cmd_clear
            || (phys_mode && (phys_wr_active || phys_wr_active_d) && !phys_wr_drq_ok);

always @(posedge clkcpu) begin
	if(drq_clr) drq <= 1'b0;
	else if(drq_set) drq <= 1'b1;
end

// MEGA65 internal floppy: every data-register write while a write op accepts
// bytes is one push into the write FIFO. A write while the FIFO is full, after
// the 512th byte or after feed fell is dropped and counted.
always @(posedge clkcpu) begin : label_wr
	phys_wr_byte_wr  <= 1'b0;
	phys_wr_active_d <= phys_wr_active;
	if (!floppy_reset || !phys_wr_active)
		phys_wr_fed <= 1'b0;
	else if (phys_wr_feed_c)
		phys_wr_fed <= 1'b1;
	if (!floppy_reset || !phys_wr_active)
		phys_wr_cnt <= 11'd0;
	else if (phys_mode && cpu_we && cpu_addr == FDC_REG_DATA) begin
		if (phys_wr_accept && !phys_wr_fifo_full) begin
			phys_wr_byte_wr   <= 1'b1;
			phys_wr_byte_data <= cpu_din;
			if (phys_wr_cnt != 11'h7FF)
				phys_wr_cnt <= phys_wr_cnt + 11'd1;
		end else
			phys_dbg_wr_drop_tgl <= ~phys_dbg_wr_drop_tgl;
	end
end

// -------------------------------------------------------------------------
// -------------------- virtual floppy drive mechanics ---------------------
// -------------------------------------------------------------------------

wire       fdn_index[FD_NUM];
wire       fdn_ready[FD_NUM];
wire [6:0] fdn_track[FD_NUM];
wire [4:0] fdn_sector[FD_NUM];
wire       fdn_sector_hdr[FD_NUM];
wire       fdn_sector_data[FD_NUM];
wire       fdn_dclk[FD_NUM];
wire       fdn_spinning[FD_NUM];

// MEGA65 port: fdn, fd_any, fd_motor and the two step strobes are declared further down
// in the upstream source, i.e. below the generate block that wires them into the floppy
// instances. Quartus resolves those references against module scope, but Verilog says an
// undeclared identifier used in a port connection creates an implicit net in the
// enclosing scope, and for a port connection inside a generate block that scope is the
// generate block. On Vivado each floppy therefore got its own private, undriven copy of
// select and motor_on: the disk never span up, ready never went high and the drive
// answered every command with "74, DRIVE NOT READY". Declaring them ahead of the
// generate block binds the ports to the real signals.
reg [WIDX:0] fdn;
always @* begin
	integer i;

	fdn = 0;
	for(i = FD_NUM-1; i >= 0; i = i - 1) if(!floppy_drive[i]) fdn = i[WIDX:0];
end

wire       fd_any = ~&floppy_drive;

reg        step_in = 0, step_out = 0;
reg        motor_on /* verilator public */ = 1'b0;
wire       fd_motor = EXT_MOTOR ? floppy_motor : motor_on;

generate
	genvar i;
	
	for(i=0; i < FD_NUM; i = i+1) begin :fdd

		floppy #(.CLK_EN(CLK_EN)) floppy
		(
			.clk         ( clkcpu             ),
			.clk8m_en    ( clk8m_en           ),

			// control signals into floppy
			.select      ( fd_any && fdn == i ),
			.motor_on    ( fd_motor           ),
			.step_in     ( step_in            ),
			.step_out    ( step_out           ),

			// physical parameters
			.sector_len  ( fdn_sector_len[i]  ),
			.spt         ( fdn_spt[i]         ),
			.sector_gap_len ( fdn_gap_len[i]  ),
			.sector_base ( sector_base        ),
			.hd          ( fdn_hd[i]          ),
			.fm          ( fdn_fm[i]          ),

			// status signals generated by floppy
			.dclk_en     ( fdn_dclk[i]        ),
			.track       ( fdn_track[i]       ),
			.sector      ( fdn_sector[i]      ),
			.sector_hdr  ( fdn_sector_hdr[i]  ),
			.sector_data ( fdn_sector_data[i] ),
			.ready       ( fdn_ready[i]       ),
			.index       ( fdn_index[i]       ),
			.spinning    ( fdn_spinning[i]    )
		);
	end
endgenerate

// -------------------------------------------------------------------------
// ----------------------------- floppy demux ------------------------------
// -------------------------------------------------------------------------

wire       fd_index       = fd_any ? fdn_index[fdn]       : 1'b0;
wire       fd_ready       = fd_any ? fdn_ready[fdn]       : 1'b0;
wire [6:0] fd_track       = fd_any ? fdn_track[fdn]       : 7'd0;
wire [4:0] fd_sector      = fd_any ? fdn_sector[fdn]      : 5'd0;
wire       fd_sector_hdr  = fd_any ? fdn_sector_hdr[fdn]  : 1'b0;
//wire     fd_sector_data = fd_any ? fdn_sector_data[fdn] : 1'b0;
wire       fd_dclk_en     = fd_any ? fdn_dclk[fdn]        : 1'b0;
wire       fd_present     = fd_any ? fdn_present[fdn]     : 1'b0;
wire       fd_writeprot   = fd_any ? img_wp[fdn]          : 1'b1;

wire       fd_doubleside  = fdn_doubleside[fdn];
wire [4:0]  fd_spt         = fdn_spt[fdn];

// MEGA65 internal floppy: ready (CIA PA1) comes from the controller, which
// models the 1581 mechanism's RDY line: motor on and index pulses seen.
assign floppy_ready = phys_mode ? phys_media_ready_c : (fd_ready && fd_present);

assign disk_present = phys_mode ? phys_media_ready_c : fd_present;

// index for the WD's own housekeeping (motor timeout, spin-up count,
// Force Interrupt on index): the real index in physical mode
wire fd_index_eff = phys_mode ? phys_index_c : fd_index;

// -------------------------------------------------------------------------
// ----------------------- internal state machines -------------------------
// -------------------------------------------------------------------------

// --------------------------- Motor handling ------------------------------

// if motor is off and type 1 command with "spin up sequnce" bit 3 set
// is received then the command is executed after the motor has
// reached full speed for 5 rotations (800ms spin-up time + 5*200ms =
// 1.8sec) If the floppy is idle for 10 rotations (2 sec) then the
// motor is switched off again
localparam MOTOR_IDLE_COUNTER = 4'd10;
reg [3:0] motor_timeout_index /* verilator public */ = 0;
reg indexD = 0;
reg busy /* verilator public */ = 0;
reg [3:0] motor_spin_up_sequence /* verilator public */ = 0;


// consider spin up done either if the motor is not supposed to spin at all or
// if it's supposed to run and has left the spin up sequence
wire motor_spin_up_done = (!motor_on) || (motor_on && (motor_spin_up_sequence == 0));

// ---------------------------- step handling ------------------------------

localparam STEP_PULSE_LEN = 16'd1;
localparam STEP_PULSE_CLKS = STEP_PULSE_LEN * CLK_EN;
reg [15:0] step_pulse_cnt = 0;

// the step rate is only valid for command type I
wire [15:0] step_rate_clk = 
           (cmd[1:0]==2'b00)               ? (16'd6 *CLK_EN-1'd1):   //  6ms
           (cmd[1:0]==2'b01)               ? (16'd12*CLK_EN-1'd1):   // 12ms
           (MODEL == 2 && cmd[1:0]==2'b10) ? (16'd2 *CLK_EN-1'd1):   //  2ms
           (cmd[1:0]==2'b10)               ? (16'd20*CLK_EN-1'd1):   // 20ms
           (MODEL == 2)                    ? (16'd3 *CLK_EN-1'd1):   //  3ms
                                             (16'd30*CLK_EN-1'd1);   // 30ms

reg [15:0] step_rate_cnt = 0;
reg [23:0] delay_cnt = 0;

assign floppy_step = step_in | step_out;

// flag indicating that a "step" is in progress
wire step_busy = (step_rate_cnt != 0);
wire delaying = (delay_cnt != 0);

wire fd_track0 = (fd_track == 0);

reg [7:0] step_to = 0;
reg RNF = 0;
reg sector_inc_strobe = 0;
reg track_inc_strobe = 0;
reg track_dec_strobe = 0;
reg track_clear_strobe = 0;

// MEGA65 port: see the note at sd_ackD. data_transfer_state is the one that matters most
// here: stuck at 2'b00 it re-requests the same sector from the host forever instead of
// advancing to the phase that hands it to the drive CPU.
reg [1:0] seek_state = 0;
reg notready_wait = 0;
reg sector_not_found = 0;
reg irq_at_index = 0;
reg [1:0] data_transfer_state = 0;
// MEGA65 (D81 enable): CDC-safe stand-in for `sd_state == SD_IDLE` (sd_state
// now lives on clk_sys). Cleared in the same cycle as the SD request; set
// again when the clk_sys FSM toggles sd_done_tgl (synced here as sd_done_tgl_c).
reg sd_io_idle = 1'b1;
wire sd_done_tgl_c;
reg sd_done_tgl_cD = 1'b0;

always @(posedge clkcpu) begin
	sector_inc_strobe <= 1'b0;
	track_inc_strobe <= 1'b0;
	track_dec_strobe <= 1'b0;
	track_clear_strobe <= 1'b0;
	irq_set <= 1'b0;

	// MEGA65 internal floppy: one-cycle strobes
	phys_set_sector <= 1'b0;
	phys_rd_start   <= 1'b0;
	phys_cmd_clear  <= 1'b0;

	sd_done_tgl_cD <= sd_done_tgl_c;
	if (sd_done_tgl_c ^ sd_done_tgl_cD) sd_io_idle <= 1'b1;

	if(!floppy_reset) begin
		motor_on <= 1'b0;
		busy <= 1'b0;
		step_in <= 1'b0;
		step_out <= 1'b0;
		sd_card_read <= 0;
		sd_card_write <= 0;
		sd_io_idle <= 1'b1;
		data_transfer_start <= 1'b0;
		seek_state <= 0;
		notready_wait <= 1'b0;
		sector_not_found <= 1'b0;
		irq_at_index <= 1'b0;
		data_transfer_state <= 2'b00;
		RNF <= 1'b0;

		// MEGA65 internal floppy: engine reset. The toggle outputs keep their
		// level (no phantom edge); the trackers re-arm to the current value.
		phys_step_busy    <= 1'b0;
		phys_rd_pending   <= 1'b0;
		phys_reading      <= 1'b0;
		phys_done_latched <= 1'b0;
		phys_verify       <= 1'b0;
		phys_reissue      <= 1'b0;
		phys_rnf_l <= 1'b0; phys_crc_l <= 1'b0; phys_del_l <= 1'b0;
		phys_step_tally <= 8'd0;
		phys_t1_min_cnt <= 14'd0;
		phys_step_ack_serviced <= phys_step_ack_c;
		phys_rd_done_serviced  <= phys_rd_done_c_d2;
		phys_wr_op       <= 1'b0;
		phys_wr_active   <= 1'b0;
		phys_wr_track_op <= 1'b0;
		phys_wr_wpterm   <= 1'b0;
		phys_wr_wp_l     <= 1'b0;
		phys_wr_lost_l   <= 1'b0;
		phys_wr_clean_l  <= 1'b0;
		phys_wr_reissue  <= 1'b0;
	end else if (clk8m_en) begin
		sd_card_read <= 0;
		sd_card_write <= 0;
		data_transfer_start <= 1'b0;

		// disable step signal after 1 msec
		if(step_pulse_cnt != 0) 
			step_pulse_cnt <= step_pulse_cnt - 16'd1;
		else begin
			step_in <= 1'b0;
			step_out <= 1'b0;
		end

		 // step rate timer
		if(step_rate_cnt != 0) 
			step_rate_cnt <= step_rate_cnt - 16'd1;

		// delay timer
		if(delay_cnt != 0)
			delay_cnt <= delay_cnt - 1'd1;

		// MEGA65 internal floppy: minimum busy time (see PHYS_T1_MIN_TICKS)
		if(phys_t1_min_cnt != 0)
			phys_t1_min_cnt <= phys_t1_min_cnt - 14'd1;

		// just received a new command
		if(cmd_rx) begin
			busy <= 1'b1;
			notready_wait <= 1'b0;
			sector_not_found <= 1'b0;
			data_transfer_state <= 2'b00;

			// MEGA65 internal floppy: every command starts from a clean engine
			phys_rd_pending   <= 1'b0;
			phys_reading      <= 1'b0;
			phys_done_latched <= 1'b0;
			phys_verify       <= 1'b0;
			phys_step_busy    <= 1'b0;
			phys_reissue      <= 1'b0;
			phys_step_tally   <= 8'd0;
			phys_wr_op        <= 1'b0;
			phys_wr_active    <= 1'b0;
			phys_wr_track_op  <= 1'b0;
			phys_wr_wpterm    <= 1'b0;
			phys_wr_reissue   <= 1'b0;

			if(cmd_type_1 || cmd_type_2 || cmd_type_3) begin
				RNF <= 1'b0;
				motor_on <= 1'b1;
				// 'h' flag '0' -> wait for spin up
				if (!motor_on && !cmd[3]) motor_spin_up_sequence <= 6;   // wait for 6 full rotations

				// MEGA65 internal floppy: accepting a command clears DRQ,
				// LOST DATA and the latched result flags, as on the real chip
				if (phys_mode) begin
					phys_cmd_clear <= 1'b1;
					phys_rnf_l <= 1'b0; phys_crc_l <= 1'b0; phys_del_l <= 1'b0;
					phys_wr_wp_l <= 1'b0; phys_wr_lost_l <= 1'b0;
					if (cmd_type_1) phys_t1_min_cnt <= PHYS_T1_MIN_TICKS;
				end
			end

			// handle "forced interrupt"
			if(cmd_type_4) begin
				busy <= 1'b0;
				if(cmd[3]) irq_set <= 1'b1;
				if(cmd[3:2] == 2'b01) irq_at_index <= 1'b1;
				// From Hatari: Starting a Force Int command when idle should set the motor bit and clear the spinup bit (verified on STF)
				if (!busy) motor_on <= 1'b1;

				// MEGA65 internal floppy: abort any op in flight; the
				// controller closes WGATE and answers with a done we ignore
				if (phys_mode) begin
					phys_rd_cancel_tgl <= ~phys_rd_cancel_tgl;
					phys_rd_pending    <= 1'b0;
					phys_reading       <= 1'b0;
					phys_done_latched  <= 1'b0;
					phys_verify        <= 1'b0;
					phys_step_busy     <= 1'b0;
					phys_reissue       <= 1'b0;
					phys_wr_op         <= 1'b0;
					phys_wr_active     <= 1'b0;
					phys_wr_track_op   <= 1'b0;
					phys_wr_wpterm     <= 1'b0;
					phys_wr_reissue    <= 1'b0;
				end
			end
		end

		// execute command if motor is not supposed to be running or
		// wait for motor spinup to finish
		// MEGA65 internal floppy: the controller owns spin-up and readiness
		if(busy && (phys_mode || motor_spin_up_done) && !step_busy && !delaying) begin

			// ------------------------ TYPE I -------------------------
			if(cmd_type_1) begin
				// MEGA65 internal floppy: Type-I commands always run, as on the
				// real WD1772 (no READY input). Only a step clears the disk-change
				// latch, so gating steps on readiness would deadlock the DOS.
				if(!phys_mode && !fd_present) begin
					// no image selected -> send irq after 6 ms
					if (!notready_wait) begin
						delay_cnt <= 16'd6*CLK_EN;
						notready_wait <= 1'b1;
					end else begin
						RNF <= 1'b1;
						busy <= 1'b0;
						irq_set <= 1'b1; // emit irq when command done
					end
				end else
				// evaluate command
				case (seek_state)
				0: begin
					// restore
					if(cmd[7:4] == 4'b0000) begin
						if (phys_mode ? phys_track0_c : fd_track0) begin
							track_clear_strobe <= 1'b1;
							seek_state <= 2;
						end else if (phys_mode && phys_step_tally == 8'd255) begin
							// MEGA65 internal floppy: like the real WD1772, give up
							// with a seek error if track 0 never shows after 255 steps
							RNF <= 1'b1;
							seek_state <= 3;
						end else begin
							step_dir <= 1'b1;
							seek_state <= 1;
						end
					end

					// seek
					if(cmd[7:4] == 4'b0001) begin
						if (track == step_to) seek_state <= 2;
						else begin
							step_dir <= (step_to < track);
							seek_state <= 1;
						end
					end

					// step
					if(cmd[7:5] == 3'b001) seek_state <= 1;

					// step-in
					if(cmd[7:5] == 3'b010) begin
						step_dir <= 1'b0;
						seek_state <= 1;
					end

					// step-out
					if(cmd[7:5] == 3'b011) begin
						step_dir <= 1'b1;
						seek_state <= 1;
					end
				   end

				// do the step
				1: begin
					if (phys_mode) begin
						// MEGA65 internal floppy: one acknowledged physical step.
						// The step ack (handshake block below) advances seek_state.
						if (!phys_step_busy) begin
							phys_step_outward <= step_dir;          // 1 = toward track 0
							phys_step_req_tgl <= ~phys_step_req_tgl;
							phys_step_busy    <= 1'b1;
							if (phys_step_tally != 8'd255)
								phys_step_tally <= phys_step_tally + 8'd1;
							if( (!cmd[6] && !cmd[5]) || ((cmd[6] || cmd[5]) && cmd[4]))
								if (step_dir)
									track_dec_strobe <= 1'b1;
								else
									track_inc_strobe <= 1'b1;
						end
					end else begin
						if (step_dir)
							step_in <= 1'b1;
						else
							step_out <= 1'b1;

						// update the track register if seek/restore or the update flag set
						if( (!cmd[6] && !cmd[5]) || ((cmd[6] || cmd[5]) && cmd[4]))
							if (step_dir)
								track_dec_strobe <= 1'b1;
							else
								track_inc_strobe <= 1'b1;

						step_pulse_cnt <= STEP_PULSE_CLKS - 1'd1;
						step_rate_cnt <= step_rate_clk;

						seek_state <= (!cmd[6] && !cmd[5]) ? 0 : 2; // loop for seek/restore
					end
				   end

				// verify
				2: begin
					if (phys_mode) begin
						if (cmd[2]) begin
							// MEGA65 internal floppy: once the head has settled, verify
							// by reading an ID field; the done finishes the command
							if (phys_head_settled_c && !phys_rd_pending && !phys_verify) begin
								phys_rd_op      <= 3'b010;    // RDOP_VERIFY (no bytes)
								phys_rd_track   <= track;
								phys_rd_side    <= floppy_side;
								phys_rd_sector  <= sector;
								phys_rd_seq     <= phys_rd_seq + 2'd1;
								phys_rd_req_tgl <= ~phys_rd_req_tgl;
								phys_rd_pending <= 1'b1;
								phys_verify     <= 1'b1;
								phys_rd_start   <= 1'b1;
							end
						end else
							seek_state <= 3;
					end else begin
						if (cmd[2]) begin
							delay_cnt <= 16'd3*CLK_EN; // TODO: implement verify, now just delay one more step
						end
						seek_state <= 3;
					end
				   end

				// finish
				3: begin
					// MEGA65 internal floppy: hold busy for the minimum time
					if (!phys_mode || phys_t1_min_cnt == 14'd0) begin
						busy <= 1'b0;
						irq_set <= 1'b1; // emit irq when command done
						seek_state <= 0;
					end
				   end
				endcase
			end // if (cmd_type_1)

			// ------------------------ TYPE II -------------------------
			if(cmd_type_2 && phys_mode) begin
				// MEGA65 internal floppy: Read Sector issues one physical read op,
				// Write Sector one write op. Each completes ONLY through the
				// controller's done, which bounds not-ready (~1.1 s) and record
				// not found (5 index edges / 1.3 s), so the two never get out
				// of step.
				// A read starts only once the read FIFO is empty: bytes left by a
				// cancelled op would otherwise be presented as this sector's.
				if (cmd[7:5] == 3'b100) begin
					// read sector
					if (!phys_rd_pending && !phys_done_latched && !phys_reissue && phys_byte_empty) begin
						phys_rd_op      <= 3'b000;    // RDOP_READ_SECTOR
						phys_rd_track   <= track;
						phys_rd_side    <= floppy_side;
						phys_rd_sector  <= sector;
						phys_rd_seq     <= phys_rd_seq + 2'd1;
						phys_rd_req_tgl <= ~phys_rd_req_tgl;
						phys_rd_pending <= 1'b1;
						phys_reading    <= 1'b1;
						phys_rd_start   <= 1'b1;
					end
				end else if (cmd[7:5] == 3'b101) begin
					// write sector: the real chip samples /WPRT at command start.
					// Every write termination holds BUSY for the minimum time.
					if (!phys_rd_pending && !phys_done_latched && !phys_wr_reissue && !phys_wr_wpterm) begin
						phys_t1_min_cnt <= PHYS_T1_MIN_TICKS;
						if (phys_wprot_c) begin
							phys_wr_wpterm     <= 1'b1;   // status bit 6 alone, no request, no DRQ
							phys_wr_wp_l       <= 1'b1;
							phys_dbg_wr_wp_tgl <= ~phys_dbg_wr_wp_tgl;
						end else begin
							phys_rd_op       <= 3'b100;   // WROP_WRITE_SECTOR
							phys_rd_track    <= track;
							phys_rd_side     <= floppy_side;
							phys_rd_sector   <= sector;
							phys_wr_deleted  <= cmd[0];   // a0 = 1 -> DAM F8
							phys_wr_precomp  <= ~cmd[1];  // P  = 0 -> precomp on
							phys_rd_seq      <= phys_rd_seq + 2'd1;
							phys_rd_req_tgl  <= ~phys_rd_req_tgl;
							phys_rd_pending  <= 1'b1;
							phys_wr_op       <= 1'b1;
							phys_wr_active   <= 1'b1;
							phys_wr_track_op <= 1'b0;
						end
					end else if (phys_wr_wpterm && phys_t1_min_cnt == 14'd0) begin
						phys_wr_wpterm <= 1'b0;
						busy           <= 1'b0;
						irq_set        <= 1'b1;
					end
				end
			end else
			if(cmd_type_2) begin
				if(!fd_present) begin
					// no image selected -> send irq after 6 ms
					if (!notready_wait) begin
						delay_cnt <= 16'd6*CLK_EN;
						notready_wait <= 1'b1;
					end else begin
						RNF <= 1'b1;
						busy <= 1'b0;
						irq_set <= 1'b1; // emit irq when command done
					end
				end else if (sector_not_found) begin
					busy <= 1'b0;
					irq_set <= 1'b1; // emit irq when command done
					RNF <= 1'b1;
				end else if (cmd[2] && !notready_wait) begin
					// e flag: 15 ms settling delay
					delay_cnt <= 16'd15*CLK_EN;
					notready_wait <= 1'b1;
					// read sector
				end else begin
					if(cmd[7:5] == 3'b100) begin
						if ((sector - sector_base) >= fd_spt) begin
							// wait 5 rotations (1 sec) before setting RNF
							sector_not_found <= 1'b1;
							delay_cnt <= 24'd1000 * CLK_EN;
						end else if (sd_io_idle) begin
							case (data_transfer_state)

							2'b00: if (fifo_cpuptr == 0) begin
								// SD Card phase
								sd_card_read <= 1;
								sd_io_idle <= 1'b0;
								data_transfer_state <= 2'b01;
							end

							2'b01: begin
								// CPU phase: FIFO is already full (we only re-enter
								// after sd_io_idle rises). Wait for the matching header.
								if(fd_ready && fd_sector_hdr && (fd_sector == sector)) data_transfer_start <= 1'b1;

								if(data_transfer_done) begin
									data_transfer_state <= 2'b00;
									if (cmd[4]) sector_inc_strobe <= 1'b1; // multiple sector transfer
									else begin
										busy <= 1'b0;
										irq_set <= 1'b1; // emit irq when command done
									end
								end
							end

							default :;
							endcase

						end
					end

					// write sector
					if(cmd[7:5] == 3'b101) begin
						if ((sector - sector_base) >= fd_spt) begin
							// wait 5 rotations (1 sec) before setting RNF
							sector_not_found <= 1'b1;
							delay_cnt <= 24'd1000 * CLK_EN;
						end else if (sd_io_idle) begin
							case (data_transfer_state)
							2'b00: begin
								// pre-read phase
									if (sector_size_code < 2) begin
										sd_card_read <= 1;
										sd_io_idle <= 1'b0;
									end
									data_transfer_state <= 2'b10;
								end
							2'b10: begin
								// CPU phase
								if (fifo_cpuptr == 0 && fd_ready && fd_sector_hdr && (fd_sector == sector)) data_transfer_start <= 1'b1;
								if (data_transfer_done) begin
									sd_card_write <= 1;
									sd_io_idle <= 1'b0;
									data_transfer_state <= 2'b11;
								end
							end

							2'b11: begin
								// SD Card phase
								data_transfer_state <= 2'b00;
								if (cmd[4]) sector_inc_strobe <= 1'b1; // multiple sector transfer
								else begin
									busy <= 1'b0;
									irq_set <= 1'b1; // emit irq when command done
								end
							end

							default: ;
							endcase

						end
					end
				end
			end

			// ------------------------ TYPE III -------------------------
			if(cmd_type_3 && phys_mode) begin
				// MEGA65 internal floppy: Read Address returns the 6 ID bytes
				// through the FIFO and copies C into the sector register. Write
				// Track is a write op like Write Sector. Read Track is not
				// supported and just finishes, as in image mode.
				if (cmd[7:4] == 4'b1100) begin
					// read address (once the read FIFO is empty, see Read Sector)
					if (!phys_rd_pending && !phys_done_latched && phys_byte_empty) begin
						phys_rd_op      <= 3'b001;    // RDOP_READ_ADDRESS
						phys_rd_track   <= track;
						phys_rd_side    <= floppy_side;
						phys_rd_sector  <= sector;
						phys_rd_seq     <= phys_rd_seq + 2'd1;
						phys_rd_req_tgl <= ~phys_rd_req_tgl;
						phys_rd_pending <= 1'b1;
						phys_reading    <= 1'b1;
						phys_rd_start   <= 1'b1;
					end
				end else if (cmd[7:4] == 4'b1111) begin
					// write track: runs index to index, no 512-byte limit
					if (!phys_rd_pending && !phys_done_latched && !phys_wr_wpterm) begin
						phys_t1_min_cnt <= PHYS_T1_MIN_TICKS;
						if (phys_wprot_c) begin
							phys_wr_wpterm     <= 1'b1;
							phys_wr_wp_l       <= 1'b1;
							phys_dbg_wr_wp_tgl <= ~phys_dbg_wr_wp_tgl;
						end else begin
							phys_rd_op       <= 3'b101;   // WROP_WRITE_TRACK
							phys_rd_track    <= track;
							phys_rd_side     <= floppy_side;
							phys_rd_sector   <= sector;
							phys_wr_deleted  <= 1'b0;
							phys_wr_precomp  <= ~cmd[1];  // P = 0 -> precomp on
							phys_rd_seq      <= phys_rd_seq + 2'd1;
							phys_rd_req_tgl  <= ~phys_rd_req_tgl;
							phys_rd_pending  <= 1'b1;
							phys_wr_op       <= 1'b1;
							phys_wr_active   <= 1'b1;
							phys_wr_track_op <= 1'b1;
						end
					end else if (phys_wr_wpterm && phys_t1_min_cnt == 14'd0) begin
						phys_wr_wpterm <= 1'b0;
						busy           <= 1'b0;
						irq_set        <= 1'b1;
					end
				end else begin
					// read track: not supported -> finish
					busy    <= 1'b0;
					irq_set <= 1'b1;
				end
			end else
			if(cmd_type_3) begin
				if(!fd_present) begin
					// no image selected -> send irq immediately
					RNF <= 1'b1;
					busy <= 1'b0; 
					irq_set <= 1'b1; // emit irq when command done
				end else begin
					// read track TODO: fake
					if(cmd[7:4] == 4'b1110) begin
						busy <= 1'b0;
						irq_set <= 1'b1; // emit irq when command done
					end

					// write track TODO: fake
					if(cmd[7:4] == 4'b1111) begin
						busy <= 1'b0;
						irq_set <= 1'b1; // emit irq when command done
					end

					// read address
					if(cmd[7:4] == 4'b1100) begin
						// we are busy until the next setor header passes under the head
						if(fd_ready && fd_sector_hdr)
							data_transfer_start <= 1'b1;

						if(data_transfer_done) begin
							busy <= 1'b0;
							irq_set <= 1'b1; // emit irq when command done
						end
					end
				end
			end
		end

		// ---------------------------------------------------------------------
		// MEGA65 internal floppy: step ack and op done handshakes. Outside the
		// execute gate, so they advance the seek FSM and finish an op even
		// while the WD is otherwise waiting. Toggles are compared against
		// their *_serviced copy, so no edge is lost across the clock crossing.
		// ---------------------------------------------------------------------
		if (phys_mode) begin
			// one physical step acknowledged -> advance the Type-I seek FSM
			if (phys_step_ack_c != phys_step_ack_serviced) begin
				phys_step_ack_serviced <= phys_step_ack_c;
				if (phys_step_busy) begin
					phys_step_busy <= 1'b0;
					// loop for seek/restore (cmd[6:5]==00), else go verify/finish
					seek_state <= (!cmd[6] && !cmd[5]) ? 2'd0 : 2'd2;
				end
			end

			// The done must carry the current op's sequence tag. A done with
			// another tag, or with no op pending, is consumed but ignored, so
			// a stale result never pairs with a newer command.
			if (phys_rd_done_c_d2 != phys_rd_done_serviced) begin
				phys_rd_done_serviced <= phys_rd_done_c_d2;
				if (phys_rd_pending && phys_rd_done_seq_c == phys_rd_seq) begin
					phys_rnf_l <= phys_rd_rnf_c;
					phys_crc_l <= phys_rd_crc_err_c;
					phys_del_l <= phys_rd_deleted_c;
					phys_c_l   <= phys_rd_c_c;
					phys_rd_pending   <= 1'b0;
					phys_done_latched <= 1'b1;
					// a failed capture is not sector data: drain it unseen
					if (phys_rd_rnf_c || phys_rd_crc_err_c)
						phys_reading <= 1'b0;
					// A write op stops taking bytes and latches its status.
					// Write protect is reported alone: the DOS's status table
					// ($CD5A) maps it together with CRC/RNF to garbage.
					if (phys_wr_op) begin
						phys_wr_active  <= 1'b0;
						phys_wr_clean_l <= (phys_rd_result_c == PHYS_RES_OK) && !phys_wr_lost_c
						                   && !phys_rd_rnf_c && !phys_rd_crc_err_c;
						if (phys_rd_result_c == PHYS_RES_WRITE_PROTECTED) begin
							phys_wr_wp_l <= 1'b1;
							phys_rnf_l   <= 1'b0;
							phys_crc_l   <= 1'b0;
						end else
							phys_wr_lost_l <= phys_wr_lost_c || (phys_rd_result_c == PHYS_RES_LOST_DATA);
					end
				end else begin
					phys_dbg_staledone_tgl <= ~phys_dbg_staledone_tgl;
				end
			end

			// Finish a read op once the done is in, the FIFO is empty and one
			// more byte-time has passed since the last byte, as on the real
			// chip: the 1581 ROM's transfer loops poll BUSY before DRQ
			// ($CD17), so busy must outlive the last DRQ. A stalled CPU gets
			// LOST DATA, never a wedged WD. A verify is a Type-I completion
			// and also honours the minimum busy time.
			if (phys_done_latched && !phys_wr_op && phys_byte_empty && phys_pace_cnt == 8'd0
			    && !phys_present_now
			    && (!phys_verify || phys_t1_min_cnt == 14'd0)) begin
				phys_done_latched <= 1'b0;
				phys_reading      <= 1'b0;
				RNF               <= phys_rnf_l;
				phys_dbg_pres_cnt <= phys_pres_cnt;
				phys_dbg_fin_tgl  <= ~phys_dbg_fin_tgl;
				if (phys_verify) begin
					// Type-I verify: seek error mirrors RNF; finish the command
					phys_verify <= 1'b0;
					busy        <= 1'b0;
					irq_set     <= 1'b1;
					seek_state  <= 2'd0;
				end else if (cmd[7:5] == 3'b100 && cmd[4] && !phys_rnf_l && !phys_crc_l) begin
					// multiple-sector read: only a clean sector continues
					sector_inc_strobe <= 1'b1;
					phys_reissue      <= 1'b1;
				end else begin
					if (cmd[7:4] == 4'b1100)  // Read Address: WD writes C into sector reg
						phys_set_sector <= 1'b1;
					busy    <= 1'b0;
					irq_set <= 1'b1;
				end
			end

			// Next sector of a multiple-sector read (sector already advanced):
			// a new op with a fresh tag. Not on cmd_rx: a command while busy
			// can only be Force Interrupt, whose cancel clears phys_reissue
			// only from the next tick on.
			if (phys_reissue && !cmd_rx && phys_byte_empty) begin
				phys_reissue    <= 1'b0;
				phys_rd_op      <= 3'b000;    // RDOP_READ_SECTOR
				phys_rd_track   <= track;
				phys_rd_side    <= floppy_side;
				phys_rd_sector  <= sector;
				phys_rd_seq     <= phys_rd_seq + 2'd1;
				phys_rd_req_tgl <= ~phys_rd_req_tgl;
				phys_rd_pending <= 1'b1;
				phys_reading    <= 1'b1;
				phys_rd_start   <= 1'b1;
			end

			// Finish a write op once the done is in, the writer is idle (the
			// last cell is on the disk) and the minimum busy time has passed.
			// A clean m = 1 Write Sector continues with the next sector.
			if (phys_done_latched && phys_wr_op && !phys_wr_busy_c
			    && phys_t1_min_cnt == 14'd0) begin
				phys_done_latched   <= 1'b0;
				phys_wr_op          <= 1'b0;
				RNF                 <= phys_rnf_l;
				phys_dbg_wr_fin_tgl <= ~phys_dbg_wr_fin_tgl;
				if (!phys_wr_track_op && cmd[7:5] == 3'b101 && cmd[4] && phys_wr_clean_l) begin
					sector_inc_strobe <= 1'b1;
					phys_wr_reissue   <= 1'b1;
				end else begin
					busy    <= 1'b0;
					irq_set <= 1'b1;
				end
			end

			// next sector of a multiple-sector write, gated like the read one
			if (phys_wr_reissue && !cmd_rx) begin
				phys_wr_reissue  <= 1'b0;
				phys_t1_min_cnt  <= PHYS_T1_MIN_TICKS;
				phys_rd_op       <= 3'b100;   // WROP_WRITE_SECTOR
				phys_rd_track    <= track;
				phys_rd_side     <= floppy_side;
				phys_rd_sector   <= sector;
				phys_wr_deleted  <= cmd[0];
				phys_wr_precomp  <= ~cmd[1];
				phys_rd_seq      <= phys_rd_seq + 2'd1;
				phys_rd_req_tgl  <= ~phys_rd_req_tgl;
				phys_rd_pending  <= 1'b1;
				phys_wr_op       <= 1'b1;
				phys_wr_active   <= 1'b1;
				phys_wr_track_op <= 1'b0;
			end
		end

		// stop motor if there was no command for 10 index pulses
		indexD <= fd_index_eff;
		if(indexD && !fd_index_eff) begin
			irq_at_index <= 1'b0;
			if (irq_at_index) irq_set <= 1'b1;

			// let motor timeout run once fdc is not busy anymore
			if(!busy && motor_spin_up_done) begin
				if(motor_timeout_index != 0)
					motor_timeout_index <= motor_timeout_index - 4'd1;
				else if(motor_on)
					motor_timeout_index <= MOTOR_IDLE_COUNTER;

				if(motor_timeout_index == 1)
					motor_on <= 1'b0;
			end

			if(motor_spin_up_sequence != 0)
				motor_spin_up_sequence <= motor_spin_up_sequence - 4'd1;
		end
		if(busy) motor_timeout_index <= 0;
	end
end

// floppy delivers data at a floppy generated rate (usually 250kbit/s), so the start and stop
// signals need to be passed forth and back from cpu clock domain to floppy data clock domain
reg data_transfer_start = 0;
reg data_transfer_done = 0;

// ==================================== FIFO ==================================

// 0.5/1 kB buffer used to receive a sector as fast as possible from from the io
// controller. The internal transfer afterwards then runs at 250000 Bit/s
reg  [10:0] fifo_cpuptr = 0;
reg  [9:0] fifo_cpuptr_adj = 0;
wire [7:0] fifo_q;
reg  [9:0] fifo_sdptr = 0;

// MEGA65 port: these two must be declared before the fifo instance below. A port
// connection naming an identifier that is not declared yet makes Vivado create a
// 1-bit implicit net instead, which silently truncated data_in to one bit -- and
// data_in also carries the seek target (step_to), so the drive could only ever
// reach track 0 or 1.
reg        data_in_strobe = 0;
reg  [7:0] data_in = 0;

always @(*) begin
	if (sector_size_code == 3)
		fifo_sdptr = { s_odd, sd_buff_addr };
	else
		fifo_sdptr = { 1'b0, sd_buff_addr };

	if (sector_size_code == 1)
		fifo_cpuptr_adj = { 1'b0, (fd_spt[0] & (track[0] ^ !floppy_side)) ^ sector[0], fifo_cpuptr[7:0] };
	else
		fifo_cpuptr_adj = fifo_cpuptr[9:0];
end

// MEGA65 (D81 enable): dual-clock FIFO. Port A is the SD/vdrives side on
// clk_sys; port B is the drive CPU on clkcpu. The RAM is the data-path CDC.
fdc1772_dpram #(8, 10) fifo
(
	.clock_a(clk_sys),
	.address_a(fifo_sdptr),
	.data_a(sd_dout),
	.wren_a(sd_dout_strobe & sd_ack),
	.q_a(sd_din),

	.clock_b(clkcpu),
	.address_b(fifo_cpuptr_adj),
	.data_b(data_in),
	.wren_b(data_in_strobe),
	.q_b(fifo_q)
);

// ------------------ SD card control ------------------------
// MEGA65 (D81 enable): SD FSM on clk_sys so sd_rd/sd_wr/sd_lba/sd_ack
// live in the QNICE/vdrives domain. clkcpu hands off via pulse-synced
// toggles; completion comes back as sd_done_tgl -> sd_io_idle.
localparam SD_IDLE = 0;
localparam SD_READ = 1;
localparam SD_WRITE = 2;

reg [1:0] sd_state = 0;
reg       sd_card_write = 0;
reg       sd_card_read = 0;

reg sd_rd_req_tgl = 1'b0;
reg sd_wr_req_tgl = 1'b0;
// MEGA65 port: delayed copies at module scope (Vivado re-inits block-local regs).
reg sd_card_readD = 1'b0;
reg sd_card_writeD = 1'b0;
always @(posedge clkcpu) begin : label_sdreq
	sd_card_readD  <= sd_card_read;
	sd_card_writeD <= sd_card_write;
	if (~sd_card_readD  & sd_card_read)  sd_rd_req_tgl <= ~sd_rd_req_tgl;
	if (~sd_card_writeD & sd_card_write) sd_wr_req_tgl <= ~sd_wr_req_tgl;
end

wire sd_rd_req_s, sd_wr_req_s, floppy_reset_s;
reg  sd_done_tgl = 1'b0;
iecdrv_sync sd_rdreq_sync (clk_sys, sd_rd_req_tgl, sd_rd_req_s);
iecdrv_sync sd_wrreq_sync (clk_sys, sd_wr_req_tgl, sd_wr_req_s);
iecdrv_sync sd_frst_sync  (clk_sys, floppy_reset,  floppy_reset_s);
iecdrv_sync sd_done_sync  (clkcpu,  sd_done_tgl,   sd_done_tgl_c);

reg sd_ackD = 1'b0;
reg sd_rd_req_sD = 1'b0;
reg sd_wr_req_sD = 1'b0;
always @(posedge clk_sys) begin : label3
	sd_ackD      <= sd_ack;
	sd_rd_req_sD <= sd_rd_req_s;
	sd_wr_req_sD <= sd_wr_req_s;
	if (sd_ack) {sd_rd, sd_wr} <= 0;

	case (sd_state)
	SD_IDLE:
	begin
		s_odd <= 1'b0;
		if (sd_rd_req_s ^ sd_rd_req_sD) begin
			sd_rd[fdn] <= 1;
			sd_lba     <= sd_lba_comb;
			sd_state   <= SD_READ;
		end
		else if (sd_wr_req_s ^ sd_wr_req_sD) begin
			sd_wr[fdn] <= 1;
			sd_lba     <= sd_lba_comb;
			sd_state   <= SD_WRITE;
		end
	end

	SD_READ:
	if (sd_ackD & ~sd_ack) begin
		if (s_odd || sector_size_code != 3) begin
			sd_state    <= SD_IDLE;
			sd_done_tgl <= ~sd_done_tgl;
		end else begin
			s_odd      <= 1;
			sd_rd[fdn] <= 1;
			sd_lba     <= sd_lba_comb;
		end
	end

	SD_WRITE:
	if (sd_ackD & ~sd_ack) begin
		if (s_odd || sector_size_code != 3) begin
			sd_state    <= SD_IDLE;
			sd_done_tgl <= ~sd_done_tgl;
		end else begin
			s_odd      <= 1;
			sd_wr[fdn] <= 1;
			sd_lba     <= sd_lba_comb;
		end
	end

	default: ;
	endcase

	if (!floppy_reset_s) begin
		sd_rd    <= 0;
		sd_wr    <= 0;
		sd_state <= SD_IDLE;
		s_odd    <= 1'b0;
	end
end

// -------------------- CPU data read/write -----------------------
reg data_in_valid = 0;

function [15:0] crc;
	input [15:0] curcrc;
	input  [7:0] val;
	reg    [3:0] i = 0;
	begin
		crc = {curcrc[15:8] ^ val, 8'h00};
		for (i = 0; i < 8; i=i+1'd1) begin
			if(crc[15]) begin
				crc = crc << 1;
				crc = crc ^ 16'h1021;
			end
			else crc = crc << 1;
		end
		crc = {curcrc[7:0] ^ crc[15:8], crc[7:0]};
	end
endfunction

// MEGA65 port: see the note at sd_ackD. data_transfer_cnt is the byte counter of the
// transfer to the drive CPU, so it has to survive from one byte to the next.
reg        data_transfer_startD = 0;
reg [10:0] data_transfer_cnt = 0;
reg [15:0] crcval = 0;
reg        crc_en = 0;

always @(posedge clkcpu) begin
	crc_en <= 0;
	if(crc_en) crcval <= crc(crcval, data_out);

	// MEGA65 internal floppy: the real WD1772 has one data register, and the
	// 1581 ROM's startup test reads back the value just written. Image mode
	// keeps its old behaviour (data_in is still the previous value here).
	if (cpu_we && cpu_addr == FDC_REG_DATA) begin
		data_out <= phys_mode ? cpu_din : data_in;
		data_in_valid <= 1;
	end

	// reset fifo read pointer on reception of a new command or
	// when multi-sector transfer increments the sector number
	if(cmd_rx || sector_inc_strobe) begin
		data_in_valid <= 0;
		data_transfer_cnt <= 0;
		fifo_cpuptr <= 0;
	end

	drq_set <= 1'b0;

	// MEGA65 internal floppy: during a write op DRQ means write FIFO room
	if (phys_wr_drq_ok) drq_set <= 1'b1;

	// MEGA65 internal floppy: move each byte from the read FIFO into the data
	// register as it arrives, at most one per byte-time (31.5 us here, a little
	// under the disk's 32 us), and raise DRQ, no matter whether the CPU took the
	// last one, as the real WD1772 does. An unconsumed byte is overwritten and
	// sets LOST DATA. The pace starts expired, so the first byte comes at once.
	phys_byte_rd_en <= 1'b0;
	if (phys_cmd_clear || phys_rd_start) phys_lost_l <= 1'b0;
	if (phys_rd_start) begin
		phys_pace_cnt <= 8'd0;
		phys_pres_cnt <= 11'd0;
	end else if (clk8m_en) begin
		if (phys_present_now) begin
			data_out        <= phys_byte_data;
			phys_byte_rd_en <= 1'b1;
			drq_set         <= 1'b1;
			phys_pace_cnt   <= PHYS_PACE_TICKS - 8'd1;
			if (phys_pres_cnt != 11'h7FF)
				phys_pres_cnt <= phys_pres_cnt + 11'd1;
			if (drq) begin
				phys_lost_l       <= 1'b1;
				phys_dbg_lost_tgl <= ~phys_dbg_lost_tgl;
			end
		end else if (phys_pace_cnt != 8'd0)
			phys_pace_cnt <= phys_pace_cnt - 8'd1;
	end

	// MEGA65 internal floppy: while no read op presents bytes, discard
	// whatever is in the read FIFO (left from a cancelled op, a disk change,
	// a reset, or a CRC error), so every op starts from an empty FIFO.
	if (phys_mode && !phys_reading && !phys_byte_empty) begin
		phys_byte_rd_en <= 1'b1;
		if (!phys_draining) phys_dbg_drain_tgl <= ~phys_dbg_drain_tgl;
		phys_draining <= 1'b1;
	end else
		phys_draining <= 1'b0;

	if (clk8m_en) data_transfer_done <= 0;
	data_transfer_startD <= data_transfer_start;
	// received request to read data
	if(~data_transfer_startD & data_transfer_start) begin

		// read_address command has 6 data bytes
		if(cmd[7:4] == 4'b1100) begin
			crcval <= 16'hB230;
			data_transfer_cnt <= 11'd6+11'd1;
		end

		// read/write sector has SECTOR_SIZE data bytes
		if(cmd[7:6] == 2'b10)
			data_transfer_cnt <= sector_size + 1'd1;

		// write sector asserts drq earlier to fill up the data register in time
		if(cmd[7:5] == 3'b101) drq_set <= !data_in_valid;
	end

	// advance fifo pointer when the write sector data consumed
	data_in_strobe <= 1'b0;
	if(cmd[7:5] == 3'b101 && data_in_strobe) fifo_cpuptr <= fifo_cpuptr + 1'd1;

	if(fd_dclk_en) begin
		if(data_transfer_cnt != 0) begin
			if(data_transfer_cnt != 1) begin
				data_lost <= 1'b0;
				if (drq) data_lost <= 1'b1;
				// raise drq, except when the last byte is already taken from the CPU for write
				if (cmd[7:5] != 3'b101 || data_transfer_cnt != 2) drq_set <= 1'b1;

				// read_address
				if(cmd[7:4] == 4'b1100) begin
					case(data_transfer_cnt)
						7: begin data_out <= fd_track; crc_en <= 1; end
						6: begin data_out <= { 7'b0000000, (INVERT_HEAD_RA != 0) ^ floppy_side }; crc_en <= 1; end
						5: begin data_out <= fd_sector; crc_en <= 1; end
						4: begin data_out <= sector_size_code[1:0]; crc_en <= 1; end // TODO: sec size 0=128, 1=256, 2=512, 3=1024
						3: data_out <= crcval[15:8];
						2: data_out <= crcval[7:0];
					endcase // case (data_read_cnt)
				end

				// read sector
				if(cmd[7:5] == 3'b100 && fifo_cpuptr != sector_size) begin
					data_out <= fifo_q;
					fifo_cpuptr <= fifo_cpuptr + 1'd1;
				end
				// write sector
				if(cmd[7:5] == 3'b101 && fifo_cpuptr != sector_size) begin
					data_in_strobe <= 1;
					data_in_valid <= 0;
				end

			end

			// count down and stop after last byte
			data_transfer_cnt <= data_transfer_cnt - 11'd1;
			if(data_transfer_cnt == 1)
				data_transfer_done <= 1'b1;
		end
	end
end

// the status byte
// MEGA65 internal floppy: in physical mode the mechanical and result bits come
// from the controller. For writes, bit 6 is the latched write-protect outcome
// and bit 2 includes the write op's LOST DATA. RNF is suppressed while the CRC
// bit is set: the 1581 DOS ($CD3F) indexes a table with (status>>3)&$0B, and
// CRC+RNF together lands on a hole that means success.
wire [7:0] status = { (MODEL == 1 || MODEL == 3) ? !floppy_ready : (phys_mode ? phys_motor_on_c : motor_on),
		      phys_mode ? ( ((cmd[7:5] == 3'b101) || (cmd[7:4] == 4'b1111)) ? phys_wr_wp_l
		                    : (cmd_type_1 ? phys_wprot_c : 1'b0) )
		                : ((cmd[7:5] == 3'b101 || cmd[7:4] == 4'b1111 || cmd_type_1) && fd_writeprot), // wrprot (only for write!)
		      cmd_type_1 ? motor_spin_up_done : ((phys_mode && cmd[7:5] == 3'b100) ? phys_del_l : 1'b0), // data mark / deleted
		      phys_mode ? (RNF & ~phys_crc_l) : RNF,  // seek error/record not found
		      phys_mode ? phys_crc_l : 1'b0,          // crc error
		      cmd_type_1 ? (phys_mode ? phys_track0_c : fd_track0)
		                 : (phys_mode ? (phys_lost_l | phys_wr_lost_l) : data_lost),   // track0/data lost
		      cmd_type_1 ? (phys_mode ? ~phys_index_c : ~fd_index) : drq,          // index mark/drq
		      busy } /* synthesis keep */;

reg [7:0] track /* verilator public */ = 0;
assign out_track = track;
assign out_we = ((cmd[7:5] == 3'b101 || cmd[7:4] == 4'b1111) && busy) || (sd_state == SD_WRITE);
reg [7:0] sector = 0;
reg [7:0] data_out = 0;

reg step_dir = 0;
reg data_lost = 0;

// ---------------------------- command register -----------------------   
reg [7:0] cmd /* verilator public */ = 0;
wire cmd_type_1 = (cmd[7] == 1'b0);
wire cmd_type_2 = (cmd[7:6] == 2'b10);
wire cmd_type_3 = (cmd[7:5] == 3'b111) || (cmd[7:4] == 4'b1100);
wire cmd_type_4 = (cmd[7:4] == 4'b1101);

localparam FDC_REG_CMDSTATUS    = 0;
localparam FDC_REG_TRACK        = 1;
localparam FDC_REG_SECTOR       = 2;
localparam FDC_REG_DATA         = 3;

// CPU register read
always @(*) begin
	cpu_dout = 8'h00;

	if(cpu_sel && cpu_rw) begin
		case(cpu_addr)
			FDC_REG_CMDSTATUS: cpu_dout = status;
			FDC_REG_TRACK:     cpu_dout = track;
			FDC_REG_SECTOR:    cpu_dout = sector;
			FDC_REG_DATA:      cpu_dout = data_out;
		endcase
	end
end

// cpu register write
reg cmd_rx /* verilator public */ = 0;
reg cmd_rx_i = 0;

always @(posedge clkcpu) begin
	if(!floppy_reset) begin
		// clear internal registers
		cmd <= 8'h00;
		track <= 8'h00;
		sector <= 8'h00;

		// reset state machines and counters
		cmd_rx_i <= 1'b0;
		cmd_rx <= 1'b0;
	end else begin

		// cmd_rx is delayed to make sure all signals (the cmd!) are stable when
		// cmd_rx is evaluated
		cmd_rx <= cmd_rx_i;

		// command reception is ack'd by fdc going busy
		if((!cmd_type_4 && busy) || (clk8m_en && cmd_type_4 && !busy)) cmd_rx_i <= 1'b0;

		// only react if stb just raised
		if(cpu_we) begin
			// MEGA65 internal floppy: the real WD1772 ignores a command
			// written while busy unless it is Force Interrupt
			if(cpu_addr == FDC_REG_CMDSTATUS && phys_mode && busy && cpu_din[7:4] != 4'b1101)
				phys_dbg_busycmd_tgl <= ~phys_dbg_busycmd_tgl;
			else if(cpu_addr == FDC_REG_CMDSTATUS) begin  // command register
				cmd <= cpu_din;
				cmd_rx_i <= 1'b1;
				// ------------- TYPE I commands -------------
				if(cpu_din[7:4] == 4'b0000) begin               // RESTORE
					step_to <= 8'd0;
					track <= 8'hff;
				end

				if(cpu_din[7:4] == 4'b0001) begin               // SEEK
					step_to <= data_in;
				end

				if(cpu_din[7:5] == 3'b001) begin                // STEP
				end

				if(cpu_din[7:5] == 3'b010) begin                // STEP-IN
				end

				if(cpu_din[7:5] == 3'b011) begin                // STEP-OUT
				end

				// ------------- TYPE II commands -------------
				if(cpu_din[7:5] == 3'b100) begin                // read sector
				end

				if(cpu_din[7:5] == 3'b101) begin                // write sector
				end

				// ------------- TYPE III commands ------------
				if(cpu_din[7:4] == 4'b1100) begin               // read address
				end

				if(cpu_din[7:4] == 4'b1110) begin               // read track
				end

				if(cpu_din[7:4] == 4'b1111) begin               // write track
				end

				// ------------- TYPE IV commands -------------
				if(cpu_din[7:4] == 4'b1101) begin               // force intrerupt
				end
			end

			if(cpu_addr == FDC_REG_TRACK)                    // track register
				track <= cpu_din;

			if(cpu_addr == FDC_REG_SECTOR)                   // sector register
				sector <= cpu_din;

			if(cpu_addr == FDC_REG_DATA) begin               // data register
				data_in <= cpu_din;
			end
		end

		if (sector_inc_strobe) sector <= sector + 1'd1;
		if (track_inc_strobe) track <= track + 1'd1;
		if (track_dec_strobe) track <= track - 1'd1;
		if (track_clear_strobe) track <= 8'd0;
		// MEGA65 internal floppy: Read Address puts the ID's track byte (C)
		// into the sector register, per the WD1772 datasheet
		if (phys_set_sector) sector <= phys_c_l;
	end
end

endmodule

// MEGA65 (D81 enable): true dual-clock dual-port RAM. Port A = SD/io on
// clk_sys; port B = drive CPU on clkcpu.
module fdc1772_dpram #(parameter DATAWIDTH=8, ADDRWIDTH=9)
(
	input                   clock_a,
	input   [ADDRWIDTH-1:0] address_a,
	input   [DATAWIDTH-1:0] data_a,
	input                   wren_a,
	output reg [DATAWIDTH-1:0] q_a,

	input                   clock_b,
	input   [ADDRWIDTH-1:0] address_b,
	input   [DATAWIDTH-1:0] data_b,
	input                   wren_b,
	output reg [DATAWIDTH-1:0] q_b
);

reg [DATAWIDTH-1:0] ram[0:(1<<ADDRWIDTH)-1];

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
