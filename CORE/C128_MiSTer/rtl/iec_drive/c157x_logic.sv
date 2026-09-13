//-------------------------------------------------------------------------------
//
// Reworked and adapted to MiSTer by Sorgelig@MiSTer (07.09.2018)
//
// Extended with 157x models by Erik Scheffers
//
//-------------------------------------------------------------------------------
//
// Model 1541B / 1570 / 1571 
//
module c157x_logic #(DRIVE)
(
	input        clk,
	input        reset,
	input  [1:0] drv_mode,      // 00: 1541, 01: 1570, 10: 1571, (11: 1571CR todo)

	input        wd_ce,
	input  [1:0] ph2_r,
	input  [1:0] ph2_f,

	output       act,		    // activity LED

	// serial bus
	input        iec_clk_in,
	input        iec_data_in,
	input        iec_atn_in,
	input        iec_fclk_in,
	output       iec_clk_out,
	output       iec_data_out,
	output       iec_fclk_out,

	input        ext_en,
	output[14:0] rom_addr,
	input  [7:0] rom_data,

	// parallel bus
	input  [7:0] par_data_in,
	input        par_stb_in,
	output [7:0] par_data_out,
	output       par_stb_out,

	// drive control signals
	output       hinit,         // init head buffer
	input        hclk,          // bit clock
	input        hf,            // signal from head
	output       ht,            // signal to head

	output       side,          // disk side  
	input        wps_n,		    // write-protect sense
	
	output       mode,          // GCR mode (0=write, 1=read)
	output       wgate,         // MFM wgate (0=read, 1=write)
	output       fdc_busy,      // WD1770 busy
	output [1:0] stp,			 	 // stepper motor control
	output       mtr,			 	 // drive motor on/off
	output [1:0] freq,		    // motor frequency
	input        tr00_sense,    // track 0 sense
	input        index_sense,   // index pulse
	input        drive_enable,  // sd busy
	input        disk_present,

	input        img_mfm,       // mfm supported by disk image

	// Linear D64/D71 sector-image GCR path. This bypasses the raw-track 64H156
	// input while preserving the original VIA and DOS behavior.
	input        sector_gcr_enable,
	input  [7:0] sector_gcr_dout,
	input        sector_gcr_sync_n,
	input        sector_gcr_byte_n,
	output [7:0] sector_gcr_din
);

// clock control
// MEGA65 port: 0 on the FPGA, X in xsim without this, and an X in accl blocks ena_f.
reg [2:0] accl = 0;
always @(posedge clk)
begin
	if (~|drv_mode)
		accl <= 3'b000;
	else if (ph2_r[accl[1]]) 
		accl <= {accl[1:0],accl_ctl};
end

wire halt  = accl[0]^accl[2];
wire ena_f = ph2_f[accl[1]] & ~halt;
wire ena_r = ph2_r[accl[1]] & ~halt;

// cpu signal decode
assign rom_addr = cpu_a[14:0];

//same decoder as on real HW
wire [3:0] ls42 = {cpu_a[15], cpu_a[12:10]};   
wire ram_cs    = |drv_mode ? cpu_a[15:12] == 0 || cpu_a[15:13] == 3            : ls42 == 0 || ls42 == 1;
wire via1_cs   = |drv_mode ? cpu_a[15:12] == 1 && cpu_a[10] == 0               : ls42 == 6;
wire via2_cs   = |drv_mode ? cpu_a[15:12] == 1 && cpu_a[10] == 1               : ls42 == 7;
wire wd_cs     = |drv_mode ? cpu_a[15:13] == 1                                 : 1'b0;
wire cia_cs    = |drv_mode ? cpu_a[15:13] == 2 && (~&drv_mode | cpu_a[4] == 0) : 1'b0;
// wire scrram_cs = &drv_mode ? cpu_a[15:13] == 2 && cpu_a[4] == 1                : 1'b0;
wire rom_cs    = cpu_a[15];

wire  [7:0] cpu_di =
	!cpu_rw    ? cpu_do :
	 ram_cs    ? ram_do :
	 via1_cs   ? via1_cpu_do :
	 via2_cs   ? via2_do :
	 wd_cs     ? wd_do :
	 cia_cs    ? cia_do :
	//  scrram_cs ? scrram_do :
	 extram_cs ? extram_do :
	 rom_cs    ? rom_data :
	 8'hFF;

wire [23:0] cpu_a;
wire  [7:0] cpu_do;
wire        cpu_rw;
wire        cpu_irq_n = ~(via1_irq | via2_irq) & cia_irq_n;

// MEGA65 port: the 64H156 signals below are declared here rather than next to their
// instance further down, because the port connections in this file name them first
// and Vivado answers that by creating 1-bit implicit nets. gcr_do is the byte the
// read head delivers to VIA2 port A, so truncating it to one bit meant the 1541/1571
// could never read anything off a disk.
wire [7:0] gcr_do;
wire       sync_n, byte_n, byte_n_poll, dgcr_we;
wire       gcr_ht, gcr_hinit;
wire [7:0] h156_do;
wire       h156_sync_n, h156_byte_n;

T65 cpu
(
	.mode(2'b00),
	.res_n(~reset),
	.enable(ena_f),
	.clk(clk),
	.rdy(1'b1),
	.abort_n(1'b1),
	.irq_n(cpu_irq_n),
	.nmi_n(1'b1),
	.so_n(byte_n),
	.r_w_n(cpu_rw),
	.A(cpu_a),
	.DI(cpu_di),
	.DO(cpu_do)
);

// optional 8k RAM at $8000-$9FFF for custom roms

wire extram_cs = ext_en && (cpu_a[15:13] == 'b100);

wire [7:0] extram_do;
iecdrv_mem #(.DATAWIDTH(8), .ADDRWIDTH(13), .WRITE_B(0)) extram
(
	.clock_a(clk),
	.address_a(cpu_a[12:0]),
	.data_a(cpu_do),
	.wren_a(ena_r & ~cpu_rw & extram_cs),

	// MEGA65 port: port B only ever reads. Leaving wren_b/data_b unconnected makes Vivado
	// infer a true dual-port RAM whose two write ports share one address, which it reports
	// as [Synth 8-5796] and whose collision behaviour is undefined. Tying them off keeps
	// the inferred RAM a simple dual-port and matches the simulation exactly.
	.clock_b(clk),
	.address_b(cpu_a[12:0]),
	.data_b(8'h00),
	.wren_b(1'b0),
	.q_b(extram_do)
);

// system 2k RAM at $0000-$07FF

wire [7:0] ram_do;
iecdrv_mem #(.DATAWIDTH(8), .ADDRWIDTH(11), .WRITE_B(0), .USE_B(0)) ram
(
	.clock_a(clk),
	.address_a(cpu_a[10:0]),
	.data_a(cpu_do),
	.wren_a(ena_r & ~cpu_rw & ram_cs),
	.q_a(ram_do),

	// Port B used to be the QNICE peek path. After that was removed it was
	// retied to clk/cpu_a, so every DOS write collided with a same-address
	// port-B read and Vivado left the 1571 side flags undefined. Leave the
	// port disconnected; USE_B=0 drops the second process entirely.
	.clock_b(1'b0),
	.address_b(11'd0),
	.data_b(8'h00),
	.wren_b(1'b0),
	.q_b()
);

// 8 bytes scratch RAM at $4010-$4017 (1571CR only)

// wire [7:0] scrram_do;
// iecdrv_mem #(8,3) scrram
// (
// 	.clock_a(clk),
// 	.address_a(cpu_a[2:0]),
// 	.data_a(cpu_do),
// 	.wren_a(ena_r & ~cpu_rw & scrram_cs),

// 	.clock_b(clk),
// 	.address_b(cpu_a[2:0]),
// 	.q_b(scrram_do)
// );

// VIA1 1571-U9 (6522) signals

wire [7:0] via1_do;
wire [7:0] via1_cpu_do;
wire       via1_irq;
wire [7:0] via1_pa_o;
wire [7:0] via1_pa_oe;
wire       via1_ca2_o;
wire       via1_ca2_oe;
wire [7:0] via1_pb_o;
wire [7:0] via1_pb_oe;
// PA7 is BYTE READY as a level the 1571 DOS polls, not as an edge; byte_n_poll is the
// stretched version built further down. In 1541 mode drv_mode is zero and this bit is
// forced high, so the stretch is invisible there.
wire [7:0] via1_pa_i = (ext_en & ~|drv_mode ? par_data_in :
                        {byte_n_poll | ~|drv_mode, 6'h3F, ~tr00_sense})
                       & (via1_pa_o | ~via1_pa_oe);
wire [7:0] via1_pb_i = {~iec_atn_in, 2'(DRIVE), 2'b11, ~iec_clk_in, 1'b1, ~iec_data_in}
                       & (via1_pb_o | ~via1_pb_oe);
wire       via1_cb1_o;
wire       via1_cb1_oe;
wire       via1_cb2_o;
wire       via1_cb2_oe;

// XSim clears IFR bit 6 on a T1-high write as the 6522 specifies, but the Vivado
// implementation can retain a previously set bit: iecdrv_via6522 assigns both the
// complete irq_flags vector and its aliased timer_a_flag bit in one clocked process.
// Keep that proven, vendored VIA untouched and suppress only the stale polled value
// for the interval just loaded into T1. The real flag becomes visible again when the
// programmed one-shot expires, so a genuine EOI timeout still reaches $E9F2.
// Masking is confined to bit 6 of the polled value: the DOS enables only CA1 in the
// IER, so T1 never feeds irq_out and bit 7 still comes straight from the VIA.
wire       via1_t1h_wr = ena_f & ~cpu_rw & via1_cs & (cpu_a[3:0] == 4'h5);
wire       via1_t1l_wr = ena_f & ~cpu_rw & via1_cs &
                         ((cpu_a[3:0] == 4'h4) | (cpu_a[3:0] == 4'h6));
reg  [7:0] via1_t1_latch_low = 0;
reg [16:0] via1_t1_guard = 0;

always @(posedge clk) begin
	if (reset) begin
		via1_t1_latch_low <= 0;
		via1_t1_guard     <= 0;
	end
	else begin
		if (via1_t1l_wr) via1_t1_latch_low <= cpu_do;
		if (via1_t1h_wr)
			via1_t1_guard <= {1'b0, cpu_do, via1_t1_latch_low} + 1'd1;
		else if (ena_f && |via1_t1_guard)
			via1_t1_guard <= via1_t1_guard - 1'd1;
	end
end

assign via1_cpu_do = (cpu_rw && cpu_a[3:0] == 4'hD && |via1_t1_guard)
                   ? (via1_do & 8'hBF) : via1_do;

wire       fser_dir     = (via1_pa_o[1] | ~via1_pa_oe[1]) & |drv_mode;
assign     side         = (via1_pa_o[2] | ~via1_pa_oe[2]) &  drv_mode[1];
wire       soe;
wire       via_accl_ctl = (via1_pa_o[5] | ~via1_pa_oe[5]) & |drv_mode;
// Match the original 1571: VIA1 PA5 is the sole 1/2 MHz selector. The DOS keeps
// it low in 1541 compatibility mode and raises it in native 1571 mode.
wire       accl_ctl     = via_accl_ctl;

assign     iec_data_out = ~(via1_pb_o[1] | ~via1_pb_oe[1]) & ~((via1_pb_o[4] | ~via1_pb_oe[4]) ^ ~iec_atn_in) & (~fser_dir | cia_sp_out);
assign     iec_clk_out  = ~(via1_pb_o[3] | ~via1_pb_oe[3]);

assign     par_stb_out  = |drv_mode ?  cia_pc_n               : (via1_ca2_o | ~via1_ca2_oe);
assign     par_data_out = |drv_mode ? (cia_pb_o | ~cia_pb_oe) : (via1_pa_o  | ~via1_pa_oe);

iecdrv_via6522 via1
(
	.clock(clk),
	.rising(ena_r),
	.falling(ena_f),
	.reset(reset),

	.addr(cpu_a[3:0]),
	.wen(~cpu_rw & via1_cs),
	.ren(cpu_rw & via1_cs),
	.data_in(cpu_do),
	.data_out(via1_do),

	.port_a_o(via1_pa_o),
	.port_a_t(via1_pa_oe),                     
	.port_a_i(via1_pa_i),

	.port_b_o(via1_pb_o),
	.port_b_t(via1_pb_oe),
	.port_b_i(via1_pb_i),

	.ca1_i(~iec_atn_in),

	.ca2_o(via1_ca2_o),
	.ca2_t(via1_ca2_oe),
	.ca2_i(wps_n & (via1_ca2_o | ~via1_ca2_oe)),

	.cb1_o(via1_cb1_o),
	.cb1_t(via1_cb1_oe),
	.cb1_i(((ext_en & ~&drv_mode) ? par_stb_in : 1'b1) & (via1_cb1_o | ~via1_cb1_oe)),

	.cb2_o(via1_cb2_o),
	.cb2_t(via1_cb2_oe),
	.cb2_i(via1_cb2_o | ~via1_cb2_oe),

	.irq(via1_irq)
);

// VIA2 1571-U4 (6522) signals

wire [7:0] via2_do;
wire       via2_irq;
wire [7:0] via2_pa_o;
wire [7:0] via2_pa_oe;
wire       via2_ca2_o;
wire       via2_ca2_oe;
wire [7:0] via2_pb_o;
wire [7:0] via2_pb_oe;
wire       via2_cb1_o;
wire       via2_cb1_oe;
wire       via2_cb2_o;
wire       via2_cb2_oe;

wire       ted    = via2_cs    | ~accl[2];
assign     soe    = via2_ca2_o | ~via2_ca2_oe;
wire [7:0] gcr_di = via2_pa_o  | ~via2_pa_oe;
assign sector_gcr_din = gcr_di;
assign gcr_do = sector_gcr_enable ? sector_gcr_dout : h156_do;
assign sync_n = sector_gcr_enable ? sector_gcr_sync_n : h156_sync_n;
// c157x_h156 applies SOE internally, the sector-image GCR module does not. Without this
// gate the CPU sees byte-ready pulses while DOS has byte-ready disabled, exactly as in
// c1541_logic.sv of the reference 1541, where cpu_so_n = byte_n | ~soe.
wire sector_byte_n_raw = sector_gcr_byte_n | ~soe;

// BYTE READY has two consumers with opposite requirements, and one signal cannot serve
// both. The CPU's SO pin is edge-triggered: the disk controller's write loops at $F58E,
// $F5AB and $F5C1 clear V and wait for one falling edge per byte, and none of them
// touches VIA2 in between, so anything that stretches the line costs them edges. VIA1
// PA7 is the opposite: the 1571 DOS polls it with the seven-cycle loop at $9456, which
// at 2 MHz only samples every 3.5 us and steps straight over the bare pulse c1541_gcr
// emits.
//
// So drive them separately. The SO pin and VIA2 CA1 keep the raw pulse, exactly as the
// 1541 has always seen it, which leaves every write loop untouched at either clock. Only
// the polled level is stretched, and only where it is read: in 1541 mode drv_mode is zero
// and PA7 reads back as a constant 1 regardless.
//
// Stretch for the polled level: hold for a quarter of the measured byte cell, which is
// 6.5 to 8 us depending on density zone. That is comfortably longer than the 3.5 us poll
// period, so the loop cannot miss it, and it still releases well inside the cell. A byte
// cell is roughly 850 clk cycles at the C128 main clock, so twelve bits hold one with
// room to spare; the counter saturates rather than wraps, capping the stretch at about
// one cell if the stream stops before the span completes.
reg [11:0] byte_span = 0;               // clk cycles since the previous byte-ready
reg  [9:0] byte_hold = 0;               // cycles left to stretch the polled level
reg        sector_byte_n_raw_r = 1;

always @(posedge clk) begin
	sector_byte_n_raw_r <= sector_byte_n_raw;

	if (reset) begin
		byte_span <= 0;
		byte_hold <= 0;
	end
	else if (~sector_byte_n_raw & sector_byte_n_raw_r) begin
		byte_hold <= byte_span[11:2];
		byte_span <= 0;
	end
	else begin
		if (~&byte_span) byte_span <= byte_span + 1'b1;
		// Reading VIA2 means the CPU has taken the byte, so drop the level early,
		// the same way c157x_h156 does for the MFM path.
		if (byte_hold) byte_hold <= (ted | ~soe) ? 10'd0 : byte_hold - 1'b1;
	end
end

wire sector_byte_n_held = sector_byte_n_raw & ~|byte_hold;

assign byte_n      = sector_gcr_enable ? sector_byte_n_raw  : h156_byte_n;
assign byte_n_poll = sector_gcr_enable ? sector_byte_n_held : h156_byte_n;

assign     stp    = via2_pb_o[1:0] | ~via2_pb_oe[1:0];
assign     mtr    = via2_pb_o[2]   | ~via2_pb_oe[2];
assign     act    = via2_pb_o[3]   | ~via2_pb_oe[3];
assign     freq   = via2_pb_o[6:5] | ~via2_pb_oe[6:5];
assign     mode   = via2_cb2_o     | ~via2_cb2_oe;

iecdrv_via6522 via2
(
	.clock(clk),
	.rising(ena_r),
	.falling(ena_f),
	.reset(reset),

	.addr(cpu_a[3:0]),
	.wen(~cpu_rw & via2_cs),
	.ren(cpu_rw & via2_cs),
	.data_in(cpu_do),
	.data_out(via2_do),

	.port_a_o(via2_pa_o),
	.port_a_t(via2_pa_oe),
	.port_a_i(gcr_do & gcr_di),

	.port_b_o(via2_pb_o),
	.port_b_t(via2_pb_oe),
	.port_b_i({sync_n, 2'b11, wps_n, 4'b1111} & (via2_pb_o | ~via2_pb_oe)),

	.ca1_i(byte_n),

	.ca2_o(via2_ca2_o),
	.ca2_t(via2_ca2_oe),
	.ca2_i(via2_ca2_o | ~via2_ca2_oe),

	.cb1_o(via2_cb1_o),
	.cb1_t(via2_cb1_oe),
	.cb1_i(via2_cb1_o | ~via2_cb1_oe),

	.cb2_o(via2_cb2_o),
	.cb2_t(via2_cb2_oe),
	.cb2_i(via2_cb2_o | ~via2_cb2_oe),

	.irq(via2_irq)
);

// CIA 1571-U20 (6526/5710) signals

wire [7:0] cia_do;
wire       cia_irq_n;
wire [7:0] cia_pa_o, cia_pa_oe, cia_pb_o, cia_pb_oe;
wire       cia_pc_n;

wire       cia_sp_out;
wire       cia_cnt_out;

assign     iec_fclk_out = ~fser_dir | cia_cnt_out;

mos6526_8520 cia
(
	.res_n(~reset & |drv_mode),
	.clk(clk),
	.mode(&drv_mode ? 2'b11 : 2'b00),
	.phi2_p(ena_f),
	.phi2_n(ena_r),
	.cs_n(~cia_cs),
	.rw(cpu_rw),

	.rs(cpu_a[3:0]),
	.db_in(cpu_do),
	.db_out(cia_do),

	.pa_out(cia_pa_o),
	.pa_oe(cia_pa_oe),
	.pa_in(cia_pa_o | ~cia_pa_oe),

	.pb_out(cia_pb_o),
	.pb_oe(cia_pb_oe),
	.pb_in((ext_en ? par_data_in : 8'hff) & (cia_pb_o | ~cia_pb_oe)),

	.pc_n(cia_pc_n),

	.flag_n(ext_en ? par_stb_in : 1'b1),

	.tod(1'b1),

	.sp_in(fser_dir | iec_data_in),
	.sp_out(cia_sp_out),

	.cnt_in(fser_dir | iec_fclk_in),
	.cnt_out(cia_cnt_out),

	.irq_n(cia_irq_n)
);

// Head signals mux

assign     ht = gcr_ht | (mfm_ht & |drv_mode);
assign     hinit = gcr_hinit | (mfm_hinit & |drv_mode);

// 64H156 1571-U6 signals

c157x_h156 c157x_h156
(
	.clk(clk),
	.reset(reset),
	.enable(drive_enable & ~sector_gcr_enable),
	.mhz1_2(accl[1]),
	
	.hinit(gcr_hinit),
	.hclk(hclk),
	.ht(gcr_ht),
	.hf(hf),

	.mode(mode),
	.soe(soe),
	.ted(ted),
	.sync_n(h156_sync_n),
	.byte_n(h156_byte_n),

	.dout(h156_do),
	.din(gcr_di)
);

// FDC 1571-U11 (WD1770) signals

wire [7:0] wd_do;
wire       mfm_ht, mfm_hinit;
wire       mfm_wgate;

assign     wgate = mfm_wgate & |drv_mode;

c157x_fdc1772 #(.MODEL(0)) c157x_fdc1772
(
	.clkcpu(clk),
	.clk8m_en(wd_ce),

	.floppy_reset(~reset & |drv_mode),
	.floppy_present(disk_present),
	// .floppy_side(side),
	.floppy_motor(mtr),
	.floppy_index(~index_sense),
	.floppy_wprot(~(wps_n & img_mfm)),
	.floppy_track00(1),

	.hinit(mfm_hinit),
	.hclk(hclk),
	.ht(mfm_ht),
	.hf(hf),
	.wgate(mfm_wgate),
	.busy(fdc_busy),

	.cpu_addr(cpu_a[1:0]),
	.cpu_sel(wd_cs),
	.cpu_rw(cpu_rw | ~ena_r),
	.cpu_din(cpu_do),
	.cpu_dout(wd_do)
);

endmodule
