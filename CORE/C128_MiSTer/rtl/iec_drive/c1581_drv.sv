//-------------------------------------------------------------------------------
//
// Commodore 1581 implementation
// (C) 2021 Alexey Melnikov
//
// Input clock 16MHz
//
//-------------------------------------------------------------------------------

module c1581_drv
(
   //clk ports
   input         clk,
   input         reset,

   input         ce,
   input         wd_ce,
   input         ph2_r,
   input         ph2_f,

   input         img_mounted,
   input         img_readonly,
   input  [31:0] img_size,

   input   [1:0] drive_num,
   output        act_led,
   output        pwr_led,

   input         iec_atn_i,
   input         iec_data_i,
   input         iec_clk_i,
   input         iec_fclk_i,
   output        iec_data_o,
   output        iec_clk_o,
   output        iec_fclk_o,

   // parallel bus
   input   [7:0] par_data_i,
   input         par_stb_i,
   output  [7:0] par_data_o,
   output        par_stb_o,

   output [14:0] rom_addr,
   input   [7:0] rom_data,

   //clk_sys ports
   input         clk_sys,

   output [31:0] sd_lba,
   output        sd_rd,
   output        sd_wr,
   input         sd_ack,
   input   [8:0] sd_buff_addr,
   input   [7:0] sd_buff_dout,
   output  [7:0] sd_buff_din,
   input         sd_buff_wr,
   output  [7:0] out_track,
   output        out_we,

   // MEGA65 internal floppy: passed straight through to c1581_fdc1772,
   // see there. Inert while phys_mode is 0.
   input         phys_mode,
   output        phys_active,
   output        phys_cia_motor_on,
   output        phys_cia_side,
   output        phys_step_req_tgl,
   output        phys_step_outward,
   output        phys_rd_req_tgl,
   output  [2:0] phys_rd_op,
   output  [7:0] phys_rd_track,
   output        phys_rd_side,
   output  [7:0] phys_rd_sector,
   output        phys_rd_cancel_tgl,
   output  [1:0] phys_rd_seq,
   input         phys_step_ack_tgl,
   input         phys_rd_done_tgl,
   input   [1:0] phys_rd_done_seq,
   input   [4:0] phys_rd_result,
   input         phys_rd_crc_err,
   input         phys_rd_rnf,
   input         phys_rd_deleted,
   input   [7:0] phys_rd_c,
   output        phys_byte_rd_en,
   input   [7:0] phys_byte_data,
   input         phys_byte_empty,
   input         phys_media_ready,
   input         phys_index,
   input         phys_track0,
   input         phys_wprot,
   input         phys_change,
   input         phys_motor_on,
   input         phys_head_settled,
   output        phys_wr_deleted,
   output        phys_wr_precomp,
   output        phys_wr_byte_wr,
   output  [7:0] phys_wr_byte_data,
   input         phys_wr_fifo_full,
   input         phys_wr_feed,
   input         phys_wr_lost,
   input         phys_wr_busy,
   output        phys_dbg_lost_tgl,
   output        phys_dbg_drain_tgl,
   output        phys_dbg_staledone_tgl,
   output        phys_dbg_busycmd_tgl,
   output        phys_dbg_fin_tgl,
   output [10:0] phys_dbg_pres_cnt,
   output        phys_dbg_wr_fin_tgl,
   output        phys_dbg_wr_wp_tgl,
   output        phys_dbg_wr_drop_tgl
);

assign rom_addr = cpu_a[14:0];

reg wps_n = 0;
always @(posedge clk) begin
   reg prev_mounted;
   prev_mounted <= img_mounted;
   if (~prev_mounted & img_mounted) wps_n <= ~img_readonly;
end

// MEGA65 internal floppy: the CIA's disk change (PA7) and write protect (PB6)
// senses follow the real drive. Slow levels from the 50 MHz controller.
// phys_change=1: disk changed or not yet stepped on; phys_wprot=1: protected.
wire phys_change_s, phys_wprot_s;
iecdrv_sync phys_chg_cia_sync(clk, phys_change, phys_change_s);
iecdrv_sync phys_wp_cia_sync (clk, phys_wprot,  phys_wprot_s);

//same decoder as on real HW
wire [2:0] ls193 = cpu_a[15:13];
wire ram_cs      = ls193 == 0;
wire via_cs      = ls193 == 1;
wire cia_cs      = ls193 == 2;
wire wd_cs       = ls193 == 3;
wire rom_cs      = cpu_a[15];

wire  [7:0] cpu_di =
   !cpu_rw ? cpu_do :
    ram_cs ? ram_do :
    via_cs ? via_do :
    cia_cs ? cia_do :
    wd_cs  ? wd_do  :
    rom_cs ? rom_data :
    8'hFF;

wire [23:0] cpu_a;
wire  [7:0] cpu_do;
wire        cpu_rw;

// MEGA65 port: declared up here because a port connection below names them before
// their original declaration site, which made Vivado create 1-bit implicit nets.
// track_fdc is the one that hurt: truncating it to a single bit left the drive
// unable to report any track number but 0 or 1.
wire        cia_irq_n;
wire        via_irq;
wire  [7:0] track_fdc;

T65 cpu
(
   .clk(clk),
   .enable(ph2_r),
   .mode(2'b00),
   .res_n(~reset),
   .irq_n(cia_irq_n & ~via_irq),
   .r_w_n(cpu_rw),
   .A(cpu_a),
   .DI(cpu_di),
   .DO(cpu_do)
);

wire [7:0] ram_do;
iecdrv_mem #(.DATAWIDTH(8), .ADDRWIDTH(13), .WRITE_B(0)) ram
(
   .clock_a(clk),
   .address_a(cpu_a[12:0]),
   .data_a(cpu_do),
   .wren_a(ph2_f & ~cpu_rw & ram_cs),

   // MEGA65 port: port B only ever reads. Leaving wren_b/data_b unconnected makes Vivado
   // infer a true dual-port RAM whose two write ports share one address, reported as
   // [Synth 8-5796], with undefined collision behaviour. Tie them off.
   .clock_b(clk),
   .address_b(cpu_a[12:0]),
   .data_b(8'h00),
   .wren_b(1'b0),
   .q_b(ram_do)
);

wire [7:0] cia_do;

assign     act_led    =  pa_out[6];
assign     pwr_led    =  pa_out[5];
wire       motor_n    =  pa_out[2];
wire       side       =  pa_out[0];
// MEGA65: CIA PA1 is /RDY. Feeding mechanical "at speed" (floppy_ready)
// made the 1581 DOS see not-ready while the motor was off, so it never
// started the motor (LED cyan, LOAD 74). MAME leaves this bit disconnected
// for the same reason ("drive not ready if ready_r() is connected to CIA").
// Report ready whenever the FDC has latched a disk; spin-up still gates
// the FDC header path via floppy_ready.
// In physical mode disk_present is the mechanism's RDY, see c1581_fdc1772.
wire [7:0] pa_in      = {phys_mode ? ~phys_change_s : disk_chng_n, 2'b11, drive_num, 1'b1, ~disk_present, 1'b1};

wire       fast_dir   =  pb_out[5];
assign     iec_clk_o  = ~pb_out[3];
assign     iec_data_o = ~pb_out[1] & ~(pb_out[4] & ~iec_atn_i) & (~fast_dir | sp_out);
assign     iec_fclk_o = ~fast_dir | cnt_out;
wire [7:0] pb_in      = {~iec_atn_i, phys_mode ? ~phys_wprot_s : wps_n, 3'b111, ~iec_clk_i, 1'b1, ~iec_data_i};

wire [7:0] pa_out;
wire [7:0] pb_out;

wire       sp_out;
wire       cnt_out;

mos6526_8520 cia
(
   .clk(clk),
   .mode(2'b10),
   .phi2_p(ph2_r),
   .phi2_n(ph2_f),
   .res_n(~reset),
   .cs_n(~cia_cs),
   .rw(cpu_rw),

   .rs(cpu_a[3:0]),
   .db_in(cpu_do),
   .db_out(cia_do),

   .pa_in(pa_in & pa_out),
   .pa_out(pa_out),
   .pb_in(pb_in & pb_out),
   .pb_out(pb_out),

   .flag_n(iec_atn_i),

   .tod(ph2_f),

   .sp_in(fast_dir | iec_data_i),
   .sp_out(sp_out),

   .cnt_in(fast_dir | iec_fclk_i),
   .cnt_out(cnt_out),

   .irq_n(cia_irq_n)
);


wire [7:0] via_do;
wire [7:0] via_pa_o;
wire [7:0] via_pa_oe;
wire       via_ca2_o;
wire       via_ca2_oe;
wire [7:0] via_pb_o;
wire [7:0] via_pb_oe;
wire       via_cb1_o;
wire       via_cb1_oe;
wire       via_cb2_o;
wire       via_cb2_oe;

assign     par_stb_o  = via_ca2_o | ~via_ca2_oe;
assign     par_data_o = via_pa_o  | ~via_pa_oe;

iecdrv_via6522 via
(
   .clock(clk),
   .rising(ph2_f),
   .falling(ph2_r),
   .reset(reset),

   .addr(cpu_a[3:0]),
   .wen(~cpu_rw & via_cs),
   .ren(cpu_rw & via_cs),
   .data_in(cpu_do),
   .data_out(via_do),

   .port_a_o(via_pa_o),
   .port_a_t(via_pa_oe),
   .port_a_i(par_data_i & (via_pa_o  | ~via_pa_oe)),

   .port_b_o(via_pb_o),
   .port_b_t(via_pb_oe),
   .port_b_i(via_pb_o | ~via_pb_oe),

   .ca1_i(1'b1),

   .ca2_o(via_ca2_o),
   .ca2_t(via_ca2_oe),
   .ca2_i(via_ca2_o | ~via_ca2_oe),

   .cb1_o(via_cb1_o),
   .cb1_t(via_cb1_oe),
   .cb1_i(par_stb_i & (via_cb1_o | ~via_cb1_oe)),

   .cb2_o(via_cb2_o),
   .cb2_t(via_cb2_oe),
   .cb2_i(via_cb2_o | ~via_cb2_oe),

   .irq(via_irq)
);


// MEGA65 internal floppy: switching between image and internal floppy is a
// disk change too, so the DOS never keeps the other medium's BAM.
reg disk_chng_n;
reg phys_modeD = 0;
always @(posedge clk) begin
   phys_modeD <= phys_mode;
   if(img_mounted | reset | (phys_modeD ^ phys_mode)) disk_chng_n <=0;
   if(floppy_step) disk_chng_n <=1;
end

wire       floppy_step;
wire [7:0] wd_do;

wire floppy_ready;
wire disk_present;

c1581_fdc1772 #(.IMG_TYPE(1), .EXT_MOTOR(1), .FD_NUM(1)) fdc
(
   .clkcpu(clk),
   .clk_sys(clk_sys),
   .clk8m_en(wd_ce),

   .floppy_drive(1'b0),
   .floppy_side(~side),
   .floppy_reset(~reset),
   .floppy_step(floppy_step),
   .floppy_motor(~motor_n),
   .floppy_ready(floppy_ready),

   .cpu_addr(cpu_a[1:0]),
   .cpu_sel(wd_cs),
   .cpu_rw(cpu_rw | ~ph2_f),
   .cpu_din(cpu_do),
   .cpu_dout(wd_do),

   .img_wp(~wps_n),

   .img_mounted(img_mounted),
   .img_size(img_size),
   .sd_lba(sd_lba),
   .sd_rd(sd_rd),
   .sd_wr(sd_wr),
   .sd_ack(sd_ack),
   .sd_buff_addr(sd_buff_addr),
   .sd_dout(sd_buff_dout),
   .sd_din(sd_buff_din),
   .sd_dout_strobe(sd_buff_wr),
   .out_track(track_fdc),
   .out_we(out_we),

   .disk_present(disk_present),

   .phys_mode(phys_mode),
   .phys_active(phys_active),
   .phys_cia_motor_on(phys_cia_motor_on),
   .phys_cia_side(phys_cia_side),
   .phys_step_req_tgl(phys_step_req_tgl),
   .phys_step_outward(phys_step_outward),
   .phys_rd_req_tgl(phys_rd_req_tgl),
   .phys_rd_op(phys_rd_op),
   .phys_rd_track(phys_rd_track),
   .phys_rd_side(phys_rd_side),
   .phys_rd_sector(phys_rd_sector),
   .phys_rd_cancel_tgl(phys_rd_cancel_tgl),
   .phys_rd_seq(phys_rd_seq),
   .phys_step_ack_tgl(phys_step_ack_tgl),
   .phys_rd_done_tgl(phys_rd_done_tgl),
   .phys_rd_done_seq(phys_rd_done_seq),
   .phys_rd_result(phys_rd_result),
   .phys_rd_crc_err(phys_rd_crc_err),
   .phys_rd_rnf(phys_rd_rnf),
   .phys_rd_deleted(phys_rd_deleted),
   .phys_rd_c(phys_rd_c),
   .phys_byte_rd_en(phys_byte_rd_en),
   .phys_byte_data(phys_byte_data),
   .phys_byte_empty(phys_byte_empty),
   .phys_media_ready(phys_media_ready),
   .phys_index(phys_index),
   .phys_track0(phys_track0),
   .phys_wprot(phys_wprot),
   .phys_change(phys_change),
   .phys_motor_on(phys_motor_on),
   .phys_head_settled(phys_head_settled),
   .phys_wr_deleted(phys_wr_deleted),
   .phys_wr_precomp(phys_wr_precomp),
   .phys_wr_byte_wr(phys_wr_byte_wr),
   .phys_wr_byte_data(phys_wr_byte_data),
   .phys_wr_fifo_full(phys_wr_fifo_full),
   .phys_wr_feed(phys_wr_feed),
   .phys_wr_lost(phys_wr_lost),
   .phys_wr_busy(phys_wr_busy),
   .phys_dbg_lost_tgl(phys_dbg_lost_tgl),
   .phys_dbg_drain_tgl(phys_dbg_drain_tgl),
   .phys_dbg_staledone_tgl(phys_dbg_staledone_tgl),
   .phys_dbg_busycmd_tgl(phys_dbg_busycmd_tgl),
   .phys_dbg_fin_tgl(phys_dbg_fin_tgl),
   .phys_dbg_pres_cnt(phys_dbg_pres_cnt),
   .phys_dbg_wr_fin_tgl(phys_dbg_wr_fin_tgl),
   .phys_dbg_wr_wp_tgl(phys_dbg_wr_wp_tgl),
   .phys_dbg_wr_drop_tgl(phys_dbg_wr_drop_tgl)
);

assign out_track = {track_fdc[6:0], 1'b0};

endmodule
