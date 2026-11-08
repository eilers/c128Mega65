; Control target test for the C128 MEGA65 core's command interface, and for a real Ultimate.
;
; Runs the commands of target 4: identification, hardware and drive information, drive A
; and B switched off and on (drive B checked on the IEC bus), and each command's answer to
; wrong parameters. It leaves things as they were: the drives' power goes back, nothing is
; erased, and SAVE_REU runs only while the REU is off. At the end a key runs FREEZE or
; REBOOT. Every line ends in PASS or FAIL; what differs between machines is shown below it.
; Build with make_image.py: UCICTRL for C128 mode, UCICTRL64 (-D C64) for C64 mode.

.include "ucilib.inc"

SETBNK  = $FF68         ; C128 only: the banks of the data and the file name
SETLFS  = $FFBA
SETNAM  = $FFBD
OPEN    = $FFC0
CLOSE   = $FFC3
READST  = $FFB7
GETIN   = $FFE4
BUS     = $DD00         ; the serial bus: bit 7 DATA, 0 while a device holds it

; Start a command for target 4 in cmd_buf
.macro CTRL c
        lda #c
        jsr ctrl_cmd
.endmacro

; Add the C string s to it
.macro ADD s
        lda #<s
        ldy #>s
        jsr add_str
.endmacro

; The command in cmd_buf, and a check of its reply's data (len bytes at want) and status
.macro ASK want, len, st
        jsr go
        EXPECT want, len, st
.endmacro

.segment "LOADADDR"
.ifdef C64
        .word $0801
.else
        .word $1C01
.endif

.segment "CODE"
        .word basic_end
        .word 10
.ifdef C64
        .byte $9E, "2061", 0    ; 10 SYS2061
.else
        .byte $9E, "7181", 0    ; 10 SYS7181
.endif
basic_end:
        .word 0

start:
        cld
        lda #0
        sta fails
        PRINT title
.ifdef BUSTEST                  ; just the bus check of device 9, for VICE
        lda #9
        sta b_addr
        PRINT t_b_there
        lda #0
        jsr on_bus
        PRINT t_b_gone
        lda #1
        jsr on_bus
        rts
.endif
        lda CMDQ
        cmp #$C9
        beq @found
        PRINT no_uci
        rts
@found: jsr release

        ; Who is there
        PRINT t_ident
        CTRL $01
        jsr go
        jsr want_data
        PRINT t_hw
        CTRL $28
        jsr go
        jsr want_data
        PRINT t_hw1
        CTRL $28
        lda #1
        jsr add_byte
        jsr go
        jsr want_counted_5
        PRINT t_hw2
        CTRL $28
        lda #2
        jsr add_byte
        ASK nothing, 0, st_invalid
        PRINT t_drv
        CTRL $29
        jsr go
        jsr want_counted_3
        lda #9                          ; drive B's address, from the information
        sta b_addr
        lda resp_buf
        cmp #2
        bcc :+
        lda resp_buf+5
        sta b_addr
:       PRINT t_ram
        CTRL $40
        jsr go
        lda #8                          ; 8 bytes, 00,OK
        cmp resp_len
        bne :+
        jsr is_ok
        bne :+
        jsr pass
        jsr show_hex
        jmp @powers
:       jsr fail
        jsr show_hex

        ; Drive A off and on, then as it was
@powers:
        PRINT t_pow_a
        CTRL $34
        jsr go
        jsr want_power
        sta a_was
        PRINT t_a_off
        CTRL $31
        ASK nothing, 0, st_ok
        PRINT t_a_is_off
        CTRL $34
        ASK s_off, 3, st_ok
        PRINT t_a_on
        CTRL $30
        ASK nothing, 0, st_ok
        PRINT t_a_is_on
        CTRL $34
        ASK s_on, 3, st_ok
        lda a_was
        bne :+
        CTRL $31
        jsr go

        ; Drive B off and on, and on the IEC bus at its address, then as it was
:       PRINT t_pow_b
        CTRL $35
        jsr go
        jsr want_power
        sta b_was
        PRINT t_b_off
        CTRL $33
        ASK nothing, 0, st_ok
        PRINT t_b_is_off
        CTRL $35
        ASK s_off, 3, st_ok
        PRINT t_b_gone
        lda #1
        jsr on_bus
        PRINT t_b_on
        CTRL $32
        ASK nothing, 0, st_ok
        PRINT t_b_is_on
        CTRL $35
        ASK s_on, 3, st_ok
        PRINT t_b_there
        lda #0
        jsr on_bus
        lda b_was
        bne :+
        CTRL $33
        jsr go

        ; Wrong parameters, and commands only other Ultimates know
:       PRINT t_track
        CTRL $11
        ASK nothing, 0, st_invalid
        PRINT t_ef
        CTRL $20
        ASK nothing, 0, st_invalid
        PRINT t_ef1
        CTRL $20
        lda #1
        jsr add_byte
        lda #0
        jsr add_byte
        jsr add_byte
        ASK nothing, 0, st_invalid
        PRINT t_ef0
        CTRL $20
        lda #0
        jsr add_byte
        ASK nothing, 0, st_invalid
        PRINT t_reu4
        CTRL $08
        lda #0
        jsr add_byte
        jsr add_byte
        ASK nothing, 0, st_invalid
        PRINT t_cfg
        CTRL $50
        ADD s_cfg
        lda #0                          ; his LOAD_CONFIG doesn't end the name itself
        jsr add_byte
        ASK nothing, 0, st_no_cfg
        PRINT t_cap
        CTRL $03
        ASK nothing, 0, st_ok
        ldx #0
@unknown:
        stx unk_at
        PRINT t_unk
        ldx unk_at
        lda unknowns,x
        pha
        jsr hex8
        lda #' '
        jsr CHROUT
        pla
        jsr ctrl_cmd
        ASK nothing, 0, st_unknown
        ldx unk_at
        inx
        cpx #unknowns_n
        bne @unknown

        ; The REU: LOAD_REU while it is off answers -2, and only then is SAVE_REU safe
        PRINT t_reu
        CTRL $08
        jsr add_3
        jsr go
        lda stat_buf
        cmp #'8'
        bne @reu_on
        lda stat_buf+1
        cmp #'4'
        bne @reu_on
        EXPECT no_reu, 4, st_reu_off
        PRINT t_save
        CTRL $09
        jsr add_3
        ASK no_reu, 4, st_reu_off
        jmp @summary
@reu_on:
        PRINT m_reu_on
        jsr show

@summary:
        lda fails
        bne @failed
        PRINT all_passed
        jmp @keys
@failed:
        PRINT some_failed
        lda fails
        jsr hex8
        lda #13
        jsr CHROUT

        ; FREEZE or REBOOT, if asked
@keys:  PRINT m_keys
@key:   jsr GETIN
        cmp #0
        beq @key
        cmp #'F'
        bne @not_f
        PRINT t_freeze
        CTRL $05
        ASK nothing, 0, st_none
        rts
@not_f: cmp #'R'
        bne @end
        PRINT t_reboot
        CTRL $06
        jsr go
@end:   rts

;-----------------------------------------------------------------------------------------
; Building and sending commands
;-----------------------------------------------------------------------------------------

ctrl_cmd:
        sta cmd_buf+1
        lda #4
        sta cmd_buf
        lda #2
        sta cmd_pos
        rts

add_byte:
        ldx cmd_pos
        sta cmd_buf,x
        inc cmd_pos
        rts

add_str:
        sta PTR2
        sty PTR2+1
        ldy #0
@l:     lda (PTR2),y
        beq @e
        jsr add_byte
        iny
        bne @l
@e:     rts

; Three bytes more: LOAD_REU and SAVE_REU take at least 5
add_3:  lda #0
        jsr add_byte
        jsr add_byte
        jmp add_byte

go:     lda #<cmd_buf
        sta PTR
        lda #>cmd_buf
        sta PTR+1
        lda cmd_pos
        sta cmd_len
        lda #0
        sta cmd_len+1
        jmp transact

;-----------------------------------------------------------------------------------------
; Checks
;-----------------------------------------------------------------------------------------

; 00,OK and some data, then the reply shown
want_data:
        jsr is_ok
        bne @bad
        lda resp_len
        ora resp_len+1
        beq @bad
        jsr pass
        jmp show
@bad:   jsr fail
        jmp show

; 00,OK and a count in the first byte, then that many entries of n bytes (want_counted_n),
; then the reply shown in hex
want_counted_5:
        lda #5
        bne want_counted
want_counted_3:
        lda #3
want_counted:
        sta entry
        jsr is_ok
        bne @bad
        lda resp_len+1
        bne @bad
        lda #1                          ; 1 + count * entry
        ldx resp_buf
        beq @cmp
@mul:   clc
        adc entry
        dex
        bne @mul
@cmp:   cmp resp_len
        bne @bad
        jsr pass
        jmp show_hex
@bad:   jsr fail
        jmp show_hex

; DISK_x_POWER: "on " or "off" and 00,OK; A = 1 if on
want_power:
        lda resp_len
        cmp #3
        bne @bad
        jsr is_ok
        bne @bad
        jsr pass
        jsr show
        lda resp_buf+1
        cmp #'n'
        beq @on
        lda #0
        rts
@on:    lda #1
        rts
@bad:   jsr fail
        jsr show
        lda #1
        rts

; Zero flag set if the status is 00,OK
is_ok:  ldx #0
@l:     lda st_ok,x
        cmp stat_buf,x
        bne @e
        inx
        cmp #0
        bne @l
@e:     rts

; The reply's data in hex (up to 16 bytes), and its status
show_hex:
        lda #' '
        jsr CHROUT
        ldx #0
@l:     cpx resp_len
        beq @st
        cpx #16
        beq @st
        stx hex_at
        lda resp_buf,x
        jsr hex8
        lda #' '
        jsr CHROUT
        ldx hex_at
        inx
        bne @l
@st:    jmp show_status

;-----------------------------------------------------------------------------------------
; Drive B on the IEC bus: OPEN of a buffer ("#"), whose name goes out at once, so it fails
; with DEVICE NOT PRESENT when nothing answers (a C64's CHKIN would wait for ever instead).
; A = 1: it must be gone, 0: it must answer. A drive just switched on starts from reset and
; holds DATA meanwhile, so it tries for about 4 seconds and says how many retries it took.
;-----------------------------------------------------------------------------------------

on_bus: sta want_gone
        lda #0
        sta tries
@try:   jsr answers
        cmp #2
        beq @again                      ; a drive is starting
        eor want_gone
        bne @good                       ; answers xor gone: as wanted
@again: lda tries
        cmp #12
        beq @bad
        inc tries
        jsr wait_third
        jmp @try
@good:  jsr pass
        lda tries
        beq @e
        PRINT m_retries
        lda tries
        jsr hex8
        lda #13
        jmp CHROUT
@e:     rts
@bad:   jmp fail

; A = 1 if drive B answers at b_addr, 0 if not, 2 while a drive holds DATA: a 1541 or 1571
; holds it from its reset (about a second) until its DOS listens for ATN, and an OPEN then
; waits for ever in the KERNAL. A failed OPEN leaves its file in the KERNAL's table, so it
; is closed either way.
answers:
        bit BUS
        bmi @free                       ; DATA free
        lda #2
        rts
@free:
.ifndef C64
        lda #0
        tax
        jsr SETBNK
.endif
        lda #1
        ldx #<s_hash
        ldy #>s_hash
        jsr SETNAM
        lda #2
        ldx b_addr
        ldy #2
        jsr SETLFS
        jsr OPEN
        lda #0
        rol                             ; 1: OPEN failed
        sta gone
        jsr READST
        and #$80                        ; DEVICE NOT PRESENT
        ora gone
        sta gone
        lda #2
        jsr CLOSE
        lda gone
        beq @yes
        lda #0
        rts
@yes:   lda #1
        rts

; About a third of a second at 1 MHz
wait_third:
        ldx #0
        ldy #0
@l:     dex
        bne @l
        dey
        bne @l
        rts

;-----------------------------------------------------------------------------------------
; Data
;-----------------------------------------------------------------------------------------

title:      .byte 147, "UCI CONTROL TEST", 13, 13, 0
no_uci:     .byte "NO COMMAND INTERFACE AT $DF1C.", 13, "SWITCH IT ON IN THE MENU.", 13, 0
t_ident:    .byte "IDENTIFY ", 0
t_hw:       .byte "GET_HWINFO: THE PRODUCT ", 0
t_hw1:      .byte "GET_HWINFO 1: ITS SIDS ", 0
t_hw2:      .byte "GET_HWINFO 2 ", 0
t_drv:      .byte "GET_DRVINFO ", 0
t_ram:      .byte "GET_RAMDISKINFO ", 0
t_pow_a:    .byte "DRIVE A POWER ", 0
t_a_off:    .byte "DISABLE DRIVE A ", 0
t_a_is_off: .byte "DRIVE A: OFF ", 0
t_a_on:     .byte "ENABLE DRIVE A ", 0
t_a_is_on:  .byte "DRIVE A: ON ", 0
t_pow_b:    .byte "DRIVE B POWER ", 0
t_b_off:    .byte "DISABLE DRIVE B ", 0
t_b_is_off: .byte "DRIVE B: OFF ", 0
t_b_gone:   .byte "DRIVE B GONE FROM THE BUS ", 0
t_b_on:     .byte "ENABLE DRIVE B ", 0
t_b_is_on:  .byte "DRIVE B: ON ", 0
t_b_there:  .byte "DRIVE B ON THE BUS ", 0
t_track:    .byte "DECODE_TRACK, NO PARAMS ", 0
t_ef:       .byte "EASYFLASH, NO PARAMS ", 0
t_ef1:      .byte "EASYFLASH 1 ", 0
t_ef0:      .byte "EASYFLASH ERASE, NO BANK ", 0
t_reu4:     .byte "LOAD_REU OF 4 BYTES ", 0
t_cfg:      .byte "LOAD_CONFIG, NO FILE ", 0
t_cap:      .byte "FINISH_CAPTURE ", 0
t_unk:      .byte "UNKNOWN $", 0
t_reu:      .byte "LOAD_REU, REU OFF ", 0
t_save:     .byte "SAVE_REU, REU OFF ", 0
t_freeze:   .byte "FREEZE ", 0
t_reboot:   .byte "REBOOT", 13, 0
m_reu_on:   .byte "SKIPPED: THE REU IS ON", 13, 0
m_retries:  .byte " AFTER RETRIES: $", 0
m_keys:     .byte 13, "F: FREEZE, R: REBOOT, ELSE END", 13, 0
all_passed: .byte 13, "ALL PASSED", 13, 0
some_failed: .byte 13, "FAILED: $", 0

; READ_RTC, the Ultimate 64's GET_PALETTE, ENCODE_TRACK, and one nobody has
unknowns:   .byte $02, $51, $12, $7F
unknowns_n = * - unknowns

st_ok:      .byte "00,OK", 0
st_unknown: .byte "21,UNKNOWN COMMAND", 0
st_invalid: .byte "81,INVALID PARAMS", 0
st_reu_off: .byte "84,REU NOT ENABLED", 0
st_no_cfg:  .byte "88,CANNOT OPEN CONFIG FILE", 0
st_none:    .byte 0
s_on:       .byte "on "
s_off:      .byte "off"
s_cfg:      .byte "/NONE/UCICTRL.CFG", 0
s_hash:     .byte "#"
no_reu:     .byte $FE, $FF, $FF, $FF
nothing:    .byte 0

.include "ucilib.s"

cmd_pos:    .res 1
entry:      .res 1
hex_at:     .res 1
unk_at:     .res 1
b_addr:     .res 1
a_was:      .res 1
b_was:      .res 1
want_gone:  .res 1
gone:       .res 1
tries:      .res 1
cmd_buf:    .res 64
