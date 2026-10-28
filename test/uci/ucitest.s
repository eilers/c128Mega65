; Ultimate Command Interface test for the C128 MEGA65 core, and for a real Ultimate.
;
; Talks to the command interface at $DF1C-$DF1F the way a program does, runs the
; transport checks of Gideon's tests/e2e/io/command_interface/uci_targets_test.py, and
; prints each reply, so a run on an Ultimate II+ and one on the core can be compared.
; Build with make_image.py: UCITEST for C128 mode, UCITEST64 (-D C64) for C64 mode.

.include "ucilib.inc"


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

        lda CMDQ
        cmp #$C9
        beq found
        PRINT no_uci
        jsr hex8
        PRINT no_uci2
        rts
found:
        jsr release

        ; 1. an empty command: an empty reply, from no target
        PRINT t_empty
        CMD cmd_echo, 0
        jsr transact
        EXPECT nothing, 0, nothing

        ; 2. the DOS on both targets: shown, its name differs between firmwares (MEGA128
        ; DOS, ULTIMATE-II DOS); target 2 must give target 1's
        PRINT t_dos1
        CMD cmd_dos1_id, 2
        jsr transact
        jsr show
        EXPECT resp_buf, 0, st_ok
        jsr keep_id
        PRINT t_dos2
        CMD cmd_dos2_id, 2
        jsr transact
        jsr show
        jsr expect_id

        ; 3. the control target: shown, its name differs between firmwares
        PRINT t_ctrl
        CMD cmd_ctrl_id, 2
        jsr transact
        jsr show
        EXPECT resp_buf, 0, st_ok_any

        ; 4. a target nobody registered, and a command nobody knows
        PRINT t_none
        CMD cmd_none_id, 2
        jsr transact
        EXPECT no_target, 9, st_ok
        PRINT t_unknown
        CMD cmd_unknown, 2
        jsr transact
        EXPECT nothing, 0, st_unknown

        ; 5. ECHO of 895 bytes, the most the command queue holds
        PRINT t_echo
        jsr fill_echo
        CMD cmd_buf, 895
        jsr transact
        EXPECT cmd_buf, 895, st_ok

        ; 6. no reply: straight back to idle
        PRINT t_noreply
        CMD cmd_noreply, 2
        jsr push
        jsr wait_idle
        jsr result

        ; 7. a push while a reply waits: ERROR, CLR_ERR, and the reply survives
        PRINT t_error
        CMD cmd_dos1_id, 2
        jsr push
        jsr wait_reply
        lda #C_PUSH
        sta CTRL
        lda CTRL
        and #S_ERROR
        beq @no_error
        lda #C_CLR
        sta CTRL
        lda CTRL
        and #S_ERROR
        bne @no_error
        jsr read_reply
        lda #C_ACC
        sta CTRL
        jsr expect_id
        jmp @error_done
@no_error:
        jsr fail
        jsr release
@error_done:

        ; 8. ABORT ends a waiting reply
        PRINT t_abort
        CMD cmd_dos1_id, 2
        jsr push
        jsr wait_reply
        lda #C_ABORT
        sta CTRL
        jsr wait_idle
        jsr result

        ; 9. the IRQ bit: $DF1D reads $49 with the reply, $C9 once it is read
        PRINT t_irq
        sei
        CMD cmd_dos1_id, 2
        jsr push_irq
        jsr wait_reply
        ldx CMDQ
        lda RESP
        ldy CMDQ
        lda #C_ACC
        sta CTRL
        cli
        cpx #$49
        bne @irq_bad
        cpy #$C9
        bne @irq_bad
        jsr pass
        jmp @irq_done
@irq_bad:
        stx got
        sty got+1
        jsr fail
        PRINT m_got
        lda got
        jsr hex8
        lda #' '
        jsr CHROUT
        lda got+1
        jsr hex8
        lda #13
        jsr CHROUT
@irq_done:

        ; How long a reply takes: polls of $DF1C, about 10 us each at 1 MHz
        PRINT t_time
        CMD cmd_dos1_id, 2
        jsr transact
        PRINT m_polls
        lda rpolls+2
        jsr hex8
        lda rpolls+1
        jsr hex8
        lda rpolls
        jsr hex8
        lda #13
        jsr CHROUT

        lda fails
        bne @failed
        PRINT all_passed
        rts
@failed:
        PRINT some_failed
        lda fails
        jsr hex8
        lda #13
        jmp CHROUT

;-----------------------------------------------------------------------------------------
; This test's own checks
;-----------------------------------------------------------------------------------------

; Keep the reply (DOS 1's name) in id_buf, and later expect it again, with status 00,OK
keep_id:
        ldx resp_len
        lda resp_len+1
        beq :+
        ldx #255
:       stx id_len
        dex
        cpx #$FF
        beq @e
@l:     lda resp_buf,x
        sta id_buf,x
        dex
        cpx #$FF
        bne @l
@e:     rts

expect_id:
        lda #<id_buf
        sta PTR
        lda #>id_buf
        sta PTR+1
        lda id_len
        sta cmp_len
        lda #0
        sta cmp_len+1
        lda #<st_ok
        sta PTR2
        lda #>st_ok
        sta PTR2+1
        jmp check_reply

; ECHO: target 1, $F0, then bytes counting on from 2
fill_echo:
        lda #<cmd_buf
        sta PTR2
        lda #>cmd_buf
        sta PTR2+1
        ldx #4          ; 4 pages, more than 895 bytes
        ldy #0
@l:     tya
        clc
        adc PTR2+1
        sta (PTR2),y
        iny
        bne @l
        inc PTR2+1
        dex
        bne @l
        lda #1
        sta cmd_buf
        lda #$F0
        sta cmd_buf+1
        rts

;-----------------------------------------------------------------------------------------
; Data
;-----------------------------------------------------------------------------------------

title:      .byte 147, "UCI TEST", 13, 13, 0
no_uci:     .byte "NO COMMAND INTERFACE AT $DF1C", 13, "($DF1D READS $", 0
no_uci2:    .byte ", NOT $C9)", 13, "SWITCH IT ON IN THE MENU.", 13, 0
t_empty:    .byte "EMPTY COMMAND ", 0
t_dos1:     .byte "DOS 1 IDENTIFY ", 0
t_dos2:     .byte "DOS 2 IDENTIFY ", 0
t_ctrl:     .byte "CONTROL IDENTIFY ", 0
t_none:     .byte "TARGET $0F IDENTIFY ", 0
t_unknown:  .byte "UNKNOWN COMMAND ", 0
t_echo:     .byte "ECHO 895 BYTES ", 0
t_noreply:  .byte "NO-REPLY COMMAND ", 0
t_error:    .byte "PUSH WHILE BUSY ", 0
t_abort:    .byte "ABORT ", 0
t_irq:      .byte "IRQ BIT ", 0
t_time:     .byte "DOS 1 IDENTIFY AGAIN", 13, 0
m_polls:    .byte "REPLY AFTER $", 0
m_got:      .byte " GOT $", 0
all_passed: .byte 13, "ALL PASSED", 13, 0
some_failed: .byte 13, "FAILED: $", 0

no_target:  .byte "NO TARGET"
st_ok:      .byte "00,OK", 0
st_unknown: .byte "21,UNKNOWN COMMAND", 0
nothing:    .byte 0

cmd_dos1_id: .byte $01, $01
cmd_dos2_id: .byte $02, $01
cmd_ctrl_id: .byte $04, $01
cmd_none_id: .byte $0F, $01
cmd_unknown: .byte $01, $7F
cmd_noreply: .byte $84, $01
cmd_echo     = cmd_dos1_id

.include "ucilib.s"

got:        .res 2
id_len:     .res 1
id_buf:     .res 256
cmd_buf:    .res 1024
