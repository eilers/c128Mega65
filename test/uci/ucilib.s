; What the UCI test programs share: the transport, checks and printing. Included at the
; end of their code; it ends in the BSS segment.

;-----------------------------------------------------------------------------------------
; Transport
;-----------------------------------------------------------------------------------------

; Leave the interface idle, whatever an earlier run left behind
release:
        lda #C_CLR
        sta CTRL
        lda CTRL
        and #S_STATE
        beq @idle
        cmp #$10
        beq @busy
        lda #C_ACC
        sta CTRL
        jmp release
@busy:  lda #C_ABORT
        sta CTRL
        jsr wait_idle
@idle:  rts

; The command at PTR, cmd_len bytes, then PUSH_CMD (push_irq: with the IRQ bit)
push:   lda #C_PUSH
        bne push_ctrl
push_irq:
        lda #C_PUSH | C_IRQ
push_ctrl:
        sta ctrl
        lda PTR
        sta PTR2
        lda PTR+1
        sta PTR2+1
        lda cmd_len
        sta count
        lda cmd_len+1
        sta count+1
        ldy #0
@loop:  lda count
        ora count+1
        beq @push
        lda (PTR2),y
        sta CMDQ
        inc PTR2
        bne :+
        inc PTR2+1
:       lda count
        bne :+
        dec count+1
:       dec count
        jmp @loop
@push:  lda ctrl
        sta CTRL
        rts

; Wait for Data Last or Data More; rpolls is how long it took. After about 13 s without
; (6.5 s in FAST mode; a mount on the core takes about 2 s),
; the reply is empty with status TIMEOUT, the interface is released, and carry is set.
wait_reply:
        jsr poll_start
@loop:  lda CTRL
        and #$20
        bne @done
        jsr poll_next
        bcc @loop
        lda #0
        sta resp_len
        sta resp_len+1
        ldx #7
@t:     lda m_timeout,x
        sta stat_buf,x
        dex
        bpl @t
        jsr release
        sec
        rts
@done:  lda polls
        sta rpolls
        lda polls+1
        sta rpolls+1
        lda polls+2
        sta rpolls+2
        clc
        rts

; Wait for idle; carry set on a timeout
wait_idle:
        jsr poll_start
@loop:  lda CTRL
        and #S_STATE
        beq @done
        jsr poll_next
        bcc @loop
        sec
        rts
@done:  clc
        rts

poll_start:
        lda #0
        sta polls
        sta polls+1
        sta polls+2
        rts

poll_next:
        inc polls
        bne @ok
        inc polls+1
        bne @ok
        inc polls+2
        lda polls+2
        cmp #6
        rts             ; carry set at 6 x 65536 polls
@ok:    clc
        rts

; The reply into resp_buf (resp_len bytes) and stat_buf (a C string)
read_reply:
        lda #<resp_buf
        sta PTR2
        lda #>resp_buf
        sta PTR2+1
        lda #0
        sta resp_len
        sta resp_len+1
        ldy #0
@data:  lda CTRL
        and #S_DATA
        beq @stat
        lda resp_len+1
        cmp #>1024
        bcs @stat
        lda RESP
        sta (PTR2),y
        inc PTR2
        bne :+
        inc PTR2+1
:       inc resp_len
        bne @data
        inc resp_len+1
        bne @data
@stat:  ldx #0
@sloop: lda CTRL
        and #S_STAT
        beq @end
        lda STATQ
        sta stat_buf,x
        inx
        cpx #255
        bne @sloop
@end:   lda #0
        sta stat_buf,x
        rts

; Push, wait, read, DATA_ACC: one block, as every command here has (reply: after a push)
transact:
        jsr push
reply:  jsr wait_reply
        bcs @out
        jsr read_reply
        lda #C_ACC
        sta CTRL
        jsr wait_idle
@out:   rts

;-----------------------------------------------------------------------------------------
; Checks and printing
;-----------------------------------------------------------------------------------------

; resp_buf against PTR (cmp_len bytes; resp_buf itself means any data) and stat_buf
; against PTR2 (st_ok_any: starts with 00)
check_reply:
        lda PTR2
        cmp #<st_ok_any
        bne @exact
        lda PTR2+1
        cmp #>st_ok_any
        bne @exact
        lda stat_buf
        cmp #'0'
        bne @bad_stat
        lda stat_buf+1
        cmp #'0'
        bne @bad_stat
        beq @data
@exact: ldy #0
@sl:    lda (PTR2),y
        cmp stat_buf,y
        bne @bad_stat
        iny
        cmp #0
        bne @sl
@data:  lda PTR
        cmp #<resp_buf
        bne @len
        lda PTR+1
        cmp #>resp_buf
        beq @ok
@len:   lda resp_len
        cmp cmp_len
        bne @bad_len
        lda resp_len+1
        cmp cmp_len+1
        bne @bad_len
        lda #<resp_buf
        sta PTR2
        lda #>resp_buf
        sta PTR2+1
        ldy #0
@dl:    lda cmp_len
        ora cmp_len+1
        beq @ok
        lda (PTR),y
        cmp (PTR2),y
        bne @bad_data
        inc PTR
        bne :+
        inc PTR+1
:       inc PTR2
        bne :+
        inc PTR2+1
:       lda cmp_len
        bne :+
        dec cmp_len+1
:       dec cmp_len
        jmp @dl
@ok:    jmp pass
@bad_stat:
        jsr fail
        PRINT m_status
        jsr show_status
        rts
@bad_len:
        jsr fail
        PRINT m_len
        lda resp_len+1
        jsr hex8
        lda resp_len
        jsr hex8
        lda #13
        jmp CHROUT
@bad_data:
        jsr fail
        PRINT m_data
        rts

; The reply's data (up to 40 bytes) and status, on one line
show:   lda #'"'
        jsr CHROUT
        ldx #0
@l:     cpx resp_len
        bne :+
        lda resp_len+1
        beq @q
:       cpx #40
        beq @q
        lda resp_buf,x
        jsr CHROUT
        inx
        bne @l
@q:     lda #'"'
        jsr CHROUT
        lda #' '
        jsr CHROUT
show_status:
        ldx #0
@s:     lda stat_buf,x
        beq @e
        jsr CHROUT
        inx
        bne @s
@e:     lda #13
        jmp CHROUT

result: bcc pass
fail:   PRINT m_fail
        inc fails
        rts
pass:   PRINT m_pass
        rts

puts:   sta PTR2
        sty PTR2+1
        ldy #0
@l:     lda (PTR2),y
        beq @e
        jsr CHROUT
        iny
        bne @l
@e:     rts

hex8:   pha
        lsr
        lsr
        lsr
        lsr
        jsr @digit
        pla
        and #$0F
@digit: cmp #10
        bcc :+
        adc #6
:       adc #'0'
        jmp CHROUT

m_pass:     .byte "PASS", 13, 0
m_fail:     .byte "FAIL", 13, 0
m_status:   .byte " STATUS: ", 0
m_len:      .byte " LENGTH $", 0
m_data:     .byte " DATA DIFFERS", 13, 0
m_timeout:  .byte "TIMEOUT", 0
st_ok_any:  .byte 0

.segment "BSS"
fails:      .res 1
ctrl:       .res 1
count:      .res 2
cmd_len:    .res 2
cmp_len:    .res 2
resp_len:   .res 2
polls:      .res 3
rpolls:     .res 3
stat_buf:   .res 256
resp_buf:   .res 1024
