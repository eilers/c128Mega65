; Self-checking cartridge test, see README.md. make_crt_tests.py builds it once per cartridge
; with MODE and the probe table probes.inc:
;   MODE 0: C64 cartridge at $8000, started through CBM80
;   MODE 1: C128 function ROM at $8000, ID 1, so the KERNAL's POLL starts it at once
;   MODE 2: Ultimax, started through the reset vector, which the script points at cold
;
; The engine runs from RAM at $0C00, so it can switch banks under itself. Each probe is four
; bytes: op, address, value. A read stores the byte at RESULTS and counts it as an error if it
; is not the value; a write stores the value. At the end the border turns green or red, and
; STATUS holds 'P' or 'F' and then the error count.

RESULTS     = $0500
STATUS      = $0400
BORDER      = $D020
TP          = $FB               ; zero page, free for user programs on the C64 and C128

OP_WRITE    = 2

.import __ENGINE_LOAD__, __ENGINE_RUN__

.segment "HEADER"
.if MODE = 0
        .word cold, cold        ; $8000: cold and warm start
        .byte $C3, $C2, $CD, $38, $30
.elseif MODE = 1
        jmp cold                ; $8000: cold start
        jmp cold                ; $8003: warm start
        .byte $01               ; $8006: ID 1, start at once
        .byte "CBM"             ; $8007
.endif

.segment "CODE"
cold:   sei
        cld
        ldx #$FF
        txs
.if MODE <> 1
        lda #$37                ; data before direction, or the port drops the cartridge
        sta $01
        lda #$2F
        sta $00
.endif
        ldx #0
@copy:  lda __ENGINE_LOAD__,x
        sta __ENGINE_RUN__,x
        lda __ENGINE_LOAD__+$100,x
        sta __ENGINE_RUN__+$100,x
        lda __ENGINE_LOAD__+$200,x
        sta __ENGINE_RUN__+$200,x
        inx
        bne @copy
        jmp engine

.segment "ENGINE"
engine: lda #<table
        sta TP
        lda #>table
        sta TP+1
@loop:  ldy #0
        lda (TP),y              ; op, 0 ends the table
        beq @done
        tax
        iny
        lda (TP),y
        sta @rd+1
        sta @wr+1
        iny
        lda (TP),y
        sta @rd+2
        sta @wr+2
        iny
        lda (TP),y              ; value
        cpx #OP_WRITE
        bne @rd
@wr:    sta $FFFF
        jmp @next
@rd:    lda $FFFF
        ldx count
        sta RESULTS,x
        inc count
        cmp (TP),y
        beq @next
        inc errors
@next:  clc
        lda TP
        adc #4
        sta TP
        bcc @loop
        inc TP+1
        bne @loop

@done:  ldx #5                  ; green, 'P'
        lda #$10
        ldy errors
        beq @show
        ldx #2                  ; red, 'F'
        lda #$06
@show:  sty STATUS+1
        sta STATUS
        stx BORDER
        lda #$0B                ; blank the screen: it all shows the border colour
        sta $D011
@hold:  jmp @hold

count:  .byte 0
errors: .byte 0
table:
        .include "probes.inc"
