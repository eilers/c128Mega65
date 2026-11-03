; INPUTTEST: what the C128 sees of its keyboard, joysticks and POT lines, for the REST API's
; machine:input (a test on the C128 MEGA65 core, run over REST: readmem reads the table).
;
; With interrupts off it scans, over and over, into a table at $1300 (and shows it on the
; 40-column screen, 8 bytes a row from the third, in hex):
;   $1300-$1307  the C64 matrix, row r: PA r low, PB read; a 1 is a key down (bit c = PB c)
;   $1308-$130A  K0-K2 the same way: a K line low ($D02F), PB read
;   $130B-$1312  the matrix the other way, by PB line c: PB c low, PA read (bit r = PA r)
;   $1313        port 1: $DC01 with nothing scanned, a 1 per line down (up 0 .. fire 4)
;   $1314        port 2: $DC00 the same
;   $1315-$1318  POTX, POTY of port 1, then of port 2 ($D419/$D41A, PA6 or PA7 selecting)
;   $1319        $D505: bit 7 is 40/80 (0 down)
;   $131A        $01: bit 6 is Caps Lock (0 down)
;   $131B        NMIs (RESTORE) counted
;   $131C        $D7FF as read
;   $131D        if not 0: written to $D7FF every round
;   $131E        rounds, counting
; Stop it with a reset.

TABLE   = $1300
MATRIX  = TABLE         ; 11 bytes
REVERSE = TABLE + $0B   ; 8 bytes
PORT1   = TABLE + $13
PORT2   = TABLE + $14
POTS    = TABLE + $15   ; 4 bytes
D4080   = TABLE + $19
CAPS    = TABLE + $1A
NMIS    = TABLE + $1B
DBG_IN  = TABLE + $1C
DBG_OUT = TABLE + $1D
ROUNDS  = TABLE + $1E
SIZE    = $1F

CIA_PA  = $DC00
CIA_PB  = $DC01
CIA_DDA = $DC02
CIA_DDB = $DC03
VIC_K   = $D02F
SID_PX  = $D419
SID_PY  = $D41A
MMU_MCR = $D505
DEBUG   = $D7FF
NMINV   = $0318         ; the KERNAL's NMI vector
NMI_END = $FF33         ; the KERNAL's interrupt exit: $FF00, Y, X, A, RTI
SCREEN  = $0400
PTR     = $FB

.segment "LOADADDR"
        .word $1C01

.segment "CODE"
        .word basic_end
        .word 10
        .byte $9E, "7181", 0    ; 10 SYS7181
basic_end:
        .word 0

start:  sei
        lda #<nmi
        sta NMINV
        lda #>nmi
        sta NMINV+1
        ldx #SIZE - 1
        lda #0
@clear: sta TABLE,x
        dex
        bpl @clear
        lda #$20                ; a blank screen
        ldx #0
@cls:   sta SCREEN,x
        sta SCREEN + $100,x
        sta SCREEN + $200,x
        sta SCREEN + $2E8,x
        inx
        bne @cls
        jsr title

loop:   lda #$FF                ; PA out, PB in
        sta CIA_DDA
        lda #0
        sta CIA_DDB

        ; The matrix by row: PA r low
        ldx #0
@row:   lda bit_n,x
        eor #$FF
        sta CIA_PA
        jsr settle
        lda CIA_PB
        eor #$FF
        sta MATRIX,x
        inx
        cpx #8
        bne @row
        lda #$FF
        sta CIA_PA

        ; K0-K2: a K line low
        ldx #0
@kline: lda bit_n,x
        eor #$FF
        sta VIC_K
        jsr settle
        lda CIA_PB
        eor #$FF
        sta MATRIX + 8,x
        inx
        cpx #3
        bne @kline
        lda #$FF
        sta VIC_K

        ; Port 1, nothing scanned
        jsr settle
        lda CIA_PB
        eor #$FF
        and #$1F
        sta PORT1

        ; The other way: PB out, PA in, PB c low
        lda #0
        sta CIA_DDA
        lda #$FF
        sta CIA_DDB
        ldx #0
@col:   lda bit_n,x
        eor #$FF
        sta CIA_PB
        jsr settle
        lda CIA_PA
        eor #$FF
        sta REVERSE,x
        inx
        cpx #8
        bne @col
        lda #$FF
        sta CIA_PB
        lda #0
        sta CIA_DDB

        ; Port 2, nothing scanned
        jsr settle
        lda CIA_PA
        eor #$FF
        and #$1F
        sta PORT2

        ; The POT lines: PA6 selects port 1, PA7 port 2; the SID needs 512 cycles a reading
        lda #$FF
        sta CIA_DDA
        lda #$7F
        sta CIA_PA
        jsr pot_wait
        lda SID_PX
        sta POTS
        lda SID_PY
        sta POTS + 1
        lda #$BF
        sta CIA_PA
        jsr pot_wait
        lda SID_PX
        sta POTS + 2
        lda SID_PY
        sta POTS + 3
        lda #$FF
        sta CIA_PA

        lda MMU_MCR
        sta D4080
        lda $01
        sta CAPS
        lda DEBUG
        sta DBG_IN
        lda DBG_OUT
        beq @shown
        sta DEBUG
@shown: inc ROUNDS
        jsr show
        jmp loop

; RESTORE: counted, the KERNAL's own NMI work left out (no RS-232 here)
nmi:    inc NMIS
        jmp NMI_END

settle: ldy #8
@wait:  dey
        bne @wait
        rts

pot_wait:
        ldy #0                  ; ~1300 cycles: two SID readings
@wait:  nop
        dey
        bne @wait
        rts

; The table on the screen, 8 bytes a row, in hex
show:   lda #<(SCREEN + 80)
        sta PTR
        lda #>(SCREEN + 80)
        sta PTR + 1
        ldx #0
        ldy #0
@byte:  lda TABLE,x
        lsr
        lsr
        lsr
        lsr
        jsr hex
        lda TABLE,x
        and #$0F
        jsr hex
        lda #$20
        sta (PTR),y
        iny
        inx
        cpx #SIZE
        beq @done
        txa
        and #$07
        bne @byte
        lda PTR                 ; the next row: 40 on, y back to 0
        clc
        adc #40
        sta PTR
        bcc :+
        inc PTR + 1
:       ldy #0
        jmp @byte
@done:  rts

hex:    stx @x + 1
        tax
        lda digits,x
        sta (PTR),y
        iny
@x:     ldx #0
        rts

title:  ldx #0
@char:  lda text,x
        beq @end
        sta SCREEN,x
        inx
        bne @char
@end:   rts

bit_n:  .byte $01, $02, $04, $08, $10, $20, $40, $80
digits: .byte "0123456789", $01, $02, $03, $04, $05, $06   ; screen codes: A-F are 1-6
text:   .byte $09, $0E, $10, $15, $14, $14, $05, $13, $14, $20   ; INPUTTEST
        .byte "1300", 0
