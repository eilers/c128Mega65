; Cartridge ROM test for the expansion port, in C64 mode.
;
; make_carts.py builds it three ways: 8 KB (/ROML), 16 KB (/ROML and /ROMH) and 8 KB Ultimax
; (/ROMH at $E000). The CPU reads the cartridge's pattern area over and over at 1 MHz and checks
; every byte against the pattern, which it computes itself. The checker runs from RAM, so a bad
; read gets counted instead of executed.
;
; make_carts.py defines MODE (0 = 8K, 1 = 16K, 2 = Ultimax), PSTART and PEND (the pattern area's
; first page and the page after it) and SEED (the pattern's start value).

SCREEN      = $0400
COLOUR      = $D800
NMI_VEC     = $0318         ; the C64 KERNAL's NMI vector
VIC_CTRL1   = $D011
VIC_SPR_ENA = $D015
VIC_CTRL2   = $D016
VIC_MEM     = $D018
VIC_IRQ_ENA = $D01A
VIC_BORDER  = $D020
VIC_BG      = $D021
VIC_CLK     = $D030         ; C128 only, bit 0: 2 MHz
CIA1_ICR    = $DC0D
CIA2_PRA    = $DD00
CIA2_DDRA   = $DD02
CIA2_ICR    = $DD0D

.import __FONT_LOAD__, __FONT_RUN__, __FONT_SIZE__
.import __PROG_LOAD__, __PROG_RUN__, __PROG_SIZE__

GREEN       = 5
RED         = 2
YELLOW      = 7

; Zero page. The C64 KERNAL never runs, so all of it is free.
SRC         = $02           ; copy source, destination and page count
DST         = $04
CNT         = $06
PTR         = $07           ; pattern read pointer; the low byte stays 0
LFSR        = $09
SCR         = $0B           ; screen pointer
TXT         = $0D           ; text table pointer
PASSES      = $10
FAILS       = $12
ERRS        = $14           ; this pass: bad bytes, then the first one's address, value and pattern
FIRSTA      = $16
FIRSTG      = $18
FIRSTW      = $19
BADB        = $1A           ; the same 6 bytes for the last pass that failed
BADA        = $1C
BADG        = $1E
BADW        = $1F

; Screen codes for letters
.repeat 26, I
        .charmap $41 + I, $01 + I
.endrepeat

.macro TEXT row, col, str
        .word SCREEN + (row) * 40 + (col)
        .byte str, $FF
.endmacro

.macro SHOW row, col, var, bytes
        lda #<(SCREEN + (row) * 40 + (col))
        sta SCR
        lda #>(SCREEN + (row) * 40 + (col))
        sta SCR+1
        ldy #0
.if bytes = 2
        lda var+1
        jsr hex8
.endif
        lda var
        jsr hex8
.endmacro

.if MODE < 2
.segment "HEADER"
        .word cold              ; $8000: cold start
        .word cold              ; $8002: warm start
        .byte $C3, $C2, $CD, $38, $30   ; CBM80
.endif

; Runs from the cartridge: copy the font and the checker to RAM until a copy reads back the same
.segment "STUB"
cold:   sei
        cld
        ldx #$FF
        txs
        lda #$37                ; BASIC, KERNAL and I/O, so /ROML and /ROMH both decode.
        sta $01                 ; Data before direction, or the port drops the cartridge.
        lda #$2F
        sta $00
        lda #YELLOW             ; stays yellow if the copy never reads back the same
        sta VIC_BORDER
copy:   jsr setfont
        jsr move
        jsr setprog
        jsr move
        jsr setfont
        jsr same
        bne copy
        jsr setprog
        jsr same
        bne copy
        jmp start

setfont:
        ldx #0
        beq set
setprog:
        ldx #5
set:    ldy #0
@l:     lda copies,x
        sta SRC,y
        inx
        iny
        cpy #5
        bne @l
        rts

move:   ldy #0
@l:     lda (SRC),y
        sta (DST),y
        iny
        bne @l
        inc SRC+1
        inc DST+1
        dec CNT
        bne @l
        rts

; Z set if the copy matches
same:   ldy #0
@l:     lda (SRC),y
        cmp (DST),y
        bne @x
        iny
        bne @l
        inc SRC+1
        inc DST+1
        dec CNT
        bne @l
@x:     rts

romrti: rti

copies: .word __FONT_LOAD__, __FONT_RUN__
        .byte >(__FONT_SIZE__ + $FF)
        .word __PROG_LOAD__, __PROG_RUN__
        .byte >(__PROG_SIZE__ + $FF)

; Runs from RAM at $0A00
.segment "PROG"
start:  lda #$7F
        sta CIA1_ICR
        sta CIA2_ICR
        lda CIA1_ICR
        lda CIA2_ICR
        lda #<ramrti            ; RESTORE goes through the KERNAL's NMI vector (not in Ultimax)
        sta NMI_VEC
        lda #>ramrti
        sta NMI_VEC+1
        lda #0
        sta VIC_CLK             ; 1 MHz on a C128
        sta VIC_SPR_ENA
        sta VIC_IRQ_ENA
        sta VIC_BG
        lda CIA2_DDRA
        ora #$03
        sta CIA2_DDRA
        lda CIA2_PRA
        ora #$03                ; VIC bank 0
        sta CIA2_PRA
        lda #$1B
        sta VIC_CTRL1
        lda #$08
        sta VIC_CTRL2
        lda #$12                ; screen at $0400, font at $0800
        sta VIC_MEM

        ldx #0
@cls:   lda #' '
        sta SCREEN,x
        sta SCREEN+$100,x
        sta SCREEN+$200,x
        sta SCREEN+$2E8,x
        lda #1                  ; white
        sta COLOUR,x
        sta COLOUR+$100,x
        sta COLOUR+$200,x
        sta COLOUR+$2E8,x
        inx
        bne @cls

        lda #<text
        sta TXT
        lda #>text
        sta TXT+1
@line:  ldy #0
        lda (TXT),y
        sta SCR
        iny
        lda (TXT),y
        beq @done
        sta SCR+1
        lda TXT
        clc
        adc #2
        sta TXT
        bcc :+
        inc TXT+1
:       ldy #0
@ch:    lda (TXT),y
        cmp #$FF
        beq @eol
        sta (SCR),y
        iny
        bne @ch
@eol:   iny
        tya
        clc
        adc TXT
        sta TXT
        bcc @line
        inc TXT+1
        bne @line

@done:  lda #0
        ldx #BADW - PASSES
:       sta PASSES,x
        dex
        bpl :-

loop:   jsr check
        inc PASSES
        bne :+
        inc PASSES+1
:       lda ERRS
        ora ERRS+1
        beq @show
        inc FAILS
        bne :+
        inc FAILS+1
:       ldx #5
:       lda ERRS,x
        sta BADB,x
        dex
        bpl :-
@show:  SHOW 5, 17, PASSES, 2
        SHOW 6, 17, FAILS, 2
        SHOW 9, 17, BADB, 2
        SHOW 10, 17, BADA, 2
        SHOW 11, 17, BADG, 1
        SHOW 11, 30, BADW, 1
        lda FAILS
        ora FAILS+1
        bne :+
        lda #GREEN
        bne :++
:       lda #RED
:       sta VIC_BORDER
        jmp loop

; Read the pattern area once. ERRS counts the bytes that differ, FIRSTA/G/W hold the first one.
check:  lda #0
        sta ERRS
        sta ERRS+1
        sta PTR
        lda #PSTART
        sta PTR+1
        lda #<SEED
        sta LFSR
        lda #>SEED
        sta LFSR+1
        ldy #0
@byte:  lsr LFSR+1              ; 16-bit Galois LFSR, taps $B400; each byte is its low half
        ror LFSR
        bcc @read
        lda LFSR+1
        eor #$B4
        sta LFSR+1
@read:  lda (PTR),y
        cmp LFSR
        bne @bad
@next:  iny
        bne @byte
        inc PTR+1
        lda PTR+1
        cmp #PEND
        bne @byte
        rts
@bad:   ldx ERRS
        bne @count
        ldx ERRS+1
        bne @count
        sta FIRSTG
        sty FIRSTA
        ldx PTR+1
        stx FIRSTA+1
        ldx LFSR
        stx FIRSTW
@count: inc ERRS
        bne @next
        inc ERRS+1
        bne @next

; A in hex at (SCR),Y
hex8:   pha
        lsr
        lsr
        lsr
        lsr
        jsr @nyb
        pla
        and #$0F
@nyb:   cmp #10
        bcc @dig
        sbc #9                  ; screen codes 1-6 are A-F
        bne @put
@dig:   ora #$30
@put:   sta (SCR),y
        iny
        rts

ramrti: rti

text:
.if MODE = 0
        TEXT 1, 0, "ROMTEST 8K  /ROML $8000-$9FFF"
.elseif MODE = 1
        TEXT 1, 0, "ROMTEST 16K  /ROML+/ROMH $8000-$BFFF"
.else
        TEXT 1, 0, "ROMTEST ULTIMAX  /ROMH $E000-$FFFF"
.endif
        TEXT 2, 0, .sprintf("CHECKS $%04X-$%04X AT 1 MHZ", PSTART * 256, PEND * 256 - 1)
        TEXT 5, 0, "PASSES          $"
        TEXT 6, 0, "FAILED PASSES   $"
        TEXT 8, 0, "LAST FAILED PASS:"
        TEXT 9, 0, "  BAD BYTES     $"
        TEXT 10, 0, "  FIRST AT      $"
        TEXT 11, 0, "  READ          $     WANTED $"
        TEXT 14, 0, "GREEN BORDER: EVERY PASS MATCHED"
        TEXT 15, 0, "RED BORDER: A PASS FAILED"
        .word 0

.segment "FONT"
        .incbin "font.bin"

.if MODE = 2
.segment "VECTORS"
        .word romrti            ; NMI
        .word cold              ; RESET
        .word romrti            ; IRQ
.endif
