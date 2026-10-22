; C128 external function ROM that runs REUTEST in C128 mode, for cartridges such as
; Kung Fu Flash. The KERNAL calls its cold-start entry at boot; it copies the program to
; $1C01, maps bank 15 and starts it. After the test the machine waits for a reset, which
; runs the test again and so checks that the REU kept its contents.

PROG        = $1C01
LAUNCH      = $1B00             ; free RAM below the start of BASIC
SRC         = $FB               ; zero page, free for user programs
DST         = $FD

.segment "ROM"
        jmp cold                ; $8000: cold start
        jmp warm                ; $8003: warm start
        .byte $02               ; $8006: start at boot
        .byte "CBM"             ; $8007

warm:   rts

cold:   sei
        lda #<payload
        sta SRC
        lda #>payload
        sta SRC+1
        lda #<PROG
        sta DST
        lda #>PROG
        sta DST+1
        ldx #(PAYLOAD_LEN + 255) / 256
        ldy #0
@page:  lda (SRC),y
        sta (DST),y
        iny
        bne @page
        inc SRC+1
        inc DST+1
        dex
        bne @page
        ldx #LAUNCH_LEN-1
@launch:
        lda launch_src,x
        sta LAUNCH,x
        dex
        bpl @launch
        jmp LAUNCH

; Runs from RAM at LAUNCH: bank 15 maps BASIC ROM over this cartridge
launch_src:
        lda #$00
        sta $FF00
        jsr 7181
@hold:  clc
        bcc @hold               ; relative, so it works from the copy
LAUNCH_LEN = * - launch_src

payload:
        .incbin "reutest.prg", 2
PAYLOAD_LEN = * - payload
