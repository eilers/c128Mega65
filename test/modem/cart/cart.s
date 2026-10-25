; A C64 program on a Magic Desk cartridge (CRT type 19). Bank 0 holds this
; loader; the banks after it hold the program, which the loader copies to
; $0801 and RUNs, as LOAD and RUN would. The program is payload.prg.

PRG_START = $0801
BANK      = $DE00               ; Magic Desk: bank in bits 0-6, bit 7 = cartridge off
src       = $FB
dst       = $FD

        .segment "HEADER"
        .word   cold, cold
        .byte   $C3, $C2, $CD, $38, $30         ; CBM80

        .segment "CODE"
cold:   sei
        ldx     #$FF
        txs
        cld
        ldx     #0
@copy:  lda     __COPIER_LOAD__,x               ; the copier must run from RAM:
        sta     __COPIER_RUN__,x                ; bank switching hides this code
        inx
        cpx     #<__COPIER_SIZE__
        bne     @copy
        jmp     copier

        .import __COPIER_LOAD__, __COPIER_RUN__, __COPIER_SIZE__

        .segment "COPIER"
copier: lda     #<PRG_START
        sta     dst
        lda     #>PRG_START
        sta     dst+1
        ldx     #1
@bank:  stx     BANK
        lda     #<$8000
        sta     src
        lda     #>$8000
        sta     src+1
        ldy     #0
@page:  lda     (src),y                         ; writes under ROML go to RAM
        sta     (dst),y
        iny
        bne     @page
        inc     dst+1
        inc     src+1
        lda     src+1
        cmp     #>$A000
        bne     @page
        inx
        cpx     #PRG_BANKS+1
        bne     @bank

        lda     #$80
        sta     BANK                            ; cartridge off: a plain C64 from here
        jsr     $FF84                           ; IOINIT
        jsr     $FF87                           ; RAMTAS (finds RAM up to $A000)
        jsr     $FF8A                           ; RESTOR
        jsr     $FF81                           ; CINT
        cli
        jsr     $E453                           ; BASIC vectors
        jsr     $E3BF                           ; BASIC RAM
        ldx     #$FB
        txs
        lda     #<PRG_END                       ; end of the program, as LOAD sets it
        sta     $2D
        lda     #>PRG_END
        sta     $2E
        jsr     $A659                           ; CLR, point at the first line
        jmp     $A7AE                           ; RUN

        .segment "PAYLOAD"
payload:
        .incbin "payload.prg", 2
payload_end:

PRG_END   = PRG_START + (payload_end - payload)
PRG_BANKS = (payload_end - payload + $1FFF) / $2000
        .assert PRG_END <= $A000, error, "program runs into BASIC ROM"
        .assert PRG_BANKS <= 7, error, "program needs more than 7 banks"
