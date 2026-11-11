; C128 RAM bank test for the C128 MEGA65 core's 256 KB RAM setting.
;
; Runs in C128 mode: LOAD"RAMTEST",8 and RUN. Works with 128 KB and 256 KB:
; it finds which from the banks and prints it.
; Build with make_image.py (ca65 and ld65 from cc65).

CHROUT      = $FFD2
MMU_CR      = $FF00
MMU_RCR     = $D506
MMU_VER     = $D50B         ; $20: 2 banks of 64 KB, $40: 4 banks
DEBUGCART   = $D7FF         ; exit code for VICE's test harness, harmless elsewhere

REU_CMD     = $DF01
REU_C128    = $DF02
REU_ADDR    = $DF04
REU_BANK    = $DF06
REU_LEN     = $DF07
REU_IMASK   = $DF09
REU_ACR     = $DF0A
CMD_STASH   = $90
CMD_FETCH   = $91

ALL_RAM     = $3F           ; RAM everywhere, no I/O; bank in bits 6-7
NADDR       = 3             ; test addresses, NBYTES bytes each
NBYTES      = 8

BUF         = $3000         ; REU transfers in bank 0, above the program

; Trampolines in common RAM (BASIC's input buffer, unused while we run), so the
; CPU keeps running while the configuration selects another bank
TRAMP_PEEK  = $0280
TRAMP_POKE  = $0290

.segment "LOADADDR"
        .word $1C01

.segment "CODE"
        ; 10 SYS7181
        .word basic_end
        .word 10
        .byte $9E, "7181", 0
basic_end:
        .word 0

start:  sei                     ; the KERNAL's IRQ handler writes $FF00
        cld
        lda #0                  ; a second RUN finds the last run's values here
        ldx #vars_end-vars-1
:       sta vars,x
        dex
        bpl :-
        lda MMU_RCR
        sta saved_rcr
        jsr print
        .byte $93, $8E, "C128 RAM BANK TEST", $0D, 0

        jsr t_version
        jsr install_tramps
        jsr save_all
        jsr write_all
        jsr t_size
        jsr t_cpu
        jsr t_dma
        jsr restore_all

        lda nfail
        bne @bad
        jsr print
        .byte "ALL TESTS PASSED.", $0D, 0
        lda #$00
        beq @exit
@bad:   jsr prdec
        jsr print
        .byte " TEST(S) FAILED.", $0D, 0
        lda #$FF
@exit:  sta DEBUGCART
        lda saved_rcr
        sta MMU_RCR
        cli
        rts

;-----------------------------------------------------------------------------
; Tests
;-----------------------------------------------------------------------------

; $D50B: the MMU's version, its high nibble the number of 64 KB banks. The
; MiSTer MMU says $40 with 256 KB; VICE's full banks leave it at $20.
t_version:
        jsr begin
        .byte "MMU $D50B = $", 0
        lda MMU_VER
        sta ver
        jsr prhex
        lda ver
        cmp #$20
        beq :+
        cmp #$40
        beq :+
        jsr fail
        .byte " NOT $20 OR $40", 0
:       jmp done

; 256 KB if bank 0 kept its own first byte, 128 KB if bank 2's write landed
; there; $D50B = $40 must mean 256 KB
t_size:
        jsr begin
        .byte "RAM SIZE", 0
        lda #0
        sta vk
        sta vi
        jsr set_addr
        lda #0
        jsr bank_cr
        tax
        ldy #0
        jsr TRAMP_PEEK
        sta got
        lda #0
        jsr sig
        cmp got
        bne @not256
        inc mode256
        jsr print
        .byte " 256 KB", 0
        jmp done
@not256:
        lda #2
        jsr sig
        cmp got
        beq :+
        jsr fail
        .byte " BANK 0 LOST ITS BYTE", 0
        jmp done
:       jsr print
        .byte " 128 KB", 0
        lda ver
        cmp #$40
        bne :+
        jsr fail
        .byte " BUT $D50B = $40", 0
:       jmp done

; Each bank holds its own signature with 256 KB; with 128 KB banks 2 and 3
; are banks 0 and 1, so banks 0 and 1 show what was written to 2 and 3
t_cpu:
        jsr begin
        .byte "CPU BANKS 0-3 VIA $FF00", 0
        lda #0
        sta vb
@bank:  lda vb
        jsr owner
        sta ob
        lda vb
        jsr bank_cr
        sta vcr
        lda #0
        sta vk
@addr:  jsr set_addr
        lda #0
        sta vi
@byte:  ldx vcr
        ldy vi
        jsr TRAMP_PEEK
        sta got
        lda ob
        jsr sig
        cmp got
        beq @next
        sta want
        jsr fail
        .byte " BANK ", 0
        lda vb
        jsr prdec
        jsr print
        .byte " $", 0
        jsr praddr
        jsr print
        .byte " GOT $", 0
        lda got
        jsr prhex
        jsr print
        .byte " NOT $", 0
        lda want
        jsr prhex
        jmp @nextb              ; one report per bank
@next:  inc vi
        lda vi
        cmp #NBYTES
        bne @byte
        inc vk
        lda vk
        cmp #NADDR
        bne @addr
@nextb: inc vb
        lda vb
        cmp #4
        beq :+
        jmp @bank
:       jmp done

; The REU's RAM bank is the VIC's, bits 6-7 of $D506. STASH from each bank,
; then FETCH into bank 3 and find it where bank 3 is.
t_dma:
        jsr begin
        .byte "REU DMA VIA $D506", 0
        jsr reu_present
        beq :+
        jsr print
        .byte " NO REU, SKIPPED", $0D, 0
        rts
:       lda #0                  ; the low address: RAM in $FF00's configuration too
        sta vk
        lda #0
        sta vb
@stash: lda vb                  ; bank vb's 8 bytes to REU vb * 8
        asl
        asl
        asl
        sta REU_ADDR
        lda #0
        sta REU_ADDR+1
        sta REU_BANK
        lda #<taddr_dma
        ldx #>taddr_dma
        ldy #CMD_STASH
        jsr dma_bank
        inc vb
        lda vb
        cmp #4
        bne @stash
        lda #0                  ; all 32 bytes back into bank 0
        sta REU_ADDR
        sta REU_ADDR+1
        sta REU_BANK
        lda #<BUF
        sta REU_C128
        lda #>BUF
        sta REU_C128+1
        lda #4*NBYTES
        sta REU_LEN
        lda #0
        sta REU_LEN+1
        lda #CMD_FETCH
        sta REU_CMD
        lda #0
        sta vb
@cbank: lda vb
        jsr owner
        sta ob
        lda #0
        sta vi
@cbyte: lda vb
        asl
        asl
        asl
        ora vi
        tax
        lda ob
        jsr sig
        cmp BUF,x
        beq :+
        jsr fail
        .byte " STASH BANK ", 0
        lda vb
        jsr prdec
        jmp @fetch
:       inc vi
        lda vi
        cmp #NBYTES
        bne @cbyte
        inc vb
        lda vb
        cmp #4
        bne @cbank

@fetch: ldx #NBYTES-1           ; a new pattern via REU $000100 into bank 3
:       txa
        asl
        asl
        asl
        eor #$C3
        sta BUF,x
        dex
        bpl :-
        lda #<BUF
        sta REU_C128
        lda #>BUF
        sta REU_C128+1
        lda #0
        sta REU_ADDR
        sta REU_BANK
        sta REU_LEN+1
        lda #1
        sta REU_ADDR+1
        lda #NBYTES
        sta REU_LEN
        lda #CMD_STASH
        sta REU_CMD
        lda #0
        sta REU_ADDR
        lda #1
        sta REU_ADDR+1
        lda #3
        sta vb
        lda #<taddr_dma
        ldx #>taddr_dma
        ldy #CMD_FETCH
        jsr dma_bank
        lda #0                  ; every bank: the pattern where bank 3 is, else its signature
        sta vb
@fbank: lda vb
        jsr owner
        sta ob
        lda vb
        jsr bank_cr
        sta vcr
        jsr set_addr
        lda #0
        sta vi
@fbyte: ldx vcr
        ldy vi
        jsr TRAMP_PEEK
        sta got
        lda #3
        jsr owner
        cmp ob
        bne @sig
        ldx vi
        lda BUF,x
        jmp @cmp
@sig:   lda ob
        jsr sig
@cmp:   cmp got
        beq :+
        jsr fail
        .byte " FETCH BANK ", 0
        lda vb
        jsr prdec
        jmp done
:       inc vi
        lda vi
        cmp #NBYTES
        bne @fbyte
        inc vb
        lda vb
        cmp #4
        bne @fbank
        jmp done

; In: vb = bank, A/X = its address, Y = command. REU address and bank are set;
; runs NBYTES with the VIC bank, and so the DMA bank, set to vb.
dma_bank:
        sta REU_C128
        stx REU_C128+1
        lda #NBYTES
        sta REU_LEN
        lda #0
        sta REU_LEN+1
        sta REU_IMASK
        sta REU_ACR
        lda vb
        lsr
        ror
        ror
        sta tmp                 ; bank in bits 6-7
        lda saved_rcr
        and #$3F
        ora tmp
        sta MMU_RCR
        sty REU_CMD
        lda saved_rcr
        sta MMU_RCR
        rts

; Out: Z set if a REU answers at $DF00
reu_present:
        lda #$55
        sta REU_C128
        cmp REU_C128
        bne @no
        lda #$AA
        sta REU_C128
        cmp REU_C128
@no:    rts

;-----------------------------------------------------------------------------
; Banks
;-----------------------------------------------------------------------------

; In: A = bank. Out: A = the last bank written_all wrote to the same RAM: itself,
; or with 128 KB bank 2 for 0 and 3 for 1
owner:  ldx mode256
        bne :+
        ora #2
:       rts

; In: A = bank. Out: A = its all-RAM configuration.
bank_cr:
        lsr
        ror
        ror
        ora #ALL_RAM
        rts

; In: A = bank; vk, vi. Out: A = the signature byte vi at address vk in that bank.
sig:    asl
        asl
        clc
        adc vk
        asl
        asl
        asl
        asl
        ora vi
        eor #$A5
        rts

; Points both trampolines at test address vk
set_addr:
        ldx vk
        lda taddr_lo,x
        sta TRAMP_PEEK + (pk_adr - peek_src) + 1
        sta TRAMP_POKE + (pp_adr - poke_src) + 1
        lda taddr_hi,x
        sta TRAMP_PEEK + (pk_adr - peek_src) + 2
        sta TRAMP_POKE + (pp_adr - poke_src) + 2
        rts

praddr: ldx vk
        lda taddr_hi,x
        jsr prhex
        ldx vk
        lda taddr_lo,x
        jmp prhex

; Calls A's routine (lo in A, hi in X) for every bank, address and byte with
; vb, vk, vi and vcr set
each:   sta @j+1
        stx @j+2
        lda #0
        sta vb
@bank:  lda vb
        jsr bank_cr
        sta vcr
        lda #0
        sta vk
@addr:  jsr set_addr
        lda #0
        sta vi
@byte:
@j:     jsr $FFFF
        inc vi
        lda vi
        cmp #NBYTES
        bne @byte
        inc vk
        lda vk
        cmp #NADDR
        bne @addr
        inc vb
        lda vb
        cmp #4
        bne @bank
        rts

; The index of byte vi at address vk in bank vb in saved
save_idx:
        lda vb
        asl
        clc
        adc vb                  ; bank * 3
        clc
        adc vk
        asl
        asl
        asl                     ; * NBYTES
        ora vi
        tax
        rts

save_all:
        lda #<@one
        ldx #>@one
        jmp each
@one:   ldx vcr
        ldy vi
        jsr TRAMP_PEEK
        pha
        jsr save_idx
        pla
        sta saved,x
        rts

write_all:
        lda #<@one
        ldx #>@one
        jmp each
@one:   lda vb
        jsr sig
        ldx vcr
        ldy vi
        jmp TRAMP_POKE

; With 128 KB, banks 2 and 3 saved what banks 0 and 1 held, so any order works
restore_all:
        lda #<@one
        ldx #>@one
        jmp each
@one:   jsr save_idx
        lda saved,x
        ldx vcr
        ldy vi
        jmp TRAMP_POKE

install_tramps:
        lda MMU_RCR             ; share the bottom of RAM, as the KERNAL does
        ora #$04
        sta MMU_RCR
        sta saved_rcr
        ldx #PEEK_LEN-1
:       lda peek_src,x
        sta TRAMP_PEEK,x
        dex
        bpl :-
        ldx #POKE_LEN-1
:       lda poke_src,x
        sta TRAMP_POKE,x
        dex
        bpl :-
        lda MMU_CR              ; the trampolines return to this configuration
        sta TRAMP_PEEK + (pk_cr - peek_src) + 1
        sta TRAMP_POKE + (pp_cr - poke_src) + 1
        rts

; In: X = configuration, Y = index. Out: A = the byte.
peek_src:
        stx MMU_CR
pk_adr: lda $FFFF,y             ; patched: the test address
pk_cr:  ldx #$00                ; patched: the caller's configuration
        stx MMU_CR
        rts
PEEK_LEN = * - peek_src

; In: A = byte, X = configuration, Y = index
poke_src:
        stx MMU_CR
pp_adr: sta $FFFF,y
pp_cr:  ldx #$00
        stx MMU_CR
        rts
POKE_LEN = * - poke_src

.assert PEEK_LEN <= TRAMP_POKE - TRAMP_PEEK, error, "the peek trampoline overlaps the poke one"

;-----------------------------------------------------------------------------
; Helpers
;-----------------------------------------------------------------------------

; Starts a test: prints the name that follows the JSR
begin:  lda #0
        sta tfail
        jmp print

done:   lda tfail
        bne @bad
        jsr print
        .byte " PASS", $0D, 0
        rts
@bad:   inc nfail
        jsr print
        .byte " FAIL", $0D, 0
        rts

; Marks the test failed and prints the message that follows the JSR
fail:   lda #1
        sta tfail
        jmp print

; Prints the zero-terminated string that follows the JSR
print:  pla
        sta @p+1
        pla
        sta @p+2
@next:  inc @p+1
        bne @p
        inc @p+2
@p:     lda $FFFF
        beq @end
        jsr CHROUT
        jmp @next
@end:   lda @p+2
        pha
        lda @p+1
        pha
        rts

prhex:  pha
        lsr
        lsr
        lsr
        lsr
        jsr @nib
        pla
        and #$0F
@nib:   cmp #10
        bcc :+
        adc #6                  ; carry is set: +7 skips to 'A'
:       adc #'0'
        jmp CHROUT

; In: A < 100
prdec:  ldx #'0'-1
        sec
@tens:  inx
        sbc #10
        bcs @tens
        adc #10+'0'
        pha
        txa
        cmp #'0'
        beq :+
        jsr CHROUT
:       pla
        jmp CHROUT

;-----------------------------------------------------------------------------
; Data
;-----------------------------------------------------------------------------

; Low in bank 0's free area, the middle, and high under the KERNAL; in bank 1
; BASIC's variables and strings, which a SYS from a one-line program leaves alone
taddr_lo:  .byte <$1310, <$8010, <$F010
taddr_hi:  .byte >$1310, >$8010, >$F010
taddr_dma = $1310

vars:
nfail:     .byte 0
tfail:     .byte 0
mode256:   .byte 0
ver:       .byte 0
saved_rcr: .byte 0
vb:        .byte 0
vk:        .byte 0
vi:        .byte 0
ob:        .byte 0
vcr:       .byte 0
got:       .byte 0
want:      .byte 0
tmp:       .byte 0
vars_end:

saved:     .res 4 * NADDR * NBYTES

.assert start = 7181, error, "the BASIC line's SYS address is wrong"
.assert NADDR * NBYTES * 4 <= 256, error, "saved is indexed by X"
