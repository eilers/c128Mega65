; C128 REU test for the C128 MEGA65 core.
;
; Runs in C128 mode: LOAD"REUTEST",8 and RUN. It overwrites all of the REU.
; Build with make_image.py (ca65 and ld65 from cc65).

CHROUT      = $FFD2
MMU_CR      = $FF00
MMU_RCR     = $D506
VIC_BORDER  = $D020
VIC_CLK     = $D030         ; bit 0: 2 MHz
DEBUGCART   = $D7FF         ; exit code for VICE's test harness, harmless elsewhere

REU_STATUS  = $DF00
REU_CMD     = $DF01
REU_C128    = $DF02
REU_ADDR    = $DF04
REU_BANK    = $DF06
REU_LEN     = $DF07
REU_IMASK   = $DF09
REU_ACR     = $DF0A

; Commands start at once. AND with CMD_FF00 to wait for the next write to $FF00.
CMD_STASH   = $90
CMD_FETCH   = $91
CMD_SWAP    = $92
CMD_VERIFY  = $93
CMD_AUTOLD  = $20
CMD_FF00    = $EF

ST_IRQ      = $80
ST_EOB      = $40           ; end of block
ST_FAULT    = $20           ; verify mismatch
ST_SIZE     = $10           ; always set on a 1750

; 256-byte buffers in bank 0; reutest.cfg keeps the program below them
BUF_A       = $3000
BUF_B       = $3100
BUF_C       = $3200
BUF_D       = $3300
BUF_E       = $3400
BUF_W       = $3600         ; whole-REU test page
BUF_S       = $3700         ; small transfers and the mark

CIA1_DDRA   = $DC02
CIA1_DDRB   = $DC03
COLOUR_RAM  = $D800
VIC_BG      = $D021

; Trampolines in common RAM (BASIC's input buffer, unused while we run), so the
; CPU keeps running when the configuration switches it to RAM bank 1
TRAMP_PEEK  = $0280
TRAMP_FF00  = $0290

; Load the transfer registers' shadow copies: C128 address, REU address, length
.macro SETX c, r, l
        lda #<(c)
        sta xc
        lda #>(c)
        sta xc+1
        lda #<(r)
        sta xr
        lda #>(r)
        sta xr+1
        lda #^(r)
        sta xr+2
        lda #<(l)
        sta xl
        lda #>(l)
        sta xl+1
.endmacro

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
        lda VIC_CLK
        sta saved_clk
        jsr print
        .byte $93, $8E, "C128 REU TEST - OVERWRITES THE REU", $0D, 0

        jsr t_registers
        lda present
        bne :+
        jmp finish
:       jsr t_size
        jsr t_mark
        jsr t_stash_fetch
        jsr t_swap
        jsr t_verify
        jsr t_autoload
        jsr t_fixed
        jsr t_ff00
        jsr t_bank1
        jsr t_ff00_bank
        jsr t_io_write
        jsr t_io_vic
        jsr t_io_cia
        jsr t_io_colour
        jsr t_irq
        jsr t_fast
        jsr t_whole
        jsr write_mark

finish: lda nfail
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
        lda saved_clk
        sta VIC_CLK
        cli
        rts

;-----------------------------------------------------------------------------
; Tests
;-----------------------------------------------------------------------------

t_registers:
        jsr begin
        .byte "REGISTERS", 0
        lda #$55
        sta REU_C128
        lda #$AA
        sta REU_C128+1
        lda REU_C128
        cmp #$55
        bne @none
        lda REU_C128+1
        cmp #$AA
        beq @found
@none:  jsr fail
        .byte $0D, "NO REU AT $DF00. SET REU TO 512 KB", $0D
        .byte "OR MORE IN THE MENU.", 0
        jmp done
@found: lda #1
        sta present
        lda REU_STATUS          ; clear what an earlier run left
        lda REU_STATUS
        ldx #ST_SIZE
        jsr expect
        lda #$33
        sta REU_ADDR
        lda #$CC
        sta REU_ADDR+1
        lda #$5A
        sta REU_LEN
        lda #$A5
        sta REU_LEN+1
        lda REU_ADDR
        ldx #$33
        jsr expect
        lda REU_ADDR+1
        ldx #$CC
        jsr expect
        lda REU_LEN
        ldx #$5A
        jsr expect
        lda REU_LEN+1
        ldx #$A5
        jsr expect
        lda #$00
        sta REU_BANK
        lda REU_BANK            ; bank bits the REU lacks read as 1, see t_size
        sta bankreg
        lda #$00
        sta REU_IMASK
        lda REU_IMASK
        ldx #$1F
        jsr expect
        lda #$E0
        sta REU_IMASK
        lda REU_IMASK
        ldx #$FF
        jsr expect
        lda #$00
        sta REU_IMASK
        sta REU_ACR
        lda REU_ACR
        ldx #$3F
        jsr expect
        lda #$C0
        sta REU_ACR
        lda REU_ACR
        ldx #$FF
        jsr expect
        lda #$00
        sta REU_ACR
        jmp done

; Bank b gets the byte b, from bank 255 down to 0. Reading from bank 0 up, the
; first bank that does not hold its own number is where the REU wraps. If all
; 256 do, it is 16 MB: full is set, and banks and nbanks are 0.
t_size:
        jsr begin
        .byte "SIZE", 0
        lda #0
        sta full
        ldx #255
@wr:    stx BUF_S
        SETX BUF_S, 0, 1
        stx xr+2
        lda #CMD_STASH
        jsr xfer
        dex
        cpx #255
        bne @wr
        ldx #0
@rd:    SETX BUF_S, 0, 1
        stx xr+2
        lda #CMD_FETCH
        jsr xfer
        cpx BUF_S
        bne @end
        inx
        bne @rd
        inc full
@end:   stx banks
        stx nbanks              ; WHOLE REU covers this or what $DF06 claims, if more
        lda full
        bne @print
        lda bankreg
        ldx #32
        cmp #$E0
        beq @claim
        ldx #8
        cmp #$F8
        bne @print
@claim: cpx nbanks
        bcc @print
        stx nbanks
@print: jsr print
        .byte ": ", 0
        lda full
        beq @not16
        jsr print
        .byte "16 MB", 0
        jmp @reg
@not16: lda banks
        cmp #8
        bne @not512
        jsr print
        .byte "512 KB", 0
        jmp @reg
@not512:
        cmp #32
        bne @odd
        jsr print
        .byte "2 MB", 0
@reg:   jsr print               ; $F8 at any size on an 8726; MiSTer's reu.sv reads $E0 at 2 MB
        .byte ", $DF06=$", 0
        lda bankreg
        jsr prhex
        jmp done
@odd:   jsr fail
        .byte "$", 0
        lda banks
        jsr prhex
        jsr print
        .byte " BANKS", 0
        jmp done

; Reports the mark that the last run left in the REU's last page
t_mark:
        lda #0
        sta runs
        lda nbanks
        ora full
        beq @rts
        jsr mark_addr
        lda #CMD_FETCH
        jsr xfer
        ldx #MARK_LEN-1
@cmp:   lda BUF_S,x
        cmp mark,x
        bne @none
        dex
        bpl @cmp
        lda BUF_S+MARK_LEN
        sta runs
        jsr print
        .byte "FOUND MARK $", 0
        lda runs
        jsr prhex
        jsr print
        .byte " FROM THE LAST RUN.", $0D, 0
        rts
@none:  jsr print
        .byte "NO MARK FROM AN EARLIER RUN.", $0D, 0
@rts:   rts

t_stash_fetch:
        jsr begin
        .byte "STASH AND FETCH", 0
        lda #>BUF_A
        ldy #$11
        jsr fill_page
        lda #>BUF_B
        ldy #$00
        jsr const_page
        SETX BUF_A, $00FFC0, 256        ; across a 64 KB boundary
        lda #CMD_STASH
        jsr xfer
        lda REU_STATUS
        ldx #ST_EOB|ST_SIZE
        jsr expect
        ; the addresses end one past the block, and the length at 1
        lda REU_C128
        ldx #<(BUF_A+256)
        jsr expect
        lda REU_C128+1
        ldx #>(BUF_A+256)
        jsr expect
        lda REU_ADDR
        ldx #$C0
        jsr expect
        lda REU_ADDR+1
        ldx #$00
        jsr expect
        lda REU_BANK
        and #$07
        ldx #$01
        jsr expect
        lda REU_LEN
        ldx #$01
        jsr expect
        lda REU_LEN+1
        ldx #$00
        jsr expect
        SETX BUF_B, $00FFC0, 256
        lda #CMD_FETCH
        jsr xfer
        lda #>BUF_A
        ldy #>BUF_B
        jsr check_pages
        jmp done

t_swap:
        jsr begin
        .byte "SWAP", 0
        lda #>BUF_A
        ldy #$22
        jsr fill_page
        lda #>BUF_C
        ldy #$99
        jsr fill_page
        SETX BUF_C, $002000, 256
        lda #CMD_STASH
        jsr xfer
        SETX BUF_A, $002000, 256
        lda #CMD_SWAP
        jsr xfer
        lda #>BUF_A             ; the C128 got the REU's bytes ...
        ldy #>BUF_C
        jsr check_pages
        SETX BUF_B, $002000, 256
        lda #CMD_FETCH
        jsr xfer
        lda #>BUF_D             ; ... and the REU the C128's
        ldy #$22
        jsr fill_page
        lda #>BUF_B
        ldy #>BUF_D
        jsr check_pages
        jmp done

t_verify:
        jsr begin
        .byte "VERIFY", 0
        lda #>BUF_A
        ldy #$33
        jsr fill_page
        SETX BUF_A, $003000, 256
        lda #CMD_STASH
        jsr xfer
        lda REU_STATUS
        lda #CMD_VERIFY
        jsr xfer
        lda REU_STATUS
        and #ST_FAULT
        ldx #$00
        jsr expect
        inc BUF_A+$80
        lda #CMD_VERIFY
        jsr xfer
        lda REU_STATUS          ; VICE leaves end of block clear here, reu.sv sets it
        and #ST_FAULT
        ldx #ST_FAULT
        jsr expect
        jmp done

t_autoload:
        jsr begin
        .byte "AUTOLOAD", 0
        SETX BUF_A, $004000, $0010
        lda #CMD_STASH|CMD_AUTOLD
        jsr xfer
        lda REU_C128
        ldx #<BUF_A
        jsr expect
        lda REU_C128+1
        ldx #>BUF_A
        jsr expect
        lda REU_ADDR
        ldx #$00
        jsr expect
        lda REU_ADDR+1
        ldx #$40
        jsr expect
        lda REU_BANK
        and #$07
        ldx #$00
        jsr expect
        lda REU_LEN
        ldx #$10
        jsr expect
        lda REU_LEN+1
        ldx #$00
        jsr expect
        jmp done

t_fixed:
        jsr begin
        .byte "FIXED ADDRESSES", 0
        lda #$5A
        sta BUF_S
        lda #$80                ; fixed C128 address: fill the REU from one byte
        sta REU_ACR
        SETX BUF_S, $005000, 256
        lda #CMD_STASH
        jsr xfer
        lda #$00
        sta REU_ACR
        SETX BUF_B, $005000, 256
        lda #CMD_FETCH
        jsr xfer
        lda #>BUF_D
        ldy #$5A
        jsr const_page
        lda #>BUF_B
        ldy #>BUF_D
        jsr check_pages
        lda #$40                ; fixed REU address: fill the C128 from one byte
        sta REU_ACR
        SETX BUF_B, $003000, 256        ; t_verify left $33 there
        lda #CMD_FETCH
        jsr xfer
        lda #$00
        sta REU_ACR
        lda #>BUF_D
        ldy #$33
        jsr const_page
        lda #>BUF_B
        ldy #>BUF_D
        jsr check_pages
        jmp done

; BASIC 7's STASH, FETCH and SWAP start their transfers this way
t_ff00:
        jsr begin
        .byte "$FF00 TRIGGER", 0
        lda #>BUF_B
        ldy #$00
        jsr const_page
        SETX BUF_B, $00FFC0, 256        ; t_stash_fetch left its block there
        lda #CMD_FETCH & CMD_FF00
        jsr xfer
        lda REU_LEN                     ; armed, so nothing has moved yet
        ldx #$00
        jsr expect
        lda REU_LEN+1
        ldx #$01
        jsr expect
        lda BUF_B
        ldx #$00
        jsr expect
        lda MMU_CR
        sta MMU_CR
        lda #>BUF_D
        ldy #$11
        jsr fill_page
        lda #>BUF_B
        ldy #>BUF_D
        jsr check_pages
        jmp done

; REU DMA reaches RAM bank 1 through the VIC bank bits in $D506, not the
; bank bits of the configuration (Mapping the Commodore 128, p. 419)
t_bank1:
        jsr begin
        .byte "BANK 1 VIA $D506", 0
        jsr install_tramps
        lda #>BUF_A
        ldy #$44
        jsr fill_page
        SETX BUF_A, $010000, 256
        lda #CMD_STASH
        jsr xfer
        lda #>BUF_E
        ldy #$A5
        jsr const_page
        SETX BUF_E, $010000, 256
        lda #CMD_FETCH
        jsr vic_bank1_xfer
        lda #>BUF_D
        ldy #$A5
        jsr const_page
        lda #>BUF_E
        ldy #>BUF_D
        jsr cmp_pages
        beq :+
        jsr fail
        .byte " BANK 0 CHANGED", 0
:       ldx #0                  ; the CPU must find the block in bank 1
@peek:  jsr TRAMP_PEEK
        cmp BUF_A,x
        bne @diff
        inx
        bne @peek
        beq @back               ; always
@diff:  stx cp_idx
        jsr fail
        .byte " BANK 1 +$", 0
        lda cp_idx
        jsr prhex
@back:  SETX BUF_E, $011000, 256        ; and back out of bank 1
        lda #CMD_STASH
        jsr vic_bank1_xfer
        SETX BUF_B, $011000, 256
        lda #CMD_FETCH
        jsr xfer
        lda #>BUF_A
        ldy #>BUF_B
        jsr check_pages
        jmp done

; A configuration with RAM bank 1, written to $FF00 to start the transfer,
; still moves bank 0 while the VIC bank is 0
t_ff00_bank:
        jsr begin
        .byte "$FF00 BANK BITS", 0
        lda #>BUF_A
        ldy #$55
        jsr fill_page
        SETX BUF_A, $012000, 256
        lda #CMD_STASH
        jsr xfer
        lda #>BUF_E
        ldy #$A5
        jsr const_page
        SETX BUF_E, $012000, 256
        lda #CMD_FETCH & CMD_FF00
        jsr xfer
        ldy #$40                ; RAM bank 1, ROMs and I/O
        ldx MMU_CR
        jsr TRAMP_FF00
        lda #>BUF_A
        ldy #>BUF_E
        jsr cmp_pages
        beq :+
        jsr fail
        .byte " NOT IN BANK 0", 0
:       jmp done

; In: A = command. Runs it with the VIC bank, and so the DMA bank, set to 1.
vic_bank1_xfer:
        pha
        lda MMU_RCR
        sta saved_rcr
        and #$3F
        ora #$40
        sta MMU_RCR
        pla
        jsr xfer
        lda saved_rcr
        sta MMU_RCR
        rts

install_tramps:
        lda MMU_RCR             ; share the bottom of RAM, as the KERNAL does
        ora #$04
        sta MMU_RCR
        ldx #PEEK_LEN-1
@peek:  lda peek_src,x
        sta TRAMP_PEEK,x
        dex
        bpl @peek
        lda MMU_CR              ; the trampoline returns to this configuration
        sta TRAMP_PEEK + (peek_cr - peek_src) + 1
        ldx #FF00_LEN-1
@ff00:  lda ff00_src,x
        sta TRAMP_FF00,x
        dex
        bpl @ff00
        rts

; In: X = offset. Out: A = byte X of BUF_E in RAM bank 1.
peek_src:
        lda #$40                ; RAM bank 1, ROMs and I/O
        sta MMU_CR
        lda BUF_E,x
peek_cr:
        ldy #$00                ; patched with the caller's configuration
        sty MMU_CR
        rts
PEEK_LEN = * - peek_src

; In: Y = configuration for the armed transfer, X = configuration to return to
ff00_src:
        sty MMU_CR
        stx MMU_CR
        rts
FF00_LEN = * - ff00_src

t_io_write:
        jsr begin
        .byte "DMA INTO VIC", 0
        lda VIC_BORDER
        sta saved
        lda #$F2
        sta BUF_S
        SETX BUF_S, $006000, 1
        lda #CMD_STASH
        jsr xfer
        SETX VIC_BORDER, $006000, 1
        lda #CMD_FETCH
        jsr xfer
        lda VIC_BORDER
        and #$0F
        ldx #$02
        jsr expect
        lda saved
        sta VIC_BORDER
        jmp done

; The CPU reads another register of the same chip first, so a DMA read that
; returns the chip's previous read shows up as that register's value.
t_io_vic:
        jsr begin
        .byte "DMA FROM VIC", 0
        lda VIC_BORDER
        sta saved
        lda VIC_BG
        clc
        adc #1
        sta VIC_BORDER          ; differs from the background colour
        lda VIC_BORDER
        sta io_want
        lda VIC_BG
        SETX VIC_BORDER, $006100, 1
        jsr io_check
        lda VIC_BG
        lda #$80                ; and 4 bytes from the same register
        sta REU_ACR
        SETX VIC_BORDER, $006100, 4
        lda #CMD_STASH
        jsr xfer
        lda #$00
        sta REU_ACR
        SETX BUF_S, $006100, 4
        lda #CMD_FETCH
        jsr xfer
        ldx #3
@four:  lda BUF_S,x
        stx cp_idx
        ldx io_want
        jsr expect
        lda tfail               ; one report is enough
        bne :+
        ldx cp_idx
        dex
        bpl @four
:
        lda saved
        sta VIC_BORDER
        jmp done

t_io_cia:
        jsr begin
        .byte "DMA FROM CIA", 0
        lda CIA1_DDRA
        sta io_want
        lda CIA1_DDRB
        SETX CIA1_DDRA, $006200, 1
        jsr io_check
        jmp done

t_io_colour:
        jsr begin
        .byte "DMA FROM COLOUR RAM", 0
        lda COLOUR_RAM
        and #$0F
        sta io_want
        SETX COLOUR_RAM, $006300, 1
        lda #CMD_STASH
        jsr xfer
        SETX BUF_S, $006300, 1
        lda #CMD_FETCH
        jsr xfer
        lda BUF_S
        and #$0F
        ldx io_want
        jsr expect
        jmp done

; STASHes the one byte set up by SETX, FETCHes it back and compares it with io_want
io_check:
        lda #CMD_STASH
        jsr xfer
        lda #<BUF_S
        sta xc
        lda #>BUF_S
        sta xc+1
        lda #CMD_FETCH
        jsr xfer
        lda BUF_S
        ldx io_want
        jmp expect

; The end-of-block interrupt shows in the status register; SEI keeps the CPU out
t_irq:
        jsr begin
        .byte "INTERRUPT", 0
        lda REU_STATUS
        lda #$C0
        sta REU_IMASK
        SETX BUF_A, $007000, 16
        lda #CMD_STASH
        jsr xfer
        lda REU_STATUS
        ldx #ST_IRQ|ST_EOB|ST_SIZE
        jsr expect
        lda REU_STATUS
        ldx #ST_SIZE
        jsr expect
        lda #$00
        sta REU_IMASK
        jmp done

t_fast:
        jsr begin
        .byte "FAST MODE", 0
        lda #>BUF_A
        ldy #$66
        jsr fill_page
        lda #>BUF_B
        ldy #$00
        jsr const_page
        lda #$01
        sta VIC_CLK
        SETX BUF_A, $008000, 256
        lda #CMD_STASH
        jsr xfer
        SETX BUF_B, $008000, 256
        lda #CMD_FETCH
        jsr xfer
        lda saved_clk
        sta VIC_CLK
        lda #>BUF_A
        ldy #>BUF_B
        jsr check_pages
        jmp done

; Every page of the size the REU claims, twice: a pattern and its inverse.
; Bytes 0, 1 and 255 of each page carry the page's number, so aliasing shows.
t_whole:
        jsr begin
        .byte "WHOLE REU", 0
        lda #0
        sta werrs
        lda nbanks
        ora full
        beq @skip
        lda #$00
        sta wpat
        jsr whole_pass
        lda #$FF
        sta wpat
        jsr whole_pass
        jsr print
        .byte " ", 0
@skip:  jmp done

whole_pass:
        lda #>BUF_W
        ldy #$5B
        jsr fill_page
        ldx #0
@eor:   lda BUF_W,x
        eor wpat
        sta BUF_W,x
        inx
        bne @eor
        lda #CMD_STASH
        sta wcmd
        jsr whole_sweep
        lda #CMD_VERIFY
        sta wcmd
        jmp whole_sweep

whole_sweep:
        lda #0
        sta pg
        sta pg+1
@page:  lda pg
        sta BUF_W
        sta xr+1
        eor #$FF
        sta BUF_W+255
        lda pg+1
        sta BUF_W+1
        sta xr+2
        lda #0
        sta xr
        sta xl
        lda #1
        sta xl+1
        lda #<BUF_W
        sta xc
        lda #>BUF_W
        sta xc+1
        lda wcmd
        jsr xfer
        lda wcmd
        cmp #CMD_VERIFY
        bne @next
        lda REU_STATUS
        and #ST_FAULT
        beq @next
        lda #1
        sta tfail
        lda werrs
        cmp #3
        bcs @next
        inc werrs
        jsr print
        .byte " BAD $", 0
        lda pg+1
        jsr prhex
        lda pg
        jsr prhex
        jsr print
        .byte "00", 0
@next:  inc pg
        bne @more
        inc pg+1
        lda wcmd                ; a dot every 2 MB verified
        cmp #CMD_VERIFY
        bne @bank
        lda pg+1
        and #$1F
        bne @bank
        lda #'.'
        jsr CHROUT
@bank:  lda pg+1                ; done after the last bank (nbanks 0: after 256)
        cmp nbanks
        bne @more
        rts
@more:  jmp @page

; Leaves a mark with a run count in the REU's last page, for t_mark to find
write_mark:
        ldx #MARK_LEN-1
@copy:  lda mark,x
        sta BUF_S,x
        dex
        bpl @copy
        ldx runs
        inx
        stx BUF_S+MARK_LEN
        jsr mark_addr
        lda #CMD_STASH
        jsr xfer
        jsr print
        .byte "LEFT MARK $", 0
        lda BUF_S+MARK_LEN
        jsr prhex
        jsr print
        .byte ". RESET AND RUN AGAIN.", $0D, 0
        rts

mark_addr:
        SETX BUF_S, $00FF00, MARK_LEN+1
        lda nbanks
        sec
        sbc #1
        sta xr+2
        rts

;-----------------------------------------------------------------------------
; Helpers
;-----------------------------------------------------------------------------

; In: A = command. Writes the shadow copies to the REU, then the command.
xfer:   pha
        lda xc
        sta REU_C128
        lda xc+1
        sta REU_C128+1
        lda xr
        sta REU_ADDR
        lda xr+1
        sta REU_ADDR+1
        lda xr+2
        sta REU_BANK
        lda xl
        sta REU_LEN
        lda xl+1
        sta REU_LEN+1
        pla
        sta REU_CMD
        rts

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

; In: A = value read, X = value wanted
expect: stx ex_want
        cmp ex_want
        beq @ok
        sta ex_got
        jsr fail
        .byte " GOT $", 0
        lda ex_got
        jsr prhex
        jsr print
        .byte " NOT $", 0
        lda ex_want
        jsr prhex
@ok:    rts

; In: A, Y = pages. Out: Z set if they are equal, else X = first differing byte
cmp_pages:
        sta @a+2
        sty @b+2
        ldx #0
@a:     lda $FF00,x
@b:     cmp $FF00,x
        bne @diff
        inx
        bne @a
@diff:  rts

check_pages:
        jsr cmp_pages
        beq @ok
        stx cp_idx
        jsr fail
        .byte " DIFF AT +$", 0
        lda cp_idx
        jsr prhex
@ok:    rts

; In: A = page, Y = first byte. Fills the page with first + 7 * index.
fill_page:
        sta @s+2
        sty fp_v
        ldx #0
@l:     lda fp_v
@s:     sta $FF00,x
        clc
        adc #7
        sta fp_v
        inx
        bne @l
        rts

; In: A = page, Y = value
const_page:
        sta @s+2
        tya
        ldx #0
@s:     sta $FF00,x
        inx
        bne @s
        rts

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

vars:
present:   .byte 0
nfail:     .byte 0
tfail:     .byte 0
banks:     .byte 0
nbanks:    .byte 0              ; 0 with full set: 256 banks
full:      .byte 0
bankreg:   .byte 0
runs:      .byte 0
saved:     .byte 0
saved_clk: .byte 0
wpat:      .byte 0
wcmd:      .byte 0
werrs:     .byte 0
pg:        .word 0
xc:        .word 0
xr:        .res 3
xl:        .word 0
ex_want:   .byte 0
ex_got:    .byte 0
cp_idx:    .byte 0
fp_v:      .byte 0
saved_rcr: .byte 0
io_want:   .byte 0
vars_end:

mark:      .byte "C128 REU TEST"
MARK_LEN = * - mark

.assert start = 7181, error, "the BASIC line's SYS address is wrong"
