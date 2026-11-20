; 17-start: thousands of 2-byte REU transfers with the display off and on, to see where
; the first byte of a transfer goes wrong with a real 1764 (c128Mega65 #9). C64 mode.
; Each transfer is started a pseudo-random time after the last, so the starts spread over the
; raster; $D011 and $D012 are read four cycles before the command write that starts it.
;
; $2000  JMP entry (SYS 8192)
; $2003  mode (POKE 8195,m):
;        0 prepare, display off: P at $4000, Q at $4100, P to REU $00000
;        1/2 verify, display off/on: 16384 runs, C64 $4000+k against REU $00000+k
;        3/4 stash, display off/on: 2000 runs, $4000+k to REU $01000+2r, read back display off
;        5/6 fetch, display off/on: 2000 runs, REU $00000+k to $6000+2r
;        7/8 swap, display off/on: 2000 runs, $7000+2r (Q) with REU $03000+2r (P)
;        r = run, k = r mod 255, P(i) = 3i and $7E, Q(i) = P(i) or $80 (no $FF in either)
; $2004  results, 16 bit: +0 bad first bytes, +2 bad second bytes (verify: faults at byte 0/1;
;        swap: the C64 side), +4/+6 swap: the REU side, +8 bad runs that started on a badline,
;        +10 runs that did not run; 8 bit: +12 bad runs recorded, +13 last status,
;        +14 prepare: status of a verify with the display off
; $C100  per mode, 64 bytes from $C100+64*(m-1): $D012 of the first 32 bad runs, then $D011
.setcpu "6502"

P       = $4000
Q       = $4100
LTAB    = $9000         ; $D012 at each start (stash, fetch, swap)
MTAB    = $9800         ; $D011 at each start
REC     = $c100
REU     = $df00
ZT      = $f7           ; -> LTAB + r
ZM      = $f9           ; -> MTAB + r
ZD      = $fb           ; data pointer
ZE      = $fd           ; -> this mode's record area
ZF      = $22           ; second data pointer (swap check)
NRUNV   = 16384
NRUN    = 2000

.macro  inc16 addr
        .local skip
        inc addr
        bne skip
        inc addr+1
skip:
.endmacro

.org $2000
        jmp entry
mode:   .byte 0
bad1:   .word 0         ; $2004
bad2:   .word 0
rbad1:  .word 0
rbad2:  .word 0
nbl:    .word 0
notrun: .word 0
nrec:   .byte 0         ; $2010
lstat:  .byte 0
prep:   .byte 0         ; $2012
r:      .word 0
k:      .byte 0
lcap:   .byte 0
mcap:   .byte 0
flag:   .byte 0
tmp:    .byte 0
lfsr:   .byte $a5
ca:     .word 0         ; parameters of xfer
ra:     .word 0
ln:     .word 0
cmd:    .byte 0

entry:  php
        sei
        lda #0
        ldx #13
clr:    sta bad1,x
        dex
        bpl clr
        jsr dispoff
        lda mode
        bne m1
        jmp prepare
m1:     sec
        sbc #1
        tax
        and #3
        lsr
        ror
        ror             ; (m-1 and 3) * 64
        sta ZE
        txa
        lsr
        lsr
        clc
        adc #>REC
        sta ZE+1
        lda mode
        cmp #3
        bcs m3
        jmp verify
m3:     cmp #5
        bcs m5
        jmp stash
m5:     cmp #7
        bcs m7
        jmp fetch
m7:     jmp swap

finish: lda #$1b
        sta $d011
        plp
        rts

; display off, or on in even modes, and let a frame pass
dispoff: lda #$0b
        sta $d011
        jmp frame
dispon: lda mode
        and #1
        bne dsk
        lda #$1b
        sta $d011
        jsr frame
dsk:    rts

; wait for line 100, then for $D012 = 10 (line 266): a frame boundary
frame:  lda $d012
        cmp #100
        bne frame
f2:     lda $d012
        cmp #10
        bne f2
        rts

; a transfer with ca, ra (bank 0), ln and cmd, status to lstat
xfer:   lda #0
        sta REU+9
        sta REU+10
        sta REU+6
        lda ca
        sta REU+2
        lda ca+1
        sta REU+3
        lda ra
        sta REU+4
        lda ra+1
        sta REU+5
        lda ln
        sta REU+7
        lda ln+1
        sta REU+8
        lda REU
        lda cmd
        sta REU+1
        lda REU
        sta lstat
        rts

; 0..63 loops of 5 cycles from an 8-bit LFSR
delay:  lda lfsr
        lsr
        bcc d1
        eor #$b8
d1:     sta lfsr
        and #$3f
        tax
        beq d3
d2:     dex
        bne d2
d3:     rts

; fill 16 pages from ZD with A
fill:   ldx #16
        ldy #0
fl:     sta (ZD),y
        iny
        bne fl
        inc ZD+1
        dex
        bne fl
        rts

; 16 pages from ZD: the table at (ZF) repeated
copy16: ldx #16
        ldy #0
cl:     lda (ZF),y
        sta (ZD),y
        iny
        bne cl
        inc ZD+1
        dex
        bne cl
        rts

; count a bad run: was its start on a badline (display on), and the first 32 lines with $D011
record: lda mcap
        bmi rc1
        and #$10
        beq rc1
        lda lcap
        cmp #$30
        bcc rc1
        cmp #$f8
        bcs rc1
        and #7
        sta tmp
        lda mcap
        and #7
        cmp tmp
        bne rc1
        inc16 nbl
rc1:    ldy nrec
        cpy #32
        bcs rc2
        lda lcap
        sta (ZE),y
        tya
        ora #32
        tay
        lda mcap
        sta (ZE),y
        inc nrec
rc2:    rts

; start of a run loop: r = k = 0, ZT/ZM at the tables
runs0:  lda #0
        sta r
        sta r+1
        sta k
        lda #<LTAB
        sta ZT
        sta ZM
        lda #>LTAB
        sta ZT+1
        lda #>MTAB
        sta ZM+1
        rts

; end of a run: k = k + 1 mod 255, r = r + 1, ZT/ZM + 1; Z set when r = NRUN
next:   inc k
        lda k
        cmp #255
        bne nx1
        lda #0
        sta k
nx1:    inc16 r
        inc16 ZT
        inc16 ZM
        lda r
        cmp #<NRUN
        bne nx2
        lda r+1
        cmp #>NRUN
nx2:    rts

; the start of a run: A = command; status to lstat, $D011/$D012 to mcap/lcap and the tables
go:     ldy REU
        ldy $d011
        ldx $d012
        sta REU+1       ; the REC takes the bus in the next cycle
        stx lcap
        sty mcap
        ldy #0
        txa
        sta (ZT),y
        lda mcap
        sta (ZM),y
        lda REU
        sta lstat
        and #$60
        bne go1
        inc16 notrun
go1:    rts

prepare:
        ldx #0
pp:     txa
        asl
        sta tmp
        txa
        clc
        adc tmp
        and #$7e
        sta P,x
        ora #$80
        sta Q,x
        inx
        bne pp
        lda #<P
        sta ca
        lda #>P
        sta ca+1
        lda #0
        sta ra
        sta ra+1
        sta ln
        lda #1
        sta ln+1
        lda #$90
        sta cmd
        jsr xfer        ; P to REU $00000
        lda #$93
        sta cmd
        jsr xfer        ; and verify it
        lda lstat
        sta prep
        jmp finish

verify: jsr dispon
        jsr runs0
vl:     lda #0
        sta REU+9
        sta REU+10
        sta REU+6
        sta REU+5
        sta REU+8
        lda k
        sta REU+2
        sta REU+4
        lda #>P
        sta REU+3
        lda #2
        sta REU+7
        jsr delay
        lda #$93
        jsr go
        lda lstat
        and #$20
        beq vn
        lda REU+2
        clc
        sbc k           ; where it stopped: 0 = first byte
        bne vf2
        inc16 bad1
        jmp vf3
vf2:    inc16 bad2
vf3:    jsr record
vn:     inc k
        lda k
        cmp #255
        bne vk
        lda #0
        sta k
vk:     inc16 r
        lda r+1
        cmp #>NRUNV
        beq vd
        jmp vl
vd:     jsr dispoff
        jmp finish

; 2-byte runs to or from consecutive C64 or REU addresses: ca/ra start, cmd
stash:  lda #$ee        ; clear REU $01000-$01FFF
        jsr fill5000
        lda #<$5000
        sta ca
        lda #>$5000
        sta ca+1
        lda #0
        sta ra
        sta ln
        lda #$10
        sta ra+1
        sta ln+1
        lda #$90
        sta cmd
        jsr xfer
        jsr dispon
        jsr runs0
        lda #0
        sta ra
        lda #$10
        sta ra+1
sl:     lda #0
        sta REU+9
        sta REU+10
        sta REU+6
        sta REU+8
        lda k
        sta REU+2
        lda #>P
        sta REU+3
        lda ra
        sta REU+4
        lda ra+1
        sta REU+5
        lda #2
        sta REU+7
        jsr delay
        lda #$90
        jsr go
        lda ra
        clc
        adc #2
        sta ra
        bcc sl1
        inc ra+1
sl1:    jsr next
        bne sl
        jsr dispoff
        lda #<$5000     ; read back
        sta ca
        lda #>$5000
        sta ca+1
        lda #0
        sta ra
        sta ln
        lda #$10
        sta ra+1
        sta ln+1
        lda #$91
        sta cmd
        jsr xfer
        lda #<$5000
        sta ZD
        lda #>$5000
        sta ZD+1
        jsr check2
        jmp finish

fill5000: ldx #<$5000
        stx ZD
        ldx #>$5000
        stx ZD+1
        jmp fill

fetch:  lda #<$6000
        sta ZD
        lda #>$6000
        sta ZD+1
        lda #$ee
        jsr fill
        jsr dispon
        jsr runs0
        lda #<$6000
        sta ca
        lda #>$6000
        sta ca+1
fl1:    lda #0
        sta REU+9
        sta REU+10
        sta REU+6
        sta REU+5
        sta REU+8
        lda ca
        sta REU+2
        lda ca+1
        sta REU+3
        lda k
        sta REU+4
        lda #2
        sta REU+7
        jsr delay
        lda #$91
        jsr go
        lda ca
        clc
        adc #2
        sta ca
        bcc fl2
        inc ca+1
fl2:    jsr next
        bne fl1
        jsr dispoff
        lda #<$6000
        sta ZD
        lda #>$6000
        sta ZD+1
        jsr check2
        jmp finish

; stash and fetch: (ZD) + 2r must hold P(k), P(k + 1)
check2: jsr runs0
c2l:    lda #0
        sta flag
        ldx k
        ldy #0
        lda (ZD),y
        cmp P,x
        beq c2a
        inc16 bad1
        inc flag
c2a:    iny
        lda (ZD),y
        cmp P+1,x
        beq c2b
        inc16 bad2
        inc flag
c2b:    lda flag
        beq c2n
        jsr recrun
c2n:    lda ZD
        clc
        adc #2
        sta ZD
        bcc c2c
        inc ZD+1
c2c:    jsr next
        bne c2l
        rts

; record run r from the tables
recrun: ldy #0
        lda (ZT),y
        sta lcap
        lda (ZM),y
        sta mcap
        jmp record

swap:   lda #<$7000     ; C64 side: Q repeated
        sta ZD
        lda #>$7000
        sta ZD+1
        lda #<Q
        sta ZF
        lda #>Q
        sta ZF+1
        jsr copy16
        lda #<$5000     ; REU side: P repeated
        sta ZD
        lda #>$5000
        sta ZD+1
        lda #<P
        sta ZF
        lda #>P
        sta ZF+1
        jsr copy16
        lda #<$5000
        sta ca
        lda #>$5000
        sta ca+1
        lda #0
        sta ra
        sta ln
        lda #$30
        sta ra+1
        lda #$10
        sta ln+1
        lda #$90
        sta cmd
        jsr xfer
        jsr dispon
        jsr runs0
        lda #<$7000
        sta ca
        lda #>$7000
        sta ca+1
        lda #0
        sta ra
        lda #$30
        sta ra+1
sw1:    lda #0
        sta REU+9
        sta REU+10
        sta REU+6
        sta REU+8
        lda ca
        sta REU+2
        lda ca+1
        sta REU+3
        lda ra
        sta REU+4
        lda ra+1
        sta REU+5
        lda #2
        sta REU+7
        jsr delay
        lda #$92
        jsr go
        lda ca
        clc
        adc #2
        sta ca
        bcc sw2
        inc ca+1
sw2:    lda ra
        clc
        adc #2
        sta ra
        bcc sw3
        inc ra+1
sw3:    jsr next
        bne sw1
        jsr dispoff
        lda #<$8000     ; REU side back to $8000
        sta ca
        lda #>$8000
        sta ca+1
        lda #0
        sta ra
        sta ln
        lda #$30
        sta ra+1
        lda #$10
        sta ln+1
        lda #$91
        sta cmd
        jsr xfer
        ; $7000 + j must hold P(j and 255), $8000 + j Q(j and 255), j = 2r and 2r + 1
        jsr runs0
        lda #<$7000
        sta ZD
        lda #>$7000
        sta ZD+1
        lda #<$8000
        sta ZF
        lda #>$8000
        sta ZF+1
sc1:    lda #0
        sta flag
        ldx ZD          ; j and 255, the tables are page aligned
        ldy #0
        lda (ZD),y
        cmp P,x
        beq sc2
        inc16 bad1
        inc flag
sc2:    lda (ZF),y
        cmp Q,x
        beq sc3
        inc16 rbad1
        inc flag
sc3:    iny
        lda (ZD),y
        cmp P+1,x
        beq sc4
        inc16 bad2
        inc flag
sc4:    lda (ZF),y
        cmp Q+1,x
        beq sc5
        inc16 rbad2
        inc flag
sc5:    lda flag
        beq sc6
        jsr recrun
sc6:    lda ZD
        clc
        adc #2
        sta ZD
        sta ZF
        bcc sc7
        inc ZD+1
        inc ZF+1
sc7:    jsr next
        bne sc1
        jmp finish
