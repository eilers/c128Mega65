; REU test for the C128 MEGA65 core's command interface, and for a real Ultimate.
;
; The control target's SAVE_REU and LOAD_REU, through the REU's file, and the DOS target's,
; through a file of its own. It fills the REU's first 64 KB with a pattern by the REU's own
; DMA, saves it, clears it, loads it back and checks it; then saves 4 KB of it into
; UCIREU.BIN on the first storage the root lists, loads that to another REU address and
; checks it there, and removes the file. The REU must be on. SAVE_REU writes the REU's file
; (the Ultimate's REU Preload Image, the core's REU File or /c128/preload.reu), keeping the
; old one as .bu, so it asks first. Messages with paths differ between machines and are shown.
; Build with make_image.py: UCIREU for C128 mode, UCIREU64 (-D C64) for C64 mode.

.include "ucilib.inc"

GETIN   = $FFE4

REU_CMD = $DF01         ; the REU's registers: command, then C128 address, REU address,
REU_C64 = $DF02         ; length, and how the addresses count
REU_REU = $DF04
REU_LEN = $DF07
REU_ACR = $DF0A
STASH   = $90           ; at once, C128 to REU
FETCH   = $91           ; at once, REU to C128

BUF     = $3000         ; 4 KB of RAM in C64 mode, and of RAM bank 0 in bank 15

; Start a command for target t in cmd_buf
.macro CMD_T t, c
        lda #t
        ldx #c
        jsr new_cmd
.endmacro

; Add the C string s to it
.macro ADD s
        lda #<s
        ldy #>s
        jsr add_str
.endmacro

; Add 4 bytes, little-endian
.macro ADD32 v
        lda #<(v)
        jsr add_byte
        lda #>(v)
        jsr add_byte
        lda #^(v)
        jsr add_byte
        lda #0
        jsr add_byte
.endmacro

; The command in cmd_buf, and a check of its reply's data (len bytes at want) and status
.macro ASK want, len, st
        jsr go
        EXPECT want, len, st
.endmacro

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
.ifdef REUTEST                  ; just the REU's DMA and the check, for VICE's REU
        jmp reu_test
.endif
        lda CMDQ
        cmp #$C9
        beq @found
        PRINT no_uci
        rts
@found: jsr release

        ; The REU must be on: LOAD_REU answers 84 while it is off
        PRINT t_on
        CMD_T 4, $08
        jsr add_3
        jsr go
        lda stat_buf
        cmp #'8'
        bne @on
        lda stat_buf+1
        cmp #'4'
        bne @on
        PRINT m_off
        rts
@on:    jsr pass
        jsr show_reu
        PRINT m_warn
@key:   jsr GETIN
        cmp #0
        beq @key

        ; A pattern in the REU's first 64 KB, a 4 KB chunk at a time
        PRINT t_fill
        lda #0
        sta chunk
@fill:  jsr make
        lda #STASH
        jsr dma
        inc chunk
        lda chunk
        cmp #16
        bne @fill
        jsr pass

        ; SAVE_REU, then the 64 KB cleared, then LOAD_REU
        PRINT t_save
        CMD_T 4, $09
        jsr add_3
        jsr go
        jsr want_reu
        ldx #3
:       lda resp_buf,x
        sta saved,x
        dex
        bpl :-
        PRINT t_clear
        jsr clear
        jsr pass
        PRINT t_load
        CMD_T 4, $08
        jsr add_3
        jsr go
        jsr want_reu
        PRINT t_same
        ldx #3
:       lda resp_buf,x
        cmp saved,x
        bne @diff
        dex
        bpl :-
        jsr pass
        jmp @check
@diff:  jsr fail
@check: PRINT t_back
        lda #0
        sta chunk
@cl:    jsr check_chunk
        bne @bad
        inc chunk
        lda chunk
        cmp #16
        bne @cl
        jsr pass
        jmp @dos
@bad:   jsr fail

        ; DOS: 4 KB from REU $1000 into a file, back to REU $2000, cleared first
@dos:   PRINT t_root
        CMD_T 1, $11
        ADD s_slash
        ASK nothing, 0, st_ok
        jsr list_root
        PRINT t_cd_dev
        CMD_T 1, $11
        ADD dev_name
        ASK nothing, 0, st_ok
        PRINT t_nofile
        CMD_T 1, $03
        jsr go                          ; nothing open, whatever an earlier run left
        CMD_T 1, $21
        ADD32 $2000
        ADD32 $1000
        ASK nothing, 0, st_no_file
        PRINT t_open_w
        CMD_T 1, $02
        lda #$0A                        ; FA_WRITE | FA_CREATE_ALWAYS
        jsr add_byte
        ADD s_file
        ASK nothing, 0, st_ok
        PRINT t_dsave
        CMD_T 1, $22
        ADD32 $1000
        ADD32 $1000
        ASK s_saved, 36, st_ok
        CMD_T 1, $03
        jsr go
        lda #2                          ; REU $2000-$2FFF cleared
        sta chunk
        jsr zeros
        lda #STASH
        jsr dma
        PRINT t_open_r
        CMD_T 1, $02
        lda #$01                        ; FA_READ
        jsr add_byte
        ADD s_file
        ASK nothing, 0, st_ok
        PRINT t_dload
        CMD_T 1, $21
        ADD32 $2000
        ADD32 $1000
        ASK s_loaded, 35, st_ok
        PRINT t_dback
        lda #2
        sta chunk
        jsr fetch
        lda #1                          ; chunk 1's pattern, now at REU $2000
        sta chunk
        jsr compare
        bne :+
        jsr pass
        jmp @top
:       jsr fail

        ; Past the end of the REU's memory: 16 bytes of 32, from the file's start
@top:   PRINT t_seek
        CMD_T 1, $06
        ADD32 0
        ASK nothing, 0, st_ok
        PRINT t_trunc
        CMD_T 1, $21
        ADD32 $FFFFF0
        ADD32 $20
        jsr go
        jsr show
        PRINT t_delete
        CMD_T 1, $03
        jsr go
        CMD_T 1, $09
        ADD s_file
        ASK nothing, 0, st_ok

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

.ifdef REUTEST
reu_test:
        PRINT t_fill
        lda #0
        sta chunk
@fill:  jsr make
        lda #STASH
        jsr dma
        inc chunk
        lda chunk
        cmp #16
        bne @fill
        jsr pass
        PRINT t_back
        lda #0
        sta chunk
@cl:    jsr check_chunk
        bne @bad
        inc chunk
        lda chunk
        cmp #16
        bne @cl
        jsr pass
        jmp @clob
@bad:   jsr fail
@clob:  PRINT t_clob                    ; chunk 5 cleared: its check must fail
        lda #5
        sta chunk
        jsr zeros
        lda #STASH
        jsr dma
        jsr check_chunk
        beq :+
        jmp pass
:       jmp fail
t_clob: .byte "A CLEARED CHUNK IS SEEN ", 0
.endif

;-----------------------------------------------------------------------------------------
; The REU, by its own DMA
;-----------------------------------------------------------------------------------------

; Chunk n's pattern into BUF: byte i is i + 3 * (i >> 8) + 17 * n
make:   jsr page_0
@page:  jsr page_start
        ldy #0
@b:     sta (PTR),y
        clc
        adc #1
        iny
        bne @b
        jsr page_next
        bne @page
        rts

; PTR at BUF, page 0
page_0: lda #<BUF
        sta PTR
        lda #>BUF
        sta PTR+1
        lda #0
        sta page
        rts

; A = the pattern's first byte in this page: 3 * page + 17 * n
page_start:
        lda page
        asl
        adc page
        sta first
        lda chunk
        asl
        asl
        asl
        asl
        clc
        adc chunk
        clc
        adc first
        rts

; The next page; zero flag set after the 16th
page_next:
        inc PTR+1
        inc page
        lda page
        cmp #16
        rts

; BUF all zeros
zeros:  lda #<BUF
        sta PTR
        lda #>BUF
        sta PTR+1
        ldx #16
        lda #0
        tay
@b:     sta (PTR),y
        iny
        bne @b
        inc PTR+1
        dex
        bne @b
        rts

; The REU's first 64 KB cleared
clear:  jsr zeros
        lda #0
        sta chunk
@c:     lda #STASH
        jsr dma
        inc chunk
        lda chunk
        cmp #16
        bne @c
        rts

; 4 KB between BUF and REU $0n000 (n = chunk): A is the command
dma:    pha
        lda #<BUF
        sta REU_C64
        lda #>BUF
        sta REU_C64+1
        lda #0
        sta REU_REU
        sta REU_REU+2
        sta REU_ACR
        lda chunk
        asl
        asl
        asl
        asl
        sta REU_REU+1
        lda #<$1000
        sta REU_LEN
        lda #>$1000
        sta REU_LEN+1
        pla
        sta REU_CMD
        rts

fetch:  lda #FETCH
        jmp dma

; Zero flag set if REU chunk n holds its pattern: fetched into BUF, then compared
check_chunk:
        jsr fetch

; Zero flag set if BUF holds chunk n's pattern
compare:
        jsr page_0
@page:  jsr page_start
        sta first
        ldy #0
@b:     lda (PTR),y
        cmp first
        bne @e
        inc first
        iny
        bne @b
        jsr page_next
        bne @page
        lda #0                          ; all equal
@e:     rts

;-----------------------------------------------------------------------------------------
; Commands
;-----------------------------------------------------------------------------------------

new_cmd:
        sta cmd_buf
        stx cmd_buf+1
        lda #2
        sta cmd_pos
        rts

add_byte:
        ldx cmd_pos
        sta cmd_buf,x
        inc cmd_pos
        rts

add_str:
        sta PTR2
        sty PTR2+1
        ldy #0
@l:     lda (PTR2),y
        beq @e
        jsr add_byte
        iny
        bne @l
@e:     rts

; Three bytes more: the control target's LOAD_REU and SAVE_REU take at least 5
add_3:  lda #0
        jsr add_byte
        jsr add_byte
        jmp add_byte

go:     lda #<cmd_buf
        sta PTR
        lda #>cmd_buf
        sta PTR+1
        lda cmd_pos
        sta cmd_len
        lda #0
        sta cmd_len+1
        jmp transact

; The control target's LOAD_REU and SAVE_REU: 00,OK and a result of at least 64 KB as 4
; bytes, then the message, shown
want_reu:
        jsr is_ok
        bne @bad
        lda resp_len
        cmp #5
        bcc @bad
        lda resp_buf+3                  ; at least $10000 (16 MB is $01000000), not -1 or -2
        bmi @bad
        ora resp_buf+2
        beq @bad
        jsr pass
        jmp show_reu
@bad:   jsr fail
show_reu:
        lda #' '
        jsr CHROUT
        ldx #3
:       lda resp_buf,x
        jsr hex8
        dex
        bpl :-
        lda #13
        jsr CHROUT
        lda #' '
        jsr CHROUT
        ldx #4
@l:     cpx resp_len
        beq @st
        lda resp_buf,x
        jsr CHROUT
        inx
        bne @l
@st:    lda #13
        jsr CHROUT
        lda #' '
        jsr CHROUT
        jmp show_status

; Zero flag set if the status is 00,OK
is_ok:  ldx #0
@l:     lda st_ok,x
        cmp stat_buf,x
        bne @e
        inx
        cmp #0
        bne @l
@e:     rts

; The root's listing, shown; into dev_name the first folder that starts with S or U (SD,
; Usb0), or else the first folder
list_root:
        PRINT t_list
        CMD_T 1, $13
        ASK nothing, 0, st_ok
        CMD_T 1, $14
        jsr go_set
        jsr push
        lda #0
        sta dev_name
        sta pref
@entry: jsr wait_reply
        bcs @out
        lda CTRL
        and #S_STATE
        sta state
        jsr read_reply
        lda #C_ACC
        sta CTRL
        lda #' '
        jsr CHROUT
        ldx #1
@name:  cpx resp_len
        beq @shown
        lda resp_buf,x
        jsr CHROUT
        inx
        bne @name
@shown: lda #13
        jsr CHROUT
        lda resp_buf
        and #$10
        beq @next                       ; not a folder
        lda pref
        bne @next
        lda resp_buf+1
        and #$DF
        cmp #'S'
        beq @prefer
        cmp #'U'
        beq @prefer
        lda dev_name
        bne @next
        beq @take
@prefer:
        inc pref
@take:  ldx #1
@copy:  cpx resp_len
        beq @copied
        lda resp_buf,x
        sta dev_name-1,x
        inx
        cpx #31
        bne @copy
@copied:
        lda #0
        sta dev_name-1,x
@next:  lda state
        cmp #$30
        beq @entry
        jsr wait_idle
@out:   lda dev_name
        bne @ok
        PRINT m_no_dev
@ok:    rts

go_set: lda #<cmd_buf
        sta PTR
        lda #>cmd_buf
        sta PTR+1
        lda cmd_pos
        sta cmd_len
        lda #0
        sta cmd_len+1
        rts

;-----------------------------------------------------------------------------------------
; Data
;-----------------------------------------------------------------------------------------

title:      .byte 147, "UCI REU TEST", 13, 13, 0
no_uci:     .byte "NO COMMAND INTERFACE AT $DF1C.", 13, "SWITCH IT ON IN THE MENU.", 13, 0
t_on:       .byte "LOAD_REU: THE REU IS ON ", 0
m_off:      .byte "FAIL", 13, "SWITCH THE REU ON FIRST.", 13, 0
m_warn:     .byte 13, "SAVE_REU OVERWRITES THE REU'S FILE", 13, "(THE OLD ONE IS KEPT AS .BU).", 13
            .byte "ANY KEY TO GO ON", 13, 13, 0
t_fill:     .byte "PATTERN INTO 64 KB OF REU ", 0
t_save:     .byte "SAVE_REU ", 0
t_clear:    .byte "64 KB OF REU CLEARED ", 0
t_load:     .byte "LOAD_REU ", 0
t_same:     .byte "LOAD_REU: AS MANY AS SAVED ", 0
t_back:     .byte "THE PATTERN IS BACK ", 0
t_root:     .byte "CD / ", 0
t_list:     .byte "OPEN_DIR / ", 0
t_cd_dev:   .byte "CD TO ITS FIRST FOLDER ", 0
t_nofile:   .byte "DOS LOAD_REU, NO FILE OPEN ", 0
t_open_w:   .byte "OPEN UCIREU.BIN TO WRITE ", 0
t_dsave:    .byte "DOS SAVE_REU $1000 FROM $1000 ", 0
t_open_r:   .byte "OPEN UCIREU.BIN TO READ ", 0
t_dload:    .byte "DOS LOAD_REU $1000 TO $2000 ", 0
t_dback:    .byte "REU $2000 HOLDS $1000'S ", 0
t_seek:     .byte "FILE_SEEK 0 ", 0
t_trunc:    .byte "DOS LOAD_REU PAST THE TOP: ", 0
t_delete:   .byte "DELETE UCIREU.BIN ", 0
m_no_dev:   .byte " NO FOLDER IN THE ROOT", 13, 0
all_passed: .byte 13, "ALL PASSED", 13, 0
some_failed: .byte 13, "FAILED: $", 0

st_ok:      .byte "00,OK", 0
st_no_file: .byte "85,NO FILE OPEN", 0
s_slash:    .byte "/", 0
s_file:     .byte "UCIREU.BIN", 0
s_saved:    .byte "$  1000 BYTES SAVED FROM REU $  1000"
s_loaded:   .byte "$  1000 BYTES LOADED TO REU $  2000"
nothing:    .byte 0

.include "ucilib.s"

cmd_pos:    .res 1
chunk:      .res 1
page:       .res 1
first:      .res 1
pref:       .res 1
state:      .res 1
saved:      .res 4
dev_name:   .res 32
cmd_buf:    .res 64
