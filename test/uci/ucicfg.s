; LOAD_CONFIG test for the C128 MEGA65 core's command interface, and for a real Ultimate.
;
; Writes settings files into the first folder in the root whose name starts with S or U (SD
; on the core) and loads each with the control target's LOAD_CONFIG: the log and status of
; Gideon's firmware for a store and an item this machine lacks and for errors (after which
; nothing changes); the REU on with 512 KB, at 16 MB where the machine has it, and off, while
; the program runs on (the core resets the C128 for a REU change in its menu); drive B off
; and on; drive A a 1571 and a 1541. A last file puts the REU and the drives back as they
; were, then the file is deleted. The REU's size is found by its own DMA, a byte in each
; bank it tries, and those bytes are put back.
; Build with make_image.py: UCICFG for C128 mode, UCICFG64 (-D C64) for C64 mode.

.include "ucilib.inc"

REU_CMD = $DF01         ; the REU's registers: command, then C128 address, REU address,
REU_C64 = $DF02         ; length, and how the addresses count
REU_REU = $DF04
REU_LEN = $DF07
REU_ACR = $DF0A
STASH   = $90           ; at once, C128 to REU
FETCH   = $91           ; at once, REU to C128
PROBE   = $3F00         ; the byte it moves: RAM in C64 mode, RAM bank 0 in bank 15

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

; The reply's data against len bytes at want and its status against st; the data is shown
; if it fails
.macro LOG want, len, st
        lda fails
        sta fails_was
        EXPECT want, len, st
        jsr show_if_failed
        jsr show_gone
.endmacro

; The file text through LOAD_CONFIG, and LOG
.macro CFG text, want, len, st
        lda #<text
        ldy #>text
        jsr load_cfg
        LOG want, len, st
.endmacro

; Add the C string s to the file in rst_buf
.macro CAT s
        lda #<s
        ldy #>s
        jsr cat
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
        sta absent
        sta absent+1
        PRINT title
.ifdef PROBETEST                ; just the REU's size, for VICE's REU
        jmp probe_test
.endif
        lda CMDQ
        cmp #$C9
        beq @found
        PRINT no_uci
        rts
@found: jsr release

        ; Into the folder, and how things are now
        CMD_T 1, $11
        ADD s_slash
        jsr go
        jsr list_root
        lda dev_name
        bne :+
        rts
:       CMD_T 1, $11
        ADD dev_name
        jsr go
        jsr make_path
        jsr state
        lda reu_banks
        sta was_banks
        lda cur_a
        sta was_a
        lda cur_b
        sta was_b
        PRINT t_now
        jsr show_state

        PRINT t_nofile
        CMD_T 4, $50
        ADD s_nosuch
        jsr add_0
        LOG nothing, 0, st_88

        PRINT t_lacks
        CFG f_lacks, l_lacks, l_lacks_end - l_lacks, st_ok

        ; The REU, while this program runs
        PRINT t_on
        CFG f_on, nothing, 0, st_ok
        PRINT t_512
        jsr probe
        cmp #8
        jsr eq_result
        PRINT t_16
        lda #<f_16
        ldy #>f_16
        jsr load_cfg
        lda stat_buf
        cmp #'8'
        bne @has16
        PRINT m_no16
        LOG l_16, l_16_end - l_16, st_89
        jmp @off
@has16: LOG nothing, 0, st_ok
        PRINT t_16b
        jsr probe
        cmp #$FF
        jsr eq_result
@off:   PRINT t_off
        CFG f_off, nothing, 0, st_ok
        PRINT t_gone
        jsr probe
        jsr eq_result
        PRINT t_err
        CFG f_err, l_err, l_err_end - l_err, st_89
        PRINT t_same
        jsr probe
        jsr eq_result

        ; Drive B off and on, drive A's type
        PRINT t_boff
        CFG f_boff, nothing, 0, st_ok
        PRINT t_boff2
        jsr state
        lda cur_b
        jsr eq_result
        PRINT t_bon
        CFG f_bon, nothing, 0, st_ok
        PRINT t_bon2
        jsr state
        lda cur_b
        cmp #1
        jsr eq_result
        lda was_a
        cmp #2
        bcc @types
        PRINT m_a1581
        jmp @back
@types: PRINT t_a71
        CFG f_a71, nothing, 0, st_ok
        PRINT t_a71b
        jsr state
        lda cur_a
        cmp #1
        jsr eq_result
        PRINT t_a41
        CFG f_a41, nothing, 0, st_ok
        PRINT t_a41b
        jsr state
        lda cur_a
        jsr eq_result

        ; As it was
@back:  PRINT t_back
        jsr make_restore
        lda #<rst_buf
        ldy #>rst_buf
        jsr load_cfg
        LOG nothing, 0, st_ok
        PRINT t_back2
        jsr state
        lda reu_banks
        cmp was_banks
        bne @diff
        lda cur_a
        cmp was_a
        bne @diff
        lda cur_b
        cmp was_b
        bne @diff
        jsr pass
        jmp @del
@diff:  jsr fail
        jsr show_state
@del:   PRINT t_delete
        CMD_T 1, $09
        ADD s_file
        jsr go
        jsr is_ok
        jsr eq_result

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

.ifdef PROBETEST
; A byte at the start of banks 0, 2, 4 ... 128, the size, and the same bytes there after
probe_test:
        ldy #7
@set:   tya
        ora #$30
        ldx try_banks,y
        jsr stash1
        dey
        bpl @set
        jsr read_banks
        ldy #7
@keep:  lda bank_bytes,y
        sta before,y
        dey
        bpl @keep
        jsr probe
        sta cur_b
        PRINT m_reu
        lda cur_b
        bne :+
        PRINT m_off
        jmp @kept
:       jsr size_text
        jsr puts
@kept:  lda #13
        jsr CHROUT
        PRINT t_kept
        jsr read_banks
        ldy #7
@cmp:   lda bank_bytes,y
        cmp before,y
        bne @res
        dey
        bpl @cmp
        lda #0                          ; all the same
@res:   jmp eq_result

read_banks:
        ldy #7
@l:     ldx try_banks,y
        sty bank
        jsr fetch1
        ldy bank
        sta bank_bytes,y
        dey
        bpl @l
        rts

try_banks:  .byte 0, 2, 4, 8, 16, 32, 64, 128
t_kept:     .byte "ITS BYTES KEPT ", 0
.endif

; PASS if the zero flag is set, else FAIL
eq_result:
        beq :+
        jmp fail
:       jmp pass

;-----------------------------------------------------------------------------------------
; The settings file: written as UCICFG.CFG in the folder, then LOAD_CONFIG with its path
;-----------------------------------------------------------------------------------------

; A/Y: the file's text, a C string
load_cfg:
        sta text
        sty text+1
        CMD_T 1, $02
        lda #$0A                        ; FA_WRITE | FA_CREATE_ALWAYS
        jsr add_byte
        ADD s_file
        jsr go
        CMD_T 1, $05                    ; WRITE_DATA: two bytes, then the data
        lda #0
        jsr add_byte
        jsr add_byte
        lda text
        ldy text+1
        jsr add_str
        jsr go
        CMD_T 1, $03
        jsr go
        CMD_T 4, $50
        ADD cfg_path                    ; with its 0: his LOAD_CONFIG doesn't end the name
add_0:  lda #0
        jsr add_byte
        jmp go

; cfg_path: /, the folder, /UCICFG.CFG
make_path:
        ldx #0
        lda #'/'
        sta cfg_path
        inx
        ldy #0
@dev:   lda dev_name,y
        beq @file
        sta cfg_path,x
        inx
        iny
        bne @dev
@file:  lda #'/'
        sta cfg_path,x
        inx
        ldy #0
@name:  lda s_file,y
        sta cfg_path,x
        beq @e
        inx
        iny
        bne @name
@e:     rts

; The REU, drives A and B as they were, into rst_buf
make_restore:
        lda #0
        sta rst_n
        CAT s_c64
        lda was_banks
        bne @on
        CAT s_reu_off
        jmp @a
@on:    CAT s_reu_on
        lda was_banks
        jsr size_text
        jsr cat
        CAT s_nl
@a:     lda was_a
        cmp #2
        bcs @b
        CAT s_drive_a
        lda was_a
        beq :+
        CAT s_1571
        jmp @b
:       CAT s_1541
@b:     CAT s_drive_b
        lda was_b
        beq :+
        CAT s_enabled
        jmp @end
:       CAT s_disabled
@end:   ldx rst_n
        lda #0
        sta rst_buf,x
        rts

cat:    sta PTR2
        sty PTR2+1
        ldy #0
        ldx rst_n
@l:     lda (PTR2),y
        beq @e
        sta rst_buf,x
        inx
        iny
        bne @l
@e:     stx rst_n
        rts

;-----------------------------------------------------------------------------------------
; The REU and the drives as they are
;-----------------------------------------------------------------------------------------

; reu_banks (and A): 0 no REU at $DF00; else the 64 KB banks after which REU address 0 comes
; again (2 for 128 KB to 128 for 8 MB), $FF for none of those (16 MB)
probe:  lda #$55
        sta REU_C64
        cmp REU_C64
        bne @none
        lda #$AA
        sta REU_C64
        cmp REU_C64
        bne @none
        lda #2
        sta bank
@try:   ldx #0
        jsr fetch1
        sta save0
        ldx bank
        jsr fetch1
        sta saveb
        ldx #0
        lda #$A5
        jsr stash1
        ldx bank
        lda #$5A
        jsr stash1
        ldx #0
        jsr fetch1
        sta got
        ldx bank                        ; after bank 0's byte: where there is no memory (a 1764
        jsr fetch1                      ; past 256 KB), VICE's bus still holds that one
        eor #$5A
        sta lost
        ldx bank                        ; put back: bank 0 last, which a wrap makes the same
        lda saveb
        jsr stash1
        ldx #0
        lda save0
        jsr stash1
        lda lost
        bne @wrap                       ; bank b holds nothing
        lda got
        cmp #$A5
        bne @wrap                       ; bank 0 holds bank b's byte
        asl bank
        bne @try
        lda #$FF
        bne @set
@wrap:  lda bank
        bne @set
@none:  lda #0
@set:   sta reu_banks
        rts

; A byte between PROBE and REU address X:0000: A to there (stash1), or from there into A
stash1: sta PROBE
        lda #STASH
        bne dma1
fetch1: lda #FETCH
        jsr dma1
        lda PROBE
        rts
dma1:   pha
        lda #<PROBE
        sta REU_C64
        lda #>PROBE
        sta REU_C64+1
        lda #0
        sta REU_REU
        sta REU_REU+1
        sta REU_LEN+1
        sta REU_ACR
        stx REU_REU+2
        lda #1
        sta REU_LEN
        pla
        sta REU_CMD
        rts

; The REU (probe) and GET_DRVINFO: cur_a drive A's type, cur_b drive B's power
state:  jsr probe
        CMD_T 4, $29
        jsr go
        lda resp_buf+1
        sta cur_a
        lda resp_buf+6
        sta cur_b
        rts

show_state:
        PRINT m_reu
        lda reu_banks
        bne :+
        PRINT m_off
        jmp @a
:       jsr size_text
        jsr puts
@a:     PRINT m_a
        lda cur_a
        cmp #3
        bcc :+
        lda #3
:       asl
        tax
        lda types,x
        ldy types+1,x
        jsr puts
        PRINT m_b
        lda cur_b
        bne :+
        PRINT m_off
        jmp @e
:       PRINT m_on
@e:     lda #13
        jmp CHROUT

; A: REU banks, not 0: A/Y his REU Size text
size_text:
        ldx #7
@l:     cmp banks,x
        beq @e
        dex
        bne @l
@e:     txa
        asl
        tax
        lda sizes,x
        ldy sizes+1,x
        rts

;-----------------------------------------------------------------------------------------
; Commands and replies
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

go:     lda #<cmd_buf
        sta PTR
        lda #>cmd_buf
        sta PTR+1
        lda cmd_pos
        sta cmd_len
        lda #0
        sta cmd_len+1
        jmp transact

; Zero flag set if the status is 00,OK
is_ok:  ldx #0
@l:     lda st_ok,x
        cmp stat_buf,x
        bne @e
        inx
        cmp #0
        bne @l
@e:     rts

; After LOG: the reply's data, in upper case and a line at a time, and its status
show_if_failed:
        lda fails
        cmp fails_was
        bne :+
        rts
:       ldx #0
@l:     cpx resp_len
        beq @st
        lda resp_buf,x
        cmp #10
        bne :+
        lda #13
        bne @out
:       cmp #'a'
        bcc @out
        cmp #'z' + 1
        bcs @out
        and #$DF
@out:   jsr CHROUT
        inx
        bne @l
@st:    lda #' '
        jsr CHROUT
        jmp show_status

; After LOG: the polls that found no interface at $DF1C since the last time, if any (an
; Ultimate's while it switches settings)
show_gone:
        lda absent
        ora absent+1
        beq @e
        PRINT m_gone
        lda absent+1
        jsr hex8
        lda absent
        jsr hex8
        PRINT m_polls
        lda #0
        sta absent
        sta absent+1
@e:     rts

; The root's listing, shown; into dev_name the first folder that starts with S or U (SD,
; Usb0), or else the first folder
list_root:
        PRINT t_list
        CMD_T 1, $13
        jsr go
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
        sta dstate
        jsr read_reply
        lda #C_ACC
        sta CTRL
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
@next:  lda dstate
        cmp #$30
        beq @entry
        jsr wait_idle
@out:   lda dev_name
        bne @ok
        PRINT m_no_dev
        rts
@ok:    lda #<dev_name
        ldy #>dev_name
        jsr puts
        lda #13
        jmp CHROUT

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

title:      .byte 147, "UCI LOAD_CONFIG TEST", 13, 13, 0
no_uci:     .byte "NO COMMAND INTERFACE AT $DF1C.", 13, "SWITCH IT ON IN THE MENU.", 13, 0
t_list:     .byte "THE FOLDER: ", 0
m_no_dev:   .byte "NO FOLDER IN THE ROOT", 13, 0
t_now:      .byte "NOW:", 13, 0
t_nofile:   .byte "NO SUCH FILE ", 0
t_lacks:    .byte "A STORE, AN ITEM IT LACKS ", 0
t_on:       .byte "REU ON, 512 KB ", 0
t_512:      .byte "STILL RUNNING, 512 KB ", 0
t_16:       .byte "REU SIZE 16 MB ", 0
m_no16:     .byte "(NOT HERE) ", 0
t_16b:      .byte "16 MB AT $DF00 ", 0
t_off:      .byte "REU OFF ", 0
t_gone:     .byte "NO REU AT $DF00 ", 0
t_err:      .byte "ERRORS ", 0
t_same:     .byte "NOTHING CHANGED ", 0
t_boff:     .byte "DRIVE B OFF ", 0
t_boff2:    .byte "GET_DRVINFO: B OFF ", 0
t_bon:      .byte "DRIVE B ON ", 0
t_bon2:     .byte "GET_DRVINFO: B ON ", 0
m_a1581:    .byte "DRIVE A IS A 1581: TYPES SKIPPED", 13, 0
t_a71:      .byte "DRIVE A 1571 ", 0
t_a71b:     .byte "GET_DRVINFO: A 1571 ", 0
t_a41:      .byte "DRIVE A 1541 ", 0
t_a41b:     .byte "GET_DRVINFO: A 1541 ", 0
t_back:     .byte "BACK AS IT WAS ", 0
t_back2:    .byte "REU, DRIVES AS BEFORE ", 0
t_delete:   .byte "DELETE UCICFG.CFG ", 0
m_reu:      .byte " REU ", 0
m_a:        .byte ", A ", 0
m_b:        .byte ", B ", 0
m_off:      .byte "OFF", 0
m_on:       .byte "ON", 0
m_gone:     .byte " NO INTERFACE FOR $", 0
m_polls:    .byte " POLLS", 13, 0
all_passed: .byte 13, "ALL PASSED", 13, 0
some_failed: .byte 13, "FAILED: $", 0

types:      .word s_t41, s_t71, s_t81, s_t__
s_t41:      .byte "1541", 0
s_t71:      .byte "1571", 0
s_t81:      .byte "1581", 0
s_t__:      .byte "?", 0
banks:      .byte $FF, 2, 4, 8, 16, 32, 64, 128
sizes:      .word s_16m, s_128k, s_256k, s_512k, s_1m, s_2m, s_4m, s_8m
s_128k:     .byte "128 KB", 0
s_256k:     .byte "256 KB", 0
s_512k:     .byte "512 KB", 0
s_1m:       .byte "1 MB", 0
s_2m:       .byte "2 MB", 0
s_4m:       .byte "4 MB", 0
s_8m:       .byte "8 MB", 0
s_16m:      .byte "16 MB", 0

st_ok:      .byte "00,OK", 0
st_88:      .byte "88,CANNOT OPEN CONFIG FILE", 0
st_89:      .byte "89,CONFIG FILE HAD ERRORS", 0
s_slash:    .byte "/", 0
s_file:     .byte "UCICFG.CFG", 0
s_nosuch:   .byte "/NOSUCH/UCICFG.CFG", 0
nothing:    .byte 0

; The files, and the logs his firmware gives for them
f_lacks:    .byte "# UCICFG", 10
            .byte "[No Such Store]", 10
            .byte "Anything=1", 10
            .byte "[C64 and Cartridge Settings]", 10
            .byte "No Such Item=1", 10, 0
l_lacks:    .byte "Line 2: Store name 'No Such Store' not found; section ignored.", 10
            .byte "Line 5: Item 'No Such Item' not found in this store [C64 and Cartridge Settings]; ignored.", 10
l_lacks_end:
f_on:       .byte "[C64 and Cartridge Settings]", 10
            .byte "RAM Expansion Unit=Enabled", 10
            .byte "REU Size=512 KB", 10, 0
f_16:       .byte "[C64 and Cartridge Settings]", 10
            .byte "REU Size=16 MB", 10, 0
l_16:       .byte "Line 2: Value '16 MB' is not a valid choice for item REU Size", 10
l_16_end:
f_off:      .byte "[C64 and Cartridge Settings]", 10
            .byte "RAM Expansion Unit=Disabled", 10, 0
f_err:      .byte "Orphan=1", 10
            .byte "[C64 and Cartridge Settings]", 10
            .byte "RAM Expansion Unit=Enabled", 10
            .byte "REU Size=3 MB", 10
            .byte "=5", 10, 0
l_err:      .byte "Line 1: Not inside valid store.", 10
            .byte "Line 4: Value '3 MB' is not a valid choice for item REU Size", 10
            .byte "Line 5: No item name.", 10
l_err_end:
f_boff:     .byte "[Drive B Settings]", 10
            .byte "Drive=Disabled", 10, 0
f_bon:      .byte "[Drive B Settings]", 10
            .byte "Drive=Enabled", 10, 0
f_a71:      .byte "[Drive A Settings]", 10
            .byte "Drive Type=1571", 10, 0
f_a41:      .byte "[Drive A Settings]", 10
            .byte "Drive Type=1541", 10, 0
s_c64:      .byte "[C64 and Cartridge Settings]", 10, 0
s_reu_off:  .byte "RAM Expansion Unit=Disabled", 10, 0
s_reu_on:   .byte "RAM Expansion Unit=Enabled", 10, "REU Size=", 0
s_nl:       .byte 10, 0
s_drive_a:  .byte "[Drive A Settings]", 10, "Drive Type=", 0
s_1541:     .byte "1541", 10, 0
s_1571:     .byte "1571", 10, 0
s_drive_b:  .byte "[Drive B Settings]", 10, "Drive=", 0
s_enabled:  .byte "Enabled", 10, 0
s_disabled: .byte "Disabled", 10, 0

.include "ucilib.s"

cmd_pos:    .res 1
text:       .res 2
fails_was:  .res 1
bank:       .res 1
save0:      .res 1
saveb:      .res 1
got:        .res 1
lost:       .res 1
reu_banks:  .res 1
cur_a:      .res 1
cur_b:      .res 1
was_banks:  .res 1
was_a:      .res 1
was_b:      .res 1
pref:       .res 1
dstate:     .res 1
rst_n:      .res 1
dev_name:   .res 32
cfg_path:   .res 48
cmd_buf:    .res 256
rst_buf:    .res 200
.ifdef PROBETEST
before:     .res 8
bank_bytes: .res 8
.endif
