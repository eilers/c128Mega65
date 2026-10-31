; DOS target test for the C128 MEGA65 core's command interface, and for a real Ultimate.
;
; Runs the DOS commands of target 1 in the root's first folder starting with S or U, else its
; first (SD on the core, Usb0 or SD on an Ultimate): a folder UCITEST with files that it writes,
; reads, seeks, renames, copies and deletes, and a D64 it makes and mounts on drive 9 and
; reads the header of over the IEC bus. It removes what it made. Every line ends in PASS or
; FAIL; names and times are shown, so two runs can be compared.
; Build with make_image.py: UCIDOS for C128 mode, UCIDOS64 (-D C64) for C64 mode.

.include "ucilib.inc"

SETBNK  = $FF68         ; C128 only: the banks of the data and the file name
SETLFS  = $FFBA
SETNAM  = $FFBD
OPEN    = $FFC0
CLOSE   = $FFC3
CHKIN   = $FFC6
CLRCHN  = $FFCC
CHRIN   = $FFCF
READST  = $FFB7

D64_SIZE = 174848
D64_HEAD = $16500       ; track 18: the BAM and the first directory sector
CHUNK    = 888          ; bytes a WRITE_DATA carries here

; Start a command for target 1 in cmd_buf
.macro DOS c
        lda #c
        jsr dos_cmd
.endmacro

; Add the C string s to it
.macro ADD s
        lda #<s
        ldy #>s
        jsr add_str
.endmacro

; FILE_STAT of the C string s, sent with its 0 byte: an Ultimate's FILE_STAT doesn't end
; the name itself, so it runs on into whatever an earlier command left in its buffer
.macro STAT s
        DOS $08
        ADD s
        lda #0
        jsr add_byte
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
.ifdef HDRTEST                  ; just the header of drive 9's image, for VICE
        PRINT t_header
        jsr read_header
        rts
.endif
        lda CMDQ
        cmp #$C9
        beq @found
        PRINT no_uci
        rts
@found: jsr release
        jsr fill_pattern

        ; The root and its first folder
        PRINT t_root
        DOS $11
        ADD s_slash
        jsr dos_go
        jsr want_ok
        PRINT t_list
        DOS $13
        jsr dos_go
        jsr want_ok
        jsr list_root
        PRINT t_cd_dev
        DOS $11
        ADD dev_name
        jsr dos_go
        jsr want_ok
        PRINT t_path
        DOS $12
        jsr dos_go
        jsr show

        ; A folder of our own
        PRINT t_mkdir
        DOS $16
        ADD s_folder
        jsr dos_go
        jsr want_ok_or_exists
        PRINT t_cd
        DOS $11
        ADD s_folder
        jsr dos_go
        jsr want_ok
        jsr tidy

        ; A file of 900 bytes: written in three parts, then read back in two blocks
        PRINT t_open_w
        DOS $02
        lda #$0A                ; FA_WRITE | FA_CREATE_ALWAYS
        jsr add_byte
        ADD s_file
        jsr dos_go
        jsr want_ok
        PRINT t_write
        jsr write_parts
        PRINT t_close
        DOS $03
        jsr dos_go
        jsr want_ok
        PRINT t_stat
        STAT s_file
        jsr dos_go
        jsr check_900
        PRINT t_open_r
        DOS $02
        lda #$01                ; FA_READ
        jsr add_byte
        ADD s_file
        jsr dos_go
        jsr want_ok
        PRINT t_read
        DOS $04
        lda #<900
        jsr add_byte
        lda #>900
        jsr add_byte
        jsr dos_all
        jsr check_read
        PRINT t_seek
        DOS $06
        lda #<700
        jsr add_byte
        lda #>700
        jsr add_byte
        lda #0
        jsr add_byte
        jsr add_byte
        jsr dos_go
        jsr want_ok
        PRINT t_read10
        DOS $04
        lda #10
        jsr add_byte
        lda #0
        jsr add_byte
        jsr dos_all
        jsr check_read10
        PRINT t_info
        DOS $07
        jsr dos_go
        jsr check_900
        DOS $03
        jsr dos_go

        ; Rename, copy into a folder, delete
        PRINT t_rename
        DOS $0A
        ADD s_file
        lda #0
        jsr add_byte
        ADD s_file2
        jsr dos_go
        jsr want_ok
        PRINT t_gone
        STAT s_file
        jsr dos_go
        EXPECT resp_buf, 0, st_not_found
        PRINT t_mksub
        DOS $16
        ADD s_sub
        jsr dos_go
        jsr want_ok_or_exists
        PRINT t_copy_rel                ; the folder is named from the root, as fcopy takes it
        DOS $0B
        ADD s_file2
        lda #0
        jsr add_byte
        ADD s_sub
        jsr dos_go
        EXPECT resp_buf, 0, st_no_path
        PRINT t_copy
        DOS $0B
        ADD s_file2
        lda #0
        jsr add_byte
        ADD s_slash
        ADD dev_name
        ADD s_sub_abs
        jsr dos_go
        jsr want_ok
        PRINT t_stat_copy
        STAT s_sub_file2
        jsr dos_go
        jsr check_900
        PRINT t_del_copy
        DOS $09
        ADD s_sub_file2
        jsr dos_go
        jsr want_ok
        PRINT t_del_sub
        DOS $09
        ADD s_sub
        jsr dos_go
        jsr want_ok

        ; The folder's listing: the one file, then nothing
        PRINT t_dir1
        DOS $13
        jsr dos_go
        jsr want_ok
        PRINT t_entries
        DOS $14
        jsr dos_all
        jsr check_one_entry
        PRINT t_del
        DOS $09
        ADD s_file2
        jsr dos_go
        jsr want_ok
        PRINT t_empty
        DOS $13
        jsr dos_go
        EXPECT resp_buf, 0, st_empty

        ; A D64 of our own on drive 9, its header read over the IEC bus
        PRINT t_d64
        jsr make_d64
        PRINT t_mount
        DOS $23
        lda #9
        jsr add_byte
        ADD s_d64
        jsr dos_go
        jsr show
        lda stat_buf
        cmp #'0'
        bne @no_mount
        lda stat_buf+1
        cmp #'0'
        bne @no_mount
        jsr pass
        PRINT t_header
        jsr read_header
        PRINT t_umount
        DOS $24
        lda #9
        jsr add_byte
        jsr dos_go
        jsr want_ok
        jmp @mounted
@no_mount:
        jsr fail
@mounted:
        DOS $09
        ADD s_d64
        jsr dos_go

        ; Mount errors
        PRINT t_drive10
        DOS $23
        lda #10
        jsr add_byte
        ADD s_d64
        jsr dos_go
        EXPECT resp_buf, 0, st_no_drive
        PRINT t_no_file
        DOS $23
        lda #9
        jsr add_byte
        ADD s_d64
        jsr dos_go
        EXPECT resp_buf, 0, st_not_found

        ; Tidy up, and the clock
        PRINT t_up
        DOS $11
        ADD s_dotdot
        jsr dos_go
        jsr want_ok
        PRINT t_rmdir
        DOS $09
        ADD s_folder
        jsr dos_go
        jsr want_ok
        PRINT t_time
        DOS $26
        lda #1
        jsr add_byte
        jsr dos_go
        jsr show

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

;-----------------------------------------------------------------------------------------
; Building and sending DOS commands
;-----------------------------------------------------------------------------------------

dos_cmd:
        sta cmd_buf+1
        lda #1
        sta cmd_buf
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

; cmd_buf, cmd_pos bytes: one block (dos_go), or all of them into resp_buf (dos_all),
; with the number in blocks
dos_go:
        jsr dos_set
        jmp transact

dos_set:
        lda #<cmd_buf
        sta PTR
        lda #>cmd_buf
        sta PTR+1
        lda cmd_pos
        sta cmd_len
        lda #0
        sta cmd_len+1
        rts

dos_all:
        jsr dos_set
        jsr push
        lda #0
        sta blocks
        sta total
        sta total+1
@block: jsr wait_reply
        bcs @out
        lda CTRL
        and #S_STATE
        sta state
        jsr read_reply
        lda #C_ACC
        sta CTRL
        inc blocks
        ; append this block to all_buf
        lda #<resp_buf
        sta PTR
        lda #>resp_buf
        sta PTR+1
        clc
        lda #<all_buf
        adc total
        sta PTR2
        lda #>all_buf
        adc total+1
        sta PTR2+1
        lda resp_len
        sta count
        lda resp_len+1
        sta count+1
        ldy #0
@copy:  lda count
        ora count+1
        beq @next
        lda (PTR),y
        sta (PTR2),y
        inc PTR
        bne :+
        inc PTR+1
:       inc PTR2
        bne :+
        inc PTR2+1
:       lda count
        bne :+
        dec count+1
:       dec count
        jmp @copy
@next:  clc
        lda total
        adc resp_len
        sta total
        lda total+1
        adc resp_len+1
        sta total+1
        lda state
        cmp #$30                ; Data More
        beq @block
        jsr wait_idle
@out:   rts

;-----------------------------------------------------------------------------------------
; Checks
;-----------------------------------------------------------------------------------------

want_ok:
        EXPECT resp_buf, 0, st_ok
        rts

; MKDIR: a folder from an earlier run is fine
want_ok_or_exists:
        lda stat_buf
        cmp #'F'
        bne want_ok
        EXPECT resp_buf, 0, st_exists
        rts

; FILE_STAT / FILE_INFO: status 00,OK and a size of 900
check_900:
        lda stat_buf
        cmp #'0'
        bne @bad
        lda resp_len
        cmp #12
        bcc @bad
        lda resp_buf
        cmp #<900
        bne @bad
        lda resp_buf+1
        cmp #>900
        bne @bad
        lda resp_buf+2
        ora resp_buf+3
        bne @bad
        jmp pass
@bad:   jsr fail
        jmp show

; READ_DATA 900: two blocks, the bytes written
check_read:
        lda blocks
        cmp #2
        bne @bad
        lda total
        cmp #<900
        bne @bad
        lda total+1
        cmp #>900
        bne @bad
        ldx #0
@l:     lda all_buf,x
        cmp pattern,x
        bne @bad
        lda all_buf+256,x
        cmp pattern+256,x
        bne @bad
        lda all_buf+512,x
        cmp pattern+512,x
        bne @bad
        inx
        bne @l
        ldx #900-768-1
@l2:    lda all_buf+768,x
        cmp pattern+768,x
        bne @bad
        dex
        bpl @l2
        jmp pass
@bad:   jsr fail
        PRINT m_blocks
        lda blocks
        jsr hex8
        lda #' '
        jsr CHROUT
        lda total+1
        jsr hex8
        lda total
        jsr hex8
        lda #13
        jmp CHROUT

check_read10:
        lda total
        cmp #10
        bne @bad
        ldx #9
@l:     lda all_buf,x
        cmp pattern+700,x
        bne @bad
        dex
        bpl @l
        jmp pass
@bad:   jmp fail

; READ_DIR: one entry, TEST2.BIN
check_one_entry:
        lda blocks
        cmp #1
        bne @bad
        ldx #0
@l:     lda s_file2,x
        beq @end
        cmp all_buf+1,x
        bne @bad
        inx
        bne @l
@end:   inx
        cpx total
        bne @bad
        jmp pass
@bad:   jmp fail

; The root's listing, shown; into dev_name the first folder that starts with S or U (SD,
; Usb0), or else the first folder
list_root:
        DOS $14
        jsr dos_set
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
        beq @next               ; not a folder
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

;-----------------------------------------------------------------------------------------
; Writing
;-----------------------------------------------------------------------------------------

; Three WRITE_DATA of 300 bytes each from pattern
write_parts:
        lda #<pattern
        sta src
        lda #>pattern
        sta src+1
        lda #3
        sta parts
@part:  lda #<300
        sta wlen
        lda #>300
        sta wlen+1
        jsr write_chunk
        jsr want_ok_quiet
        bne @bad
        clc
        lda src
        adc #<300
        sta src
        lda src+1
        adc #>300
        sta src+1
        dec parts
        bne @part
        jmp pass
@bad:   jsr fail
        jmp show_status

; One WRITE_DATA: wlen bytes from src (0: zeros)
write_chunk:
        lda src+1
        beq write_zeros
        lda #1
        sta cmd_buf
        lda #$05
        sta cmd_buf+1
        lda #0
        sta cmd_buf+2
        sta cmd_buf+3
        lda #<(cmd_buf+4)
        sta PTR2
        lda #>(cmd_buf+4)
        sta PTR2+1
        lda src
        sta PTR
        lda src+1
        sta PTR+1
        lda wlen
        sta count
        lda wlen+1
        sta count+1
        ldy #0
@l:     lda count
        ora count+1
        beq @send
        lda (PTR),y
        sta (PTR2),y
        inc PTR
        bne :+
        inc PTR+1
:       inc PTR2
        bne :+
        inc PTR2+1
:       lda count
        bne :+
        dec count+1
:       dec count
        jmp @l
@send:  lda #<cmd_buf
        sta PTR
        lda #>cmd_buf
        sta PTR+1
        clc
        lda wlen
        adc #4
        sta cmd_len
        lda wlen+1
        adc #0
        sta cmd_len+1
        jmp transact

; WRITE_DATA of wlen zeros, straight into the queue: 9 cycles a byte, where a copy through
; cmd_buf takes about 100 (18 s for the D64 at 1 MHz)
write_zeros:
        lda #1
        sta CMDQ
        lda #$05
        sta CMDQ
        lda #0
        sta CMDQ
        sta CMDQ
        ldx wlen+1
        beq @rest
        ldy #0
@page:  sta CMDQ
        dey
        bne @page
        dex
        bne @page
@rest:  ldy wlen
        beq @push
@last:  sta CMDQ
        dey
        bne @last
@push:  lda #C_PUSH
        sta CTRL
        jmp reply

; Delete what a run that stopped half way may have left in UCITEST; the replies don't matter
tidy:   DOS $09
        ADD s_d64
        jsr dos_go
        DOS $09
        ADD s_sub_file2
        jsr dos_go
        DOS $09
        ADD s_sub
        jsr dos_go
        DOS $09
        ADD s_file2
        jmp dos_go

; Zero flag set if the status is 00,OK
want_ok_quiet:
        ldx #0
@l:     lda st_ok,x
        cmp stat_buf,x
        bne @e
        inx
        cmp #0
        bne @l
@e:     rts

; UCITEST.D64: zeros, with an empty disk's BAM and directory on track 18
make_d64:
        DOS $02
        lda #$0A
        jsr add_byte
        ADD s_d64
        jsr dos_go
        jsr want_ok_quiet
        beq :+
        jmp @bad
:
        lda #0
        sta src+1               ; zeros
        lda #<D64_SIZE
        sta left
        lda #>D64_SIZE
        sta left+1
        lda #^D64_SIZE
        sta left+2
@chunk: lda left+2
        bne @full
        lda left+1
        cmp #>CHUNK
        bcc @last
        bne @full
        lda left
        cmp #<CHUNK
        bcc @last
@full:  lda #<CHUNK
        sta wlen
        lda #>CHUNK
        sta wlen+1
        bne @write
@last:  lda left
        sta wlen
        lda left+1
        sta wlen+1
        ora left
        beq @head
@write: jsr write_chunk
        jsr want_ok_quiet
        beq :+
        jmp @bad
:
        sec
        lda left
        sbc wlen
        sta left
        lda left+1
        sbc wlen+1
        sta left+1
        lda left+2
        sbc #0
        sta left+2
        jmp @chunk
@head:  DOS $06                 ; to track 18
        lda #<D64_HEAD
        jsr add_byte
        lda #>D64_HEAD
        jsr add_byte
        lda #^D64_HEAD
        jsr add_byte
        lda #0
        jsr add_byte
        jsr dos_go
        lda #<d64_head
        sta src
        lda #>d64_head
        sta src+1
        lda #<512
        sta wlen
        lda #>512
        sta wlen+1
        jsr write_chunk
        jsr want_ok_quiet
        beq :+
        jmp @bad
:
        DOS $03
        jsr dos_go
        jsr want_ok_quiet
        beq :+
        jmp @bad
:
        jmp pass
@bad:   jsr fail
        jmp show_status

;-----------------------------------------------------------------------------------------
; The disk header over the IEC bus: LOAD"$",9's first line holds the disk's name. A drive
; can take a moment after a mount (an empty drive of the core starts from reset; a 1541
; notices a new disk late), so it tries for about 4 seconds, and says why if it fails.
;-----------------------------------------------------------------------------------------

read_header:
        lda #0
        sta tries
@try:   jsr header_once
        beq @good
        sta why
        lda tries
        cmp #12
        beq @bad
        inc tries
        jsr wait_third
        jmp @try
@good:  jsr pass
        lda tries
        beq @e
        PRINT m_retries
        lda tries
        jsr hex8
        lda #13
        jmp CHROUT
@e:     rts
@bad:   jsr fail
        lda why
        cmp #1
        bne :+
        PRINT m_open
        rts
:       cmp #2
        bne :+
        PRINT m_absent
        rts
:       PRINT m_name                    ; the 16 characters after the first quote
        ldx name_at
        ldy #16
@n:     lda hdr_buf,x
        cmp #'"'
        beq @q
        jsr CHROUT
        inx
        dey
        bne @n
@q:     lda #'"'
        jsr CHROUT
        lda #13
        jmp CHROUT

; One try: A = 0 and Z set if the name is UCI MOUNT, else 1 OPEN failed, 2 CHKIN failed
; (no device), 3 another name
header_once:
.ifndef C64
        lda #0
        tax
        jsr SETBNK
.endif
        lda #1
        ldx #<s_dollar
        ldy #>s_dollar
        jsr SETNAM
        lda #2
        ldx #9
        ldy #0
        jsr SETLFS
        jsr OPEN
        bcc :+
        lda #1
        rts
:       ldx #2
        jsr CHKIN
        bcc :+
        jsr CLRCHN
        lda #2
        jsr CLOSE
        lda #2
        rts
:       lda #0
        sta hdr_at
        sta hdr_buf
@l:     jsr CHRIN
        ldx hdr_at
        sta hdr_buf,x
        jsr READST
        bne @done
        inc hdr_at
        lda hdr_at
        cmp #40
        bne @l
@done:  jsr CLRCHN
        lda #2
        jsr CLOSE
        ldx #0                          ; the name follows the first quote
@q:     lda hdr_buf,x
        cmp #'"'
        beq @name
        inx
        cpx #30
        bne @q
        stx name_at
        lda #3
        rts
@name:  inx
        stx name_at
        ldy #0
@n:     lda s_disk,y
        beq @good
        cmp hdr_buf,x
        bne @other
        inx
        iny
        bne @n
@good:  lda #0
        rts
@other: lda #3
        rts

; About a third of a second at 1 MHz
wait_third:
        ldx #0
        ldy #0
@l:     dex
        bne @l
        dey
        bne @l
        rts

; 900 bytes that differ from block to block
fill_pattern:
        ldx #0
@l:     txa
        sta pattern,x
        eor #$55
        sta pattern+256,x
        eor #$FF
        sta pattern+512,x
        txa
        asl
        sta pattern+768,x
        inx
        bne @l
        rts

;-----------------------------------------------------------------------------------------
; Data
;-----------------------------------------------------------------------------------------

title:      .byte 147, "UCI DOS TEST", 13, 13, 0
no_uci:     .byte "NO COMMAND INTERFACE AT $DF1C.", 13, "SWITCH IT ON IN THE MENU.", 13, 0
t_root:     .byte "CD / ", 0
t_list:     .byte "OPEN_DIR / ", 0
t_cd_dev:   .byte "CD TO THE FIRST ", 0
t_path:     .byte "GET_PATH ", 0
t_mkdir:    .byte "MKDIR UCITEST ", 0
t_cd:       .byte "CD UCITEST ", 0
t_open_w:   .byte "OPEN TEST.BIN TO WRITE ", 0
t_write:    .byte "WRITE 3 X 300 BYTES ", 0
t_close:    .byte "CLOSE ", 0
t_stat:     .byte "FILE_STAT: 900 BYTES ", 0
t_open_r:   .byte "OPEN TO READ ", 0
t_read:     .byte "READ 900: 2 BLOCKS ", 0
t_seek:     .byte "SEEK 700 ", 0
t_read10:   .byte "READ 10 THERE ", 0
t_info:     .byte "FILE_INFO: 900 BYTES ", 0
t_rename:   .byte "RENAME TO TEST2.BIN ", 0
t_gone:     .byte "OLD NAME GONE ", 0
t_mksub:    .byte "MKDIR SUB ", 0
t_copy_rel: .byte "COPY TO SUB: FROM THE ROOT ", 0
t_copy:     .byte "COPY INTO /.../UCITEST/SUB ", 0
t_stat_copy: .byte "THE COPY: 900 BYTES ", 0
t_del_copy: .byte "DELETE THE COPY ", 0
t_del_sub:  .byte "DELETE SUB ", 0
t_dir1:     .byte "OPEN_DIR ", 0
t_entries:  .byte "READ_DIR: TEST2.BIN ", 0
t_del:      .byte "DELETE TEST2.BIN ", 0
t_empty:    .byte "EMPTY FOLDER ", 0
t_d64:      .byte "WRITE UCITEST.D64 ", 0
t_mount:    .byte "MOUNT ON 9 ", 0
t_header:   .byte "DRIVE 9 HEADER ", 0
t_umount:   .byte "UMOUNT 9 ", 0
t_drive10:  .byte "MOUNT ON 10 ", 0
t_no_file:  .byte "MOUNT NOTHING ", 0
t_up:       .byte "CD .. ", 0
t_rmdir:    .byte "DELETE UCITEST ", 0
t_time:     .byte "GET_TIME ", 0
m_blocks:   .byte " BLOCKS, BYTES $", 0
m_no_dev:   .byte " NO FOLDER IN THE ROOT", 13, 0
m_retries:  .byte " AFTER RETRIES: $", 0
m_open:     .byte " OPEN FAILED", 13, 0
m_absent:   .byte " DEVICE NOT PRESENT", 13, 0
m_name:     .byte " ITS NAME: ", 34, 0
all_passed: .byte 13, "ALL PASSED", 13, 0
some_failed: .byte 13, "FAILED: $", 0

st_ok:        .byte "00,OK", 0
st_exists:    .byte "FILE EXISTS", 0
st_not_found: .byte "82,FILE NOT FOUND", 0
st_empty:     .byte "01,DIRECTORY EMPTY", 0
st_no_drive:  .byte "90,DRIVE NOT PRESENT", 0
st_no_path:   .byte "PATH DOESN'T EXIST", 0
s_slash:      .byte "/", 0
s_dotdot:     .byte "..", 0
s_folder:     .byte "UCITEST", 0
s_file:       .byte "TEST.BIN", 0
s_file2:      .byte "TEST2.BIN", 0
s_sub:        .byte "SUB", 0
s_sub_abs:    .byte "/UCITEST/SUB", 0
s_sub_file2:  .byte "SUB/TEST2.BIN", 0
s_d64:        .byte "UCITEST.D64", 0
s_disk:       .byte "UCI MOUNT", 0
s_dollar:     .byte "$"

; The BAM and first directory sector of an empty disk (make_image.py)
d64_head:   .incbin "d64head.bin"

.include "ucilib.s"

cmd_pos:    .res 1
pref:       .res 1
blocks:     .res 1
state:      .res 1
parts:      .res 1
total:      .res 2
src:        .res 2
wlen:       .res 2
left:       .res 3
dev_name:   .res 32
hdr_buf:    .res 41
hdr_at:     .res 1
name_at:    .res 1
tries:      .res 1
why:        .res 1
pattern:    .res 1024
cmd_buf:    .res 1024
all_buf:    .res 1024
