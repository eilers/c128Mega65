; Network target test for the C128 MEGA65 core's command interface, and for a real Ultimate.
;
; Runs the commands of target 3: the interface's addresses and wrong parameters, then
; sockets against ucinet_peer.py on a computer on the same network, whose address it asks
; for (TCP and UDP port 6464; TCP port 6465 must be closed there): TCP and UDP reads and
; writes, replies over several blocks, a truncated datagram, a reply given up half way, and
; host names. At the end it leaves four UDP sockets open, which a reset of the C128 must
; close: after a reset, a second run's UDP SOCKETS line shows as many as the first. Every
; line ends in PASS or FAIL; what differs between machines is shown below it.
; Build with make_image.py: UCINET for C128 mode, UCINET64 (-D C64) for C64 mode.

.include "ucilib.inc"

GETIN   = $FFE4
MMUCR   = $FF00         ; C128: the memory configuration
JIFFY   = $A0           ; the jiffy clock, $A0-$A2 (high byte first), C64 and C128 mode

PORT    = 6464          ; ucinet_peer.py's TCP and UDP port
SHUT    = 6465          ; a TCP port nothing listens on
NBUF    = 1600          ; a READ's reply, all its blocks

; Start a command for target 3 in cmd_buf
.macro NET c
        lda #c
        jsr net_cmd
.endmacro

.macro ADDB v
        lda #v
        jsr add_byte
.endmacro

; Add the C string s to it
.macro ADD s
        lda #<s
        ldy #>s
        jsr add_str
.endmacro

; The command in cmd_buf, and a check of its reply's data (len bytes at want) and status
.macro ASK want, len, st
        jsr go
        EXPECT want, len, st
.endmacro

; READ of socket sock, len bytes: every block of the reply into nbuf
.macro READ len
        lda #<(len)
        ldx #>(len)
        jsr read_sock
.endmacro

; Z set if the status is st
.macro STATUS st
        lda #<st
        ldy #>st
        jsr st_eq
.endmacro

; WRITE of the C string s to socket sock, its bytes sent; carry set (and FAIL) if not
.macro REQUEST s
        lda #<s
        ldy #>s
        jsr request
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

; Its buffers reach past $4000: in C128 mode RAM goes there while it runs, in place of
; BASIC's ROM (the KERNAL and I/O stay), and BASIC's bank comes back at the end
start:
.ifndef C64
        lda MMUCR
        pha
        lda #$0E
        sta MMUCR
        jsr main
        pla
        sta MMUCR
        rts
.endif

main:   cld
        lda #0
        sta fails
        sta give_up
        PRINT title
        lda CMDQ
        cmp #$C9
        beq @found
        PRINT no_uci
        rts
@found: jsr release

        ; The target and the interface
        PRINT t_ident
        NET $01
        jsr go
        jsr want_data
        lda resp_buf                    ; "NO TARGET": nothing more to try
        cmp #'N'
        bne @count
        lda resp_buf+1
        cmp #'O'
        bne @count
        PRINT no_net
        jmp done
@count: PRINT t_count
        NET $02
        jsr go
        lda resp_buf
        sta ifn
        beq :+
        lda #1
:       jsr want_n
        PRINT t_ip
        jsr each_ip
        PRINT t_ip_past
        NET $05
        lda ifn
        jsr add_byte
        ASK nothing, 0, st_range
        PRINT t_mac
        NET $04
        jsr add_ifx
        jsr go
        lda #6
        jsr want_n
        PRINT t_ip_short
        NET $05
        ASK nothing, 0, st_invalid
        PRINT t_set
        lda ip12
        ora ip12+1
        ora ip12+2
        ora ip12+3
        bne @set
        PRINT m_no_ip
        jmp @set_short
@set:   NET $06
        jsr add_ifx
        lda #0
        sta idx
@set_l: ldx idx
        lda ip12,x
        jsr add_byte
        inc idx
        lda idx
        cmp #12
        bne @set_l
        ASK nothing, 0, st_ok
        PRINT t_same
        NET $05
        jsr add_ifx
        ASK ip12, 12, st_ok
@set_short:
        PRINT t_set_short
        NET $06
        ADDB 0
        ASK nothing, 0, st_invalid
        PRINT t_setif
        NET $03
        ADDB 0
        ASK nothing, 0, st_unknown

        ; A socket nobody opened, and commands too short
        lda #$7F
        sta sock
        PRINT t_read7f
        READ 1472
        lda #<st_nodata9
        ldy #>st_nodata9
        jsr want_none
        PRINT t_read_big
        READ 1473
        STATUS st_range
        bne :+
        jsr no_reply
:       jsr result_z
        PRINT t_read_short
        NET $10
        ADDB $7F
        ADDB 0
        ASK nothing, 0, st_invalid
        PRINT t_write7f
        NET $11
        ADDB $7F
        ADDB 'X'
        ASK minus1, 2, st_send9
        PRINT t_close7f
        NET $09
        ADDB $7F
        ASK nothing, 0, st_close9
        PRINT t_open_short
        NET $07
        ADDB 0
        ADDB 0
        ASK nothing, 0, st_invalid
        PRINT t_udp_n
        jsr count_sockets

        PRINT t_peer
        jsr input
        lda peer_len
        bne tcp_part
        PRINT m_no_peer
        jmp names

;-----------------------------------------------------------------------------------------
; TCP, against the peer
;-----------------------------------------------------------------------------------------

tcp_part:
        PRINT t_refused
        lda #$07
        ldx #<SHUT
        ldy #>SHUT
        jsr open_peer
        lda #<st_connect
        ldy #>st_connect
        jsr st_starts
        beq @refused
        jsr fail
        jsr show
        jsr is_ok                       ; it connected after all: let it go
        bne @tcp
        lda resp_buf
        sta sock
        jsr close_sock
        jmp @tcp
@refused:
        jsr pass
        jsr show_status

@tcp:   PRINT t_tcp
        lda #$07
        ldx #<PORT
        ldy #>PORT
        jsr open_peer
        jsr take_handle
        bcc :+
        jmp udp_part
:       PRINT t_nothing             ; each READ waits 40 ms for data first
        jsr clock_start
        lda #0
        sta idx
@nl:    READ 64
        lda #<st_nodata11
        ldy #>st_nodata11
        jsr is_none
        bne @nbad
        inc idx
        lda idx
        cmp #10
        bne @nl
        jsr elapsed
        lda den+1
        bne :+
        lda den
        cmp #10                         ; a sixth of a second (an Ultimate's wait is under 40 ms)
        bcc @nfast
:       jsr pass
        jmp @nshow
@nfast: jsr fail
@nshow: PRINT m_indent
        lda den
        ldx den+1
        jsr dec16
        PRINT m_jiffies
        jmp @p200
@nbad:  jsr fail
        jsr show_read

@p200:  PRINT t_p200
        REQUEST r_p200
        bcs @p1420
        lda #<200
        ldx #>200
        sta want
        stx want+1
        lda #64
        ldx #0
        jsr read_all
        jsr show_all

@p1420: PRINT t_p1420
        REQUEST r_p1420
        bcs @up
        lda #<1420
        ldx #>1420
        sta want
        stx want+1
        lda #<1472
        ldx #>1472
        jsr read_all
        jsr show_all

@up:    PRINT t_up
        REQUEST r_e8000
        bcs @down
        jsr clock_start
        lda #0
        sta wpat
        sta idx
@up_l:  lda #<892                       ; 8 x 892, then 864
        ldx #>892
        ldy idx
        cpy #8
        bne :+
        lda #<864
        ldx #>864
:       jsr write_pat
        bcs @up_bad
        inc idx
        lda idx
        cmp #9
        bne @up_l
        jsr read_line
        bcs @up_bad
        lda #<s_ok8000
        ldy #>s_ok8000
        jsr line_eq
        bne @up_bad
        jsr pass
        lda #<(8000 * 60)
        ldx #>(8000 * 60)
        ldy #^(8000 * 60)
        jsr show_rate
        jmp @down
@up_bad:
        jsr fail
        jsr show_line

@down:  PRINT t_down
        REQUEST r_p32k
        bcs @bye
        jsr clock_start
        lda #<32768
        ldx #>32768
        sta want
        stx want+1
        lda #<1472
        ldx #>1472
        jsr read_all
        bcs :+
        jsr pass
        lda #<(32768 * 60)
        ldx #>(32768 * 60)
        ldy #^(32768 * 60)
        jsr show_rate
        jmp @bye
:       jsr show_all

@bye:   PRINT t_bye
        REQUEST r_bye
        bcs @after
        lda #0
        sta waits
@bye_r: READ 64
        STATUS st_nodata11
        bne :+
        inc waits
        lda waits
        cmp #100
        bne @bye_r
:       STATUS st_closed
        bne @bye_bad
        lda nlen
        cmp #2
        bne @bye_bad
        lda nlen+1
        ora nbuf
        ora nbuf+1
        bne @bye_bad
        jsr pass
        jmp @after
@bye_bad:
        jsr fail
        jsr show_read
@after: PRINT t_after
        READ 64
        lda #<st_nodata9
        ldy #>st_nodata9
        jsr want_none

;-----------------------------------------------------------------------------------------
; UDP, against the peer
;-----------------------------------------------------------------------------------------

udp_part:
        PRINT t_udp
        lda #$08
        ldx #<PORT
        ldy #>PORT
        jsr open_peer
        jsr take_handle
        bcc :+
        jmp names
:       PRINT t_u64
        REQUEST u_p64
        bcs @u200
        lda #64
        ldx #0
        jsr dgram_whole

@u200:  PRINT t_u200
        REQUEST u_p200
        bcs @u1420
        lda #0
        sta pat
        lda #64
        sta want
        lda #0
        sta want+1
        lda #64
        ldx #0
        jsr read_one
        lda #<st_trunc200
        ldy #>st_trunc200
        jsr want_dgram
        PRINT t_gone
        READ 64
        lda #<st_nodata11
        ldy #>st_nodata11
        jsr want_none

@u1420: PRINT t_u1420
        REQUEST u_p1420
        bcs @u1472
        lda #<1420
        ldx #>1420
        jsr dgram_whole

@u1472: PRINT t_u1472
        REQUEST u_p1472
        bcs @u1473
        lda #<1472
        ldx #>1472
        jsr dgram_whole

@u1473: PRINT t_u1473
        REQUEST u_p1473
        bcs @aband
        lda #0
        sta idx
@u1473_l:
        READ 1472
        STATUS st_nodata11
        bne @u1473_bad
        inc idx
        lda idx
        cmp #10
        bne @u1473_l
        jsr pass
        jmp @aband
@u1473_bad:
        jsr fail
        jsr show_read

@aband: PRINT t_aband
        REQUEST u_d1420
        bcs @echo
        jsr abandon
        PRINT t_next_cmd
        NET $01
        jsr go
        jsr want_data
        PRINT t_next_dg
        lda #1
        sta pat
        lda #<1420
        ldx #>1420
        sta want
        stx want+1
        jsr read_one
        lda #<st_ok
        ldy #>st_ok
        jsr want_dgram

@echo:  PRINT t_echo
        lda #0
        sta wpat
        lda #<892
        ldx #>892
        jsr write_pat
        bcs @echo_bad
        lda #<892
        ldx #>892
        jsr dgram_whole
        jmp @uclose
@echo_bad:
        jsr fail
        jsr show_hex

@uclose:
        PRINT t_uclose
        NET $09
        lda sock
        jsr add_byte
        ASK nothing, 0, st_ok

;-----------------------------------------------------------------------------------------
; Host names, then sockets left for a reset to close
;-----------------------------------------------------------------------------------------

names:  PRINT t_nosuch
        NET $07
        ADDB 80
        ADDB 0
        ADD s_nosuch
        ASK nothing, 0, st_unresolved
        PRINT t_web
        NET $07
        ADDB 80
        ADDB 0
        ADD s_web
        jsr go
        jsr is_ok
        bne @web_bad
        lda resp_len
        cmp #1
        bne @web_bad
        lda resp_buf
        sta sock
        REQUEST r_head
        bcs @web_close
        jsr read_line
        bcs @web_line
        lda #<s_http
        ldy #>s_http
        jsr line_starts
        bne @web_line
        jsr pass
        PRINT m_indent
        jsr show_line_only
        jmp @web_close
@web_line:
        jsr fail
        jsr show_line
@web_close:
        jsr close_sock
        jmp leave
@web_bad:
        jsr fail
        jsr show

leave:  PRINT t_leave
        lda #0
        sta idx
@l:     jsr open_local
        bcs @bad
        inc idx
        lda idx
        cmp #4
        bne @l
        jsr pass
        PRINT m_leave
        jmp done
@bad:   jsr fail
        jsr show

done:   lda fails
        bne @f
        PRINT all_passed
        rts
@f:     PRINT some_failed
        lda fails
        jsr hex8
        lda #13
        jmp CHROUT

;-----------------------------------------------------------------------------------------
; Commands
;-----------------------------------------------------------------------------------------

net_cmd:
        sta cmd_buf+1
        lda #3
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

go:     lda #<cmd_buf
        sta PTR
        lda #>cmd_buf
        sta PTR+1
        lda cmd_pos
        sta cmd_len
        lda #0
        sta cmd_len+1
        jmp transact

; OPEN_TCP (A = $07) or OPEN_UDP ($08) to the peer, port X (low), Y (high)
open_peer:
        stx port
        sty port+1
        jsr net_cmd
        lda port
        jsr add_byte
        lda port+1
        jsr add_byte
        ADD peer
        jmp go

; OPEN_UDP to 127.0.0.1:9, which needs no peer; carry set if it fails
open_local:
        NET $08
        ADDB 9
        ADDB 0
        ADD s_local
        jsr go
        jsr is_ok
        bne @bad
        lda resp_len
        cmp #1
        bne @bad
        clc
        rts
@bad:   sec
        rts

close_sock:
        NET $09
        lda sock
        jsr add_byte
        jmp go

; OPEN's reply: 00,OK and the socket, into sock; carry set (and FAIL) if not
take_handle:
        jsr is_ok
        bne @bad
        lda resp_len
        cmp #1
        bne @bad
        lda resp_buf
        sta sock
        jsr pass
        PRINT m_sock
        lda sock
        jsr hex8
        lda #13
        jsr CHROUT
        clc
        rts
@bad:   jsr fail
        jsr show
        sec
        rts

; WRITE of the C string at A/Y: its bytes sent and 00,OK, carry clear; else FAIL, carry set
request:
        sta rq
        sty rq+1
        NET $11
        lda sock
        jsr add_byte
        lda rq
        ldy rq+1
        jsr add_str
        lda cmd_pos
        sec
        sbc #3
        sta rq_n
        jsr go
        jsr is_ok
        bne @bad
        lda resp_len
        cmp #2
        bne @bad
        lda resp_buf
        cmp rq_n
        bne @bad
        lda resp_buf+1
        bne @bad
        clc
        rts
@bad:   jsr fail
        PRINT m_write
        jsr show_hex
        sec
        rts

; WRITE of A/X bytes of the pattern, going on from wpat: carry clear if all were sent
write_pat:
        sta wn
        stx wn+1
        lda #3
        sta wbuf
        lda #$11
        sta wbuf+1
        lda sock
        sta wbuf+2
        lda #<(wbuf+3)
        sta PTR2
        lda #>(wbuf+3)
        sta PTR2+1
        lda wn
        sta cnt
        lda wn+1
        sta cnt+1
        ldy #0
@l:     lda cnt
        ora cnt+1
        beq @go
        lda wpat
        sta (PTR2),y
        inc wpat
        lda wpat
        cmp #251
        bne :+
        lda #0
        sta wpat
:       inc PTR2
        bne :+
        inc PTR2+1
:       lda cnt
        bne :+
        dec cnt+1
:       dec cnt
        jmp @l
@go:    lda #<wbuf
        sta PTR
        lda #>wbuf
        sta PTR+1
        clc
        lda wn
        adc #3
        sta cmd_len
        lda wn+1
        adc #0
        sta cmd_len+1
        jsr transact
        jsr is_ok
        bne @bad
        lda resp_len
        cmp #2
        bne @bad
        lda resp_buf
        cmp wn
        bne @bad
        lda resp_buf+1
        cmp wn+1
        bne @bad
        clc
        rts
@bad:   sec
        rts

;-----------------------------------------------------------------------------------------
; READ: every block of a reply into nbuf (nlen bytes), the last block's status in stat_buf,
; how many blocks, and the first one's length and state ($30: Data More). With give_up set,
; a reply in more than one block is given up after its first: ABORT, then CLR_ERR.
;-----------------------------------------------------------------------------------------

read_sock:
        sta rl
        stx rl+1
        NET $10
        lda sock
        jsr add_byte
        lda rl
        jsr add_byte
        lda rl+1
        jsr add_byte
        lda #<cmd_buf
        sta PTR
        lda #>cmd_buf
        sta PTR+1
        lda cmd_pos
        sta cmd_len
        lda #0
        sta cmd_len+1
        jsr push
        lda #0
        sta nlen
        sta nlen+1
        sta blocks
        lda #<nbuf
        sta PTR2
        lda #>nbuf
        sta PTR2+1
@block: jsr wait_reply
        bcc :+
        rts                             ; TIMEOUT in stat_buf
:       lda CTRL
        and #S_STATE
        sta st_now
        lda #0
        sta blen
        sta blen+1
        ldy #0
@data:  lda CTRL
        and #S_DATA
        beq @stat
        lda blen+1
        cmp #4                          ; a block that doesn't end: give up on it
        bcs @stat
        lda RESP
        ldx nlen+1
        cpx #>NBUF
        bcs @count                      ; past nbuf: counted, not kept
        sta (PTR2),y
        inc PTR2
        bne @count
        inc PTR2+1
@count: inc nlen
        bne :+
        inc nlen+1
:       inc blen
        bne @data
        inc blen+1
        jmp @data
@stat:  ldx #0
@sl:    lda CTRL
        and #S_STAT
        beq @send
        lda STATQ
        sta stat_buf,x
        inx
        cpx #255
        bne @sl
@send:  lda #0
        sta stat_buf,x
        lda blocks
        bne @next
        lda blen
        sta blk1
        lda blen+1
        sta blk1+1
        lda st_now
        sta more1
@next:  inc blocks
        lda st_now
        cmp #$30
        bne @last
        lda give_up
        beq @acc
        lda #C_ABORT
        sta CTRL
        jsr wait_idle
        lda #C_CLR
        sta CTRL
        rts
@acc:   lda #C_ACC
        sta CTRL
        jmp @block
@last:  lda #C_ACC
        sta CTRL
        jmp wait_idle

; No data at all came
no_reply:
        lda nlen
        ora nlen+1
        rts

; READs of A/X bytes until want bytes have come: each 00,OK, its header the bytes it carries,
; and the pattern going on from 0; "02,NO DATA: 11" (nothing yet) up to 100 times in all.
; Carry clear if so; reads and the most blocks a reply took are counted.
read_all:
        sta rl2
        stx rl2+1
        lda #0
        sta got
        sta got+1
        sta pat
        sta reads
        sta waits
        sta maxblk
@next:  lda got
        cmp want
        lda got+1
        sbc want+1
        bcs @end
        lda rl2
        ldx rl2+1
        jsr read_sock
        STATUS st_nodata11
        bne @data
        inc waits
        lda waits
        cmp #100
        bne @next
        sec
        rts
@data:  jsr is_ok
        bne @bad
        jsr hdr_ok
        bcs @bad
        jsr check_pat
        bcs @bad
        clc
        lda got
        adc cnt
        sta got
        lda got+1
        adc cnt+1
        sta got+1
        inc reads
        lda blocks
        cmp maxblk
        bcc @next
        sta maxblk
        jmp @next
@end:   lda got                         ; exactly as many
        cmp want
        bne @bad
        lda got+1
        cmp want+1
        bne @bad
        clc
        rts
@bad:   sec
        rts

; read_all's line: PASS and how many reads and blocks, or FAIL and where it stopped
show_all:
        bcs @bad
        jsr pass
        PRINT m_indent
        lda reads
        ldx #0
        jsr dec16
        PRINT m_reads
        lda maxblk
        ldx #0
        jsr dec16
        PRINT m_most
        rts
@bad:   jsr fail
        PRINT m_at
        lda got
        ldx got+1
        jsr dec16
        PRINT m_comma
        jmp show_read

; READ of A/X bytes, again while "02,NO DATA: 11" (up to 50 times)
read_one:
        sta rl2
        stx rl2+1
        lda #0
        sta waits
@r:     lda rl2
        ldx rl2+1
        jsr read_sock
        STATUS st_nodata11
        bne @e
        inc waits
        lda waits
        cmp #50
        bne @r
@e:     rts

; A datagram of A/X bytes of the pattern from 0, read whole with a READ of 1472
dgram_whole:
        sta want
        stx want+1
        lda #0
        sta pat
        lda #<1472
        ldx #>1472
        jsr read_one
        lda #<st_ok
        ldy #>st_ok
        ; fall through

; The READ's reply: status A/Y, want bytes of the pattern from pat
want_dgram:
        jsr st_eq
        bne @bad
        jsr hdr_ok
        bcs @bad
        lda cnt
        cmp want
        bne @bad
        lda cnt+1
        cmp want+1
        bne @bad
        jsr check_pat
        bcs @bad
        jsr pass
        jmp show_blocks
@bad:   jsr fail
        jmp show_read

; A READ given up after its first block (895 bytes, Data More): the interface idle again.
; The datagram may not be there yet: a reply of one block is taken, and tried again.
abandon:
        lda #0
        sta waits
@try:   lda #1
        sta give_up
        READ 1472
        lda #0
        sta give_up
        lda more1
        cmp #$30
        beq @gave
        STATUS st_nodata11
        bne @bad
        inc waits
        lda waits
        cmp #50
        bne @try
@bad:   jsr fail
        jmp show_read
@gave:  lda CTRL
        and #S_STATE | S_ERROR
        bne @bad2
        lda blk1
        cmp #<895
        bne @bad2
        lda blk1+1
        cmp #>895
        bne @bad2
        jmp pass
@bad2:  jsr fail
        PRINT m_first
        lda blk1
        ldx blk1+1
        jsr dec16
        PRINT m_state
        lda CTRL
        jsr hex8
        lda #13
        jmp CHROUT

; Carry clear if nlen >= 2 and the header (nbuf, 2 bytes) is nlen - 2, which is left in cnt
hdr_ok: sec
        lda nlen
        sbc #2
        sta cnt
        lda nlen+1
        sbc #0
        sta cnt+1
        bcc @bad
        lda nbuf
        cmp cnt
        bne @bad
        lda nbuf+1
        cmp cnt+1
        bne @bad
        clc
        rts
@bad:   sec
        rts

; The READ's data after its header against the pattern from pat, which goes on; carry set
; at the first byte that differs
check_pat:
        lda #<(nbuf+2)
        sta PTR2
        lda #>(nbuf+2)
        sta PTR2+1
        sec
        lda nlen
        sbc #2
        sta cnt
        lda nlen+1
        sbc #0
        sta cnt+1
        ldy #0
@l:     lda cnt
        ora cnt+1
        beq @ok
        lda (PTR2),y
        cmp pat
        bne @bad
        inc pat
        lda pat
        cmp #251
        bne :+
        lda #0
        sta pat
:       inc PTR2
        bne :+
        inc PTR2+1
:       lda cnt
        bne :+
        dec cnt+1
:       dec cnt
        jmp @l
@ok:    sec                             ; cnt again, for the caller
        lda nlen
        sbc #2
        sta cnt
        lda nlen+1
        sbc #0
        sta cnt+1
        clc
        rts
@bad:   sec
        rts

; A reply with no data: header $FFFF (-1), status A/Y
want_none:
        jsr is_none
        bne @bad
        jmp pass
@bad:   jsr fail
        jmp show_read

; The same, Z set if so
is_none:
        jsr st_eq
        bne @e
        lda nlen
        cmp #2
        bne @e
        lda nlen+1
        bne @e
        lda nbuf
        and nbuf+1
        cmp #$FF
@e:     rts

; Reads of 64 bytes until a CR: the line before it into line; carry set if it doesn't come
read_line:
        lda #0
        sta line_n
        sta line
        sta waits
@r:     READ 64
        STATUS st_nodata11
        bne @got
        inc waits
        lda waits
        cmp #100
        bne @r
        sec
        rts
@got:   jsr is_ok
        bne @no
        jsr hdr_ok
        bcs @no
        ldx #0
@c:     cpx cnt
        beq @r
        lda nbuf+2,x
        cmp #13
        beq @end
        ldy line_n
        cpy #79
        beq :+
        sta line,y
        lda #0
        sta line+1,y
        inc line_n
:       inx
        bne @c
@end:   clc
        rts
@no:    sec
        rts

; Z set if line is (line_eq) or starts with (line_starts) the C string at A/Y
line_eq:
        sta PTR
        sty PTR+1
        ldy #0
@l:     lda (PTR),y
        cmp line,y
        bne @e
        iny
        cmp #0
        bne @l
@e:     rts

line_starts:
        sta PTR
        sty PTR+1
        ldy #0
@l:     lda (PTR),y
        beq @e
        cmp line,y
        bne @e
        iny
        bne @l
@e:     rts

;-----------------------------------------------------------------------------------------
; Checks and printing
;-----------------------------------------------------------------------------------------

; 00,OK and some data, then the reply shown
want_data:
        jsr is_ok
        bne @bad
        lda resp_len
        ora resp_len+1
        beq @bad
        jsr pass
        jmp show
@bad:   jsr fail
        jmp show

; 00,OK and A bytes of data, then shown in hex
want_n: cmp resp_len
        bne @bad
        lda resp_len+1
        bne @bad
        jsr is_ok
        bne @bad
        jsr pass
        jmp show_hex
@bad:   jsr fail
        jmp show_hex

; GET_IPADDR of each interface the count names (up to 4): each 00,OK with 12 bytes, one of
; them with an address (an II+L has two, the first without); the first such is ifx, its
; addresses in ip12. Then each shown.
each_ip:
        lda #0
        sta ifi
        sta ipbad
        ldx #11
@z:     sta ip12,x
        dex
        bpl @z
        lda #$FF
        sta ifx
@l:     lda ifi
        cmp ifn
        beq @done
        cmp #4
        beq @done
        NET $05
        lda ifi
        jsr add_byte
        jsr go
        lda resp_len
        cmp #12
        bne @bad
        lda resp_len+1
        bne @bad
        jsr is_ok
        bne @bad
        ldx ifi
        lda if_at,x
        tax
        ldy #0
@cp:    lda resp_buf,y
        sta ipall,x
        inx
        iny
        cpy #12
        bne @cp
        lda ifx
        bpl @next                       ; one with an address already
        lda resp_buf
        ora resp_buf+1
        ora resp_buf+2
        ora resp_buf+3
        beq @next
        lda ifi
        sta ifx
        ldx #11
@ip:    lda resp_buf,x
        sta ip12,x
        dex
        bpl @ip
@next:  inc ifi
        jmp @l
@bad:   inc ipbad
        jmp @next
@done:  lda ipbad
        bne @fail
        lda ifx
        bmi @fail
        jsr pass
        jmp @show
@fail:  jsr fail
@show:  lda #0
        sta ifi
@s:     lda ifi
        cmp ifn
        beq @e
        cmp #4
        beq @e
        PRINT m_indent
        lda ifi
        ldx #0
        jsr dec16
        PRINT m_colon
        ldx ifi
        lda if_at,x
        tax
        jsr dotted
        PRINT m_mask
        ldx ifi
        lda if_at,x
        clc
        adc #4
        tax
        jsr dotted
        PRINT m_gw
        ldx ifi
        lda if_at,x
        clc
        adc #8
        tax
        jsr dotted
        lda #13
        jsr CHROUT
        inc ifi
        jmp @s
@e:     rts

; The interface with the address (0 if none) into the command
add_ifx:
        lda ifx
        bpl :+
        lda #0
:       jmp add_byte

; The address at ipall + X, as a.b.c.d
dotted: stx dot_at
        lda #4
        sta dot_n
@l:     ldx dot_at
        lda ipall,x
        ldx #0
        jsr dec16
        inc dot_at
        dec dot_n
        beq @e
        lda #'.'
        jsr CHROUT
        jmp @l
@e:     rts

; PASS if Z is set, else FAIL and the status
result_z:
        bne @bad
        jmp pass
@bad:   jsr fail
        jmp show_read

; Zero flag set if the status is 00,OK
is_ok:  lda #<st_ok
        ldy #>st_ok
        ; fall through

; Z set if the status is the C string at A/Y
st_eq:  sta PTR
        sty PTR+1
        ldy #0
@l:     lda (PTR),y
        cmp stat_buf,y
        bne @e
        iny
        cmp #0
        bne @l
@e:     rts

; Z set if the status starts with the C string at A/Y
st_starts:
        sta PTR
        sty PTR+1
        ldy #0
@l:     lda (PTR),y
        beq @e
        cmp stat_buf,y
        bne @e
        iny
        bne @l
@e:     rts

; The reply's data in hex (up to 16 bytes), and its status
show_hex:
        lda #' '
        jsr CHROUT
        ldx #0
@l:     cpx resp_len
        beq @st
        cpx #16
        beq @st
        stx hex_at
        lda resp_buf,x
        jsr hex8
        lda #' '
        jsr CHROUT
        ldx hex_at
        inx
        bne @l
@st:    jmp show_status

; A READ's reply: how many bytes, the header in hex, the status
show_read:
        PRINT m_got
        lda nlen
        ldx nlen+1
        jsr dec16
        PRINT m_bytes
        lda nlen
        ora nlen+1
        beq @st
        lda nbuf+1
        jsr hex8
        lda nbuf
        jsr hex8
        lda #' '
        jsr CHROUT
@st:    jmp show_status

; A reply in more than one block: how many, and the first one's length
show_blocks:
        lda blocks
        cmp #2
        bcc @e
        PRINT m_indent
        lda blocks
        ldx #0
        jsr dec16
        PRINT m_blocks
        lda blk1
        ldx blk1+1
        jsr dec16
        PRINT m_bytes_cr
@e:     rts

show_line:
        PRINT m_line
show_line_only:
        lda #<line
        ldy #>line
        jsr puts
        lda #13
        jmp CHROUT

; A number, A (low) X (high), in decimal; dec24 takes num (3 bytes)
dec16:  sta num
        stx num+1
        lda #0
        sta num+2
dec24:  lda #0
        sta lead
        ldx #0
@pow:   ldy #'0'
@sub:   lda num
        sec
        sbc p_lo,x
        sta t0
        lda num+1
        sbc p_mid,x
        sta t1
        lda num+2
        sbc p_hi,x
        bcc @digit
        sta num+2
        lda t1
        sta num+1
        lda t0
        sta num
        iny
        bne @sub
@digit: tya
        cmp #'0'
        bne @print
        ldy lead
        beq @skip
@print: sta lead
        stx dec_x
        jsr CHROUT
        ldx dec_x
@skip:  inx
        cpx #7
        bne @pow
        lda num
        ora #'0'
        jmp CHROUT

; The jiffy clock now, into tn (low byte first)
tick:   lda JIFFY+2
        sta tn
        lda JIFFY+1
        sta tn+1
        lda JIFFY
        sta tn+2
        lda JIFFY+2
        cmp tn
        bne tick
        rts

clock_start:
        jsr tick
        lda tn
        sta t_start
        lda tn+1
        sta t_start+1
        rts

; Jiffies since clock_start, into den (at least 1)
elapsed:
        jsr tick
        sec
        lda tn
        sbc t_start
        sta den
        lda tn+1
        sbc t_start+1
        sta den+1
        ora den
        bne :+
        inc den
:       rts

; The rate since clock_start: A/X/Y = 60 times the bytes moved, divided by the jiffies
show_rate:
        sta num
        stx num+1
        sty num+2
        jsr elapsed
        lda #0
        sta rem
        sta rem+1
        ldx #24
@div:   asl num
        rol num+1
        rol num+2
        rol rem
        rol rem+1
        lda rem
        sec
        sbc den
        tay
        lda rem+1
        sbc den+1
        bcc :+
        sta rem+1
        sty rem
        inc num
:       dex
        bne @div
        PRINT m_indent
        jsr dec24
        PRINT m_rate
        lda den
        ldx den+1
        jsr dec16
        PRINT m_jiffies
        rts

; UDP sockets to 127.0.0.1 until one fails (at most 16), then all of them closed: PASS if at
; least 4 opened, the one after them failed with 85, and every close was 00,OK
count_sockets:
        lda #0
        sta nsock
        sta closebad
@open:  jsr open_local
        bcs @full
        ldx nsock
        lda resp_buf
        sta socks,x
        inc nsock
        lda nsock
        cmp #16
        bne @open
        lda #0
        sta stat2
        beq @close
@full:  ldx #0                          ; the status of the one that failed
@cp:    lda stat_buf,x
        sta stat2,x
        beq @close
        inx
        cpx #40
        bne @cp
        lda #0
        sta stat2,x
@close: lda #0
        sta idx
@cl:    lda idx
        cmp nsock
        beq @done
        NET $09
        ldx idx
        lda socks,x
        jsr add_byte
        jsr go
        jsr is_ok
        beq :+
        inc closebad
:       inc idx
        jmp @cl
@done:  lda nsock
        cmp #4
        bcc @bad
        lda closebad
        bne @bad
        lda stat2
        beq @good                       ; 16 opened
        ldx #0
@cmp:   lda st_nosock,x
        cmp stat2,x
        bne @bad
        inx
        cmp #0
        bne @cmp
@good:  jsr pass
        jmp @show
@bad:   jsr fail
@show:  PRINT m_indent
        lda nsock
        ldx #0
        jsr dec16
        PRINT m_opened
        lda #<stat2
        ldy #>stat2
        jsr puts
        lda #13
        jmp CHROUT

; A line typed at the keyboard into peer (up to 40 characters), RETURN to end it
input:  lda #0
        sta peer_len
@key:   jsr GETIN
        cmp #0
        beq @key
        cmp #13
        beq @end
        cmp #20                         ; DEL
        bne @add
        ldx peer_len
        beq @key
        dec peer_len
        jsr CHROUT
        jmp @key
@add:   ldx peer_len
        cpx #40
        beq @key
        sta peer,x
        inc peer_len
        jsr CHROUT
        jmp @key
@end:   ldx peer_len
        lda #0
        sta peer,x
        lda #13
        jmp CHROUT

;-----------------------------------------------------------------------------------------
; Data
;-----------------------------------------------------------------------------------------

title:      .byte 147, "UCI NETWORK TEST", 13, 13, 0
no_uci:     .byte "NO COMMAND INTERFACE AT $DF1C.", 13, "SWITCH IT ON IN THE MENU.", 13, 0
no_net:     .byte "NO NETWORK TARGET.", 13, 0
t_ident:    .byte "IDENTIFY ", 0
t_count:    .byte "GET_INTERFACE_COUNT ", 0
t_mac:      .byte "GET_NETADDR, THAT ONE ", 0
t_ip:       .byte "GET_IPADDR, EACH ", 0
t_ip_past:  .byte "GET_IPADDR PAST THE LAST ", 0
t_ip_short: .byte "GET_IPADDR, NO INTERFACE ", 0
t_set:      .byte "SET_IPADDR, THE SAME ", 0
t_same:     .byte "GET_IPADDR: UNCHANGED ", 0
t_set_short: .byte "SET_IPADDR, NO ADDRESSES ", 0
t_setif:    .byte "SET_INTERFACE ", 0
t_read7f:   .byte "READ $7F, NEVER OPENED ", 0
t_read_big: .byte "READ OF 1473 BYTES ", 0
t_read_short: .byte "READ, NO LENGTH ", 0
t_write7f:  .byte "WRITE $7F ", 0
t_close7f:  .byte "CLOSE $7F ", 0
t_open_short: .byte "OPEN_TCP, NO NAME ", 0
t_udp_n:    .byte "UDP SOCKETS ", 0
t_peer:     .byte 13, "PEER'S ADDRESS (RETURN: NONE)?", 13, 0
t_refused:  .byte "OPEN_TCP PEER:6465, CLOSED ", 0
t_tcp:      .byte "OPEN_TCP PEER:6464 ", 0
t_nothing:  .byte "10 READS, NOTHING SENT ", 0
t_p200:     .byte "TCP 200 BYTES, READS OF 64 ", 0
t_p1420:    .byte "TCP 1420 BYTES, READS OF 1472 ", 0
t_up:       .byte "TCP WRITE 8000 BYTES ", 0
t_down:     .byte "TCP READ 32768 BYTES ", 0
t_bye:      .byte "CLOSED BY THE PEER ", 0
t_after:    .byte "READ AFTER THAT ", 0
t_udp:      .byte "OPEN_UDP PEER:6464 ", 0
t_u64:      .byte "UDP 64 BYTES ", 0
t_u200:     .byte "UDP 200 BYTES, READ OF 64 ", 0
t_gone:     .byte "THE REST IS GONE ", 0
t_u1420:    .byte "UDP 1420 BYTES ", 0
t_u1472:    .byte "UDP 1472 BYTES ", 0
t_u1473:    .byte "UDP 1473 BYTES: DROPPED ", 0
t_aband:    .byte "READ GIVEN UP AFTER A BLOCK ", 0
t_next_cmd: .byte "THE NEXT COMMAND ", 0
t_next_dg:  .byte "THE NEXT DATAGRAM, WHOLE ", 0
t_echo:     .byte "UDP WRITE 892 BYTES, ECHOED ", 0
t_uclose:   .byte "CLOSE ", 0
t_nosuch:   .byte "OPEN_TCP NOSUCH.INVALID ", 0
t_web:      .byte "OPEN_TCP EXAMPLE.COM:80, HEAD ", 0
t_leave:    .byte "LEAVE 4 UDP SOCKETS OPEN ", 0
m_no_ip:    .byte "SKIPPED: NO ADDRESS", 13, 0
m_no_peer:  .byte "NO PEER: TCP AND UDP SKIPPED", 13, 0
m_leave:    .byte "  RESET, THEN RUN AGAIN: UDP SOCKETS", 13
            .byte "  MUST SHOW AS MANY AS THIS RUN DID", 13, 0
m_indent:   .byte "  ", 0
m_colon:    .byte ": ", 0
m_mask:     .byte " MASK ", 0
m_gw:       .byte 13, "     GATEWAY ", 0
m_sock:     .byte "  SOCKET $", 0
m_write:    .byte " WRITE:", 0
m_reads:    .byte " READS, BLOCKS UP TO ", 0
m_most:     .byte 13, 0
m_at:       .byte "  AT BYTE ", 0
m_comma:    .byte ":", 13, 0
m_got:      .byte "  GOT ", 0
m_bytes:    .byte " BYTES ", 0
m_blocks:   .byte " BLOCKS, THE FIRST ", 0
m_bytes_cr: .byte " BYTES", 13, 0
m_first:    .byte "  FIRST BLOCK ", 0
m_state:    .byte " BYTES, THEN $DF1C $", 0
m_line:     .byte "  LINE: ", 0
m_rate:     .byte " BYTES/S IN ", 0
m_jiffies:  .byte " JIFFIES", 13, 0
m_opened:   .byte " OPENED, THEN: ", 0
all_passed: .byte 13, "ALL PASSED", 13, 0
some_failed: .byte 13, "FAILED: $", 0

st_ok:      .byte "00,OK", 0
st_closed:  .byte "01,CONNECTION CLOSED BY HOST", 0
st_nodata9: .byte "02,NO DATA: 9", 0
st_nodata11: .byte "02,NO DATA: 11", 0
st_trunc200: .byte "04,DATAGRAM TRUNCATED: 200", 0
st_connect: .byte "11,ERROR ON CONNECT", 0
st_send9:   .byte "12,SEND ERROR: 9", 0
st_close9:  .byte "12,ERROR ON CLOSE: 9", 0
st_unknown: .byte "21,UNKNOWN COMMAND", 0
st_invalid: .byte "81,INVALID PARAMS", 0
st_range:   .byte "82,PARAMETER(S) OUT OF RANGE", 0
st_unresolved: .byte "84,UNRESOLVED HOST", 0
st_nosock:  .byte "85,ERROR OPENING SOCKET", 0

; What the peer is asked over TCP (a line each) and UDP (a datagram each)
r_p200:     .byte "P 200", 13, 0
r_p1420:    .byte "P 1420", 13, 0
r_e8000:    .byte "E 8000", 13, 0
r_p32k:     .byte "P 32768", 13, 0
r_bye:      .byte "C", 13, 0
u_p64:      .byte "P 64", 0
u_p200:     .byte "P 200", 0
u_p1420:    .byte "P 1420", 0
u_p1472:    .byte "P 1472", 0
u_p1473:    .byte "P 1473", 0
u_d1420:    .byte "D 1420 2", 0
r_head:     .byte "HEAD / HTTP/1.0", 13, 10, "HOST: EXAMPLE.COM", 13, 10, 13, 10, 0
s_ok8000:   .byte "OK 8000", 0
s_http:     .byte "HTTP/", 0
s_local:    .byte "127.0.0.1", 0
s_nosuch:   .byte "NOSUCH.INVALID", 0
s_web:      .byte "EXAMPLE.COM", 0
minus1:     .byte $FF, $FF
nothing:    .byte 0

if_at:      .byte 0, 12, 24, 36       ; each interface's addresses in ipall

; 10^7 down to 10, for dec24
p_lo:       .byte <10000000, <1000000, <100000, <10000, <1000, <100, <10
p_mid:      .byte >10000000, >1000000, >100000, >10000, >1000, >100, >10
p_hi:       .byte ^10000000, ^1000000, ^100000, ^10000, ^1000, ^100, ^10

.include "ucilib.s"

cmd_pos:    .res 1
hex_at:     .res 1
idx:        .res 1
sock:       .res 1
port:       .res 2
rq:         .res 2
rq_n:       .res 1
wn:         .res 2
wpat:       .res 1
pat:        .res 1
cnt:        .res 2
rl:         .res 2
rl2:        .res 2
want:       .res 2
got:        .res 2
reads:      .res 1
waits:      .res 1
maxblk:     .res 1
nlen:       .res 2
blen:       .res 2
blk1:       .res 2
blocks:     .res 1
more1:      .res 1
st_now:     .res 1
give_up:    .res 1
ip12:       .res 12
ipall:      .res 4 * 12
ifn:        .res 1
ifi:        .res 1
ifx:        .res 1
ipbad:      .res 1
dot_at:     .res 1
dot_n:      .res 1
num:        .res 3
t0:         .res 1
t1:         .res 1
lead:       .res 1
dec_x:      .res 1
tn:         .res 3
t_start:    .res 2
den:        .res 2
rem:        .res 2
nsock:      .res 1
closebad:   .res 1
socks:      .res 16
stat2:      .res 41
peer_len:   .res 1
peer:       .res 41
line_n:     .res 1
line:       .res 81
cmd_buf:    .res 64
wbuf:       .res 3 + 892
nbuf:       .res NBUF
