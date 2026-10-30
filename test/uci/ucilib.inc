; The command interface's registers and the macros all the test programs use

CHROUT  = $FFD2

CTRL    = $DF1C         ; write: control, read: status
CMDQ    = $DF1D         ; write: command queue, read: identification ($C9, $49 with IRQ)
RESP    = $DF1E         ; response queue
STATQ   = $DF1F         ; status queue

C_PUSH  = $01
C_ACC   = $02
C_ABORT = $04
C_CLR   = $08
C_IRQ   = $20

S_DATA  = $80
S_STAT  = $40
S_STATE = $30
S_ERROR = $08

PTR     = $FB           ; free in C64 and C128 mode
PTR2    = $FD

.macro PRINT s
        lda #<s
        ldy #>s
        jsr puts
.endmacro

; The command at c, len bytes
.macro CMD c, len
        lda #<c
        sta PTR
        lda #>c
        sta PTR+1
        lda #<(len)
        sta cmd_len
        lda #>(len)
        sta cmd_len+1
.endmacro

; Compare the reply's data with want, len bytes, and its status with st
.macro EXPECT want, len, st
        lda #<want
        sta PTR
        lda #>want
        sta PTR+1
        lda #<(len)
        sta cmp_len
        lda #>(len)
        sta cmp_len+1
        lda #<st
        sta PTR2
        lda #>st
        sta PTR2+1
        jsr check_reply
.endmacro
