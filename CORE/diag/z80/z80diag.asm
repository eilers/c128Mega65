; C128 Z80 integration diagnostic (VICE golden / later MEGA65)
;
; Load at $3000.
;
; Final register snapshot at halt_loop (VICE smoke reads this):
;   A='Z', B=T2, C=T3, D=T4, E='D'
;
; During the run, shadow state lives in the alternate register set:
;   B'=T2 XOR, C'=T3 keys, D'=T4 status
;
; RESULT @$1300 is also written when Z80 RAM stores work (MEGA65).

RESULT:  equ 0x1300
MBOX:    equ 0x1200
MMU_CPU: equ 0xd505

        org 0x3000

start:
        di
        ld sp,0x31ff

        exx
        ld b,0                  ; T2
        ld c,0                  ; T3
        ld d,0xff               ; T4 default = no stub
        exx

        ld a,'Z'
        ld (RESULT),a
        xor a
        ld (RESULT+1),a
        ld (RESULT+2),a
        ld (RESULT+3),a
        ld a,0xff
        ld (RESULT+4),a

        ; ---- T2: idle CIA1 matrix ----
        ld h,0xfe               ; column walker
t2_loop:
        ld bc,0xdc00
        ld a,h
        out (c),a
        ld bc,0xdc01
        in a,(c)
        cp 0xff
        jr z,t2_next
        ld l,a                  ; faulting row (must stay on this reg bank)
        exx
        ld a,b
        exx
        xor l
        exx
        ld b,a
        exx
t2_next:
        ld a,h
        scf
        rla
        ld h,a
        jr c,t2_loop
        ld bc,0xdc00
        ld a,0xff
        out (c),a
        exx
        ld a,b
        exx
        ld (RESULT+2),a

        ; ---- T3 ----
        ld de,0x2000
t3_poll:
        call scan_keys
        dec de
        ld a,d
        or e
        jr nz,t3_poll
        exx
        ld a,c
        exx
        ld (RESULT+3),a

        ; ---- T4 ----
        ld a,(MBOX+0x7f)
        cp 1
        jr nz,t4_done
        xor a
        ld (MBOX+1),a
        ld a,0xa5
        ld (MBOX),a
        ld a,(MMU_CPU)
        or 1
        ld (MMU_CPU),a
        ld a,(MBOX+1)
        cp 0x5a
        jr nz,t4_fail
        exx
        ld d,0
        exx
        jr t4_store
t4_fail:
        exx
        ld d,1
        exx
t4_store:
        exx
        ld a,d
        exx
        ld (RESULT+4),a
t4_done:
        ld a,'D'
        ld (RESULT+5),a

        ; Publish shadow regs into main BCDE + A for the monitor
        exx
        ld a,b
        ld l,c
        ld h,d
        exx
        ld b,a                  ; T2
        ld c,l                  ; T3
        ld d,h                  ; T4
        ld e,'D'
        ld a,'Z'

halt_loop:
        jr halt_loop

scan_keys:
        ld bc,0xdc00
        ld a,0x7f
        out (c),a
        ld bc,0xdc01
        in a,(c)
        bit 0,a
        jr nz,sk_z
        exx
        ld a,c
        set 0,a
        ld c,a
        exx
sk_z:
        ld bc,0xdc00
        ld a,0xfd
        out (c),a
        ld bc,0xdc01
        in a,(c)
        bit 4,a
        jr nz,sk_ret
        exx
        ld a,c
        set 1,a
        ld c,a
        exx
sk_ret:
        ld bc,0xdc00
        ld a,0xfe
        out (c),a
        ld bc,0xdc01
        in a,(c)
        bit 1,a
        jr nz,sk_done
        exx
        ld a,c
        set 2,a
        ld c,a
        exx
sk_done:
        ld bc,0xdc00
        ld a,0xff
        out (c),a
        ret
