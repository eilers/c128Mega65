; ****************************************************************************
; MiSTer2MEGA65 (M2M) QNICE ROM
;
; Load a file into a QNICE device by DMA from the SD card
;
; A device that reads SDL_MAGIC at its CSR $030 copies runs of consecutive SD
; blocks into its memory by itself (CORE/vhdl/sd_dma.vhd in the C128 core).
; DMA_LOAD walks the cluster chain of the file and hands it one run after the
; other, so the file does not pass through QNICE byte by byte. MAP_LOAD does
; the same walk for a drive that reads its image straight from the card.
;
; The FAT walk follows FAT32$FILE_MAP of Megamiga (johansmolinski/QNICE-FPGA).
; Done by Jeff Flitton in 2026 and licensed under GPL v3
; ****************************************************************************

SDL_MAGIC       .EQU    0x5D4A
SDL_CSR_MAGIC   .EQU    0x7030                  ; R: SDL_MAGIC
SDL_CSR_CTRL    .EQU    0x7031                  ; W: start, R: bit 0 busy, 1 error
SDL_CSR_BLK_LO  .EQU    0x7032                  ; first SD block
SDL_CSR_BLK_HI  .EQU    0x7033
SDL_CSR_COUNT   .EQU    0x7034                  ; number of blocks
SDL_CSR_DEST_LO .EQU    0x7035                  ; byte offset within the device
SDL_CSR_DEST_HI .EQU    0x7036
SDL_CSR_LEN_LO  .EQU    0x7037                  ; nothing is written from here on
SDL_CSR_LEN_HI  .EQU    0x7038

SDL_MAP_MAGIC   .EQU    0x5D4D                  ; R: a drive that maps images
SDL_CSR_NBLK    .EQU    0x7032                  ; map: blocks of the image
SDL_MAP_MAX     .EQU    2048                    ; map: blocks a table holds
SDL_PIECE       .EQU    8                       ; map: blocks per table entry
SDL_PIECE_M     .EQU    7                       ; map: a block within a piece

SDL_CHUNK       .EQU    128                     ; blocks per DMA: a progress step
SDL_ERR_DMA     .EQU    0xEE40                  ; the transfer of the device failed
SDL_ERR_CHAIN   .EQU    0xEE41                  ; the cluster chain ends too early
SDL_ERR_TWICE   .EQU    0xEE42                  ; the image is in another drive
SDL_ERR_LARGE   .EQU    0xEE43                  ; the image is too large to map
SDL_ERR_SPLIT   .EQU    0xEE44                  ; a 4 KB piece of it is split

SDL_STR_TWICE   .ASCII_W "This disk image is already\nin the other drive.\n"
SDL_STR_SPLIT   .ASCII_P "This disk image is split into\n"
                .ASCII_P "pieces under 4 KB on the card.\n"
                .ASCII_W "Copy it again, or format the\ncard with 4 KB clusters.\n"

; ----------------------------------------------------------------------------
; DMA_LOAD
;
; Input:  R8: device id
;         R9: file handle, position 0
;         R10: 4k window of the device where the file starts
; Output: R8: 0 = loaded, 1 = the device has no DMA (nothing was done),
;             otherwise an error code
;         R9 to R12 unchanged
; ----------------------------------------------------------------------------

DMA_LOAD        INCRB
                MOVE    R9, @--SP
                MOVE    R10, @--SP
                MOVE    R11, @--SP
                MOVE    R12, @--SP

                MOVE    R9, R0                  ; R0: file handle
                MOVE    R10, R1                 ; R1: 4k window
                MOVE    SDL_DEVICE, R2
                MOVE    R8, @R2
                MOVE    SDL_MODE, R2
                MOVE    0, @R2

                ; does the device take a DMA?
                MOVE    SDL_CSR_MAGIC, R9
                RSUB    CRTROM_CSR_R, 1         ; R10: magic
                MOVE    1, R12                  ; R12: return value
                CMP     SDL_MAGIC, R10
                RBRA    _SDL_RET, !Z

                ; blocks of the file (32 MB and more: left to the byte loop)
                MOVE    R0, R8
                RSUB    _SDL_BLOCKS, 1          ; R9: blocks, C: too large
                RBRA    _SDL_RET, C
                MOVE    R9, R7                  ; R7: blocks to load
                XOR     R12, R12
                CMP     0, R7
                RBRA    _SDL_RET, Z             ; an empty file is loaded

                ; the device stops writing at the end of the file
                MOVE    SDL_DEVICE, R8
                MOVE    @R8, R8
                MOVE    R0, R2
                ADD     FAT32$FDH_SIZE_LO, R2
                MOVE    SDL_CSR_LEN_LO, R9
                MOVE    @R2, R10
                RSUB    CRTROM_CSR_W, 1
                ADD     1, R2
                MOVE    SDL_CSR_LEN_HI, R9
                MOVE    @R2, R10
                RSUB    CRTROM_CSR_W, 1

                ; destination: one byte per word of the 4k windows
                MOVE    R1, R2
                SHL     12, R2
                AND     0xF000, R2
                SHR     4, R1
                AND     0x0FFF, R1
                MOVE    SDL_DEST, R3
                MOVE    R2, @R3
                ADD     1, R3
                MOVE    R1, @R3

                MOVE    R0, R8
                MOVE    R7, R9
                RSUB    _SDL_WALKF, 1
                MOVE    R8, R12

_SDL_RET        MOVE    R12, R8
                MOVE    @SP++, R12
                MOVE    @SP++, R11
                MOVE    @SP++, R10
                MOVE    @SP++, R9
                DECRB
                RET

; ----------------------------------------------------------------------------
; MAP_LOAD: mount a disk image by its block table instead of loading it
;
; A device that reads SDL_MAP_MAGIC at its CSR $030 reads the image straight
; from the SD card (CORE/vhdl/vd_sd.vhd in the C128 core) and only needs to
; know where each 4 KB piece of it starts. A piece must not be split on the
; card, which clusters of 4 KB and more guarantee.
;
; Input:  R8: device id of the drive
;         R9: file handle, position 0
;         R10: drive number
; Output: R8: 0 = mounted, 1 = the device cannot map (nothing was done),
;             otherwise an error code
;         R9: 0 or a message for the error
;         R10 to R12 unchanged
; ----------------------------------------------------------------------------

MAP_LOAD        INCRB
                MOVE    R10, @--SP
                MOVE    R11, @--SP
                MOVE    R12, @--SP

                MOVE    R9, R0                  ; R0: file handle
                MOVE    R10, R1                 ; R1: drive number
                MOVE    SDL_DEVICE, R2
                MOVE    R8, @R2
                MOVE    SDL_MODE, R2
                MOVE    1, @R2
                XOR     R6, R6                  ; R6: message

                MOVE    SDL_CSR_MAGIC, R9
                RSUB    CRTROM_CSR_R, 1
                MOVE    1, R12
                CMP     SDL_MAP_MAGIC, R10
                RBRA    _SDL_MRET, !Z

                ; the same file in two drives: their changes would overwrite
                ; each other on the card, so the second mount is refused
                XOR     R2, R2                  ; R2: other drive
_SDL_MDUP       CMP     R2, R1
                RBRA    _SDL_MDUP_N, Z
                MOVE    VDRIVES_BUFS, R8
                ADD     R2, R8
                MOVE    @R8, R8                 ; R8: its device
                MOVE    SDL_CSR_CTRL, R9
                RSUB    CRTROM_CSR_R, 1
                AND     1, R10                  ; mounted?
                RBRA    _SDL_MDUP_N, Z
                MOVE    HNDL_VD_FILES, R8
                ADD     R2, R8
                MOVE    @R8, R8                 ; R8: its file handle
                ADD     FAT32$FDH_START_CLUS_LO, R8
                MOVE    R0, R9
                ADD     FAT32$FDH_START_CLUS_LO, R9
                CMP     @R8, @R9
                RBRA    _SDL_MDUP_N, !Z
                ADD     1, R8
                ADD     1, R9
                CMP     @R8, @R9
                RBRA    _SDL_MDUP_N, !Z
                MOVE    SDL_ERR_TWICE, R12
                MOVE    SDL_STR_TWICE, R6
                RBRA    _SDL_MRET, 1
_SDL_MDUP_N     ADD     1, R2
                MOVE    VDRIVES_NUM, R8
                CMP     @R8, R2
                RBRA    _SDL_MDUP, !Z

                ; switch the drive off: its changes go to the card, then it
                ; forgets the old image
                MOVE    SDL_DEVICE, R8
                MOVE    @R8, R8
                MOVE    R8, R1                  ; R1: device
                RSUB    VDSD_OFF, 1

                ; blocks of the image: a table holds SDL_MAP_MAX
                MOVE    R0, R8
                RSUB    _SDL_BLOCKS, 1
                RBRA    _SDL_MLARGE, C
                MOVE    SDL_MAP_MAX, R8
                CMP     R9, R8                  ; N: blocks > table
                RBRA    _SDL_MLARGE, N
                MOVE    R9, R7                  ; R7: blocks
                MOVE    SDL_IDX, R2
                MOVE    0, @R2

                MOVE    R0, R8
                MOVE    R7, R9
                RSUB    _SDL_WALKF, 1
                MOVE    R8, R12
                CMP     SDL_ERR_SPLIT, R12
                RBRA    _SDL_MSPLIT, !Z
                MOVE    SDL_STR_SPLIT, R6
_SDL_MSPLIT     CMP     0, R12
                RBRA    _SDL_MRET, !Z

                ; size, then on
                MOVE    R1, R8
                MOVE    SDL_CSR_NBLK, R9
                MOVE    R7, R10
                RSUB    CRTROM_CSR_W, 1
                MOVE    SDL_CSR_CTRL, R9
                MOVE    1, R10
                RSUB    CRTROM_CSR_W, 1
_SDL_MON_W      RSUB    CRTROM_CSR_R, 1         ; on before the drive asks
                AND     4, R10
                RBRA    _SDL_MON_W, !Z
                XOR     R12, R12
                RBRA    _SDL_MRET, 1

_SDL_MLARGE     MOVE    SDL_ERR_LARGE, R12

_SDL_MRET       MOVE    R12, R8
                MOVE    R6, R9
                MOVE    @SP++, R12
                MOVE    @SP++, R11
                MOVE    @SP++, R10
                DECRB
                RET

; ----------------------------------------------------------------------------
; VDSD_OFF: switch a drive of vd_sd off and wait until its changes are on the
; card (if the device is one; otherwise nothing happens)
;
; Input:  R8: device id of the drive
; Output: all registers unchanged
; ----------------------------------------------------------------------------

VDSD_OFF        INCRB
                MOVE    R9, R0
                MOVE    R10, R1
                MOVE    SDL_CSR_MAGIC, R9
                RSUB    CRTROM_CSR_R, 1
                CMP     SDL_MAP_MAGIC, R10
                RBRA    _SDL_OFF_R, !Z
                MOVE    SDL_CSR_CTRL, R9
                MOVE    0, R10
                RSUB    CRTROM_CSR_W, 1
_SDL_OFF_W      RSUB    CRTROM_CSR_R, 1         ; wait while busy
                AND     4, R10
                RBRA    _SDL_OFF_W, !Z
_SDL_OFF_R      MOVE    R0, R9
                MOVE    R1, R10
                DECRB
                RET

; ----------------------------------------------------------------------------
; _SDL_BLOCKS: 512-byte blocks of a file
;
; Input:  R8: file handle
; Output: R9: (size + 511) / 512; C = 1: 32 MB or more (R9 undefined)
; ----------------------------------------------------------------------------

_SDL_BLOCKS     INCRB
                MOVE    R8, R0
                ADD     FAT32$FDH_SIZE_LO, R0
                MOVE    @R0, R2                 ; R3|R2: size + 511
                ADD     1, R0
                MOVE    @R0, R3
                ADD     0x01FF, R2
                ADDC    0, R3
                CMP     0x0200, R3              ; N: below 32 MB
                RBRA    _SDL_BL_OK, N
                OR      0x0004, SR              ; set Carry: too large
                RBRA    _SDL_BL_RET, 1
_SDL_BL_OK      SHR     9, R2
                AND     0x007F, R2
                SHL     7, R3
                AND     0xFF80, R3
                OR      R3, R2
                MOVE    R2, R9
                AND     0xFFFB, SR              ; clear Carry
_SDL_BL_RET     DECRB
                RET

; ----------------------------------------------------------------------------
; _SDL_WALKF: walk the cluster chain of a file and hand each run of
; consecutive clusters to _SDL_EMIT (DMA or block table, see SDL_MODE)
;
; Input:  R8: file handle, R9: blocks of the file (at least 1)
; Output: R8: 0 or an error code; R9 to R11 changed
; ----------------------------------------------------------------------------

_SDL_WALKF      INCRB
                MOVE    R8, R0                  ; R0: file handle
                MOVE    R9, R7                  ; R7: blocks still to go

                ; device handle and sectors per cluster
                MOVE    R0, R1
                ADD     FAT32$FDH_DEVICE, R1
                MOVE    @R1, R1                 ; R1: device handle
                MOVE    SDL_DEVH, R2
                MOVE    R1, @R2

                ; The FAT is read through the 512-byte sector buffer: write the
                ; buffer back and hand it to a handle that never is dirty, so
                ; every real handle reads its sector again on its next access.
                ; (A buffer owned by "0" is not safe: FAT32$FLUSH then leaves
                ; R9 alone and FAT32$READ_FDH takes R9 = 1 of a seek for an error.)
                MOVE    R1, R2
                ADD     FAT32$DEV_BUFFERED_FDH, R2
                MOVE    @R2, R8                 ; R8: owner of the buffer
                CMP     0, R8
                RBRA    _SDL_OWN, Z
                RSUB    FAT32$FLUSH, 1
                MOVE    R9, R8
                CMP     0, R8
                RBRA    _SDL_WRET, !Z
_SDL_OWN        MOVE    SDL_NOFDH, R3
                MOVE    R3, @R2
                ADD     FAT32$FDH_FLAGS, R3
                MOVE    0, @R3                  ; not dirty
                MOVE    SDL_FATSEC, R3
                MOVE    0xFFFF, @R3             ; no FAT sector in the buffer
                ADD     1, R3
                MOVE    0xFFFF, @R3

                ; the first run starts with the first cluster of the file
                MOVE    R1, R2
                ADD     FAT32$DEV_SECT_PER_CLUS, R2
                MOVE    @R2, R6                 ; R6: blocks in this run
                MOVE    R0, R2
                ADD     FAT32$FDH_START_CLUS_LO, R2
                MOVE    @R2, R4                 ; R5|R4: first cluster of the run
                ADD     1, R2
                MOVE    @R2, R5
                MOVE    R4, R2                  ; R3|R2: current cluster
                MOVE    R5, R3
                MOVE    R6, R0                  ; R0: sectors per cluster

                ; a run that covers the rest of the file is the last one
_SDL_WALK       CMP     R7, R6                  ; N: more blocks left than in the run
                RBRA    _SDL_MORE, N
                MOVE    R4, R8
                MOVE    R5, R9
                MOVE    R7, R10
                RSUB    _SDL_EMIT, 1
                RBRA    _SDL_WRET, 1

                ; the run goes on if the next cluster follows the current one
_SDL_MORE       MOVE    R2, R8
                MOVE    R3, R9
                RSUB    _SDL_FATNEXT, 1         ; R11|R10: next cluster
                CMP     0, R8
                RBRA    _SDL_WRET, !Z
                ADD     1, R2
                ADDC    0, R3
                CMP     R10, R2
                RBRA    _SDL_JUMP, !Z
                CMP     R11, R3
                RBRA    _SDL_JUMP, !Z
                ADD     R0, R6
                RBRA    _SDL_WALK, 1

                ; it does not: hand over the run, the next cluster starts one
_SDL_JUMP       MOVE    R10, R2
                MOVE    R11, R3
                MOVE    R4, R8
                MOVE    R5, R9
                MOVE    R6, R10
                RSUB    _SDL_EMIT, 1
                CMP     0, R8
                RBRA    _SDL_WRET, !Z
                SUB     R6, R7
                MOVE    R2, R4
                MOVE    R3, R5
                MOVE    R0, R6
                RBRA    _SDL_WALK, 1

_SDL_WRET       DECRB
                RET

; ----------------------------------------------------------------------------
; _SDL_EMIT: DMA a run of consecutive clusters, SDL_CHUNK blocks at a time,
; or (SDL_MODE = 1) write the SD block of each 4 KB piece into the block table
;
; Input:  R8/R9: first cluster (low/high), R10: blocks
; Output: R8: 0 or an error code; R9 to R11 changed
; ----------------------------------------------------------------------------

_SDL_EMIT       INCRB
                MOVE    R10, R7                 ; R7: blocks left in the run

                ; SD block = cluster begin + (cluster - 2) * sectors per cluster
                SUB     2, R8
                SUBC    0, R9
                MOVE    SDL_DEVH, R1
                MOVE    @R1, R1                 ; R1: device handle
                MOVE    R1, R10
                ADD     FAT32$DEV_SECT_PER_CLUS, R10
                MOVE    @R10, R10
                XOR     R11, R11
                RSUB    MTH$MULU32, 1           ; R9|R8: blocks into the data area
                ADD     FAT32$DEV_CLUSTER_LO, R1
                MOVE    @R1, R10
                ADD     1, R1
                MOVE    @R1, R11
                ADD     R10, R8
                ADDC    R11, R9
                MOVE    R8, R2                  ; R3|R2: SD block
                MOVE    R9, R3

                ; map: the SD block of each piece of the run into the table;
                ; a run must start on a piece, or that piece is split
                MOVE    SDL_MODE, R4
                CMP     1, @R4
                RBRA    _SDL_EM_LOOP, !Z
                MOVE    SDL_IDX, R4
                MOVE    @R4, R5                 ; R5: image block of the run
                MOVE    SDL_ERR_SPLIT, R8
                MOVE    R5, R6
                AND     SDL_PIECE_M, R6
                RBRA    _SDL_EM_RET, !Z
                ADD     R7, @R4                 ; image block of the next run
                MOVE    SDL_DEVICE, R6
                MOVE    M2M$RAMROM_DEV, R8
                MOVE    @R6, @R8
                MOVE    M2M$RAMROM_4KWIN, R8
                MOVE    0, @R8
_SDL_EM_MAP     MOVE    R5, R6                  ; two words per entry:
                SHR     2, R6                   ; block / 8 * 2
                AND     0x3FFE, R6
                ADD     M2M$RAMROM_DATA, R6
                MOVE    R2, @R6++
                MOVE    R3, @R6
                ADD     SDL_PIECE, R2
                ADDC    0, R3
                ADD     SDL_PIECE, R5
                SUB     SDL_PIECE, R7           ; borrow or 0: the run is done
                RBRA    _SDL_EM_MEND, C
                RBRA    _SDL_EM_MAP, !Z
_SDL_EM_MEND    XOR     R8, R8
                RBRA    _SDL_EM_RET, 1

_SDL_EM_LOOP    MOVE    SDL_CHUNK, R4           ; R4: blocks in this DMA
                CMP     R4, R7                  ; N: fewer left than a chunk
                RBRA    _SDL_EM_GO, !N
                MOVE    R7, R4

_SDL_EM_GO      MOVE    SDL_DEVICE, R8
                MOVE    @R8, R8                 ; R8: device id
                MOVE    SDL_CSR_BLK_LO, R9
                MOVE    R2, R10
                RSUB    CRTROM_CSR_W, 1
                MOVE    SDL_CSR_BLK_HI, R9
                MOVE    R3, R10
                RSUB    CRTROM_CSR_W, 1
                MOVE    SDL_CSR_COUNT, R9
                MOVE    R4, R10
                RSUB    CRTROM_CSR_W, 1
                MOVE    SDL_DEST, R5
                MOVE    SDL_CSR_DEST_LO, R9
                MOVE    @R5, R10
                RSUB    CRTROM_CSR_W, 1
                ADD     1, R5
                MOVE    SDL_CSR_DEST_HI, R9
                MOVE    @R5, R10
                RSUB    CRTROM_CSR_W, 1
                MOVE    SDL_CSR_CTRL, R9        ; start
                RSUB    CRTROM_CSR_W, 1
_SDL_EM_WAIT    RSUB    CRTROM_CSR_R, 1         ; R10: status
                MOVE    R10, R11
                AND     1, R11
                RBRA    _SDL_EM_WAIT, !Z        ; busy
                MOVE    SDL_ERR_DMA, R8
                AND     2, R10
                RBRA    _SDL_EM_RET, !Z         ; the transfer failed

                ; on to the next chunk: SD block, destination, progress bar
                ADD     R4, R2
                ADDC    0, R3
                MOVE    R4, R8                  ; R9|R8: bytes = blocks * 512
                SHL     9, R8
                AND     0xFE00, R8
                MOVE    R4, R9
                SHR     7, R9
                AND     0x01FF, R9
                MOVE    SDL_DEST, R5
                MOVE    @R5, R10
                ADD     1, R5
                MOVE    @R5, R11
                ADD     R8, R10
                ADDC    R9, R11
                MOVE    R11, @R5
                SUB     1, R5
                MOVE    R10, @R5
                RSUB    LI_PROGRESS, 1
                SUB     R4, R7
                RBRA    _SDL_EM_LOOP, !Z
                XOR     R8, R8

_SDL_EM_RET     DECRB
                RET

; ----------------------------------------------------------------------------
; _SDL_FATNEXT: the cluster that follows a cluster in the chain
;
; The FAT sector is read into the sector buffer unless SDL_FATSEC says it is
; already there.
;
; Input:  R8/R9: cluster (low/high)
; Output: R8: 0, SDL_ERR_CHAIN (no data cluster follows) or an error code
;         R10/R11: next cluster (low/high); R9 changed
; ----------------------------------------------------------------------------

_SDL_FATNEXT    INCRB
                MOVE    R8, R0                  ; R0: cluster low

                ; FAT sector = FAT start + cluster / 128
                MOVE    R8, R2
                SHR     7, R2
                AND     0x01FF, R2
                MOVE    R9, R3
                SHL     9, R3
                AND     0xFE00, R3
                OR      R3, R2
                MOVE    R9, R3
                SHR     7, R3
                AND     0x01FF, R3              ; R3|R2: cluster / 128
                MOVE    SDL_DEVH, R1
                MOVE    @R1, R1                 ; R1: device handle
                MOVE    R1, R4
                ADD     FAT32$DEV_FAT_LO, R4
                MOVE    @R4, R5
                ADD     1, R4
                MOVE    @R4, R6
                ADD     R5, R2
                ADDC    R6, R3                  ; R3|R2: SD block of the FAT sector

                ; read it unless it is in the buffer already
                MOVE    SDL_FATSEC, R4
                CMP     @R4, R2
                RBRA    _SDL_FN_READ, !Z
                ADD     1, R4
                CMP     @R4, R3
                RBRA    _SDL_FN_HAVE, Z
_SDL_FN_READ    MOVE    R2, R8
                MOVE    R3, R9
                MOVE    FAT32$DEV_BLOCK_READ, R10
                MOVE    R1, R11
                RSUB    FAT32$CALL_DEV, 1       ; R8: 0 or an error code
                MOVE    SDL_FATSEC, R4
                CMP     0, R8
                RBRA    _SDL_FN_RDOK, Z
                MOVE    0xFFFF, @R4             ; the buffer holds something unknown
                RBRA    _SDL_FN_RET, 1
_SDL_FN_RDOK    MOVE    R2, @R4
                ADD     1, R4
                MOVE    R3, @R4

                ; the entry: 4 bytes at (cluster % 128) * 4, 28 bits used
_SDL_FN_HAVE    MOVE    R1, R8
                MOVE    R0, R9
                AND     0x007F, R9
                SHL     2, R9
                AND     0x01FC, R9
                RSUB    FAT32$READ_DW, 1        ; R11|R10: entry
                AND     0x0FFF, R11

                ; data clusters are 2 .. 0x0FFFFFF6
                XOR     R8, R8
                CMP     0, R11
                RBRA    _SDL_FN_HI, !Z
                CMP     2, R10                  ; N: entry < 2, free or reserved
                RBRA    _SDL_FN_BAD, N
_SDL_FN_HI      CMP     0x0FFF, R11
                RBRA    _SDL_FN_RET, !Z
                CMP     0xFFF7, R10             ; N: below bad cluster and end marks
                RBRA    _SDL_FN_RET, N
_SDL_FN_BAD     MOVE    SDL_ERR_CHAIN, R8

_SDL_FN_RET     DECRB
                RET

; ----------------------------------------------------------------------------
; LI_PROGRESS: move the progress bar of LOAD_IMAGE on by R9|R8 bytes
;
; SCRATCH_DWORD counts the bytes down to the next progress char, LI_PROGSTEP
; is the number of bytes per char (see LOAD_IMAGE in shell.asm).
; ----------------------------------------------------------------------------

LI_PROGRESS     INCRB
                MOVE    R8, R0                  ; R1|R0: bytes to account for
                MOVE    R9, R1
                MOVE    SCRATCH_DWORD, R2       ; R2: countdown low, R3: high
                MOVE    R2, R3
                ADD     1, R3

_LIP_LOOP       MOVE    R0, R6                  ; R7|R6: bytes - countdown
                MOVE    R1, R7
                SUB     @R2, R6
                SUBC    @R3, R7
                RBRA    _LIP_CHAR, !C           ; no borrow: a char is due

                MOVE    @R2, R4                 ; countdown - bytes
                MOVE    @R3, R5
                SUB     R0, R4
                SUBC    R1, R5
                MOVE    R4, @R2
                MOVE    R5, @R3
                DECRB
                RET

_LIP_CHAR       MOVE    R6, R0
                MOVE    R7, R1
                MOVE    LI_PROGSTEP, R4
                MOVE    @R4, @R2
                ADD     1, R4
                MOVE    @R4, @R3
                MOVE    @R2, R4                 ; a step of 0 would never end
                OR      @R3, R4
                RBRA    _LIP_PRINT, !Z
                MOVE    1, @R2

_LIP_PRINT      MOVE    SCRATCH_HEX, R8         ; behind the progress char..
                ADD     2, R8                   ; .. is memory we can use
                RSUB    SAVE_DEVSEL, 1          ; save target device selection
                MOVE    M2M$RAMROM_DEV, R8      ; activate the video ram
                MOVE    M2M$VRAM_DATA, @R8
                MOVE    M2M$RAMROM_4KWIN, R8
                MOVE    0, @R8
                MOVE    SCRATCH_HEX, R8
                RSUB    SCR$PRINTSTR, 1         ; print next progress char
                MOVE    SCRATCH_HEX, R8
                ADD     2, R8
                RSUB    RESTORE_DEVSEL, 1       ; restore target dev. sel.
                RBRA    _LIP_LOOP, 1
