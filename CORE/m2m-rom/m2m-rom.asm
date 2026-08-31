; ****************************************************************************
; YOUR-PROJECT-NAME (GITHUB-REPO-SHORTNAME) QNICE ROM
;
; Main program that is used to build m2m-rom.rom by make-rom.sh.
; The ROM is loaded by CORE/vhdl/globals.vhd via QNICE_FIRMWARE_M2M.
;
; The execution starts at the label START_FIRMWARE.
;
; done by YOURNAME in YEAR and licensed under GPL v3
; ****************************************************************************

; If the define RELEASE is defined, then the ROM will be a self-contained and
; self-starting ROM that includes the Monitor (QNICE "operating system") and
; jumps to START_FIRMWARE. In this case it is assumed, that the firmware is
; located in ROM and the variables are located in RAM.
;
; If RELEASE is not defined, then it is assumed that we are in the develop and
; debug mode so that the firmware runs in RAM and can be changed/loaded using
; the standard QNICE Monitor mechanisms such as "M/L" or QTransfer.

#define RELEASE

; If VDRIVE_DIAG_LOG is defined, the Shell dumps the 128-word diagnostic snapshot of
; virtual drive 8 to the JTAG UART about three times a second. Each dump is roughly 650
; characters, and the UART is written synchronously from the main loop, so the Shell
; stops servicing the virtual drives for tens of milliseconds per dump. That is visible
; from the core as stuttering. Enable it only while debugging a drive.

;#define VDRIVE_DIAG_LOG

; ----------------------------------------------------------------------------
; Firmware: M2M system
; ----------------------------------------------------------------------------

; main.asm is the mandatory, so always include it
; It jumps to START_FIRMWARE (see below) after the QNICE "operating system"
; called "Monitor" has been included and initialized
#include "../../M2M/rom/main.asm"

; Only include the Shell, if you want to use the pre-build core automation
; and user experience. If you build your own, then remove this include and
; also remove the include "shell_vars.asm" in the variables section below.
#include "../../M2M/rom/shell.asm"

; ----------------------------------------------------------------------------
; Firmware: Main Code
; ----------------------------------------------------------------------------

                ; Run the Shell: This is where you could put your own system
                ; instead of the shell
START_FIRMWARE  RBRA    START_SHELL, 1

; ----------------------------------------------------------------------------
; Core specific callback functions: Submenus
; ----------------------------------------------------------------------------

; SUBMENU_SUMMARY callback function:
;
; Called when displaying the main menu for every %s that is found in the
; "headline" / starting point of any submenu in config.vhd: You are able to
; change the standard semantics when it comes to summarizing the status of the
; very submenu that is meant by the "headline" / starting point.
;
; Input:
;   R8: pointer to the string that includes the "%s"
;   R9: pointer to the menu item within the M2M$CFG_OPTM_GROUPS structure
;  R10: end-of-menu-marker: if R9 == R10: we reached end of the menu structure
; Output:
;   R8: 0, if no custom SUBMENU_SUMMARY, else:
;       string pointer to completely new headline (do not modify/re-use R8)
;   R9, R10: unchanged

SUBMENU_SUMMARY XOR     R8, R8                  ; R8 = 0 = no custom string
                RET

; ----------------------------------------------------------------------------
; Core specific callback functions: File browsing and disk image mounting
; ----------------------------------------------------------------------------

; FILTER_FILES callback function:
;
; Called by the file- and directory browser. Used to make sure that the 
; browser is only showing valid files and directories.
;
; Input:
;   R8: Name of the file in capital letters
;   R9: 0=file, 1=directory
;  R10: Context (CTX_* constants in sysdef.asm)
;  R11: Menu group id (see config.vhd) of the menu item that is responsible
;       for triggering FILTER_FILES
; Output:
;   R8: 0=do not filter file, i.e. show file
FILTER_FILES    INCRB
                MOVE    R9, R0                  ; R0: remember the directory flag

                CMP     1, R9                   ; directories are always shown
                RBRA    _FFILES_SHOW, Z

                ; Mounting a disk image is the only context this core browses for, but be
                ; explicit about it so that adding a second context later cannot silently
                ; inherit the disk-image extension list.
                CMP     CTX_MOUNT_DISKIMG, R10
                RBRA    _FFILES_SHOW, !Z

                MOVE    DISKIMG_EXT, R1         ; R1: 0-terminated table of ext. pointers
_FFILES_EXT     MOVE    @R1++, R9               ; R9: next extension (0 = end of table)
                RBRA    _FFILES_HIDE, Z         ; end of table: no extension matched
                RSUB    M2M$CHK_EXT, 1          ; leaves R8/R9/R10 and our R1 alone
                RBRA    _FFILES_SHOW, C         ; extension matched
                RBRA    _FFILES_EXT, 1          ; try the next extension

_FFILES_HIDE    MOVE    1, R8                   ; filter the file
                RBRA    _FFILES_RET, 1

_FFILES_SHOW    XOR     R8, R8                  ; do not filter the file

_FFILES_RET     MOVE    R0, R9
                DECRB
                RET

; PREP_LOAD_IMAGE callback function:
;
; Some images need to be parsed, for example to extract configuration data or
; to move the file read pointer to the start position of the actual data.
; Sanity checks ("is this a valid file") can also be implemented here.
; Last but not least: The mount system supports the concept of a 2-bit
; "image type". In case this is used at the core of your choice, make sure
; you return the correct image type.
;
; Input:
;   R8: File handle: You are allowed to modify the read pointer of the handle
;   R9: Context (CTX_* constants in sysdef.asm)
;  R10: Menu group id (see config.vhd) of the menu item that is responsible
;       for triggering PREP_LOAD_IMAGE
; Output:
;   R8: 0=OK, error code otherwise
;   R9: image type if R8=0, otherwise 0 or optional ptr to  error msg string
;
; D64/D71/D81 are raw sector dumps without a header, so the file size is the only thing
; that identifies them -- and it has to identify them, because the image type decides
; whether iec_drive turns that drive into a 1541/1571 or into a 1581. The read pointer
; therefore stays at 0: the whole file is payload.
PREP_LOAD_IMAGE INCRB

                CMP     CTX_MOUNT_DISKIMG, R9   ; other contexts pass through unchecked
                RBRA    _PREP_LI_OTHER, !Z

                MOVE    R8, R0
                MOVE    R8, R1
                ADD     FAT32$FDH_SIZE_LO, R0
                MOVE    @R0, R0                 ; R0: low word of the file size
                ADD     FAT32$FDH_SIZE_HI, R1
                MOVE    @R1, R1                 ; R1: high word of the file size

                MOVE    IMGSIZE_TBL, R2         ; R2: table of (lo, hi, image type)
_PREP_LI_CMP    MOVE    @R2++, R3               ; R3: expected low word
                MOVE    @R2++, R4               ; R4: expected high word
                MOVE    @R2++, R5               ; R5: image type for this size
                CMP     IMGSIZE_END, R3         ; end of table reached?
                RBRA    _PREP_LI_WRONG, Z       ; yes: no size matched
                CMP     R3, R0
                RBRA    _PREP_LI_CMP, !Z        ; low word differs: next entry
                CMP     R4, R1
                RBRA    _PREP_LI_CMP, !Z        ; high word differs: next entry

                XOR     R8, R8                  ; no errors
                MOVE    R5, R9                  ; R9: image type
                RBRA    _PREP_LI_RET, 1

_PREP_LI_WRONG  MOVE    1, R8                   ; R8: error code
                MOVE    WRN_WRONG_IMG, R9       ; R9: error message
                RBRA    _PREP_LI_RET, 1

_PREP_LI_OTHER  XOR     R8, R8                  ; no errors
                XOR     R9, R9                  ; image type 0

_PREP_LI_RET    DECRB
                RET

; ----------------------------------------------------------------------------
; Core specific callback functions: Custom tasks
; ----------------------------------------------------------------------------

; PREP_START callback function:
;
; Called right before the core is being started. At this point, the core
; is ready to run, settings are loaded (if the core uses settings) and the
; core is still held in reset (if RESET_KEEP is on). So at this point in time,
; you can execute tasks that change the run-state of the core.
;
; Input: None
; Output:
;   R8: 0=OK, else pointer to string with error message
;   R9: 0=OK, else error code
PREP_START      INCRB
                XOR     R8, R8
                XOR     R9, R9
                DECRB
                RET

; OSM_SEL_POST callback function:
;
; Called each time the user selects something in the on-screen-menu (OSM),
; and while the OSM is still visible. This means, that this callback function
; is called on each press of one of the valid selection keys with the
; exception that pressing a selection key while hovering over a submenu entry
; or exit point does not call this function. All the functionality and
; semantics associated with a certain menu item is already handled by the
; framework when OSM_SELECTED is called, so you are not able to change the
; basic semantics but you are able to add core specific additional
; "intelligent" semantics and behaviors.
;
; Input:
;   R8: selected menu group (as defined in config.vhd)
;   R9: selected item within menu group
;       in case of single selected items: 0=not selected, 1=selected
;   R10: OPTM_KEY_SELECT (by default means "Return") or
;        OPTM_KEY_SELALT (by default means "Space")
; Output:
;   R8: 0=OK, else pointer to string with error message
;   R9: 0=OK, else error code
OSM_SEL_POST    INCRB
                XOR     R8, R8
                XOR     R9, R9
                DECRB
                RET

; OSM_SEL_PRE callback function:
;
; Identical to the OSM_SEL_POST callback function (see above) but it is being
; called before the functionality and semantics associated with a certain
; menu item has been handled by the framework.
OSM_SEL_PRE     INCRB
                XOR     R8, R8
                XOR     R9, R9
                DECRB
                RET

; ----------------------------------------------------------------------------
; Core specific callback functions: Custom messages
; ----------------------------------------------------------------------------

; CUSTOM_MSG callback function:
;
; Called in various situations where the Shell needs to output a message
; to the end user. The situations and contexts are described in sysdef.asm
;
; Input:
;   R8: Situation (CMSG_* constants in sysdef.asm)
;   R9: Context   (CTX_* constants in sysdef.asm)
; Output:
;   R8: 0=no custom message available, otherwise pointer to string

CUSTOM_MSG      INCRB
                MOVE    R8, R0
                XOR     R8, R8                  ; no custom message

                CMP     CMSG_BROWSENOTHING, R0  ; "the folder has nothing to show"?
                RBRA    _CUSTOM_MSG_RET, !Z
                CMP     CTX_MOUNT_DISKIMG, R9   ; while browsing for a disk image?
                RBRA    _CUSTOM_MSG_RET, !Z
                MOVE    WRN_NO_DISKIMG, R8      ; yes: name the formats we accept

_CUSTOM_MSG_RET DECRB
                RET

; ----------------------------------------------------------------------------
; Core specific constants and strings
; ----------------------------------------------------------------------------

; Disk image file extensions offered by the file browser. The list is
; 0-terminated, and the strings are compared against an upper-cased filename.
IMGEXT_D64      .ASCII_W ".D64"
IMGEXT_D71      .ASCII_W ".D71"
IMGEXT_D81      .ASCII_W ".D81"
DISKIMG_EXT     .DW      IMGEXT_D64, IMGEXT_D71, IMGEXT_D81, 0

; Image types as expected by the 2-bit img_type of vdrives.vhd, which main.vhd expands
; into the {img_hd, img_mfm, img_gcr, img_ds} vector of iec_drive. Raw GCR images (G64,
; G71) would need a fourth value and are therefore not supported.
IMGTYPE_D64     .EQU    0x0000                  ; 1541: single sided GCR
IMGTYPE_D71     .EQU    0x0001                  ; 1571: double sided GCR
IMGTYPE_D81     .EQU    0x0002                  ; 1581: MFM, 3.5 inch

; Accepted image sizes as (low word, high word, image type) triples. Only exact standard
; sizes are accepted: an error-info variant or a truncated image would hand the drive a
; wrong track count, and a too-large one would run past its 819,200 byte HyperRAM buffer.
;   D64, 35 tracks: 174,848 = 0x0002AB00      D71, 70 tracks: 349,696 = 0x00055600
;   D64, 40 tracks: 196,608 = 0x00030000      D81, 80 tracks: 819,200 = 0x000C8000
IMGSIZE_END     .EQU    0xFFFF                  ; end-of-table marker (no real size has it)
IMGSIZE_TBL     .DW     0xAB00, 0x0002, IMGTYPE_D64
                .DW     0x0000, 0x0003, IMGTYPE_D64
                .DW     0x5600, 0x0005, IMGTYPE_D71
                .DW     0x8000, 0x000C, IMGTYPE_D81
                .DW     IMGSIZE_END, IMGSIZE_END, 0

; Warning: the selected file is not an exact-size standard disk image
WRN_WRONG_IMG   .ASCII_P "\n\nThis is not a supported disk image. Sizes\n"
                .ASCII_P "must be exact: D64 174848 (35 tracks) or\n"
                .ASCII_P "196608 (40 tracks), D71 349696, D81 819200.\n"
                .ASCII_W "\nPress SPACE to continue.\n"

; Warning: the folder the user browsed into holds no mountable disk image
WRN_NO_DISKIMG  .ASCII_P "This core uses D64, D71 and D81 disk\n"
                .ASCII_P "images.\n\n"
                .ASCII_P "Please copy at least one D64, D71 or D81\n"
                .ASCII_P "file to any sub-directory or to the root\n"
                .ASCII_P "directory of this SD card.\n\n"
                .ASCII_P "If you use a folder called /c128, then\n"
                .ASCII_P "the file browser will always start there.\n\n"
                .ASCII_P "You can use long file names and you can\n"
                .ASCII_P "also use nested sub-directories to nicely\n"
                .ASCII_P "order your collection of disk images.\n\n"
                .ASCII_P "Nothing to browse.\n\n"
                .ASCII_W "Press Space to continue."

; This needs to be the last thing before the "Variables" sections starts
END_OF_ROM      .DW 0

; ----------------------------------------------------------------------------
; Variables: Need to be located in RAM
; ----------------------------------------------------------------------------

#ifdef RELEASE
                .ORG    0x8000                  ; RAM starts at 0x8000
#endif

;
; add your own variables here
;

; M2M Shell variables (only include, if you included "shell.asm" above)
#include "../../M2M/rom/shell_vars.asm"

; ----------------------------------------------------------------------------
; Heap and Stack: Need to be located in RAM after the variables
; ----------------------------------------------------------------------------

; The On-Screen-Menu uses the heap for several data structures. This heap
; is located before the main system heap in memory.
; You need to deduct MENU_HEAP_SIZE from the actual heap size below.
; Example: If your HEAP_SIZE would be 29696, then you write 29696-1024=28672
; instead, but when doing the sanity check calculations, you use 29696
MENU_HEAP_SIZE  .EQU 1024

#ifndef RELEASE

; heap for storing the sorted structure of the current directory entries
; this needs to be the last variable before the monitor variables as it is
; only defined as "BLOCK 1" to avoid a large amount of null-values in
; the ROM file
HEAP_SIZE       .EQU 6144                       ; 7168 - 1024 = 6144
HEAP            .BLOCK 1

; in RELEASE mode: 28k of heap which leads to a better user experience when
; it comes to folders with a lot of files
#else

HEAP_SIZE       .EQU 28672                      ; 29696 - 1024 = 28672
HEAP            .BLOCK 1

; The monitor variables use 22 words, round to 32 for being safe and subtract
; it from FF00 because this is at the moment the highest address that we
; can use as RAM: 0xFEE0
; The stack starts at 0xFEE0 (search var VAR$STACK_START in osm_rom.lis to
; calculate the address). To see, if there is enough room for the stack
; given the HEAP_SIZE do this calculation: Add 29696 words to HEAP which
; is currently 0xXXXX and subtract the result from 0xFEE0. This yields
; currently a stack size of more than 1.5k words, which is sufficient
; for this program.

                .ORG    0xFEE0                  ; TODO: automate calculation
#endif

; STACK_SIZE: Size of the global stack and should be a minimum of 768 words
; after you subtract B_STACK_SIZE.
; B_STACK_SIZE: Size of local stack of the the file- and directory browser. It
; should also have a minimum size of 768 words. If you are not using the
; Shell, then B_STACK_SIZE is not used.
STACK_SIZE      .EQU    1536
B_STACK_SIZE    .EQU    768

#include "../../M2M/rom/main_vars.asm"
