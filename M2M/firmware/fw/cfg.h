// The core's configuration: config.vhd and globals.vhd (through the sysinfo device), and
// the menu's settings bits (control_m, QNICE's M2M$CFM_DATA). Selectors and addresses
// follow QNICE's sysdef.asm.
#pragma once
#include <stdint.h>

// config.vhd selectors (M2M$CFG_*)
#define CFG_WHS          0x1000u   // + record << 8 + page: welcome and help screens
#define CFG_DIR_START    0x0100u
#define CFG_CFG_FILE     0x0101u
#define CFG_GENERAL      0x0110u
#define CFG_CORENAME     0x0200u
#define CFG_OPTM_ITEMS   0x0300u
#define CFG_OPTM_GROUPS  0x0301u
#define CFG_OPTM_STDSEL  0x0302u
#define CFG_OPTM_LINES   0x0303u
#define CFG_OPTM_START   0x0304u
#define CFG_OPTM_ICOUNT  0x0305u
#define CFG_OPTM_MOUNT   0x0306u
#define CFG_OPTM_SINGLE  0x0307u
#define CFG_OPTM_MSTR    0x0308u
#define CFG_OPTM_DIM     0x0309u
#define CFG_OPTM_SSTR    0x030Au
#define CFG_OPTM_HELP    0x0310u
#define CFG_OPTM_CRTROM  0x0311u
#define CFG_OPTM_CRSTR   0x0312u

// CFG_GENERAL words
#define CFG_RP_COUNTER   1
#define CFG_RP_PAUSE     2
#define CFG_RP_WELCOME   3
#define CFG_RP_WLCM_RST  4
#define CFG_RP_KB_RST    5
#define CFG_RP_J1_RST    6
#define CFG_RP_J2_RST    7
#define CFG_RP_KB_OSD    8
#define CFG_RP_J1_OSD    9
#define CFG_RP_J2_OSD    10
#define CFG_ASCAL_USAGE  11
#define CFG_ASCAL_MODE   12
#define CFG_VD_AT_DELAY  13
#define CFG_VD_ITERSIZE  14
#define CFG_SAVEOSDCFG   15

#define CFG_AUSE_CFG     0         // CFG_ASCAL_USAGE: the mode from config.vhd
#define CFG_AUSE_CUSTOM  1         //                  left to the firmware
#define CFG_AUSE_AUTO    2         //                  follows the core

// Sysinfo device windows (M2M$SYS_*)
#define SYS_VDRIVES      0x0000u
#define SYS_VGA          0x0010u
#define SYS_CRTSANDROMS  0x0020u

uint16_t cfg_get(uint32_t sel, uint32_t idx);
int      cfg_str(uint32_t sel, char *buf, int size);   // a string from config.vhd, cut to size - 1
int      cfg_str_at(uint32_t sel, int from, char *buf, int size);   // ditto, from its character from
uint16_t sysinfo_get(uint32_t sel, uint32_t idx);

// The settings bits, one per menu item (M2M$GET_SETTING, M2M$SET_SETTING)
int      setting_get(int idx);
void     setting_set(int idx, int on);
