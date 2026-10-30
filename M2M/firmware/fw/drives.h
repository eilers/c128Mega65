// The virtual drives (QNICE's vdrives.asm), whose disk images are read straight from the
// SD card (MAP_LOAD and VDSD_OFF in sdload.asm).
#pragma once
#include <stdint.h>

#define DRIVES_MAX 4

void drives_init(void);          // VD_INIT: globals.vhd's drives
int  drives_count(void);
int  drive_mounted(int d);
int  drives_changed(void);       // the mount status changed since last asked (VD_MNT_ST_GET)
// No 1541/1571 is acting out a disk change: after a mount, low until the drive has the disk
int  drives_ready(void);
// A drive's power: automatic (on while it has a disk or is the internal floppy), on (with
// no disk too) or off (with a disk too); kept over a C128 reset. drive_running: out of reset.
enum drive_power { DRIVE_AUTO, DRIVE_ON, DRIVE_OFF };
void drive_power(int d, enum drive_power p);
enum drive_power drive_power_of(int d);
int  drive_running(int d);
// Mount a disk image, or another one in its place: 0, or an error code and maybe *msg.
// ro: write protected (the drive refuses writes).
int  drive_mount(int d, const char *path, int group, const char **msg);
int  drive_mount_ro(int d, const char *path, int group, int ro, const char **msg);
const char *drive_path(int d);   // the mounted image's path on the card, or ""
// REST's unlink: the drive onto a copy of its image without a disk change. Hold it (its
// requests wait), write its changes back (drive_flush, then drive_quiet is 1 once they are
// on the card), copy, then drive_remap the copy (same size) and let go.
void drive_hold(int d, int on);
void drive_flush(int d);
int  drive_quiet(int d);
int  drive_remap(int d, const char *path);
void drive_unmount(int d);       // its changes go to the card first
int  drive_image_type(int d);    // 0 D64, 1 D71, 2 D81 (core_prep_load's), -1 none
