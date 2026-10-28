# The firmware's portable parts, built for and run on this Mac (run.sh makes the FAT
# images). Started by `make host-test` one folder up, which builds the firmware first.

HOSTCC ?= cc
CFLAGS := -O1 -g -Wall -Wextra -Wno-unused-parameter -I../include -I../lib -I../fatfs

all: build/test_boot build/test_fw build/test_shell
	./run.sh build

build/test_boot: test_boot.c diskio_file.c ../boot/load.c ../lib/crc32.c ../fatfs/ff.c \
                 ../boot/ffconf.h hostmem.h
	@mkdir -p build
	$(HOSTCC) $(CFLAGS) -I../boot -include hostmem.h $(filter %.c,$^) -o $@

build/test_fw: test_fw.c diskio_file.c ../lib/fmt.c ../fatfs/ff.c ../fatfs/ffunicode.c \
               ../fw/ffconf.h
	@mkdir -p build
	$(HOSTCC) $(CFLAGS) -I../fw $(filter %.c,$^) -o $@

# The Shell, with registers, devices and HyperRAM faked in host_m2m.c
SHELL_SRC := test_shell.c host_m2m.c diskio_file.c ../fw/browse.c ../fw/cfg.c ../fw/drives.c \
             ../fw/heap.c ../fw/keys.c ../fw/menu.c \
             ../fw/roms.c ../fw/screen.c ../fw/sdcard.c ../fw/shell.c ../../../CORE/firmware/core.c \
             ../lib/console.c ../lib/fmt.c ../fatfs/ff.c ../fatfs/ffunicode.c
build/test_shell: $(SHELL_SRC) host_m2m.h $(wildcard ../fw/*.h) ../build/fw/config_ids.h
	@mkdir -p build
	$(HOSTCC) $(CFLAGS) -I../fw -I../build/fw -include host_m2m.h $(filter %.c,$^) -o $@

clean:
	rm -rf build

.PHONY: all clean
