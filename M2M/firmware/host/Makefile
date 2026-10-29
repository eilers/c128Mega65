# The firmware's portable parts, built for and run on this Mac (run.sh makes the FAT
# images). Started by `make host-test` one folder up, which builds the firmware first.

HOSTCC ?= cc
CFLAGS := -O1 -g -Wall -Wextra -Wno-unused-parameter -I../include -I../lib -I../fatfs

all: build/test_boot build/test_fw build/test_shell build/test_uci build/test_prg build/test_mon
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
             ../fw/roms.c ../fw/screen.c ../fw/sdcard.c ../fw/shell.c $(wildcard ../../../CORE/firmware/*.c) \
             ../lib/console.c ../lib/fmt.c ../fatfs/ff.c ../fatfs/ffunicode.c
build/test_shell: $(SHELL_SRC) host_m2m.h $(wildcard ../fw/*.h) ../build/fw/config_ids.h
	@mkdir -p build
	$(HOSTCC) $(CFLAGS) -I../fw -I../build/fw -I../../../CORE/firmware -include host_m2m.h $(filter %.c,$^) -o $@

# The Ultimate Command Interface, its registers modelled in test_uci.c
UCI_SRC := test_uci.c host_uci_model.c host_m2m.c host_reu_sd.c diskio_file.c ../../../CORE/firmware/uci.c \
           ../../../CORE/firmware/uci_dos.c ../../../CORE/firmware/uci_ctrl.c ../../../CORE/firmware/reu.c \
           ../fw/heap.c ../lib/fmt.c \
           ../fatfs/ff.c ../fatfs/ffunicode.c
build/test_uci: $(UCI_SRC) host_uci.h host_uci_model.h host_m2m.h host_reu_sd.h ../../../CORE/firmware/uci.h \
                ../../../CORE/firmware/reu.h ../fw/ffconf.h \
                ../build/fw/config_ids.h
	@mkdir -p build
	$(HOSTCC) $(CFLAGS) -I../fw -I../build/fw -I../../../CORE/firmware -include host_uci.h $(filter %.c,$^) -o $@

# test/uci's programs on a 6502, against the UCI firmware
PRG_SRC := test_prg.c $(filter-out test_uci.c,$(UCI_SRC))
build/test_prg: $(PRG_SRC) host_uci.h host_uci_model.h host_m2m.h host_reu_sd.h ../../../CORE/firmware/uci.h \
                ../build/fw/config_ids.h
	@mkdir -p build
	$(HOSTCC) $(CFLAGS) -I../fw -I../build/fw -I../../../CORE/firmware -include host_uci.h $(filter %.c,$^) -o $@

build/test_mon: test_mon.c ../fw/monitor.c ../fw/monitor.h
	@mkdir -p build
	$(HOSTCC) $(CFLAGS) -I../fw $(filter %.c,$^) -o $@

clean:
	rm -rf build

.PHONY: all clean
