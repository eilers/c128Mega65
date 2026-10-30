# The firmware's portable parts, built for and run on this Mac (run.sh makes the FAT
# images). Started by `make host-test` one folder up, which builds the firmware first.

HOSTCC ?= cc
CFLAGS := -O1 -g -Wall -Wextra -Wno-unused-parameter -I../include -I../lib -I../fatfs

# The network (CORE/firmware/net) with lwIP; UCI's network target needs it, on host_net.c's
# loopback
NET      := ../../../CORE/firmware/net
LWIP     := $(wildcard $(NET)/lwip/core/*.c) $(wildcard $(NET)/lwip/core/ipv4/*.c) $(NET)/lwip/netif/ethernet.c
REST_SRC := $(NET)/rest.c $(NET)/rest_machine.c $(NET)/rest_configs.c $(NET)/rest_drives.c \
            $(NET)/rest_files.c $(NET)/rest_runners.c $(NET)/multipart.c
NET_SRC  := host_net.c $(NET)/uci_net.c $(NET)/http.c $(REST_SRC) $(NET)/netcfg.c $(NET)/log.c $(LWIP)
NET_FLAGS := -I$(NET) -I$(NET)/lwip/include

all: build/test_boot build/test_fw build/test_shell build/test_uci build/test_prg build/test_mon build/test_modem build/test_http
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
             ../lib/console.c ../lib/fmt.c ../fatfs/ff.c ../fatfs/ffunicode.c $(NET_SRC)
build/test_shell: $(SHELL_SRC) host_m2m.h $(wildcard ../fw/*.h) ../build/fw/config_ids.h
	@mkdir -p build
	$(HOSTCC) $(CFLAGS) -I../fw -I../build/fw -I../../../CORE/firmware $(NET_FLAGS) -include host_m2m.h $(filter %.c,$^) -o $@

# The Ultimate Command Interface, its registers modelled in test_uci.c
UCI_SRC := test_uci.c host_uci_model.c host_m2m.c host_reu_sd.c diskio_file.c ../../../CORE/firmware/uci.c \
           ../../../CORE/firmware/uci_dos.c ../../../CORE/firmware/fr_text.c ../../../CORE/firmware/uci_ctrl.c ../../../CORE/firmware/reu.c \
           ../../../CORE/firmware/stores.c ../../../CORE/firmware/rompick.c ../../../CORE/firmware/c128dma.c \
           ../fw/heap.c ../lib/fmt.c \
           ../fatfs/ff.c ../fatfs/ffunicode.c $(NET_SRC)
build/test_uci: $(UCI_SRC) host_uci.h host_uci_model.h host_m2m.h host_reu_sd.h host_net.h ../../../CORE/firmware/uci.h \
                ../../../CORE/firmware/reu.h ../fw/ffconf.h \
                ../build/fw/config_ids.h $(wildcard $(NET)/*.h)
	@mkdir -p build
	$(HOSTCC) $(CFLAGS) -I../fw -I../build/fw -I../../../CORE/firmware $(NET_FLAGS) -include host_uci.h $(filter %.c,$^) -o $@

# test/uci's programs on a 6502, against the UCI firmware
PRG_SRC := test_prg.c $(filter-out test_uci.c,$(UCI_SRC))
build/test_prg: $(PRG_SRC) host_uci.h host_uci_model.h host_m2m.h host_reu_sd.h host_net.h ../../../CORE/firmware/uci.h \
                ../build/fw/config_ids.h $(wildcard $(NET)/*.h)
	@mkdir -p build
	$(HOSTCC) $(CFLAGS) -I../fw -I../build/fw -I../../../CORE/firmware $(NET_FLAGS) -include host_uci.h $(filter %.c,$^) -o $@

build/test_mon: test_mon.c ../fw/monitor.c ../fw/monitor.h
	@mkdir -p build
	$(HOSTCC) $(CFLAGS) -I../fw $(filter %.c,$^) -o $@

# The SwiftLink modem with lwIP, against a C128 and a server (CORE/firmware/net)
build/test_modem: test_modem.c $(NET)/modem.c $(NET)/log.c ../lib/fmt.c $(LWIP) $(wildcard $(NET)/*.h)
	@mkdir -p build
	$(HOSTCC) $(CFLAGS) $(NET_FLAGS) $(filter %.c,$^) -o $@

# The REST server with lwIP, against clients on a loopback (-serve: on a port of this Mac)
# The routes on host_rest.c's fakes, with the real stores.c and FatFs on a card image
build/test_http: test_http.c host_rest.c diskio_file.c $(NET)/http.c $(REST_SRC) ../../../CORE/firmware/stores.c \
                 ../../../CORE/firmware/rompick.c \
                 ../../../CORE/firmware/fr_text.c ../fatfs/ff.c ../fatfs/ffunicode.c \
                 $(NET)/log.c ../lib/fmt.c $(LWIP) $(wildcard $(NET)/*.h) host_rest.h host_rest_regs.h \
                 ../build/fw/config_ids.h ../build/fw/version.h ../fw/ffconf.h
	@mkdir -p build
	$(HOSTCC) $(CFLAGS) -I../fw -I../build/fw -I../include -I../fatfs -I../../../CORE/firmware $(NET_FLAGS) \
	          -include host_rest_regs.h -DMEMP_NUM_TCP_PCB=32 $(filter %.c,$^) -o $@

clean:
	rm -rf build

.PHONY: all clean
