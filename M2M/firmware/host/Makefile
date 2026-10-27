# The firmware's portable parts, built for and run on this Mac (run.sh makes the FAT
# images). Started by `make host-test` one folder up, which builds the firmware first.

HOSTCC ?= cc
CFLAGS := -O1 -g -Wall -Wextra -Wno-unused-parameter -I../include -I../lib -I../fatfs

all: build/test_boot build/test_fw
	./run.sh build

build/test_boot: test_boot.c diskio_file.c ../boot/load.c ../lib/crc32.c ../fatfs/ff.c \
                 ../boot/ffconf.h hostmem.h
	@mkdir -p build
	$(HOSTCC) $(CFLAGS) -I../boot -include hostmem.h $(filter %.c,$^) -o $@

build/test_fw: test_fw.c diskio_file.c ../lib/fmt.c ../fatfs/ff.c ../fatfs/ffunicode.c \
               ../fw/ffconf.h
	@mkdir -p build
	$(HOSTCC) $(CFLAGS) -I../fw $(filter %.c,$^) -o $@

clean:
	rm -rf build

.PHONY: all clean
