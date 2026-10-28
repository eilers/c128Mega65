# Firmware of the RISC-V system CPU (M2M/vhdl/rv/rv_soc.vhd).
# Needs the xPack RISC-V toolchain (riscv-none-elf-gcc) on PATH, or CROSS=<prefix>.
#   make            boot.rom, the boot ROM that rv_soc.vhd reads at synthesis;
#                   build/firmware.bin, the firmware, for /c128/firmware.bin on the SD card;
#                   test/selftest.rom and test/fwboot.rom, boot ROMs of CORE/sim/tb_rv_soc.vhd
#   make host-test  the boot ROM's loader, run on this Mac against FAT32 images (host/)
# The .rom files are committed, so a core builds without the toolchain.

CROSS   ?= riscv-none-elf-
CC      := $(CROSS)gcc
OBJCOPY := $(CROSS)objcopy
SIZE    := $(CROSS)size
NM      := $(CROSS)nm

ROM_WORDS  := 2048
FW_BASE    := 0x40400000
HW_VERSION := $(shell sed -n 's/^\#define M2M_RV_HW_VERSION *\([0-9]*\)u.*/\1/p' include/m2m_rv.h)
FW_VERSION ?= $(shell git describe --always --dirty --match=NONE 2>/dev/null || echo unknown)
# The core's part of the firmware (the callbacks of QNICE's m2m-rom.asm) and its config.vhd
CORE_DIR   ?= ../../CORE/firmware
CONFIG_VHD ?= ../../CORE/vhdl/config.vhd
FILTERS    := ../video_filters/lanczos2_12.asm ../video_filters/Scan_Br_110_80.asm

ARCH    := -march=rv32imc_zicsr_zifencei -mabi=ilp32
CFLAGS  := $(ARCH) -Os -g -std=gnu11 -ffunction-sections -fdata-sections -MMD -MP \
           -Wall -Wextra -Iinclude -Ilib -Ifatfs
# Data and stack in the 16 KB data RAM; code and constants in the boot ROM, or for the
# firmware in its HyperRAM region
LD      := $(ARCH) -nostartfiles -Wl,--gc-sections -T neorv32/neorv32.ld --specs=nano.specs \
           -Wl,--defsym,__neorv32_ram_base=0x80000000 -Wl,--defsym,__neorv32_ram_size=16K
ROM_LD  := $(LD) -Wl,--defsym,__neorv32_rom_base=0x0 -Wl,--defsym,__neorv32_rom_size=8K
FW_LD   := $(LD) -Wl,--defsym,__neorv32_rom_base=$(FW_BASE) -Wl,--defsym,__neorv32_rom_size=4088K

COMMON  := build/common/lib/console.o build/common/lib/trap.o build/common/lib/crc32.o \
           build/common/lib/sd.o build/crt0.o
# FatFs and its disk layer are built twice, once with each ffconf.h
BOOT    := build/rom/boot/boot.o build/rom/boot/load.o build/rom/fatfs/ff.o build/rom/lib/diskio.o
FW      := $(patsubst %.c,build/fw/%.o,$(wildcard fw/*.c)) build/fw/core.o build/fw/lib/fmt.o \
           build/fw/fatfs/ff.o build/fw/fatfs/ffunicode.o build/fw/lib/diskio.o

all: boot.rom build/firmware.bin test/selftest.rom test/fwboot.rom

build/common/%.o: %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

build/rom/%.o: %.c
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -Iboot -c $< -o $@

FW_GEN  := build/fw/version.h build/fw/filters.h build/fw/config_ids.h

build/fw/%.o: %.c | $(FW_GEN)
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -Ifw -Ibuild/fw -c $< -o $@

build/fw/core.o: $(CORE_DIR)/core.c | $(FW_GEN)
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -Ifw -Ibuild/fw -c $< -o $@

build/fw/filters.h: $(FILTERS) tools/asm_dw.py
	@mkdir -p $(dir $@)
	python3 -I tools/asm_dw.py $@ $(FILTERS)

# config.vhd's menu groups, OPTM_G_<name> = <number>, for the core's callbacks
build/fw/config_ids.h: $(CONFIG_VHD)
	@mkdir -p $(dir $@)
	sed -n 's/^constant \(OPTM_G_[A-Z0-9_]*\) *: *integer *:= *\([0-9][0-9]*\);.*/#define \1 \2/p' $< > $@

# Rewritten only when the version changes, so only then the files that show it rebuild
build/fw/version.h: FORCE
	@mkdir -p $(dir $@)
	@echo '#define FW_VERSION "$(FW_VERSION)"' > $@.new
	@cmp -s $@.new $@ && rm $@.new || mv $@.new $@

build/crt0.o: neorv32/crt0.S
	@mkdir -p build
	$(CC) $(ARCH) -c $< -o $@

build/boot.elf: $(BOOT) $(COMMON)
	$(CC) $(ROM_LD) -Wl,-Map,build/boot.map $^ -o $@ -lc -lgcc
	$(SIZE) $@

build/selftest.elf: build/common/test/selftest.o $(COMMON)
	$(CC) $(ROM_LD) -Wl,-Map,build/selftest.map $^ -o $@ -lc -lgcc
	$(SIZE) $@

build/fw.elf: $(FW) $(COMMON)
	$(CC) $(FW_LD) -Wl,-Map,build/fw.map $^ -o $@ -lc -lgcc
	$(SIZE) $@

build/fwboot.elf: test/fwboot.S
	@mkdir -p build
	$(CC) $(ARCH) -nostdlib -Wl,-Ttext=0 $< -o $@

boot.rom: build/boot.elf tools/rom_txt.py
	$(OBJCOPY) -O binary -j .text -j .rodata -j .data $< build/boot.bin
	python3 -I tools/rom_txt.py build/boot.bin $@ $(ROM_WORDS)

test/selftest.rom: build/selftest.elf tools/rom_txt.py
	$(OBJCOPY) -O binary -j .text -j .rodata -j .data $< build/selftest.bin
	python3 -I tools/rom_txt.py build/selftest.bin $@ $(ROM_WORDS)

test/fwboot.rom: build/fwboot.elf tools/rom_txt.py
	$(OBJCOPY) -O binary -j .text $< build/fwboot.bin
	python3 -I tools/rom_txt.py build/fwboot.bin $@ $(ROM_WORDS)

# The firmware file, and its image as tb_rv_soc's HyperRAM reads it (build/fw_hr.txt)
build/firmware.bin: build/fw.elf tools/fw_image.py tools/rom_txt.py
	$(OBJCOPY) -O binary -j .text -j .rodata -j .data $< build/fw_image.bin
	python3 -I tools/fw_image.py build/fw_image.bin $@ $(HW_VERSION) $(FW_BASE) \
	   0x$$($(NM) $< | sed -n 's/ T _start$$//p') "$(FW_VERSION)"
	python3 -I tools/rom_txt.py build/fw_image.bin build/fw_hr.txt 1048576

host-test: build/firmware.bin
	$(MAKE) -C host

clean:
	rm -rf build
	$(MAKE) -C host clean

-include $(shell find build -name '*.d' 2>/dev/null)

.PHONY: all host-test clean FORCE
