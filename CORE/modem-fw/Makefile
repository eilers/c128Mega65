# Modem firmware for the NEORV32 in the C128 core.
# Needs the xPack RISC-V toolchain (riscv-none-elf-gcc) on PATH, or CROSS=<prefix>.
#   make          build/modem.bin, which goes on the SD card as /c128/modem.bin
#   make bootrom  regenerate the boot ROM VHDL (only after changing boot/boot.S)

CROSS   ?= riscv-none-elf-
CC      := $(CROSS)gcc
OBJCOPY := $(CROSS)objcopy
SIZE    := $(CROSS)size

ARCH    := -march=rv32imc_zicsr_zifencei -mabi=ilp32
CFLAGS  := $(ARCH) -Os -g -std=gnu11 -ffunction-sections -fdata-sections \
           -Wall -Wextra -Wno-unused-parameter -Isrc -Ilwip/include
# Code and constants run from HyperRAM (instructions through the i-cache), data from the 16 KB DMEM.
LDFLAGS := $(ARCH) -nostartfiles -Wl,--gc-sections -T neorv32/neorv32.ld \
           -Wl,--defsym,__neorv32_rom_base=0x40000010 -Wl,--defsym,__neorv32_rom_size=0xFEFF0 \
           -Wl,--defsym,__neorv32_ram_base=0x80000000 -Wl,--defsym,__neorv32_ram_size=16K \
           -Wl,-Map,build/modem.map --specs=nano.specs

LWIP    := $(wildcard lwip/core/*.c) $(wildcard lwip/core/ipv4/*.c) lwip/netif/ethernet.c
SRC     := $(wildcard src/*.c) $(LWIP)
OBJ     := $(patsubst %.c,build/%.o,$(SRC)) build/crt0.o

BOOTROM_VHD := ../vhdl/modem/neorv32_bootrom_image.vhd

all: build/modem.bin

build/%.o: %.c src/lwipopts.h src/hw.h
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

build/crt0.o: neorv32/crt0.S
	@mkdir -p build
	$(CC) $(ARCH) -c $< -o $@

build/modem.elf: $(OBJ)
	$(CC) $(LDFLAGS) $(OBJ) -o $@ -lc -lgcc
	$(SIZE) $@

build/modem.bin: build/modem.elf tools/mkimage.py
	$(OBJCOPY) -O binary -j .text -j .rodata -j .data $< build/firmware.bin
	python3 -I tools/mkimage.py build/firmware.bin $@

bootrom: $(BOOTROM_VHD)

$(BOOTROM_VHD): boot/boot.S tools/bootrom_vhd.py
	@mkdir -p build
	$(CC) $(ARCH) -nostdlib -Wl,-Ttext=0xF0003000 $< -o build/boot.elf
	$(OBJCOPY) -O binary build/boot.elf build/boot.bin
	python3 -I tools/bootrom_vhd.py build/boot.bin $@

clean:
	rm -rf build

.PHONY: all bootrom clean
