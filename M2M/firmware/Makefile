# Firmware of the RISC-V system CPU (M2M/vhdl/rv/rv_soc.vhd).
# Needs the xPack RISC-V toolchain (riscv-none-elf-gcc) on PATH, or CROSS=<prefix>.
#   make   boot.rom, the boot ROM that rv_soc.vhd reads at synthesis, and
#          test/selftest.rom, the boot ROM of CORE/sim/tb_rv_soc.vhd
# Both .rom files are committed, so a core builds without the toolchain.

CROSS   ?= riscv-none-elf-
CC      := $(CROSS)gcc
OBJCOPY := $(CROSS)objcopy
SIZE    := $(CROSS)size

ROM_WORDS := 2048

ARCH    := -march=rv32imc_zicsr_zifencei -mabi=ilp32
CFLAGS  := $(ARCH) -Os -g -std=gnu11 -ffunction-sections -fdata-sections \
           -Wall -Wextra -Iinclude -Ilib
# Code and constants in the boot ROM, data and stack in the 16 KB data RAM
ROM_LD  := $(ARCH) -nostartfiles -Wl,--gc-sections -T neorv32/neorv32.ld \
           -Wl,--defsym,__neorv32_rom_base=0x0 -Wl,--defsym,__neorv32_rom_size=8K \
           -Wl,--defsym,__neorv32_ram_base=0x80000000 -Wl,--defsym,__neorv32_ram_size=16K \
           --specs=nano.specs

LIB := build/lib/console.o build/lib/trap.o build/crt0.o

all: boot.rom test/selftest.rom

build/%.o: %.c include/m2m_rv.h lib/console.h
	@mkdir -p $(dir $@)
	$(CC) $(CFLAGS) -c $< -o $@

build/crt0.o: neorv32/crt0.S
	@mkdir -p build
	$(CC) $(ARCH) -c $< -o $@

build/boot.elf: build/boot/boot.o $(LIB)
	$(CC) $(ROM_LD) -Wl,-Map,build/boot.map $^ -o $@ -lc -lgcc
	$(SIZE) $@

build/selftest.elf: build/test/selftest.o $(LIB)
	$(CC) $(ROM_LD) -Wl,-Map,build/selftest.map $^ -o $@ -lc -lgcc
	$(SIZE) $@

boot.rom: build/boot.elf tools/rom_txt.py
	$(OBJCOPY) -O binary -j .text -j .rodata -j .data $< build/boot.bin
	python3 -I tools/rom_txt.py build/boot.bin $@ $(ROM_WORDS)

test/selftest.rom: build/selftest.elf tools/rom_txt.py
	$(OBJCOPY) -O binary -j .text -j .rodata -j .data $< build/selftest.bin
	python3 -I tools/rom_txt.py build/selftest.bin $@ $(ROM_WORDS)

clean:
	rm -rf build

.PHONY: all clean
