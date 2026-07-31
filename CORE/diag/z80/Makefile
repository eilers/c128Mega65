# Offline Z80 diagnostic (no Vivado / no MEGA65 required)
ASM     ?= z80asm
PYTHON  ?= python3
OUT     := build
BIN     := $(OUT)/z80diag.bin
PRG8502 := $(OUT)/cia_matrix8502.prg

.PHONY: all clean vice-smoke vice-cia bins

all: bins

bins: $(BIN) $(PRG8502)

$(OUT):
	mkdir -p $(OUT)

$(BIN): z80diag.asm | $(OUT)
	$(ASM) --list=$(OUT)/z80diag.lst --label=$(OUT)/z80diag.lab -o $@ $<

$(PRG8502): mk_cia_prg.py | $(OUT)
	$(PYTHON) mk_cia_prg.py -o $@

vice-smoke: $(BIN)
	$(PYTHON) run_vice_smoke.py --bin $(BIN) --lab $(OUT)/z80diag.lab --no-mailbox

vice-smoke-mailbox: $(BIN)
	$(PYTHON) run_vice_smoke.py --bin $(BIN) --lab $(OUT)/z80diag.lab

vice-cia: $(PRG8502)
	@echo "Run: x128 -autostart $(PRG8502)"
	@echo "Then press 1, Z, Return — on-screen matrix should react."

clean:
	rm -rf $(OUT)
