// Boot ROM of tb_rv_soc's firmware run: the testbench has put the firmware into its
// HyperRAM already, as the real boot ROM would, so this only jumps to it, straight after
// the reset.
.section .text.crt0
.global _start
_start:
   li   t0, 0x40400000
   jr   t0
