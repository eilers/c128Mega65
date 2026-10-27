// Boot ROM of tb_rv_soc's firmware run: the testbench has put the firmware into its
// HyperRAM already, as the real boot ROM would, so this only jumps to it. It waits a few
// microseconds first: for 63 clocks after a reset rv_hr_bridge answers with bus errors
// (the real boot ROM reaches HyperRAM only milliseconds later, after the SD card).
.section .text.crt0
.global _start
_start:
   li   t1, 200
1: addi t1, t1, -1
   bnez t1, 1b
   li   t0, 0x40400000
   jr   t0
