// Boot code for the modem's NEORV32, at 0xF0003000 (a LUT ROM in modem.vhd). The Shell
// loads /c128/modem.bin into HyperRAM; wait until a whole, checksummed image is there,
// then jump to it.
// Header (16 bytes at 0x40000000): magic "MDM1", length, sum of the image's words, version.

  .section .text
  .global _start
_start:
  li    s0, 0x40000000
  li    s1, 0x314D444D            // "MDM1"
  li    s2, 0x000FEFF0            // largest image: one byte per HyperRAM word, 255 windows

check:
  fence                           // drop cached lines, the Shell may still be loading
  lw    t0, 0(s0)
  bne   t0, s1, wait
  lw    t1, 4(s0)
  bgtu  t1, s2, wait
  addi  t1, t1, 3
  andi  t1, t1, -4
  addi  t3, s0, 16
  add   t4, t3, t1
  li    t5, 0
sum:
  beq   t3, t4, summed
  lw    t6, 0(t3)
  add   t5, t5, t6
  addi  t3, t3, 4
  j     sum
summed:
  lw    t0, 8(s0)
  bne   t0, t5, wait
  fence.i
  addi  t0, s0, 16
  jr    t0

wait:
  li    t0, 2500000               // about 0.1 s
1:
  addi  t0, t0, -1
  bnez  t0, 1b
  j     check
