# R6 Core Constraint Rationale

This document explains the core-specific constraints in `CORE/CORE.xdc` for the C128 R6 build.

## Generated clocks

- `main_clk` is named on `CORE/clk_gen/main_clk_bufg/O`.
  - Reason: this is the actual buffered core clock used by the design hierarchy.
- `vdc_clk` is named on `CORE/clk_vdc_gen/mmcm_adv_inst/CLKOUT0`.
  - Reason: explicit naming keeps timing/debug reports readable and stable.

## Intentional DRC handling

- `ALLOW_COMBINATORIAL_LOOPS` on `CORE/i_main/cart_reset_o`.
  - Reason: known reset feedback path; acknowledged to avoid false blocker while preserving intended behavior.
- DRC `AVAL-139` set to warning.
  - Reason: VDC MMCM uses a fractional parameter that Vivado rounds to legal hardware granularity.
  - Effect: does not block bitstream generation; still visible in reports.

## Policy

- Any new waiver must include:
  - exact object and check name,
  - reason it is functionally safe,
  - expected runtime symptom if removed.
