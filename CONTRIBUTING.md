Contributing to c128Mega65
==========================

You want to help? Welcome! Highly appreciated :-)

Make sure that you are familiar with the following before you start:

* MEGA65
* FPGA development using VHDL or Verilog and the Vivado toolchain
* MiSTer2MEGA65 framework: https://github.com/sy2002/MiSTer2MEGA65
* NEORV32, the RISC-V CPU that runs the on-screen menu, the drives and the
  ROM loading: https://github.com/stnolting/neorv32 and
  [doc/riscv-system-cpu.md](doc/riscv-system-cpu.md)

[doc/build-and-sim.md](doc/build-and-sim.md) explains how to build the core
and run the simulations.

This is meant to be a relaxed project. We have no written code of conduct and
no written coding guidelines, but here are some basics that we expect you to
follow when contributing.

Style
-----

* Ensure consistency: when you edit an existing file, code or documentation,
  stick as closely as you can to the style of that file. Styles differ
  between files, but we try to keep each file consistent.

* Keep comments short: a line or two on what the code does and why.

* Use American English: color, behavior, favorite, center, license, etc.

Branches
--------

* `master` is the latest release.

* `Release_Vx_y` (for example `Release_V1_1`) is a release line. It gets
  fixes for that release's features. Everything merged into a release branch
  is also merged into `develop`.

* `develop` is where most of the development happens, and it is the default
  branch. Keep it building and booting.

* Each change gets its own feature branch with a short kebab-case name, such
  as `cart-dma` or `swiftlink-modem`. It is merged with `git merge --no-ff`.

Pull Requests
-------------

* Make pull requests for the `develop` branch. If your change fixes a bug in
  a release, say so: we will also merge it into that release branch.

* Keep each PR to one topic, and use GitHub's features to describe it: what
  changes, why, and how you tested it.

* Test your changes thoroughly:
  * Run the simulations for the parts you touched (see
    [doc/build-and-sim.md](doc/build-and-sim.md)).
    `CORE/scripts/run_all_sims.sh` runs the whole Vivado suite.
  * Try the core on real hardware and say in the PR which MEGA65 boards you
    used. R3/R3A boards have no SDRAM; if you don't have one,
    `CORE/scripts/build_boards.sh 6r3` builds an R6 core with R3's memory map.
  * Share a beta version of the core in `#c128-core` on the MEGA65 Discord,
    so that it gets tested on many programs and hardware combinations.

* If you plan a larger contribution, discuss it with us first by opening an
  issue: https://github.com/eilers/c128Mega65/issues
  Describe what you plan to do and at-mention @eilers and @jflitton.

What goes where
---------------

* `CORE/C128_MiSTer` is a submodule (https://github.com/eilers/C128_MiSTer).
  Make changes to the MiSTer core there, on a branch off `Mega65_main`, and
  update the submodule in your c128Mega65 PR.

* The RISC-V firmware is committed as `sdcard/c128/firmware.bin`, so the core
  builds without a RISC-V toolchain. If you change its sources, rebuild it
  with `make -C M2M/firmware`, copy `M2M/firmware/build/firmware.bin` to
  `sdcard/c128/` and commit it with your change.
  `CORE/scripts/check_firmware_fresh.sh` checks this.

* Code taken from other projects keeps its license header and says where it
  came from, in its header or in a README next to it.

* Don't commit build output, logs, or editor and tool settings.

AI-assisted contributions
-------------------------

If an AI tool helped with a change, say so at the end of the commit message:

    Assisted-by: <agent>:<model version>

For example:

    Assisted-by: Claude Code:claude-opus-5-5

Use this instead of a `Co-Authored-By` line for the tool. You are still the
author: read and test everything you submit.

License & Credit
----------------

* If you contribute, make sure that your code is under GPL v3 (see LICENSE).
  Code from other projects needs a license that is compatible with it.

* We will give you credit in the AUTHORS file.
