This is a collection of test programs, primarily used for the VICE project, but
useful for other commodore emulators too.
--------------------------------------------------------------------------------

Note: all scripts will, by default, assume that:

a) both the VICE "trunk" and "testprogs" repositories were checked out into the
   same parent and using the names "trunk" and "testprogs", so you will have a
   directory structure like:

   foo/trunk
   foo/testprogs

b) VICE was configured to be built in-tree and was compiled already, so the
   binaries can be found in foo/trunk/src/

This allows the tests to easily operate on WIP builds without the need to set
up anything.

However, you can usually override this by setting EMUDIR to the path the
binaries can be located in.

--------------------------------------------------------------------------------

In the root directory you will find the following files and directories:

readme.txt      this text
Makefile        the toplevel (simple) interface to all tests

$ make
available targets:
petcat                   test 'petcat'
c1541                    test 'c1541'
cartconv                 test 'cartconv'
vice-autostart           autostart tests
vice-monitor             test ml-monitor
vice-remotemonitor       test remote monitor protocol
vice                     all vice- tests
testbench                run the emulation testbench
runtests                 do all of the above

If you want to run individual (emulation-) tests, use the testbench.sh script
in the testbench directory.


* VICE tools

./c1541
./cartconv
./petcat

* VICE features

testbench/autostart
Monitor
remotemonitor

* Emulator testbench

./testbench contains the testbench scripts. see the readme.txt in this directory
for instructions on how to do automated testing.

* Target test programs

All other directories contain programs that run on the target computer (or the
emulator, obviously) and tests various aspects of the emulation:

./general         - tests that do not fit into any other categories, usually
                    combined tests

* tests related to the various emulated chips

./CPU
./CIA
./SID
./VICII

./VDC             - VDC related, these run on C128
./TED             - TED related, these run on Plus4
./CRTC            - CRTC related, these run on PET

* tests specifically related to the various commodore computers

./C64             - specific C64 related tests
./interrupts
./c64-cpm

./c128
./c128-cpm

./DTV
./VIC20
./CBM2
./PET
./Plus4
./SCPU

./drive           - floppy drive tests

./vsid

* expansions

./memory-expansions
./plus256k
./plus60k
./ramcart
./GEO-RAM
./REU

./mouse
./propmouse

./audio-io
./digimax
./sfx_soundexpander
./sfx_soundsampler

./RTC
./cp-clockf83
./userportrtc

./joystick
./keypad
./testjoy
./userportsnes

* VICE subsystems

./crtemulation
./printer
./vdrive
./RS232
./MIDI

--------------------------------------------------------------------------------

TODO: the long term goal is to have some tests for everything that is emulated
      by VICE. still a long way to go, this is what might be missing:

* some existing tests are missing proper source code

VIC20/vic6581
VDC/40columns
DTV/tsuitDTV

* tests related to the various emulated chips

6510
----
- add more elaborated SHA/SHY/SHX page-boundary crossing tests

ACIA
----

CRTC
----

SID
---
- make test to check the POTX/Y sample period
- make envelope generator timing test (like waveform check)
- make test to check noise LFSR behavior on reset / test bit (it should take
  about 0x8000 cycles until it resets)
- make test to check correct noise LFSR sequence (like waveform check?)
- make proper test for "new" waveforms created by selecting noise with other
  waveforms (the regular waveform should get ANDed into the LFSR)

VIA
---
- make test program for power-on values
- make VIA shiftregister test program

VIC-II
------
- make test to check that the correct value is fetched for the "FLI-bug" area
- make more detailed sprite-collision timing test(s)
- make sprite-stretch test (cl13 plasma)

* tests specifically related to the various commodore computers

C64, C128, VIC20, PET, PLUS4, CBM2 ...

* tests specifically related to drives

1541:
-----
- make test program that measures mechanical delays (such as stepping)
- make a test program to check half tracks
- make a testcase for the case when V flag is set by "byte ready" and it is
  modified by an opcode at the same time. (ARR?)
- make test program to check various track lengths (in a g64)
- make test program to check various speed zones (in a g64)

vdrive
------

- add tests for directory wildcard handling, still some bugs: multiple wildcards
  aren't supported (eg "$:A*,B*") (#614)
- add tests for various CBMDOS commands

* expansions

super snapshot v5:
------------------

- make test for using SSV5+REU
