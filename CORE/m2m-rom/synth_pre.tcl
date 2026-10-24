# build_boards.sh assembles the ROM once and sets M2M_ROM_PREBUILT, so boards
# synthesizing side by side don't rewrite the same files.
if {[info exists ::env(M2M_ROM_PREBUILT)] && $::env(M2M_ROM_PREBUILT) eq "1"} {
    puts "synth_pre: M2M ROM already assembled, skipping make_rom.sh"
    return
}

set cur_dir [exec pwd]
# ../../CORE/CORE-R3.runs/synth_1/

cd ../../../CORE/m2m-rom/
exec ./make_rom.sh <@stdin >@stdout 2>@stderr
cd $cur_dir
