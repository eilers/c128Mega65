Simulation-only models from C64MEGA65 (MJoergen/C64MEGA65 `develop`, commit
61fdbb2, by sy2002, GPL v3), copied unchanged:

- `mech_model_1581.vhd`: the drive mechanism and a formatted DD disk behind
  the floppy connector, spinning at 300 RPM.
- `golden_1581_pkg.vhd`: the disk's contents, so a bench can check every byte.
- `mfm_flux_gen_pkg.vhd`: the MFM flux generator the model uses.

`CORE/sim/tb_c1581_track.vhd` uses them.
