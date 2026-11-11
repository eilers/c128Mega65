C128_MiSTer RTL
===============

`rtl/` is the C128 core from MiSTer, https://github.com/MiSTer-devel/C128_MiSTer
by Erik Scheffers (credits in `/AUTHORS`), with our MEGA65 changes.

Its base is upstream's Release 20240904 (`5f0bdb4`). Until 2026-11 it was a
submodule, the fork https://github.com/eilers/C128_MiSTer (branch
`Mega65_main`). That fork's `rtl/` history is merged in here, so `git log` and
`git blame` go back through upstream. Commit hashes from before the merge
differ from the ones on GitHub; "upstream issue N" and "upstream PR N" in old
messages refer to the repository the commit came from.

To borrow a fix from upstream (or from any C128_MiSTer clone):

```bash
git -C <C128_MiSTer clone> format-patch -1 <commit> --stdout -- rtl \
  | git am --directory=CORE/C128_MiSTer
```
