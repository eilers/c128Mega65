# 1541/1571 read-write qualification images

`157x-rw-test.d64` and `157x-rw-test.d71` contain the tokenized BASIC
program `RWTEST`.

1. Mount either image on virtual device 8 or 9.
2. Run `LOAD"RWTEST",8` (or `,9`), then `RUN`.
3. Select the same device number when prompted.

The program reads `SIDE1DAT`, creates and verifies 100 records in
`RWTEMP`, and scratches the temporary file. On the D71 image,
`SIDE1DAT` starts on track 36 and `SIDE0FILL` consumes the usable side-0
data area, so the temporary write is allocated on side 1. `SIDE0FILL` is
locked and exists only to force this qualification path.

Also check manually:

- `LOAD"$",8`/`,9` lists a valid directory.
- `LOAD"DOESNOTEXIST",8` makes the green LED blink; `PRINT DS$` clears it.
- After `RWTEST`, remount the image and confirm `RWTEMP` is absent.
- Repeat independently on devices 8 and 9, with the other virtual drive
  unmounted.

Rebuild deterministically with:

```sh
python3 test/157x-rw/create_images.py
```

Expected SHA-256:

- D64: `36895910c93ccb35f371763516eedb23eeab7134e13f21330312f50e9bcfd9da`
- D71: `c6e45b2774aefd110359b72fe723a4f65bbd90e7a75a520677640f425030f9a3`
