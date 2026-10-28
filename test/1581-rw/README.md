# 1581 read-write qualification image

`1581-rw-test.d81` contains the BASIC program `RWTEST` (source: `rwtest.bas`).

1. Mount the image on virtual device 8 or 9.
2. Run `LOAD"RWTEST",8` (or `,9`), then `RUN`.
3. Select the same device number when prompted.

The program writes 500 records to `RWTEMP`, reads them back and verifies them,
scratches the file, and checks that opening it again reports `62 FILE NOT FOUND`.
It ends with `PASS: READ/WRITE OK ON DEVICE 8` (or 9). After the test, remount the
image and confirm `RWTEMP` is absent.

Rebuild deterministically with VICE's `petcat` and `c1541`:

```sh
petcat -w2 -l 1c01 -o rwtest.prg -- rwtest.bas
c1541 -format "1581 rw test,rt" d81 1581-rw-test.d81 -write rwtest.prg rwtest
rm rwtest.prg
```

Expected SHA-256: `292486a1fe0c014f48101d52c6401c40b0485d0f35094b66bc1e7c11af97cd76`
