# Build Order

Module dependency and implementation sequence:

1. `Formula.idr` + `Canonical.idr` + `Restriction.idr` -- get the core types right
2. `Enumerate.idr` -- verify formula counts match the test vectors
3. `RestrictionImage.idr` -- verify sigma values match
4. `SubCube.idr` + `CompatCSP.idr` -- verify BENT numbers match
5. `M2Gen.idr` -- generate `.m2` scripts
6. `M2Parse.idr` + `NSDriver.idr` -- run M2 if available
7. `Main.idr` -- CLI wiring
8. Push to d=3 s<=7 and d=4 s<=5 (new territory)
