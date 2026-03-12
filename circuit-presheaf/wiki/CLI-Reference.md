# CLI Reference

```
circuit-presheaf enumerate --dim D --max-size S
circuit-presheaf scaling --max-size S
circuit-presheaf convergence --dim D --max-size S
circuit-presheaf bent --size S
circuit-presheaf bent --size S --m2gen FILE.m2
circuit-presheaf m2run FILE.m2
```

## Commands

### enumerate
Enumerate all canonical formulas up to a given size for dimension D.

### scaling
Comxxxxxxxx scaling law sigma(s,d) for d in {2,3,4} at the given max size.

### convergence
Show how sigma converges as max size increases for a fixed dimension.

### bent
Analyze the inner product bent function (x0 AND x1) XOR (x2 AND x3) at n=4, d=2. Reports sub-cube count, structural edges, and edge compatibility classification.

With `--m2gen FILE.m2`: also generates a Macaulay2 script encoding the compatibility CSP and runs M2 if available.

### m2run
Run a previously generated `.m2` script through Macaulay2 and parse the results.

## Options

- `--json` - Machine-readable JSON output (planned)
