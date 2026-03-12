# Macaulay2 Integration

## M2 script generation (M2Gen.idr)

Given a CSP from CompatCSP, generates a `.m2` script:

```m2
-- Auto-generated
R = QQ[v0_0, v0_1, ..., vN_K];

I = ideal(
  -- Boolean: v^2 - v for each variable
  v0_0^2 - v0_0,
  ...
  -- Exactly-one per node: sum of node's vars = 1
  v0_0 + v0_1 + v0_2 - 1,
  ...
  -- Incompatibility: product = 0 for each incompatible pair
  v3_0 * v7_0,
  ...
);

-- Unsatisfiability check
if 1 % I == 0 then print "UNSAT" else print "SAT";

-- Groebner basis
G = gens gb I;
print G;
```

For NS degree scanning:

```m2
print hilbertFunction(0, R/I);
-- If this is 0, the system is unsatisfiable
```

## M2 output parsing (M2Parse.idr)

Detects:
- `"UNSAT"` or `"SAT"` from the membership test
- `{1}` in Groebner basis output (= unsatisfiable)
- Hilbert function values

## Orchestration (NSDriver.idr)

1. Take a CSP from CompatCSP
2. Optionally select a subgraph (by node indices) to keep the instance small
3. Call M2Gen to write the `.m2` file
4. Shell out: `system "M2 --script generated.m2 > output.txt"`
5. Call M2Parse on the output
6. Report result

## Source files

- `src/Algebra/M2Gen.idr`
- `src/Algebra/M2Parse.idr`
- `src/Algebra/NSDriver.idr`
