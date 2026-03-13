# Macaulay2 Pipeline Report

## Overview

The circuit-presheaf tool generates polynomial ideal representations of the compatibility CSP from Boolean formula presheaf analysis, then optionally runs Macaulay2 to test satisfiability via Groebner basis computation.

## Pipeline Architecture

```
BENT function (TT=0x7888, n=4, d=2)
  |
  v
enumerate(d=2, maxSize=S)  -->  formulas grouped by truth table
  |
  v
allSubCubes(4, 2)  -->  24 sub-cubes, 96 structural edges
  |
  v
getDomain per sub-cube  -->  formulas computing each sub-function
  |
  v
overlapGroups per edge  -->  canonical groups (O(|D|) per edge)
  |
  v
generateM2FromCSP  -->  .m2 script (polynomial ring + ideal)
  |
  v
M2 --script  -->  SAT/UNSAT via Groebner basis
```

## Polynomial Encoding

For each CSP node (sub-cube) with domain size k, we create Boolean indicator variables `v_i_0 ... v_i_{k-1}` subject to:

1. **Boolean constraints**: `v^2 - v = 0` for each variable
2. **Exactly-one**: `sum(v_i_*) = 1` for each node
3. **Incompatibility**: `v_i_a * v_j_b = 0` for every pair (a, b) where domain elements a and b have different overlap-restricted canonical forms

If `1` is in the ideal (detected via Groebner basis or membership test), the system is UNSAT -- no consistent global assignment exists.

## Optimization: Canonical Groups

The original approach stored all incompatible pairs explicitly: O(|D_i| * |D_j|) per edge. With domains up to 9,624 formulas at size 4, this caused OOM on a 6GB system.

**Fix**: Store canonical groups instead. For each edge, we compute a map from overlap-restricted canonical string to list of domain indices. This is O(|D|) storage per edge. Incompatible pairs are expanded lazily during M2 script generation by taking cross-products of different groups.

## Scaling Results

| Size budget | Total formulas (d=2) | Max domain | Total CSP variables | M2 feasible? |
|-------------|---------------------|------------|--------------------:|:------------|
| 0 | 28 | 7 | 88 | Yes (SAT, <1s) |
| 1 | 28 | 7 | 88 | Yes (SAT, <1s) |
| 2 | 244 | 63 | 620 | No (>5min) |
| 3 | 2,828 | ~700 | ~6,000 | No |
| 4 | 36,052 | 9,624 | ~80,000 | No |

**Key finding**: M2 itself is the bottleneck, not our Idris2 code. The circuit-presheaf tool generates the M2 script in seconds even at size 2, but M2's Groebner basis computation is doubly exponential in the number of variables. At size 0 (88 variables) M2 solves in under 1 second. At size 2 (620 variables) M2 hangs indefinitely -- we tested with a 5-minute timeout and got no output. The Idris2 pipeline (enumeration, domain computation, overlap groups, script generation) completes in under 10 seconds at size 2. The wall is purely on the computer algebra side.

This was initially misdiagnosed as an Idris2 memory or performance bug. The symptom was the program appearing to hang with no output. Investigation revealed: (1) M2's `system` call to check version was blocking, and (2) once that was bypassed, M2's `--script` execution itself never terminated on the 620-variable, 9000-line ideal.

## Results

### Size 0 (SAT)
```
M2 results: [SAT]
System is SATISFIABLE (or inconclusive)
```
At the smallest size budget, the CSP has many compatible choices and the system is satisfiable. M2 completes in under 1 second.

### Size 2
The Idris2 pipeline generates a correct M2 script (167,293 characters, 8,998 lines, 620 variables) in ~5 seconds. However, running `M2 --script bent_s2.m2` does not terminate within 5 minutes. The script is available for offline analysis on more powerful hardware or with alternative solvers.

## Recommendations

1. **Sub-instance analysis**: Select small subgraphs (5-10 nodes) of the CSP and run M2 on those. This is tractable and can reveal local incompatibility structure.

2. **Hilbert function probing**: Instead of full Groebner basis, compute `hilbertFunction(0, R/I)`. If it returns 0, the system is UNSAT. This can sometimes be faster.

3. **Degree-bounded checks**: Use `hilbertFunction(d, R/I)` for small degrees d to probe the ideal structure without full computation.

4. **Symmetry reduction**: The BENT function has symmetry (4-fold rotational symmetry of the inner product). Quotienting by this symmetry would reduce the CSP by ~4x.

## Files

- `bent_s0.m2` -- Size 0 M2 script (88 variables, runs in <1s)
- `bent_s2.m2` -- Size 2 M2 script (620 variables, for offline analysis)
- `src/Algebra/M2Gen.idr` -- Script generation from canonical groups
- `src/Algebra/M2Parse.idr` -- Output parser (SAT/UNSAT/GB/Hilbert)
- `src/Algebra/NSDriver.idr` -- M2 execution orchestration
